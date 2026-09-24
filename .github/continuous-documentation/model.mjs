import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { spawn, spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import {
  documents, evidence, names, read, validateProposal,
} from './pilot.mjs';

/**
 * Reject aliases, automatic routing, and other models before any inference.
 * @param {object[]} models Account-specific Copilot model catalogue.
 * @param {string} requested Explicit requested model identifier.
 * @returns {object} The enabled vision-capable Astra catalogue entry.
 */
export function selectModel(models, requested) {
  assert.equal(requested, 'gpt-6-astra',
    'Only the explicit gpt-6-astra ID is approved; no model fallback is allowed');
  const model = models.find(candidate => candidate.id === requested);
  assert(model, 'GitHub Copilot account does not offer gpt-6-astra');
  assert(!model.policy || model.policy.state === 'enabled',
    'GPT-6 Astra is not enabled by the account model policy');
  assert.equal(model.capabilities?.supports?.vision, true,
    'GPT-6 Astra must explicitly advertise image support');
  const vision = model.capabilities?.limits?.vision;
  if (vision?.supported_media_types) {
    assert(vision.supported_media_types.includes('image/png'));
  }
  if (vision?.max_prompt_images !== undefined) {
    assert(vision.max_prompt_images >= names.length,
      'GPT-6 Astra cannot accept all five screenshots');
  }
  return model;
}

/**
 * Query the CLI's native SDK JSON-RPC transport without an SDK dependency.
 * Official protocol: github/copilot-sdk nodejs/src/client.ts (models.list).
 * No session or inference request is created by this capability probe.
 * @param {object} environment Isolated CLI environment.
 * @param {string} workingDirectory Empty, trusted working directory.
 * @returns {Promise<object[]>} Account-specific available models.
 */
function listModels(environment, workingDirectory) {
  return new Promise((resolve, reject) => {
    const child = spawn('copilot', [
      '--headless', '--stdio', '--no-auto-update',
      '--no-custom-instructions', '--disable-builtin-mcps',
      '--log-level', 'none',
    ], { env: environment, cwd: workingDirectory });
    let buffered = Buffer.alloc(0);
    let finished = false;
    const finish = (error, result) => {
      if (finished) return;
      finished = true;
      clearTimeout(timer);
      child.kill('SIGTERM');
      if (error) reject(error);
      else resolve(result);
    };
    const timer = setTimeout(() => finish(
      new Error('GitHub Copilot model discovery timed out')), 90_000);
    child.on('error', error => finish(error));
    child.on('exit', () => finish(
      new Error('GitHub Copilot model discovery exited without a catalogue')));
    child.stderr.on('data', data => {
      fs.appendFileSync('test-results/model/discovery.log', data);
    });
    child.stdout.on('data', data => {
      try {
        buffered = Buffer.concat([buffered, data]);
        assert(buffered.length <= 20_000_000, 'Oversized CLI RPC response');
        while (true) {
          const boundary = buffered.indexOf('\r\n\r\n');
          if (boundary < 0) break;
          const header = buffered.subarray(0, boundary).toString();
          const match = /^Content-Length: (\d+)$/im.exec(header);
          assert(match, 'Unsupported CLI SDK framing');
          const size = Number(match[1]);
          assert(size <= 20_000_000);
          if (buffered.length < boundary + 4 + size) break;
          const message = JSON.parse(buffered.subarray(
            boundary + 4, boundary + 4 + size).toString());
          buffered = buffered.subarray(boundary + 4 + size);
          if (message.id !== 1) continue;
          assert(!message.error,
            'Copilot model discovery failed; check Copilot Requests entitlement');
          assert(Array.isArray(message.result?.models),
            'Unsupported Copilot model catalogue schema');
          finish(null, message.result.models);
        }
      } catch (error) {
        finish(error);
      }
    });
    const request = JSON.stringify({
      jsonrpc: '2.0', id: 1, method: 'models.list', params: {},
    });
    child.stdin.on('error', error => finish(error));
    child.stdin.write(
      `Content-Length: ${Buffer.byteLength(request)}\r\n\r\n${request}`);
  });
}

/**
 * Check the CLI's structured audit stream before accepting any model output.
 * Unsupported event formats fail closed rather than relying on self-report.
 * @param {object[]} events Copilot JSONL session events.
 * @param {string} model Exact approved model ID.
 * @returns {object} Schema-validated prose proposal.
 */
export function auditResponse(events, model) {
  const shutdown = events.find(event => event.type === 'session.shutdown');
  assert(shutdown, 'Missing Copilot shutdown audit event');
  assert.equal(shutdown.data.shutdownType, 'routine',
    'Copilot did not complete normally');
  assert.deepEqual(Object.keys(shutdown.data.modelMetrics), [model],
    'Copilot used an unexpected model; refusing its output');
  assert.deepEqual(shutdown.data.codeChanges.filesModified, [],
    'Copilot modified files despite the read-only contract');
  const messages = events.filter(event => event.type === 'assistant.message');
  assert.equal(messages.length, 1, 'Expected one final JSON proposal');
  for (const event of events) {
    assert(!['session.error', 'tool.execution_start',
      'assistant.server_tool_progress'].includes(event.type),
    'Copilot attempted a tool or reported an error');
    if (event.type === 'session.model_change') {
      assert.equal(event.data.newModel, model, 'Model substitution is forbidden');
    }
    if (event.type === 'assistant.usage' ||
        event.type === 'assistant.message') {
      assert.equal(event.data.model, model, 'Missing or different inference model');
      assert(!event.data.isAuto && !event.data.isByok);
      if (event.data.availableToolCount !== undefined) {
        assert.equal(event.data.availableToolCount, 0);
      }
    }
  }
  return validateProposal(JSON.parse(messages[0].data.content));
}

/**
 * Supply actual PNG attachments through the documented CLI --attachment flag.
 * Tools are completely disabled: the model only returns constrained JSON.
 * @returns {Promise<void>} Resolves after a verified Astra proposal is saved.
 */
async function main() {
  const outputDirectory = 'test-results/model';
  const privateDirectory = path.resolve('test-results/private-model');
  const workingDirectory = path.join(privateDirectory, 'workspace');
  fs.mkdirSync(outputDirectory, { recursive: true });
  fs.mkdirSync(workingDirectory, { recursive: true });
  const home = path.join(privateDirectory, 'home');
  const config = path.join(home, '.copilot');
  fs.mkdirSync(config, { recursive: true });
  fs.writeFileSync(path.join(config, 'config.json'), JSON.stringify({
    continueOnAutoMode: false,
    customAgents: { defaultLocalOnly: true },
  }));
  const token = process.env.COPILOT_GITHUB_TOKEN;
  assert(token, 'Set the protected COPILOT_DOCUMENTATION_TOKEN environment secret');
  const model = process.env.ASTRA_MODEL_ID || 'gpt-6-astra';
  assert.equal(model, 'gpt-6-astra', 'Only the exact Astra model ID is approved');
  // Do not inherit GITHUB_TOKEN, provider keys, shell hooks, plugins, or proxies.
  const environment = {
    PATH: process.env.PATH, HOME: home, COPILOT_HOME: config,
    XDG_CONFIG_HOME: path.join(home, '.config'),
    LANG: 'C.UTF-8', CI: 'true',
    COPILOT_GITHUB_TOKEN: token,
  };
  const version = spawnSync('copilot', ['--no-auto-update', '--version'], {
    env: environment, encoding: 'utf8', timeout: 30_000,
  });
  assert.equal(version.status, 0, 'Pinned Copilot CLI is unavailable');
  assert.match(version.stdout, /GitHub Copilot CLI 1\.0\.88\./);
  fs.writeFileSync(`${outputDirectory}/cli-version.txt`, version.stdout);
  const models = await listModels(environment, workingDirectory);
  fs.writeFileSync(`${outputDirectory}/models.json`, JSON.stringify(models, null, 2));
  const selected = selectModel(models, model);
  const evidenceDirectory = 'test-results/before/evidence';
  const captured = evidence(evidenceDirectory);
  const manifest = JSON.parse(read(path.join(evidenceDirectory, 'manifest.json')));
  assert.equal(manifest.tested_sha, process.env.TESTED_SHA);
  assert.deepEqual(captured.hashes, manifest.hashes);
  const articles = Object.fromEntries(Object.entries(documents).map(
    ([section, filename]) => [section, read(filename).toString()]));
  const prompt = read('.github/continuous-documentation/prompt.txt').toString() +
    '\n\nBEGIN UNTRUSTED OBSERVATIONS AND EXISTING ARTICLES\n' +
    JSON.stringify({ manifest, observations: captured.observations, articles }) +
    '\nEND UNTRUSTED OBSERVATIONS AND EXISTING ARTICLES\n' +
    'Attached PNGs, in order: ' + names.join(', ') + '.';
  assert(Buffer.byteLength(prompt) < 120_000, 'Evidence exceeds pilot prompt limit');
  const args = [
    '--no-auto-update', '--model', model,
    '--available-tools=', '--deny-tool=shell', '--deny-tool=write',
    '--deny-tool=url',
    '--disable-builtin-mcps', '--no-custom-instructions', '--no-ask-user',
    '--no-remote', '--no-remote-export', '--no-bash-env', '--disallow-temp-dir',
    '--secret-env-vars=COPILOT_GITHUB_TOKEN',
    '--output-format', 'json', '--stream', 'on',
    '--log-level', 'error', '--prompt', prompt,
  ];
  for (const name of names) {
    const filename = path.resolve(evidenceDirectory, `${name}.png`);
    const limit = selected.capabilities?.limits?.vision?.max_prompt_image_size;
    if (limit !== undefined) {
      assert(read(filename).length <= limit, 'Screenshot exceeds model image limit');
    }
    // Stage only the trusted images inside the isolated session working tree.
    const staged = path.join(workingDirectory, `${name}.png`);
    fs.copyFileSync(filename, staged);
    args.push('--attachment', staged);
  }
  fs.writeFileSync(`${outputDirectory}/request.json`, JSON.stringify({
    model, tested_sha: manifest.tested_sha,
    image_delivery: 'GitHub Copilot CLI --attachment (five image/png files)',
    attachments: names.map(name => `${name}.png`),
    evidence_hashes: captured.hashes,
    tools: [], continueOnAutoMode: false,
  }, null, 2));
  const result = spawnSync('copilot', args, {
    cwd: workingDirectory, env: environment, encoding: 'utf8',
    timeout: 15 * 60_000, maxBuffer: 20_000_000,
  });
  fs.writeFileSync(`${outputDirectory}/events.jsonl`, result.stdout || '');
  fs.writeFileSync(`${outputDirectory}/stderr.log`, result.stderr || '');
  assert.equal(result.status, 0,
    'GPT-6 Astra screenshot review failed; no fallback or proposal accepted');
  const events = result.stdout.trim().split('\n').map(line => JSON.parse(line));
  const proposal = auditResponse(events, model);
  fs.writeFileSync(`${outputDirectory}/proposal.json`,
    JSON.stringify(proposal, null, 2));
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  main().catch(error => {
    fs.mkdirSync('test-results/model', { recursive: true });
    fs.writeFileSync('test-results/model/failure.txt', error.message + '\n');
    console.error(`Continuous documentation pilot stopped: ${error.message}`);
    process.exitCode = 1;
  });
}
