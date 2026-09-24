import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { execFileSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import {
  documents, evidence, names, read, validateProposal,
} from './pilot.mjs';

/**
 * Select only the explicitly enabled, vision-capable Astra catalogue entry.
 * @param {object[]} models Account-specific Copilot model catalogue.
 * @param {string} requested Explicit requested model identifier.
 * @returns {object} The enabled Astra catalogue entry.
 */
export function selectModel(models, requested) {
  assert.equal(requested, 'gpt-6-astra',
    'Only gpt-6-astra is approved; no model fallback is allowed');
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
 * Audit typed SDK events, not the CLI's unspecified JSONL presentation format.
 * This checks model accounting, not whether the model understood every pixel.
 * @param {object[]} events Copilot SDK session events.
 * @param {string} model Exact approved model ID.
 * @returns {object} Schema-validated prose proposal.
 */
export function auditResponse(events, model) {
  assert(events.some(event => event.type === 'session.idle'),
    'Copilot did not complete the review');
  const usages = events.filter(event => event.type === 'assistant.usage');
  assert(usages.length > 0, 'Missing Copilot inference-model accounting');
  const messages = events.filter(event => event.type === 'assistant.message');
  assert.equal(messages.length, 1, 'Expected one final JSON proposal');
  for (const event of events) {
    assert(!['session.error', 'session.warning', 'model.call_failure',
      'tool.execution_start', 'assistant.server_tool_progress'].includes(event.type),
    'Copilot reported a warning, failed/retried request, or attempted a tool');
    if (event.type === 'session.model_change') {
      assert.equal(event.data.newModel, model, 'Model substitution is forbidden');
    }
    if (event.type === 'assistant.usage') {
      assert.equal(event.data.model, model, 'Missing or different inference model');
      assert(!event.data.isAuto && !event.data.isByok);
      if (event.data.availableToolCount !== undefined) {
        assert.equal(event.data.availableToolCount, 0);
      }
    }
    if (event.type === 'assistant.message' && event.data.model !== undefined) {
      assert.equal(event.data.model, model, 'Unexpected response model');
    }
  }
  return validateProposal(JSON.parse(messages[0].data.content));
}

/**
 * Review native PNG attachments through the pinned GitHub Copilot SDK.
 * Empty mode has no ambient tools, instructions, agents, skills, or MCP servers.
 * @returns {Promise<void>} Resolves after a verified Astra proposal is saved.
 */
async function main() {
  const outputDirectory = 'test-results/model';
  const privateDirectory = path.resolve('test-results/private-model');
  const logsDirectory = path.resolve(outputDirectory, 'runtime-logs');
  const workingDirectory = path.join(privateDirectory, 'workspace');
  const home = path.join(privateDirectory, 'home');
  const config = path.join(home, '.copilot');
  fs.mkdirSync(outputDirectory, { recursive: true });
  fs.mkdirSync(logsDirectory, { recursive: true });
  fs.mkdirSync(workingDirectory, { recursive: true });
  fs.mkdirSync(config, { recursive: true });
  fs.writeFileSync(path.join(config, 'config.json'),
    JSON.stringify({ continueOnAutoMode: false }));
  const token = process.env.COPILOT_GITHUB_TOKEN;
  assert(token, 'Set the protected COPILOT_DOCUMENTATION_TOKEN environment secret');
  const model = process.env.ASTRA_MODEL_ID || 'gpt-6-astra';
  assert.equal(model, 'gpt-6-astra', 'Only the exact Astra model ID is approved');
  const { CopilotClient, RuntimeConnection } = await import('@github/copilot-sdk');
  const executable = execFileSync('which', ['copilot'], {
    encoding: 'utf8',
  }).trim();
  const client = new CopilotClient({
    mode: 'empty',
    connection: RuntimeConnection.forStdio({
      path: executable, args: ['--log-dir', logsDirectory],
    }),
    workingDirectory, baseDirectory: config,
    gitHubToken: token, useLoggedInUser: false, logLevel: 'warning',
    // Never inherit repository tokens, provider keys, proxies, or shell hooks.
    env: { PATH: process.env.PATH, HOME: home, LANG: 'C.UTF-8', CI: 'true' },
  });
  const events = [];
  let session;
  let proposal;
  try {
    await client.start();
    const status = await client.getStatus();
    assert.equal(status.version, '1.0.88', 'Unexpected Copilot runtime version');
    fs.writeFileSync(`${outputDirectory}/runtime.json`, JSON.stringify(status));
    const models = await client.listModels();
    fs.writeFileSync(`${outputDirectory}/models.json`,
      JSON.stringify(models, null, 2));
    const selected = selectModel(models, model);
    const directory = 'test-results/before/evidence';
    const captured = evidence(directory);
    const manifest = JSON.parse(read(path.join(directory, 'manifest.json')));
    assert.equal(manifest.tested_sha, process.env.TESTED_SHA);
    assert.deepEqual(captured.hashes, manifest.hashes);
    const attachments = names.map(name => {
      const image = read(path.join(directory, `${name}.png`));
      const limit = selected.capabilities?.limits?.vision?.max_prompt_image_size;
      if (limit !== undefined) {
        assert(image.length <= limit, 'Screenshot exceeds model image limit');
      }
      return {
        type: 'blob', mimeType: 'image/png', displayName: `${name}.png`,
        data: image.toString('base64'),
      };
    });
    const articles = Object.fromEntries(Object.entries(documents).map(
      ([section, filename]) => [section, read(filename).toString()]));
    const prompt = JSON.stringify({
      manifest, observations: captured.observations, articles,
      attachment_order: names,
    });
    assert(Buffer.byteLength(prompt) <= 500_000, 'Evidence exceeds pilot limit');
    fs.writeFileSync(`${outputDirectory}/request.json`, JSON.stringify({
      model, sdk: '1.0.14', tested_sha: manifest.tested_sha,
      image_delivery: 'GitHub Copilot SDK native image/png blob attachments',
      attachments: names, evidence_hashes: captured.hashes, tools: [],
    }, null, 2));
    session = await client.createSession({
      model, availableTools: [], tools: [],
      excludedTools: ['builtin:*', 'mcp:*', 'custom:*'],
      workingDirectory, infiniteSessions: { enabled: false },
      systemMessage: {
        mode: 'replace',
        content: read('.github/continuous-documentation/prompt.txt').toString(),
      },
      onPermissionRequest: () => ({
        kind: 'reject', feedback: 'Documentation review has no tool permissions.',
      }),
    });
    session.on(event => {
      events.push(event);
      fs.appendFileSync(`${outputDirectory}/events.jsonl`,
        JSON.stringify(event) + '\n');
    });
    await session.sendAndWait({ prompt, attachments }, 15 * 60_000);
    proposal = auditResponse(events, model);
  } finally {
    try {
      if (session) await session.disconnect();
    } finally {
      const errors = await client.stop();
      // The runtime can retry image failures without images. Warning/error
      // logs supplement typed events; neither may permit a prose-only retry.
      let imageDiagnostic = false;
      for (const filename of fs.readdirSync(logsDirectory, { recursive: true })) {
        const logfile = path.join(logsDirectory, filename);
        if (!fs.lstatSync(logfile).isFile()) continue;
        const contents = read(logfile).toString().replaceAll(token, '[REDACTED]');
        fs.writeFileSync(logfile, contents);
        imageDiagnostic ||= /image|attachment|vision/i.test(contents);
      }
      assert(!imageDiagnostic,
        'Copilot logged an image warning/error; refusing the proposal');
      assert.equal(errors.length, 0,
        'Copilot runtime did not shut down cleanly: ' + errors.join('; '));
    }
  }
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
