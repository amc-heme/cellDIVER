import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import fs from 'node:fs';
import path from 'node:path';
import { execFileSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';

export const names = ['config', 'dimplot', 'featureplot', 'subset', 'dge'];
export const documents = {
  config: 'vignettes/dataset_setup_walkthrough.Rmd',
  plotting: 'vignettes/scRNA_Plots_Explained.Rmd',
};
const start = '<!-- continuous-documentation: start -->';
const end = '<!-- continuous-documentation: end -->';
const hash = value => createHash('sha256').update(value).digest('hex');

/**
 * Read a bounded, regular data file; artifacts must never supply symlinks.
 * @param {string} filename File to read.
 * @returns {Buffer} File contents.
 */
export function read(filename) {
  const stat = fs.lstatSync(filename);
  assert(stat.isFile() && stat.size <= 20_000_000, `Unsafe file: ${filename}`);
  return fs.readFileSync(filename);
}

/**
 * Validate the evidence contract and collect immutable content hashes.
 * @param {string} directory Trusted capture artifact directory.
 * @returns {object} Evidence observations and hashes.
 */
export function evidence(directory) {
  const observations = [];
  const hashes = {};
  for (const name of names) {
    for (const suffix of ['.png', '.json', '-logs.csv']) {
      const filename = `${name}${suffix}`;
      hashes[filename] = hash(read(path.join(directory, filename)));
    }
    const image = read(path.join(directory, `${name}.png`));
    assert.equal(image.subarray(0, 8).toString('hex'), '89504e470d0a1a0a');
    assert.equal(image.readUInt32BE(16), 1440, 'Unexpected screenshot width');
    assert.equal(image.readUInt32BE(20), 1000, 'Unexpected screenshot height');
    const observation = JSON.parse(read(path.join(directory, `${name}.json`)));
    assert.equal(observation.name, name);
    assert.equal(observation.screenshot, `${name}.png`);
    for (const field of ['description', 'text']) {
      assert.equal(typeof observation[field], 'string');
      assert(observation[field].trim(), `Missing ${name} ${field}`);
    }
    observations.push(observation);
  }
  return { observations, hashes };
}

/**
 * Enforce an exact, prose-only schema rather than accepting generated files.
 * @param {object} proposal Parsed model response.
 * @returns {object} Validated response.
 */
export function validateProposal(proposal) {
  assert.deepEqual(Object.keys(proposal).sort(), [
    'config', 'plotting', 'reviewed_images', 'unverified',
  ]);
  assert.deepEqual(proposal.reviewed_images, names);
  for (const field of ['config', 'plotting', 'unverified']) {
    const values = proposal[field];
    assert(Array.isArray(values) && values.length <= 12);
    for (const value of values) {
      assert.equal(typeof value, 'string');
      assert(value.trim().length > 0 && value.length <= 2000);
      assert(!/[\u0000-\u001f\u007f]/u.test(value),
        'Paragraphs must be single-line plain text');
    }
  }
  return proposal;
}

/**
 * Preserve readable letters, numbers, and spaces; encode syntax and controls.
 * Knitr/Pandoc cannot interpret the prose as chunks, HTML, links, or images.
 * @param {string} value Untrusted prose.
 * @returns {string} Safe literal HTML paragraph.
 */
export function paragraph(value) {
  return `<p>${[...value].map(character =>
    /[\p{L}\p{N} ]/u.test(character) ? character :
      `&#${character.codePointAt(0)};`).join('')}</p>`;
}

/**
 * Replace only our unique managed section, preserving all surrounding bytes.
 * An empty proposal means preserve the existing document without any change.
 * @param {string} original Existing article.
 * @param {string[]} paragraphs Model's plain-text paragraphs.
 * @param {string[]} images Fixed, trusted screenshot names.
 * @returns {string} Constrained article.
 */
export function render(original, paragraphs, images) {
  const starts = original.split(start).length - 1;
  const ends = original.split(end).length - 1;
  assert(starts === ends && starts <= 1, 'Ambiguous managed section');
  assert(starts === 0 || original.indexOf(start) < original.indexOf(end));
  if (!paragraphs.length) return original;
  const section = [
    start, '', '## Browser walkthrough (documentation pilot)', '',
    ...paragraphs.map(paragraph), '',
    ...images.map(name =>
      `![Captured ${name} view](continuous-documentation/${name}.png)`),
    '', end,
  ].join('\n');
  if (starts) {
    return original.slice(0, original.indexOf(start)) + section +
      original.slice(original.indexOf(end) + end.length);
  }
  return `${original}${original.endsWith('\n') ? '' : '\n'}\n${section}\n`;
}

/**
 * Apply schema-checked prose and trusted capture images; never model file paths.
 * @param {string} directory Evidence directory.
 * @param {string} proposalFile Model JSON file.
 * @returns {object} Hashes of the exact allowed output files.
 */
export function apply(directory, proposalFile) {
  evidence(directory);
  const proposal = validateProposal(JSON.parse(read(proposalFile)));
  const outputs = {};
  for (const [section, filename] of Object.entries(documents)) {
    const images = section === 'config' ? ['config'] : ['dimplot', 'featureplot'];
    const original = read(filename).toString();
    const rendered = render(original, proposal[section], images);
    if (!proposal[section].length) continue;
    fs.writeFileSync(filename, rendered);
    outputs[filename] = hash(rendered);
    for (const name of images) {
      const target = `vignettes/continuous-documentation/${name}.png`;
      fs.mkdirSync(path.dirname(target), { recursive: true });
      const image = read(path.join(directory, `${name}.png`));
      fs.writeFileSync(target, image);
      outputs[target] = hash(image);
    }
  }
  return outputs;
}

/**
 * Link only snapshots present at the base or proposed commit.
 * @param {string} repository GitHub owner/repository.
 * @param {string} base Tested base commit.
 * @param {string} head Proposed commit.
 * @param {object} outputs Validated proposed files.
 * @param {string[]} beforeImages Snapshot paths present before applying prose.
 * @returns {string} Reviewer-facing before/after screenshot table.
 */
export function screenshotTable(repository, base, head, outputs, beforeImages) {
  const rows = [
    '| View | Before | After |',
    '| --- | --- | --- |',
  ];
  for (const name of ['config', 'dimplot', 'featureplot']) {
    const filename = `vignettes/continuous-documentation/${name}.png`;
    const link = (revision, label) =>
      `[${label}](https://github.com/${repository}/blob/${revision}/${filename})`;
    const existed = beforeImages.includes(filename);
    const before = existed ? link(base, 'Prior snapshot') :
      outputs[filename] ? 'New image / no prior pilot snapshot' :
        'No prior pilot snapshot';
    const after = outputs[filename] || existed ?
      link(head, 'New commit snapshot') :
      'Not added for this unchanged section; see capture artifact';
    rows.push(`| ${name} | ${before} | ${after} |`);
  }
  return rows.join('\n');
}

/**
 * Publish validated data via GitHub's Git database API, without a shell or build.
 * The commit's sole parent is the tested main SHA, never a newly resolved main.
 * @param {object} outputs Allowed paths and expected hashes.
 * @param {object} proposal Validated model response, for reviewer limitations.
 * @param {string[]} beforeImages Snapshot paths present on the tested base.
 * @returns {Promise<void>} Resolves when a draft PR exists or no change is needed.
 */
async function publish(outputs, proposal, beforeImages) {
  if (!Object.keys(outputs).length) {
    fs.appendFileSync(process.env.GITHUB_STEP_SUMMARY,
      'No documentation changes were proposed; no branch or PR created.\n');
    return;
  }
  const repository = process.env.GITHUB_REPOSITORY;
  const base = process.env.TESTED_SHA;
  assert(/^[a-f0-9]{40}$/.test(base));
  const request = async (route, body) => {
    const response = await fetch(
      `https://api.github.com/repos/${repository}/${route}`, {
        method: body ? 'POST' : 'GET',
        headers: {
          Authorization: 'Bearer ' + process.env.GITHUB_TOKEN,
          Accept: 'application/vnd.github+json',
          'Content-Type': 'application/json',
          'X-GitHub-Api-Version': '2022-11-28',
        },
        ...(body ? { body: JSON.stringify(body) } : {}),
      });
    assert(response.ok, `GitHub API ${route}: ${response.status}`);
    return response.json();
  };
  const commit = await request(`git/commits/${base}`);
  const tree = [];
  for (const [filename, expected] of Object.entries(outputs)) {
    const contents = read(filename);
    assert.equal(hash(contents), expected, 'Validated output changed');
    const blob = await request('git/blobs', {
      content: contents.toString('base64'), encoding: 'base64',
    });
    tree.push({ path: filename, mode: '100644', type: 'blob', sha: blob.sha });
  }
  const nextTree = await request('git/trees', {
    base_tree: commit.tree.sha, tree,
  });
  if (nextTree.sha === commit.tree.sha) {
    fs.appendFileSync(process.env.GITHUB_STEP_SUMMARY,
      'The proposed documentation is unchanged; no branch or PR created.\n');
    return;
  }
  const next = await request('git/commits', {
    message: 'docs: update evidence-backed browser walkthroughs\n\n' +
      'Co-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>',
    tree: nextTree.sha, parents: [base],
  });
  const branch = `continuous-documentation/${process.env.GITHUB_RUN_ID}-` +
    process.env.GITHUB_RUN_ATTEMPT;
  await request('git/refs', { ref: `refs/heads/${branch}`, sha: next.sha });
  const run = `https://github.com/${repository}/actions/runs/` +
    process.env.GITHUB_RUN_ID;
  const limitations = proposal.unverified.length ?
    proposal.unverified.map(paragraph).join('\n') : 'None reported by model.';
  const pull = await request('pulls', {
    title: 'docs: evidence-backed browser walkthrough pilot',
    head: branch, base: 'main', draft: true,
    body: [
      `Tested base: \`${base}\`. [Evidence and validation artifacts](${run}).`,
      `**Before:** [\`documentation-before\` artifact](${run}#artifacts) — ` +
        'original articles/rendered documentation and tested UI captures.',
      `**After:** [\`documentation-after\` artifact](${run}#artifacts) — ` +
        'revised articles/rendered documentation and proposed snapshots.',
      '## Before/after screenshots',
      screenshotTable(repository, base, next.sha, outputs, beforeImages),
      'Draft only; human review and normal merge protection remain required.',
      'Both base and proposal passed the full suite without skips, R CMD check',
      'and pkgdown rendering. Only managed prose and captured images changed.',
      'GITHUB_TOKEN-created PRs do not trigger normal pull_request workflows.',
      'The explicit validation above is not a substitute for required PR checks;',
      'a maintainer must trigger the normal checks before merging.',
      '## Unverified or unavailable behavior',
      limitations,
    ].join('\n\n'),
  });
  fs.appendFileSync(process.env.GITHUB_STEP_SUMMARY,
    `Created draft documentation PR: ${pull.html_url}\n`);
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const [command, directory, proposalFile] = process.argv.slice(2);
  if (command === 'verify') {
    const outputs = JSON.parse(
      read('test-results/pilot/validated-files.json'));
    for (const [filename, expected] of Object.entries(outputs)) {
      assert.equal(hash(read(filename)), expected,
        'Validation changed a proposed file');
    }
  } else if (command === 'capture') {
    const captured = evidence(directory);
    const manifest = {
      tested_sha: process.env.TESTED_SHA,
      viewport: { width: 1440, height: 1000 }, seed: 325,
      timezone: process.env.TZ,
      locale: process.env.LC_ALL,
      node: process.version,
      chrome: execFileSync(process.env.CHROMOTE_CHROME, ['--version'],
        { encoding: 'utf8' }).trim(),
      hashes: captured.hashes,
    };
    assert(/^[a-f0-9]{40}$/.test(manifest.tested_sha));
    fs.writeFileSync(path.join(directory, 'manifest.json'),
      JSON.stringify(manifest, null, 2));
  } else if (command === 'apply' || command === 'publish') {
    const manifest = JSON.parse(read(path.join(directory, 'manifest.json')));
    assert.equal(manifest.tested_sha, process.env.TESTED_SHA);
    assert.deepEqual(evidence(directory).hashes, manifest.hashes);
    const beforeImages = ['config', 'dimplot', 'featureplot']
      .map(name => `vignettes/continuous-documentation/${name}.png`)
      .filter(filename => fs.existsSync(filename));
    const outputs = apply(directory, proposalFile);
    if (command === 'apply') {
      fs.mkdirSync('test-results/pilot', { recursive: true });
      fs.writeFileSync('test-results/pilot/validated-files.json',
        JSON.stringify(outputs, null, 2));
    } else {
      assert.deepEqual(outputs,
        JSON.parse(read('test-results/validated/validated-files.json')));
      await publish(
        outputs, validateProposal(JSON.parse(read(proposalFile))), beforeImages);
    }
  } else {
    throw new Error('Expected capture, apply, verify, or publish');
  }
}
