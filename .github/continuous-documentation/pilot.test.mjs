import test, { mock } from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import {
  apply, documents, evidence, names, paragraph, read, render, validateProposal,
  screenshotTable,
} from './pilot.mjs';
import { auditResponse, selectModel } from './model.mjs';

const valid = () => ({
  config: ['Use the configuration preview.'],
  plotting: ['Select the displayed reduction.'],
  reviewed_images: [...names],
  unverified: [],
});

test('requires exact schema and all reviewed images', () => {
  assert.deepEqual(validateProposal(valid()), valid());
  assert.throws(() => validateProposal({ ...valid(), command: 'run something' }));
  assert.throws(() => validateProposal({ ...valid(), reviewed_images: [] }));
  assert.throws(() => validateProposal({ ...valid(), config: 'not an array' }));
  assert.throws(() => validateProposal({ ...valid(), config: [''] }));
  assert.throws(() => validateProposal({ ...valid(), config: ['a\nb'] }));
  assert.throws(() => validateProposal({ ...valid(), plotting: ['x'.repeat(2001)] }));
});

test('model prose cannot create executable chunks, HTML, or remote images', () => {
  const hostile = '`r system("bad")` ```{r} <script>alert(1)</script> ' +
    '![image](https://example.org/a.png) &copy;';
  const encoded = paragraph(hostile);
  assert.match(encoded, /^<p>(?:[\p{L}\p{N} ]|&#\d+;)+<\/p>$/u);
  assert(!encoded.includes('https:'));
  assert(!encoded.includes('`'));
  const decoded = encoded.slice(3, -4).replace(/&#(\d+);/g,
    (_, value) => String.fromCodePoint(Number(value)));
  assert.equal(decoded, hostile);
  assert.equal(paragraph('Configure naïve CD4 cells — λ2'),
    '<p>Configure naïve CD4 cells &#8212; λ2</p>');
});

test('only the managed section changes and rendering is idempotent', () => {
  const original = '---\ntitle: Existing\n---\n\nOriginal correct prose.\n';
  const first = render(original, valid().config, ['config']);
  assert(first.startsWith(original));
  assert.equal(render(first, valid().config, ['config']), first);
  const surrounded = first + '\nKeep this tail byte-for-byte.\n';
  const second = render(surrounded, ['Updated prose.'], ['config']);
  assert(second.startsWith(original));
  assert(second.endsWith('\nKeep this tail byte-for-byte.\n'));
  assert.equal(render(second, [], ['config']), second);
});

test('ambiguous or malformed managed sections fail closed', () => {
  assert.throws(() => render('<!-- continuous-documentation: start -->',
    ['paragraph'], ['config']));
  assert.throws(() => render(
    '<!-- continuous-documentation: end -->' +
    '<!-- continuous-documentation: start -->', ['paragraph'], ['config']));
});

test('only an explicitly enabled, image-capable Astra model is accepted', () => {
  const model = {
    id: 'gpt-6-astra',
    policy: { state: 'enabled' },
    capabilities: { supports: { vision: true } },
  };
  assert.equal(selectModel([model], model.id), model);
  assert.throws(() => selectModel([], model.id));
  assert.throws(() => selectModel([model], 'gpt-5.4'));
  assert.throws(() => selectModel([{ ...model,
    policy: { state: 'disabled' } }], model.id));
  assert.throws(() => selectModel([{ ...model,
    capabilities: { supports: { vision: false } } }], model.id));
});

test('model responses require exact model accounting and no tool execution', () => {
  const events = [
    { type: 'assistant.message', data: {
      model: 'gpt-6-astra', content: JSON.stringify(valid()),
    } },
    { type: 'assistant.usage', data: { model: 'gpt-6-astra' } },
    { type: 'session.idle', data: {} },
  ];
  assert.deepEqual(auditResponse(events, 'gpt-6-astra'), valid());
  const withoutOptionalModel = structuredClone(events);
  delete withoutOptionalModel[0].data.model;
  assert.deepEqual(auditResponse(withoutOptionalModel, 'gpt-6-astra'), valid());
  assert.throws(() => auditResponse(
    events.filter(event => event.type !== 'assistant.usage'), 'gpt-6-astra'));
  assert.throws(() => auditResponse(events.slice(0, 1), 'gpt-6-astra'));
  assert.throws(() => auditResponse(events, 'gpt-5.4'));
  assert.throws(() => auditResponse([
    ...events, { type: 'tool.execution_start', data: {} },
  ], 'gpt-6-astra'));
  assert.throws(() => auditResponse([
    ...events,
    { type: 'session.model_change', data: { newModel: 'gpt-5.4' } },
  ], 'gpt-6-astra'));
  assert.throws(() => auditResponse([
    ...events, { type: 'assistant.usage', data: { model: 'gpt-5.4' } },
  ], 'gpt-6-astra'));
  for (const type of ['session.warning', 'model.call_failure']) {
    assert.throws(() => auditResponse([
      ...events, { type, data: { message: 'Retrying without images' } },
    ], 'gpt-6-astra'));
  }
});

test('application is a no-op or writes only fixed docs and captured images', () => {
  // An in-memory contract fixture tests boundaries, not browser rendering.
  const files = new Map();
  const writes = [];
  for (const name of names) {
    const header = Buffer.alloc(24);
    Buffer.from('89504e470d0a1a0a', 'hex').copy(header);
    header.writeUInt32BE(1440, 16);
    header.writeUInt32BE(1000, 20);
    files.set(`evidence/${name}.png`, header);
    files.set(`evidence/${name}.json`, Buffer.from(JSON.stringify({
      name, description: 'Fixture observation', text: 'Fixture visible text',
      screenshot: `${name}.png`,
    })));
    files.set(`evidence/${name}-logs.csv`, Buffer.from('level,message\n'));
  }
  for (const filename of Object.values(documents)) {
    files.set(filename, Buffer.from('Correct existing prose.\n'));
  }
  files.set('proposal.json', Buffer.from(JSON.stringify({
    ...valid(), config: [], plotting: [],
  })));
  mock.method(fs, 'lstatSync', filename => {
    assert(files.has(filename), `Missing fixture ${filename}`);
    return { isFile: () => true, size: files.get(filename).length };
  });
  mock.method(fs, 'readFileSync', filename => files.get(filename));
  mock.method(fs, 'mkdirSync', () => undefined);
  mock.method(fs, 'writeFileSync', (filename, contents) => {
    writes.push(filename);
    files.set(filename, Buffer.from(contents));
  });
  try {
    assert.deepEqual(apply('evidence', 'proposal.json'), {});
    assert.deepEqual(writes, []);
    files.set('proposal.json', Buffer.from(JSON.stringify({
      ...valid(), config: ['`r system("not executed")` <img src="https://bad">'],
      plotting: [],
    })));
    const outputs = apply('evidence', 'proposal.json');
    assert.deepEqual(Object.keys(outputs), [
      documents.config, 'vignettes/continuous-documentation/config.png',
    ]);
    assert.deepEqual(writes, Object.keys(outputs));
    assert.equal(files.get(documents.plotting).toString(),
      'Correct existing prose.\n');
    assert(!files.get(documents.config).toString().includes('`r'));
    assert(!files.get(documents.config).toString().includes('https://bad'));
    assert.deepEqual(files.get('vignettes/continuous-documentation/config.png'),
      files.get('evidence/config.png'));
    files.set('proposal.json', Buffer.from(JSON.stringify({
      ...valid(), config: [],
    })));
    writes.length = 0;
    const plotting = apply('evidence', 'proposal.json');
    assert.deepEqual(Object.keys(plotting), [
      documents.plotting,
      'vignettes/continuous-documentation/dimplot.png',
      'vignettes/continuous-documentation/featureplot.png',
    ]);
    assert.deepEqual(writes, Object.keys(plotting));
    assert(!files.get(documents.plotting).toString().includes('subset.png'));
    assert(!files.get(documents.plotting).toString().includes('dge.png'));
    files.get('evidence/config.png').writeUInt32BE(1, 16);
    assert.throws(() => evidence('evidence'), /screenshot width/);
    files.delete('evidence/config.png');
    assert.throws(() => evidence('evidence'), /Missing fixture/);
  } finally {
    mock.restoreAll();
  }
});

test('artifact reader refuses symlinks and oversized files', () => {
  try {
    mock.method(fs, 'lstatSync', () => ({
      isFile: () => false, size: 20,
    }));
    assert.throws(() => read('symlink'), /Unsafe file/);
    mock.restoreAll();
    mock.method(fs, 'lstatSync', () => ({
      isFile: () => true, size: 20_000_001,
    }));
    assert.throws(() => read('oversized'), /Unsafe file/);
  } finally {
    mock.restoreAll();
  }
});

test('PR screenshot links use exact commits without linking missing images', () => {
  const filename = 'vignettes/continuous-documentation/config.png';
  const first = screenshotTable('owner/repository', 'base', 'head',
    { [filename]: 'hash' }, []);
  assert(first.includes(`/blob/head/${filename}`));
  assert(first.includes('New image / no prior pilot snapshot'));
  assert(!first.includes('/blob/base/'));
  assert(!first.includes('/blob/head/vignettes/continuous-documentation/dge.png'));
  assert(!first.includes('/blob/head/vignettes/continuous-documentation/dimplot.png'));
  const later = screenshotTable('owner/repository', 'base', 'head',
    { [filename]: 'hash' }, [filename]);
  assert(later.includes(`/blob/base/${filename}`));
  assert(later.includes(`/blob/head/${filename}`));
});
