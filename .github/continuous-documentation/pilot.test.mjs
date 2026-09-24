import test from 'node:test';
import assert from 'node:assert/strict';
import { names, paragraph, render, validateProposal } from './pilot.mjs';
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
  assert.match(encoded, /^<p>(?:&#\d+;)+<\/p>$/);
  assert(!encoded.includes('https:'));
  assert(!encoded.includes('`'));
  const decoded = encoded.slice(3, -4).replace(/&#(\d+);/g,
    (_, value) => String.fromCodePoint(Number(value)));
  assert.equal(decoded, hostile);
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
    { type: 'session.shutdown', data: {
      shutdownType: 'routine', modelMetrics: { 'gpt-6-astra': {} },
      codeChanges: { filesModified: [] },
    } },
  ];
  assert.deepEqual(auditResponse(events, 'gpt-6-astra'), valid());
  assert.throws(() => auditResponse(events.slice(0, 1), 'gpt-6-astra'));
  assert.throws(() => auditResponse(events, 'gpt-5.4'));
  assert.throws(() => auditResponse([
    ...events, { type: 'tool.execution_start', data: {} },
  ], 'gpt-6-astra'));
  assert.throws(() => auditResponse([
    ...events,
    { type: 'session.model_change', data: { newModel: 'gpt-5.4' } },
  ], 'gpt-6-astra'));
  const fallback = structuredClone(events);
  fallback[1].data.modelMetrics['gpt-5.4'] = {};
  assert.throws(() => auditResponse(fallback, 'gpt-6-astra'));
});
