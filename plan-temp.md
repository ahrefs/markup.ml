# Temporary plan: pull-based tokenizer for `markup.lite`

## Objective

Refactor only `src/lite/` so that the copied HTML tree builder consumes tokens synchronously from a resumable Ragel scanner. Avoid allocating a located pair and a stream node for every token. Keep the original parser under `src/` unchanged as the reference implementation.

The final hot path should approximately be:

```ocaml
string -> Ragel scanner -> next token -> HTML tree builder -> signal stream
```

The scanner receives reusable mutable location outputs and returns one token at a time. EOF remains an explicit nullary token.

## Correctness oracle

Run the full differential corpus after every meaningful step:

```bash
make test-lite
```

For quicker iteration:

```bash
make test-lite LITE_TEST_CORPUS=big_tests/readability
```

The test compares:

- complete signal lists structurally;
- complete error and location sequences structurally;
- exceptions when either implementation raises;
- accumulated wall time and minor-word allocation for each implementation.

Current `readability` bootstrap baseline:

```text
oracle: wall_seconds=0.821427 minor_words=348168668
lite:   wall_seconds=0.822675 minor_words=347995355
```

Treat timings as indicative rather than stable. Exact differential equivalence is mandatory.

## Invariants

- Do not modify `src/html_parser.ml` during this work.
- Keep `(line, -1)` Ragel locations exactly.
- Preserve the explicit `` `EOF `` token.
- Preserve current Ragel/crawler behavior, including malformed HTML behavior.
- Preserve decoded text and attribute values.
- Preserve script, style, title, and `<br/>` behavior.
- Preserve shared `Markup_common` signal, stream, error, and location identities.
- Keep direct compatibility with `Soup.from_signals`.
- Do not add a DOM or change the public signal representation.
- Keep each refactoring stage independently testable and preferably independently committed.
- Check generated Ragel OCaml into the repository.
- Retain upstream licensing notices when vendoring Ragel code.

## Target tokenizer interface

Start with a small private interface under `src/lite/`:

```ocaml
type location_out = {
  mutable line : int;
  mutable column : int;
}

type t

val create : string -> t

val next :
  t ->
  Html_tokenizer.state ->
  location_out ->
  Html_tokenizer.token
```

The parser allocates `location_out` once. Every successful `next` writes the token location into that mutable record and returns only the token.

The tokenizer state argument keeps the parser/tokenizer boundary suitable for `Data`, `RCDATA`, `RAWTEXT`, `Script_data`, and `PLAINTEXT`, even if the initial historical Ragel implementation ignores some of it.

Do not encode termination as `option`; `` `EOF `` is part of HTML parser processing and may be reprocessed parser-side.

## Step 0: establish a Lite benchmark baseline

The differential executable already records wall time and minor allocations. Record full-corpus release numbers before changing the implementation:

```bash
make test-lite
```

Optionally add a Lite-only throughput benchmark if differential execution obscures performance due to retaining both result lists. The benchmark must fully drain the signal stream.

## Step 1: introduce a list-backed pull source

Add the target pull interface without changing Ragel yet.

Implement it over the current eager result of:

```ocaml
Ragel_html_tokenizer.tokenize :
  string -> (location * token) list
```

The adapter should:

- retain the unconsumed list;
- copy line and column into `location_out`;
- return the token;
- return `` `EOF `` according to a documented post-EOF contract.

This step intentionally does not improve allocation. It isolates parser-interface changes from scanner-generation changes.

Run `readability`, then all `big_tests`.

## Step 2: change the Lite parser boundary to pull tokens

Change only `src/lite/html_parser.ml` and its private interface. Replace:

```ocaml
(tokens, set_tokenizer_state, set_foreign)
```

with the pull token source.

The parser should own:

- the current tokenizer state;
- the reusable mutable location record;
- parser-side pushed tokens.

Initially it is acceptable to materialize a `(line, column)` pair immediately after pulling each token so that the rest of the parser remains almost unchanged. This is an architectural checkpoint, not an allocation optimization.

Likely edit points:

- `parse` entry point;
- `Context.initialize` and context detection;
- `dispatch`;
- tokenizer-state transitions;
- token pushback and reprocessing;
- foreign-content tokenizer context, if still needed.

`Markup_lite.parse_html` always supplies an explicit document or fragment context, so automatic context checkpoint/replay is not on its normal path. Preserve it only if the private parser interface still needs `None` contexts.

### Pushback

Normal tokens should not be wrapped. Reprocessed or transformed tokens may use:

```ocaml
type pushed_token = {
  token : Html_tokenizer.token;
  line : int;
  column : int;
}
```

A small list is sufficient initially. Pushback allocation is acceptable because it is not the per-token hot path.

Run `readability`, then all `big_tests`.

## Step 3: vendor the Ragel scanner

Copy the historical `.rl` source into `src/lite/`, retaining its licensing notice. Generate and check in the corresponding `.ml`.

Do not initially redesign token semantics, entity decoding, or malformed-input behavior. First reproduce the current test-local oracle exactly.

Add focused scanner tests for:

- ordinary text and tags;
- text and attribute entities;
- `<br/>`;
- script, style, and title bodies;
- malformed tags represented in the corpus;
- line tracking;
- EOF;
- multiple calls after EOF according to the selected contract.

## Step 4: make Ragel execution resumable

Move the generated machine state from local variables into the scanner value. Expected persistent state includes:

- `cs`;
- `p`, `pe`, and `eof`;
- mark and mark-end offsets;
- current tag, key, attributes, and directive;
- line number;
- pending tokens or events.

Each `next` resumes the machine and stops when one token is available.

Be careful when pausing inside Ragel actions. Some historical actions logically emit several events:

- self-closing tags;
- script bodies;
- style bodies;
- title bodies.

Use a tiny internal pending queue where necessary. Externally, `next` still returns exactly one HTML token. Ensure the scanner pauses only after an entire semantic action has populated that queue, rather than throwing out of an action before all outputs are recorded.

The scanner should reference the input string and offsets directly. Avoid constructing the complete token list.

Run focused scanner tests, `readability`, and all `big_tests`.

## Step 5: remove the Devkit production dependency

Once the vendored scanner is equivalent, stop using `Devkit.HtmlStream` from `markup.lite` production code.

Replace `Devkit.Web.htmldecode` with decoding based on `markup.entities`, preserving exact observed behavior. Keep Devkit only in differential tests as the independent oracle.

Run all tests before and after removing the dependency.

## Step 6: eliminate normal token location-pair allocation

After the pull scanner is stable, change parser dispatch from located-token matching:

```ocaml
| location, token -> ...
```

to token-only matching using the current mutable line and column.

Create a real shared location pair only when required by:

- an error report;
- a located output signal;
- retained element/recovery metadata;
- parser pushback;
- a public location observation.

This is a broad mechanical change because locations are threaded throughout `html_parser.ml`; keep it separate from Ragel work.

Where practical, change internal helpers from a location pair to split integer arguments or accessors. Do not change `Markup_common.location` publicly.

Run the full differential corpus frequently during this conversion.

## Step 7: remove token `Kstream` overhead

Ensure the parser no longer creates or consumes a `Kstream` node per input token. The output may remain the shared signal stream representation.

Keep synchronous control flow direct in the input hot path. Measure whether parser CPS continuations remain material after input/token overhead has been removed before redesigning output streaming.

## Step 8: measure and profile

For every major checkpoint, record:

- full-corpus wall time;
- minor words;
- bytes and files processed;
- signals or `<a>` count as a drain sanity check.

Then profile the Lite-only corpus executable with:

```bash
perf record --call-graph=dwarf -- <lite benchmark> big_tests/llm_tracker
perf report -i perf.data
```

Expected initial improvements:

- removal of the eager token list;
- removal of token-stream continuation/node allocation;
- removal of normal located-token pair allocation;
- disappearance of generic Uutf/input/encoding overhead from the Lite path.

Only after measuring the new profile should work begin on smaller element frames, lazy recovery records, subtree buffering, or parser CPS removal.

## Suggested commit sequence

1. Add private pull-source interface with list-backed implementation.
2. Move the Lite parser boundary to that source; no semantic change.
3. Vendor unchanged Ragel source and generated output.
4. Add direct scanner differential tests.
5. Make the scanner resumable and remove the eager token list.
6. Remove Devkit from the production Lite library.
7. Convert parser location handling to reusable mutable outputs.
8. Remove remaining token-side `Kstream` machinery.
9. Record benchmark and profile results.

Every commit should pass at least the `readability` differential test. Every completed stage should pass all 840 files under `big_tests`.
