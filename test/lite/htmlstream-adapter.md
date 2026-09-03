# HtmlStream parser adapter

`Oracle.adapt` in `oracle.ml` is a test-only adapter. It parses each byte string
once with `Devkit.HtmlStream` and produces one token list. The strict corpus
comparison and strict AFL harness derive both the baseline and Lite parser
inputs from that list.

## Common policy

The adapter applies the same omissions and defaults to both parsers:

- locations are `(HtmlStream line, -1)`, because HtmlStream does not expose a
  useful common column;
- start tags default `self_closing` to `false`;
- comments are omitted because HtmlStream does not expose them;
- `Script` and `Style` values become a start tag, one `String` token, and an end
  tag;
- other text remains in `String` runs;
- attributes are kept in HtmlStream's source order and their values are decoded
  once;
- HtmlStream's synthetic `Close "br"` after a `br` tag is omitted;
- one explicit `EOF` token is appended.

HtmlStream has no fragment tokenizer mode. Fragment context is therefore
applied only when the common token list is passed to each parser. Depth limits
are likewise parser settings and do not affect adaptation.

## Suitability

This adapter is suitable for differential parser testing because both parsers
receive the same token values and the same missing information. It is not a
production tokenizer adapter: it loses comments, self-closing syntax, columns,
raw-text boundary details, tokenizer reports, and tree-builder feedback. Devkit
therefore remains a test/fuzz dependency and is not linked into `markup.lite`.
