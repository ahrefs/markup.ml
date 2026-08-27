# Frozen mainline HTML tokenizer reference

The tokenizer was copied from commit
`b0ed9fdf07b6418cb9b11f92fb38a8cf70059667`.

SHA-256 hashes of the copied source files:

```text
fdcac0a07b69e4131dd15a7dc2651dbb6aa55cd086f737b62bc2b9ec41f9f11b  html_tokenizer.ml
cfdcc1c967f7b876a90157d5416a776a2771786711fc0f9b004261dc2428b171  html_tokenizer.mli
```

This private Dune library is an immutable test oracle. Do not make semantic
changes here. Any intentional tokenizer behavior change must be made to both
implementations only after compatibility work is complete.
