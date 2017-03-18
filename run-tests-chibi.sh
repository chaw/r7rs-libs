# Run tests using Chibi Scheme

chibi-scheme -I srfis/chibi robin-tests/abbrev-test.sps
chibi-scheme -I srfis/chibi robin-tests/text-test.sps

chibi-scheme -I srfis/chibi slib-tests/chapter-order-test.sps
chibi-scheme -I srfis/chibi slib-tests/diff-test.sps
chibi-scheme -I srfis/chibi slib-tests/format-test.sps
chibi-scheme -I srfis/chibi slib-tests/soundex-test.sps
chibi-scheme -I srfis/chibi slib-tests/tree-test.sps
chibi-scheme -I srfis/chibi slib-tests/wt-tree-test.sps
