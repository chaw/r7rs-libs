# Run tests using Kawa

kawa --r7rs robin-tests/abbrev-test.sps
kawa --r7rs robin-tests/text-test.sps

kawa --r7rs slib-tests/chapter-order-test.sps
kawa --r7rs slib-tests/diff-test.sps
kawa --r7rs slib-tests/format-test.sps
kawa --r7rs slib-tests/soundex-test.sps
kawa --r7rs slib-tests/tree-test.sps
# kawa --r7rs slib-tests/wt-tree-test.sps # CURRENTLY wt-tree NOT COMPILING