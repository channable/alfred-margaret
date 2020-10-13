# Changelog

## v1.1.0.0 - "Moore Features" (2020-10-13)

The most notable addition in this release is the implementation of the Boyer-Moore string search algorithm.

**Compatibility:**

- Extracted the UTF-16 manipulation functions from `Data.Text.AhoCorasick.Automaton` into `Data.Text.Utf16`
- Changed `Data.Text.AhoCorasick.Searcher.Searcher` to remember the case sensitivity used for constructing the searcher
- Removed `Data.Text.AhoCorasick.Searcher.containsAnyIgnoreCase`, the correct implementation is now chosen by `containsAny` based on the case sensitivity of the searcher

Other changes:

- Added `Data.Text.AhoCorasick.Splitter` for splitting a lot of text using the same needle
- Added `Data.Text.BoyerMoore.Automaton`, a UTF-16 implementation of Boyer-Moore
- Added `Data.Text.BoyerMoore.Searcher` for searching for multiple needles at once using Boyer-Moore
- Added `Data.Text.BoyerMoore.Replacer` for replacing text based on the Boyer-Moore search
- Added optional `FromJSON`/`ToJSON` instances for most types (can be toggled via `aeson` cabal flag)

## v1.0.0.0 - "Initial Release" (2019-03-19)

This is the initial open-source release.

- Added `Data.Text.AhoCorasick.Automaton`, a UTF-16 implementation of the Aho-Corasick search algorithm
- Added `Data.Text.AhoCorasick.Searcher`, a bulk search abstraction based on Aho-Corasick
- Added `Data.Text.AhoCorasick.Replacer`, a bulk replace abstraction based on Aho-Corasick
