# Parsnip

Quick and unopinionated parser combinators.

Other parser combinator libraries I've found in the Common Lisp ecosystem is either extremely macro-heavy, or warns that it is not production-ready.
I don't trust third-party libraries without self-confidence, so I've made my own, going for a simple interface with a production-readiness goal.

## Goal to Production-Readiness

- [ ] All primitive parsers and non-macro parser combinators are complete, as far as external behavior is concerned
  - [ ] All parsers are limited to a non-seeking stream with a 1-character peek buffer (outside `parser-try`)
  - [ ] All primitive parsers and non-macro parser combinators are unit-tested
- [ ] The parser combinator has reasonable speed (TODO: how do I measure speed as "reasonable"? How would I benchmark?)
- [ ] 95% test code coverage (Coveralls, perhaps?)
- [ ] I sat on it for some time and I didn't feel tempted to change the API.
- [ ] The system is clearly documented within the code as docstrings, within the README as a quickstart, and within the repository as examples

## Quickstart

TODO

## Examples

- [ ] TODO JSON
- [ ] TODO Symbolic Expressions
