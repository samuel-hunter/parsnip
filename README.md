# Parsnip

Quick and easy file parsers.

Other parser combinator libraries I've found in the Common Lisp ecosystem is either extremely macro-heavy, or warns that it is not production-ready.
I don't trust third-party libraries without self-confidence, so I've made my own, going for a simple interface with a production-readiness goal.

## Road to Production-Readiness

- [ ] The external API is stable, including primitive parsers and parser combinators
  - [ ] All parsers are limited to a non-seeking stream with a 1-character peek buffer (outside `parser-try`)
- [ ] Code tests
  - [ ] All primitive parsers and non-macro parser combinators are unit-tested
  - [ ] 95% code coverage (use Coveralls, perhaps?)
  - [ ] Benchmarks should have a reasonable speed:
        I'm not targeting speed in this library, but it shouldn't be snailing either (TODO: How do I measure speed as "reasonable"?)
- [ ] Documentation
  - [ ] Code examples with real formats
  - [ ] Docstrings in functions
  - [ ] Quickstart within the README
  - [ ] A full reference somewhere, maybe within the README

## Quickstart

TODO

## Examples

[The JSON example](./examples/json.lisp) matches extremely close to the grammar notation of the [RFC8259 JSON specification](https://datatracker.ietf.org/doc/html/rfc8259).
Outside of a couple outliers (the value grammar is moved to the end), the code is laid out nearly section-by-section as stated in the RFC.

TODO Symbolic Expressions
