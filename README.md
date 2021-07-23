# Parsnip
[![Build Status](https://travis-ci.com/samuel-hunter/parsnip.svg?branch=master)](https://travis-ci.com/samuel-hunter/parsnip)
[![Coverage Status](https://coveralls.io/repos/github/samuel-hunter/parsnip/badge.svg?branch=master)](https://coveralls.io/github/samuel-hunter/parsnip?branch=master)

Quick and easy file parsers.

Other parser combinator libraries I've found in the Common Lisp ecosystem is either extremely macro-heavy, or warns that it is not production-ready.
I don't trust third-party libraries without self-confidence, so I've made my own, going for a simple interface with a production-readiness goal.

## Road to Production-Readiness

- [ ] The external API is stable, including primitive parsers and parser combinators
  - [x] All parsers are limited to a non-seeking stream with a 1-character peek buffer (outside `parse-try`)
  - [ ] Some robust way to figure out error handling.
        An ideal error grabs everything at start, and signals it off to a dispatcher.
	The current system in place might be good already.
	I should program some example error reporting to showcase.
- [ ] Code tests
  - [ ] Every function is unit-tested.
  - [ ] 95% code coverage in `parsnip.lisp` (Tried using Coveralls, didn't work out very well. Maybe sb-cover?)
  - [ ] Benchmarks should have a reasonable speed:
        I'm not targeting speed in this library, but it shouldn't be snailing either. (TODO: Maybe compare speed of JSON parser with cl-json?)
- [ ] Documentation
  - [ ] Code examples with real formats
    - [X] JSON
    - [ ] Symbolic expressions
    - [ ] Minimal C-family grammar?
  - [ ] Docstrings in functions
  - [ ] Quickstart within the README
  - [ ] A full reference somewhere, maybe within the README

## Quickstart

TODO

## Examples

[The JSON example](./examples/json.lisp) matches extremely close to the grammar notation of the [RFC8259 JSON specification](https://datatracker.ietf.org/doc/html/rfc8259).
Outside of a couple outliers (the value grammar is moved to the end), the code is laid out nearly section-by-section as stated in the RFC.

TODO Symbolic Expressions
