# Parsnip

Quickly make combine small parsers together.

Other parser combinator libraries I've found in the Common Lisp ecosystem is either extremely macro-heavy, or warns that it is not production-ready.
I don't trust third-party libraries without self-confidence, so I've made my own, going for a simple interface with a production-readiness goal.

## Road to Production-Readiness

The main bottleneck is figuring out an 80% stable API and calling it good.
Most everything else (quickstart documentation, benchmarking) can follow, but per-function docs and unit tests can be done on the side.

- [ ] The external API is stable, including primitive parsers and parser combinators
  - [x] All parsers are limited to a non-seeking stream with a 1-character peek buffer (outside `parse-try`)
  - [ ] Some robust way to figure out error handling and debugging.
  	The current failure mechanism makes a condition on error and passes it as a value.
	This might be good enough for error handling, but stack traces don't give enough information about what went wrong.
	Maybe failures could build up a return trace a la [Zig](https://ziglang.org/documentation/master/#Error-Return-Traces).
	I should program some example error reporting to showcase.
- [ ] Code tests
  - [ ] Every external function and macro is unit-tested.
  - [ ] 95% code coverage in `parsnip.lisp` (Tried using Coveralls, didn't work out very well. Maybe use [sb-cover](http://www.sbcl.org/manual/#sb_002dcover)?)
  - [ ] Benchmarks should have a reasonable speed.
        I don't plan for this library to be the fastest, but it shouldn't be snailing either.
	The current speed of the JSON example is about 7x slower than cl-json; I don't think this is acceptable.
	I think an obtainable goal is being twice as slow.
- [ ] Documentation
  - [ ] Code examples with real formats
    - [X] JSON
    - [ ] Minimal C-family grammar?
  - [ ] Docstrings in functions
  - [ ] Quickstart within the README
  - [ ] A full reference somewhere, maybe within the README
- [ ] A nice drawing of a parsnip :)

## Quickstart

TODO

## Examples

[The JSON example](./examples/json.lisp) matches extremely close to the grammar notation of the [RFC8259 JSON specification](https://datatracker.ietf.org/doc/html/rfc8259).
Outside of a couple outliers (the value grammar is moved to the end), the code is laid out nearly section-by-section as stated in the RFC.

TODO Symbolic Expressions
