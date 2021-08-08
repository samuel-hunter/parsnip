# Parsnip
[![builds.sr.ht status](https://builds.sr.ht/~shunter/parsnip/commits/test.yml.svg)](https://builds.sr.ht/~shunter/parsnip/commits/test.yml)

![Parsnip brand doodle](./brand.png)

Quickly combine small parsers together.

Other parser combinator libraries I've found in the Common Lisp ecosystem is either extremely macro-heavy, or warns that it is not production-ready.
I don't trust third-party libraries that don't trust themselves, so I've made my own, going for a simple interface with a production-readiness goal.

## Road to Production-Readiness

The general API is figured out -- it should change minimally through to 0.1.0.
Most everything else (quickstart documentation, benchmarking) can now follow.

- [ ] The external API is stable, including primitive parsers and parser combinators
  - [x] All parsers are limited to a non-seeking stream with a 1-character peek buffer (outside `parse-try`)
  - [x] Some robust way to figure out parser debugging.
  	I've decided to go for return traces during failures. It seems to work pretty well!
  - [x] Parselets for common idioms (like digits and numbers).
  - [x] Inestigate multi-stage parsers with generic streams.
        I know that many parsers work by having a lexing stage, and then a tree-building stage.
	I experimented with this with the JSON example to see if it made any improvement, and the speed slowed down from 2.5x to ~5x.
- [ ] Code tests
  - [x] Every external function is unit-tested.
  - [x] 95% code coverage in `parsnip.lisp` as reported by `sb-cover`.
        True as of commit `0b7a7173cd5b54799378a2b306035bc1feef13e3` - 95.7% coverage in expressions, and 100% coverage in branches
  - [ ] Benchmarks should have a reasonable speed.
        I don't plan for this library to be the fastest, but it shouldn't be snailing either.
	The current speed of the JSON example is about 2.5x slower than cl-json.
	This is close to my target of being only twice as slow.
- [x] Documentation
  - [x] Code examples with real formats
    - [X] JSON
    - [x] Some C-family programming language
  - [x] Docstrings in all external functions and macros
  - [x] Quickstart within the README
  - [x] A full reference somewhere, maybe within the README
- [ ] Peer review. I need more than myself looking at the project. Many eyes are welcome :)
- [ ] Time * Exposure.
- [x] A nice drawing of a parsnip :)

## Contributions

Any comments, questions, issues, or patches are greatly appreciated!
I do my main development on [Sourcehut](https://sr.ht/~shunter/parsnip/), with a [mailing list](https://lists.sr.ht/~shunter/public-inbox) and [issue tracker](https://todo.sr.ht/~shunter/parsnip).

## Quickstart

Install parsnip (and [alexandria](https://common-lisp.net/project/alexandria/) if you don't have Quicklisp) to your local projects:

```sh
$ cd ~/common-lisp/
$ git clone https://git.sr.ht/~shunter/parsnip
```

```lisp
(require :parsnip)
(use-package :parsnip)
```

Parsnip has a few parser primitives and many combinators to put them together:

```lisp
;; digit := [0-9]
(defparameter *digit*
  (predicate-parser #'digit-char-p))

(defparameter *digits*
  (parse-collect1 *digit*))

;; integer := digit+
(defparameter *integer*
  (parse-let ((digits *digits*))
    (parse-integer (coerce digits 'string))))

;; decimal-part := '.' integer
(defparameter *decimal-part*
  (parse-let ((digits (parse-progn (char-parser #\.)
                                   *digits*)))
    (/ (parse-integer (coerce digits 'string))
       (expt 10 (length digits)))))

;; number := integer [ decimal-part ]
(defparameter *number*
  (parse-let ((integer *integer*)
              (decimal (parse-optional *decimal-part* 0)))
    (+ integer decimal)))

(with-input-from-string (stream "123.47")
  (let ((number (parse *number* stream)))
    (print number))) ;; => 12347/100
```

You can use `defparser` to define a parser as a function, enabling forward-referencing for circular grammar:

```lisp
;; Not a complete parser:

;; list := '(' value* ')'
(defparameter *list*
  (parse-prog2 list-begin #'value list-end))

;; value := list | number | symbol | keyword
(defparser value
  (parse-or *list*
            *number*
	    *symbol*
	    *keyword*))
```

The [test suite](./test.lisp) shows how each function works, and how it's expected to perform.

## Examples

The [JSON example](./examples/json.lisp) matches extremely close to the grammar notation of the [RFC8259 JSON specification](https://datatracker.ietf.org/doc/html/rfc8259).
Outside of a couple outliers (the value grammar is moved to the end), the code is laid out nearly section-by-section as stated in the RFC.

The [Tiny C example](./examples/tiny-c.lisp) demonstrates an extremely stripped down version of C with no types and little control structures.
It is meant to show a very small yet turing-complete C-family language.

I plan to be writing a parser for [ABC notation v2.1](http://abcnotation.com/wiki/abc:standard:v2.1) after I feel reasonably finished with this project.

## Full Reference

The library provides parsers and parser combinators.
Parsers accept character input and return some value.
Parser combinators take in parsers and return other parsers with enhanced behavior, like parsing multiple times or returning a different result.

Parsers may fail, but unless it was partially parsed, some parser combinators like `parse-optional` or `parse-many` recover.

### Parselets

Parselets are parsers meant to be building blocks for greater parsers:

**char-parser** *char* - Return a parser that accepts the given character value.

**predicate-parser** *predicate* - Return a parser that accepts the character if, applied to the provided predicate, the predicate returns true.

**string-parser** *string* - Return a parser that accepts the given string value. May partially parse on failure.

**eof-parser** *value* - Return a parser that accepts the end of a file and returns the given value.

Non-primitive parselets include:

**digit-parser** *&optional (radix 10)* - Return a parser that accepts a digit and returns its number value.

**integer-parser** *&optional (radix 10)* - Return a parser that accepts a series of digits and returns its number value.

### Parser Combinators

Parser combinators take in one or more parsers and return a parser with enhanced behavior:

**parse-map** *function &rest parsers* - Compose multiple parsers to run in sequence, and apply the function to all parsers' values.

**parse-progn** *&optional parsers* - Compose multiple parsers to run in sequence, returning the last parser's value.

**parse-prog1** *&rest parsers* - Compose multiple parsers to run in sequence, returning the first parser's value.

**parse-prog2** *&rest parsers* - Compose multiple parsers to run in sequence, returning the second parser's value.

**parse-collect** *parser* - Enhance the parser to keep running and collect results until failure.

**parse-collect1** *parser* - Enhance the parser to keep running and collect at least one result until failure.

**parse-reduce** *function parser initial-value* - Enhance the parser to keep running and reduce all results into a single value until failure.

**parse-take** *times parser* - Enhance the parser to keep running and collect EXACTLY the given number of times.

**parse-or** *&rest parsers* - Attempts each given parser in order until one succeeds.

**parse-optional** *&rest parsers* - Enhance the parser to resume from a failure with a default value.

**parse-try** *parser* - Enhance the parser to try to rewind the stream on any partial-parse failure.
Only works on seekable streams, and is the only parser that can recover from partial-parse failures.

**parse-tag** *tag parser* - Enhance the parser's failures to report expecting the given tag instead of an element.

### Parser Macros

**parse-let** *bindings &body body* - Compose multiple parsers together to bind their results to variables and return a value within the body:

```lisp
(defparameter *id-parser*
  (parse-let ((letter (predicate-parser #'alpha-char-p))
              (digits (parse-collect1 (predicate-parser #'digit-char-p))))
    (make-instance 'identifier :letter letter
                               :number (parse-integer (coerce digits 'string)))))
```

**parse-defer** *form* - Return a parser that defers evaluating itself until it is called.

**defparser** *name () form* - Define a parser as a function:

```lisp
(defparser alpha-parser ()
  (predicate-parser #'alpha-char-p))

(defparser id-parser ()
  (parse-let ((letter #'alpha-parser)
              (digits (parse-collect1 (predicate-parser #'digit-char-p))))
    (make-instance 'identifier :letter letter
                               :number (parse-integer (coerce digits 'string)))))
```
