# Parsnip
[![builds.sr.ht status](https://builds.sr.ht/~shunter/parsnip/commits/test.yml.svg)](https://builds.sr.ht/~shunter/parsnip/commits/test.yml)

![Parsnip brand doodle](./brand.png)

Quickly combine small parsers together.

Other parser combinator libraries I've found in the Common Lisp ecosystem are either too macro-heavy for my tastes, or warn that they are not production-ready.
I don't trust third-party libraries that don't trust themselves, and so I've made my own, going for a simple interface with a production-readiness goal.

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

## Examples

The [JSON example](./examples/json.lisp) matches close to the grammar notation of the [RFC8259 JSON specification](https://datatracker.ietf.org/doc/html/rfc8259).
Outside of a couple outliers (e.g. the value definition is moved to the end), the code is laid out nearly section-by-section as stated in the RFC.

The [Tiny C example](./examples/tiny-c.lisp) demonstrates a stripped down version of C with no types, no preprocessing, and only an `if` control structure.
It is meant to show a very small yet turing-complete C-family language.

I plan to be writing a parser for [ABC notation v2.1](http://abcnotation.com/wiki/abc:standard:v2.1) after I feel reasonably finished with this project.

## Parser Reference

The library provides parsers, ways to combine them, and a `parser-error`.
Parsers accept character input and return some value.
Parser combinators take in parsers and return other parsers with enhanced behavior, like parsing multiple times or returning a different result.

Parsers may fail, but unless it was partially parsed, some parser combinators like `parse-optional` or `parse-many` recover.

### Parselets

Parselets are parsers meant to be building blocks for greater parsers:

**char-parser** *char* - Consume and return the given character.

**predicate-parser** *predicate* - Consume and return a character that passes the given predicate.

**string-parser** *string* - Consume and return the given text. May partially parse on failure.

**eof-parser** *&optional value* - Return the given value (or NIL) if at EOF.

Non-primitive parselets include:

**digit-parser** *&optional (radix 10)* - Consume a single digit and return its integer value.

**integer-parser** *&optional (radix 10)* - Consume one or more digits and return its integer value.

### Parser Combinators

Parser combinators take in one or more parsers and return a parser with enhanced behavior:

**parse-map** *function &rest parsers* - Run the parsers in sequence and apply the given function to all results.

**parse-progn** *&optional parsers* - Run the parsers in sequence and return the last result.

**parse-prog1** first-parser *&rest parsers* - Run the parsers in sequence and return the first result

**parse-prog2** *&rest parsers* - Run the parsers in sequence and return the second result.

**parse-collect** *parser* - Run until failure, and then return the collected results.

**parse-collect1** *parser* - Run until failure, and then return at LEAST one collected result.

**parse-collect-string** *parser* - RUn until failure, and then return the collected characters as a string.

**parse-reduce** *function parser initial-value* - Run until failure, and then reduce the results into one value.

**parse-take** *times parser* - Run and collect EXACTLY the given number of results.

**parse-or** *&rest parsers* - Attempt each given parser in order until one succeeds.

**parse-optional** *&rest parsers* - Resume from a failure with a default value.

**parse-try** *parser* - Try to rewind the stream on any partial-parse failure.
Only works on seekable streams, and is the only parser that can recover from partial-parse failures.

**parse-tag** *tag parser* - Report fails as expecting the given tag instead of an element.

### Parser Macros

**parse-let** *bindings &body body* - Compose multiple parsers together to bind their results to variables and return a value within the body:

```lisp
(defparameter *id-parser*
  (parse-let ((letter (predicate-parser #'alpha-char-p))
              (digits (parse-collect1 (predicate-parser #'digit-char-p))))
    (make-instance 'identifier :letter letter
                               :number (parse-integer (coerce digits 'string)))))
```

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

### `parser-error`

If a parser fails to read text, it signals a `parser-error`, a subclass of `stream-error`, with these readers:

**stream-error-stream** - Part of the CL standard, returns the stream the parser was reading from.

**parser-error-expected** - Return the value that the parser error expected to read. May be overridden with `parser-tag`.

**parser-error-return-trace** - Every parser defined with `defparser` adds its symbol to the return trace as the error bubbles up.
The return trace, along with `(file-position stream)`, should assist developers debug their parsers
