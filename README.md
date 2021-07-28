# Parsnip
[![builds.sr.ht status](https://builds.sr.ht/~shunter/parsnip/commits/test.yml.svg)](https://builds.sr.ht/~shunter/parsnip/commits/test.yml)

Quickly make combine small parsers together.

Other parser combinator libraries I've found in the Common Lisp ecosystem is either extremely macro-heavy, or warns that it is not production-ready.
I don't trust third-party libraries that don't trust themselves, so I've made my own, going for a simple interface with a production-readiness goal.

## Road to Production-Readiness

The general API is figured out -- it should change minimally through 0.1.0.
Most everything else (quickstart documentation, benchmarking) can now follow.

- [ ] The external API is stable, including primitive parsers and parser combinators
  - [x] All parsers are limited to a non-seeking stream with a 1-character peek buffer (outside `parse-try`)
  - [ ] Some robust way to figure out parser debugging.
  	The current failure mechanism makes a condition on error and passes it as a value.
	This might be good enough for error handling, but stack traces don't give enough information about what went wrong.
	Maybe failures could build up a return trace a la [Zig](https://ziglang.org/documentation/master/#Error-Return-Traces).
	I should program some example error reporting to showcase.
- [ ] Code tests
  - [ ] Every external function and macro is unit-tested.
        This is almost true. `defparser` and `parser-name` is the only two left.
  - [x] 95% code coverage in `parsnip.lisp` as reported by `sb-cover`.
        True as of commit `0b7a7173cd5b54799378a2b306035bc1feef13e3` - 95.7% coverage in expressions, and 100% coverage in branches
  - [ ] Benchmarks should have a reasonable speed.
        I don't plan for this library to be the fastest, but it shouldn't be snailing either.
	The current speed of the JSON example is about 2.25x slower than cl-json.
	This is very close to my target of being only twice as slow.
- [ ] Documentation
  - [ ] Code examples with real formats
    - [X] JSON
    - [ ] Some C-family programming language
    - [ ] Lay structure for `cl-abc2` project to link
  - [x] Docstrings in all external functions and macros
  - [x] Quickstart within the README
  - [ ] A full reference somewhere, maybe within the README
- [ ] Peer review. I need more than myself looking at the project. Many eyes are welcome :)
- [ ] A nice drawing of a parsnip :)

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
  (parse-any *list*
             *number*
	     *symbol*
	     *keyword*))
```

## Reference

The [test suite](./test.lisp) shows how each function works, and how it's expected to perform

## Examples

The [JSON example](./examples/json.lisp) matches extremely close to the grammar notation of the [RFC8259 JSON specification](https://datatracker.ietf.org/doc/html/rfc8259).
Outside of a couple outliers (the value grammar is moved to the end), the code is laid out nearly section-by-section as stated in the RFC.

I plan to be writing a parser for [ABC notation v2.1](http://abcnotation.com/wiki/abc:standard:v2.1) after I feel reasonabily finished with this project.

TODO I would like to demonstrate a C-family programming language being parsed in the future.
