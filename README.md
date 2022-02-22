# Parsnip
[![builds.sr.ht status](https://builds.sr.ht/~shunter/parsnip/commits/test.yml.svg)](https://builds.sr.ht/~shunter/parsnip/commits/test.yml)

![The library brand image: a crudely drawn parsnip surrounded by parentheses](./brand.png)

Monadic parser combinator library.

Conventional parsers are modeled as a two-stage process: a scanner that takes characters and produce tokens, and a parser that takes tokens and produce syntax trees.
Monadic parsers instead model parsers as smaller parsers that can compose together, much like the procedures of a conventional program.

Other parser combinator libraries I've found for Common Lisp are either too macro-heavy for me, or warn that they are not production-ready.
I don't trust third-party libraries that don't trust themselves, and so I've made my own, going for a simple interface targeted for public consumption.

Parsnip targets user-facing compilers, interpreters, or other readers of character-based languages, where end-users would like to know information when input fails to parse.
Parsnip does **not** target performance-intensive or byte-based decoders, such as those used in network stacks, or JSON/XML decoders for request data for web applications.

## Contributions

Any comments, questions, issues, or patches are greatly appreciated!
I do my main development on [Sourcehut](https://sr.ht/~shunter/parsnip/), with a [mailing list](https://lists.sr.ht/~shunter/public-inbox) and [issue tracker](https://todo.sr.ht/~shunter/parsnip).

## Usage

Parsnip is available on Quicklisp:
```lisp
(ql:quickload :parsnip)
```

Parsnip can also be loaded remotely.
Make sure to also install the sole dependency Alexandria:

```sh
$ cd ~/common-lisp/ # Or wherever you store your definitions
$ git clone https://git.sr.ht/~shunter/parsnip
```

```lisp
(require :parsnip)
(use-package :parsnip)

;; digit := [0-9]
(defparser one-digit ()
  (char-if #'digit-char-p))

(defparser digits ()
  (collect1 'one-digit))

;; whole-part := digit+
(defparser whole-part ()
  (let! ((digits 'digits))
    (ok (parse-integer (coerce digits 'string)))))

;; decimal-part := '.' [0-9]+
(defparser decimal-part ()
  (let! ((digits (progn! (char-of #\.) 'digits)))
    (ok (/ (parse-integer (coerce digits 'string))
           (expt 10 (length digits))))))

;; number := whole-part [ decimal-part ]
(defparser decimal-number ()
  (let! ((whole-value 'whole-part)
         (decimal-value (or! 'decimal-part (ok 0))))
    (ok (+ whole-value decimal-value))))

(defun parse-from-string (parser string)
  (with-input-from-string (stream string)
    (parse parser stream)))

(parse-from-string 'decimal-number "123.47") ;; => 12347/100
```

Parsnip aims to provide rich information for parsers aimed at end-users:

```
(use-package :xyz.shunter.parsnip.examples.json)

;; bad.json: [10,20,,]
(with-open-file (s "/tmp/bad.json")
  (decode-json s))
/tmp/bad.json:1:7: Expected (#\f #\n #\t #\{ #\[
                             (:integer . 10) #\") on #<STREAM>
[Condition of type PARSER-ERROR]

(with-open-file (s "/tmp/bad.json")
  (handler-case (decode-json s)
    (parser-error (c)
      (values (parser-error-line c)
              (parser-error-column c)))))
1
7

(handler-case (decode-json-from-string "[10,20,{\"foo\":\"bar\",}]")
  (parser-error (c)
    (format t "~A~%" c)
    (parser-error-return-trace c)))
NIL:1:20: Expected #\" on #<STRING-INPUT-STREAM>
((xyz.shunter.parsnip.examples.json::value 1 0)
 (xyz.shunter.parsnip.examples.json::json-array 1 0)
 (xyz.shunter.parsnip.examples.json::value 1 7)
 (xyz.shunter.parsnip.examples.json::json-object 1 7)
 (xyz.shunter.parsnip.examples.json::json-string 1 20))
```

The [test suite](./test.lisp) shows how each function works, and how it's expected to perform.

## Is this Production-ready?

After a couple months of working on this project in my spare time, I believe it is ready for public use.
However, you may have certain requirements for your own project which would hold you off from using this library over something else:

- API Stability.
  I've tried and switched through a few different methods for parsing data, including value-based evaluation and continuations.
  Through experimentation, I've found that the external API remains largely the same.
  I'll feel confident enough in this when I create my own decoder on-top of this library.
- Robustness.
  I've targeted at least 95% coverage reported by `sb-cover` while developing this API, to limit erroneous behavior stemming from edge cases.
  Every release includes a code coverage report, and every push to the repository triggers an automated system test.
- Development Speed.
  Something similar to the example json decoder can be reasonably written within an afternoon.
  During anothe one of my projects, I was able to write a parser that describes notes, durations, note dots, pitches, beams, chords, and measure bars, within a similar amount of time.
  I've designed the API to match CL's standard library as closely as possible to make it as learnable as possible.
- Maturity.
  The best solution for this that I can think of is `Time * Exposure`.
  I also appreciate multiple eyes looking at this project.
  Any comments, questions, and suggestions are well appreciated :)

## Breaking Changes

When the library reaches 1.0, I need to consider what parts of the library to solidify.
I recognize these as breaking changes:

- Removing functions or macros
- Removing parameters from a function or macro.
- Changing a function to a macro, or vice-versa.
- Changing the specified behavior of a pre-existing function or macro, given the same parameters.
- Changing, adding, or removing any package names or nicknames.

I recognize these as non-breaking changes:

- Extending functions or macros with on-required parameters.
  The default behavior should still match
- Adding new external functions, macros, or other symbols to the package.
- Changing the behavior of pre-existing function or macro, if the original behavior was a bug.
- Adding new dependencies to the system (though I hardly foresee this happening).

## Examples

The [JSON example](./examples/json.lisp) matches close to the grammar notation of the [RFC8259 JSON specification](https://datatracker.ietf.org/doc/html/rfc8259).
Outside of a couple outliers (e.g. the value definition is moved to the end), the code is laid out nearly section-by-section as stated in the RFC.

I plan to be writing a parser for [ABC notation v2.1](http://abcnotation.com/wiki/abc:standard:v2.1) in the future.

## API

### [Function] **ok** *value* => *parser*

Return a parser that consumes nothing and returns the given value.

### [Function] **fail** *expected &optional trace* => *parser*

Return a parser that consumes nothing and fails, reporting the expected value.

### [Function] **char-if** *predicate &optional message* => *char-parser*

Return a parser that consumes a character that satisfies the given predicate.

### [Function] **char-of** *char &optional message* => *char-parser*

Return a parser that consumes the given character.

### [Function] **char-in** *charbag &optional message* => *char-parser*

Return a parser that consumes a character in the given character bag.

### [Function] **eof** *&optional value* => *parser*

Return a parser that consumes nothing and returns the given value (or `nil`) if the input stream is exhausted.

### [Function] **flatmap** *function parser* => *parser*

Return a new parser that applies the given function to the parser's result, and then runs the parser the function returns.
This function forms the basis of stringing multiple parsers together.

### [Macro] **let!** *(&rest bindings) &body body* => *parser*

Return a parser that runs all given parsers, binds them all to their variables, evaluates the body, and then runs the parser the body returns.

### [Function] **handle** *parser handler* => *parser*

Return a new parser that, on failure, applies the handler function to the parser's expected value and parse trace (as a list), and then runs the parser the handler returns.

`handle` does not handle partial-parse failures, which can be recovered from via `handle-rewind`.

### [Function] **handle-rewind** *parser handler* => *parser*

Return a new parser that saves the stream's current position and, on failure, rewinds the stream, applies the handler function to the parser's expected value and parse trace (as a list), and then runs the parser the handler returns.

`handle-rewind` only functions if the parser is given a seekable stream.

### [Function] **progn!** *&rest parsers* => *parser*

Return a parser that strings together all given parsers and returns the last parser's result.

### [Function] **prog1!** *first &rest parsers* => *parser*

Return a parser that strings together all given parsers and returns the first parser's result.

### [Function] **prog2!** *first second &rest parsers* => *parser*

Return a parser that strings together all given parsers and returns the second parser's result.

### [Function] **or!** *&rest parsers* => *parser*

Return a parser that tries each given parser in order (until a partial-parse failure) and returns the result of the first successful parse.

### [Function] **collect** *parser* => *list-parser*

Return a parser that runs the given parser until failure, and collects all results into a list.

### [Function] **collect1** *parser* => *list-parser*

Return a parser that runs the given parser once, keeps parsing until failure, and then collects all results into a list.

### [Function] **collect-into-string** *char-parser* => *string-parser*

Return a parser that runs the given character parser until failure, and collects all characters into a string.

### [Function] **sep** *value-parser sep-parser* => *value-list-parser*

Return a parser that accepts a sequence of `value-parser` input separated by `sep-parser` input; such as values separated by commas.

### [Function] **reduce!** *function parser &key initial-value* => *parser*

Return a parser that keeps running until failure, and reduces its result into one value.

If `initial-value` is supplied, the parser may succeed without parsing by returning `initial-value`.

### [Function] **skip** *parser* => *parser*

Parse and pretend no input was consumed.

### [Function] **skip-many** *parser* => *parser*

Keep parsing until failure, discard the results, and pretend no input was consumed.

### [Function] **digit** *&optional (radix 10)* => *integer-parser*

Consume and return the number value of a digit.

### [Function] **natural** *&optional (radix 10)* => *integer-parser*

Consume and return a natural number.

### [Macro] **defparser** *name () &body (form)* => *symbol*

Define a parser as a function. It can then be referenced as a function designator.

### [Function] **parse** *parser stream* => *object*

Run a parser through a given stream and raise any failures as a `parser-error`.

### [Condition] **parser-error**

### [Function] **parser-error-line**, **parser-error-column** *parser-error* => *integer*

Return the line and column that the parser stopped at. The parser assumes the line starts at 1 and column starts at 0 before beginning. Newlines count as EOL and tabs count as one space.

### [Function] **parser-error-expected** *parser-error* => *string-designator*

Return a description of the item the parser expected.
