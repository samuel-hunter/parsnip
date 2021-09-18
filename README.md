# Parsnip
[![builds.sr.ht status](https://builds.sr.ht/~shunter/parsnip/commits/test.yml.svg)](https://builds.sr.ht/~shunter/parsnip/commits/test.yml)

![Parsnip brand doodle](./brand.png)

Quickly combine small parsers together.

Other parser combinator libraries I've found in the ecosystem are either too macro-heavy for me, or warn that they are not production-ready.
I don't trust third-party libraries that don't trust themselves, and so I've made my own, going for a simple interface targeted for public consumption.

Parsnip targets user-facing compilers, interpreters, or other readers of character-based languages, where end-users would like to know information when input fails to parse.
Parsnip does **not** target performance-intensive or byte-based decoders, such as those used in network stacks, or JSON/XML decoders for user input for web applications.

## Contributions

Any comments, questions, issues, or patches are greatly appreciated!
I do my main development on [Sourcehut](https://sr.ht/~shunter/parsnip/), with a [mailing list](https://lists.sr.ht/~shunter/public-inbox) and [issue tracker](https://todo.sr.ht/~shunter/parsnip).

## Usage

Parsnip is not yet available on Quicklisp:

```sh
$ cd ~/common-lisp/ # Or wherever you store your definitions
$ git clone https://git.sr.ht/~shunter/parsnip
```

```lisp
(ql:quickload :parsnip)
(use-package :parsnip)

;; digit := [0-9]
(defparser digit ()
  (parse-tag :digit (predicate-parser 'digit-char-p)))

(defparser digits ()
  (parse-collect1 'digit))

;; whole-part := digit+
(defparser whole-part ()
  (parse-let ((digits 'digits))
    (parse-integer (coerce digits 'string))))

;; decimal-part := '.' [0-9]+
(defparser decimal-part ()
  (parse-let ((dot (char-parser #\.))
              (digits 'digits))
    (declare (ignore dot))
    (/ (parse-integer (coerce digits 'string))
       (expt 10 (length digits)))))

;; number := whole-part [ decimal-part ]
(defparser decimal-number ()
  (parse-map #'+
             'whole-part
	     (parse-optional #'decimal-part 0)))

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

The [Tiny C example](./examples/tiny-c.lisp) demonstrates a stripped down version of C with no types, no preprocessing, and only an `if` control structure.
It is meant to show a very small yet turing-complete C-family language.

I plan to be writing a parser for [ABC notation v2.1](http://abcnotation.com/wiki/abc:standard:v2.1) in the future.

## API

### [Function] **parse** *parser stream*

Parse from the given stream and raise any failures as a **parser-error**.

### [Function] **char-parser** *char*

Consume and return the given character.

### [Function] **predicate-parser** *predicate*

Consume and return a character that passes the given predicate.

### [Function] **string-parser** *string*

Consume and return the given text. May partially parse on failure.

### [Function] **eof-parser** *&optional value*

Return the given value (or NIL) if at EOF.

### [Function] **digit-parser** *&optional (radix 10)*
Consume a single digit and return its integer value.

### [Function] **integer-parser** *&optional (radix 10)*

Consume one or more digits and return its integer value.

### [Function] **parse-map** *function &rest parsers*

Run the parsers in sequence and apply the given function to all results.

### [Function] **parse-progn** *&optional parsers*

Run the parsers in sequence and return the last result.

### [Function] **parse-prog1** first-parser *&rest parsers*
Run the parsers in sequence and return the first result

### [Function] **parse-prog2** *&rest parsers*

Run the parsers in sequence and return the second result.

### [Function] **parse-collect** *parser*

Run until failure, and then return the collected results.

### [Function] **parse-collect1** *parser*

Run until failure, and then return at **least** one collected result.

### [Function] **parse-collect-string** *parser*

Run until failure, and then return the collected characters as a string.

### [Function] **parse-reduce** *function parser initial-value*

Run until failure, and then reduce the results into one value.

### [Function] **parse-take** *times parser*

Run and collect **exactly** the given number of results.

### [Function] **parse-or** *&rest parsers*

Attempt each given parser in order until one succeeds.

### [Function] **parse-optional** *optional &result default*

Resume from a failure with a default value.

### [Function] **parse-try** *parser*

Try to rewind the stream on any partial-parse failure.
Only works on seekable streams, and is the only parser that can recover from partial-parse failures.

### [Function] **parse-skip-many** *parser*

Keep parsing until failure and pretend no input was consumed.

```lisp
(defparser spaces ()
  (parse-skip-many (char-parser #\Space)))

(defun ws (parser)
  (parse-prog2 spaces parser spaces))
```

### [Function] **parse-tag** *tag parser*

Report fails as expecting the given tag instead of an element.

```lisp
* (defparser digit ()
    (parse-tag :igit (predicate-parser #'digit-char-p)))
DIGIT

* (with-input-from-string (s "a")
    (parse #'digit s))
Expected element :DIGIT on #<dynamic-extent STRING INPUT STREAM>
[Condition of type PARSER-ERROR]
```

### [Macro] **parse-let** *bindings &body body*

Compose multiple parsers together to bind their results to variables and return a value within the body:

```lisp
(defparameter *id-parser*
  (parse-let ((letter (predicate-parser #'alpha-char-p))
              (digits (parse-collect1 (predicate-parser #'digit-char-p))))
    (make-instance 'identifier :letter letter
                               :number (parse-integer (coerce digits 'string)))))
```

### [Macro] **defparser** *name () form*

Define a parser as a function.
They can then be referenced with function designators:

```lisp
(defparser alpha-parser ()
  (predicate-parser #'alpha-char-p))

(defparser id-parser ()
  (parse-let ((letter #'alpha-parser)
              (digits 'digits-parser))
    (make-instance 'identifier :letter letter
                               :number (parse-integer (coerce digits 'string)))))

(defparser digits-parser ()
  (parse-collect1 (predicate-parser #'digit-char-p)))
```

### [Condition] **parser-error** *(stream-error)*

If a parser fails to read text, it signals a `parser-error`, containing a stream, its expected value, and a return trace of parsers.

### [Function] **stream-error-stream** *parser-error*

Return the stream the parser was reading from.

ABCL users: [ABCL currently has a bug](https://github.com/armedbear/abcl/issues/388) that makes this function break on parser errors.
There's a PR that fixes `stream-error-stream` and all other affected error readers.

### [Function] **parser-error-line**, **parser-error-column** *parser-error*

Return the line and column information from the parser.

```lisp
(use-package :xyz.shunter.parsnip.examples.json)

(handler-case (decode-json-from-string "[10,20,,]")
  (parser-error (c)
    (values (parser-error-line c)
            (parser-error-column c))))
1
7
```

### [Function] **parser-error-expected** *parser-error*

Return the value that the parser error expected to read.
May be overridden with `parser-tag`.

### [Function] **parser-error-return-trace** *parser-error*

Every parser defined with `defparser` adds its symbol to the return trace as the error bubbles up.
The return trace, along with `(file-position stream)`, should assist developers debug their parsers

```lisp
(handler-case (decode-json-from-string "[10,20,{\"foo\":\"bar\",}]")
           (parser-error (c)

                         (format t "~A" c)
                         (parser-error-return-trace c)))
NIL:1:20: Expected #\" on #<STRING-INPUT-STREAM>
((XYZ.SHUNTER.PARSNIP.EXAMPLES.JSON::VALUE 1 0)
 (XYZ.SHUNTER.PARSNIP.EXAMPLES.JSON::JSON-ARRAY 1 0)
 (XYZ.SHUNTER.PARSNIP.EXAMPLES.JSON::VALUE 1 7)
 (XYZ.SHUNTER.PARSNIP.EXAMPLES.JSON::JSON-OBJECT 1 7)
 (XYZ.SHUNTER.PARSNIP.EXAMPLES.JSON::JSON-STRING 1 20))
```
