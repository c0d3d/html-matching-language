# HML

## Vocabulary, Semantics, and Scoping

### Definitions
----

* A *pattern* is a collection of *pattern variables*, *literals*, and sub-*patterns*.
  These are the base data in HML. *pattern*s should be interpreted as a means to
  destructure (matching) HTML documents and capture values contained therein. *patterns* are provided
  to *pattern-match expressions* in order to match documents.

* A *pattern variable* is an identifier that will be present as a mapping inside the *match map*
  for a *pattern-match expression*. These variables are placeholders inside of patterns, and will match
  any value at the same relative position within a matched HTML document.

* A *pattern-match expression* is an expression that consumes an HTML document and attempts to
  match it against a given *pattern*. If the given *pattern* does match the HTML document, all of
  the *pattern variables* contained in the *pattern* capture the values inside the document at their corresponding
  positions. These values are stored inside a *match map* that is in scope for the body of the match expression.
  When evaluated *pattern-match expressions* yields either false, if the document didn't match the pattern, or a list
  containing one element for each matching instance inside the html document. The elements in the list are the
  value of the body of the *pattern-match expression* evaluated with the *match map* set up for that specific match.

* A *match map* is an implicitly bound map inside of the body of a *pattern-match expression*. When a match is successful
  the map contains a mapping from *pattern variable* name to its value.

* A *literal* in the context of an HML pattern means a symbol that is matched literally. In order for a literal to match
  the exact symbol must be present in the same structural position. These are things like strings, numeric literals, and
  html element names (in function call position).


## Grammar
----

The grammar is copied from [`X-expression`][xml-docs] (Racket's xml module). The `cdata`, `misc`, and `valid-char?` terminals are removed.
HML's pattern syntax borrows from their `X-expression` to help provide a more familiar interface.

```
make-pattern = (make-pattern pattern)

pattern-match = (match/html (pattern doc) body)

pattern = string
        | (symbol ((symbol string) ...) pattern ...)
        | (symbol pattern ...)
        | symbol

doc = any-racket-expression

body = any-racket-expression ...

```

The explanation is the same as the xml module's with a few exceptions.
In HML each pair creates a new sub-pattern. Sub patterns match against nesting inside of HTML documents.
The first part of the pair is interpreted as the tag name. Any symbol that is not at the beginning of a sub pattern is
interpreted to be a pattern variable, and will be captured inside the match map.

a `pattern-match` expression performs the matching against the `body`. Note that pattern can be a literal `pattern` expression (expressed inline),
or a pattern value (created by `make-pattern`).

a `make-pattern` expression compiles a given literal pattern into a pattern value that can be passed around like other racket values.

## Sample Code
----

The following examples have the following HTML document bound to `document`

```
<!DOCTYPE html>
<html>
  <head>
    <title>Example page</title>
  </head>
  <body>

    <div>
      <h1>Secion 1</h1>
      <p>Text1</p>
    </div>

    <div>
      <h1>Section 2</h1>
      <p>Text2</p>
    </div>

	<p>
	  <h1>Unmatched</h1>
	  <p>More text</p>
    </p>

  </body>
</html>
```

### Example 1: Simple Extraction

```
(define match-output
  (match/html
   ((div (h1 title)
         (p  content))
    document)
   (cons (hash-ref mm "title") (hash-ref mm "content"))))

;; There were two structural matches, one for each section.
;; The body of the match was evaluated twice, and the results are
;; in the output.
(check-equal? match-output
              '(("Section 1" . "Text1")
                ("Section 2" . "Text2")))

```

### Example 2: Patterns as Data

```
(define pat1
  (make-pattern
   (div (h1 "Section 1")
        (p contents))))

(define pat2
  (make-pattern
   (p (h1 title)
      (p contents))))

(define match-output
  (match/html
   ; or attempts both patterns
   (or pat1 pat2)
   (if (hash-has-key? mm "title")
       (string-append
        (hash-ref mm "title") ": "
        (hash-ref mm "contents"))
       (hash-ref mm "contents"))))

(check-equal?
 match-output
 '("Text1" "Unmatched: More text"))

```

[xml-docs]:https://docs.racket-lang.org/xml/#%28def._%28%28lib._xml%2Fprivate%2Fxexpr-core..rkt%29._xexpr~3f%29%29
