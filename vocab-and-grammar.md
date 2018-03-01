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
  the exact symbol must be present in the same structural position.


## Grammar
----

The grammar is copied from [`X-expression`][xml-docs] (Racket's xml module). The `cdata` and `misc` terminals are removed.
HML's pattern syntax borrows from their `X-expression` to help provide a more familiar interface.

```
pattern-match = (match/html (pattern doc) body)

pattern = string
        | (symbol ((symbol string) ...) pattern ...)
        | (symbol pattern ...)
        | symbol
        | valid-char?

doc = any-racket-expression

body = any-racket-expression ...

```

The explanation is the same as the xml module's with a few exceptions.
In HML each pair creates a new sub-pattern. Sub patterns match against nesting inside of HTML documents.
The first part of the pair is interpreted as the tag name. Any symbol that is not at the beginning of a sub pattern is
interpreted to be a pattern variable, and will be captured inside the match map.

## Sample Code
----

The following example has the following HTML document bound to `document`

```
<!DOCTYPE html>
<html>
  <head>
    <title>Example page</title>
  </head>
  <body>

    <div>
      <h1>Secion 1</h1>
      <p>Some text in section 1</p>
    </div>

    <div>
      <h1>Section 2</h1>
      <p>Some text in section 2</p>
    </div>

    <h1>Unmatched section</h1>
    <p>Yet some more text</p>

  </body>
</html>
```

Here is example code to extract the section 1 & 2 data:

```
(define match-output
  (match/html
   ((div (h1 title)
         (p  content))
    document)
   (cons (hash-ref mm title) (hash-ref mm content))))

;; There were two structural matches, one for each section.
;; The body of the match was evaluated twice, and the results are
;; in the output.
(check-equal? match-output
              '(("Section 1" . "Some text in section 1")
                ("Section 2" . "Some text in section 2")))

```


[xml-docs]:https://docs.racket-lang.org/xml/#%28def._%28%28lib._xml%2Fprivate%2Fxexpr-core..rkt%29._xexpr~3f%29%29
