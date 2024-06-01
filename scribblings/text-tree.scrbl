#lang scribble/manual

@(require racket/sandbox
          scribble/eval
          (for-label racket/base
                     racket/contract
                     racket/dict
                     racket/format
                     racket/port
                     racket/sequence
                     text-tree))

@;{============================================================================}

@(define example-eval (make-base-eval '(require text-tree)))

@;{============================================================================}

@title[#:version  "1.0"]{Text-tree output.}
@author[(author+email "Simon Johnston" "johnstonskj@gmail.com")]
@defmodule[text-tree]

This package provides a simple interface to output tree-structured data. The single function @racket[write-text-tree]
writes the provided racket data structures out to a port. 

@defproc[(write-text-tree
          [value tree-input/c]
          [out output-port? (current-output-port)])
         void?]{
TBD

@examples[#:eval example-eval
(require racket/port)

(displayln
  (with-output-to-string
    (λ () (write-text-tree
            (make-hash
              `((collection . "text-tree")
                (deps base)
                (build-deps scribble-lib racket-doc rackunit-lib)
                (scribblings "scribblings/text-tree.scrbl")
                (test-omit-paths "scribblings")
                (pkg-desc . "Function to output tree-structured data.")
                (version . 1.0)
                (pkg-authors johnstonskj)
                (license . Apache-2.0)))))))
]

}

@defproc[(text-tree->string
          [value tree-input/c])
         string?]{
A convenience wrapper around @racket[write-text-tree] that writes @racket[value] to, and returns, a string.
}

@defproc[(atom? [val any/c]) boolean?]{
Returns @racket[#t] if the @racket[val] is one of @racket[boolean?], @racket[char?], @racket[number?],
@racket[string?], or @racket[symbol?]. Atoms are written out using the @racket[~a] format function.
}

@defthing[dotted-pair? contract?]{
Returns @racket[#t] if the @racket[val] is an @italic{dotted} or @italic{improper} pair where the @racket[cdr] is not a
list.
}

@defthing[tree-input/c contract?]{
This is the value accepted by the function and consists of atomic values, sequences (restricted to lists,
mutable lists, vectors, fixnum vectors, flonum vectors, hash tables, sets, and streams), and dictionaries.

@itemlist[
  @item{@racket[atom?] -- Individual value.}
  @item{@racket[sequence/c] -- Where every member is a @racket[tree-input/c].}
  @item{@racket[dict?] -- Where every key is a @racket[atom?] and every value is a @racket[tree-input/c].}
]
}

@section[]{Line Drawing}

@defparam[tree-representation-line-chars
          line-chars
          (list/c char? char? char? char? char?)]{
A parameter that is used in drawing the tree's connecting lines. The default value is @racket[unicode-line-chars],
whereas the value @racket[ascii-line-chars] may be more broadly supported but is a lower quality output.

@itemlist[#:style 'ordered
  @item{Horizontal bar character, default @racket[#\─].}
  @item{Vertical bar character, default @racket[#\│].}
  @item{Bottom-left corner character, default @racket[#\└].}
  @item{Right-facing tee character, default @racket[#\├].}
  @item{Spacing character, default @racket[#\space].}
]
}

@defthing[ascii-line-chars (list/c char? char? char? char? char?)]{
TBD

@examples[#:eval example-eval
(require racket/port)

(parameterize ((tree-representation-line-chars ascii-line-chars))
(displayln
  (with-output-to-string
    (λ () (write-text-tree
            (make-hash
              '((collection . "text-tree")
                (deps "base")
                (build-deps "scribble-lib" "racket-doc" "rackunit-lib")
                (license . Apache-2.0))))))))
]
}

@defthing[unicode-line-chars (list/c char? char? char? char? char?)]{
TBD
}
