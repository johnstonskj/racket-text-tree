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

@defproc[#:kind "predicate"
         (atom? [val any/c]) boolean?]{
Returns @racket[#t] if the @racket[val] is one of @racket[boolean?], @racket[char?], @racket[number?],
@racket[string?], @racket[bytes], or @racket[symbol?]. Atoms are written out using the @racket[~a] format function.
}

@defproc[#:kind "predicate"
         (dotted-pair? [val any/c]) boolean?]{
Returns @racket[#t] if the @racket[val] is an @italic{dotted} or @italic{improper} pair where the @racket[cdr] is not a
list.
}

@defproc[#:kind "predicate"
         (simple-dictionary? [val any/c]) boolean?]{
Returns @racket[#t] if the @racket[val] is either a @racket[hash?] or a structure implementing @racket[gen:dict?].
}

@defproc[#:kind "predicate"
         (simple-sequence? [val any/c]) boolean?]{
Returns @racket[#t] if the @racket[val] is one of @racket[list?], @racket[vector?],
@racket[flvector?], @racket[fxvector?], or @racket[set?].
}

@defthing[tree-input/c contract?]{
This is the value accepted by the function and consists of atomic values, sequences (restricted to lists,
mutable lists, vectors, fixnum vectors, flonum vectors, hash tables, sets, and streams), and dictionaries.

@itemlist[
  @item{@racket[atom?] -- Individual value.}
  @item{@racket[simple-sequence?] -- Where every member is a @racket[tree-input/c].}
  @item{@racket[simple-sequence?] -- Where every key is a @racket[atom?] and every value is a @racket[tree-input/c].}
]
}

@section[]{Parameters}

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
A set of simple ASCII characters for a low-fi tree.

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
A set of Unicode line-drawing characters for a higher fidelity tree.

@examples[#:eval example-eval
(require racket/port)

(parameterize ((tree-representation-line-chars unicode-line-chars))
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

@defparam[unnamed-sequence-string
          unnamed
          (or/c string?
                (-> (or/c simple-dictionary? simple-sequence?)
                    string?))
          #:value "<empty>"]{
This parameter determines the string to use as the root value for anonymous sequences and dictionaries.

@examples[#:eval example-eval
(displayln (text-tree->string (list (list "date" "May 2024"))))
]

@examples[#:eval example-eval
(parameterize ((unnamed-sequence-string "nil"))
  (displayln (text-tree->string (list (list "date" "May 2024")))))
]

@examples[#:eval example-eval
(define (name-for-unnamed val)
  (cond ((list? val) "empty?")
        ((hash? val) "hash-empty?")))

(parameterize ((unnamed-sequence-string name-for-unnamed))
  (displayln (text-tree->string (list (list "date" "May 2024")))))
]
}
