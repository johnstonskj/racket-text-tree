#lang info

(define collection "text-tree")
(define pkg-desc "Functions to write tree-structured data in text form.")
(define version "1.1")
(define pkg-authors '(johnstonskj))
(define license 'Apache-2.0)

(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "sandbox-lib" "rackunit-lib"))
(define scribblings '(("scribblings/text-tree.scrbl" ())))
(define test-omit-paths '("scribblings"))
