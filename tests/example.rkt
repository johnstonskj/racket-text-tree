#lang racket/base

(require racket/port
         "../main.rkt")

(displayln
  (with-output-to-string
    (λ () (write-text-tree
            (make-hash
              '((collection . "text-tree")
                (deps "base")
                (build-deps "scribble-lib" "racket-doc" "sandbox-lib" "rackunit-lib")
                (scribblings "scribblings/text-tree.scrbl")
                (test-omit-paths "scribblings")
                (pkg-desc . "Simple interface to output tree-structured data.")
                (version . "1.0")
                (pkg-authors johnstonskj)
                (license . Apache-2.0)))))))
