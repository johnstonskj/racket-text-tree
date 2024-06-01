#lang racket/base

(require racket/port
         ;; --------------------------------------
         rackunit
         rackunit/text-ui
         ;; --------------------------------------
         "../main.rkt")

(provide text-tree-test-suite)

(define (check-output-equal? input expected-output)
  (let ((actual-output
         (with-output-to-string
           (λ () (write-text-tree input)))))
    (check-equal? actual-output expected-output)))

;; -----------------------------------------------------------------------------------------------
;; Test Suite(s)
;; -----------------------------------------------------------------------------------------------

(define text-tree-test-suite
  (test-suite
   "Module `text-tree`"

   (test-case
    "input: string"
    (check-output-equal?
     "hello"
     "hello\n"))

   (test-case
    "input: dotted-pair"
    (check-output-equal?
     (cons "date" "May 2024")
     "date
└── May 2024
"))

   (test-case
    "input: list"
    (check-output-equal?
     (list "date" "May 2024")
     "├── date
└── May 2024
"))

   (test-case
    "input: list of list"
    (check-output-equal?
     (list (list "date" "May 2024"))
     "└── ()
    ├── date
    └── May 2024
"))

   (test-case
    "input: hash"
    (check-output-equal?
     (make-hash '((one . ("Alice"))
                  (two . ("Bob" "Charlie"))))
     "├── one
│   └── Alice
└── two
    ├── Bob
    └── Charlie
"))

   (test-case
    "input: hash with list"
    (check-output-equal?
     (make-hash '(("name" . "me")
                  ("id" . 21)
                  ("groups" group-1 group-2)))
     "├── groups
│   ├── group-1
│   └── group-2
├── id
│   └── 21
└── name
    └── me
"))
   ))

;; TODO: fix this ...

;; (make-hash
;;  `((collection . "text-tree")
;;    (deps base)
;;    (build-deps scribble-lib racket-doc rackunit-lib)
;;    (deps ,(make-hash '((deps base)
;;                        (build-deps scribble-lib racket-doc rackunit-lib))))
;;    (scribblings "scribblings/text-tree.scrbl")
;;    (test-omit-paths "scribblings")))

;; -----------------------------------------------------------------------------------------------
;; Test Runner
;; -----------------------------------------------------------------------------------------------

(run-tests text-tree-test-suite)
