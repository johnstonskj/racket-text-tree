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
    "input: list of list with empty"
    (check-output-equal?
     (list (list "date" "May 2024"))
     "└── <empty>
    ├── date
    └── May 2024
"))

   (test-case
       "input: list of list with custom empty"
     (parameterize ((unnamed-sequence-string "nil"))
       (check-output-equal?
        (list (list "date" "May 2024"))
        "└── nil
    ├── date
    └── May 2024
")))

   (test-case
       "input: list of list with empty proc"
     (parameterize ((unnamed-sequence-string (λ (_) "yay!")))
       (check-output-equal?
        (list (list "date" "May 2024"))
        "└── yay!
    ├── date
    └── May 2024
")))

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

   (test-case
       "input: hash with hashes and sequences"
     (check-output-equal?
      (make-hash
       `((a . 1)
         (b . (2 3))
         (c 2 3)
         (d . ,(make-hash
                '((x . 99)
                  (y . ((45 46 47)))
                  (z . 79))))))
      "├── a
│   └── 1
├── b
│   ├── 2
│   └── 3
├── c
│   ├── 2
│   └── 3
└── d
    ├── x
    │   └── 99
    ├── y
    │   └── <empty>
    │       ├── 45
    │       ├── 46
    │       └── 47
    └── z
        └── 79
"))
   ))

;; -----------------------------------------------------------------------------------------------
;; Test Runner
;; -----------------------------------------------------------------------------------------------

(run-tests text-tree-test-suite)
