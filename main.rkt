#lang racket/base

(require racket/contract
         racket/dict
         racket/fixnum
         racket/flonum
         racket/list
         racket/logging
         racket/port
         racket/sequence
         racket/set
         racket/string)

(provide (contract-out
          (line-chars/c contract?)
          (tree-representation-line-chars (parameter/c line-chars/c))
          (unnamed-sequence-string (parameter/c (or/c string?
                                                      (-> (or/c simple-dictionary? simple-sequence?)
                                                          string?))))
          (ascii-line-chars line-chars/c)
          (unicode-line-chars line-chars/c)
          (atom? contract?)
          (dotted-pair? contract?)
          (simple-sequence? contract?)
          (simple-dictionary? contract?)
          (tree-input/c contract?)
          (write-text-tree (->* (tree-input/c) (output-port?) void?))
          (text-tree->string (-> tree-input/c string?))))

;; ------------------------------------------------------------------------------------------------
;; Provided -- Parameters
;; ------------------------------------------------------------------------------------------------

(define ascii-line-chars '(#\- #\| #\' #\+ #\space))

(define unicode-line-chars '(#\─ #\│ #\└ #\├ #\space))

(define line-chars/c (list/c char? char? char? char? char?))

(define tree-representation-line-chars
  (make-parameter unicode-line-chars #f 'tree-representation-line-chars))

(define unnamed-sequence-string (make-parameter "<empty>"))

;; ------------------------------------------------------------------------------------------------
;; Provided -- Predicates
;; ------------------------------------------------------------------------------------------------

(define (atom? va)
  (or (boolean? va) (number? va) (string? va) (bytes? va) (char? va) (symbol? va)))

(define (dotted-pair? val)
  (and (pair? val)
       (not (list? (cdr val)))))

(define (simple-sequence? val)
  (or (list? val) (vector? val) (flvector? val) (fxvector? val) (set? val)))

(define (simple-dictionary? val)
  (or (hash? val) (dict? val)))

;; ------------------------------------------------------------------------------------------------
;; Internals -- Prefixes
;; ------------------------------------------------------------------------------------------------

(define (make-one-prefix-string kind)
  (let* ((line-draw-chars (tree-representation-line-chars))
         (hbar (list-ref line-draw-chars 0))
         (vbar (list-ref line-draw-chars 1))
         (corner (list-ref line-draw-chars 2))
         (tee (list-ref line-draw-chars 3))
         (space (list-ref line-draw-chars 4)))
    (cond
      ((eq? kind 'root) (string))
      ((eq? kind 'none) (make-string 3 space))
      ((eq? kind 'skip) (string vbar space space))
      ((eq? kind 'last) (string corner hbar hbar))
      ((eq? kind 'midd) (string tee hbar hbar)))))

(define (make-prefix-string path)
  (if (empty? path)
      (string)
      (string-append
       (string-join
        (map make-one-prefix-string (reverse path))
        (string #\space))
       (string #\space))))

;; ------------------------------------------------------------------------------------------------
;; Internals -- Tree construction
;; ------------------------------------------------------------------------------------------------

(define (adjust-prefixes prefixes)
  (cond
    ((and (not (empty? prefixes)) (eq? (car prefixes) 'midd))
     (cons 'skip (cdr prefixes)))
    ((and (not (empty? prefixes)) (eq? (car prefixes) 'last))
     (cons 'none (cdr prefixes)))
    (else prefixes)))

(define (atom->text-tree atom prefixes out)
  (displayln (format "~a~a" (make-prefix-string prefixes) atom) out))

(define (sequence->text-tree seq prefixes as-dict? out)
  (let ((seq-length (sequence-length seq))
        (adjusted (adjust-prefixes prefixes)))
    (if (and (= seq-length 1)
             (or (simple-sequence? (car seq))
                 (simple-dictionary? (car seq))))
        (let ((unnamed (unnamed-sequence-string)))
          (pair->text-tree (cons (if (procedure? unnamed)
                                     (unnamed seq)
                                     unnamed)
                                 (car seq))
                         (cons 'last adjusted) out))
        (for-each
         (λ (index value)
           (let ((last? (= (+ index 1) seq-length)))
             (let ((adjusted (cons (if last? 'last 'midd) adjusted)))
               (if as-dict?
                   (pair->text-tree value adjusted out)
                   (text-tree value adjusted out)))
             ))
         (range seq-length)
         (sequence->list seq)))))

(define (pair->text-tree pair prefixes out)
  (let* ((name (car pair))
         (value (cdr pair))
         (adjusted (adjust-prefixes prefixes))
         (name-prefix (make-prefix-string prefixes))
         (value-prefix (if (atom? value)
                           (cons 'last adjusted)
                           adjusted)))
    (displayln (format "~a~a" name-prefix name) out)
    (text-tree value value-prefix out)))

(define (text-tree value prefixes out)
  (cond
    ((dotted-pair? value)
     (pair->text-tree value prefixes out))
    ((atom? value)
     (atom->text-tree value prefixes out))
    ((simple-dictionary? value)
     (sequence->text-tree (if (hash? value)
                              (hash->list value #t)
                              (dict->list value))
                          prefixes #t out))
    ((simple-sequence? value)
     (sequence->text-tree value prefixes #f out))
    (else (raise-argument-error 'value "tree-input/c" value))))

;; ------------------------------------------------------------------------------------------------
;; Provided
;; ------------------------------------------------------------------------------------------------

(define tree-input/c (or/c atom?
                           dotted-pair?
                           simple-dictionary?
                           simple-sequence?))

(define (write-text-tree value (out (current-output-port)))
  (text-tree value '() out))

(define (text-tree->string value)
  (with-output-to-string
    (λ () (write-text-tree value))))
