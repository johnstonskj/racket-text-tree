#lang racket/base

(require racket/contract
         racket/list
         racket/port
         racket/set
         racket/string)

(provide (contract-out
          (line-chars/c contract?)
          (tree-representation-line-chars (parameter/c line-chars/c))
          (ascii-line-chars line-chars/c)
          (unicode-line-chars line-chars/c)
          (atom? contract?)
          (dotted-pair? contract?)
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

;; ------------------------------------------------------------------------------------------------
;; Provided -- Predicates
;; ------------------------------------------------------------------------------------------------

(define (atom? value)
  (or (boolean? value) (number? value) (string? value) (char? value) (symbol? value)))

(define (dotted-pair? val)
  (and (pair? val)
       (not (list? (cdr val)))))

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

(define (atom->text-tree atom prefixes out)
  ;(displayln (format "atom->text-tree ~s ~s" prefixes atom))
  (displayln (format "~a~a" (make-prefix-string prefixes) atom) out))

(define (list->text-tree list prefixes out)
  ;(displayln (format "list->text-tree ~s ~s" prefixes list))
  (if (and (= (length list) 1) (list? (car list)))
      (begin
        (atom->text-tree "()" (cons 'last prefixes) out)
        (text-tree (car list) (cons 'none prefixes) out))
      (let ((list-length (length list)))
        (for-each
         (λ (index value)
           (let ((last? (= (+ index 1) list-length)))
             (text-tree value (if last?
                                  (cons 'last prefixes)
                                  (cons 'midd prefixes)) out)))
         (range list-length) list))))

(define (hash->text-tree hash prefixes out)
  ;(displayln (format "hash->text-tree ~s ~s" prefixes hash))
  (let* ((map-length (hash-count hash)))
    (for-each
     (λ (index pair)
       (let* ((name (car pair))
              (value (cdr pair))
              (last? (= (+ index 1) map-length))
              (name-prefix (make-prefix-string
                            (if last?
                                (cons 'last prefixes)
                                (cons 'midd prefixes))))
              (value-prefix (if last?
                                (cons 'none prefixes)
                                (cons 'skip prefixes))))
         (displayln (format "~a~a" name-prefix name) out)
         (text-tree value
                    (if (atom? value)
                        (cons 'last value-prefix)
                        value-prefix)
                    out)))
     (range map-length)
     (hash->list hash #t))))

(define (pair->text-tree pair prefixes out)
  ;(displayln (format "pair->text-tree ~s ~s" prefixes pair))
  (let* ((name (car pair))
        (value (cdr pair))
        (name-prefix (make-prefix-string prefixes))
        (value-prefix (if (or (list? value) (set? value) (vector? value) (hash? value))
                          prefixes
                          (cons 'last
                            (cond
                              ((empty? prefixes) prefixes)
                              ((eq? (car prefixes) 'last)
                               (cons 'none (cdr prefixes)))
                              (else (cons 'midd (cdr prefixes))))))))
    (displayln (format "~a~a" name-prefix name) out)
    (text-tree value value-prefix out)))

(define (text-tree value prefixes out)
  ;(displayln (format "text-tree ~s ~s" prefixes value))
  (cond
    ((dotted-pair? value) (pair->text-tree value prefixes out))
    ((atom? value) (atom->text-tree value prefixes out))
    ((hash? value) (hash->text-tree value prefixes out))
    ((list? value) (list->text-tree value prefixes out))
    ((vector? value) (list->text-tree (vector->list value) prefixes out))
    ((set? value) (list->text-tree (set->list value) prefixes out))
    (else (raise-argument-error 'value "(or/c atom? hash? list? set? dotted-pair?)" value))))

;; ------------------------------------------------------------------------------------------------
;; Provided
;; ------------------------------------------------------------------------------------------------

(define tree-input/c (or/c atom?
                           dotted-pair?
                           (hash/c atom? any/c)
                           (listof any/c)
                           (vector/c any/c)
                           (set/c any/c)))

(define (write-text-tree value (out (current-output-port)))
  (text-tree value '() out))

(define (text-tree->string value)
  (with-output-to-string
    (λ (v) (write-text-tree value))))
