#!/usr/bin/env scheme --script


(load "utils.ss")


;; https://www.scheme.com/tspl4/syntax.html#./syntax:s70
(define-syntax define-structure
  (lambda (x)
    (define gen-id
      (lambda (template-id . args)
        (datum->syntax template-id
                       (string->symbol
                         (apply string-append
                                (map (lambda (x)
                                       (if (string? x)
                                         x
                                         (symbol->string (syntax->datum x))))
                                     args))))))
    (syntax-case x ()
                 [(_ name field ...)
                  (with-syntax ([constructor (gen-id #'name "make-" #'name)]
                                [predicate (gen-id #'name #'name "?")]
                                [(access ...)
                                 (map (lambda (x) (gen-id x #'name "-" x))
                                      #'(field ...))]
                                [(assign ...)
                                 (map (lambda (x)
                                        (gen-id x "set-" #'name "-" x "!"))
                                      #'(field ...))]
                                [structure-length (+ (length #'(field ...)) 1)]
                                [(index ...)
                                 (let f ([i 1] [ids #'(field ...)])
                                   (if (null? ids)
                                     '()
                                     (cons i (f (+ i 1) (cdr ids)))))])
                               #'(begin
                                   (define constructor
                                     (lambda (field ...)
                                       (vector 'name field ...)))
                                   (define predicate
                                     (lambda (x)
                                       (and (vector? x)
                                            (= (vector-length x) structure-length)
                                            (eq? (vector-ref x 0) 'name))))
                                   (define access
                                     (lambda (x)
                                       (vector-ref x index)))
                                   ...
                                   (define assign
                                     (lambda (x update)
                                       (vector-set! x index update)))
                                   ...))])))


;; Example from the assignment using list
(define orbits '(COM B (G H) C D (I) E (J K L) F))

(define-structure orbit name direct)
;; Orbit is (make-orbit Symbol Orbit|(listof Orbit))
;; interp. (make-orbit name direct)
;; - name is Symbol
;; - direct is one of:
;;   - Orbit
;;   - (listof Orbit) if multiple direct orbits on object

;; Same example using Orbit struct
(define J (make-orbit 'J
                      (list (make-orbit 'K
                                        (list (make-orbit 'L '()))))))
(define E (make-orbit 'E
                      (list J
                            (make-orbit 'F '()))))
(define I (make-orbit 'I '()))
(define D (make-orbit 'D
                      (list I E)))
(define C (make-orbit 'C
                      (list D)))
(define G (make-orbit 'G
                      (list (make-orbit 'H '()))))
(define B (make-orbit 'B
                      (list G C)))
(define orbits2 (make-orbit 'COM
                            (list B)))


;; Orbit Symbol -> Orbit | False
;; Recursively looks for orbit with given name and returns either:
;; reference to an orbit or false
(define (find-orbit orbit name)
  (define (fn-for-orbit o n)
    (if (eq? (orbit-name o) n)
      o
      (fn-for-loo (orbit-direct o) n)))
  (define (fn-for-loo loo n)
    (cond [(null? loo) #f]
          [else
            (or (fn-for-orbit (car loo) n)
                (fn-for-loo (cdr loo) n))]))
  (fn-for-orbit orbit name))


;; Orbit -> String -> Orbit | False
;; Given string of format "{name}){next}" find orbit with symbol name
;; and if found mutate its orbit-direct list bt adding orbit with symbol `next`,
;; return #f if left orbit not found.
(define (add-mapping orbit)
  (lambda (mapping)
    (let* [(m (string-split mapping #\)))
           (name (string->symbol (car m)))
           (next (make-orbit (string->symbol (cadr m)) '()))
           (target (find-orbit orbit name))]
      (and target (set-orbit-direct! target
                                     (cons next
                                           (orbit-direct target)))))))


(define (build-orbits source mappings)
  (for-each (add-mapping source) mappings))

(define example-mappings
  "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L")

(define orbits3 (make-orbit 'COM '()))
(build-orbits orbits3 (string-split example-mappings #\newline))

(assert (equal? orbits2 orbits3))


;; Orbit -> Number
;; Count total number of direct orbits
(define (direct orbit)
  ;; rsf is Integer; result of direct nodes visited so far
  ;; todo is (listof Orbit)
  (define (fn-for-o o todo rsf)
    (fn-for-todo (append (orbit-direct o) todo)
                 (add1 rsf)))
  (define (fn-for-todo todo rsf)
    (cond [(null? todo) rsf]
          [else
            (fn-for-o (car todo)
                      (cdr todo)
                      rsf)]))
  (fn-for-o orbit '() -1))


(test (direct (make-orbit 'COM '())) 0)
(test (direct (make-orbit 'COM
                          (list (make-orbit 'B '()))))
      1)
(test (direct (make-orbit 'COM
                          (list (make-orbit 'B
                                            (list (make-orbit 'C '()))))))
      2)


;; Orbit -> Number
;; Count the number of direct and indirect orbits in tree;
;; Whenever A orbits B and B orbits C, then A indirectly orbits C.
(define (direct+indirect o)
  (define (iter o acc)
    (apply + acc
           (map (lambda (o) (iter o (add1 acc))) (orbit-direct o))))
  (iter o 0))

(test (direct+indirect orbits3) 42)


