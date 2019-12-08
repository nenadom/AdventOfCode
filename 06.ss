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


;; --- Day 6: Universal Orbit Map ---

; You've landed at the Universal Orbit Map facility on Mercury. Because
; navigation in space often involves transferring between orbits, the orbit
; maps here are useful for finding efficient routes between, for example, you
; and Santa. You download a map of the local orbits (your puzzle input).
; 
; Except for the universal Center of Mass (COM), every object in space is in
; orbit around exactly one other object. An orbit looks roughly like this:
; 
;                   \
;                    \
;                     |
;                     |
; AAA--> o            o <--BBB
;                     |
;                     |
;                    /
;                   /
; 
; In this diagram, the object BBB is in orbit around AAA. The path that BBB
; takes around AAA (drawn with lines) is only partly shown. In the map data,
; this orbital relationship is written AAA)BBB, which means "BBB is in orbit
; around AAA".
; 
; Before you use your map data to plot a course, you need to make sure it
; wasn't corrupted during the download. To verify maps, the Universal Orbit Map
; facility uses orbit count checksums - the total number of direct orbits (like
; the one shown above) and indirect orbits.
; 
; Whenever A orbits B and B orbits C, then A indirectly orbits C. This chain
; can be any number of objects long: if A orbits B, B orbits C, and C orbits D,
; then A indirectly orbits D.
; 
; For example, suppose you have the following map:
; 
; COM)B
; B)C
; C)D
; D)E
; E)F
; B)G
; G)H
; D)I
; E)J
; J)K
; K)L
; 
; Visually, the above map of orbits looks like this:
; 
;         G - H       J - K - L
;        /           /
; COM - B - C - D - E - F
;                \
;                 I
; 
; In this visual representation, when two objects are connected by a line, the
; one on the right directly orbits the one on the left.
; 
; Here, we can count the total number of orbits as follows:
; 
;     D directly orbits C and indirectly orbits B and COM, a total of 3 orbits.
;     L directly orbits K and indirectly orbits J, E, D, C, B, and COM, a total of 7 orbits.
;     COM orbits nothing.
; 
; The total number of direct and indirect orbits in this example is 42.
; 
; What is the total number of direct and indirect orbits in your map data?


(define-structure orbit name direct)
;; Orbit is (make-orbit Symbol Orbit|(listof Orbit))
;; interp. (make-orbit name direct)
;; - name is Symbol
;; - direct is one of:
;;   - Orbit
;;   - (listof Orbit) if multiple direct orbits on object

;; Example using Orbit struct
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


;; Orbit Symbol -> Boolean
(define (has-orbit-direct? orbit name)
  (member name (map orbit-name (orbit-direct orbit))))

(test (has-orbit-direct? orbits2 'B) '(B))
(test (has-orbit-direct? B 'L) #f)
(test (has-orbit-direct? B 'G) '(G C))


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
      (cond [(false? target) target]
            [(has-orbit-direct? target (orbit-name next)) #f]
            [else
              (and (set-orbit-direct! target
                                      (cons next
                                            (orbit-direct target)))
                   #t)]))))


;; Orbit (listof String) -> Orbit
;; Mutate orbits by adding all mappings to it
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
;; Count total number of direct orbits (i.e. node count)
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


(define input-mappings (read-file "inputs/06.txt"))

;; If left orbit not found, mapping added to end of queue.
;; Assumes input is valid & will finish.
(define (build-orbits-revolving orbit mappings)
  (cond [(null? mappings) "done building orbits"]
        [else
          (let [(added? ((add-mapping orbit) (car mappings)))]
            (if added?
              (build-orbits-revolving orbit
                                      (cdr mappings))
              (build-orbits-revolving orbit
                                      (snoc (car mappings) (cdr mappings)))))]))


;; Setup orbit tree from input
(define O (make-orbit 'COM '()))
(build-orbits-revolving O input-mappings)
(test (direct O) (length input-mappings))

(print
  (direct+indirect O))

