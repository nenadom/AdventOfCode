#!/usr/bin/env scheme --script

(load "utils.ss")


;; Line is Pair
;; interp. line equation of slope-intercept form
;;   y = ax + b
;; where '(a . b) are the parameters; used as key


;; Point is Pair
;; interp. point '(x . y) components;
;; all points are on an inverted plane -- origin is top left


;; Pair -> Integer
;; produce symbol hash of pair of integers by converting it to 
;; string, joining, then converting to symbol
(define (pair-hash p)
  (symbol-hash
    (string->symbol
      (string-append
        (number->string (car p))
        (if (number? (cdr p))
          (number->string (cdr p))
          (symbol->string (cdr p)))))))


;; Hashtable<Line, (listof Point)>
(define ht (make-hashtable pair-hash equal?))

(define (make-ht)
  (make-hashtable pair-hash equal?))

(define (reset-ht)
  (set! ht (make-hashtable pair-hash equal?)))


;; some test entries
(define __test-ht (make-hashtable pair-hash equal?))
(hashtable-cell __test-ht '(3 . 2) '('(5 . 1)))
(hashtable-cell __test-ht '(3 . 2) '('(5 . 2))) ;; stays '(5 . 1)
(hashtable-cell __test-ht '(1 . 0) '('(4 . 4)))


;; Hashtable<Line, (listof Point)> Line Point -> void | (cons Line Point)
;; associate point with line (points not ordered)
(define (add-point ht line pt)
  (let [(current (hashtable-ref ht line #f))]
    (if current
      (hashtable-set! ht line (if (member pt current)
                                current
                                (cons pt current)))
      (hashtable-cell ht line (list pt)))))


;; Point Point -> Line
;; produce Line (slope-intercept form) from two Points
(define (slope-intercept pt1 pt2)
  (let [(Δx (- (car pt2) (car pt1)))]
    (if (zero? Δx)
      (cons (car pt1) 'undefined)
      (cons (/ (- (cdr pt2) (cdr pt1)) Δx)
            (- (cdr pt1)
               (* (/ (- (cdr pt2) (cdr pt1)) Δx)
                  (car pt1)))))))


(test (slope-intercept '(2 . 2) '(3 . 4)) '(2 . -2))
(test (slope-intercept '(3 . 4) '(2 . 2)) '(2 . -2))
(test (slope-intercept '(2 . 2) '(1 . 0)) '(2 . -2))


(define (add-points ht pts)
  (for-each
    (lambda (pt1)
      (for-each
        (lambda (pt2)
          (if (equal? pt1 pt2)
            (void)
            (let [(line (slope-intercept pt1 pt2))]
              (begin
                (add-point ht line pt1)
                (add-point ht line pt2)))))
        pts))
    pts))

; .#..#
; .....
; #####
; ....#
; ...##
(define ex1
  (list '(1 . 0) '(4 . 0) '(0 . 2) '(1 . 2) '(2 . 2) '(3 . 2) '(4 . 2)
        '(4 . 3) '(3 . 4) '(4 . 4)))

(add-points ht ex1)


(define (in-line? ht)
  (let [(vals (vector->list (hashtable-values ht)))]
    (lambda (pt)
      (apply + (map (lambda (pts)
                      (if (member pt pts) 1 0))
                    vals)))))


(define (map->points m)
  (define (map->points x y acc chars)
    (cond [(empty? chars) (reverse acc)]
          [(char=? #\. (car chars))
           (map->points (add1 x) y acc (cdr chars))]
          [(char=? #\# (car chars))
           (map->points (add1 x) y (cons (cons x y) acc) (cdr chars))]
          [(char=? #\newline (car chars))
           (map->points 0 (add1 y) acc (cdr chars))]))
  (map->points 0 0 '() (string->list m)))


(define m1
".#..#
.....
#####
....#
...##")

(test (map->points m1) ex1)

(define m2
"......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####")

(define ex2 (map->points m2))

(reset-ht)

(add-points ht ex2)

(define ex2a
  (map cons
       ex2
       (map (in-line? ht) ex2)))

(define m3
"#.#...#.#.
.###....#.
.#....#...
##.#.#.#.#
....#.#.#.
.##..###.#
..#...##..
..##....##
......#...
.####.###.")

(define ex3 (map->points m3))


(define m4
".#..#..###
####.###.#
....###.#.
..###.##.#
##.##.#.#.
....###..#
..#.#..#.#
#..#.#.###
.##...##.#
.....#.#..")

(define m5
".#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##")

(define (get-approx-guess m)
  (let [(ht  (make-ht))
        (pts (map->points m))]
    (begin
      (add-points ht pts)
      (cdar
        (sort
          (lambda (q r) (> (cdr q) (cdr r)))
          (map cons pts (map (in-line? ht) pts)))))))
