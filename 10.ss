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


;; Void -> Hashtable<Line, (listof Point)>
(define (make-ht)
  (make-hashtable pair-hash equal?))


;; some test entries
;; Hashtable<Line, (listof Point)>
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


;; Hashtable<Line, (listof Point)> (listof Point) -> Void
;; find slope-intercept line form between each point and every other
;; point in pts, and add them to hashtable ht
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


;; Map is String
;; interp. a 2D map of asteroids; origin is top left
;;  - #: asteroid
;;  - .: not asteroid
;; following are examples from puzzle:

(define m1
  ".#..#
  .....
  #####
  ....#
  ...##")

;; same as above
(define ex1
  (list '(1 . 0) '(4 . 0) '(0 . 2) '(1 . 2) '(2 . 2) '(3 . 2) '(4 . 2)
        '(4 . 3) '(3 . 4) '(4 . 4)))

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


;; Map -> (listof Point)
;; produce map from list of points
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


;; Point Point -> Boolean
;; <= comparison for two Points
(define (pair<= p1 p2)
  (and (<= (car p1) (car p2))
       (<= (cdr p1) (cdr p2))))


;; (listof Point) -> (listof Point)
;; sort pts by <=
(define (sort-pts pts)
  (sort pair<= pts))


;; Hashtable<Line, (listof Point)> -> Point -> Integer
;; Produce thunk which calculates how many other asteroids (Points) visible
;; from its position, taking into account line of sight. For each two asteroids:
;; - 0 if no line from one to another asteroid
;; - 1 if on end of line of sight
;; - 2 otherwise, if somewhere else in same line
(define (asteroids-visible ht)
  (let [(vals (map sort-pts (vector->list (hashtable-values ht))))]
    (lambda (pt)
      (apply + (map
                 (lambda (pts)
                   (cond [(not (member pt pts)) 0]
                         [(equal? pt (car pts)) 1]
                         [(equal? pt (car (reverse pts))) 1]
                         [else 2]))
                 vals)))))


;; Map -> Pair<Point, Integer>
;; produce pair which represents the asteroid from which most other asteroids
;; are visible, and the number of asteroids visible from it
(define (most-asteroids m)
  (let [(ht  (make-ht))
        (pts (map->points m))]
    (begin
      (add-points ht pts)
      (let [(fn (asteroids-visible ht))]
        (car
          (sort
            (lambda (q r) (> (cdr q) (cdr r)))
            (map cons pts (map fn pts))))))))


;; Solve Part 1
(let [(f (read-file "inputs/10.txt"))]
  (display
    (most-asteroids
      (apply string-append
             (map string-append f (map (lambda (_) "\n") (iota (length f))))))))


