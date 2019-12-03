#!/usr/bin/env scheme --script

(load "utils.ss")

;; Point is (Integer . Integer)
;; interp. a pair of coordinates representing a single point on a plane
(define P0 '(00 . 00)) ;; origin
(define P1 '(12 . 00)) ;; on x axis
(define P2 '(00 . -4)) ;; on y axis
(define P3 '(-8 . -5)) ;; 3rd quadrant


;; Vector is String
;; interp. a direction (Up, Right, Down, Left) and discrete quantity
(define V1 "R24")   ;; move right 24 steps
(define V2 "D1234") ;; move down 1234 steps


;; Point Vector -> (listof Point)
;; produce a list of points resulting from moving from pt to direction
;; (not including point of origin)
(define (move pt vec)
  (define (next-point pt dir steps)
    (cond [(string=? dir "U") (cons       (car pt)  (add1 (cdr pt)))]
          [(string=? dir "R") (cons (add1 (car pt))       (cdr pt))]
          [(string=? dir "D") (cons       (car pt)  (sub1 (cdr pt)))]
          [else               (cons (sub1 (car pt))       (cdr pt))]))
  (define (move last dir steps pts)
    (cond [(zero? steps) pts]
          [else
            (let [(next (next-point last dir steps))]
              (move next dir (sub1 steps) (snoc next pts)))]))
  (move pt
        (string-slice vec 0 1)
        (string->number (string-slice vec 1))
        '()))

(test (move P0 "L0") '())
(test (move P0 "R8")
      (list '(1 . 0) '(2 . 0) '(3 . 0) '(4 . 0) '(5 . 0) '(6 . 0) '(7 . 0) '(8 . 0)))
(test (move (cons 8 0) "U5")
      (list '(8 . 1) '(8 . 2) '(8 . 3) '(8 . 4) '(8 . 5)))
(test (move (cons -1 -3) "D3")
      (list (cons -1 -4) (cons -1 -5) (cons -1 -6)))


;; (listof Vector) -> (listof Point)
;; produce list of all points from list of vector movement directions
(define (move-all vecs)
  (define (move-all origin vecs rsf)
    (cond [(empty? vecs) rsf]
          [else
            (let [(this-move (move origin (car vecs)))]
              (move-all (car (reverse this-move))
                        (cdr vecs)
                        (append rsf this-move)))]))
  (move-all (cons 0 0) vecs '()))

(define example-a (move-all (string-split "R8,U5,L5,D3" #\,)))
(define example-b (move-all (string-split "U7,R6,D4,L4" #\,)))


;; I mean how cool is this?? It's a Unicode symbol as a function name!
(define (∩ lsta lstb)
  (filter (lambda (m) (member m lstb)) lsta))


;; Point -> Integer
;; produce absolute distance of point from origin
(define (|pt| pt)
  (+ (abs (car pt)) (abs (cdr pt))))


;; (listof Integer) -> Integer
;; Now we just find the closest intersecting point
(define (closest distances)
  (car (sort < distances)))


;; String String -> Integer
;; 1. Convert strings to (listof Vector)
;; 2. Execute all moves (listof Vector) -> (listof Point)
;; 3. Find intersecting points
;; 4. Produce closest intersection
(define (smallest-manhattan-distance s1 s2)
  (closest (map |pt| (∩ (move-all (string-split s1 #\,))
                        (move-all (string-split s2 #\,))))))

(test (smallest-manhattan-distance 
        "R75,D30,R83,U83,L12,D49,R71,U7,L72"
        "U62,R66,U55,R34,D71,R55,D58,R83")
      159)
(test (smallest-manhattan-distance
        "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
        "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")
      135)

; Prints 303; horribly slow to run
; (print
;   (let [(input (read-file "03.txt"))]
;     (smallest-manhattan-distance (car input) (cadr input))))


