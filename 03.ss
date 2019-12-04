#!/usr/bin/env scheme --script

(load "utils.ss")

;; --- Day 3: Crossed Wires ---

; The gravity assist was successful, and you're well on your way to the Venus
; refuelling station. During the rush back on Earth, the fuel management system
; wasn't completely installed, so that's next on the priority list.
; 
; Opening the front panel reveals a jumble of wires. Specifically, two wires
; are connected to a central port and extend outward on a grid. You trace the
; path each wire takes as it leaves the central port, one wire per line of text
; (your puzzle input).
; 
; The wires twist and turn, but the two wires occasionally cross paths. To fix
; the circuit, you need to find the intersection point closest to the central
; port. Because the wires are on a grid, use the Manhattan distance for this
; measurement. While the wires do technically cross right at the central port
; where they both start, this point does not count, nor does a wire count as
; crossing with itself.
; 
; For example, if the first wire's path is R8,U5,L5,D3, then starting from the
; central port (o), it goes right 8, up 5, left 5, and finally down 3:
; 
; ...........
; ...........
; ...........
; ....+----+.
; ....|....|.
; ....|....|.
; ....|....|.
; .........|.
; .o-------+.
; ...........
; 
; Then, if the second wire's path is U7,R6,D4,L4, it goes up 7, right 6, down
; 4, and left 4:
; 
; ...........
; .+-----+...
; .|.....|...
; .|..+--X-+.
; .|..|..|.|.
; .|.-X--+.|.
; .|..|....|.
; .|.......|.
; .o-------+.
; ...........
; 
; These wires cross at two locations (marked X), but the lower-left one is
; closer to the central port: its distance is 3 + 3 = 6.
; 
; Here are a few more examples:
; 
;     R75,D30,R83,U83,L12,D49,R71,U7,L72
;     U62,R66,U55,R34,D71,R55,D58,R83 = distance 159
;     R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
;     U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 = distance 135
; 
; What is the Manhattan distance from the central port to the closest intersection?


;; Vector is String
;; interp. a direction (Up, Right, Down, Left) and discrete quantity
(define V1 "R24")   ;; move right 24 steps
(define V2 "D1234") ;; move down 1234 steps


;; Complex Vector -> (listof Complex)
;; produce a list of points (complex numbers) resulting from moving 
;; from pt to direction (not including point of origin)
(define (move pt vec)
  ;; vector direction determines incrementing step for current movement
  (define (get-inc dir)
    (cond [(string=? dir "U") 0+i]
          [(string=? dir "R") 1]
          [(string=? dir "D") 0-i]
          [else              -1]))
  ;; recursively add points to list while steps not 0
  (define (move last inc steps pts)
    (cond [(zero? steps) pts]
          [else
            (let [(next (+ last inc))]
              (move next inc (sub1 steps) (snoc next pts)))]))
  
  (move pt
        (get-inc (string-slice vec 0 1))
        (string->number (string-slice vec 1))
        '()))

(test (move 0 "L0") '())
(test (move 0 "R8") (list 1 2 3 4 5 6 7 8))
(test (move 8 "U5") (list 8+i 8+2i 8+3i 8+4i 8+5i))
(test (move -1-3i "D3") (list -1-4i -1-5i -1-6i))


;; (listof Vector) -> (listof Complex)
;; produce list of all points from list of vector movement directions
(define (move-all vecs)
  (define (move-all origin vecs rsf)
    (cond [(empty? vecs) rsf]
          [else
            (let [(this-move (move origin (car vecs)))]
              (move-all (car (reverse this-move))
                        (cdr vecs)
                        (append rsf this-move)))]))
  (move-all 0 vecs '()))

(define example-a (move-all (string-split "R8,U5,L5,D3" #\,)))
(define example-b (move-all (string-split "U7,R6,D4,L4" #\,)))


;; I mean how cool is this?? It's a Unicode symbol as a function name
(define (∩ lsta lstb)
  (filter (lambda (m) (member m lstb)) lsta))


;; Complex -> Integer
;; produce absolute distance of point, represented as pair, from origin
(define (|pt| pt)
  (+ (abs (real-part pt)) (abs (imag-part pt))))


;; (listof Integer) -> Integer
;; Now we just find the closest intersecting point
(define (closest distances)
  (car (sort < distances)))


;; Compose: 
;; - String -> (listof Vector)
;; - (listof Vector) -> (listof Complex)
(define (string->moves s)
  (move-all (string-split s #\,)))


;; String String -> Integer
;; 1. Convert strings to (listof Vector)
;; 2. Execute all moves (listof Vector) -> (listof Complex)
;; 3. Find intersecting points
;; 4. Produce closest intersection
(define (smallest-manhattan-distance s1 s2)
  (closest (map |pt| (∩ (string->moves s1)
                        (string->moves s2)))))

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
;   (let [(input (read-file "inputs/03.txt"))]
;     (smallest-manhattan-distance (car input) (cadr input))))

(define input (read-file "inputs/03.txt"))
(define wire-1 (string->moves (car input)))
(define wire-2 (string->moves (cadr input)))

(define (abs< a b)
  (< (|pt| a) (|pt| b)))


;; Does the same, but intersects closest points
;; `closest` is an expanding manhattan radius, growing by order 
;; of magnitude if intersection empty
(define (smallest-manhattan-distance-expanding w1 w2)
  (let [(sorted-w1 (sort abs< w1))
        (sorted-w2 (sort abs< w2))]
    (define (iter intersection radius)
      (if (empty? intersection)
        (iter (∩ (take sorted-w1 radius) (take sorted-w2 radius)) (* 10 radius))
        (closest (map |pt| intersection))))
    (iter '() 1)))

(display (smallest-manhattan-distance-expanding wire-1 wire-2)) ; 303


;; --- Part Two ---

; It turns out that this circuit is very timing-sensitive; you actually need to
; minimize the signal delay.
; 
; To do this, calculate the number of steps each wire takes to reach each
; intersection; choose the intersection where the sum of both wires' steps is
; lowest. If a wire visits a position on the grid multiple times, use the steps
; value from the first time it visits that position when calculating the total
; value of a specific intersection.
; 
; The number of steps a wire takes is the total number of grid squares the wire
; has entered to get to that location, including the intersection being
; considered. Again consider the example from above:
; 
; ...........
; .+-----+...
; .|.....|...
; .|..+--X-+.
; .|..|..|.|.
; .|.-X--+.|.
; .|..|....|.
; .|.......|.
; .o-------+.
; ...........
; 
; In the above example, the intersection closest to the central port is reached
; after 8+5+5+2 = 20 steps by the first wire and 7+6+4+3 = 20 steps by the
; second wire for a total of 20+20 = 40 steps.
; 
; However, the top-right intersection is better: the first wire takes only
; 8+5+2 = 15 and the second wire takes only 7+6+2 = 15, a total of 15+15 = 30
; steps.
; 
; Here are the best steps for the extra examples from above:
; 
;     R75,D30,R83,U83,L12,D49,R71,U7,L72
;     U62,R66,U55,R34,D71,R55,D58,R83 = 610 steps
;     R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
;     U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 = 410 steps
; 
; What is the fewest combined steps the wires must take to reach an intersection?


;; Winged it here with 10000 because finding an intersection is 
;; exponentially complex, and it's almost certain the point with fewest
;; combined steps will be the first point of intersection (of either wire).
;; Additionally, add 2 because we're using 0-based index, twice.
(define (fewest-combined-steps w1 w2)
  (let [(intersections (∩ (take w1 10000) (take w2 10000)))]
    (+ 2 (index (car intersections) w1) (index (car intersections) w2))))

(test (fewest-combined-steps (string->moves "R8,U5,L5,D3")
                             (string->moves "U7,R6,D4,L4"))
      30)

(test (fewest-combined-steps (string->moves "R75,D30,R83,U83,L12,D49,R71,U7,L72")
                             (string->moves "U62,R66,U55,R34,D71,R55,D58,R83"))
      610)

(test (fewest-combined-steps (string->moves "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51")
                             (string->moves "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"))
      410)

(print
  (fewest-combined-steps wire-1 wire-2)) ;; 11222

