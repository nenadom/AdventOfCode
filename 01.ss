#!/usr/bin/env scheme --script

(load "utils.ss")

;; https://adventofcode.com/2019/day/1
;; --- Day 1: The Tyranny of the Rocket Equation ---

(define (mass->fuel m)
  (- (floor (/ m 3)) 2))

(assert (= (mass->fuel 12) 2))
(assert (= (mass->fuel 14) 2))
(assert (= (mass->fuel 1969) 654))
(assert (= (mass->fuel 100756) 33583))

;; 3464458
(define module-fuel
  (fold-left + 0
             (map mass->fuel 
                  (map string->number (read-file "01.txt")))))

(print module-fuel)

;; --- Part Two ---

(define (total-fuel f)
  (define (total-fuel total last)
    (cond [(<= last 0) total]
          [else
            (total-fuel (+ total last) (mass->fuel last))]))
  (total-fuel 0 (mass->fuel f)))

(assert (= (total-fuel 14) 2))
(assert (= (total-fuel 1969) 966))
(assert (= (total-fuel 100756) 50346))


;; calculate the fuel requirements for each module separately, then add them up
;; at the end

(define (module+fuel m)
  (let [(fuel (mass->fuel m))]
    (+ fuel (total-fuel fuel))))

(print
  (fold-left + 0
             (map
               (lambda (s)
                 (module+fuel (string->number s)))
               (read-file "01.txt"))))
;; 5193796

