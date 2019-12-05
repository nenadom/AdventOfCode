#!/usr/bin/env scheme --script

;; --- Day 4: Secure Container ---

; You arrive at the Venus fuel depot only to discover it's protected by a
; password. The Elves had written the password on a sticky note, but someone
; threw it out.
; 
; However, they do remember a few key facts about the password:
; 
;     It is a six-digit number.
;     The value is within the range given in your puzzle input.
;     Two adjacent digits are the same (like 22 in 122345).
;     Going from left to right, the digits never decrease; they only ever
;     increase or stay the same (like 111123 or 135679).
; 
; Other than the range rule, the following are true:
; 
;     111111 meets these criteria (double 11, never decreases).
;     223450 does not meet these criteria (decreasing pair of digits 50).
;     123789 does not meet these criteria (no double).
; 
; How many different passwords within the range given in your puzzle input meet
; these criteria?
; 
; Your puzzle input is 171309-643603

(load "utils.ss")


;; String -> Boolean
;; true if string contains same adjacent pair of chars
(define (adjacent-same? s)
  (define (loop last remaining)
    (if (eq? "" remaining) #f
      (let [(next (string-slice remaining 0 1))]
        (or (string=? last next)
            (loop next (string-slice remaining 1))))))
  (loop "" s))

(test (adjacent-same? "afgh") #f)
(test (adjacent-same? "") #f)
(test (adjacent-same? "122345") #t)
(test (adjacent-same? "111111") #t)

;; String -> Boolean
;; true if string representation of number contains 
;; ever increasing digits
(define (adjacent-increasing? s)
  (define (loop last remaining)
    (if (eq? "" remaining) #t
      (let [(next (string-slice remaining 0 1))]
        (and (string<=? last next)
             (loop next (string-slice remaining 1))))))
  (loop "" s))

(test (adjacent-increasing? "111111") #t)
(test (adjacent-increasing? "223450") #f)
(test (adjacent-increasing? "123789") #t)


(define INPUT-MIN 171309)
(define INPUT-MAX 643603)

(print
  (length
    (filter adjacent-increasing?
            (filter adjacent-same?
                    (map number->string
                         (map (lambda (x) (+ x INPUT-MIN))
                              (range (- INPUT-MAX INPUT-MIN))))))))


;; --- Part Two ---

; An Elf just remembered one more important detail: the two adjacent matching
; digits are not part of a larger group of matching digits.
; 
; Given this additional criterion, but still ignoring the range rule, the
; following are now true:
; 
;   - 112233 meets these criteria because the digits never decrease and all
;     repeated digits are exactly two digits long. 
;   - 123444 no longer meets the criteria (the repeated 44 is part of 
;     a larger group of 444).
;   - 111122 meets the criteria (even though 1 is repeated more than twice, 
;     it still contains a double 22).
; 
; How many different passwords within the range given in your puzzle input meet
; all of the criteria?


;; String -> (listof String)
;; Returns list of adjacent substrings in s
(define (string->adjacent-substrings s)
  (define (loop s cur rsf)
    (cond [(eq? "" s) (append rsf (list cur))]
          [(eq? "" cur) (loop (rest s) (first s) rsf)]
          [(string=? (first cur) (first s))
            (loop (rest s) (string-append cur (first s)) rsf)]
          [else
            (loop (rest s) (first s) (append rsf (list cur)))]))
  (loop s "" '()))

(test (string->adjacent-substrings "") (list ""))
(test (string->adjacent-substrings "112233") (list "11" "22" "33"))
(test (string->adjacent-substrings "123456") (list "1" "2" "3" "4" "5" "6"))


;; String -> Boolean
;; Returns true only if string contains a pair of adjacent characters
(define (has-pair? s)
  (member 2 (map string-length (string->adjacent-substrings s))))

(test (has-pair? "111122") (list 2))
(test (has-pair? "123444") #f)
(test (has-pair? "112233") (list 2 2 2))


(print
  (length
    (filter has-pair?
            (filter adjacent-increasing?
                    (filter adjacent-same?
                            (map number->string
                                 (map (lambda (x) (+ x INPUT-MIN))
                                      (range (- INPUT-MAX INPUT-MIN)))))))))
