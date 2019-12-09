#!/usr/bin/env scheme --script


(load "intcode-computer.ss")

(define p1
  (map string->number
       (string-split "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0" #\,)))

(define p2 (intcode-compute p1))
(define p3 (intcode-compute p2))
(define p4 (intcode-compute p3))
(define p5 (intcode-compute p4))
(define p6 (intcode-compute p5))

(display p6)

