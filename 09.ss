#!/usr/bin/env scheme --script

(load "intcode-computer.ss")

(set! RELATIVE-BASE 0)

(define BOOST 
  (string->program (car (read-file "inputs/09.txt"))))

(intcode-compute (add-memory BOOST 1024))

