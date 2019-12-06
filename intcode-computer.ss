#!/usr/bin/env scheme

(load "utils.ss")


(define (upper-bound lst)
  (if (zero? (length lst)) 0 (- (length lst) 1)))


(define (intcode:arithmetic program ptr m1 m2 m3 operator)
  (let* [(a1 (list-ref program (+ 1 ptr)))
         (a2 (list-ref program (+ 2 ptr)))
         ;; Parameters that an instruction writes to 
         ;; will never be in immediate mode.
         (addr (list-ref program (+ 3 ptr)))
         (p1 (if (zero? m1) (list-ref program a1) a1))
         (p2 (if (zero? m2) (list-ref program a2) a2))]
    (list-set program addr (operator p1 p2))))


(define (intcode:set program ptr)
  (let [(in (read))
        (addr (list-ref program (+ 1 ptr)))]
    (list-set program addr (if (string? in) (string->number in) in))))


(define (intcode:get program ptr)
  (let [(addr (list-ref program (+ 1 ptr)))]
    (and (print (list-ref program addr)) program)))


(define (pad-left lst n pad)
  (if (>= (length lst) n)
    lst
    (pad-left (cons pad lst) n pad)))


(define (parse-instruction instruction)
  (pad-left (string->list (number->string instruction)) 5 #\0))

(define (A instr)
  (string->number (string (car instr))))

(define (B instr)
  (string->number (string (cadr instr))))

(define (C instr)
  (string->number (string (caddr instr))))

(define (DE instr)
  (string->number (list->string (cdddr instr))))

(define (intcode-compute program)
  (define (exec-instruction program ptr)
    (if (>= ptr (upper-bound program))
      program
      (let* [(instruction (parse-instruction (list-ref program ptr)))
             (m1 (C instruction))
             (m2 (B instruction))
             (m3 (A instruction))
             (opcode (DE instruction))]
        (case opcode
          [(1)  (exec-instruction (intcode:arithmetic program ptr m1 m2 m3 +) (+ 4 ptr))]
          [(2)  (exec-instruction (intcode:arithmetic program ptr m1 m2 m3 *) (+ 4 ptr))]
          [(3)  (exec-instruction (intcode:set program ptr) (+ 2 ptr))]
          [(4)  (exec-instruction (intcode:get program ptr) (+ 2 ptr))]
          [(99) program]))))
  (exec-instruction program 0))


(test (intcode-compute '(1 0 0 0 99)) '(2 0 0 0 99))
(test (intcode-compute '(2 3 0 3 99)) '(2 3 0 6 99))
(test (intcode-compute '(2 4 4 5 99 0)) '(2 4 4 5 99 9801))
(test (intcode-compute '(1 1 1 4 99 5 6 0 99)) '(30 1 1 4 2 5 6 0 99))

