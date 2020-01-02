#!/usr/bin/env scheme

(load "utils.ss")


(define (upper-bound lst)
  (if (zero? (length lst)) 0 (- (length lst) 1)))


(define RELATIVE-BASE 0)


(define (get-param program mode value)
  (cond [(= 0 mode) (list-ref program value)]    ; position mode
        [(= 1 mode) value]                       ; immediate mode
        [(= 2 mode) (list-ref program (+ RELATIVE-BASE value))]))  ; relative mode TODO


(define (intcode:arithmetic program ptr m1 m2 m3 operator)
  (let* [(a1 (list-ref program (+ 1 ptr)))
         (a2 (list-ref program (+ 2 ptr)))
         ;; Parameters that an instruction writes to 
         ;; will never be in immediate mode.
         (addr (list-ref program (+ 3 ptr)))]
    (list-set program
              (if (= 2 m3)
                (+ RELATIVE-BASE addr)
                addr)
              (operator (get-param program m1 a1)
                        (get-param program m2 a2)))))


(define (intcode:set program ptr m read-port)
  (let [(in (read-port))]
    (cond
      [(= 0 m) (list-set program (list-ref program (+ 1 ptr)) in)]
      [(= 2 m) (list-set program (+ RELATIVE-BASE (list-ref program (+ 1 ptr))) in)]
      [else program])))


(define (intcode:get program ptr m write-port)
  ;(let [(addr (list-ref program (+ 1 ptr)))]
  (let [(addr (get-param program m (list-ref program (add1 ptr))))]
    (and (write-port addr)
         program)))


(define (intcode:jump program ptr m1 m2 predicate)
  (let* [(a1 (list-ref program (+ 1 ptr)))
         (a2 (list-ref program (+ 2 ptr)))]
    (if (predicate (get-param program m1 a1)) (get-param program m2 a2) (+ 3 ptr))))


(define (intcode:set-relative-base! program ptr m)
  (let [(a (list-ref program (+ 1 ptr)))]
    (and
      (set! RELATIVE-BASE (+ RELATIVE-BASE (get-param program m a)))
      program)))


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


(define (intcode-compute program . buffers)
  (let [(buf:read
          (if (= 2 (length buffers)) (car buffers)
            read))
        (buf:write
          (if (= 2 (length buffers)) (cadr buffers)
            print))]

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
            [(3)  (exec-instruction (intcode:set program ptr m1 buf:read) (+ 2 ptr))]
            [(4)  (exec-instruction (intcode:get program ptr m1 buf:write) (+ 2 ptr))]
            [(5)  (exec-instruction program (intcode:jump program ptr m1 m2 (lambda (x) (not (zero? x)))))] ; jump-if-true
            [(6)  (exec-instruction program (intcode:jump program ptr m1 m2 zero?))] ; jump-if-false
            [(7)  (exec-instruction (intcode:arithmetic program ptr m1 m2 m3 (lambda (x y) (if (< x y) 1 0))) (+ 4 ptr))] ; less than
            [(8)  (exec-instruction (intcode:arithmetic program ptr m1 m2 m3 (lambda (x y) (if (= x y) 1 0))) (+ 4 ptr))] ; equals
            [(9)  (exec-instruction (intcode:set-relative-base! program ptr m1) (+ 2 ptr))]
            [(99) program]))))
      (exec-instruction program 0)))


(define (add-memory program bytes)
  (append program (map (lambda (x) 0) (iota bytes))))

(test (intcode-compute '(1 0 0 0 99)) '(2 0 0 0 99))
(test (intcode-compute '(2 3 0 3 99)) '(2 3 0 6 99))
(test (intcode-compute '(2 4 4 5 99 0)) '(2 4 4 5 99 9801))
(test (intcode-compute '(1 1 1 4 99 5 6 0 99)) '(30 1 1 4 2 5 6 0 99))
(set! RELATIVE-BASE 2000)
(intcode-compute '(109 19))
(test RELATIVE-BASE 2019)
; (intcode-compute (add-memory '(204 -34) 2000)) ; outputs 0


(set! RELATIVE-BASE 0)

(display "Testing...\n")

(define test-p1
  (add-memory 
    (string->program
      "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99")
    128))
;; produce copy of itself
(intcode-compute test-p1)


(define test-p2
  (add-memory
    (string->program
      "1102,34915192,34915192,7,4,7,99,0")
    256))
;; produce 16-digit number
(intcode-compute test-p2)


(define test-p3 (string->program "104,1125899906842624,99"))
; produce number in middle
(intcode-compute test-p3)

;(set! RELATIVE-BASE 0)
;(display "outputs -1:\n")
;(intcode-compute '(109  -1 4 1 99))
;
;(set! RELATIVE-BASE 0)
;(display "outputs 1:\n")
;(intcode-compute '(109  -1 104 1 99))
;
;(set! RELATIVE-BASE 0)
;(display "outputs 109:\n")
;(intcode-compute '(109  -1 204 1 99))
;
;(set! RELATIVE-BASE 0)
;(display "outputs 204:\n")
;(intcode-compute '(109 1 9 2 204 -6 99))
;
;(set! RELATIVE-BASE 0)
;(display "outputs 204:\n")
;(intcode-compute '(109 1 109 9 204 -6 99))
;
;(set! RELATIVE-BASE 0)
;(display "outputs 204:\n")
;(intcode-compute '(109 1 209 -1 204 -106 99))
;
;(set! RELATIVE-BASE 0)
;(display "outputs input:\n")
;(intcode-compute '(109 1 3 3 204 2 99))
;
;(set! RELATIVE-BASE 0)
;(display "outputs input:\n")
;(intcode-compute '(109 1 203 2 204 2 99))
