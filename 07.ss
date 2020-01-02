#!/usr/bin/env scheme --script


(load "intcode-computer.ss")

(define p1 (string->program "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"))


(define BUFFER '())

(define (read-buffer!)
  (let [(x (car BUFFER))]
    (and (set! BUFFER (cdr BUFFER))
         x)))

(define (write-buffer! x)
  (set! BUFFER (cons x BUFFER)))


;; this program outputs whatever input it is given
(write-buffer! 42)
(intcode-compute '(3 0 4 0 99) read-buffer! write-buffer!)
(test BUFFER (list 42))


;; Example 1:
(set! BUFFER (list 4 0))
(define p2 (intcode-compute p1 read-buffer! write-buffer!))
(write-buffer! 3)
(define p3 (intcode-compute p2 read-buffer! write-buffer!))
(write-buffer! 2)
(define p4 (intcode-compute p3 read-buffer! write-buffer!))
(write-buffer! 1)
(define p5 (intcode-compute p4 read-buffer! write-buffer!))
(write-buffer! 0)
(define p6 (intcode-compute p5 read-buffer! write-buffer!))

(display (car BUFFER)) ;; 43210


;; (listof Integer) (listof Integer[0, 4]) -> Integer
;; produce thruster output of setting amps to given phase-settings;
;; initial thruster input is set to 0, output read from BUFFER.
(define (amp-circuit program phase-settings)
  (define (amp p)
    (intcode-compute p read-buffer! write-buffer!))
  (define (loop program phase-settings)
    (if (null? phase-settings)
      program
      (and (write-buffer! (car phase-settings))
;           (display BUFFER)
;           (display "\n")
           (loop (amp program) (cdr phase-settings)))))
  (and (set! BUFFER (list 0))
       (loop program phase-settings)
       (car BUFFER)))


(test (amp-circuit p1 '(4 3 2 1 0)) 43210)
(test (amp-circuit
        (string->program "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0")
        '(0 1 2 3 4))
      54321)
(test (amp-circuit
        (string->program "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0")
        '(1 0 4 3 2))
      65210)


(define (pset n)
  (fold-left append '()
             (fold-left append '()
                        (fold-left append '()
                                   (fold-left append '()
                                              (map (lambda (i)
                                                     (map (lambda (j)
                                                            (map (lambda (k)
                                                                   (map (lambda (l)
                                                                          (map (lambda (m)
                                                                                 (list i j k l m))
                                                                               (iota n)))
                                                                        (iota n)))
                                                                 (iota n)))
                                                          (iota n)))
                                                   (iota n)))))))


(define program (string->program (car (read-file "inputs/07.txt"))))

(define (find-highest program)
  (let [(thruster-outputs
          (map (lambda (phases)
                 (amp-circuit program phases))
               (pset 5)))]
    (car (sort > thruster-outputs))))

(print
  (find-highest program))

