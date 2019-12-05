#!/usr/bin/env scheme

(load "utils.ss")


;; Program is (listof Integer)

;; Program -> Program
;; Returns a program after running all operations on it
(define (intcode-compute program)
  (define (intcode-compute program! pos)
    (cond [(>= pos (- (length program!) 1)) program!]
          [else
            (let [(opcode (list-ref program! pos))]
              (if (= 99 opcode)
                program!
                (let [(p1 (list-ref program! (list-ref program! (+ 1 pos))))
                      (p2 (list-ref program! (list-ref program! (+ 2 pos))))
                      (p3 (list-ref program! (+ 3 pos)))]
                  (intcode-compute (list-set program!
                                             p3
                                             (if (= 1 opcode)
                                               (+ p1 p2)
                                               (* p1 p2)))
                                   (+ 4 pos)))))]))
  (intcode-compute program 0))


(test (intcode-compute '(1 0 0 0 99)) '(2 0 0 0 99))
(test (intcode-compute '(2 3 0 3 99)) '(2 3 0 6 99))
(test (intcode-compute '(2 4 4 5 99 0)) '(2 4 4 5 99 9801))
(test (intcode-compute '(1 1 1 4 99 5 6 0 99)) '(30 1 1 4 2 5 6 0 99))


