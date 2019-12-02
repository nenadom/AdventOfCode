#!/usr/bin/env scheme

;; File -> (listof String)
;; produce list where each string is one line in file
(define (file->list f)
  (define (file->list ln lst)
    (cond [(eq? #!eof ln) (reverse lst)]
          [else
            (file->list (get-line f) (cons ln lst))]))
  (file->list (get-line f) '()))


;; String -> (listof String)
;; produce list where each string is one line from file name
(define (read-file fname)
  (file->list (open-input-file fname)))


(define (print s)
  (display (string-append (if (number? s) (number->string s) s) "\n")))
