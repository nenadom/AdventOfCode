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


;; Display string (or number) with newline after it
(define (print s)
  (display (string-append (if (number? s) (number->string s) s) "\n")))


;; String Char -> (listof String)
;; produce list of substrings from str split with tok
(define (string-split str tok)
  (define (string-split pos los acc)
    (cond [(= pos (string-length str)) (append los (list acc))]
          [else
            (let [(c (string-ref str pos))]
              (if (char=? c tok)
                (string-split (add1 pos)
                              (append los (list acc))
                              "")
                (string-split (add1 pos)
                              los
                              (string-append acc (string c)))))]))
  (string-split 0 '() ""))


(assert (equal?
          (string-split "a,bc,def" #\,)
          (list "a" "bc" "def")))
(assert (equal? 
          (string-split "1:0:0:3:1:1:2" #\:) 
          (list "1" "0" "0" "3" "1" "1" "2")))
(assert (equal?
          (string-split "123 567 789" #\ )
          (list "123" "567" "789")))


;; String -> String | (listof T) -> T
;; Returns first element of either string or list
(define (first x)
  (cond [(string? x) (if (eq? "" x) "" (string-slice x 0 1))]
        [(list? x) (car x)]
        [else
          (and (display x)
               (raise
                 (condition
                   (make-error)
                   (make-message-condition
                     (string-append
                       "first called with above ^^;\n"
                       "should be called with list or string")))))]))


;; String -> String | (listof T) -> (listof T)
;; Returns remaining elements after first of either string or list
(define (rest x)
  (cond [(string? x) (if (eq? "" x) "" (string-slice x 1))]
        [(list? x) (cdr x)]
        [else
          (and (display x)
               (raise
                 (condition
                   (make-error)
                   (make-message-condition
                     (string-append
                       "rest called with above ^^;\n"
                       "should be called with list or string")))))]))


;; Alias empty list
(define empty '())

;; (listof X) -> Boolean
;; produce true if list empty
(define (empty? lst)
  (equal? '() lst))


;; (listof X) Integer X -> (listof X)
;; produce new list from lst such that at pos value is x
;; if pos out of bounds for lst, there's no change to lst
(define (list-set lst pos x)
  (cond [(zero? pos) (cons x (rest lst))]
        [(empty? lst) empty]
        [else
          (cons (first lst) (list-set (rest lst) (sub1 pos) x))]))


;; X (listof X) -> (listof X)
;; like cons, but in reverse
(define (snoc x lox)
  (reverse (cons x (reverse lox))))

(assert (equal? (snoc 4 (list 1 2 3)) (list 1 2 3 4)))
(assert (equal? (snoc "z" (list "x" "y")) (list "x" "y" "z")))


(define (test a b)
  (assert (equal? a b)))


;; Integer -> (listof Integer)
;; produce a range of integers [0, n) (up to n exclusive)
(define (range n)
  (define (range i acc)
    (cond [(= i n) (reverse acc)]
          [else
            (range (add1 i) (cons i acc))]))
  (range 0 '()))


;; String Number [Number] -> String
;; produce substring of str with start [, end] params
(define (string-slice str start . end)
  (substring str start (if (empty? end) (string-length str) (car end))))


;; (listof X) Natural -> (listof X)
;; return list of first n elements in lst
(define (take lst n)
  (define (take lst n rsf)
    (cond [(empty? lst) rsf]
          [(zero? n) rsf]
          [else
            (take (cdr lst) (sub1 n) (snoc (car lst) rsf))]))
  (take lst n '()))


;; X (listof X) -> Natural|False
;; produce index of el in lst, #f if not found
(define (index el lst)
  (let [(tail (member el (reverse lst)))]
    (and tail (length (cdr tail)))))


;; TODO refactor last to work with strings
(define (last lst)
  (car (reverse lst)))

