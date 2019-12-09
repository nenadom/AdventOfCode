#!/usr/bin/env scheme

;; https://www.scheme.com/tspl4/syntax.html#./syntax:s70
;; https://en.wikibooks.org/wiki/Scheme_Programming/Macros
(define-syntax define-structure
  (lambda (x)
    (define gen-id
      (lambda (template-id . args)
        (datum->syntax template-id
                       (string->symbol
                         (apply string-append
                                (map (lambda (x)
                                       (if (string? x)
                                         x
                                         (symbol->string (syntax->datum x))))
                                     args))))))
    (syntax-case x ()
                 [(_ name field ...)
                  (with-syntax ([constructor (gen-id #'name "make-" #'name)]
                                [predicate (gen-id #'name #'name "?")]
                                [(access ...)
                                 (map (lambda (x) (gen-id x #'name "-" x))
                                      #'(field ...))]
                                [(assign ...)
                                 (map (lambda (x)
                                        (gen-id x "set-" #'name "-" x "!"))
                                      #'(field ...))]
                                [structure-length (+ (length #'(field ...)) 1)]
                                [(index ...)
                                 (let f ([i 1] [ids #'(field ...)])
                                   (if (null? ids)
                                     '()
                                     (cons i (f (+ i 1) (cdr ids)))))])
                               #'(begin
                                   (define constructor
                                     (lambda (field ...)
                                       (vector 'name field ...)))
                                   (define predicate
                                     (lambda (x)
                                       (and (vector? x)
                                            (= (vector-length x) structure-length)
                                            (eq? (vector-ref x 0) 'name))))
                                   (define access
                                     (lambda (x)
                                       (vector-ref x index)))
                                   ...
                                   (define assign
                                     (lambda (x update)
                                       (vector-set! x index update)))
                                   ...))])))

