#lang racket
(require racket/gui/base)
(provide get-longest-strings)
(define get-text-w-h
  (lambda (string)
    (let* ([TEST (new frame% [label "test"])]
           [MESSAGE (new message% [label string] [parent TEST])])
      (cons (send MESSAGE get-width)
            (send MESSAGE get-height)))))

; takes in a list of list of strings, (representing a table) returns
; which strings are the longest in each column
(define get-longest-strings
  (lambda (table longests)
    (let ([lengths (lambda (x) (map (lambda (y) (car (get-text-w-h y))) x))]
          [compare (lambda (x y) (map max x y))])
      (cond
        [(null? table) longests]
        [else (get-longest-strings (cdr table) (compare (lengths (car table)) longests))]))))
