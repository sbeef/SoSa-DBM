#lang racket
(define (render-element element type)
  `(,type ,element))

(define (render-row fields type)
  `(tr 