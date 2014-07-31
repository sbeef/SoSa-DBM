#lang racket
(require racket/gui/base)
; Sample is
; - a list of sample elements

; a sample element is
; - (field-name value)

(define render-sample-element
  (lambda (sample-element container)
    (new message% 
         [label (cadr sample-element)]
         [parent (new panel% 
                      [parent container]
                      [style (list 'border)])])))

(define render-sample
  (lambda (sample container)
    (let ([row (new horizontal-panel% [parent container])])
      (map (lambda (x) (render-sample-element x row)) sample))))

(define render-sample-table
  (lambda (sample-list container)
    (let ([table (new vertical-panel% [parent container])])
      (map (lambda (x) (render-sample x table)) sample-list))))

(define frame (new frame% [label "example"]))
(define test-sample-list '((("name" "ch-106") ("date" "5/26/13") ("run" "7/2/14"))
                      (("name" "ch-107") ("date" "5/26/13") ("run" "7/3/14"))
                      (("name" "ch-108") ("date" "5/26/13") ("run" "7/7/14"))
                      (("name" "ch-109") ("date" "5/26/13") ("run" ""))
                      (("name" "TRR-12") ("date" "1/14/14") ("run" "7/11/14"))))
  