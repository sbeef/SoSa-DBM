#lang racket
(require racket/gui/base)
(require "strinfo.rkt")

(define frame (new frame% [label "example"]))

; takes in any number n, creates a list with a number of entries 
; equal to n, all of which are 0
(define create-zeros
  (lambda (n)
    (cond
      [(= 0 n) '()]
      [else (cons '0 (create-zeros (- n 1)))])))
; takes in a number representing a string length, and converts it
; to a width for a window, based on a rough approximation
(define text-length-to-width
  (lambda (n)
    (+ 5 (* n 11))))

; the table of data
(define TABLE-VALUES '(
                       ("Name"   "Date Aquired" "Date Run")
                       ("CH-107" "5/26/13"      "7/02/14")
                       ("CH-108" "5/26/13"      "7/03/14")
                       ("CH-109" "5/26/13"      "7/04/14")
                       ("CH-110" "5/26/13"      "")
                       ("TRR-22" "1/12/14"      "7/08/14")))

; the number of columns in the table
(define COLUMN-NUM (length (car TABLE-VALUES)))
; the size of each column
(define COLUMN-WIDTHS (get-longest-strings TABLE-VALUES (create-zeros COLUMN-NUM)))
(define COLUMN-HEIGHT 17)

(define render-table-entry
  (lambda (value width height container)
    (new canvas% 
         [parent container]
         [min-width width]
         [min-height height]
         [paint-callback
          (lambda (canvas dc)
            (send dc draw-rectangle 0 0 width height)
            (send dc draw-text value 1 0))])))
(define render-table-row
  (lambda (row container)
    (let ([row-container (new horizontal-panel% [parent container])])
      (map (lambda (v w) (render-table-entry v w COLUMN-HEIGHT row-container)) row COLUMN-WIDTHS))))

