#lang racket

(define table-parameters '((name "Sample Name") (c-date "Collection Date") (r-date "Run Date")))

(define test-sample '((name "CH-151") (c-date "07-08-15") (r-date "20-10-15")))

; a 'bit' is an html element.  a 'div bit' would be any element wrapped in a div,
; e.g. <div>Some Text</div>

; render-div takes in a bit element ("some text" in the example above) and a bit 
; type (div, in the example above)

(define (render-bit bit-type bit-element)
  `(,bit-type bit-element))

; so, along those lines, a table data (<td>) element is just a td bit,
; and so constructing an entry in a table is just rendering a bit,
(define (render-table-data data-element)
  (render-bit 'td data-element))

; rendering a row is also essentially a bit call, but in this case the element to be 
; wrapped in the bit is a list of other bits.  so 
