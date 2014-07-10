#lang racket
(require racket/gui)

; (build-tabs parent tab-list) -> tab-frame
;   parent : container?
;   tab-list: (("name" ((field-symbol "field name")...))...)


; (build-panels parent list) -> list-of panels
;   parent : container?
;   list : any list
; constructs a number of panels equal to the length of the list, returns the panels in a list
(define build-panels
 (lambda (parent-container lat)
   (letrec ([f (lambda (l)
                 (cond
                   [(null? l) '()]
                   [(cons (new panel$ [parent parent-container])
                          (f (cdr l)))]))])
     (f lat))))
   