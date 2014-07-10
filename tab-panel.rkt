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
 (lambda (parent-container l)
   (map (lambda (e) (new panel% [parent parent-container])) l)))

; (build-fields parent list-of field-info) -> list-of field objects
;   parent : container?
;   field-info : ('field-symbol "field name)
; constructs text fields for each field info thing
(define build-fields
  (lambda (parent-container field-list)
    (map (lambda (e) (new text-field% [parent parent-container] [label (cadr e)])) field-list)))
   