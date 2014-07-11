#lang racket
(require racket/gui)
(define cur-tab-list '(("general" ('name "name") ('c-date "date collected"))
                       ("harbin" ('r-date "run date") ('r-time "run time"))
                       ("angle" ('cs "Cs137") ('pb "Pb210") ('be "Be7"))))
(define test-frame (new frame% [label "test"]))
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

; (populate-tab-panels list-of panels list-of tab-settings) -> list-of list-of fields)
; constructs text fields for all the pannels
(define populate-tab-panels
  (lambda (panel-list tab-list)
    (map build-fields panel-list (map cdr tab-list))))

; (construct-tab-panel parent tab-settings) -> tab-panel)
(define construct-tab-panel
  (lambda (parent-container tab-list)
    (new tab-panel%
         [choices (map car tab-list)]
         [parent parent-container])))

; (get-field-values field-list) -> value-list
;   field-list : list-of fields
;   value-list : list-of strings
(define get-field-values
  (lambda (field-list)
    (map (lambda (f) (send f get-value)) field-list)))

; (save-cur-value tab-id field-list value-hash) -> void?
;   tab-id : number
;   field-list : list-of fields
;   value-hash : hash
(define save-cur-values
  (lambda (tab-id field-list value-hash)
    (hash-set! value-hash tab-id (get-field-values field-list))))

;(define show-tab
 ; (lambda (tab-panel cur-tab panels)
    