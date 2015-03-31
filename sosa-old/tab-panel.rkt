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

; (make-field-hash parent tab-list) -> hash
;   parent : container?
;   tab-list: (("name" ((field-symbol "field name")...))...)
; takes in a parent and a list of tabs, constructs the fields in a hash
(define make-field-hash
  (lambda (parent-container tab-list)
    (letrec ([field-hash (make-hash)]
             [f (lambda (id f-list)
                  (if (null? f-list)
                      field-hash
                      (begin
                        (hash-set! field-hash id (car f-list))
                        (f (+ id 1) (cdr f-list)))))])
      (f 0 (map (lambda (l) (build-fields parent-container l)) (map cdr tab-list))))))

(define set-up-tabs
  (lambda (parent-container tab-list)
    (let ([tab-panel (construct-tab-panel parent-container tab-list)])
      (list tab-panel (make-field-hash tab-panel tab-list) (make-hash)))))
            
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
  (lambda (tab-id field-hash value-hash)
    (hash-set! value-hash tab-id 
               (get-field-values (hash-ref field-hash tab-id)))))

;(define show-tab