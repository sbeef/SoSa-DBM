#lang racket
(require racket/gui)
(provide make-tab-panel-obj)
(provide save-current-values)
(define make-tab-panel-obj
  (lambda (parent-container tab-settings)
    (let* ([tab-hash (make-hash)]
           [cur-hash (make-hash)]
           [value-hash (make-hash)]
           [field-info-hash (make-hash)]
           [tab-obj (list '() 
                          cur-hash
                          (set-blank-hash value-hash tab-settings)
                          (setup-field-hash field-info-hash tab-settings))]
           [t-panel (new tab-panel%
                         [choices (map car tab-settings)]
                         [parent parent-container]
                         [callback (lambda (tab-panel event)
                                     (show-new-tab 
                                      (cons tab-panel (cdr tab-obj))))])])
      (begin
        (hash-set! cur-hash 0 0)
        (draw-current-tab (cons t-panel (cdr tab-obj)))
        (cons t-panel (cdr tab-obj))))))

(define show-new-tab
  (lambda (tab-obj)
    (let ([tab-panel (car tab-obj)])
      (begin
        (save-current-values tab-obj)
        (clear-tab tab-obj)
        (hash-set! (cadr tab-obj) 0 (send tab-panel get-selection))
        (draw-current-tab tab-obj)))))

(define set-cur-tab
  (lambda (tab-obj new-id)
    (display (cons (car tab-obj) (cons new-id (cddr tab-obj))))))

; takes in a list of fields, and list of values, loads corresponding values
(define load-saved-data
  (lambda (field-list value-list)
    (cond
      [(null? field-list)]
      [(null? value-list)]
      [else (begin
              (send (car field-list) set-value (car value-list))
              (load-saved-data (cdr field-list) (cdr value-list)))])))
; (build-fields parent list-of field-info) -> list-of field objects
;   parent : container?
;   field-info : ('field-symbol "field name)
; constructs text fields for each field info thing
(define build-fields
  (lambda (parent-container field-list)
    (map (lambda (e) (new text-field% [parent parent-container] [label (cadr e)])) field-list)))

; loads a tab obj, draws fields based on whatever the "current tab" is, loads saved values
(define draw-current-tab
  (lambda (tab-obj)
    (let* ([tab-pannel (car tab-obj)]
           [cur-tab (hash-ref (cadr tab-obj) 0)]
           [field-values (hash-ref (caddr tab-obj) cur-tab)]
           [field-info (hash-ref (cadddr tab-obj) cur-tab)])
      (load-saved-data (build-fields tab-pannel field-info) field-values))))

; saves the values of the current fields in the value hash
(define save-current-values
  (lambda (tab-obj)
    (let* ([tab-panel (car tab-obj)]
           [cur-tab (hash-ref (cadr tab-obj) 0)]
           [children (send tab-panel get-children)]
           [value-hash (caddr tab-obj)])
      (hash-set! value-hash cur-tab (map (lambda (f) (send f get-value)) children)))))
; deletes all children of a tab obj
(define clear-tab
  (lambda (tab-obj)
    (let ([panel (car tab-obj)])
      (map (lambda (x) (send panel delete-child x)) (send panel get-children)))))

; takes and a hash and a list, sets an entry in the hash 
; for each list entry to the null list
; returns that hash
(define set-blank-hash
  (lambda (hash list)
    (letrec ([f (lambda (id list)
                  (cond
                    [(null? list) hash]
                    [else (begin
                            (hash-set! hash id '())
                            (f (+ id 1) (cdr list)))]))])
      (f 0 list))))
; takes in a hash, and a list of tab-settings
; returns the hash with each entry corresponding to field-settings of tab settings
(define setup-field-hash
  (lambda (field-info-hash tab-settings)
    (letrec ([field-settings (map cdr tab-settings)]
             [f (lambda (id list)
                  (cond
                    [(null? list) field-info-hash]
                    [else (begin
                            (hash-set! field-info-hash id (car list))
                            (f (+ id 1) (cdr list)))]))])
      (f 0 field-settings))))

(define test-frame (new frame% [label "test"]))
(define cur-tab-list '(("general" ('name "name") ('c-date "date collected"))
                       ("harbin" ('r-date "run date") ('r-time "run time"))
                       ("angle" ('cs "Cs137") ('pb "Pb210") ('be "Be7"))))