#lang racket
(require racket/gui)

; the hash-table of tab lists (eventually will be auto-built from a config file
(define cur-tab-list '(("general" ('name "name") ('c-date "date collected"))
                       ("harbin" ('r-date "run date") ('r-time "run time"))
                       ("angle" ('cs "Cs137") ('pb "Pb210") ('be "Be7"))))

; the frame where everything lives
(define window (new frame%
                   [label "Enter new sample"]))
(define frame (new panel%
                   [parent window]))

; gets a list of the names of tabs from a tab list
(define get-tab-names
  (lambda (tab-list)
    (cond
      [(null? tab-list) '()]
      [else (cons (caar tab-list) (get-tab-names (cdr tab-list)))])))

; the tabs with the different input areas
(define input-tabs
  (new tab-panel%
       [choices (get-tab-names cur-tab-list)]
       [parent frame]
       [callback (lambda (tab-panel event) (tab-switch tab-panel))]))




; gets a list of fields, and constructs the text fields based on that
(define draw-fields
  (lambda (field-list panel)
    (cond
      [(null? field-list) '()]
      [else (cons (new text-field%
                       [parent panel]
                       [label (cadr (car field-list))])
                  (draw-fields (cdr field-list) panel))])))

; constructs a new panel for each tab, draws the fields, returns a list of panels
(define draw-tabs
  (lambda (tab-list id)
    (cond
      [(null? tab-list)]
      [else (begin
              (hash-set! cur-tab-fields id 
                         (draw-fields (cdr (car tab-list))
                                      input-tabs))
              (hash-set! cur-tab-values id '())
              ;(new horizontal-panel% [parent input-tabs]))) 
              (draw-tabs (cdr tab-list) (+ id 1)))])))
;the function for switching between tabs
(define tab-switch
  (lambda (tab-panel)
    ;(send tab-panel change-children
    ;(lambda (children)
    ;(get-current-children (send tab-panel get-selection) (draw-tabs cur-tab-list))))))
    (show-tab (send tab-panel get-selection) tab-pannels)));))

; a hashset containing the values for any tab field
(define cur-tab-values (make-hash))
; a hashset containing the field objects for any tab
(define cur-tab-fields (make-hash))

; saves the state of a current tab
(define save-tab
  (lambda (tab-id)
    (letrec ([tab-fields (hash-ref cur-tab-fields tab-id)]
             [f (lambda (field-list)
                  (cond
                    [(null? field-list) '()]
                    [else (cons (send (car field-list) get-value) 
                                (f (cdr field-list)))]))])
      (hash-set! cur-tab-values tab-id (f tab-fields)))))
;the tab pannels
(define tab-pannels (draw-tabs cur-tab-list 0))
; the id of the current-tab
(define cur-tab 0)
; gets a list of fields of a tab
(define get-fields
  (lambda (tab-id tab-list)
    (cond
      [(= 0 tab-id) (cdar tab-list)]
      [else (get-fields (- tab-id 1) (cdr tab-list))])))
; draws the fields and adds the value to the hash-map
(define set-and-draw-fields
  (lambda (id panel)
    (begin
      (hash-set! cur-tab-fields id (draw-fields (get-fields id cur-tab-list) panel))
      (hash-ref cur-tab-fields id))))
; loads the values in the fields of the current tab
(define load-tab
  (lambda (tab-id)
    (letrec ([tab-fields (set-and-draw-fields tab-id input-tabs)]
             [field-values (hash-ref cur-tab-values tab-id)]
             [f (lambda (field-list value-list)
                  (cond
                    [(null? field-list)]
                    [else (if (not (null? value-list))
                              (begin
                                (send (car field-list) set-value (car value-list))
                                (f (cdr field-list) (cdr value-list)))
                              (f (cdr field-list) '()))]))])
      (f tab-fields field-values))))
; deletes a tabs children
(define clear-panel
  (lambda ()
      (map (lambda (x) (send input-tabs delete-child x)) (send input-tabs get-children))))
; shows the right tab
(define show-tab
  (lambda (tab-id tab-pannel-list)
    (begin
      (save-tab cur-tab)
      (clear-panel)
      (load-tab tab-id)
      (set! cur-tab tab-id))))
;      (cond
;        [(null? tab-pannel-list)]
;        [(eq? -1 tab-id) (send (car tab-pannel-list) show #f)]
;        [(eq? 0 tab-id ) (begin
;                           (send (car tab-pannel-list) show #t)
;                           (when (not (null? tab-pannel-list))
;                             (show-tab -1 (cdr tab-pannel-list))))]
;        [else (begin
;                (send (car tab-pannel-list) show #f)
;                (show-tab (- tab-id 1) (cdr tab-pannel-list)))]))))




;the function for getting a list of current children of a tab pane
(define get-current-children
  (lambda (tab-id tab-pannel-list)
    (cond
      [(eq? 0 tab-id) (begin(list (car tab-pannel-list)))]
      [else (get-current-children (- tab-id 1) (cdr tab-pannel-list))])))
(define reset
  (begin
    (clear-panel)
    (load-tab 0)))
(send window show #t)
; clears the screen
