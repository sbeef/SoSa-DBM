#lang racket
(require racket/gui)
(define vp-width 50)
(define vp-height 50)
(require "tab-panel2.rkt")
; the hash-table of tab lists (eventually will be auto-built from a config file
(define cur-tab-list '(("general" ('name "name") ('c-date "date collected"))
                       ("harbin" ('r-date "run date") ('r-time "run time"))
                       ("angle" ('cs "Cs137") ('pb "Pb210") ('be "Be7"))))
(define frame (new frame% [label "SoSa DB"]))
(define main (new horizontal-panel% [parent frame]))
(define sidebar (new vertical-panel% [parent main]))
(define viewport (new vertical-panel% 
                      [parent main]
                      [min-width vp-width]
                      [min-height vp-height]))
(new button%
     [parent sidebar]
     [label "new sample"])
(new button%
     [parent sidebar]
     [label "find sample"])
(new button%
     [parent sidebar]
     [label "quit"])
(define input-form
  (let ([tab-obj (make-tab-panel-obj viewport cur-tab-list)])
    (new button%
         [parent viewport]
         [label "submit"]
         [callback (lambda (t e)
                     (let ([values (hash-values (caddr tab-obj))]
                           [fields (hash-values (cadddr tab-obj))])
                       ;(display values fields)))])))
                       (display (pair-f-v fields values))))])))
(define atom? (lambda (e) (not (or (null? e) (pair? e)))))
(define pair-f-v
  (lambda (fields values)
    (letrec ([f (lambda (l1 l2)
                  (cond
                    [(null? l1) '()]
                    [(null? l2) '()]
                    [else (cons (cons (caar l1) (car l2)) (f (cdr l1) (cdr l2)))]))])
      (map f fields values))))
(send frame show #t)