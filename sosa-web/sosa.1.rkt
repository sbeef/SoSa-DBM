#lang web-server/insta
;#lang racket

(define fields '((name "name") (cdate "collection date") (rdate "run date") (mass "sample weight")))
(define sample '((name "CH-001") (cdate "06-15-2013") (rdate "") (mass "80g")))

(define (start request)
  (response/xexpr
   `(html
     (head (title "SoSa DB"))
     (body (h1 "Soil Sample Database")
           ,(ngen-form fields "search" "query" "post")))))

(define (ngen-form field-list name action method)
  (append (list 'form) `((name ,name) (action ,action) (method ,method))
         (gen-inputs field-list)))

(define (gen-form field-list name action method)
  (append (list 'form)
         (list (list
                 (list 'name name)
                 (list 'action action)
                 (list 'method method)))
                (gen-inputs field-list)))
  
  (define (gen-inputs field-list)
    (apply append (map gen-input field-list)))
  
  (define (gen-input field)
    (list (cadr field) 
          (list 'input 
                (list (list 'type "text") 
                      (list 'name (symbol->string (car field)))))))