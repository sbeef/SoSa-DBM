#lang web-server/insta
;#lang racket

(define t-fields '((name "name") (cdate "collection date") (rdate "run date") (mass "sample weight")))
(define t-sample '((name "CH-001") (cdate "06-15-2013") (rdate "") (mass "80g")))
  

(define (start request)
  (response/xexpr
   `(html
     (head (title "SoSa DB"))
     (body (h1 "Soil Sample Database")
           ,(gen-form t-fields "search" "query" "post")))))

(define (gen-form field-list name action method)
  `(form ((name ,name) (action ,action) (method ,method))
          ,@(gen-inputs field-list)))

(define (gen-inputs field-list)
  (apply append (map gen-input field-list)))

(define (gen-input field)
  `(,(cadr field) 
    (input 
     ((type "text") 
      (name ,(symbol->string (car field)))))))