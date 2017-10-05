(load "point.scm")

; positive-integer?: any -> boolean
; Tests if the argument is a positive integer
(define positive-integer?
  (lambda (n)
    (and (integer? n)
         (<= 0 n))))

; cpoint? : any -> boolean
; Tests if the argument is a cpoint i.e.
; a pair consisting of a point and a positive
; integer
(define cpoint?
  (lambda (p)
    (and (pair? p)
         (positive-integer? (cdr p))
         (point? (car p)))))

; cpoints?: any -> boolean
; tests if the input is a list of cpoints
; of the same dimension
(define cpoints?
  (lambda (l)
    (and (list? l)
         (or (null? l)
             (and (list-forall cpoint? l)
                  (let ((len (length(car(car l)))))
                    (list-forall (lambda (p)(= len(length(car p)))) l)))))))

; cpoint-class?: positive-integer cpoint -> boolean
; tests if a given cpoint belongs to a given class.
(define cpoint-class?
  (lambda (class cpoint)
    (= (cdr cpoint) class)))

; point->cpoint: point -> cpoint
; builds a cpoint with initial class 0
(define (point->cpoint point) (cons point 0))

