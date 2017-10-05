(load "list.scm")
(load "list-compl.scm")

; point?: any -> boolean
; tests is the input is a point, i.e.
; a list of real numbers
(define point?
  (lambda (v)
    (or (null? v)
        (and (pair? v)
             (real? (car v))
             (point? (cdr v))))))

; bfn->pfn: procedure -> procedure
; The input of this procedure should
; be a procedure f: a b -> c for some
; types a, b and c
; The output is a procedure that
; computes the same thing than the
; initial procedure f but this
; new procedure takes only one
; argument that is a pair.
; The first component of the pair
; should have type a, the second
; one type b.
(define bfn->pfn
  (lambda (f)
    (lambda (p) (f (car p) (cdr p)))))

; p-pointwise: procedure point point -> point
; (p-pointwise f v1 v2) first combines
; the two points (represented by lists
; of numbers of the same length) into a list of
; pairs and then apply pointwisely the procedure
; f taking two numbers and returning one number
; to the each of the pairs.
(define p-pointwise
  (lambda (f v1 v2)
    (list-map (bfn->pfn f) (list-zip v1 v2))))

; p-mul: point point -> point
; Pointwise multiplication of points
(define (p-mul v1 v2) (p-pointwise * v1 v2))

; p-dot: point point -> number
; (p-dot v1 v2) computes the
; dot product of two points v1 and v2
(define (p-dot v1 v2) (sum (p-mul v1 v2)))

; p-sub: point point -> point
; Pointwise substraction of points
(define (p-sub v1 v2) (p-pointwise - v1 v2))

; p-add: point point -> point
; Pointwise addition of points
(define (p-add v1 v2) (p-pointwise + v1 v2))

; p-sqr: point -> real
; Dot product of a point with itself
(define p-sqr (lambda (v) (p-dot v v)))

; p-dist: point point -> number
; (p-dist v1 v2) computes the euclidian
; distance between points v1 and v2
(define p-dist
  (lambda (v1 v2) (sqrt(p-sqr(p-sub v1 v2)))))

; point=?: point point -> boolean
; Equivalence predicate for points
(define point=?
  (lambda (p1 p2)
    (and (length p1) (length p2)
         (list-forall (lambda (n) (= n 0))
                      (p-sub p1 p2)))))

; points?: any -> boolean
; tests if the input is a list of points
; of the same dimension
(define points?
  (lambda (l)
    (if (null? l)
        #t
        (and (list-forall point? l)
             (let ((len (length(car l))))
               (list-forall (lambda (p)(= len(length p))) l))))))

; generate: positive-integer positive-integer positive-integer -> points
; (generate dim np bound) generates a list of nb points where each point
; has dimention dim, and coordinates are between 0 and bound.
(define (generate dimension num-points bound)
  (list-map
   (lambda (dum) (list-map (lambda (dum)(random bound))(from-to 1 dimension)))
   (from-to 1 num-points)))
