; list-update: list positive-integer any -> list
; (list-update l n v) returns l but where element
; at index n is now v.
; The call returns and error "invalid index" if
; n is not a valid index of list l.
(define list-update
  (lambda (l index value)
    (if (null? l)
        (error 'list-update "invalid index")
        (if (= index 0)
            (cons value (cdr l))
            (cons (car l)
                  (list-update (cdr l) (- index 1) value))))))

; list-forall: predicate list -> boolean
; (list-forall p l) returns true is all the
; elements of the list satisfy predicate p,
; false otherwise
(define list-forall
  (lambda (p l)
    (list-foldl
     (lambda (res e) (and (p e) res))
     #t
     l)))

; from-to: integer integer -> list
; (from-to n1 n2) create a list of
; integers (n1 ... n2) 
(define (from-to n1 n2)
  (letrec ((aux
            (lambda (l n2)
              (if (> n1 n2)
                  l
                  (aux (cons n2 l) (- n2 1))))))
    (aux null n2)))
