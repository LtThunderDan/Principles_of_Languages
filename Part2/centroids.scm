(load "list.scm")
(load "list-compl.scm")
(load "point.scm")

(define choose-indices
  (lambda (num-points initial-list)
      (list-map (lambda (x) (random num-points))
           initial-list)))

(define repeat-until
  (lambda (f test initial)
    (let ( (result (f initial)) )
      (if (test result)
          result
          (repeat-until f test result)))))

(define choose-distinct
  (lambda (k points)
    (let ((num-points (length points)))
      (repeat-until
       (lambda (l) (choose-indices num-points l))
       (lambda (l)
         (all-diff?
          point=?
          (list-map (lambda (i) (list-ref points i)) l)))
       (list-rep k 0)))))

; initial-centroids: positive-integer points -> points
; (initial-centroids k points) randomly selects k distinct
; points in the list of points 
(define initial-centroids
  (lambda (k points)
    (list-map (lambda(i)(list-ref points i))
              (choose-distinct k points))))
