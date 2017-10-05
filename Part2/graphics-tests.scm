(load "list.scm")
(load "point.scm")
(load "k-means.scm")


(define points1 (generate 2 50 25))
(define points2 (generate 2 50 30))
(define points3 (generate 2 125 50))
(define points4 (generate 2 75 40))

(define points
  (append (map (lambda (p) (p-add '(-25 -25) p)) points1)
          (map (lambda (p) (p-add '(25 -25) p)) points2)
          (map (lambda (p) (p-add '(-25 25) p)) points3)
          (map (lambda (p) (p-add '(25 25) p)) points4)))

(require graphics/graphics)

(open-graphics)

(define (borders points)
  (let
      ( (xs (list-map car points))
        (ys (list-map cadr points)) )
  (list
   (list-min xs)
   (list-max xs)
   (list-min ys)
   (list-max ys))))

(define (size points)
  (let*
      ((borders (borders points))
       (x-min (car borders))
       (x-max (cadr borders))
       (y-min (caddr borders))
       (y-max (cadddr borders)))
    (cons
     (+ 1 (abs (inexact->exact (- x-max x-min))))
     (+ 1 (abs (inexact->exact (- y-max y-min)))))))

(define (make-view points)
  (let ((s (size points)))
    (open-viewport "Points" (* 4 (car s)) (* 4 (cdr s)))))

(define (color n)
  (cond ((= 0 n) (make-rgb 0 0 0))
        ((= 1 n) (make-rgb 0.75 0 0))
        ((= 2 n) (make-rgb 0 0.75 0))
        ((= 3 n) (make-rgb 0 0 0.75))
        ((= 4 n) (make-rgb 0.75 0.75 0))
        ((= 5 n) (make-rgb 0.75 0 0.75))
        ((= 6 n) (make-rgb 0 0.75 0.75))
        ((= 7 n) (make-rgb 1 0 0))
        ((= 8 n) (make-rgb 0 1 0))
        ((= 9 n) (make-rgb 0 0 1))
        (else (make-rgb 0.5 0.5 0.5))))
        
(define (draw-cpoints view cpoints style)
  (let*
      ((borders (borders points))
       (x-min (car borders))
       (y-min (caddr borders)))
    (map (lambda (cp)
           ((if style (draw-solid-rectangle view) (draw-solid-ellipse view))
            (apply make-posn (p-mul (list 4 4) (p-sub (car cp) (list x-min y-min))))
            (if style 4 8) (if style 4 8) (color (cdr cp))))
         cpoints)))

(define (draw-solution view solution)
  (begin
    (draw-cpoints view (car solution) #t)
    (draw-cpoints view (list-zip (cdr solution)
                                 (from-to 0 (- (length (cdr solution)) 1))) #f)))

(define (make-view points)
  (let ((s (size points)))
    (open-viewport "Points" (* 4 (car s)) (* 4 (cdr s)))))

(map (lambda (k)
       (draw-solution (make-view points) (k-means k points 100)))
     (from-to 2 5))