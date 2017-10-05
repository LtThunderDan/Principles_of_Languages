(load "centroids.scm")
(load "cpoint.scm")
(load "list.scm")
(load "point.scm")

;(define cpoints '( ((0 0) . 0) ((1 0) . 0) ((0 1) . 0) ((0 2) . 1) ((2 2) . 1) ((42 2) . 2)))
;================================================================================

;class-size:  int list -> int

;(class-size 0 cpoints) ;should return: 3
;(class-size 1 cpoints) ;should return: 2
        
(define (class-size int list)
    (if (empty? list)
        0
        (if (= (cdr(car list)) int)
            (+ 1 (class-size int (cdr list)))
            (+ 0 (class-size int (cdr list))))))

;=================================================================================

;class-sum: int list -> list

;(class-sum 0 cpoints) ;should return: (1 1)
;(class-sum 2 cpoints) ;should return: (42 2)

(define (class-sum int list)
    (if (empty? list)
        '(0 0)
        (append (list(getX int list)) (list(getY int list)))))

;=================================================================================

;getX: int list -> int
;gets X value to append to get-centroid

(define (getX int list)
    (if ( empty? list)
        0
        (if (= (cdr(car list)) int)
            (+ (car(car(car list))) (getX int (cdr list)))
            (+ 0 (getX int (cdr list))))))

;getY: int list -> int
;gets Y value to append to get-centroid

(define (getY int list)
    (if ( empty? list)
        0
        (if (= (cdr(car list)) int)
            (+ (second(car(car list))) (getY int (cdr list)))
            (+ 0 (getY int (cdr list))))))

;get-centroid int list -> list
;(get-centroid 0 cpoints) ;should return (1/3 1/3)

(define (get-centroid int L1)
    (append (list(/ (getX int L1) (class-size int L1))) (list(/ (getY int L1) (class-size int L1)))))

;=================================================================================

;helpCentroids int list -> list
;helper function does get-centroids, but returned it to me in a reversed order.

(define (helpCentroids int L1)
    (if (= 0 int)
        empty
        (append (list(get-centroid (- int 1) L1)) (helpCentroids (- int 1) L1))))

;get-centroids int list -> list
;reverses the order of the helper function.

;(get-centroids 3 cpoints) ;should return ((1/3 1/3) (1 2) (42 2))

(define (get-centroids int list)
    (reverse(helpCentroids int list)))



;=================================================================================

;getDistance int list -> list
;helper function that finds the distance.
(define (getDistance cp centroids)
    (if (empty? centroids)
        empty
        (append (list(p-dist (car cp) (car centroids))) (getDistance cp (cdr centroids)))))

;getMin list -> list
;helper function that finds the smallest value.
(define (getMin list)
    (list-min list))

;get-class int list -> int
(define (get-class cp centroids)
    (if (empty? centroids)
        0
        (list-index (getMin (getDistance cp centroids)) (list-map exact->inexact(getDistance cp centroids)))))

;================================================================================

;update int int -> pair
;helper function
(define ((updateHelper centroids) cpoint)
      (cons (car cpoint) (get-class cpoint centroids)))

;class-update list list -> list
(define (class-update cpoint centroids)
    (list-map (updateHelper centroids) cpoint))

;================================================================================

;repeat int function int -> int
;(repeat 10 (lambda (x) (+ 4 x)) 2) ;should return 42

(define (repeat count f initial)
    (if (= count 0)
        initial
        (repeat (- count 1) f (f initial))))

;================================================================================

;kmeansHelper int int -> pair
(define ((kmeansHelper k) p)
       (let ((i (class-update (car p) (cdr p))))
        (cons i (get-centroids k i))))

;initialHelper int -> pair
(define (initialHelper points)
    (if (empty? points)
        empty
        (cons (point->cpoint (car points)) (initialHelper (cdr points)))))

;k-means int list int -> pair
;returns a pair consisting of the updated cpoints and k centroids after count iterations of the algorithm
(define (k-means k points count)
    (let ((centriod (initial-centroids k points)) (cpoint (initialHelper points)))
      (let ((i (class-update cpoint centriod)))
       (repeat count (kmeansHelper k) (cons i (get-centroids k i))))))

;================================================================================

