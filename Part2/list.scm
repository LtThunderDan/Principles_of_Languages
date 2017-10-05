(define list-rep
  (lambda (n x)
    (if (<= n 0)
        empty
        (cons x (list-rep (- n 1) x)))))

(define list-map
  (lambda (f l)
    (if (null? l)
        null
        (cons (f (first l))(list-map f (rest l))))))

(define list-zip
  (lambda (l1 l2)
    (cond
      ( (and (empty? l1)(empty? l2)) empty)
      ( (and (not(null? l1)) (not(null? l2)))
        (cons (cons (first l1)(first l2)) (list-zip (rest l1)(rest l2))))
      (else (error 'list-zip "not same length")))))

(define list-foldl
  (lambda (op e l)
    (if (null? l)
        e
        (list-foldl op (op e (first l)) (rest l)))))

(define list-min
  (lambda (l)
    (list-foldl min (first l) (rest l))))

(define list-max
  (lambda (l)
    (list-foldl max (first l)(rest l))))

(define (sum l) (list-foldl + 0 l))

(define list-index
  (lambda (n l)
    (cond
      ( (empty? l) (error 'list-index "not-found"))
      ( (= n (first l)) 0)
      ( else (+ 1 (list-index n (rest l)))))))

(define list-mem?
  (lambda (eq? e l)
    (if (empty? l)
        #f
        (or (eq? e (first l))
            (list-mem? eq? e (rest l))))))

(define all-diff?
  (lambda (eq? l)
    (if (empty? l)
        #t
        (and (not (list-mem? eq? (first l) (rest l)))
             (all-diff? eq? (rest l))))))
