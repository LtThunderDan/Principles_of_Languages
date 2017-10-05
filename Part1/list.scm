;list-rep=?: integer integer -> list
(define list-rep
  (lambda (n v)
    (if (= n 0)
        empty
        (append(list v)(list-rep(- n 1) v)))))

;list-map: integer list -> list
(define list-map
  (lambda (f l)
    (if (empty? l)
        empty
        (append (list (f (first l))) (list-map f (rest l))))))


;list-zip: list list -> list
(define list-zip
  (lambda (i j)
    (if (= (length i) (length j))
        (cond
        ((empty? i) empty)
    (else (append (list (cons (first i)(first j))) (list-zip (rest i) (rest j)))))
        ERROR)))

;list-foldl: operation integer list -> list
;used to reverse the list to pass tests.
;algorithm provided by James Todd
(define list-foldl
  (lambda (op e l)
    (helperlist-foldl op e (reverse l))))

;helperlist-foldl: operation integer list -> integer
(define helperlist-foldl
  (lambda (op e l)
    (if (empty? l)
        e
        (op (helperlist-foldl op e (rest l)) (first l)))))

;greater: int int -> int
;used to compare values and find which is greater.
(define greater
  (lambda (i j)
    (if (> i j)
        i
        j)))

;list-max: list -> int
(define list-max
  (lambda (l)
    (if (empty? l)
        ERROR
        (list-foldl greater (first l) l))))

;lesser: int int -> int
;used to compare values and find which is lesser.
(define lesser
  (lambda (i j)
    (if (< i j)
        i
        j)))

;list-min: list -> int
(define list-min
  (lambda (l)
    (if (empty? l)
        ERROR
        (list-foldl lesser (first l) l))))

;sum: list -> int
(define sum
  (lambda (l)
    (if (empty? l)
        0
        (list-foldl + 0 l))))

;list-mem?: operation value list -> boolean
(define list-mem?
  (lambda (op v l)
    (if (empty? l)
        #f
        (cond
          ((op v (first l)) #t)
          (else
           (list-mem? op v (rest l)))))))

;all-diff?: operation list -> boolean
(define all-diff?
  (lambda (op l)
    (if (empty? l)
        #t
        (cond
          ((list-mem? op (first l) (rest l)) #f)
        (else
          (all-diff? op (rest l)))))))

;list-index integer list -> int
(define list-index
  (lambda (i l)
    (if (not (list-mem? = i l))
        #f
        (if (= i (first l))
            0
            (+ 1 (list-index i (rest l)))))))

(list-index 6 (list 4 5 3 8)) ;the integer is NOT in the list, should return #f.
(list-index 6 (list 4 5 3 6)) ;the integer is in the 3rd indicie, should return 3.
(list-index 6 (list 4 6 3 8)) ;the integer is in the 1st indicie, should return 1.

