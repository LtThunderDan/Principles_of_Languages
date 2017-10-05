;=======================================================================================
;The code you gave us:
(define (iflt e1 e2 then else)  (quote (if (< e1 e2) then else)))                
(define functions (vector '( + . 2) '( - . 2) '( * . 2 ) '( / . 2 ) '(iflt . 4)))
(define terminals (vector 'x 'y 1 5 10))

;A fake random generator
(define side #f)
(define last -1)
(define not_random
  (lambda args
    (if (null? args)
        (if side 0.0 (begin (set! side (not side)) 0.99))
        (let ((bound (car args)))
          (begin (set! last (modulo (+ 1 last) bound))
                 last)))))
;=======================================================================================
;func_set is a function set
;term_set is a terminal set
;max_d is the maximum allowed depth for expressions
;method is either full or grow

;this assignment was to randomly generate a tree with functions and terminals(variables/leafs) 

;gen_rnd_expr: int set set int method -> list

(define (gen_rnd_expr x func_set term_set max_d method)
    (if
     (or(= max_d 0)
        (and (symbol=? method 'grow) (< (x)(/ (vector-length term_set) (+ (vector-length term_set) (vector-length func_set))))))
        (vector-ref term_set (modulo(inexact->exact(round(*(random)10)))(vector-length terminals)))
        (let ((func (vector-ref func_set (modulo(inexact->exact(round(*(random)10)))(vector-length terminals)))))
          (let ((end (cdr func)) (root (car func)) (counter 1))
            (append (list root) (for_loop x func_set term_set max_d method end counter))))))


;helper function that does the for loop in steps 5-7

;for_loop: random set set int method int int -> list
(define (for_loop x func_set term_set max_d method end counter)
    (if
     (= counter end)
        (list(gen_rnd_expr x func_set term_set (- max_d 1) method))
        (append (list (gen_rnd_expr x func_set term_set (- max_d 1) method)) (for_loop x func_set term_set max_d method end (+ counter 1)))))


;=======================================================================================
;Tests:

(gen_rnd_expr not_random functions terminals 2 'grow)
(gen_rnd_expr not_random functions terminals 2 'grow)
(gen_rnd_expr not_random functions terminals 2 'full)
;Should generate results such as:
;(+ y 1)
;5
;(iflt (+ y 1) (/ 10 x) (- 1 5) (iflt x y 1 5))

(display "\n\n")

(gen_rnd_expr random functions terminals 2 'grow)
(gen_rnd_expr random functions terminals 3 'full)

;Should generate resuslts such as:
;(- 5 (* x 5))
;(- (- (iflt 5 1 x x) (- 5 1)) (- (- 5 y) (* 1 1)))
;=======================================================================================



