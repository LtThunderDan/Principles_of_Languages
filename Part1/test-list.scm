(load "list.scm")

; list=?: predicate list list -> boolean
; (list=? eq? l1 l2) returns if both lists are equal
; considering eq? as the equality predicate on elements

(define (list=? =? l1 l2)
  (if (null? l1)
      (null? l2)
      (if (null? l2)
          #f
          (and (=? (car l1) (car l2))
               (list=? =? (cdr l1) (cdr l2))))))

; number-list=?: number-list -> boolean
; (number-list=? l1 l2) tests is two list of
; numbers are equal

(define number-list=?
  (lambda (l1 l2)
    (list=? = l1 l2)))

; from-to: integer integer -> list
; (from-to n1 n2) returns the list of
; consecutive integers from n1 to n2
; included

(define from-to
  (lambda (n1 n2)
    (letrec ((aux (lambda (n l)
                    (if (> n1 n)
                        l
                        (aux (- n 1) (cons n l))))))
      (aux n2 empty))))

; print-errors: exn -> void
; (print-errors exn) print a message
; corresponding to the exception exn 

(define print-errors
  (lambda (exn)
    (begin
      (display "FAILED (")
      (display (exn-message exn))
      (display ")"))))

; header: string symbol any boolean -> void
; (header name test-expr result mode)
; prints the header of test messages
; - name is the title of the test
; - test-expr is the quoted expression to test
; - result is the expected result
; - mode is the mode in which to print the result

(define (header name test-expr result err-mode)
  (begin
    (display "== Test ")(display name)(display "  ==")
    (newline)
    (display "Tested expression: ")(write test-expr)
    (newline)
    (display "Expected result:   ")
    ((if err-mode display write)result)
    (newline)
    (display "Test: ")
    ))

; test-error: string symbol -> void
; (test-error name test-expr)
; evaluates the quoted expression test-expr
; that is supposed to fail

(define (test-error name test-expr)
  (begin
    (header name test-expr "ERROR" #t)
    (with-handlers ((exn:fail? (lambda (exn) (display "PASSED"))))
      (begin (let ((test-result (eval test-expr)))
               (begin
                 (display "FAILED (result is ")
                 (display test-result)
                 (display ")")))))
    (newline)(newline))
  )


; test: string symbol any predicate -> void
; (test-error name test-expr result pred?)
; evaluates the quoted expression test-expr
; that is supposed evaluate to result.
; The values are compared using the predicate pred?

(define (test name test-expr result equiv?)
  (begin
    (header name test-expr result #f)
    (with-handlers ((exn:fail? print-errors))
      (let ((test-result (eval test-expr)))
        (if (equiv? test-result result)
            (begin (display "PASSED") #t)
            (begin (display "FAILED (unexpected result: ")
                   (display test-result)
                   (display ")")
                   #f))))
      (newline)(newline))
  )

;; --------- The tests -------------

(define test1 '(list-map (lambda (x) (* 2 x)) (from-to 1 4)))
(define result1 (list 2 4 6 8))

(define test2 '(list-map string->number (list "42" "0" "12")))
(define result2 (list 42 0 12))

(define test3 '(list-zip (list 1 2 3) (list #t #f #t)))
(define result3 (list '(1 . #t) '(2 . #f) '(3 . #t)))

(define test4 '(list-zip (list 1 2 3) (list #t #f)))

(define test5 '(list-foldl + 0 (list 1 2 3 4)))
(define result5 10)

(define test6 '(list-foldl string-append "" (list "1" "+" "2" "=" "3")))
(define result6 "1+2=3")

(define test7 '(list-foldl - 0 (list 1 2)))
(define result7 -3)

(define test8 '(list-max (list 1 42 2 0 -42 -1 pi)))
(define result8 42)

(define test9 '(list-max (list 0+1i -1 0)))
(define test10 '(list-max (list)))

(define test11 '(list-min (list 1 42 2 0 -42 -1 pi)))
(define result11 -42)

(define test12 '(list-min (list)))

(test "list-rep 1" '(list-rep 5 0) (list 0 0 0 0 0) number-list=?)

(test "list-rep 2" '(list-rep 2 #\f) (list #\f #\f) (lambda (l1 l2) (list=? char=? l1 l2)))

(test "list-map 1" test1 result1 number-list=?)

(test "list-map 2" test2 result2 number-list=?)

(define (test3=? l1 l2)
  (list=? (lambda (p1 p2) (and (= (car p1)(car p2))(boolean=? (cdr p1) (cdr p2))))
          l1 l2))

(test "list-zip 1" test3 result3 test3=?)

(test-error "list-zip 2" test4)

(test "list-foldl 1" test5 result5 =)

(test "list-foldl 2" test6 result6 string=?)

(test "list-foldl 3" test7 result7 =)

(test "list-max 1" test8 result8 =)

(test-error "list-max 2" test9)

(test-error "list-max 3" test10)

(test "list-min 1" test11 result11 =)

(test-error "list-min 2" test12)

(test "sum 1" '(sum (list 1 2 3)) 6 =)
(test "sum 2" '(sum (list)) 0 =)
(test "sum 3" '(sum (list pi 0+1i)) (+ pi 0+1i) =)

(test "list-mem? 1" '(list-mem? = 42 (list 0 1 42 12 5)) #t boolean=?)
(test "list-mem? 2" '(list-mem? = 41 (list 0 1 42 12 5)) #f boolean=?)
(test "list-mem? 3" '(list-mem? string=? "FortyTwo" (list "Zero" "FortyTwo" "Two")) #t boolean=?)

(test "all-diff? 1" '(all-diff? = (list 1 2 3 4)) #t boolean=?)
(test "all-diff? 2" '(all-diff? string=? (list "One" "Zero" "One")) #f boolean=?)
