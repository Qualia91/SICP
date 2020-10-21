#lang sicp
; Ex 1.2 Translate into prefix form. Should equal -37/150
(/
 (+ 5 4
    (- 2
       (- 3
          (+ 6
             (/ 4 5)))))
 (* 3 (- 6 2) (- 2 7)))

; Ex 1.3 define procedure which takes 3 numbers and returns sum of squares of 2 largest numbers
;; first square
(define (sqr-num x) (* x x))
(sqr-num 5)

;; then sum of square
(define (sum-sqr x y) (+ (sqr-num x) (sqr-num y)))
(sum-sqr 5 6)

;; finally cond to find largets 2 numbers and use function on them
(define (max-two-sum-sqr x y z func)
  (cond ((and (>= x y) (>= y z)) (func x y))
        ((and (>= x z) (>= z y)) (func x z))
        (else (func z y))))

;; 13
(max-two-sum-sqr 1 2 3 sum-sqr)
;; 2
(max-two-sum-sqr 1 1 1 sum-sqr)
;; 8
(max-two-sum-sqr 1 2 2 sum-sqr)
;; 5
(max-two-sum-sqr 1 1 2 sum-sqr)

; Exercise 1.4
;; If b is bigger than 0, return plus functions, then add a and b
;; else return minus function then minus a and b
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; Exercise 1.5
;; if applicative order evaluation, it will try to evaluate leftymost, innermost first
;; meaning it will try to find p and get stuck in a loop
;; in normal order evaluation, it will evaluate outermost first,
;; meaning it will return 0
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

; Example 1.7 - Newtons method
;; Square roots by newtons method
(define (quotient target current) (/ target current))
;; 1.3 rec
(quotient 2 1.5)

(define (average x y) (/ (+ x y) 2))

(define (improve-guess guess x) (average guess (/ x guess)))

;; basic good-enough?
(define (good-enough? guess x) (< (abs (- (sqr-num guess) x)) 0.001))
;; #t
(good-enough? 2 4)
;; #f
(good-enough? 2 5)

;; actual newtons method with function for approximating the nth root dependant on function
(define (newtons-method-iter x guess improve-guess-function good-enough-func?)
  (if (good-enough-func? guess x)
      guess
      (newtons-method-iter x (improve-guess-function guess x) improve-guess-function good-enough-func?)))

(newtons-method-iter 25.0 1.0 improve-guess good-enough?)
      
; Ex 1.8
;; Newtons cube root method
;; New improve function for cube root
(define (improve-guess-cube y x)
  (/ (+ (/ x (* y y)) (* 2 y )) 3))

(define (good-enough-cube? guess x) (< (abs (- (* guess guess guess) x)) 0.000001))

(newtons-method-iter 5.0 1.0 improve-guess-cube good-enough-cube?)

; Ex 1.9
;; Recursive
(define (recursive-f n)
  (if (< n 3)
      n
      (+ (recursive-f (- n 1)) (* 2 (recursive-f (- n 2))) (* 3 (recursive-f (- n 3))))))

;; -1
(recursive-f -1)
;; 0
(recursive-f 0)
;; 25
(recursive-f 5)
;; 18.700000000000003
(recursive-f 4.7)

;; Iterative
;; like finoaccia numbers example, start with 3 ints a, b and c initialised at 2, 1 and 0
;; and notice the transform netween them are:
;; a <- a + 2b + 3c
;; b <- a
;; c <- b
(define (iterative-f n)
  (define (iter-f a b c counter)
    (cond
      ((< n 3) n)
      ((<= counter 0) a)
      (else (iter-f (+ a (* 2 b) (* 3 c)) a b (- counter 1)))))
  (iter-f 2 1 0 (- n 2)))

;; -1
(iterative-f -1)
;; 0
(iterative-f 0)
;; 25
(iterative-f 5)
;; 25 (does not cope with decimals due to 
(iterative-f 4.7)

; Ex 1.12
;; Pascals triangle recursive procedure
(define (pascal-rec row col)
  (cond
    ((> col row) 0)
    ((< col 0) 0)
    ((< row 2) 1)
    (else (+ (pascal-rec (- row 1) col) (pascal-rec (- row 1) (- col 1))))))

(pascal-rec 0 0)
(pascal-rec 1 0)
(pascal-rec 1 1)
(pascal-rec 2 0)
(pascal-rec 2 1)
(pascal-rec 2 2)
(pascal-rec 3 0)
(pascal-rec 3 1)
(pascal-rec 3 2)
(pascal-rec 4 0)
(pascal-rec 4 1)
(pascal-rec 4 2)
(pascal-rec 4 3)

(pascal-rec 3 3)
(pascal-rec -1 0)
(pascal-rec -1 -1)

; Ex 1.16
;; Successive squaring procedure (recursive)
(define (succ-sqr x n)
  (cond
    ((= n 0) 0)
    ((= n 1) x)
    ((= (modulo x 2) 0) (sqr-num (succ-sqr x (/ n 2))))
    (else (* x (succ-sqr x (- n 1))))))

(succ-sqr 1 0)
(succ-sqr 5 1)
(succ-sqr 5 2)
(succ-sqr 5 3)
(succ-sqr 5 4)
(succ-sqr 5 5)

;; Successive squaring procedure (iterative)
(define (suc-sqr-iter x n)
  (define (iter-inner X N a)
    (cond
      ((= N 0) a)
      ((= (modulo N 2) 0) (iter-inner (* X X) (/ N 2) a))
      (else (iter-inner X (- N 1) (* X a)))))
  (iter-inner x n 1)
  )

(suc-sqr-iter 1 0)
(suc-sqr-iter 5 1)
(suc-sqr-iter 5 2)
(suc-sqr-iter 5 3)
(suc-sqr-iter 5 4)
(suc-sqr-iter 5 5)

; Ex 1.7
;; Multiplication via double and half procedures
(define (double a) (* a 2))
(define (half a) (/ a 2))

;; recursive
(define (fast-* a b)
  (cond
    ((= b 0) 0)
    ((= (modulo b 2) 0) (fast-* (double a) (half b)))
    (else (+ a (fast-* a (- b 1))))))

(fast-* 0 0)
(fast-* 1 0)
(fast-* 1 1)
(fast-* 1 2)
(fast-* 2 2)
(fast-* 5 6)
(fast-* 7 8)
(fast-* 9 9)

;; iterative
(define (fast-iter-* a b)
  (define (iter A B C)
  (cond
    ((= B 0) C)
    ((= (modulo B 2) 0) (iter (double A) (half B) C))
    (else (iter A (- B 1) (+ C A)))))
  (iter a b 0))

(fast-iter-* 0 0)
(fast-iter-* 1 0)
(fast-iter-* 1 1)
(fast-iter-* 1 2)
(fast-iter-* 2 2)
(fast-iter-* 5 6)
(fast-iter-* 7 8)
(fast-iter-* 9 9)

; Ex 1.19
;; complete the following
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (sqr-num p) (sqr-num q))
                   (+ (sqr-num q) (* 2 p q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(fib 0 )
(fib 1 )
(fib 2 )
(fib 3 )
(fib 4 )
(fib 5 )

; Ex 1.21:
;;Use smallest divisor procedure to find the smallest divisor
;;of the following numbers: 199, 1999, 19999
(define (remainder num div)
  (define (remainder-iter num div current-val)
    (cond
      ((= current-val num) 0)
      ((> current-val num) (- current-val num))
      ((and (< current-val num) (> (+ current-val div) num)) (- num current-val))
      (else (remainder-iter num div (+ current-val div)))))
          
  (remainder-iter num div div))

"remainder"
(remainder 0 0)
(remainder 2 1)
(remainder 3 2)
(remainder 54 55)
(remainder 6 3)
(remainder 12 5)



(define (gcd a b)
  (define (gcd-iter A B)
    (if (= B 0)
        A
        (gcd-iter B (remainder A B))))
  (gcd-iter a b))

"gcd"
(gcd 206 40)
(gcd 40 206)

(define (divides? a b) (= (remainder b a) 0))

(define (smallest-divisor n next-func)
  (define (find-divisor N current-div)
    (cond
      ((> (sqr-num current-div) n) n)
      ((divides? current-div n) current-div)
      (else (find-divisor N (next-func current-div)))))
  (find-divisor n 2))

(define (next-simple x) (+ x 1))

"smallest-divisor"
(smallest-divisor 206 next-simple)
(smallest-divisor 199 next-simple)
(smallest-divisor 1999 next-simple)
(smallest-divisor 19999 next-simple)


; Ex 1.23
;; Improvements to smallest-divisor
(define (next-odd x)
  (if (= x 2)
      3
      (+ x 2)))


(smallest-divisor 206 next-odd)
(smallest-divisor 199 next-odd)
(smallest-divisor 1999 next-odd)
(smallest-divisor 19999 next-odd)


; Ex 1.29
;; Simpsons rule
;; sum procedure
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc x) (+ x 1))

(define (simpsons f a b n)
  (define h (/ (- b a) n))
  (define (yk k) (f (+ a (* k h))))
  (define (multi-term k)
    (cond
      ((or (= k 0) (= k n)) 1)
      ((even? k) 2)
      (else 4)))
  (define (simp-term k) (* (multi-term k) (yk k)))
  (* (/ h 3) (sum simp-term a inc n))
)

(define (cube x) (* x x x))

"Simpsons"
(simpsons cube 0 1 100) 
(simpsons cube 0 1 1000) 

; Ex 1.30
;; iterative sum procedure
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

; Ex 1.31
;; product procedure
(define (prod term a next b)
  (if (> a b)
      1
      (* (term a)
         (prod term (next a) next b))))

;; iterative prod procedure
(define (prod-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

;; factorial
(define (fact-prod x)
  (define (fact-term x) x)
  (prod-iter fact-term 2 inc x))

"Factorial"
(fact-prod 0)
(fact-prod 1)
(fact-prod 2)
(fact-prod 3)
(fact-prod 4)
(fact-prod 5)

;; PI using above formula
(define (pi-term n)
  (if (even? n)
      (/ (+ n 2) (+ n 1))
      (/ (+ n 1) (+ n 2))))

(* (prod-iter pi-term 1 inc 6) 4.0)
(* (prod-iter pi-term 1 inc 100) 4.0) 


; Ex 1.32
;; accumulate procedure recursive
(define (accumulate combiner null-value term a next b)
    (if (> a b)
      null-value
      (combiner (term a)
         (accumulate combiner null-value term (next a) next b))))

;; sum using accumulate
(define (sum-term a) a)
(define (sum-combiner a b) (+ a b))
(accumulate sum-combiner 0 sum-term 0 inc 5)

;; product using accumulate
(define (prod-term a) a)
(define (prod-combiner a b) (* a b))
(accumulate prod-combiner 1 prod-term 1 inc 5)

;; accumulate procedure iterative
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

;; sum using accumulate
(accumulate-iter sum-combiner 0 sum-term 0 inc 5)

;; product using accumulate
(accumulate-iter prod-combiner 1 prod-term 1 inc 5)

; Ex 1.33









