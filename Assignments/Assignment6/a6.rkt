#lang racket
(require C311/pmatch)
(require C311/trace)
;;(require "a6-student-tests.rkt")
;;(test-file #:file-name "a6.rkt")


;;--------------------------
;The given empty-k function 

(define empty-k
 (lambda ()
  (let ((once-only #f))
   (lambda (v)
    (if once-only 
     (error 'empty-k "You can only invoke the empty continuation once")
     (begin (set! once-only #t) v))))))


;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;Part I : call/cc -------------------
;1. last-non-zero, a function which takes a list of numbers and returns the last cdr whose car is 0.

(define last-non-zero
 (lambda (ls) 
  (call/cc
   (lambda (k)
    (letrec
     ((lnz
      (lambda (ls)
       (cond
        ((null? ls) '())
        ((zero? (car ls)) (k (lnz (cdr ls))))  ;;Try building a list and whenever a zero is occuring , try to break the loop and print the numbers after zero.
        (else (cons (car ls) (lnz (cdr ls)))))
	  )))
    (lnz ls))))))


;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;2. Direct vs. accumulator-passing vs. call/cc. 
;Consider the following definitions of mult and mult/acc, a function which takes a list of numbers and returns the product, respectively written in direct and accumulator-passing styles.
;mult/cc, which uses a system continuation k to return with 0 if the list contains a 0.

(define my-*
 (lambda (m n)
  (* m n)))

(define mult
 (lambda (n*)
  (letrec
   ((m
    (lambda (n*)
     (cond
      ((null? n*) 1)
      ((zero? (car n*)) 0)
      (else (my-* (car n*) (mult (cdr n*))))))))
    (m n*))))

(define mult/acc
 (lambda (n*)
  (letrec 
   ((m/acc
   (lambda (n* acc)
    (cond
     ((null? n*) acc)
     ((zero? (car n*)) 0)
     (else (m/acc (cdr n*) (my-* (car n*) acc)))))))
  (m/acc n* 1))))


(define mult/cc
 (lambda (n*)
  (call/cc
   (lambda (k)
    (letrec
     ((m/cc
     (lambda (n*)
      (cond 
       [(null? n*) 1]
       [(zero? (car n*)) (k 0)]
       [else (my-* (m/cc (cdr n*)) (car n*))])
     )))
    (m/cc n*))))))


;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;Part II : CPS ----------------
;3. Define and test a procedure times-cps that is a CPSed version of the following times procedure. 

(define times
 (lambda (ls)
  (cond
   [(null? ls) 1]
   [(zero? (car ls)) 0]
   [else (* (car ls) (times (cdr ls)))])))


(define times-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) (k 0)]
      [else (times-cps (cdr ls) (lambda (x) (k (* (car ls) x))))])))


;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;4. Define a modified version of your times-cps above, called times-cps-shortcut that doesn't apply k in the zero case. Instead, maintain the behavior of the zero? case in times - simply returning the 0 and not performing further computation. While this certainly violates the standard rules of CPSing the program, it provides an interesting look at optimizations CPSing allows us. 

(define times-cps-shortcut
 (lambda (ls k)
  (cond
   [(null? ls) (k 1)]
   [(zero? (car ls)) 0]
   [else (times-cps-shortcut (cdr ls) (lambda (x) (k (* (car ls) x))))])))



;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;5. Define and test a procedure plus-cps that is a CPSed version of the following plus procedure: 

(define plus
 (lambda (m)
  (lambda (n)
   (+ m n))))


(define plus-cps
 (lambda (m k)
  (k (lambda (n k)
   (k (+ m n))))))


;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;6. Define and test a procedure count-syms*-cps that is a CPSed version of the following count-syms* procedure: 

(define count-syms*
 (lambda (ls)
  (cond
   [(null? ls) 0]
   [(pair? (car ls)) (+ (count-syms* (car ls)) (count-syms* (cdr ls)))]
   [(symbol? (car ls)) (add1 (count-syms* (cdr ls)))]
   [else (count-syms* (cdr ls))])))


(define count-syms*-cps
 (lambda (ls k)
  (cond
   [(null? ls) (k 0)]
   [(pair? (car ls)) (count-syms*-cps (car ls) (lambda (x) (count-syms*-cps (cdr ls) (lambda (y) (k (+ x y))))))]
   [(symbol? (car ls)) (count-syms*-cps (cdr ls) (lambda (x) (k (add1 x))))]
   [else (count-syms*-cps (cdr ls) k)])))


;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;7. Define and test a procedure cons-cell-count-cps that is a CPSed version of the following cons-cell-count procedure

(define cons-cell-count
 (lambda (ls)
  (cond
   [(pair? ls) (add1 (+ (cons-cell-count (car ls)) (cons-cell-count (cdr ls))))]
   [else 0])))


(define cons-cell-count-cps
 (lambda (ls k)
  (cond
   [(pair? ls) (cons-cell-count-cps (car ls) (lambda (x) (cons-cell-count-cps (cdr ls) (lambda (y) (k (add1 (+ x y)))))))]
   [else (k 0)])))


;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;8. Define and test a procedure walk-cps that is a CPSed version of the following walk procedure: 


(define walk
 (lambda (v ls)
  (cond
   [(symbol? v) 
    (let ((p (assq v ls)))
    (cond
     [p (walk (cdr p) ls)]
     [else v]))]
   [else v])))


(define walk-cps
 (lambda (v ls k)
  (cond
   [(symbol? v) (let ((p (assq v ls)))
    (cond
     [p (walk-cps (cdr p) ls k)]
     [else (k v)]))]
   [else (k v)])))

;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;9. Define and test a procedure ack-cps that is a CPSed version of the following ack procedure: 

;; ack: computes the Ackermann function
;; (http://en.wikipedia.org/wiki/Ackermann_function).  Warning: if you
;; run this program with m >= 4 and n >= 2, you'll be in for a long
;; wait.
(define ack
 (lambda (m n)
  (cond
   [(zero? m) (add1 n)]
   [(zero? n) (ack (sub1 m) 1)]
   [else (ack (sub1 m) (ack m (sub1 n)))])))


(define ack-cps
 (lambda (m n k)
  (cond
   [(zero? m) (k (add1 n))]
   [(zero? n) (ack-cps (sub1 m) 1 k)]
   [else (ack-cps m (sub1 n) (lambda (x) (ack-cps (sub1 m) x k)))])))


;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;10. Define and test a procedure fib-cps that is a CPSed version of the following fib procedure: 

(define fib
 (lambda (n)
  ((lambda (fib)
    (fib fib n))
   (lambda (fib n)
    (cond
     [(zero? n) 0]
     [(= 1 n) 1]
     [else (+ (fib fib (sub1 n)) (fib fib (sub1 (sub1 n))))])))))


(define fib-cps
 (lambda (n k)
  ((lambda (fib-cps k)
    (fib-cps fib-cps n k))
   (lambda (fib-cps n k)
    (cond
     [(zero? n) (k 0)]
     [(= 1 n) (k 1)]
     [else (fib-cps fib-cps (sub1 (sub1 n)) (lambda (x) (fib-cps fib-cps (sub1 n) (lambda (y) (k (+ x y))))))])) 
   k)))


;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;11. Define and test a procedure unfold-cps that is a CPSed version of the following unfold procedure: 

(define unfold
 (lambda (p f g seed)
  ((lambda (h)
    ((h h) seed '()))
   (lambda (h)
    (lambda (seed ans)
     (if (p seed) 
      ans 
      ((h h) (g seed) (cons (f seed) ans))))))))

(define unfold-cps
 (lambda (p f g seed k)
  ((lambda (h k)
    (h h (lambda (x) (x seed '() k))))
   (lambda (h k)
    (k (lambda (seed ans k)
        (p seed (lambda (pv)
        (if pv (k ans) (h h (lambda (hh) (g seed (lambda (gs) (f seed (lambda (fs) (hh gs (cons fs ans) k)))))))))))))
   k)))



;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;12. Define and test a procedure unify-cps that uses your walk-cps from question 5. Treat extend-s as simple. 

(define empty-s
 (lambda ()
  '()))
 
(define extend-s
 (lambda (x v s)
  (cons `(,x . ,v) s)))
 
(define unify
 (lambda (v w s)
  (let ([v (walk v s)])
   (let ([w (walk w s)])
    (cond
     [(eqv? v w) s]
     [(symbol? v) (extend-s v w s)]
     [(symbol? w) (extend-s w v s)]
     [(and (pair? v) (pair? w))
      (let ((s (unify (car v) (car w) s)))
       (cond
        [s (unify (cdr v) (cdr w) s)]
        [else #f]))]
     [(equal? v w) s]
     [else #f])))))


(define unify-cps
 (lambda (v w s k)
  (walk-cps v s (lambda (v1) (let ((v v1)) (walk-cps w s (lambda (v2) (let ((w v2))
   (cond 
    [(eqv? v w) (k s)]
    [(symbol? v) (k (extend-s v w s))]
    [(symbol? w) (k (extend-s w v s))]
    [(and (pair? v) (pair? w))
     (unify-cps (car v) (car w) s (lambda (v3) (let ((s v3))
      (cond
       [s (unify-cps (cdr v) (cdr w) s (lambda (v4) (k v4)))]
       [else (k #f)]))))]
    [(equal? v w) (k s)]
    [else (k #f)])))))))))



;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;13. Define and test a procedure M-cps that is a CPSed version of M, which is a curried version of map. Assume for the CPSed version that any f passed in will also be CPSed. 

(define M
 (lambda (f)
  (lambda (ls)
   (cond
    ((null? ls) '())
    (else (cons (f (car ls)) ((M f) (cdr ls))))))))


(define M-cps
  (lambda (f k)
   (k (lambda (ls k)
      (cond
        ((null? ls) (k '()))
        (else (M-cps f (lambda (x) (x (cdr ls) (lambda (y) (f (car ls) (lambda (z) (k (cons z y))))))))))))))


;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;14. Consider the corresponding call to M, called use-of-M. Using your CPSed M-cps, re-write use-of-M to call M-cps, and make all the appropriate changes (including CPSing the argument). Name it use-of-M-cps

(define use-of-M
 ((M (lambda (n) (add1 n))) '(1 2 3 4 5)))


(define use-of-M-cps
  (M-cps (lambda (n k) (k (add1 n))) (lambda (x) (x  '(1 2 3 4 5) (empty-k)))))

;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;Brainteasers -------
;15. CPS the following program, and call it strange-cps: 


(define strange
  (lambda (x)
    ((lambda (g) (lambda (x) (g g)))
     (lambda (g) (lambda (x) (g g))))))


(define strange-cps
 (lambda (x k)
  ((lambda (g k) (k (lambda (x k) (g g k))))
   (lambda (g k) (k (lambda (x k) (g g k)))) k)))


;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;16. Consider the following use of strange, called use-of-strange. Using your CPSed strange, re-write use-of-strange to call strange-cps, and make all the appropriate changes. Name it use-of-strange-cps. 

(define use-of-strange
 (let ([strange^ (((strange 5) 6) 7)])
  (((strange^ 8) 9) 10)))

(define use-of-strange-cps
 (strange-cps 5 (lambda (a) (a 6 (lambda (b) (b 7 (lambda (x) (let ((strange^ x))
  (strange^ 8 (lambda (v1) (v1 9 (lambda (v2) (v2 10 (empty-k))))))))))))))


;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;17. CPS the following program, and call it why-cps: 

(define why
 (lambda (f)
  ((lambda (g)
    (f (lambda (x) ((g g) x))))
   (lambda (g)
    (f (lambda (x) ((g g) x)))))))

(define almost-length
 (lambda (f)
  (lambda (ls)
   (if (null? ls)
    0
    (add1 (f (cdr ls)))))))


(define why-cps
 (lambda (f k)
  ((lambda (g k)
    (f (lambda (x k) (g g (lambda (v) (v x k)))) k))
   (lambda (g k)
    (f (lambda (x k) (g g (lambda (v) (v x k)))) k)) 
   k)))


(define almost-length-cps
 (lambda (f k)
  (k (lambda (ls k)
   (if (null? ls)
    (k 0)
    (f (cdr ls) (lambda (x) (k (add1 x)))))))))


;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;Just Dessert -----
;18. CPS why-cps, and call it why-cps-cps. 

(define why-cps-cps
  (lambda (f k k1)
    ((lambda (g k k1)
       (f (lambda (x k k1) (g g (lambda (v k1) (v x k k1)) k1)) k k1))
     (lambda (g k k1)
       (f (lambda (x k k1) (g g (lambda (v k1) (v x k k1)) k1)) k k1)) k k1)))



(define almost-length-cps-cps
    (lambda (f k k1)
      (k (lambda (ls k k1)
        (if (null? ls)
            (k 0 k1)
            (f (cdr ls) (lambda (x k1) (k (add1 x) k1)) k1))) k1)))



;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------

