#lang racket
(require C311/let-pair)
(provide (all-defined-out))
(require C311/trace)
(require C311/pmatch)
(require C311/monads)
;;(require "a12-student-tests.rkt")
;;(test-file #:file-name "a12.rkt")

;;----------------------------------------------------------------------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------------------------------------------------------------------
;;Maybe Monad
;;1. assv-maybe

(define assv-maybe
 (lambda (x ls)
  (cond
   [(eqv? ls '()) (fail)]
   [(eqv? (car (car ls)) x) (return-maybe (cdr (car ls)))]
   [else (assv-maybe x (cdr ls))])))

;;----------------------------------------------------------------------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------------------------------------------------------------------
;;Writer Monad
;;2. partition-writer

(define partition-writer
 (lambda (pred ls)
  (cond
   [(eqv? ls '()) (return-writer '())]
   [(pred (car ls)) (bind-writer (tell-writer (car ls)) (lambda (x) (partition-writer pred (cdr ls))))]
   [else (bind-writer (partition-writer pred (cdr ls)) (lambda (d) (return-writer (cons (car ls) d))))])))

;;----------------------------------------------------------------------------------------------------------------------------------------------
;;Writer Monad
;;3. powerXpartials

(define power
 (lambda (x n)
  (cond
   [(zero? n) 1]
   [(= n 1) x]
   [(odd? n) (* x (power x (sub1 n)))]
   [(even? n) 
    (let ((nhalf (/ n 2)))
     (let ((y (power x nhalf)))
      (* y y)))])))

(define powerXpartials
 (lambda (x n)
  (cond
   [(zero? n) (return-writer 1)]
   [(= n 1) (return-writer x)]
   [(odd? n) (bind-writer (powerXpartials x (sub1 n)) (lambda (d) `(,(* x d) . (,d))))]
   [(even? n) 
    (let ((nhalf (/ n 2)))
     (bind-writer (powerXpartials x nhalf) (lambda (d) `(,(* d d) . (,d)))))])))

;;----------------------------------------------------------------------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------------------------------------------------------------------
;;State Monad
;;4. abc-game

(define even-length?
 (lambda (l)
  (cond
   [(null? l) (return-state '_)]
   [else
    (do bind-state
     (s <- get-state)
     (put-state (not s))
     (even-length? (cdr l)))])))

(define abc-game
 (lambda (l)
  (cond
   [(null? l) (return-state '__)]
   [(eqv? (car l) 'a)
    (do bind-state
     (s <- get-state)
     (put-state (add1 s))
     (abc-game (cdr l)))]
   [(eqv? (car l) 'b)
    (do bind-state
     (s <- get-state)
     (put-state (sub1 s))
     (abc-game (cdr l)))]
   [else
    (do bind-state
     (abc-game (cdr l)))])))

;;----------------------------------------------------------------------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------------------------------------------------------------------
;;Mixed Monad Problems
;;5. reciprocal

(define traverse
 (lambda (return bind f)
  (letrec
   ((trav
    (lambda (tree)
     (cond
      [(pair? tree)
       (do bind
        (a <- (trav (car tree)))
        (d <- (trav (cdr tree)))
        (return (cons a d)))]
      [else (f tree)]))))
   trav)))

(define reciprocal
 (lambda (n)
  (cond
   [(zero? n) (fail)]
   [else (return-maybe (/ 1 n))])))

(define traverse-reciprocal
 (traverse return-maybe bind-maybe reciprocal))

;;----------------------------------------------------------------------------------------------------------------------------------------------
;;Mixed Monad Problems
;;6. halve

(define halve
 (lambda (n)
  (cond 
   [(even? n) (return-writer (/ n 2))]
   [else (bind-writer (tell-writer n) (lambda (_) (return-writer n)))])))

(define traverse-halve
 (traverse return-writer bind-writer halve))

;;----------------------------------------------------------------------------------------------------------------------------------------------
;;Mixed Monad Problems
;;7. state/sum

(define state/sum
 (lambda (n)
  (do bind-state 
   (s <- get-state)
   (put-state (+ n s))
   (return-state s))))

(define traverse-state/sum
 (traverse return-state bind-state state/sum))

;;----------------------------------------------------------------------------------------------------------------------------------------------
;;Brainteaser
;;8. monadic interpreter 

(define empty-k
 (lambda ()
  (let ((once-only #f))
   (lambda (v)
    (if once-only 
     (error 'empty-k "You can only invoke the empty continuation once")
     (begin (set! once-only #t) v))))))


(define empty-env
 (lambda ()
  (list 'empty-env)))

(define extend-env
 (lambda (x a env)
  (list 'extend-env x a env)))

(define apply-env
 (lambda (env y)
  (pmatch env
   [`(empty-env) (error 'error)]
   [`(extend-env ,x ,a ,env) (if (eqv? y x) (return-cont a) (bind-cont (apply-env env y) (lambda (a) (return-cont a))))])))

(define closure
 (lambda (x body env)
  (list 'closure x body env)))

(define apply-closure
 (lambda (p a)
  (pmatch p
   [`(closure ,x ,body, env) (bind-cont (value-of-cps body (extend-env x a env)) (lambda (a) (return-cont a)))])))

(define fact-5
 '((lambda (f)
   ((f f) 5))
   (lambda (f)
    (lambda (n)
     (if (zero? n)
      1
      (* n ((f f) (sub1 n))))))))

(define capture-fun
 '(* 3 (capture q (* 2 (return 4 q)))))

(define value-of-cps
 (lambda (expr env)
  (pmatch expr
   [`,n (guard (or (number? n) (boolean? n))) (return-cont n)]
   [`(+ ,x1 ,x2) (bind-cont (value-of-cps x1 env) (lambda (x1^) (bind-cont (value-of-cps x2 env) (lambda (x2^) (return-cont (+ x1^ x2^))))))]
   [`(* ,x1 ,x2) (bind-cont (value-of-cps x1 env) (lambda (x1^) (bind-cont (value-of-cps x2 env) (lambda (x2^) (return-cont (* x1^ x2^))))))]
   [`(sub1 ,x) (bind-cont (value-of-cps x env) (lambda (x^) (return-cont (sub1 x^))))]
   [`(zero? ,x) (bind-cont (value-of-cps x env) (lambda (x^) (return-cont (zero? x^))))]
   [`(if ,test ,conseq ,alt) 
    (bind-cont 
     (value-of-cps test env) 
     (lambda (test^) 
      (if test^
       (bind-cont (value-of-cps conseq env) (lambda (a) (return-cont a)))
       (bind-cont (value-of-cps alt env) (lambda (b) (return-cont b))))))]
   [`(capture ,k-id ,body) 
    (bind-cont 
     (callcc 
      (lambda (k) 
       (bind-cont 
        (value-of-cps body (extend-env k-id k env)) (lambda (b) (return-cont b))))) 
      (lambda (a) (return-cont a)))]
   [`(return ,v-exp ,k-exp) (bind-cont (value-of-cps k-exp env) (lambda (k-exp^) (bind-cont (value-of-cps v-exp env) k-exp^)))]
   [`,x (guard (symbol? x)) (bind-cont (apply-env env x) (lambda (d) (return-cont d)))]
   [`(lambda (,id) ,body) (return-cont (closure id body env))]
   [`(,rator ,rand)
    (bind-cont 
     (value-of-cps rator env) 
      (lambda (closure^) 
       (bind-cont 
        (value-of-cps rand env) 
         (lambda (arg) (bind-cont (apply-closure closure^ arg) (lambda (h) (return-cont h)))))))])))



