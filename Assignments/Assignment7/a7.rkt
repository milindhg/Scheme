#lang racket
(require C311/trace)
(require C311/pmatch)
;;(require "a7-student-tests.rkt")
;;(test-file #:file-name "a7.rkt")


(define empty-k
 (lambda ()
  (let ((once-only #f))
   (lambda (v)
    (if once-only 
     (error 'empty-k "You can only invoke the empty continuation once")
     (begin (set! once-only #t) v))))))

;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;Define and test a procedure binary-to-decimal-cps that is a CPSed version of the following binary-to-decimal procedure:


(define binary-to-decimal
 (lambda (n)
  (cond
   [(null? n) 0]
   [else (+ (car n) (* 2 (binary-to-decimal (cdr n))))])))


(define binary-to-decimal-cps
 (lambda (n k)
  (cond
   [(null? n) (k 0)]
   [else (binary-to-decimal-cps (cdr n) (lambda (x) (k (+ (car n) (* 2 x)))))])))


;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;Define and test a procedure rember*1-cps that is a CPSed version of the following rember*1 procedure, which removes the first ? in the arbitrarily nested list ls:

(define rember*1
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(pair? (car ls))
       (cond
         [(equal? (car ls) (rember*1 (car ls)))
          (cons (car ls) (rember*1 (cdr ls)))]
         [else (cons (rember*1 (car ls)) (cdr ls))])]
      [(eqv? (car ls) '?) (cdr ls)]
      [else (cons (car ls) (rember*1 (cdr ls)))])))


(define rember*1-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k '())]
      [(pair? (car ls))
       (cond
         [(rember*1-cps (car ls) (lambda (x) (equal? (car ls) x)))
          (rember*1-cps (cdr ls) (lambda (y) (k (cons (car ls) y))))]
         [else (rember*1-cps (car ls) (lambda (p) (k (cons p (cdr ls)))))])]
      [(eqv? (car ls) '?) (k (cdr ls))]
      [else (rember*1-cps (cdr ls) (lambda (z) (k (cons (car ls) z))))])))


;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;value-of-cps - CPSed interpreter that is representation-dependent WRT continuations

(define empty-env
 (lambda ()
  (list 'empty-env)))

(define extend-env
 (lambda (x a env)
  (list 'extend-env x a env)))

(define apply-env
 (lambda (env y k)
  (pmatch env
   (`(empty-env) (error 'error))
   (`(extend-env ,x ,a ,env) (if (eqv? y x) (k a) (apply-env env y k))))))

(define closure
 (lambda (x body env)
  (list 'closure x body env)))

(define apply-closure
 (lambda (p a k)
  (pmatch p
   (`(closure ,x ,body, env) (value-of-cps body (extend-env x a env) k)))))


(define value-of-cps
  (lambda (expr env k)
    (pmatch expr
      [`,n (guard (or (number? n) (boolean? n))) (k n)]
      [`(+ ,x1 ,x2) (value-of-cps x1 env (lambda (x1^) (value-of-cps x2 env (lambda (x2^) (k (+ x1^ x2^))))))]
      [`(* ,x1 ,x2) (value-of-cps x1 env (lambda (x1^) (value-of-cps x2 env (lambda (x2^) (k (* x1^ x2^))))))]
      [`(sub1 ,x) (value-of-cps x env (lambda (x^) (k (sub1 x^))))]
      [`(zero? ,x) (value-of-cps x env (lambda (x^) (k (zero? x^))))]
      [`(if ,test ,conseq ,alt) (value-of-cps test env (lambda (test^) (if test^
                                                                           (value-of-cps conseq env k)
                                                                           (value-of-cps alt env k))))]
      [`(capture ,k-id ,body) (value-of-cps body (extend-env k-id k env) k)]
      [`(return ,v-exp ,k-exp) (value-of-cps k-exp env (lambda (k-exp^) (value-of-cps v-exp env k-exp^)))]
      [`,x (guard (symbol? x)) (apply-env env x k)]
      [`(lambda (,id) ,body) (k (closure id body env))]
      [`(,rator ,rand) (value-of-cps rator env (lambda (closure^) (value-of-cps rand env (lambda (arg) (apply-closure closure^ arg k)))))])))


;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;value-of-cps-fn - CPSed interpreter that is representation-independent WRT continuations, and uses functional continuation helpers


(define apply-env-fn
 (lambda (env y k)
  (pmatch env
   (`(empty-env) (error 'error))
   (`(extend-env ,x ,a ,env) (if (eqv? y x) (apply-k-fn k a) (apply-env-fn env y k))))))

(define apply-closure-cps-fn
 (lambda (p a k)
  (pmatch p
   (`(closure ,x ,body, env) (value-of-cps-fn body (extend-env x a env) k)))))


(define empty-k-fn
  (lambda ()
    (lambda (v)
      v)))
    
(define apply-k-fn
  (lambda (k v)
    (k v)))

(define inner-k-fn
  (lambda (closure k)
    (lambda (arg)
      (apply-closure-cps-fn closure arg k))))

(define outer-k-fn
  (lambda (rand env k)
    (lambda (closure)
     (value-of-cps-fn rand env (inner-k-fn closure k)))))


(define if-k-fn
  (lambda (conseq alt env k)
    (lambda (test)
     (if test
      (value-of-cps-fn conseq env k)
      (value-of-cps-fn alt env k)))))

(define return-k-fn
  (lambda (v-exp env)
    (lambda (k-exp^) 
     (value-of-cps-fn v-exp env k-exp^))))

(define inner-operand-k-fn
  (lambda (func k^ pred x1)
    (lambda (x2^)
      (func k^ (pred x1 x2^)))))
      
(define outer-operand-k-fn
  (lambda (pred x2 env k^)
    (lambda (x1^)
      (value-of-cps-fn x2 env (inner-operand-k-fn apply-k-fn k^ pred x1^)))))

(define single-operand-k-fn
  (lambda (func k^ pred)
    (lambda (x^)
      (func k^ (pred x^)))))


(define value-of-cps-fn
  (lambda (expr env k)
    (pmatch expr
      [`,n (guard (or (number? n) (boolean? n))) (apply-k-fn k n)]
      [`(+ ,x1 ,x2) (value-of-cps-fn x1 env (outer-operand-k-fn + x2 env k))]
      [`(* ,x1 ,x2) (value-of-cps-fn x1 env (outer-operand-k-fn * x2 env k))]
      [`(sub1 ,x) (value-of-cps-fn x env (single-operand-k-fn apply-k-fn k sub1))]
      [`(zero? ,x) (value-of-cps-fn x env (single-operand-k-fn apply-k-fn k zero?))]
      [`(if ,test ,conseq ,alt) (value-of-cps-fn test env (if-k-fn conseq alt env k))]
      [`(capture ,k-id ,body) (value-of-cps-fn body (extend-env k-id k env) k)]
      [`(return ,v-exp ,k-exp) (value-of-cps-fn k-exp env (return-k-fn v-exp env))]
      [`,x (guard (symbol? x)) (apply-env-fn env x k)]
      [`(lambda (,id) ,body) (apply-k-fn k (closure id body env))]
      [`(,rator ,rand) (value-of-cps-fn rator env (outer-k-fn rand env k))])))



;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;value-ofcps-ds - CPSed interpreter that is representation-independent WRT continuations, and uses data-structural continuation helpers


(define apply-env-ds
 (lambda (env y k)
  (pmatch env
   (`(empty-env) (error 'error))
   (`(extend-env ,x ,a ,env) (if (eqv? y x) (apply-k-ds k a) (apply-env-ds env y k))))))

(define apply-closure-cps-ds
 (lambda (p a k)
  (pmatch p
   (`(closure ,x ,body, env) (value-of-cps-ds body (extend-env x a env) k)))))


(define single-operand-k-ds
  (lambda (func k^ pred)
    (list 'single-operand-k-ds func k^ pred)))


(define apply-k-ds
  (lambda (k v)
    (pmatch k
            [`(empty-k-ds) v]
            [`(inner-k-ds ,closure ,k^) (apply-closure-cps-ds closure v k^)]
            [`(outer-k-ds ,rand ,env ,k^) (value-of-cps-ds rand env (inner-k-ds v k^))]
            [`(if-k-ds ,conseq ,alt ,env ,k^) (if v (value-of-cps-ds conseq env k^) (value-of-cps-ds alt env k^))]
            [`(return-k-ds ,v-exp ,env) (value-of-cps-ds v-exp env v)]
            [`(inner-operand-k-ds ,func ,k^ ,pred ,x1) (func k^ (pred x1 v))]
            [`(outer-operand-k-ds ,pred ,x2 ,env ,k^) (value-of-cps-ds x2 env (inner-operand-k-ds apply-k-ds k^ pred v))]
            [`(single-operand-k-ds ,func ,k^ ,pred) (func k^ (pred v))]
            [else (k v)])))

(define empty-k-ds
  (lambda ()
      (list 'empty-k-ds)))

(define inner-k-ds
  (lambda (closure k^)
      (list 'inner-k-ds closure k^)))

(define outer-k-ds
  (lambda (rand env k^)
    (list 'outer-k-ds rand env k^)))

(define if-k-ds
  (lambda (conseq alt env k^)
    (list 'if-k-ds conseq alt env k^)))

(define return-k-ds
  (lambda (v-exp env)
    (list 'return-k-ds v-exp env)))

(define inner-operand-k-ds
  (lambda (func k^ pred x1)
    (list 'inner-operand-k-ds func k^ pred x1)))
      
(define outer-operand-k-ds
  (lambda (pred x2 env k^)
    (list 'outer-operand-k-ds pred x2 env k^)))


(define value-of-cps-ds
  (lambda (expr env k)
    (pmatch expr
      [`,n (guard (or (number? n) (boolean? n))) (apply-k-ds k n)]
      [`(+ ,x1 ,x2) (value-of-cps-ds x1 env (outer-operand-k-ds + x2 env k))]
      [`(* ,x1 ,x2) (value-of-cps-ds x1 env (outer-operand-k-ds * x2 env k))]
      [`(sub1 ,x) (value-of-cps-ds x env (single-operand-k-ds apply-k-ds k sub1))]
      [`(zero? ,x) (value-of-cps-ds x env (single-operand-k-ds apply-k-ds k zero?))]
      [`(if ,test ,conseq ,alt) (value-of-cps-ds test env (if-k-ds conseq alt env k))]
      [`(capture ,k-id ,body) (value-of-cps-ds body (extend-env k-id k env) k)]
      [`(return ,v-exp ,k-exp) (value-of-cps-ds k-exp env (return-k-ds v-exp env))]
      [`,x (guard (symbol? x)) (apply-env-ds env x k)]
      [`(lambda (,id) ,body) (apply-k-ds k (closure id body env))]
      [`(,rator ,rand) (value-of-cps-ds rator env (outer-k-ds rand env k))])))

;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;Brainteaser
;;


(define-syntax cons$
 (syntax-rules ()
  ((cons$ x y) (cons x (delay y)))))

(define car$ car)

(define cdr$
 (lambda ($) (force (cdr $))))

(define inf-1s (cons$ 1 inf-1s))

(define take$
 (lambda (n $)
  (cond
   ((zero? n) '())
   (else (cons (car$ $) (take$ (sub1 n) (cdr$ $)))))))

(define worst-random
 (delay (random 4)))

(define trib
  (lambda (x1 x2 x3)
    
     (begin
       (let ([sum (+ x1 x2 x3)])
      (cons$ sum (trib x2 x3 sum))
      ))))


(define trib$ (cons$ 0 (cons$ 1 (cons$ 1 (trib 0 1 1)))))


;;-----------------------------------------------------------------------------------------------------------------------------------------------------
