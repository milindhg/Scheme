#lang racket
(require C311/pmatch)
(require C311/trace)
;;(require "a4-student-tests.rkt")
;;(test-file #:file-name "a4.rkt")


;;Discussed brainteaser problem with Renuka Deshmukh and David Kempe for various approaches and scenarios.
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;PART I
;;Interpreter: value-of-fn
;;

(define value-of-fn
 (lambda (exp env)
  (pmatch exp
   (`,n (guard (number? n)) n)
   (`,x (guard (symbol? x)) (apply-env env x))
   (`,b (guard (boolean? b)) b)
   (`(zero? ,n-exp) (zero? (value-of-fn n-exp env)))
   (`(* ,x ,y) (* (value-of-fn x env) (value-of-fn y env)))
   (`(let ([ ,x ,val ]) ,body) (value-of-fn body (extend-env x (value-of-fn val env) env)))
   (`(sub1 ,x) (sub1 (value-of-fn x env)))
   (`(if ,test-exp ,then-exp ,else-exp) (if (value-of-fn test-exp env) (value-of-fn then-exp env) (value-of-fn else-exp env)))
   (`(lambda (,x) ,body) (closure-fn x body env))
   (`(,rator ,rand) (apply-closure-fn (value-of-fn rator env) (value-of-fn rand env))))))


(define empty-env
 (lambda ()
  (list 'empty-env)))

(define extend-env
 (lambda (x a env)
  (list 'extend-env x a env)))

(define apply-env
 (lambda (env y)
  (pmatch env
   (`(empty-env) (error 'error))
   (`(extend-env ,x ,a ,env) (if (eqv? y x) a (apply-env env y))))))

(define closure-fn
 (lambda (x body env)
  (lambda (a)
   (value-of-fn body (extend-env x a env)))))

(define apply-closure-fn
 (lambda (p a)
  (p a)))


;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;Interpreter: value-of-ds

(define value-of-ds
 (lambda (exp env)
  (pmatch exp
   (`,n (guard (number? n)) n)
   (`,x (guard (symbol? x)) (apply-env env x))
   (`,b (guard (boolean? b)) b)
   (`(zero? ,n-exp) (zero? (value-of-ds n-exp env)))
   (`(* ,x ,y) (* (value-of-ds x env) (value-of-ds y env)))
   (`(let ([ ,x ,val ]) ,body) (value-of-ds body (extend-env x (value-of-ds val env) env)))
   (`(sub1 ,x) (sub1 (value-of-ds x env)))
   (`(if ,test-exp ,then-exp ,else-exp) (if (value-of-ds test-exp env) (value-of-ds then-exp env) (value-of-ds else-exp env)))
   (`(lambda (,x) ,body) (closure-ds x body env))
   (`(,rator ,rand) (apply-closure-ds (value-of-ds rator env) (value-of-ds rand env))))))



(define closure-ds
 (lambda (x body env)
  (list 'closure x body env)))

(define apply-closure-ds
 (lambda (p a)
  (pmatch p
   (`(closure ,x ,body, env) (value-of-ds body (extend-env x a env))))))



;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;PART II
;;Interpreter: value-of-scopes

(define value-of-scopes
 (lambda (exp env)
  (pmatch exp
   (`,n (guard (number? n)) n)
   (`,x (guard (symbol? x)) (apply-env env x))
   (`,b (guard (boolean? b)) b)
   (`(quote()) '())
   (`(null? ,ls) (null? (value-of-scopes ls env)))
   (`(cons ,a ,d) (cons (value-of-scopes a env) (value-of-scopes d env)))
   (`(car ,ls) (car (value-of-scopes ls env)))
   (`(cdr ,ls) (cdr (value-of-scopes ls env)))
   (`(zero? ,n-exp) (zero? (value-of-scopes n-exp env)))
   (`(* ,x ,y) (* (value-of-scopes x env) (value-of-scopes y env)))
   (`(let ([ ,x ,val ]) ,body) (value-of-scopes body (extend-env x (value-of-scopes val env) env)))
   (`(sub1 ,x) (sub1 (value-of-scopes x env)))
   (`(if ,test-exp ,then-exp ,else-exp) (if (value-of-scopes test-exp env) (value-of-scopes then-exp env) (value-of-scopes else-exp env)))
   (`(lambda (,x) ,body) (closure x body env))
   (`(d-lambda (,x) ,body) (closure-scopes x body env))
   (`(,rator ,rand) (apply-closure-scopes (value-of-scopes rator env) (value-of-scopes rand env) env)))))



(define closure
 (lambda (x body env)
  (list 'closure x body env)))

(define closure-scopes
 (lambda (x body env)
  (list 'closure-scopes x body env)))

(define apply-closure-scopes
 (lambda (p a env^)
  (pmatch p
   (`(closure ,x ,body, env) (value-of-scopes body (extend-env x a env)))
   (`(closure-scopes ,x ,body, env) (value-of-scopes body (extend-env x a env^))))))


;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;4. Brainteaser
;;Interpreter: value-of-ri
;;Discussed brainteaser problem with Renuka Deshmukh and David Kempe for various approaches and scenarios.


(define value-of-ri
 (lambda (empty-env extend-env apply-env closure apply-closure)
  (lambda (exp)
   (letrec (
    [lambda-exp exp]
    [interpreter (lambda (myexp env) 
     (pmatch myexp
      (`,n (guard (number? n)) n)
      (`,x (guard (symbol? x)) (apply-env env x))
      (`,b (guard (boolean? b)) b)
      (`(zero? ,n-exp) (zero? (interpreter n-exp env)))
      (`(* ,x ,y) (* (interpreter x env) (interpreter y env)))
      (`(let ([ ,x ,val ]) ,body) (interpreter body (extend-env x (interpreter val env) env)))
      (`(sub1 ,x) (sub1 (interpreter x env)))
      (`(if ,test-exp ,then-exp ,else-exp) (if (interpreter test-exp env) (interpreter then-exp env) (interpreter else-exp env)))
      (`(lambda (,x) ,body) (closure x body env))
      (`(,rator ,rand) (apply-closure (interpreter rator env) (interpreter rand env)))))]
    [env empty-env]) (interpreter lambda-exp env)))))


(define empty-env-fn
 (lambda ()
  (lambda (y)
   (error 'value-of "unbound variable ~s" y))))

(define extend-env-fn
 (lambda (x a env)
  (lambda (y)
   (if (eqv? x y) a (apply-env-fn env y)))))

(define apply-env-fn
 (lambda (env y)
  (env y)))


(define empty-env-ds
 (lambda ()
  (list 'empty-env)))

(define extend-env-ds
 (lambda (x a env)
  (list 'extend-env-ds x a env)))

(define apply-env-ds
 (lambda (env y)
  (pmatch env
   (`(empty-env-ds) (error 'error))
   (`(extend-env-ds ,x ,a ,env) (if (eqv? y x) a (apply-env-ds env y))))))


(define closure-ds-ri
 (lambda (x body env)
  (list 'closure-ds-ri x body env)))

(define apply-closure-ds-ri
 (lambda (p a)
  (pmatch p
   (`(closure-ds-ri ,x ,body, env) (if (list? env) 
   ((value-of-ri (extend-env-ds x a env) extend-env-ds apply-env-ds closure-ds-ri apply-closure-ds-ri) body) 
   ((value-of-ri (extend-env-fn x a env) extend-env-fn apply-env-fn closure-ds-ri apply-closure-ds-ri) body))))))


(define closure-fn-ri
 (lambda (x body env)
  (lambda (a)
   (if (list? env) 
    ((value-of-ri (extend-env-ds x a env) extend-env-ds apply-env-ds closure-fn-ri apply-closure-fn-ri) body) 
    ((value-of-ri (extend-env-fn x a env) extend-env-fn apply-env-fn closure-fn-ri apply-closure-fn-ri) body)))))

	
(define apply-closure-fn-ri
 (lambda (p a)
  (p a)))



;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
