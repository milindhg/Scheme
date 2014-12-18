#lang racket
(require C311/pmatch) ;; <-- important
(require C311/trace)
;;(require "a3-student-tests.rkt")
;;(test-file #:file-name "a3.rkt")


;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;Interpreter: value-of - covers problems 1, 4 and 6
;;1. The representation dependent version of interpreter along with implementation for handling the following forms: numbers, booleans, variables, lambda-abstraction, application, zero?, sub1, *, if, and let.
;;4. You should add to your representation dependent interpreter, value-of, forms which handle functions of two arguments and applications of two arguments. As this too must be implemented compositionally, you cannot simply implement it as nested calls to the lambda-abstraction and application forms you've defined previously
;;6. Extend your interpreter value-of to support set! and begin2, where begin2 is a variant of Racket's begin that takes exactly two arguments, and set! implements performing side-effects.
;;Discussed problem 6 test cases with Renuka Deshmukh and David Kempe for a walk through in various scenarios. 

(define value-of
 (lambda (exp env)
  (pmatch exp
   (`,n (guard (number? n)) n)
   (`,x (guard (symbol? x)) (unbox (env x)))
   (`,b (guard (boolean? b)) b)
   (`(zero? ,n-exp) (zero? (value-of n-exp env)))
   (`(* ,x ,y) (* (value-of x env) (value-of y env)))
   (`(sub1 ,x) (sub1 (value-of x env)))
   (`(let ([,x ,val]) ,body) (value-of body (lambda (y) (if (eqv? y x) (box (value-of val env)) (env y))))) 
   (`(if ,test-exp ,then-exp ,else-exp) (if (value-of test-exp env) (value-of then-exp env) (value-of else-exp env)))
   (`(lambda (,x) ,body) (lambda (a) (value-of body (lambda (y) (if (eqv? y x) a (env y)))))) 
   (`(lambda (,x ,y) ,body) (lambda (a b) (value-of body (lambda (q) (if (eqv? q x) a (if (eqv? q y) b (env y)))))))
   (`(,rator ,rand) ((value-of rator env) (box (value-of rand env))))
   (`(begin2 ,arg1 ,arg2) (begin (value-of arg1 env) (value-of arg2 env)))
   (`(set! ,x ,valexp) (set-box! (env x) (value-of valexp env)))
   (`(,rator ,rand1 ,rand2) ((value-of rator env) (box (value-of rand1 env)) (box (value-of rand2 env)))))))


;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;Interpreter: value-of-fn - covers problem 2
;;2. The interpreter with functional representation of environment along with implementation for handling the following forms: numbers, booleans, variables, lambda-abstraction, application, zero?, sub1, *, if, and let.

(define value-of-fn
 (lambda (exp env)
  (pmatch exp
   (`,n (guard (number? n)) n)
   (`,x (guard (symbol? x)) (apply-env-fn env x))
   (`,b (guard (boolean? b)) b)
   (`(zero? ,n-exp) (zero? (value-of-fn n-exp env)))
   (`(* ,x ,y) (* (value-of-fn x env) (value-of-fn y env)))

   (`(let ([ ,x ,val ]) ,body) (value-of-fn body (extend-env-fn x (value-of-fn val env) env)))

   (`(sub1 ,x) (sub1 (value-of-fn x env)))
   (`(if ,test-exp ,then-exp ,else-exp) (if (value-of-fn test-exp env) (value-of-fn then-exp env) (value-of-fn else-exp env)))
   (`(lambda (,x) ,body) (lambda (a) (value-of-fn body (extend-env-fn x a env))))
   (`(,rator ,rand) ((value-of-fn rator env) (value-of-fn rand env))))))

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


;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;Interpreter: value-of-fn - covers problem 3
;;3. The interpreter with data structure representation of environment along with implementation for handling the following forms: numbers, booleans, variables, lambda-abstraction, application, zero?, sub1, *, if, and let.

(define value-of-ds
 (lambda (exp env)
  (pmatch exp
   (`,n (guard (number? n)) n)
   (`,x (guard (symbol? x)) (apply-env-ds env x))
   (`,b (guard (boolean? b)) b)
   (`(zero? ,n-exp) (zero? (value-of-ds n-exp env)))
   (`(* ,x ,y) (* (value-of-ds x env) (value-of-ds y env)))

   (`(let ([ ,x ,val ]) ,body) (value-of-ds body (extend-env-ds x (value-of-ds val env) env)))

   (`(sub1 ,x) (sub1 (value-of-ds x env)))
   (`(if ,test-exp ,then-exp ,else-exp) (if (value-of-ds test-exp env) (value-of-ds then-exp env) (value-of-ds else-exp env)))
   (`(lambda (,x) ,body) (lambda (a) (value-of-ds body (extend-env-ds x a env))))
   (`(,rator ,rand) ((value-of-ds rator env) (value-of-ds rand env))))))


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


;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;Interpreter: fo-eulav - covers problem 5
;;5. Having successfully completed the first part of the assignment, you are now well prepared to implement fo-eulav. Sample runs are provided below. Only forms present in those sample runs are required. You can use the same empty-env as above. 

(define fo-eulav
 (lambda (exp env)
  (pmatch exp
   (`,n (guard (number? n)) n)
   (`,x (guard (symbol? x)) (env x))
   (`,b (guard (boolean? b)) b)
   (`(,x ?orez) (zero? (fo-eulav x env)))
   (`(,x 1bus) (sub1 (fo-eulav x env)))
   (`(,y ,x *) (* (fo-eulav x env) (fo-eulav y env)))
   (`(,else-exp ,then-exp ,test-exp fi) (if (fo-eulav test-exp env) (fo-eulav then-exp env) (fo-eulav else-exp env)))
   (`(,rand ,rator) ((fo-eulav rator env) (fo-eulav rand env)))
   (`(,body (,x) adbmal) (lambda (a) (fo-eulav body (lambda (y) (if (eqv? y x) a (env y)))))))))


(define empty-env
  (lambda ()
    (lambda (y)
      (error 'value-of "unbound variable ~s" y))))

;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;Interpreter: value-of-lex which covers problem 7
;;7. Consider the following interpreter for a deBruijnized version of the lambda-calculus (i.e. lambda-calculus expressions using lexical addresses instead of variables). Notice this interpreter is representation-independent with respect to environments.
;;Discussed problems and test cases with Renuka Deshmukh for a walk through. 


(define value-of-lex
 (lambda(exp env)
  (pmatch exp
   (`,c (guard (or (boolean? c) (number? c))) c)
   (`(sub1 ,body) (sub1 (value-of-lex body env)))
   (`(zero? ,body) (zero? (value-of-lex body env)))
   (`(* ,n1 ,n2) (* (value-of-lex n1 env) (value-of-lex n2 env)))
   (`(if ,t ,c ,a) (if (value-of-lex t env) (value-of-lex c env) (value-of-lex a env)))
   (`(var ,num) (apply-env-lex env num))
   (`(lambda ,body) (lambda (a) (value-of-lex body (extend-env-lex a env))))
   (`(,rator ,rand) ((value-of-lex rator env) (value-of-lex rand env))))))


(define empty-env-lex
 (lambda () '()))

(define extend-env-lex
 (lambda (a env)
  (list 'extend-env-lex a env)))

(define apply-env-lex
 (lambda (env num)
  (pmatch env
   (`(empty-env-lex) (error 'error))
   (`(extend-env-lex ,a ,env) (cond ((zero? num) a) (else (apply-env-lex env (sub1 num))))))))



;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
