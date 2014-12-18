#lang racket
(require C311/pmatch) ;; <-- important
(require C311/trace)


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

;(trace value-of-ds)

(value-of-ds
'((lambda (x) (if (zero? x)
12
47))
0)
(empty-env-ds))
;;12

(value-of-ds
'((lambda (x) (if (zero? x)
12
47))
10)
(empty-env-ds))
;;47

(value-of-ds
'(((lambda (f)
(lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
(lambda (f)
(lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
5)
(empty-env-ds))
;;120

(value-of-ds
'(let ([y (* 3 4)])
((lambda (x) (* x y)) (sub1 6)))
(empty-env-ds))
;;60

(value-of-ds
'(let ([x (* 2 3)])
(let ([y (sub1 x)])
(* x y)))
(empty-env-ds))
;;30


(value-of-ds
'(let ([x (* 2 3)])
(let ([x (sub1 x)])
(* x x)))
(empty-env-ds))
;;25

