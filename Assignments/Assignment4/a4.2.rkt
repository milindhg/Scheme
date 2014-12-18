#lang racket
(require C311/pmatch)
(require C311/trace)

;Part I
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

(define closure-ds
 (lambda (x body env)
  (list 'closure x body env)))

(define apply-closure-ds
 (lambda (p a)
  (pmatch p
   (`(closure ,x ,body, env) (value-of-ds body (extend-env x a env))))))

;;-------------------------------------------------------------------------------------------
;Testcases

(value-of-ds
    '((lambda (x) (if (zero? x) 
                      12 
                      47)) 
       0) 
    (empty-env))
;12    


(value-of-ds
   '(let ([y (* 3 4)])
      ((lambda (x) (* x y)) (sub1 6)))
   (empty-env))
;60


(value-of-ds
  '(let ([x (* 2 3)])
      (let ([y (sub1 x)])
        (* x y)))
   (empty-env))
;30


(value-of-ds
   '(let ([x (* 2 3)])
      (let ([x (sub1 x)])
        (* x x)))
   (empty-env))
;25

