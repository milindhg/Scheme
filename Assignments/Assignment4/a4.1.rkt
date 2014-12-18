#lang racket
(require C311/pmatch)
(require C311/trace)

;Part I
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


;;-------------------------------------------------------------------------------------------
;Testcases

(value-of-fn 
    '((lambda (x) (if (zero? x) 
                      12 
                      47)) 
       0) 
    (empty-env))
;12    


(value-of-fn
   '(let ([y (* 3 4)])
      ((lambda (x) (* x y)) (sub1 6)))
   (empty-env))
;60

(value-of-fn
   '(let ([x (* 2 3)])
      (let ([y (sub1 x)])
        (* x y)))
   (empty-env))
;30


(value-of-fn
   '(let ([x (* 2 3)])
      (let ([x (sub1 x)])
        (* x x)))
   (empty-env))
;25
