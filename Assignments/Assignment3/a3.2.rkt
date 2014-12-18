#lang racket
(require C311/pmatch) ;; <-- important
(require C311/trace)


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


;;--------------------------------------------------------------------------
;TestCases


(value-of-fn
'((lambda (x) (if (zero? x)
12
47))
0)
(empty-env-fn))

(value-of-fn
'((lambda (x) (if (zero? x)
12
47))
10)
(empty-env-fn))


(value-of-fn
'(((lambda (f)
(lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
(lambda (f)
(lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
5)
(empty-env-fn))


(value-of-fn
'(let ([y (* 3 4)])
((lambda (x) (* x y)) (sub1 6)))
(empty-env-fn))
;;60

(value-of-fn
'(let ([x (* 2 3)])
(let ([y (sub1 x)])
(* x y)))
(empty-env-fn))
;;30

(value-of-fn
'(let ([x (* 2 3)])
(let ([x (sub1 x)])
(* x x)))
(empty-env-fn))
;;25

(value-of-fn
'(((lambda (f)
(lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
(lambda (f)
(lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
5)
(empty-env-fn))
;;120

