#lang racket
(require C311/pmatch)
(require C311/trace)

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
    [env empty-env])(interpreter lambda-exp env)))))


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
;Testcases


((value-of-ri empty-env-fn extend-env-fn apply-env-fn closure-fn-ri apply-closure-fn-ri) '((lambda (x) x) 5))
;5

((value-of-ri empty-env-ds extend-env-ds apply-env-ds closure-ds-ri apply-closure-ds-ri) '((lambda (x) x) 5))
;5

((value-of-ri empty-env-fn extend-env-fn apply-env-fn closure-ds-ri apply-closure-ds-ri) '((lambda (x) x) 5))
;5

((value-of-ri empty-env-ds extend-env-ds apply-env-ds closure-fn-ri apply-closure-fn-ri) '((lambda (x) x) 5))
;5

((value-of-ri empty-env-ds extend-env-ds apply-env-ds closure-fn-ri apply-closure-fn-ri)
'((lambda (x) (if (zero? x) 
                     12 
                     47)) 
     0))
;12

((value-of-ri empty-env-ds extend-env-ds apply-env-ds closure-ds-ri apply-closure-ds-ri)
'(let ((! (lambda (x) (* x x))))
      (let ((! (lambda (n)
                 (if (zero? n) 1 (* n (! (sub1 n)))))))
        (! 5))))
;80

((value-of-ri empty-env-fn extend-env-fn apply-env-fn closure-fn-ri apply-closure-fn-ri)
 '(((lambda (f)
        (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
      (lambda (f)
        (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
     5))
;120

