#lang racket
(require C311/pmatch)
(require C311/trace)


;;-----------------------------------------------------------------------------------------------------------------

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

(define empty-env
 (lambda ()
  (list 'empty-env)))

(define extend-env
 (lambda (x a env)
  (list 'extend-env x a env)))

(define apply-env-ds
 (lambda (env y k)
  (pmatch env
   (`(empty-env) (error 'error))
   (`(extend-env ,x ,a ,env) (if (eqv? y x) (apply-k-ds k a) (apply-env-ds env y k))))))

(define closure
 (lambda (x body env)
  (list 'closure x body env)))

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


(value-of-cps-ds fact-5 (empty-env) (empty-k-ds))
;120
(value-of-cps-ds capture-fun (empty-env) (empty-k-ds))
;12



(value-of-cps-ds
    '((lambda (x) (if (zero? x) 
                      12 
                      47)) 
       0) 
    (empty-env) (empty-k-ds))
;12    

