#lang racket
(require C311/pmatch)
(require C311/trace)



;;------------------------------------------------------------------------------------------------------------------------
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


(define empty-env
 (lambda ()
  (list 'empty-env)))

(define extend-env
 (lambda (x a env)
  (list 'extend-env x a env)))

(define apply-env-fn
 (lambda (env y k)
  (pmatch env
   (`(empty-env) (error 'error))
   (`(extend-env ,x ,a ,env) (if (eqv? y x) (apply-k-fn k a) (apply-env-fn env y k))))))

(define closure
 (lambda (x body env)
  (list 'closure x body env)))

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



(value-of-cps-fn fact-5 (empty-env) (empty-k-fn))
;120
(value-of-cps-fn capture-fun (empty-env) (empty-k-fn))
;12



(value-of-cps-fn
    '((lambda (x) (if (zero? x) 
                      12 
                      47)) 
       0) 
    (empty-env) (empty-k-fn))
;12    


