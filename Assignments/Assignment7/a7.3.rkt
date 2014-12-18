#lang racket
(require C311/pmatch)
(require C311/trace)


(define empty-k
 (lambda ()
  (let ((once-only #f))
   (lambda (v)
    (if once-only 
     (error 'empty-k "You can only invoke the empty continuation once")
     (begin (set! once-only #t) v))))))

;(define value-of
;  (lambda (expr env)
;    (pmatch expr
;      [`,n (guard (or (number? n) (boolean? n))) n]
;      [`(+ ,x1 ,x2) (+ (value-of x1 env) (value-of x2 env))]
;      [`(* ,x1 ,x2) (* (value-of x1 env) (value-of x2 env))]
;      [`(sub1 ,x) (sub1 (value-of x env))]
;      [`(zero? ,x) (zero? (value-of x env))]
;      [`(if ,test ,conseq ,alt) (if (value-of test env)
;                                    (value-of conseq env)
;                                    (value-of alt env))]
;      [`(capture ,k-id ,body) (call/cc (lambda (k)
;                                       (value-of body (extend-env k-id k env))))]
;      [`(return ,v-exp ,k-exp) ((value-of k-exp env) (value-of v-exp env))]
;      [`,x (guard (symbol? x)) (apply-env env x)]
;      [`(lambda (,id) ,body) (closure id body env)]
;      [`(,rator ,rand) (apply-closure (value-of rator env) (value-of rand env))])))
;

;(define empty-env
; (lambda ()
;  (lambda (y)
;   (error 'value-of "unbound variable ~s" y))))

;(define extend-env
; (lambda (x a env)
;  (lambda (y k)
;   (if (eqv? x y) (k a) (apply-env env y k)))))

;(define apply-env
; (lambda (env y k)
;  (env y k)))

;(define closure
; (lambda (x body env)
;  (lambda (a k)
;   (value-of-cps body (extend-env x a env) k))))

;(define apply-closure
; (lambda (p a k)
;  (p a k)))

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


;;-------------------------------------------------------------------------------------------
;Testcases

;(value-of 
;    '((lambda (x) (if (zero? x) 
;                      12 
;                      47)) 
;       0) 
;    (empty-env))
;12    



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


(value-of-cps fact-5 (empty-env) (empty-k))
;120
(value-of-cps capture-fun (empty-env) (empty-k))
;12



(value-of-cps
    '((lambda (x) (if (zero? x) 
                      12 
                      47)) 
       0) 
    (empty-env) (empty-k))
;12    



