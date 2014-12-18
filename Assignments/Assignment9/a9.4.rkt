#lang racket
(require "parse.rkt")
(require "parenthec.rkt")
(require C311/trace)

;(parse '((lambda (a b c) (* a c)) 5 6 7))

(define-union exp
  (const n)
  (var v)
  (if test conseq alt)
  (mult rand1 rand2)
  (sub1 rand)
  (zero rand)
  (capture body)
  (return vexp kexp)
  (let vexp body)
  (lambda body)
  (app rator rand))
 
(define value-of-cps
  (lambda (expr env k)
    (union-case expr exp
      [(const n) (apply-continuation k n)]
      [(var v) (apply-env env v k)]
      [(if test conseq alt)
       (value-of-cps test env (cont_inner_if conseq alt env k))]                               
      [(mult rand1 rand2) (value-of-cps rand1 env (cont_outer_mult rand2 env k))]
      [(sub1 rand) (value-of-cps rand env (cont_inner_sub1 k))]
      [(zero rand) (value-of-cps rand env (cont_inner_zero k))]
      [(capture body)
	  (value-of-cps body (envr_extend k env) k)]
      [(return vexp kexp)
       (value-of-cps kexp env (cont_inner_return vexp env))]
      [(let vexp body)
       (value-of-cps vexp env (cont_inner_let body env k))]
      [(lambda body) (apply-continuation k (clos_closure body env))]
      [(app rator rand)
        (value-of-cps rator env (cont_outer_rator_rand rand env k))])))
 
(define-union envr
  (empty)
  (extend arg env))
 
(define apply-env
  (lambda (env num k)
    (union-case env envr
      [(empty) (apply-continuation k (error 'env "unbound variable"))]
      [(extend arg env)
       (if (zero? num)
	   (apply-continuation k arg)
	   (apply-env env (sub1 num) k))])))
 
(define-union clos
  (closure code env))
 
(define apply-closure
  (lambda (c a k)
    (union-case c clos
      [(closure code env)
       (value-of-cps code (envr_extend a env) k)])))
 
(define-union cont
  (empty)
  (inner_if conseq alt env k)
  (inner_mult x k)
  (outer_mult rand2 env k)
  (inner_sub1 k)
  (inner_zero k)
  (inner_return vexp env)
  (inner_let body env k)
  (inner_rator_rand y k)
  (outer_rator_rand rand env k))

(define apply-continuation
  (lambda (k^ v)
    (union-case k^ cont
      [(empty) v]
      [(inner_if conseq alt env k)
        (if v 
	(value-of-cps conseq env k)
	 (value-of-cps alt env k))]
      [(inner_mult x k) (apply-continuation k (* x v))]
      [(outer_mult rand2 env k) (value-of-cps rand2 env (cont_inner_mult v k))]
      [(inner_sub1 k) (apply-continuation k (- v 1))]
      [(inner_zero k) (apply-continuation k (zero? v))]
      [(inner_return vexp env) (value-of-cps vexp env v)]
      [(inner_let body env k) (let ((v^ v))
         (value-of-cps body (envr_extend v^ env) k))]
      [(inner_rator_rand y k) (apply-closure y v k)]
      [(outer_rator_rand rand env k) (value-of-cps rand env (cont_inner_rator_rand v k))])))
;;---------------------------------------------------------------------------------------------------------------------------------
;;Testcases
; Basic test...should be 5.
(pretty-print
 (value-of-cps (exp_app
            (exp_app
             (exp_lambda (exp_lambda (exp_var 1)))
             (exp_const 5))
            (exp_const 6))
           (envr_empty) (cont_empty)))
 

; Factorial of 5...should be 120.
(pretty-print
 (value-of-cps (exp_app
	    (exp_lambda
	     (exp_app
	      (exp_app (exp_var 0) (exp_var 0))
	      (exp_const 5)))
	    (exp_lambda
	     (exp_lambda
	      (exp_if (exp_zero (exp_var 0))
		      (exp_const 1)
		      (exp_mult (exp_var 0)
				(exp_app
				 (exp_app (exp_var 1) (exp_var 1))
				 (exp_sub1 (exp_var 0))))))))
	   (envr_empty) (cont_empty)))
	   
 
					
; Test of capture and return...should evaluate to 24.
(pretty-print
 (value-of-cps
  (exp_mult (exp_const 2)
	    (exp_capture
	     (exp_mult (exp_const 5)
		       (exp_return (exp_mult (exp_const 2) (exp_const 6))
                                   (exp_var 0)))))
  (envr_empty) (cont_empty))) 
 
;; (let ([fact (lambda (f)                                                      
;;               (lambda (n)                                                    
;;                 (if (zero? n)                                                
;;                     1                                                        
;;                     (* n ((f f) (sub1 n))))))])                              
;;   ((fact fact) 5))                                                           
 
(pretty-print
 (value-of-cps (exp_let
	    (exp_lambda
	     (exp_lambda
	      (exp_if
	       (exp_zero (exp_var 0))
	       (exp_const 1)
	       (exp_mult
		(exp_var 0)
		(exp_app
		 (exp_app (exp_var 1) (exp_var 1))
		 (exp_sub1 (exp_var 0)))))))
	    (exp_app (exp_app (exp_var 0) (exp_var 0)) (exp_const 5)))
	   (envr_empty) (cont_empty)))
;120