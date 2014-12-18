#lang racket
(require "parse.rkt")
(require "parenthec.rkt")
(require C311/trace)

(define k 'hukarz)
(define expr 'hukarz)
(define v 'hukarz)
(define c 'hukarz)
(define a 'hukarz)
(define env 'hukarz)
(define num 'hukarz)

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


(define-label value-of-cps
    (union-case expr exp
                [(const n) 
                 (begin
                   (set! v n)
                   (apply-k))]
                [(var v) 
                 (begin
                   (set! num v)
                   (apply-env))]
                [(if test conseq alt)
                 (begin 
                   (set! k (kt_inner_if conseq alt env k))
                   (set! expr test)
                   (value-of-cps))]                               
                [(mult rand1 rand2) 
                 (begin 
                   (set! k (kt_outer_mult rand2 env k))
                   (set! expr rand1)
                   (value-of-cps))]
                [(sub1 rand) 
                 (begin
                   (set! k (kt_inner_sub1 k)) 
                   (set! expr rand)
                   (value-of-cps))]
                [(zero rand) 
                 (begin
                   (set! k (kt_inner_zero k))
                   (set! expr rand)
                   (value-of-cps))]
                [(capture body)
                 (begin
                   (set! expr body)
                   (set! env (envr_extend k env))
                   (value-of-cps))]
                [(return vexp kexp)
                 (begin 
                   (set! k (kt_inner_return vexp env))
                   (set! expr kexp)
                   (value-of-cps))]
                [(let vexp body)
                 (begin
                   (set! k (kt_inner_let body env k))
                   (set! expr vexp)
                   (value-of-cps))]
                [(lambda body) 
                 (begin
                   (set! k k)
                   (set! v (clos_closure body env))
                   (apply-k))]
                [(app rator rand)
                 (begin 
                   (set! k (kt_outer_rator_rand rand env k))
                   (set! expr rator) 
                   (value-of-cps))]))

(define-union envr
  (empty)
  (extend arg env^))

(define-label apply-env
    (union-case env envr
                [(empty) (error 'env "unbound variable")]
                [(extend arg env^)
                 (if (zero? num)
                     (begin 
                       (set! v arg)
                       (apply-k))
                     (begin
                       (set! env env^)
                       (set! num (sub1 num))
                       (apply-env)))]))

(define-union clos
  (closure code env^^))

(define-label apply-closure
    (union-case c clos
                [(closure code env^^)
                 (begin
                   (set! env (envr_extend a env^^))
                   (set! expr code)
                   (value-of-cps))]))

(define-union kt
  (empty)
  (inner_if conseq alt env^ k^)
  (inner_mult x k^)
  (outer_mult rand2 env^ k^)
  (inner_sub1 k^)
  (inner_zero k^)
  (inner_return vexp env^)
  (inner_let body env^ k^)
  (inner_rator_rand y k^)
  (outer_rator_rand rand env^ k^))

(define-label apply-k
    (union-case k kt
                [(empty) v]
                [(inner_if conseq alt env^ k^)
                 (if v 
                     (begin
                       (set! k k^)
                       (set! expr conseq) 
                       (set! env env^)
                       (value-of-cps))
                     (begin
                       (set! k k^)
                       (set! expr alt)
                       (set! env env^)
                       (value-of-cps)))]
                [(inner_mult x k^)
                 (begin
                   (set! k k^)
                   (set! v (* x v))
                   (apply-k))]
                [(outer_mult rand2 env^ k^) 
                 (begin
                   (set! k (kt_inner_mult v k^))
                   (set! expr rand2)
                   (set! env env^)
                   (value-of-cps))]
                [(inner_sub1 k^)
                 (begin
                   (set! k k^)
                   (set! v (- v 1))
                   (apply-k))]
                [(inner_zero k^) 
                 (begin
                   (set! k k^)
                   (set! v (zero? v))
                   (apply-k))]
                [(inner_return vexp env^) 
                 (begin 
                   (set! k v)
                   (set! expr vexp)
                   (set! env env^)
                   (value-of-cps))]
                [(inner_let body env^ k^)
                 (begin
                   (set! k k^)
                   (set! expr body)
                   (set! env (envr_extend v env^))
                   (value-of-cps))]
                [(inner_rator_rand y k^) 
                 (begin
                   (set! k k^)
                   (set! c y)
                   (set! a v)
                   (apply-closure))]
                [(outer_rator_rand rand env^ k^) 
                 (begin 
                   (set! k (kt_inner_rator_rand v k^))
                   (set! expr rand)
                   (set! env env^)
                   (value-of-cps))]))

;;---------------------------------------------------------------------------------------------------------------------------------
;;Testcases
; Basic test...should be 5.
(define-label test1
    (begin
    (set! expr 
          (exp_app
                       (exp_app
                        (exp_lambda (exp_lambda (exp_var 1)))
                        (exp_const 5))
                       (exp_const 6)))
  (set! k (kt_empty))
  (set! env (envr_empty))
  (value-of-cps)))

(define-label test2
    (begin
    (set! expr 
          (exp_app
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
				 (exp_sub1 (exp_var 0)))))))))
  (set! k (kt_empty))
  (set! env (envr_empty))
  (value-of-cps)))

(define-label test3
    (begin
    (set! expr 
          (exp_mult (exp_const 2)
	    (exp_capture
	     (exp_mult (exp_const 5)
		       (exp_return (exp_mult (exp_const 2) (exp_const 6))
                                   (exp_var 0))))))
  (set! k (kt_empty))
  (set! env (envr_empty))
  (value-of-cps)))

(define-label test4
    (begin
    (set! expr 
          (exp_let
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
	    (exp_app (exp_app (exp_var 0) (exp_var 0)) (exp_const 5))))
  (set! k (kt_empty))
  (set! env (envr_empty))
  (value-of-cps)))

(define-label main
    (begin
      (printf "Testcase 1: ~a\n" (test1))
      (printf "Testcase 2: ~a\n" (test2))
      (printf "Testcase 3: ~a\n" (test3))
      (printf "Testcase 4: ~a\n" (test4))))


(main)