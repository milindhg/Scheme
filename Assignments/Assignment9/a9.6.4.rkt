#lang racket
(require "parse.rkt")
(require "parenthec.rkt")
(require C311/trace)

;(parse '((lambda (a b c) (* a c)) 5 6 7))

(define expr 'hukarz)
(define env 'hukarz)
(define k 'hukarz)
(define v 'hukarz)
(define c 'hukarz)
(define a 'hukarz)
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
 

(trace-define value-of-cps
  (lambda () ;(expr env k)
    (printf "\n")(print expr)(print env)(print k)(print v)(print c)(print a)(print num)(printf "\n")
    (union-case expr exp
      [(const n) 
       (begin
         (set! v n)
         (apply-continuation))]
      [(var v) 
       (begin
         (apply-env))]
      [(if test conseq alt)
       (begin 
         (set! expr test)
         (set! k (cont_inner_if conseq alt env k))
         (value-of-cps))]                               
      [(mult rand1 rand2) 
       (begin 
         (set! expr rand1)
         (set! k (cont_outer_mult rand2 env k))
         (value-of-cps))]
      [(sub1 rand) 
       (begin
         (set! expr rand)
         (set! k (cont_inner_sub1 k)) 
         (value-of-cps))]
      [(zero rand) 
       (begin
         (set! expr rand)
         (set! k (cont_inner_zero k))
         (value-of-cps))]
      [(capture body)
       (begin 
         (set! expr body)
         (set! env (envr_extend k env))
         (value-of-cps))]
      [(return vexp kexp)
       (begin 
         (set! expr kexp) 
         (set! k (cont_inner_return vexp env))
         (value-of-cps))]
      [(let vexp body)
       (begin
         (set! expr vexp) 
         (set! k (cont_inner_let body env k))
         (value-of-cps))]
      [(lambda body) 
       (begin
         (set! k k)
         (set! v (clos_closure body env))
         (apply-continuation))]
      [(app rator rand)
       (begin 
         (set! expr rator)
         (set! k (cont_outer_rator_rand rand env k)) 
         (value-of-cps))])))
 
(define-union envr
  (empty)
  (extend arg env))
 
(define apply-env
  (lambda () ;(env num k)
    (printf "\n")(print expr)(print env)(print k)(print v)(print c)(print a)(print num)(printf "\n")
    (union-case env envr
      [(empty) 
       (begin 
         (set! k k)
         (set! v (error 'env "unbound variable"))
         (apply-continuation))]
      [(extend arg env)
       (if (zero? num)
	   (begin 
             (set! k k)
             (set! v arg)
             (apply-continuation))
	   (begin
             (set! num (sub1 num))
             (apply-env)))])))
 
(define-union clos
  (closure code env))
 
(define apply-closure
  (lambda () ;(c a k)
    (printf "\n")(print expr)(print env)(print k)(print v)(print c)(print a)(print num)(printf "\n")
    (union-case c clos
      [(closure code env)
       (begin
         (set! expr code)
         (set! env (envr_extend a env))
         (value-of-cps))])))
 
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
  (lambda () ;(k v)
    (printf "\n")(print expr)(print env)(print k)(print v)(print c)(print a)(print num)(printf "\n")
    (union-case k cont
      [(empty) v]
      [(inner_if conseq alt env k)
        (if v 
	(begin
          (set! expr conseq) 
          (value-of-cps))
	 (begin
           (set! expr alt)
           (value-of-cps)))]
      [(inner_mult x k) 
       (begin 
         (set! v (* x v)) 
         (apply-continuation))]
      [(outer_mult rand2 env k) 
       (begin
         (set! expr rand2)
         (set! k (cont_inner_mult v k))
         (value-of-cps))]
      [(inner_sub1 k) 
       (begin 
         (set! v (- v 1))
         (apply-continuation))]
      [(inner_zero k) 
       (begin
         (set! v (zero? v))
         (apply-continuation))]
      [(inner_return vexp env) 
       (begin 
        (set! expr vexp)
        (set! k v)
        (value-of-cps))]
      [(inner_let body env k)
       (begin
         (set! expr body)
         (set! env (envr_extend v env))
         (value-of-cps))]
      [(inner_rator_rand y k) 
       (begin
         (set! c y)
         (set! a v)
         (apply-closure))]
      [(outer_rator_rand rand env k) 
       (begin 
         (set! expr rand)
         (set! k (cont_inner_rator_rand v k))
         (value-of-cps))])))

(trace-define value-of-driver
  (lambda (exp)
    (printf "\n")(print expr)(print env)(print k)(print v)(print c)(print a)(print num)(printf "\n")
    (begin
      (set! expr exp)
      (set! env (envr_empty))
      (set! k (cont_empty))
      (printf "\n")(print expr)(print env)(print k)(print v)(print c)(print a)(print num)(printf "\n")
      (value-of-cps))))
;;---------------------------------------------------------------------------------------------------------------------------------
;;Testcases
; Basic test...should be 5.
(pretty-print
 (value-of-driver (exp_app
            (exp_app
             (exp_lambda (exp_lambda (exp_var 1)))
             (exp_const 5))
            (exp_const 6))
           ))
