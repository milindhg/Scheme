#lang racket
(require "parse.rkt")
(require "parenthec.rkt")
(require C311/trace)

;(parse '((lambda (a b c) (* a c)) 5 6 7))

(define k 'hukarz)
(define expr 'hukarz)
(define v 'hukarz)
(define c 'hukarz)
(define a 'hukarz)
(define env 'hukarz)
(define num 'hukarz)
(define pc 'hukarz)

(define print-regs
  (lambda ()
    (printf "\n")
    (printf "k=~a" k)(printf " expr=~a" expr)(printf " v=~a" v)(printf " c=~a" c)(printf " a=~a" a)(printf " env=~a" env)(printf " num=~a" num)
    (printf "\n")))

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
  (lambda () ;(expr env k)
    (union-case expr exp
                [(const n) 
                 (begin
                   (set! v n)
                   (set! pc apply-continuation))]
                [(var v) 
                 (begin
                   (set! num v)
                   (set! pc apply-env))]
                [(if test conseq alt)
                 (begin 
                   (set! k (cont_inner_if conseq alt env k))
                   (set! expr test)
                   (set! pc value-of-cps))]                               
                [(mult rand1 rand2) 
                 (begin 
                   (set! k (cont_outer_mult rand2 env k))
                   (set! expr rand1)
                   (set! pc value-of-cps))]
                [(sub1 rand) 
                 (begin
                   (set! k (cont_inner_sub1 k)) 
                   (set! expr rand)
                   (set! pc value-of-cps))]
                [(zero rand) 
                 (begin
                   (set! k (cont_inner_zero k))
                   (set! expr rand)
                   (set! pc value-of-cps))]
                [(capture body)
                 (begin
                   (set! expr body)
                   (set! env (envr_extend k env))
                   (set! pc value-of-cps))]
                [(return vexp kexp)
                 (begin 
                   (set! k (cont_inner_return vexp env))
                   (set! expr kexp)
                   (set! pc value-of-cps))]
                [(let vexp body)
                 (begin
                   (set! k (cont_inner_let body env k))
                   (set! expr vexp)
                   (set! pc value-of-cps))]
                [(lambda body) 
                 (begin
                   (set! k k)
                   (set! v (clos_closure body env))
                   (set! pc apply-continuation))]
                [(app rator rand)
                 (begin 
                   (set! k (cont_outer_rator_rand rand env k))
                   (set! expr rator) 
                   (set! pc value-of-cps))])))

(define-union envr
  (empty)
  (extend arg env^))

(define apply-env
  (lambda () ;(env num k)
    
    (union-case env envr
                [(empty) 
                 (begin 
                   (set! k k)
                   (set! v (error 'env "unbound variable"))
                   (set! pc apply-continuation))]
                [(extend arg env^)
                 (if (zero? num)
                     (begin 
                       ;(set! k k)
                       (set! v arg)
                       (set! pc apply-continuation))
                     (begin
                       (set! env env^)
                       (set! num (sub1 num))
                       (set! pc apply-env)))])))

(define-union clos
  (closure code env^^))

(define apply-closure
  (lambda () ;(c a k)
    
    (union-case c clos
                [(closure code env^^)
                 (begin
                   (set! env (envr_extend a env^^))
                   (set! expr code)
                   (set! pc value-of-cps))])))

(define-union cont
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

(define apply-continuation
  (lambda () ;(k v)
    
    (union-case k cont
                [(empty) v]
                [(inner_if conseq alt env^ k^)
                 (if v 
                     (begin
                       (set! k k^)
                       (set! expr conseq) 
                       (set! env env^)
                       (set! pc value-of-cps))
                     (begin
                       (set! k k^)
                       (set! expr alt)
                       (set! env env^)
                       (set! pc value-of-cps)))]
                [(inner_mult x k^)
                 (begin
                   (set! k k^)
                   (set! v (* x v))
                   (set! pc apply-continuation))]
                [(outer_mult rand2 env^ k^) 
                 (begin
                   (set! k (cont_inner_mult v k^))
                   (set! expr rand2)
                   (set! env env^)
                   (set! pc value-of-cps))]
                [(inner_sub1 k^)
                 (begin
                   (set! k k^)
                   (set! v (- v 1))
                   (set! pc apply-continuation))]
                [(inner_zero k^) 
                 (begin
                   (set! k k^)
                   (set! v (zero? v))
                   (set! pc apply-continuation))]
                [(inner_return vexp env^) 
                 (begin 
                   (set! k v)
                   (set! expr vexp)
                   (set! env env^)
                   (set! pc value-of-cps))]
                [(inner_let body env^ k^)
                 (begin
                   (set! k k^)
                   (set! expr body)
                   (set! env (envr_extend v env^))
                   (set! pc value-of-cps))]
                [(inner_rator_rand y k^) 
                 (begin
                   (set! k k^)
                   (set! c y)
                   (set! a v)
                   (set! pc apply-closure))]
                [(outer_rator_rand rand env^ k^) 
                 (begin 
                   (set! k (cont_inner_rator_rand v k^))
                   (set! expr rand)
                   (set! env env^)
                   (set! pc value-of-cps))])))

(define value-of-driver
  (lambda (exp)
    (begin
      (set! expr exp)
      (set! k (cont_empty))
      (set! env (envr_empty))
      
      (value-of-cps))))

(define main
  (begin
    (pretty-print
     (value-of-driver (exp_app
                       (exp_app
                        (exp_lambda (exp_lambda (exp_var 1)))
                        (exp_const 5))
                       (exp_const 6))
                      ))
    (pretty-print
     (value-of-driver (exp_app
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
                      ))
    (pretty-print
     (value-of-driver
      (exp_mult (exp_const 2)
                (exp_capture
                 (exp_mult (exp_const 5)
                           (exp_return (exp_mult (exp_const 2) (exp_const 6))
                                       (exp_var 0)))))
      ))
    (pretty-print
     (value-of-driver (exp_let
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
                      ))))


(main)
