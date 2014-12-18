#lang racket
(require C311/let-pair)
(provide (all-defined-out))
(require C311/trace)
(require C311/pmatch)
(require C311/monads)

(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only 
            (error 'empty-k "You can only invoke the empty continuation once")
            (begin (set! once-only #t) v))))))


(define empty-env
  (lambda ()
    (list 'empty-env)))

(define extend-env
  (lambda (x a env)
    (list 'extend-env x a env)))

(define apply-env
  (lambda (env y)
    (pmatch env
            [`(empty-env) (error 'error)]
            [`(extend-env ,x ,a ,env) (if (eqv? y x) (return-cont a) (bind-cont (apply-env env y) (lambda (a) (return-cont a))))])))

(define closure
  (lambda (x body env)
    (list 'closure x body env)))

(define apply-closure
  (lambda (p a)
    (pmatch p
            [`(closure ,x ,body, env) (bind-cont (value-of-cps body (extend-env x a env)) (lambda (a) (return-cont a)))])))




(define value-of-cps
  (lambda (expr env)
    (pmatch expr
            [`,n (guard (or (number? n) (boolean? n))) (return-cont n)]
            [`(+ ,x1 ,x2) (bind-cont (value-of-cps x1 env) (lambda (x1^) (bind-cont (value-of-cps x2 env) (lambda (x2^) (return-cont (+ x1^ x2^))))))]
            [`(* ,x1 ,x2) (bind-cont (value-of-cps x1 env) (lambda (x1^) (bind-cont (value-of-cps x2 env) (lambda (x2^) (return-cont (* x1^ x2^))))))]
            [`(sub1 ,x) (bind-cont (value-of-cps x env) (lambda (x^) (return-cont (sub1 x^))))]
            [`(zero? ,x) (bind-cont (value-of-cps x env) (lambda (x^) (return-cont (zero? x^))))]
            [`(if ,test ,conseq ,alt) (bind-cont 
                                       (value-of-cps test env) 
                                       (lambda (test^) 
                                         (if test^
                                             (bind-cont (value-of-cps conseq env) (lambda (a) (return-cont a)))
                                             (bind-cont (value-of-cps alt env) (lambda (b) (return-cont b))))))]
            [`(capture ,k-id ,body) (bind-cont 
                                     (callcc 
                                      (lambda (k) 
                                        (bind-cont (value-of-cps body (extend-env k-id k env)) (lambda (b) (return-cont b))))) 
                                     (lambda (a) (return-cont a)))]
            [`(return ,v-exp ,k-exp) (bind-cont (value-of-cps k-exp env) (lambda (k-exp^) (bind-cont (value-of-cps v-exp env) k-exp^)))]
            [`,x (guard (symbol? x)) (bind-cont (apply-env env x) (lambda (d) (return-cont d)))]
            [`(lambda (,id) ,body) (return-cont (closure id body env))]
            [`(,rator ,rand) (bind-cont 
                              (value-of-cps rator env) 
                              (lambda (closure^) 
                                (bind-cont 
                                 (value-of-cps rand env) 
                                 (lambda (arg) (bind-cont (apply-closure closure^ arg) (lambda (h) (return-cont h)))))))])))


(define fact-5
  '((lambda (f)
      ((f f) 5))
    (lambda (f)
      (lambda (n)
        (if (zero? n)
            1
            (* n ((f f) (sub1 n))))))))

((value-of-cps fact-5 (empty-env)) (lambda (v) v))
;120

(define capture-fun
  '(* 3 (capture q (* 2 (return 4 q)))))

((value-of-cps capture-fun (empty-env)) (lambda (v) v))
;12
