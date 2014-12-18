#lang racket
(require C311/pmatch)
(require C311/trace)

(trace-define value-of-ri
  (lambda (empty-env extend-env apply-env closure apply-closure)
    (lambda (exp)
      (letrec ([lambda-exp exp]
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
                                      ;(`(lambda (,x) ,body) (closure x body env extend-env))
                                      (`(lambda (,x) ,body) (lambda (a) (interpreter body (extend-env x a env))))
                                      ;(`(,rator ,rand) (apply-closure (interpreter rator env) (interpreter rand env)))
                                      (`(,rator ,rand) ((interpreter rator env) (interpreter rand env)))
                              ))]
               [env empty-env]
               )
        (interpreter lambda-exp env)))))


(trace-define empty-env-fn
  (lambda ()
    (lambda (y)
      (error 'value-of "unbound variable ~s" y))))

(trace-define extend-env-fn
  (lambda (x a env)
    (lambda (y)
      (if (eqv? x y) a (apply-env-fn env y)))))

(trace-define apply-env-fn
  (lambda (env y)
    (env y)))


(trace-define empty-env-ds
  (lambda ()
    (list 'empty-env)))

(trace-define extend-env-ds
  (lambda (x a env)
    (list 'extend-env-ds x a env)))

(trace-define apply-env-ds
  (lambda (env y)
    (pmatch env
            (`(empty-env-ds) (error 'error))
            (`(extend-env-ds ,x ,a ,env) (if (eqv? y x) a (apply-env-ds env y))))))


(trace-define closure-ds-ri
  (lambda (x body env)
    (list 'closure-ds-ri x body env)))

(trace-define apply-closure-ds-ri
  (lambda (p a)
    ;(pmatch p
    ;(`(closure-ds-ri ,x ,body, env) (value-of-scopes body (extend-env x a env)))
    'apply-closure-ds-ri))


(trace-define closure-fn-ri
  (lambda (x body env extend-env)
    (lambda (a)
      '(interpreter extend-env x a env))))

(trace-define apply-closure-fn-ri
  (lambda (p a)
    (p a)))


((value-of-ri empty-env-fn extend-env-fn apply-env-fn closure-fn-ri apply-closure-fn-ri) '((lambda (x) x) 5))
;5

((value-of-ri empty-env-ds extend-env-ds apply-env-ds closure-ds-ri apply-closure-ds-ri) '((lambda (x) x) 5))
;5

((value-of-ri empty-env-fn extend-env-fn apply-env-fn closure-ds-ri apply-closure-ds-ri) '((lambda (x) x) 5))
;5

((value-of-ri empty-env-ds extend-env-ds apply-env-ds closure-fn-ri apply-closure-fn-ri) '((lambda (x) x) 5))
;5
