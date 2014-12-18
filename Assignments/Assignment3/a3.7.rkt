#lang racket
(require C311/pmatch) ;; <-- important
(require C311/trace)

(define value-of-lex
  (lambda(exp env)
    (pmatch exp
            (`,c (guard (or (boolean? c) (number? c))) c)
            (`(sub1 ,body) (sub1 (value-of-lex body env)))
            (`(zero? ,body) (zero? (value-of-lex body env)))
            (`(* ,n1 ,n2) (* (value-of-lex n1 env) (value-of-lex n2 env)))
            (`(if ,t ,c ,a) (if (value-of-lex t env) (value-of-lex c env) (value-of-lex a env)))
            (`(var ,num) (apply-env-lex env num))
            (`(lambda ,body) (lambda (a) (value-of-lex body (extend-env-lex a env))))
            (`(,rator ,rand) ((value-of-lex rator env) (value-of-lex rand env))))))


(define empty-env-lex
  (lambda () '()))

(define extend-env-lex
  (lambda (a env)
    (list 'extend-env-lex a env)
          ))

(define apply-env-lex
  (lambda (env num)
    (pmatch env
            (`(empty-env-lex) (error 'error))
            (`(extend-env-lex ,a ,env) (cond ((zero? num) a) (else (apply-env-lex env (sub1 num)))))
            )))


(value-of-lex '((lambda (var 0)) 5) (empty-env-lex))
;;5

(value-of-lex '((lambda ((lambda (var 1)) 4)) 5) (empty-env-lex))
;;Answer should be 5 here also :(

;;(lambda (x) (lambda (x) 4)5)


;;(value-of-lex '((lambda (var 0) ) (lambda (lambda (var 1) ) 4) 5) (empty-env-lex))

