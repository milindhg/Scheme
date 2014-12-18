#lang racket
(require C311/pmatch) ;; <-- important
(require C311/trace)

(define (var-occurs-free? x ls)
              (pmatch-who "vof?" ls
                          (`,y (guard (symbol? y)) (eqv? x y))
                          (`(lambda (,y) ,body) (and (not (eqv? x y)) (var-occurs-free? x body)))
                          (`(,rator ,rand) (or (var-occurs-free? x rator) (var-occurs-free? x rand)) )
                          ))


(var-occurs-free? 'x 'x)   ;;#t
(var-occurs-free? 'x '(lambda (y) y))   ;;#f
(var-occurs-free? 'x '(lambda (x) (x y)))  ;;#f
(var-occurs-free? 'y '(lambda (x) (x y)))  ;;#t
(var-occurs-free? 'y '((lambda (y) (x y)) (lambda (x) (x y))))  ;;#t
(var-occurs-free? 'x '((lambda (x) (x x)) (x x)))   ;;#t
(var-occurs-free? 'y '(lambda (x) (y (lambda (y) (x z)))))  ;;#f ?
