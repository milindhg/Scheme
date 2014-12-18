#lang racket
(require C311/pmatch) ;; <-- important
(require C311/trace)


(define (var-occurs-free? x ls)
              (pmatch-who "vof?" ls
                          (`,y (guard (symbol? y)) (eqv? x y))
                          (`(lambda (,y) ,body) (and (not (eqv? x y)) (var-occurs-free? x body)))
                          (`(,rator ,rand) (or (var-occurs-free? x rator) (var-occurs-free? x rand)) )
                          ))

(define (var-occurs-bound? x ls)
              (pmatch-who "vof?" ls
                          (`,y (guard (symbol? y)) #f)
                          (`(lambda (,y) ,body) (or (var-occurs-bound? x body) (and (eqv? y x) (var-occurs-free? x body))))
                          (`(,rator ,rand) (or (var-occurs-bound? x rator) (var-occurs-bound? x rand)))
                          ))
                          

(var-occurs-bound? 'x 'x)
;;#f
(var-occurs-bound? 'x '(lambda (x) x))
;;#t
(var-occurs-bound? 'y '(lambda (x) x))
;;#f
(var-occurs-bound? 'x '((lambda (x) (x x)) (x x)))
;;#t
(var-occurs-bound? 'z '(lambda (y) (lambda (x) (y z))))
;;#f
(var-occurs-bound? 'z '(lambda (y) (lambda (z) (y z))))
;;#t
(var-occurs-bound? 'x '(lambda (x) y))
;;#f
(var-occurs-bound? 'x '(lambda (x) (lambda (x) x)))
;;#t


(var-occurs-bound? 'f
 '(lambda (d) 
    ((((lambda (g) f) (lambda (e) (lambda (f) (lambda (e) f))))
      (lambda (e) (lambda (f) f)))
     ((lambda (e) d) (lambda (d) (lambda (f) g)))))
)
