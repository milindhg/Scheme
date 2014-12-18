#lang racket
(require C311/pmatch) ;; <-- important
(require C311/trace)

(define union
              (λ (ls1 ls2)
                (cond
                  ((null? ls2) ls1)
                  ((eqv? #f (memv (car ls2) ls1)) (cons (car ls2) (union ls1 (cdr ls2))) ) 
                  (else (union ls1 (cdr ls2)))
                  )))


(define unique-vars
  (λ (e)
    (pmatch e
            (`,x (guard (symbol? x)) (list x) )            
            (`(lambda (,x) ,body) (cond ((eqv? x body) (unique-vars x)) (else (unique-vars body) )))            
            (`(,rator ,rand) (union (unique-vars rator) (unique-vars rand)))
            )))


(unique-vars 'x)
;;(x)
(unique-vars '(lambda (x) x))
;;(x)
(unique-vars '((lambda (y) (x x)) (x y)))
;;(x x x y)
(unique-vars '(lambda (z) ((lambda (y) (a z)) (h (lambda (x) (h a))))))
;;(a z h h a)

(unique-vars '((lambda (y) (x x)) (x y)))
;;(x y)
(unique-vars '((lambda (z) (lambda (y) (z y))) x))
;;(z y x)
(unique-vars '((lambda (a) (a b)) ((lambda (c) (a c)) (b a))))
;;(c b a)


