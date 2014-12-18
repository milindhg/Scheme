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
            (`(,rator ,rand) (union (unique-vars rand) (unique-vars rator)))
            )))


(define unique-free-vars
              (lambda (ls)
                (pmatch ls
                        (`,x (guard (symbol? x)) (list x))
                        (`(lambda (,x) ,body) (remv x (unique-free-vars body) ))
                        (`(,rator ,rand)  (union (unique-free-vars rator) (unique-free-vars rand) ))
                        )))



(unique-free-vars 'x)
;;(x)
(unique-free-vars '(lambda (x) (x y)))
;;(y)
(unique-free-vars '((lambda (x) ((x y) e)) (lambda (c) (x (lambda (x) (x (e c)))))))
;;(y e x)
(unique-free-vars '(lambda (z) ((lambda (y) (a z)) (h (lambda (x) (h a))))))
;;(h a)
(unique-free-vars '((lambda (z) (lambda (y) (z y))) x))
;;(x)


(unique-free-vars 'x)
;;(x)
(unique-free-vars '(lambda (x) (x y)))
;;(y)
(unique-free-vars '((lambda (x) ((x y) e)) (lambda (c) (x (lambda (x) (x (e c)))))))
;;(y e x)
(unique-free-vars '(lambda (x) y))
;;(y)
(unique-free-vars '(lambda (x) (y z)))
;;(y z)


(unique-free-vars 
 '(lambda (d) 
    ((((lambda (g) f) (lambda (e) (lambda (f) (lambda (e) f))))
      (lambda (e) (lambda (f) f)))
     ((lambda (e) d) (lambda (d) (lambda (f) g)))))
)
