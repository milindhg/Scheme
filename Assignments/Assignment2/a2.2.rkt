#lang racket
(require C311/pmatch) ;; <-- important
(require C311/trace)


(define union
              (λ (ls1 ls2)
                (cond
                  ((null? ls2) ls1)                  
                  ((eqv? #f (memv (car ls2) ls1)) (cons (car ls2) (union ls1 (cdr ls2))) ) 
                  (else (union (cdr ls2) ls1))
                  )))


;;(union '() '())
(union '(x) '())
(union '(x) '(x))
(union '(x y) '(x y))
(union '(x y z w) '(x y z))
(union '() '(e r t))
(union '(e r t) '(e r t z))
(union '(e r t y u) '(e w t r q w))
(union '(x y z c) '(x y b h c))

(union '(x y z) '(a x b))

(union '(x y) '(x z))