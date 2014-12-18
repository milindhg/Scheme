#lang racket
(require C311/pmatch) ;; <-- important
(require C311/trace)


(define extend 
              (λ (n pred?)
                (λ input
                  (cond
                    ((or (eq? n (car input)) (pred? (car input))))
                    (else #f)
                    ))))

                 
                    
                      
                      
((extend 1 even?) 0)
;;#t
((extend 1 even?) 1)
;;#t
((extend 1 even?) 2)
;;#t
((extend 1 even?) 3)
;;#f
(filter (extend 1 even?) '(0 1 2 3 4 5))
;;(0 1 2 4)
(filter (extend 3 (extend 1 even?)) '(0 1 2 3 4 5))
;;(0 1 2 3 4)
(filter (extend 7 (extend 3 (extend 1 even?))) '(0 1 2 3 4 5))
;;(0 1 2 3 4)