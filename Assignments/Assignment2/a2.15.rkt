#lang racket
(require C311/pmatch) ;; <-- important
(require C311/trace)


(define walk-symbol-update
  (? (x ls)
    (cond
      ((assv x ls) 
       (cond
         ((number? (unbox (cdr (assv x ls))))  (unbox (cdr (assv x ls))))
         (else (cond 
            ((void? (set-box! (cdr (assv x ls)) (walk-symbol-update (unbox (cdr (assv x ls))) ls) )) (unbox (cdr (assv x ls))) )
            (else (unbox (cdr (assv x ls))))
            ))))                  
       (else x)         
       )))


