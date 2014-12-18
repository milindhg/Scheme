#lang racket
(require C311/pmatch) ;; <-- important
(require C311/trace)


(define walk-symbol-update
  (lambda (x ls)
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


(define a-list `((c . ,(box 15)) (e . ,(box 'f)) (b . ,(box 'c)) (a . ,(box 'b))))
a-list
(walk-symbol-update 'a a-list)
a-list

(define b-list `((c . ,(box 'e)) (e . ,(box 'f)) (b . ,(box 'c)) (a . ,(box 'b))))
b-list
(walk-symbol-update 'a b-list)
b-list

