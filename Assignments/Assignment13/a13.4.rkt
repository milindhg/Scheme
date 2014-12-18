#lang racket
(require C311/numbers)
(require C311/mk)
(require C311/let-pair)
(provide (all-defined-out))

(define lookupo
  (lambda (x vars vals o)
    (fresh (y vars^ v vals^)
      (== `(,vars . ,vals) `((,y . ,vars^) . (,v . ,vals^)))
      (conde
        ((== x y) (== v o))
        ((=/= x y) (lookupo x vars^ vals^ o))))))

;(define valof*o
;  (lambda (args vars vals o)
;    (conde
;      ((== '() args) (== '() o))
;      ((fresh (arg args^)
;         (== `(,arg . ,args^) args) ;; <- accidentally said 'o'
;         (fresh (v vs)
;           (== `(,v . ,vs) o) ;; <-- this was /below/ the recursive calls 
;           (val-ofo arg vars vals v) 
;           (valof*o args^ vars vals vs)))))))

(define valof*o
  (lambda (args vars vals o)
    (conde
      ((== '() args) (== '() o))
      ((fresh (a d)
         (== `(,a . ,d) args) ;; <- accidentally said 'o'
         (fresh (v vs)
           (== `(,v . ,vs) o) ;; <-- this was /below/ the recursive calls 
           (val-ofo a vars vals v) 
           (valof*o d vars vals vs)))))))

(define fo-lavo*o
  (lambda (args vars vals o)
    (conde
      ((== '() args) (== '() o))
      ((fresh (arg args^)
         (== `(,arg . ,args^) args) ;; <- accidentally said 'o'
         (fresh (v vs)
           (== `(,v . ,vs) o) ;; <-- this was /below/ the recursive calls 
           (fo-lavo arg vars vals v) 
           (fo-lavo*o args^ vars vals vs)))))))

(define val-ofo
  (lambda (exp vars vals o)
    (conde
      [(== `(quote ,o) exp) (absento 'closure o)]
      [(fresh (args)
         (== `(list . ,args) exp)
         (valof*o args vars vals o))]
;;    ((numbero exp) (== exp o))
      ((symbolo exp) (lookupo exp vars vals o))
      ((fresh (x body)
         (== `(lambda (,x) ,body) exp)
         (symbolo x)
         (== `(closure ,x ,body ,vars ,vals) o)))
      ((fresh (rator rand)
         (== `(,rator ,rand) exp)
         (fresh (x body vars^ vals^)
           (val-ofo rator vars vals `(closure ,x ,body ,vars^ ,vals^))
           (fresh (a)
             (val-ofo rand vars vals a)
             (val-ofo body `(,x . ,vars^) `(,a . ,vals^) o))))))))

(define fo-lavo
  (lambda (exp vars vals o)
    (conde
      ((symbolo exp) (lookupo exp vars vals o))
      [(== `(,o etouq) exp) 
       (absento 'closure o)
       (absento 'etouq vars)
       ]
      [(fresh (car_a cdr_a args)
         (== `(,car_a ,cdr_a) args)
         (== `(,car_a ,cdr_a tsil) exp)
         (absento 'tsil vars)
         (fo-lavo*o args vars vals o))]
      ((fresh (x body)
         (== `( ,body (,x) adbmal) exp)
         (symbolo x)
	 (absento 'adbmal vars)
         (== `(closure ,x ,body ,vars ,vals) o)))
      ((fresh (rand rator)
         (== `(,rand ,rator) exp)
         (fresh (x body vars^ vals^)
           (fo-lavo rator vars vals `(closure ,x ,body ,vars^ ,vals^))
           (fresh (a)
             (fo-lavo rand vars vals a)
             (fo-lavo body `(,x . ,vars^) `(,a . ,vals^) o))))))))



;(run 1 (q) (fo-lavo q '() '() q))
;(((((((_.0 (etouq etouq) tsil) _.0 tsil) (_.0) adbmal)
;     etouq)
;    (((_.0 (etouq etouq) tsil) _.0 tsil) (_.0) adbmal))
;   (=/= ((_.0 closure)) ((_.0 etouq)) ((_.0 tsil)))
;   (sym _.0)))


;(run 3 (q) (fresh (a c d)
;	       (valofo `(,a ,d) '() '() c)
;	       (fo-lavo `(,c ,d) '() '() a)
;	       (== `(,a ,c ,d) q)))


  
;(run 1 (q) (fo-lavo '((x (x) adbmal) (etouq etouq) tsil) '() '() q))



;(run 1 (q) (fo-lavo '(((((((_.0 (etouq etouq)) tsil) _.0) tsil) (_.0) adbmal) etouq) (((((_.0 (etouq etouq)) tsil) _.0) tsil) (_.0) adbmal)) '() '() q))





;(run 1 (q) (fo-lavo '(((((x (etouq etouq) tsil) x tsil) (x) adbmal) etouq) (((x (etouq etouq) tsil) x tsil) (x) adbmal)) '() '() q));


;(run 2 (q) (fresh (a b) (val-ofo q '() '() a) (fo-lavo q '() '() b)))
;'('etouq ((lambda (_.0) adbmal) (sym _.0)))

;(run 3 (q) (fresh (a b) (val-ofo q '() '() a) (fo-lavo q '() '() b)))
;'('etouq
;	   ((lambda (_.0) adbmal) (sym _.0))
;	   ((lambda (adbmal) adbmal) (lambda (lambda) adbmal)))

(run 3 (q) (fresh (a c d)
		      (val-ofo `(,a ,d) '() '() c)
		      (fo-lavo `(,c ,d) '() '() a)
		      (== `(,a ,c ,d) q)))
;'(((quote ('etouq (_.0) adbmal) ('etouq (_.0) adbmal))
;	    (=/= ((_.0 closure)) ((_.0 etouq)))
;	    (sym _.0))
;	   ((quote
;	     (((_.0 etouq) ('etouq (_.1) adbmal)) (_.2) adbmal)
;	     (((_.0 etouq) ('etouq (_.1) adbmal)) (_.2) adbmal))
;	    (=/= ((_.1 closure)) ((_.1 etouq)) ((_.2 adbmal))
;		 ((_.2 closure)) ((_.2 etouq)))
;	    (sym _.1 _.2)
;	    (absento (closure _.0)))
;	   ((quote
;	     ((_.0 etouq) (('etouq (_.1) adbmal) (_.2) adbmal))
;	     ((_.0 etouq) (('etouq (_.1) adbmal) (_.2) adbmal)))
;	    (=/= ((_.1 closure))
;		 ((_.1 etouq))
;		 ((_.2 adbmal))
;		 ((_.2 closure)))
;	    (sym _.1 _.2)
;	    (absento (closure _.0))))