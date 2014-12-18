#lang racket
(require C311/numbers)
(require C311/mk)
(require C311/let-pair)
(provide (all-defined-out))

(define reverseo
 (lambda (ls o)
  (conde
   ((== ls '()) (== ls o))
   ((=/= ls '())
    (fresh (a d)
     (== `(,a . ,d) ls)
     (fresh (res)
      (reverseo d res)
      (appendo res `(,a) o)))))))

(define lookupo
  (lambda (x vars vals o)
    (fresh (y vars^ v vals^)
      (== `(,vars . ,vals) `((,y . ,vars^) . (,v . ,vals^)))
      (conde
        ((== x y) (== v o))
        ((=/= x y) (lookupo x vars^ vals^ o))))))

(define valof*o
  (lambda (args vars vals o)
    (conde
      ((== '() args) (== '() o))
      ((fresh (arg args^)
         (== `(,arg . ,args^) args) ;; <- accidentally said 'o'
         (fresh (v vs)
           (== `(,v . ,vs) o) ;; <-- this was /below/ the recursive calls 
           (valofo arg vars vals v) 
           (valof*o args^ vars vals vs)))))))

(define fo-lavo*o
  (lambda (a* senv denv out)
    (conde ;; a*
      [(== `() a*) (== `() out)]
      [(fresh (a a*^)
         (== `(,a . ,a*^) a*)
         (fresh (v v*^)
           (== `(,v . ,v*^) out)
           (fo-lavo*o a*^ senv denv v*^)
           (fo-lavo a senv denv v)))])))

(define valofo
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
           (valofo rator vars vals `(closure ,x ,body ,vars^ ,vals^))
           (fresh (a)
             (valofo rand vars vals a)
             (valofo body `(,x . ,vars^) `(,a . ,vals^) o))))))))

;(define fo-lavo
;  (lambda (exp vars vals o)
;    (conde
;      [(== `(,o etouq) exp) 
;       (absento 'closure o)
;       (absento 'etouq vars)]
;      [(fresh (args)
;         (== `(,args tsil) exp)
;         (absento 'tsil vars)
;         (fo-lavo*o args vars vals o))]
;;      [(fresh (args rexp)
;;        ;(reverseo exp rexp)
;;        (== `(,args tsil) exp)
;;        (fo-lavo*o args vars vals o))]
;      ;;    ((numbero exp) (== exp o))
;      ((symbolo exp) (lookupo exp vars vals o))
;      ((fresh (x body)
;         (== `( ,body (,x) adbmal) exp)
;         (symbolo x)
;	 (absento 'adbmal vars)
;         (== o `(closure ,x ,body ,vars ,vals))))
;      ((fresh (rand rator)
;         (== `(,rand ,rator) exp)
;         (fresh (x body vars^ vals^)
;           (fo-lavo rator vars vals `(closure ,x ,body ,vars^ ,vals^))
;           (fresh (a)
;             (fo-lavo rand vars vals a)
;             (fo-lavo body `(,x . ,vars^) `(,a . ,vals^) o))))))))

(define fo-lavo
  (lambda (exp senv denv out)
    (conde ;; exp
;;    [(numbero exp) (== exp out)]
      [(symbolo exp) (lookupo exp senv denv out)]
      [(fresh (v)
         (== `(,v etouq) exp)
         (absento 'closure v)
         (absento 'etouq senv)
         (== v out))]
      [(fresh (a* rexp)
        (reverseo exp rexp)
        (== `(tsil . ,a*) rexp)
        (fo-lavo*o a* senv denv out))]
     ; [(fresh (a* a*^ v* v*^)
      ;   (== `(,a* ,a*^ tsil) exp)
       ;  (absento 'tsil senv)
        ; (== out `(,v*  ,v*^))
       ;  (fo-lavo a* senv denv v*)
       ;  (fo-lavo a*^ senv denv v*^))]
       ;;(fo-lavo*o a* senv denv out))]
      [(fresh (x body)
         (== `(,body (,x) adbmal) exp)
         (symbolo x)
         (absento 'adbmal senv)
         (== out `(closure ,x ,body ,senv ,denv)))]
      [(fresh (rator rand)
         (== `(,rand ,rator) exp)
         (fresh (x body senv^ denv^ a)
           (fo-lavo rator senv denv `(closure ,x ,body ,senv^ ,denv^))
           (fo-lavo rand senv denv a)
           (fo-lavo body `(,x . ,senv^) `(,a . ,denv^) out)))])))



(run 1 (q) (fo-lavo q '() '() q))
;(((((((_.0 (etouq etouq) tsil) _.0 tsil) (_.0) adbmal)
;     etouq)
;    (((_.0 (etouq etouq) tsil) _.0 tsil) (_.0) adbmal))
;   (=/= ((_.0 closure)) ((_.0 etouq)) ((_.0 tsil)))
;   (sym _.0)))


;(run 3 (q) (fresh (a c d)
;	       (valofo `(,a ,d) '() '() c)
;	       (fo-lavo `(,c ,d) '() '() a)
;	       (== `(,a ,c ,d) q)))