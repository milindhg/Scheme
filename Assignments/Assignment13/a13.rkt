#lang racket
(require C311/numbers)
(require C311/mk)
(require C311/let-pair)
(provide (all-defined-out))
;;(require "a13-student-tests.rkt")
;;(test-file #:file-name "a13.rkt")

;;----------------------------------------------------------------------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------------------------------------------------------------------
;;
;;1. listo

(define listo
  (lambda (ls)
    (conde
     ((== ls '()))
     ((=/= ls '())
      (fresh (a d)
             (== `(,a . ,d) ls) (listo d))))))



;;----------------------------------------------------------------------------------------------------------------------------------------------
;;
;;2. facto

(define facto
  (lambda (n o)
    (conde
     [(== n '()) (== o (build-num 1))]
     [(fresh (sub1num res)
            (minuso n (build-num 1) sub1num)
            (facto sub1num res)
            (*o res n o))])))

;;----------------------------------------------------------------------------------------------------------------------------------------------
;;
;;3. fibso

(define fibs
    (lambda (n)
      (cond
        ((eqv? n 0) (values 1 1))
        (else
         (let ((n- (- n 1)))
           (let-values (((u v) (fibs n-)))
             (let ((u+v (+ u v)))
               (values v u+v))))))))

(define fibso
  (lambda (num o1 o2)
    (conde
     [(== num '()) (== o1 (build-num 1)) (== o2 (build-num 1))]
     [(fresh (ad sub1num res1 res2)
             (minuso num (build-num 1) sub1num)
             (fibso sub1num res1 res2)
             (pluso res1 res2 ad)
             (== res2 o1)
             (== ad o2)
             )])))


;;----------------------------------------------------------------------------------------------------------------------------------------------
;;
;;4. fo-lavo

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
           (val-ofo arg vars vals v) 
           (valof*o args^ vars vals vs)))))))

(define fo-lavo*o
  (lambda (args vars vals o)
    (conde
      ((== '() args) (== '() o))
      ((fresh (arg args^)
         (== `(,arg . ,args^) args)
         (fresh (v vs)
           (== `(,v . ,vs) o)
           (fo-lavo arg vars vals v) 
           (fo-lavo*o args^ vars vals vs)))))))

(define val-ofo
  (lambda (exp vars vals o)
    (conde
      [(== `(quote ,o) exp) (absento 'closure o)]
      [(fresh (args)
         (== `(list . ,args) exp)
         (valof*o args vars vals o))]
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
       (absento 'etouq vars)]
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

;;----------------------------------------------------------------------------------------------------------------------------------------------
;;
;;5. Color Middle Earth

(define middle-earth
    '((lindon eriador forodwaith)
      (forodwaith lindon rhovanion eriador)
      (eriador lindon forodwaith rhovanion enedwaith)
      (rhovanion forodwaith eriador enedwaith rohan rhun)
      (enedwaith eriador rhovanion rohan gondor)
      (rohan enedwaith rhovanion rhun gondor mordor)
      (gondor enedwaith rohan mordor)
      (rhun rohan rhovanion khand mordor)
      (mordor gondor rohan rhun khand harad)
      (khand mordor rhun harad)
      (harad mordor khand)))

(define caro
  (lambda (p a)
    (fresh (d)
      (== (cons a d) p))))

(define cdro
  (lambda (p d)
    (fresh (a)
      (== (cons a d) p))))

(define membero
  (lambda (x l)
    (conde
      ((== l '()))
      ((caro l x))
        ((fresh (d)
          (cdro l d)
          (membero x d))))))

(define color-middle-earth
  (lambda (lst)
    (run 1 (q) (four-CE lst q))))

(define four-CE
  (lambda (ls out)
    (fresh (c1 c2 c3 c4 c5 c6 c7 c8 c9 c0 c10)
       (== out `((lindon . ,c0) (forodwaith . ,c1)
                  (eriador . ,c2) (rhovanion . ,c3) (enedwaith . ,c4)
                  (rohan . ,c5) (gondor . ,c6) (rhun . ,c7)
                  (mordor . ,c8) ( khand . ,c9) (harad . ,c10)))
        (=/= c0 c2) 
        (=/= c0 c1)
        (=/= c1 c3)
        (=/= c1 c2)
        (=/= c2 c3)
        (=/= c2 c4)
        (=/= c3 c4)
        (=/= c3 c5)
        (=/= c3 c7)
        (=/= c4 c5)
        (=/= c4 c6)
        (=/= c5 c7) 
        (=/= c5 c6) 
        (=/= c5 c8)
        (=/= c6 c8)
        (=/= c7 c9) 
        (=/= c7 c8)
        (=/= c8 c9)
        (=/= c8 c10)
        (=/= c9 c10)
        (membero c0 ls)
        (membero c1 ls)
        (membero c2 ls)
        (membero c3 ls)
        (membero c4 ls)
        (membero c5 ls)
        (membero c6 ls)
        (membero c7 ls)
        (membero c8 ls)
        (membero c9 ls)
        (membero c10 ls))))

;;----------------------------------------------------------------------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------------------------------------------------------------------
