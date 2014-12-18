#lang racket
(require C311/pmatch)
(require C311/trace)


(define val-of-cbr
 (lambda (exp env)
  (pmatch exp
   [`,n (guard (number? n)) n]
   [`,x (guard (symbol? x)) (unbox (apply-env env x))]
   [`,b (guard (boolean? b)) b]
   [`(zero? ,n-exp) (zero? (val-of-cbr n-exp env))]
   [`(* ,x ,y) (* (val-of-cbr x env) (val-of-cbr y env))]
   [`(let ([ ,x ,val ]) ,body) (val-of-cbr body (extend-env x (val-of-cbr val env) env))]
   [`(sub1 ,x) (sub1 (val-of-cbr x env))]
   [`(if ,test-exp ,then-exp ,else-exp) (if (val-of-cbr test-exp env) (val-of-cbr then-exp env) (val-of-cbr else-exp env))]
   [`(set! ,x ,rhs) (let ((vrhs (val-of-cbr rhs env))) (set-box! (apply-env env x) vrhs))]
   [`(begin2 ,e1 ,e2) (begin (val-of-cbr e1 env) (val-of-cbr e2 env))]
   [`(random ,n) (random (val-of-cbr n env))]
   [`(lambda (,x) ,body) (closure-cbr x body env)]
   [`(,rat ,x) (guard (symbol? x)) ((val-of-cbr rat env) (apply-env env x))]
   [`(,rator ,rand) (apply-closure (val-of-cbr rator env) (box (val-of-cbr rand env)))])))

(define val-of-cbv
 (lambda (exp env)
  (pmatch exp
   [`,n (guard (number? n)) n]
   [`,x (guard (symbol? x)) (unbox (apply-env env x))]
   [`,b (guard (boolean? b)) b]
   [`(zero? ,n-exp) (zero? (val-of-cbv n-exp env))]
   [`(* ,x ,y) (* (val-of-cbv x env) (val-of-cbv y env))]
   [`(let ([ ,x ,val ]) ,body) (val-of-cbv body (extend-env x (val-of-cbv val env) env))]
   [`(sub1 ,x) (sub1 (val-of-cbv x env))]
   [`(if ,test-exp ,then-exp ,else-exp) (if (val-of-cbv test-exp env) (val-of-cbv then-exp env) (val-of-cbv else-exp env))]
   [`(set! ,x ,rhs) (let ((vrhs (val-of-cbv rhs env))) (set-box! (apply-env env x) vrhs))]
   [`(begin2 ,e1 ,e2) (begin (val-of-cbv e1 env) (val-of-cbv e2 env))]
   [`(random ,n) (random (val-of-cbv n env))]
   [`(lambda (,x) ,body) (closure-cbv x body env)]
   [`(,rat ,x) (guard (symbol? x)) ((val-of-cbv rat env) (box (unbox (apply-env env x))))]
   [`(,rator ,rand) (apply-closure (val-of-cbv rator env) (box (val-of-cbv rand env)))])))



(define val-of-cbname
 (lambda (exp env)
  (pmatch exp
   [`,n (guard (number? n)) n]
   [`,x (guard (symbol? x)) ((unbox (apply-env env x)))]
   [`,b (guard (boolean? b)) b]
   [`(zero? ,n-exp) (zero? (val-of-cbname n-exp env))]
   [`(* ,x ,y) (* (val-of-cbname x env) (val-of-cbname y env))]
   [`(let ([ ,x ,val ]) ,body) (val-of-cbname body (extend-env x (val-of-cbname val env) env))]
   [`(sub1 ,x) (sub1 (val-of-cbname x env))]
   [`(if ,test-exp ,then-exp ,else-exp) (if (val-of-cbname test-exp env) (val-of-cbname then-exp env) (val-of-cbname else-exp env))]
   [`(set! ,x ,rhs) (let ((vrhs (val-of-cbname rhs env))) (set-box! (apply-env env x) vrhs))]
   [`(begin2 ,e1 ,e2) (begin (val-of-cbname e1 env) (val-of-cbname e2 env))]
   [`(random ,n) (random (val-of-cbname n env))]
   [`(lambda (,x) ,body) (closure-cbname x body env)]
   [`(,rat ,x) (guard (symbol? x)) ((val-of-cbname rat env) (apply-env env x))]
   [`(,rator ,rand) (apply-closure (val-of-cbname rator env) (box (lambda () (val-of-cbname rand env))))])))



(define val-of-cbneed
 (lambda (exp env)
  (pmatch exp
   [`,n (guard (number? n)) n]
   [`,x (guard (symbol? x)) (unbox/need (apply-env env x))]
   [`,b (guard (boolean? b)) b]
   [`(zero? ,n-exp) (zero? (val-of-cbneed n-exp env))]
   [`(* ,x ,y) (* (val-of-cbneed x env) (val-of-cbneed y env))]
   [`(let ([ ,x ,val ]) ,body) (val-of-cbneed body (extend-env x (val-of-cbneed val env) env))]
   [`(sub1 ,x) (sub1 (val-of-cbneed x env))]
   [`(if ,test-exp ,then-exp ,else-exp) (if (val-of-cbneed test-exp env) (val-of-cbneed then-exp env) (val-of-cbneed else-exp env))]
   [`(set! ,x ,rhs) (let ((vrhs (val-of-cbneed rhs env))) (set-box! (apply-env env x) vrhs))]
   [`(begin2 ,e1 ,e2) (begin (val-of-cbneed e1 env) (val-of-cbneed e2 env))]
   [`(random ,n) (random (val-of-cbneed n env))]
   [`(lambda (,x) ,body) (closure-cbneed x body env)]
   [`(,rat ,x) (guard (symbol? x)) ((val-of-cbneed rat env) (apply-env env x))]
   [`(,rator ,rand) (apply-closure (val-of-cbneed rator env) (box (lambda () (val-of-cbneed rand env))))])))

(define unbox/need
  (lambda (x)
    (let ([val ((unbox x))])
      (set-box! x (lambda () val)) val)))


(define empty-env
 (lambda ()
  (lambda (y)
   (error 'val-of "unbound variable ~s" y))))

(define extend-env
 (lambda (x a env)
  (lambda (y)
   (if (eqv? x y) a (apply-env env y)))))

(define apply-env
 (lambda (env y)
  (env y)))

(define closure-cbr
 (lambda (x body env)
  (lambda (a)
   (val-of-cbr body (extend-env x a env)))))

(define closure-cbv
 (lambda (x body env)
  (lambda (a)
   (val-of-cbv body (extend-env x a env)))))

(define closure-cbname
 (lambda (x body env)
  (lambda (a)
   (val-of-cbname body (extend-env x a env)))))

(define closure-cbneed
 (lambda (x body env)
  (lambda (a)
   (val-of-cbneed body (extend-env x a env)))))

(define apply-closure
 (lambda (p a)
  (p a)))


;;------------------------------------------------------------------------------------------------------------------------------------------------------
;Testcases

;; Making sure set! works
(val-of-cbr
   '((lambda (x) (begin2 (set! x #t)
                         (if x 3 5))) #f)
   (empty-env))
;3

;; Returns 4 under CBR...
(val-of-cbr
   '((lambda (a)
       ((lambda (p)
          (begin2
           (p a)
           a)) (lambda (x) (set! x 4)))) 3)
   (empty-env))
;4


;; ...but returns 3 under CBV.
(val-of-cbv
   '((lambda (a)
       ((lambda (p)
          (begin2
           (p a)
           a)) (lambda (x) (set! x 4)))) 3)
   (empty-env))
;3

;; returns 44 under CBR...
;(test "interesting-cbr-2"
  (val-of-cbr
   '((lambda (f)
       ((lambda (g)
          ((lambda (z) (begin2
                        (g z)
                        z))
           55))
        (lambda (y) (f y)))) (lambda (x) (set! x 44)))
   (empty-env))
;  44

;; ...but returns 55 under CBV!  You can change the "begin2" to
;; "begin" and evaluate this in the Racket REPL as evidence that
;; Racket uses CBV.
(val-of-cbv
   '((lambda (f)
       ((lambda (g)
          ((lambda (z) (begin2
                        (g z)
                        z))
           55))
        (lambda (y) (f y)))) (lambda (x) (set! x 44)))
   (empty-env))
;55

;; Returns 44 under CBR...
(val-of-cbr
   '((lambda (swap)
       ((lambda (a)
          ((lambda (b)
             (begin2
              ((swap a) b)
              a)) 44)) 33))
     (lambda (x)
       (lambda (y)
         ((lambda (temp)
            (begin2
             (set! x y)
             (set! y temp))) x))))
   (empty-env))
;44

;; ...but returns 33 under CBV.
(val-of-cbv
   '((lambda (swap)
       ((lambda (a)
          ((lambda (b)
             (begin2
              ((swap a) b)
              a)) 44)) 33))
     (lambda (x)
       (lambda (y)
         ((lambda (temp)
            (begin2
             (set! x y)
             (set! y temp))) x))))
   (empty-env))
;33

(val-of-cbname
   '((lambda (z) 100)
     ((lambda (x) (x x)) (lambda (x) (x x))))
   (empty-env))
;100

(define random-sieve
    '((lambda (n)
        (if (zero? n)
            (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) #t #f) #f) #f) #f) #f) #f)
            (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f #t))))))))
      (random 2)))
;; call-by-name
;;P(false positive) <= .01
(val-of-cbname random-sieve (empty-env))
;#f

;; call-by-need
(val-of-cbneed random-sieve (empty-env))
;#t


;-----------------------------------------------
(val-of-cbneed 
 ((lambda (x) 120)
  ((lambda (x y) (y x y))
   (lambda (y x) (y x y))
   (lambda (x y) (x x y))))
 (empty-env))