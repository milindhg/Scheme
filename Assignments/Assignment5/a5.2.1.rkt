#lang racket
(require C311/pmatch)
(require C311/trace)


(trace-define val-of-cbv
 (lambda (exp env)
  (pmatch exp
   [`,n (guard (number? n)) n]
   [`,x (guard (symbol? x)) (unbox (apply-env env x))]
   [`,b (guard (boolean? b)) b]
   [`(null? ,x) (null? ((val-of-cbv x env)))]
   [`(quote ()) '() ]
   [`(car ,x) (car (val-of-cbv x env))]
   [`(cdr ,x) (cdr (val-of-cbv x env))]
   [`(car^ ,x) (car ((val-of-cbv x env)))]
   [`(cdr^ ,x) (cdr ((val-of-cbv x env)))]
   [`(cons^ ,x ,y) (cons (val-of-cbv x env) (val-of-cbv y env))]
   [`(cons ,x ,y) (cons (val-of-cbv x env) (val-of-cbv y env))]
   [`(zero? ,n-exp) (zero? (val-of-cbv n-exp env))]
   [`(* ,x ,y) (* (val-of-cbv x env) (val-of-cbv y env))]
   [`(let ([ ,x ,val ]) ,body) (val-of-cbv body (extend-env x (box (val-of-cbv val env)) env))]
   [`(sub1 ,x) (sub1 (val-of-cbv x env))]
   [`(if ,test-exp ,then-exp ,else-exp) (if (val-of-cbv test-exp env) (val-of-cbv then-exp env) (val-of-cbv else-exp env))]
   [`(set! ,x ,rhs) (let ((vrhs (val-of-cbv rhs env))) (set-box! (apply-env env x) vrhs))]
   [`(begin2 ,e1 ,e2) (begin (val-of-cbv e1 env) (val-of-cbv e2 env))]
   [`(random ,n) (random (val-of-cbv n env))]
   [`(lambda (,x) ,body) (closure-cbv x body env)]
   [`(,rat ,x) (guard (symbol? x)) ((val-of-cbv rat env) (box (unbox (apply-env env x))))]
   [`(,rator ,rand) (apply-closure (val-of-cbv rator env) (box (val-of-cbv rand env)))])))




(define empty-env
 (lambda ()
  (list 'empty-env)))

(define extend-env
 (lambda (x a env)
  (list 'extend-env x a env)))

(define apply-env
 (lambda (env y)
  (pmatch env
   (`(empty-env) (error 'error))
   (`(extend-env ,x ,a ,env) (if (eqv? y x) a (apply-env env y))))))

(define closure-cbv
 (lambda (x body env)
  (lambda (a)
   (val-of-cbv body (extend-env x a env)))))

(define apply-closure
 (lambda (p a)
  (p a)))


;;------------------------------------------------------------------------------------------------------------------------------------------------------
;Testcases



;(val-of-cbv
;   '((lambda (a)
;       ((lambda (p)
;          (begin2
;           (p a)
;           a)) (lambda (x) (set! x 4)))) 3)
;   (empty-env))
;;3
;
;
;(val-of-cbv
;   '((lambda (f)
;       ((lambda (g)
;          ((lambda (z) (begin2
;                        (g z)
;                        z))
;           55))
;        (lambda (y) (f y)))) (lambda (x) (set! x 44)))
;   (empty-env))
;;55
;
;
;(val-of-cbv
;   '((lambda (swap)
;       ((lambda (a)
;          ((lambda (b)
;             (begin2
;              ((swap a) b)
;              a)) 44)) 33))
;     (lambda (x)
;       (lambda (y)
;         ((lambda (temp)
;            (begin2
;             (set! x y)
;             (set! y temp))) x))))
;   (empty-env))
;;33
;

> (define cons-test
    '(let ((fix (lambda (f)
                 ((lambda (x) (f (lambda (v) ((x x) v))))
                  (lambda (x) (f (lambda (v) ((x x) v))))))))
        (let ((map (fix (lambda (map)
                          (lambda (f)
                            (lambda (l)
                               (if (null? l)
                                   '()
                                   (cons^ (f (car^ l))
                                          ((map f) (cdr^ l))))))))))
          (let ((take (fix (lambda (take)
                             (lambda (l)
                               (lambda (n)
                                 (if (zero? n)
                                     '()
                                      (cons (car^ l) 
                                            ((take (cdr^ l)) (sub1 n))))))))))
            ((take ((fix (lambda (m)
                           (lambda (i)
                             (cons^ 1 ((map (lambda (x) (add1 x))) (m i)))))) 0)) 5)))))
(val-of-cbv cons-test (empty-env))
;(1 2 3 4 5)