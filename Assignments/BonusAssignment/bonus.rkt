#lang racket
(require C311/let-pair)
(provide (all-defined-out))
(require C311/pmatch)
(require C311/trace)


;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;Part I: SPS
;;1. filter-sps

(define filter-sps
 (lambda (pred ls s)
  (cond
   [(null? ls) '(())]
   [(let-pair ((a . s^) (filter-sps pred (cdr ls) s))
     (cond 
      ((pred (car ls)) (cons (cons (car ls) a) s^))
      (else (cons a (cons (car ls) s^)))))])))

;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;2. filter*-sps

(define filter*
 (lambda (f ls)
  (cond
   [(null? ls) '()]
   [(pair? (car ls)) (cons (filter* f (car ls)) (filter* f (cdr ls)))]
   [(null? (car ls)) '()]
   [(f (car ls)) (cons (car ls) (filter* f (cdr ls)))]
   [else (filter* f (cdr ls))])))
 
(define filter*-sps
 (lambda (pred ls s)
  (cond
   [(null? ls) `(() . ,s)]
   [(pair? (car ls)) 
    (let-pair ([a . b] (filter*-sps pred (car ls) s))
     (let-pair ([c . d] (filter*-sps pred (cdr ls) s))
      `((,a . ,c) . (,b . ,d))))]
   [(let-pair ((e . s^)
     (filter*-sps pred (cdr ls) s))
     (cond 
      ((pred (car ls)) (cons (cons (car ls) e) s^))
      (else (cons e (cons (car ls) s^)))))])))

;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;3. fib-sps

(define fib-sps
 (lambda (n s)
  (cond
   ((assv n s) => (lambda (pr) `(,(cdr pr) . ,s)))
   ((< n 2) `(,n . ((,n . ,n) . ,s)))
   (else
    (let-pair ((v . s^) (fib-sps (sub1 (sub1 n)) s))
     (let-pair ((u . s^^) (fib-sps (sub1 n) s^))
      (let ((v+u (+ v u)))
       `(,v+u . ((,n . ,v+u) . ,s^^)))))))))

;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;Part I: Macros
;;4. and*

(define-syntax and*
 (syntax-rules ()
  ((_) #t) ;; 0
  ((_ b) b) ;; 1
  ((_ b0 b1 b ...) ;; 2*
   (let ((v b0))
    (if (and* b1 b ...)
     (and* b1 b ...)
     #f )))))

;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;5. cons*

(define-syntax cons*
 (syntax-rules()
  ((_) (raise-syntax-error 'cons* "Incorrect argument-count to cons*"))
  ((_ b) b)
  ((_ b0 b1 b2 ...)
   (let ((v b0))
    (cons v (cons* b1 b2 ...))))))

;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;5. macro-list

(define-syntax macro-list
 (syntax-rules()
  ((_) '())
  ((_ b) `(,b))
  ((_ b0 b1 b2 ...)
   (let ((v b0))
    `(,b0 ,b1 ,b2 ...)))))

;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;6. mcond

(define-syntax mcond
  (syntax-rules(else)
    ((_ (t0 c0) (t1 c1) (t2 c2) ...)
     (if t0 c0 (mcond (t1 c1) (t2 c2) ...)))
    ((_ (else a)) a)
    ((t0 any ...)
     (raise-syntax-error
      'mcond 
      "unbound identifier in module"))))

;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;7. macro-map
;Discussed approach with Renuka Deshmukh

(define-syntax macro-map
 (syntax-rules()
  [(_ m) '()]
  [(_ m '()) '()]
  [(_ m '(a0 a1 ...)) (cons (m a0) (macro-map m '(a1 ...)))]))

