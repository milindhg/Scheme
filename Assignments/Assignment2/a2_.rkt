#lang racket
(require C311/pmatch) ;; <-- important
(require C311/trace)
;;(require "a2-student-tests.rkt")
;;(test-file #:file-name "a2.rkt")


(define list-ref
  (lambda (ls n)
    (letrec
        ((nth-cdr
          (lambda (n)
            (cond
              ((zero? n) ls)
              (else (cdr (nth-cdr (sub1 n))) )
              ))))
      (car (nth-cdr n)))))

(define union
  (λ (ls1 ls2)
    (cond
      ((null? ls2) ls1)                  
      ((eqv? #f (memv (car ls2) ls1)) (cons (car ls2) (union ls1 (cdr ls2))) ) 
      (else (union (cdr ls2) ls1))
      )))

(define extend 
  (λ (n pred?)
    (λ input
      (cond
        ((or (eq? n (car input)) (pred? (car input))))
        (else #f)
        ))))

(define walk-symbol
  (λ (a origls)
    (define helper 
      (λ (a ls)
        (cond
          ((null? ls) a)          
          ((eqv? (car (car ls)) a)  (cond ((number? (cdr (car ls))) (cdr (car ls)))  (else(walk-symbol (cdr (car ls)) origls)) ))         
          (else (helper a (cdr ls)))
          )
        ))
    (helper a origls)))

(define lambda->lumbda
  (λ (e)
    (pmatch e
            (`,x (guard (symbol? x)) x)
            (`(lambda (,x) ,body) `(lumbda (,x) ,(lambda->lumbda body)))
            (`(,rator ,rand) `(,(lambda->lumbda rator) ,(lambda->lumbda rand))) 
            )))

(define vars
  (λ (e)
    (pmatch e
            (`,x (guard (symbol? x)) (list x) )            
            (`(lambda (,x) ,body) (cond ((eqv? x body) (vars x)) (else (vars body) )))            
            (`(,rator ,rand) (append (vars rator) (vars rand)))
            )))

(define unique-vars
  (λ (e)
    (pmatch e
            (`,x (guard (symbol? x)) (list x) )            
            (`(lambda (,x) ,body) (cond ((eqv? x body) (unique-vars x)) (else (unique-vars body) )))            
            (`(,rator ,rand) (union (unique-vars rator) (unique-vars rand)))
            )))


(define (var-occurs-free? x ls)
  (pmatch-who "vof?" ls
              (`,y (guard (symbol? y)) (eqv? x y))
              (`(lambda (,y) ,body) (and (not (eqv? x y)) (var-occurs-free? x body)))
              (`(,rator ,rand) (or (var-occurs-free? x rator) (var-occurs-free? x rand)) )
              ))

(define (var-occurs-bound? x ls)
  (pmatch-who "vof?" ls
              (`,y (guard (symbol? y)) #f)
              (`(lambda (,y) ,body) (or (var-occurs-bound? x body) (and (eqv? y x) (var-occurs-free? x body))))
              (`(,rator ,rand) (or (var-occurs-bound? x rator) (var-occurs-bound? x rand)))
              ))

(define unique-free-vars
  (lambda (ls)
    (pmatch ls
            (`,x (guard (symbol? x)) (list x))
            (`(lambda (,x) ,body) (remv x (unique-free-vars body) ))
            (`(,rator ,rand)  (union (unique-free-vars rator) (unique-free-vars rand) ))
            )))


(define unique-bound-vars 
  (lambda (ls)
    (pmatch ls
            (`,x (guard (symbol? x)) '())
            (`(lambda (,x) ,body) (cond ((eqv? #f (memv x (unique-vars body))) (unique-bound-vars x)) (else (cons x (unique-bound-vars body))) ))
            (`(,rator ,rand) (union (unique-bound-vars rator) (unique-bound-vars rand)) )
            )))


(define lex                    ;;Main function
  (lambda (ls acc)
    (define helper             ;;helper function which will help me get the lexical address of the variable passed in as argument.
      (lambda (x acc)
        (cond 
          ((eqv? x (car acc)) 0)          
          (else (add1 (helper x (cdr acc))))
          )
        ))
    (pmatch ls
            (`,x (guard (symbol? x)) (cond ((not (memv x acc)) `(free-var ,x)) (else `(var ,(helper x acc)))) )
            (`(lambda (,x) ,body) (cons 'lambda (list (lex body (cons x acc)))))
            (`(,rator ,rand) (list (lex rator acc) (lex rand acc)) )
            )))
