#lang racket
(require C311/pmatch) ;; <-- important
(require C311/trace)

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

;;(walk-symbol 'a '((a . 5)))           ;;5
(walk-symbol 'a '((b . c) (a . b)))   ;;c
(walk-symbol 'a '((a . 5) (b . 6) (c . a)))   ;; 5  !!!!!!!!!!!!!!!!!!!!!!!!
(walk-symbol 'c '((a . 5) (b . (a . c)) (c . a)))  ;;5
(walk-symbol 'b '((a . 5) (b . ((c . a))) (c . a)))  ;; ((c . a))
(walk-symbol 'd '((a . 5) (b . (1 2)) (c . a) (e . c) (d . e)))  ;;5
(walk-symbol 'd '((a . 5) (b . 6) (c . f) (e . c) (d . e)))  ;;f

(walk-symbol 'b '((b . a) (a . c) (c . 5)))  ;; #5
(walk-symbol 'z '((b . a) (a . c) (c . 5)))  ;; #5
