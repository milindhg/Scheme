#lang racket
(require C311/pmatch) ;; <-- important
(require C311/trace)

(trace-define walk-symbol
  (λ (a ls)
        (cond
          ((and (not (assv a ls)) (not (assq a ls))) a)  
          ((not (symbol? (car ls))) (walk-symbol (walk-symbol a (cdr ls)) (car ls)))
          (else (walk-symbol a (cdr ls)))
          )
        ))

(walk-symbol 'a '((a . 5)))           ;;5
(walk-symbol 'a '((b . c) (a . b)))   ;;c
(walk-symbol 'a '((a . 5) (b . 6) (c . a)))   ;; 5  !!!!!!!!!!!!!!!!!!!!!!!!
(walk-symbol 'c '((a . 5) (b . (a . c)) (c . a)))  ;;5
(walk-symbol 'b '((a . 5) (b . ((c . a))) (c . a)))  ;; ((c . a))
(walk-symbol 'd '((a . 5) (b . (1 2)) (c . a) (e . c) (d . e)))  ;;5
(walk-symbol 'd '((a . 5) (b . 6) (c . f) (e . c) (d . e)))  ;;f

(walk-symbol 'b '((b . a) (a . c) (c . 5)))  ;; #5
