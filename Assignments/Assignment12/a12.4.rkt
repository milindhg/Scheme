#lang racket
(require C311/let-pair)
(provide (all-defined-out))
(require C311/trace)
(require C311/monads)

(define even-length?
  (lambda (l)
    (cond
      [(null? l) (return-state '_)]
      [else
       (do bind-state
         (s <- get-state)
         (put-state (not s))
         (even-length? (cdr l)))])))

((even-length? '(1 2 3 4)) 0)

(define abc-game
  (lambda (l)
    (cond
      [(null? l) (return-state '_)]
      [(eqv? (car l) 'a)
       (do bind-state
         (s <- get-state)
         (put-state (add1 s))
         (abc-game (cdr l)))]
      [(eqv? (car l) 'b)
       (do bind-state
         (s <- get-state)
         (put-state (sub1 s))
         (abc-game (cdr l)))]
      [else
       (do bind-state
         (abc-game (cdr l)))])))

((abc-game '(a b c c b a)) 0)
;(__ . 0)
 
((abc-game '(a b c c b a a)) 0)
;(__ . 1)
 
((abc-game '(a a a)) 0)
;(__ . 3)
