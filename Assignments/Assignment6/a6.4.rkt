#lang racket
(require C311/trace)

(define times-cps-shortcut
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) 0]
      [else (times-cps-shortcut (cdr ls) (lambda (x) (k (* (car ls) x))))])))

(times-cps-shortcut '(1 2 3 4 5) (lambda (x) x))
;120
(times-cps-shortcut '(1 2 3 0 3) (lambda (x) x))
;0
