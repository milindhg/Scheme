#lang racket
(require macro-debugger/expand)
(provide
 (all-defined-out)
 (all-from-out macro-debugger/expand))


(define-syntax mcond
  (syntax-rules(else)
    ((_ (t0 c0) (t1 c1) (t2 c2) ...)
     (if t0 c0 (mcond (t1 c1) (t2 c2) ...)))
    ((_ (else a)) a)
    ((t0 any ...)
     (raise-syntax-error
      'mcond 
      "unbound identifier in module"))))

(mcond (#f #t) (else 'dog))

(mcond (else 'cat))

(mcond 
    (#t #t) 
    (unbound variables))


(mcond 
    (#f #t) 
    (unbound variables))