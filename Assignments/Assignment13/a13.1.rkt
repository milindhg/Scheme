#lang racket
(require C311/numbers)
(require C311/mk)
(require C311/let-pair)
(provide (all-defined-out))

(define listo
  (lambda (ls)
    (conde
     ((== ls '()))
     ((=/= ls '())
      (fresh (a d)
             (== `(,a . ,d) ls) (listo d))))))

(run 1 (q) (listo '(a b c d e)))
;(_.0)
 
(run 1 (q) (listo '(a b c d . e)))
;()
 
(run 4 (q) (listo q))
;(() (_.0) (_.0 _.1) (_.0 _.1 _.2))
 
(run 4 (q) (listo `(a b ,q)))
;(_.0)