#lang racket
(require C311/let-pair)
(provide (all-defined-out))
(require C311/trace)
(require C311/monads)

(define power
  (lambda (x n)
    (cond
      [(zero? n) 1]
      [(= n 1) x]
      [(odd? n) (* x (power x (sub1 n)))]
      [(even? n) (let ((nhalf (/ n 2)))
                   (let ((y (power x nhalf)))
                     (* y y)))])))

;(power 2 6)

;WIthout the do syntax. This is required.
(define powerXpartials
  (lambda (x n)
    (cond
      [(zero? n) (return-writer 1)]
      [(= n 1) (return-writer x)]
      [(odd? n) (bind-writer (powerXpartials x (sub1 n)) (lambda (d) `(,(* x d) . (,d))))]
      [(even? n) (let ((nhalf (/ n 2)))
                   (bind-writer (powerXpartials x nhalf) (lambda (d) `(,(* d d) . (,d)))))])))

;;With do syntax. This is tried to understand the do syntax for using bind-writer.
(trace-define powerXpartialsOk
  (lambda (x n)
    (cond
      [(zero? n) (return-writer 1)]
      [(= n 1) (return-writer x)]
      [(odd? n) (do bind-writer (d <- (powerXpartials x (sub1 n))) `(,(* x d) . (,d)))]
      [(even? n) (let ((nhalf (/ n 2)))
                   (do bind-writer (d <- (powerXpartials x nhalf)) `(,(* d d) . (,d))))])))

(trace-define powerXpartialsAnother
  (lambda (x n)
    (cond
      [(zero? n) (return-writer 1)]
      [(= n 1) (return-writer x)]
      [(odd? n) (bind-writer (powerXpartials x (sub1 n)) (lambda (a) (bind-writer (tell-writer a) (lambda (_) (return-writer (* x a))))))]
      [(even? n) (let ((nhalf (/ n 2)))
                   (let ((y (powerXpartials x nhalf)))
                   (bind-writer y (lambda (a) (bind-writer (tell-writer a) (lambda (_) (return-writer (* a a))))))))])))

(powerXpartials 4 4)

(powerXpartials 4 0)

(powerXpartials 4 1)

(powerXpartials 4 2)

(powerXpartials 4 3)

(powerXpartials 4 5)

(powerXpartials 2 6)
;(64 . (2 4 8))

(powerXpartials 3 5)
;(243 . (3 9 81))
 
(powerXpartials 5 7)
;(78125 . (5 25 125 15625))

