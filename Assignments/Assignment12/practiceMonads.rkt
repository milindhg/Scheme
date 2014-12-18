#lang racket

(assv 'c '((a 4) (b 5) (c 6)))

(define my-assv
  (lambda (x ls)
    (cond
      ((eqv? ls '()) '())
      ((eqv? (car (car ls)) x) (car ls))
      (else (my-assv x (cdr ls))))))

(my-assv 'c '((a 4) (b 5) (c 6)))

(define return-id (lambda (a) a))

(define bind-id (lambda (ma f) (f ma)))

(define my-plus
  (lambda (a b)
    (bind-id (return-id (+ a b)) (lambda (x) (return-id x)))))

(my-plus 4 5)



(define return-maybe
  (lambda (a)
    `(Just ,a)))

(define bind-maybe
  (lambda (ma f)
    (cond
      [(eqv? (car ma) 'Just) (f (cdr ma))]
      [(eqv? (car ma) 'Nothing) '(Nothing)])))

(define fail
  (lambda ()
    '(Nothing)))

(define divide-maybe
  (lambda (a b)
    (if (zero? b) (fail) (return-maybe (/ a b)))))

(divide-maybe 4 0)

(divide-maybe 4 8)


