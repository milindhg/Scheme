#lang racket
(require C311/pmatch)

(define empty-k
  (lambda ()
    (lambda (v) v)))


(define ack
  (lambda (m n k)
    (cond
      [(zero? m) (k (add1 n))]
      [(zero? n) (ack (sub1 m) 1 k)]
      [else (ack m (sub1 n) (lambda (v) (ack (sub1 m) v k)))])))
 
(define depth
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(pair? (car ls))
       (depth (car ls)
	      (lambda (l)
		(depth (cdr ls)
		       (lambda (r)
			 (let ((l (add1 l)))
			   (k (if (< l r) r l)))))))]
      [else (depth (cdr ls) k)])))
 
(define fact
  (lambda (n k)
    ((lambda (fact k)
       (fact fact n k))
     (lambda (fact n k)
       (cond
         [(zero? n) (k 1)]
         [else (fact fact (sub1 n) (lambda (v) (k (* n v))))]))
     k)))
 
(define pascal
  (lambda (n k)
    (let ((pascal
           (lambda (pascal k)
             (k (lambda (m a k)
		  (cond
		    [(> m n) (k '())]
		    [else (let ((a (+ a m)))
			    (pascal pascal (lambda (f) (f (add1 m) a (lambda (v) (k (cons a v)))))))]))))))
      (pascal pascal (lambda (f) (f 1 0 k))))))


(ack 0 0 (empty-k))
;7
(depth '(1 (2 (3 (4)))) (empty-k))
;4
(fact 5 (empty-k))
;120
(pascal 10 (empty-k))
;(1 3 6 10 15 21 28 36 45 55)


