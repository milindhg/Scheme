#lang racket
(require C311/trace)
(require C311/pmatch)

(trace-define my-*
    (lambda (m n)
      (* m n)))

(define mult
    (lambda (n*)
      (letrec
        ((m
          (lambda (n*)
            (cond
              ((null? n*) 1)
              ((zero? (car n*)) 0)
              (else (my-* (car n*) (mult (cdr n*))))))))
        (m n*))))

(define mult/acc
    (lambda (n*)
      (letrec 
        ((m/acc
          (lambda (n* acc)
            (cond
              ((null? n*) acc)
              ((zero? (car n*)) 0)
              (else (m/acc (cdr n*) (my-* (car n*) acc)))))))
        (m/acc n* 1))))


(define mult/cc
    (lambda (n*)
      (call/cc
       (lambda (k)
         (letrec
           ((m/cc
             (lambda (n*)
               (cond 
                 [(null? n*) 1]
                 [(zero? (car n*)) (k 0)]
                 [else (my-* (m/cc (cdr n*)) (car n*))])
               )))
           (m/cc n*))))))

;(mult '(1 2 3 4 5))
;(mult '(0 1 2 3 4 5))
;(mult '(1))
;(mult '(2 2  4 4 5 4 3  22 0))
;(mult '(2 2  4 4 5 4 3  22 ))
;
;(mult/acc '(1 2 3 4 5))
;(mult/acc '(0 1 2 3 4 5))
;(mult/acc '(1))
;(mult/acc '(2 2  4 4 5 4 3  22 0))
;(mult/acc '(2 2  4 4 5 4 3  22 ))
;
;(mult/cc '(1 2 3 4 5))
;(mult/cc '(0 1 2 3 4 5))
;(mult/cc '(1))
;(mult/cc '(2 2  4 4 5 4 3  22 0))
;(mult/cc '(2 2  4 4 5 4 3  22 ))
;
;(mult/cc '(1 2 3 4 6 7 8 9))
;(mult '(1 2 3 4 0 6 7 8 9))
;(mult/acc '(1 2 3 4 0 6 7 8 9))
;
;(mult/cc '(1 2 3 4 0 6 7 8 9))

(mult/cc '(1 2 3 4 5))
;
(mult/cc '(1 2 3 4 5 0))
