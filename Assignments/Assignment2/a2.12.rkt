#lang racket
(require C311/pmatch) ;; <-- important
(require C311/trace)


(define lex                    ;;Main function
  (lambda (ls acc)
    (trace-define helper             ;;helper function which will help me get the lexical address of the variable passed in as argument.
      (lambda (x acc)
        (cond 
          ((eqv? x (car acc)) 0)          
          (else (add1 (helper x (cdr acc))))
          )
        ))
    (pmatch ls
            (`,x (guard (symbol? x)) (cond ((not (memv x acc)) `(free-var ,x)) (else `(var ,(helper x acc)))) )
            (`(lambda (,x) ,body) (cons 'lambda (list (lex body (cons x acc)))))
            (`(,rator ,rand) (list (lex rator acc) (lex rand acc)) )
            )))


;;Testcases below

(lex 'x '())  
;;(free-var x)
;;(free-var x)
(lex '(lambda (x) x) '())
;;(lambda (var 0))
;;(lambda (var 0))
(lex '(lambda (x) y) '())
;;(lambda (free-var y))
;;(lambda (free-var y))
(lex '(lambda (x) (x y)) '())
;;(lambda ((var 0) (free-var y)))
;;(lambda ((var 0) (free-var y)))
(lex '((lambda (x) (x y)) (lambda (c) (lambda (d) (e c)))) '()) 
;;((lambda ((var 0) (free-var y))) (lambda (lambda ((free-var e) (var 1)))))
;;((lambda ((var 0) (free-var y))) (lambda (lambda ((free-var e) (var 1)))))
(lex '(lambda (a)
        (lambda (b)
          (lambda (c)
            (lambda (a)
              (lambda (b)
                (lambda (d)
                  (lambda (a)
                    (lambda (e)
                      (((((a b) c) d) e) f))))))))) '())
;;(lambda (lambda (lambda (lambda (lambda (lambda (lambda (lambda ((((((var 1) (var 3)) (var 5)) (var 2)) (var 0)) (free-var f))))))))))
;;My Output!! :( (lambda (lambda (lambda (lambda (lambda (lambda (lambda (lambda (((((((var 1)) (var 3)) (var 5)) (var 2)) (var 0)) (free-var f))))))))))
;;(lambda (lambda (lambda (lambda (lambda (lambda (lambda (lambda ((((((var 1) (var 3)) (var 5)) (var 2)) (var 0)) (free-var f))))))))))

(lex '((lambda (a)
         (lambda (b)
           (lambda (c)
             (((((a b) c) w) x) y))))
       (lambda (w)
         (lambda (x)
           (lambda (y)
             (((((a b) c) w) x) y))))) '())
;;((lambda (lambda (lambda ((((((var 2) (var 1)) (var 0)) (free-var w)) (free-var x)) (free-var y)))))(lambda (lambda (lambda ((((((free-var a) (free-var b)) (free-var c)) (var 2)) (var 1)) (var 0))))))
;;myoutput value below
;;((lambda (lambda (lambda ((((((var 2) (var 1)) (var 0)) (free-var w)) (free-var x)) (free-var y)))))(lambda (lambda (lambda ((((((free-var a) (free-var b)) (free-var c)) (var 2)) (var 1)) (var 0))))))
;;((lambda (lambda (lambda ((((((var 2) (var 1)) (var 0)) (free-var w)) (free-var x)) (free-var y)))))(lambda (lambda (lambda ((((((free-var a) (free-var b)) (free-var c)) (var 2)) (var 1)) (var 0))))))

