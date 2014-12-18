#lang racket
(require C311/mk)
(require C311/numbers)
(provide (all-defined-out) (all-from-out C311/mk) (all-from-out C311/numbers))
(require C311/let-pair)
(require C311/trace)
;;(require "a10-student-tests.rkt")
;;(test-file #:file-name "a10.rkt")
;;------------------------------------------------------------------------------------------------------------------------------------------------
;;------------------------------------------------------------------------------------------------------------------------------------------------
;; Part I Write the answers to the following problems using your
;; knowledge of miniKanren.  For each problem, explain how miniKanren
;; arrived at the answer.  You will be graded on the quality of your
;; explanation; a full explanation will require several sentences.

;; 1 What is the value of 
(run 2 (q)
  (== 5 q)
  (conde
    [(conde [(== 5 q)
	     (== 6 q)])
     (== 5 q)]
    [(== q 5)]))
;;
;;
;;We can have conjunction of various goals in minikanren. However, Conde gives us the ability to have disjunctions of goals as well. 
;;The Answer we get for the above example is (5) 
;;In the given example - There are 2 disjoint goals to be met.
;;First goal in the conde says that (q should be equal to 5 or q should be equal to 6) and (q should be equal to 5). This is never possible. So don't get any output for this conde line.
;;Second goal is that q must be equal to 5. There is only one way it can hold true i.e. when q = 5. 
;;So no matter how many times q is run, we get only one output 5. Since the first goal is not met anyway and the second goal has only one way to hold true.
;;So we can also say that we have only one way the given example holds true. and hence no matter how many times we run it, we get only one answer i.e. (5)


;; 2 What is the value of
(run 1 (q) 
  (fresh (a b) 
    (== `(,a ,b) q)
    (absento 'tag q)
    (symbolo a)))
;;
;;
;;The answer we get for the above example is (((_.0 _.1) (=/= ((_.0 tag))) (sym _.0) (absento (tag _.1))))
;;In this example we begin by introducing 2 variables and try to assert 3 goals.
;;The first goal asserts that q should be a pair (i.e. a . d).
;;The second goal asserts that - tag should not be present anywhere in q.
;;The third goal says that a (i.e. the car of the pair) should be a symbol. 
;;There is only one way all the three constraints on q can hold true. i.e when q is a pair and the car of the pair is a symbol and there is no presence of the element tag anywhere in q.
;;1. (_.0 _.1) - q should be a pair.
;;2. (sym _.0) - the car of the pair must be a symbol
;;3. (absento (tag _.1)) - There should not be a single instance of tag present in the cdr of the pair.
;;4. (=/= ((_.0 tag))) - The car of the pair should not be tag. i.e. 3 and 4 together mean that tag should not be present anywhere in q.
;;Hence we get the answer as : (((_.0 _.1) (=/= ((_.0 tag))) (sym _.0) (absento (tag _.1))))

;; 3 What do the following miniKanren constraints mean?
;; a not-pairo
;; b =/=
;; c absento
;; d numbero
;; e symbolo

;; a not-pairo - not-pairo takes one argument. It asserts that the given argument must not be a pair.
;;Example: 
(run* (q)
 (not-pairo q))
;;this will give us the output as ((_.0 (not-pair _.0))) i.e. q can be any element but q must not be be a pair
;;Here the example has the constraint on q that it must not be a pair. So we get the output as q can be anything but q must be a pair.
;;
;; b =/= - This constraint takes two arguments. It asserts that the first argument should not be equal to the second argument.
;;Example: 
(run* (q)
 (=/= q 5))
;;this will give us the output as ((_.0 (=/= ((_.0 5))))) i.e. q can be any element but q must not be equal to 5
;;Here the example has the constraint on q that it must not be 5. So we get the output as q can be anything but 5.
;;
;; c absento - absento takes two arguments. It asserts that the first argument must be absent in the second argument.
;;Example: 
(run* (q)
 (absento 'cat q))
;;this will give us the output as ((_.0 (absento (cat _.0)))) i.e. q can be any element and it should not contain the symbol cat.
;;Here the example has the constraint on q that it must not contain the symbol cat. So we get the output as q can be anything and it must not have the symbol cat anywhere in it.
;;
;; d numbero - numbero takes one argument. It asserts that the given argument must be a number.
;;Example: 
(run* (q)
 (numbero q))
;;this will give us the output as ((_.0 (num _.0))) i.e. q can be any element and it should be a number
;;Here the example has the constraint on q that it must be a symbol. So we get the output must be a number.
;;
;; e symbolo - symbolo takes one argument. It asserts that the given argument must be a symbol.
;;Example: 
(run* (q)
 (symbolo q))
;;this will give us the output as ((_.0 (sym _.0))) i.e. q can be any element and it should be a symbol
;;Here the example has the constraint on q that it must be a symbol. So we get the output must be a symbol.
;;

;;------------------------------------------------------------------------------------------------------------------------------------------------
;; Part II goes here.

(define assoc
 (lambda (x ls)
  (let-pair ((a . d) ls)
   (let-pair ((aa . da) a)
    (cond
     ((equal? aa x) a)
     ((not (equal? aa x)) (assoc x d)))))))

(define assoco
 (lambda (x ls o)
  (fresh (a d)
   (== `(,a . ,d) ls)
   (fresh (aa da)
   (== `(,aa . ,da) a)
   (conde
    ((== aa x) (== a o))
    ((=/= aa x) (assoco x d o)))))))


(define reverse
 (lambda (ls)
  (cond
   ((equal? ls '()) '())
   ((not (equal? ls '()))
   (let-pair ((a . d) ls)
    (let ((res (reverse d)))
     (append res `(,a))))))))

(define reverseo
 (lambda (ls o)
  (conde
   ((== ls '()) (== ls o))
   ((=/= ls '())
    (fresh (a d)
     (== `(,a . ,d) ls)
     (fresh (res)
      (reverseo d res)
      (appendo res `(,a) o)))))))


(define stutter
 (lambda (ls)
  (cond
   ((null? ls) '())
   (else 
    (let-pair ((a . d) ls)
    (let ((res (stutter d)))
     `(,a ,a . ,res)))))))

(define stuttero
 (lambda (ls o)
  (conde
   ((== ls '()) (== ls o))
   ((=/= ls '()) 
    (fresh (a d)
     (== `(,a . ,d) ls)
     (fresh (res)
      (== `(,a ,a . ,res) o)
      (stuttero d res)))))))

;;------------------------------------------------------------------------------------------------------------------------------------------------
;;brainteaser
(define lengtho
 (lambda (ls o)
  (conde
   ((== ls '()) (== o ls))
   ((fresh (a d res)
    (== `(,a . ,d) ls)
    (pluso res '(1) o)
    (lengtho d res))))))

