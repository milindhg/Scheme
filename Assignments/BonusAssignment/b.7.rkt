#lang racket


(map (lambda (x) (list x x)) '(a b c))
;((a a) (b b) (c c))

(define-syntax copy-code
    (syntax-rules ()
      [(_ x ) `(,x x)]
      ))
;(copy-code '2)
;(2 '2)
(copy-code (lambda (x) x))

;(#<procedure> (lambda (x) x))
;(copy-code 'a)
;(a 'a)

;(map copy-code '(a b c))
;stdin::167: copy-code: bad syntax
;  in: copy-code
;  context...:

(define-syntax macro-map
 (syntax-rules()
  [(_ m) '()]
  [(_ m '()) '()]
  [(_ m '(a0 a1 ...)) (cons (m a0) (macro-map m '(a1 ...)))]))

(macro-map quote)
(macro-map quote '((trinidad and tobago) (saint vincent and the grenadines) (antigua and barbuda)))
(macro-map quote '(a))


(macro-map quote '((trinidad and tobago) (saint vincent and the grenadines) (antigua and barbuda)))
;((trinidad and tobago)
; (saint vincent and the grenadines)
; (antigua and barbuda))
(macro-map copy-code '((lambda (x) x) (lambda (x) (+ 2 x)) (lambda (x) 7)))
;((#<procedure> (lambda (x) x))
; (#<procedure> (lambda (x) (+ 2 x)))
; (#<procedure> (lambda (x) 7)))
(define-syntax quote-quote
    (syntax-rules ()
      [(_ e) (quote (quote e))]))
(macro-map quote-quote '((trinidad and tobago) (saint vincent and the grenadines) (antigua and barbuda)))
;('(trinidad and tobago)
; '(saint vincent and the grenadines)
; '(antigua and barbuda))