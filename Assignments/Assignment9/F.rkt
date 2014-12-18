#lang racket
(define print-regs
  (lambda ()
    (printf "\n")
    (printf "k=~a" k)(printf " expr=~a" expr)(printf " v=~a" v)(printf " c=~a" c)(printf " a=~a" a)(printf " env=~a" env)(printf " num=~a" num)
    (printf "\n")))
