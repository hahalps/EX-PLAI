#lang plai-typed

;; parenthesized sequences (stream of token) -> s-expression -> syntax tree -> ...

(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

(define (parse [s : s-expression]) : ArithC
  (cond [(s-exp-number? s) (numC (s-exp->number s))]
        [(s-exp-list? s)
         (let ([s1 (s-exp->list s)])
           (case (s-exp->symbol (first s1))
             ['+ (plusC (parse (second s1)) (parse (third s1)))]
             ['* (multC (parse (second s1)) (parse (third s1)))]))]
        (else (error 'parse "invalid input"))))

;; warning up
;; case: alternative must be a symbol or a number in: (quote +)

(test (parse '(+ (* 1 2) (+ 2 3)))
      (plusC (multC (numC 1) (numC 2))
             (plusC (numC 2) (numC 3))))


