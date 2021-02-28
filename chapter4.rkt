#lang plai-typed

;; how to parse negative number. for example -1.

(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

(define-type ArithS
  [numS (n : number)]
  [uminusS (e : ArithS)]
  [plusS (l : ArithS) (r : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)])

(define (desugar [as : ArithS]) : ArithC
  (type-case ArithS as
    [numS (n) (numC n)]
    [uminusS (s) (desugar (bminusS (numS 0) s))]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [multS (l r) (multC (desugar l) (desugar r))]))

(define (parse [s : s-expression]) : ArithS
  (cond [(s-exp-number? s) (numS (s-exp->number s))]
        [(s-exp-list? s)
         (let [(s1 (s-exp->list s))]
           (case (s-exp->symbol (first s1))
             ['+ (plusS (parse (second s1)) (parse (third s1)))]
             ['- (bminusS (parse (second s1)) (parse (third s1)))]
             ['* (multS (parse (second s1)) (parse (third s1)))]))]
        [else (error 'interp "invaild input")]))

(define (interp [a : ArithC]) : number
  (type-case ArithC a
    [numC (n) n ]
    [plusC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]))

(test (interp (desugar (parse '(+ (* 1 (- 3 1)) (* 2 3)))))
      8)





