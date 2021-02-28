#lang plai-typed

(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)])

(define (interp [e : ExprC] [fds : (listof FunDefC)]) : number
  (type-case ExprC e
    [numC (n) n]
    [idC (s) (error 'interp "shouldn't get here")]
    [appC (fun arg) (local ([define fd (get-fundef fun fds)])
                      (interp (subst arg
                                     (fdC-arg fd)
                                     (fdC-body fd))
                              fds))]
    [plusC (l r) (+ (interp l fds) (interp r fds))]
    [multC (l r) (* (interp l fds) (interp r fds))]))

(define (get-fundef [s : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond [(empty? fds) (error 'get-fundef "reference to undefined function")]
        [(cons? fds) (cond [(equal? s (fdC-name (first fds))) (first fds)]
                           [else (get-fundef s (rest fds))])]))

(define (subst [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
  (type-case ExprC in
    [numC (n) in]
    [idC (s) (cond [(symbol=? s for) what]
                   [else in])]
    [appC (fun arg) (appC fun (subst what for arg))]
    [plusC (l r) (plusC (subst what for l)
                        (subst what for r))]
    [multC (l r) (multC (subst what for l)
                        (subst what for r))]))

(define fun1 (fdC 'double 'x (plusC (idC 'x) (idC 'x))))
(define fun2 (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x)))))
(define fun3 (fdC 'const5 '_ (numC 5)))
(define funs (list fun1 fun2 fun3))

(test (interp (appC 'const5 (numC 12)) funs)
      5)

(test (interp (appC 'double (numC 12)) funs)
      24)

(test (interp (appC 'quadruple (numC 12)) funs)
      48)

