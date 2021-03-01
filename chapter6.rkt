#lang plai-typed

(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)])

(define (get-fundef [s : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond [(empty? fds) (error 'get-fundef "reference to undefined function")]
        [(cons? fds) (cond [(equal? s (fdC-name (first fds))) (first fds)]
                           [else (get-fundef s (rest fds))])]))

(define-type Binding
  [bind (name : symbol) (val : number)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define (interp [expr : ExprC] [env : Env] [fds : (listof FunDefC)]) : number
  (type-case ExprC expr
    [numC (n) n]
    [idC (s) (lookup s env)]
    [appC (fun arg) (local [(define f (get-fundef fun fds))]
                      (interp (fdC-body f)
                              (extend-env (bind (fdC-arg f)
                                                (interp arg env fds))
                                          mt-env)
                              fds))]
    [plusC (l r) (+ (interp l env fds)
                    (interp r env fds))]
    [multC (l r) (* (interp l env fds)
                    (interp r env fds))]))

(define (lookup [s : symbol] [env : Env]) : number
  (cond [(empty? env) (error 'lookup "name not found")]
        [else (cond [(symbol=? s (bind-name (first env)))
                     (bind-val (first env))]
                    [else (lookup s (rest env))])]))

;;  test
(define fun1 (fdC 'double 'x (plusC (idC 'x) (idC 'x))))
(define fun2 (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x)))))
(define fun3 (fdC 'const5 '_ (numC 5)))
(define funs (list fun1 fun2 fun3))


(test (interp (appC 'const5 (numC 12)) mt-env funs)
      5)

(test (interp (appC 'double (numC 12)) mt-env funs)
      24)

(test (interp (appC 'quadruple (numC 12)) mt-env funs)
      48)

(test/exn (interp (appC 'f1 (numC 3))
                  mt-env
                  (list (fdC 'f1 'x (appC 'f2 (numC 4)))
                        (fdC 'f2 'y (plusC (idC 'x) (idC 'y)))))
          "lookup: name not found")
          
