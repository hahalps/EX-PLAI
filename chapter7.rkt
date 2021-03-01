#lang plai-typed

;; we can put number in position of appC fun. occur error. how to fix?

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define-type Binding
  [bind (name : symbol) (val : Value)])

(define (lookup [s : symbol] [env : Env]) : Value
  (cond [(empty? env) (error 'lookup "name not found")]
        [else (cond [(symbol=? s (bind-name (first env)))
                     (bind-val (first env))]
                    [else (lookup s (rest env))])]))

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [lamC (arg : symbol) (body : ExprC)])

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)])

(define (interp [expr : ExprC] [env : Env]) : Value
  (type-case ExprC expr
    [numC (n) (numV n)]
    [idC (n) (lookup n env)]
    [appC (f a) (local ([define fd (interp f env)])
                 (interp (closV-body fd)
                         (extend-env (bind (closV-arg fd)
                                           (interp a env))
                                     (closV-env fd))))]
    [plusC (l r) (num+ (interp l env)
                       (interp r env))]
    [multC (l r) (num* (interp l env)
                       (interp r env))]
    [lamC (a b) (closV a b env)]))

(define (num+ [a : Value] [b : Value]) : Value
  (cond [(and (numV? a) (numV? b))
         (numV (+ (numV-n a) (numV-n b)))]
        [else (error 'num+ "one argument was not a number")]))

(define (num* [a : Value] [b : Value]) : Value
  (cond [(and (numV? a) (numV? b))
         (numV (* (numV-n a) (numV-n b)))]
        [else (error 'num* "one argument was not a number")]))

;; test
(test (interp (plusC (numC 10) (appC (lamC  '_ (numC 5)) (numC 10)))
              mt-env)
      (numV 15))
 
(test (interp (appC (lamC  'x (appC (lamC  'y (plusC (idC 'x) (idC 'y)))
                                          (numC 4)))
                        (numC 3))
                  mt-env)
          (numV 7))