#lang plai-typed

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)]
  [objV (ns : (listof symbol)) (vs : (listof Value))])

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
  [lamC (arg : symbol) (body : ExprC)]
  [objC (ns : (listof symbol)) (vs : (listof ExprC))]
  [msgC (o : ExprC) (n : symbol)])

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
    [lamC (a b) (closV a b env)]
    [objC (ns es) (objV ns (map (lambda (e)
                                  (interp e env))
                                es))]
    [msgC (o n) (lookup-msg n (interp o env))]))

(define (lookup-msg [s : symbol] [v : Value]) : Value
  (let ([ns (objV-ns v)]
        [vs (objV-vs v)])
    (letrec ([f (lambda(nl vl)
                  (cond [(empty? nl) (error 'look-msg "can't find symbol")]
                        [else (cond [(symbol=? s (first nl)) (first vl)]
                                    [else (f (rest nl) (rest vl))])]))])
      (f ns vs))))

(define (num+ [a : Value] [b : Value]) : Value
  (cond [(and (numV? a) (numV? b))
         (numV (+ (numV-n a) (numV-n b)))]
        [else (error 'num+ "one argument was not a number")]))

(define (num* [a : Value] [b : Value]) : Value
  (cond [(and (numV? a) (numV? b))
         (numV (* (numV-n a) (numV-n b)))]
        [else (error 'num* "one argument was not a number")]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                           desugar                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type ExprS
  [numS (n : number)]
  [idS (s : symbol)]
  [appS (fun : ExprS) (arg : ExprS)]
  [plusS (l : ExprS) (r : ExprS)]
  [multS (l : ExprS) (r : ExprS)]
  [lamS (arg : symbol) (body : ExprS)]
  [objS (ns : (listof symbol)) (vs : (listof ExprS))]
  [msgS (o : ExprS) (n : symbol) [a : ExprS]]
  [letS (lhs : symbol) (rhs : ExprS) (body : ExprS)])

(define (desugar [s : ExprS]) : ExprC
  (type-case ExprS s
    [numS (n) (numC n)]
    [idS (s) (idC s)]
    [appS (f a) (appC (desugar f) (desugar a))]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    [lamS (a b) (lamC a (desugar b))]
    [objS (ns vs) (objC ns (map desugar vs))]
    [msgS (o n a) (appC (msgC (desugar o) n) (desugar a))]
    [letS (l r b) (appC (lamC l (desugar b)) (desugar r))])) 
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                         test                                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test (interp (appC (msgC (objC (list 'double 'const5) (list (lamC 'x (plusC (idC 'x) (idC 'x)))
                                                           (lamC '_ (numC 5))))
                          'const5)
                    (numC 12))
              mt-env)
      (numV 5))

(test (interp (desugar (letS 'o (objS (list 'add1 'sub1)
                                      (list (lamS 'x (plusS (idS 'x) (numS 1)))
                                            (lamS 'x (plusS (idS 'x) (numS -1)))))
                             (msgS (idS 'o) 'add1 (numS 3))))
              mt-env)
      (numV 4))
    
