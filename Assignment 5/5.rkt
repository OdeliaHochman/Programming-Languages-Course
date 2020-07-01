  ;; The Flang interpreter, using environments

  #lang pl





#| The grammar:
      <FLANG> ::= <num>
                 | { with { <id> <FLANG> } <FLANG> }
                 | <id>
                 | { fun { <id> } <FLANG> } ;;a function may have a single formal parameter
                 | { fun { <id> <id> } <FLANG> } ;; or two formal parameters
                 | { call <FLANG> <FLANG> } ;;a function has either a single actual parameter
                 | { call <FLANG> <FLANG> <FLANG> } ;; or two actual parameters
                                   


   eval: Evaluation rules:
     eval(N,env)                  = N
     eval(x,env)                  = lookup(x,env)
     eval({with {x E1} E2},env)   = eval(E2,extend(x,eval(E1,env),env))
     eval({fun {x1} E},env)       = <{fun {x1} E}, env>
     eval({fun {x1 x2} E},env)    = <{fun {x1 x2} E}, env>
     eval({call E-op E1},env1)
             = eval(Ef,extend(x1,eval(E1,env),envf))
                          if eval(E-op,env) = <{fun {x} Ef}, envf>
              = error! otherwise
     eval({call E-op E1 E2},env1)
             = eval(Ef,extend(x2,eval(E2,env),extend(x1,eval(E1,env),envf))
                         if eval(E-op,env) = <{fun {x1 x2} Ef}, envf>
               = error! Otherwise
  |#




  (define-type FLANG
    [Num Number]
    [Id Symbol]
    [Add FLANG FLANG] ; Never created by user
    [Sub FLANG FLANG] ; Never created by user
    [Mul FLANG FLANG] ; Never created by user
    [Div FLANG FLANG] ; Never created by user
    [With Symbol FLANG FLANG]
    [Fun1  Symbol FLANG]
    [Fun2  Symbol Symbol FLANG]
    [Call1 FLANG FLANG]
    [Call2 FLANG FLANG FLANG])


#|

   The function receives Sexpr and returns FLANG. 
|#
  (: parse-sexpr : Sexpr -> FLANG)
  ;; to convert s-expressions into FLANGs
  (define (parse-sexpr sexpr)
    (match sexpr
      [(number: n)    (Num n)]
      [(symbol: name) (Id name)]
      [(cons 'with more)
       (match sexpr
         [(list 'with (list (symbol: name) named) body)
          (With name (parse-sexpr named) (parse-sexpr body))]
         [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
      [(cons 'fun more)
        (match sexpr
          [(list 'fun (list (symbol: name)) body)
          (Fun1 name (parse-sexpr body))]
          [(list 'fun (list (symbol: name) (symbol: name2)) body)
          (Fun2 name name2 (parse-sexpr body))]
          [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
      [(list 'call fun arg) (Call1 (parse-sexpr fun) (parse-sexpr arg))]
      [(list 'call fun arg1 arg2) (Call2 (parse-sexpr fun) (parse-sexpr arg1) (parse-sexpr arg2))]
      [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))




  (: parse : String -> FLANG)
  ;; parses a string containing a FLANG expression to a FLANG AST
  (define (parse str)
    (parse-sexpr (string->sexpr str)))

  ;; Types for environments, values, and a lookup function

  (define-type ENV
    [EmptyEnv]
    [Extend Symbol VAL ENV])

;; I took the original code from "original FLANG ENV" file and add the new FunV2 like in the "define".
  (define-type VAL
    [NumV Number]
    [FunV1 Symbol FLANG ENV]
    [FunV2 Symbol Symbol FLANG ENV]) 

  (: lookup : Symbol ENV -> VAL)
  (define (lookup name env)
    (cases env
      [(EmptyEnv) (error 'lookup "no binding for ~s" name)]
      [(Extend id val rest-env)
       (if (eq? id name) val (lookup name rest-env))]))

  (: arith-op : (Number Number -> Number) VAL VAL -> VAL)
  ;; gets a Racket numeric binary operator, and uses it within a NumV
  ;; wrapper
  (define (arith-op op val1 val2)
    (: NumV->number : VAL -> Number)
    (define (NumV->number v)
      (cases v
        [(NumV n) n]
        [else (error 'arith-op "expects a number, got: ~s" v)]))
    (NumV (op (NumV->number val1) (NumV->number val2))))



#|
   The function receives Sexpr and returns FLANG.

|#
(: eval : FLANG ENV -> VAL)
;; evaluates FLANG expressions by reducing them to values
(define (eval expr env)
(cases expr
  [(Num n) (NumV n)]
  [(With bound-id named-expr bound-body)
    (eval bound-body
          (Extend bound-id (eval named-expr env) env))]
  [(Id name) (lookup name env)]
  [(Add l r) (arith-op + (eval l env) (eval r env))]
  [(Sub l r) (arith-op - (eval l env) (eval r env))]
  [(Mul l r) (arith-op * (eval l env) (eval r env))]
  [(Div l r) (arith-op / (eval l env) (eval r env))]
  [(Fun1 bound-id bound-body) (FunV1 bound-id bound-body env)]
  [(Fun2 bound-id bound-id2 bound-body) (FunV2 bound-id bound-id2 bound-body env)]
  [(Call1 fun-expr arg-expr1)
       (let ([fval (eval fun-expr env)])
         (cases fval
           [(FunV1 bound-id bound-body f-env)
            (eval bound-body
                  (Extend bound-id (eval arg-expr1 env) f-env))]
           [(FunV2 bound-id bound-id2 bound-body f-env)
                (error 'eval "expected two arguments, got one in: ~s" fval)]
           [else (error 'eval "`call' expects a function, got: ~s" fval)]))]
   [(Call2 fun-expr arg-expr1 arg-expr2)
       (let ([fval (eval fun-expr env)])
         (cases fval
           [(FunV2 bound-id bound-id2 bound-body f-env)
            (eval bound-body
                  (Extend bound-id (eval arg-expr1 env)(Extend bound-id2 (eval arg-expr2 env) f-env)))]
           [(FunV1 bound-id bound-body f-env)
               (error 'eval "expected a single argument, got two in: ~s" fval)]
           [else (error 'eval "`call' expects a function, got: ~s" fval)]))]))


#|
  Environment that is provided to every run.
  Here we incorporate  basic arithmetic operations to let the programs to know and use them.

|#
(: createGlobalEnv : -> ENV)
(define (createGlobalEnv)
(Extend '+ (FunV2 'a 'b (Add (Id 'a) (Id 'b)) (EmptyEnv))
(Extend '- (FunV2 'a 'b (Sub (Id 'a) (Id 'b)) (EmptyEnv))
(Extend '* (FunV2 'a 'b (Mul (Id 'a) (Id 'b)) (EmptyEnv))
(Extend '/ (FunV2 'a 'b (Div (Id 'a) (Id 'b)) (EmptyEnv)) (EmptyEnv))))))



#|
  The function is unchanged except for the environment it gets in the eval of the string. I sent her the new environment - "createGlobalEnv".
  The function receives String and returns Number.
|#
  (: run : String -> Number)
  ;; evaluate a FLANG program contained in a string
  (define (run str)
  (let  ([result (eval (parse str) (createGlobalEnv))])
    (cases result
     [(NumV n) n]
     [else (error 'run "evaluation returned a non-number: ~s" result)])))








    

;; tests
(test (run "{call + 4 5}") => 9)
(test (run "{with {add3 {fun {x} {call + x 3}}} {call add3 1}}") => 4)
(test (run "{with {x 3} {with {f {fun {y} {call + x y}}} {with {x 5} {call f 4}}}}") => 7)
(test (run "{call {fun {x y} {call + x { call - y 1}}} 4 2}") => 5)
(test (run "{with {first {fun {x y} x}} {with {second {fun {x y} y}} {call first {call second 2 123} 124}}}") => 123)
(test (run "{+ 4 5}") =error> "parse-sexpr: bad syntax in (+ 4 5)")
(test (run "{* 4 5}") =error> "parse-sexpr: bad syntax in (* 4 5)")

(test (run "{with {add3 {fun {x} {call + x 3}}} {call add3 1 2}}") =error> "eval: expected a single argument, got two in:")
(test (run "{with {add3 {fun {x stam} {call + x 3}}} {call add3 1}}") =error> "eval: expected two arguments, got one in:") 

(test (run "{call {with {x 5} {with {x x} x}} 5}") =error> "eval: `call' expects a function, got: #(struct:NumV 5)")
(test (run "{with {add3 {fun {x} {call * x 3}}} {call add3 1}}") => 3)
(test (run "{with {add3 {fun {x} {call / x 2}}} {call add3 1}}") => 1/2)
(test (run "{with {{x x} x}}") =error> "bad `with' syntax in")
(test (run "{call {fun {4} {- x 1}} x}") =error> "parse-sexpr: bad `fun' syntax in (fun (4) (- x 1))")
(test (run "{call {with {x 5} {with {x x} x}} 5 2}") =error> "eval: `call' expects a function, got:")
(test (run "{with {x 3} {with {f {fun {y} {call + x y}}} {with {x 5} {call + x z}}}}") =error> "lookup: no binding for z")
(test (run "{call + x 5}") =error> "lookup: no binding for x")
(test (run "{call + {fun {x} {call / x 2}} 5}") =error> "arith-op: expects a number, got:")
(test (run "{x}") =error> "parse-sexpr: bad syntax in (x)")
(test (run "{fun {x} {call * x x}}") =error> "run: evaluation returned a non-number:")

;;Eran's tests
(run "  {with {mycons {fun {f s} 
                   {fun {sel} {call sel f s}}}}
      {with {myfirst {fun {p} {call p {fun {a b} a}}}}
         {with {mysecond {fun {p} {call p {fun {a b} b}}}}
             {with {p1 {call mycons 1 2}}
                 {with {p2 {call mycons 3 4}}
                     {call + {call myfirst p1}
                             {call mysecond p2}}}}}}}")
(test (run "  {with {mycons {fun {f s} 
                   {fun {sel} {call sel f s}}}}
      {with {myfirst {fun {p} {call p {fun {a b} a}}}}
         {with {mysecond {fun {p} {call p {fun {a b} b}}}}
             {with {p1 {call mycons 1 2}}
                 {with {p2 {call mycons 3 4}}
                     {call + {call myfirst p1}
                             {call mysecond p2}}}}}}}") => 5)

