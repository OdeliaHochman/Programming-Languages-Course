;; The Flang interpreter
#lang pl 04



#| The grammar:
<FLANG> ::= <num> ;; Rule 1
| { + <FLANG> <FLANG> } ;; Rule 2
| { - <FLANG> <FLANG> } ;; Rule 3
| { * <FLANG> <FLANG> } ;; Rule 4
| { / <FLANG> <FLANG> } ;; Rule 5
| { with { <id> <FLANG> } <FLANG> } ;; Rule 6
| <id> ;; Rule 7
| { fun { <id> } <FLANG> } ;; Rule 8
| { call <FLANG> <FLANG> } ;; Rule 9
| True  ;; add rule for True ;; Rule 10
| False  ;; Rule 11 
|{ = <FLANG> <FLANG> }  ;; add rule for = ;; Rule 12
|{ > <FLANG> <FLANG> }  ;; Rule 13
|{ < <FLANG> <FLANG> }  ;; Rule 14
| {not <FLANG>} ;; Rule 15
|{if <FLANG> {then-do <FLANG>} {else-do <FLANG>}}  ;; add rule 16 for (the above) if expressions



Evaluation rules:

    subst:
      N[v/x]                = N
      {+ E1 E2}[v/x]        = {+ E1[v/x] E2[v/x]}
      {- E1 E2}[v/x]        = {- E1[v/x] E2[v/x]}
      {* E1 E2}[v/x]        = {* E1[v/x] E2[v/x]}
      {/ E1 E2}[v/x]        = {/ E1[v/x] E2[v/x]}
      y[v/x]                = y
      x[v/x]                = v
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]} ; if y =/= x
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
      {call E1 E2}[v/x]     = {call E1[v/x] E2[v/x]}
      {fun {y} E}[v/x]      = {fun {y} E[v/x]}           ; if y =/= x
      {fun {x} E}[v/x]      = {fun {x} E}
      B[v/x] = B ;; B is Boolean
      {= E1 E2}[v/x] = {= E1[v/x] E2[v/x]}
      {> E1 E2}[v/x] = {> E1[v/x] E2[v/x]}
      {< E1 E2}[v/x] = {< E1[v/x] E2[v/x]}
      { not E}[v/x] = {not E[v/x]}
      {if Econd {then-do Edo} {else-do Eelse}}[v/x] = {if Econd[v/x] {then-do Edo[v/x]} {else-do Eelse[v/x]}}


    eval:
      eval(N)            = N
      eval({+ E1 E2})    = eval(E1) + eval(E2)  \ if both E1 and E2
      eval({- E1 E2})    = eval(E1) - eval(E2)   \ evaluate to numbers
      eval({* E1 E2})    = eval(E1) * eval(E2)   / otherwise error!
      eval({/ E1 E2})    = eval(E1) / eval(E2)  /
      eval(id)           = error!
      eval({with {x E1} E2}) = eval(E2[eval(E1)/x])
      eval(FUN)          = FUN ; assuming FUN is a function expression
      eval({call E1 E2}) = eval(Ef[eval(E2)/x]) if eval(E1)={fun {x}Ef}
                         = error!               otherwise
      eval(B) = B ;; B is an expression for a Boolean value
      eval({= E1 E2}) = eval(E1) = eval(E2) \ if both E1 and E2
      eval({> E1 E2}) = eval(E1) > eval(E2) \ evaluate to numbers
      eval({< E1 E2}) = eval(E1) < eval(E2) / otherwise error!
      eval({not E}) = not(eval(E)) /E may be anything
      eval({if Econd {then-do Edo} {else-do Eelse}})
                                                      = eval(Edo) if eval(Econd) =/= false,
                                                                     eval(Eelse),     otherwise.

  |#

#|
In this part we completed the missing parts of the FLANG type definition.
|#
  (define-type FLANG
    [Num  Number]
    [Add  FLANG FLANG]
    [Sub  FLANG FLANG]
    [Mul  FLANG FLANG]
    [Div  FLANG FLANG]
    [Id   Symbol]
    [With Symbol FLANG FLANG]
    [Fun  Symbol FLANG]
    [Call FLANG FLANG]
    [Bool Boolean]
    [Bigger FLANG FLANG]
    [Smaller FLANG FLANG]
    [Equal FLANG FLANG]
    [Not FLANG]
    [If FLANG FLANG FLANG])


#|
This function get input Sexpr and return as output FLANG
We wrote the expressions of binary operations like expressions of arithmetic operations
In the same way we also wrote "Not".
we added the Boolean part:
when we get a True symbol the output will be #t and when we get False symbol the output will be #f.
In addition, when we get an expression 'if that consist three arguments we convert this expressions into FLANGs.
|#
  (: parse-sexpr : Sexpr -> FLANG)
  ;; to convert s-expressions into FLANGs
  (define (parse-sexpr sexpr)
    (match sexpr
      [(number: n)(Num n)]
      ['True (Bool true)]
      ['False (Bool false)]
      [(symbol: name) (Id name)]
      [(cons 'with more)
       (match sexpr
         [(list 'with (list (symbol: name) named) body)
          (With name (parse-sexpr named) (parse-sexpr body))]
         [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
      [(cons 'fun more)
       (match sexpr
         [(list 'fun (list (symbol: name)) body)
          (Fun name (parse-sexpr body))]
         [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
      [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list 'call fun arg) (Call (parse-sexpr fun) (parse-sexpr arg))]
      [(list '= lhs rhs) (Equal(parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '> lhs rhs)(Bigger(parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '< lhs rhs)(Smaller(parse-sexpr lhs) (parse-sexpr rhs))]
      [(list 'not exp) (Not(parse-sexpr exp))]
      [(cons 'if more)
       (match sexpr
         [(list 'if expr (list 'then-do body1) (list 'else-do body2))
          (If (parse-sexpr expr) (parse-sexpr body1) (parse-sexpr body2))]
         [else (error 'parse-sexpr "bad `if' syntax in ~s" sexpr)])]
      [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

#|
This function get input variable type string and return as output FLANG
|#
  (: parse : String -> FLANG)
  ;; parses a string containing a FLANG expression to a FLANG AST
  (define (parse str)
    (parse-sexpr (string->sexpr str)))



#|
The function receives FLANG, Symbol and FLANG and returns FLANG.
Form of thinking: 
We wrote the expressions of binary operations like expressions of arithmetic operations.
In the same way we also wrote "Not" and "IF".
We wrote Bool like Num.
|#
  (: subst : FLANG Symbol FLANG -> FLANG)
  ;; substitutes the second argument with the third argument in the
  ;; first argument, as per the rules of substitution; the resulting
  ;; expression contains no free instances of the second argument
  (define (subst expr from to)
    (cases expr
      [(Num n) expr]
      [(Add l r) (Add (subst l from to) (subst r from to))]
      [(Sub l r) (Sub (subst l from to) (subst r from to))]
      [(Mul l r) (Mul (subst l from to) (subst r from to))]
      [(Div l r) (Div (subst l from to) (subst r from to))]
      [(Id name) (if (eq? name from) to expr)]
      [(With bound-id named-expr bound-body)
       (With bound-id
             (subst named-expr from to)
             (if (eq? bound-id from)
               bound-body
               (subst bound-body from to)))]
      [(Call l r) (Call (subst l from to) (subst r from to))]
      [(Fun bound-id bound-body)
       (if (eq? bound-id from)
         expr
         (Fun bound-id (subst bound-body from to)))]
      [(Bool b) expr]
      [(Equal l r) (Equal (subst l from to) (subst r from to))]
      [(Bigger l r) (Bigger (subst l from to) (subst r from to))]
      [(Smaller l r) (Smaller (subst l from to) (subst r from to))]
      [(Not n) (Not (subst n from to))]
      [(If exp1 exp2 exp3) (If (subst exp1 from to) (subst exp2 from to) (subst exp3 from to))]))


;; The following function is used in multiple places below,
;; hence, it is now a top-level definition
(: Num->number : FLANG -> Number)
;; gets a FLANG -- presumably a Num variant -- and returns the
;; unwrapped number
(define (Num->number e)
(cases e
[(Num n) n]
[else (error 'Num->number "expected a number, got: ~s" e)]))


(: arith-op : (Number Number -> Number) FLANG FLANG -> FLANG)
;; gets a Racket numeric binary operator, and uses it within a FLANG
;; `Num' wrapper
(define (arith-op op expr1 expr2)
(Num (op (Num->number expr1) (Num->number expr2))))


#|
 way of thinking:
 We copied the code from the "arith-op" function and did Cast to Bool.
|#
(: logic-op : (Number Number -> Boolean) FLANG FLANG -> FLANG)
;; gets a Racket Boolean binary operator (on numbers), and applies it
;; to two `Num' wrapped FLANGs
(define (logic-op op expr1 expr2)
(Bool (op (Num->number expr1) (Num->number expr2))))


#|
  The function receives FLANG and returns Boolean.
  way of thinking:
  In RACKET any expression that is not false is considered true.
  So we divided into 2 cases: If our FLANG is a Bool form then we will check if it is True or False and return accordingly.
  Otherwise, we will simply return True.
|#

(: flang->bool : FLANG -> Boolean)
;; gets a Flang E (of any kind) and returns a its appropiate
;; Boolean value -- which is true if and only if E does not
;; represent false
;; Remark: the `flang->bool` function will also be top-level
;; since it's used in more than one place.
(define (flang->bool e)
(cases e
[(Bool e) (if e #t #f)]
[else #t]))




#|
This function receives FLANG and returns FLANG
We wrote the expressions of binary operations like expressions of arithmetic operations.
The difference was that we used the logic-op function instead of arith-op function.
because logic-op function receives function that consist a function that receives two numbers and returns a boolean,
and in our eval we send to this function binary operation(Boolean). We wrote Bool like Num.

We first took the left expression of the "If" and did "eval" function on it.
Then we sent it to the flang->bool function to get a boolean value from it.
Then we made "If" condition and returned the appropriate "eval" according to the Boolean value of the left expression.

"Not exp" - We took the expr and sent it to the flang->bool function which returns us a boolean value.
Then, we used the  reserved word in RACKET - "not" and we sent it to the Bool constructor.
|#
  (: eval : FLANG -> FLANG)
  ;; evaluates FLANG expressions by reducing them to *expressions*
  (define (eval expr)
    (cases expr
      [(Num n) expr]
      [(Add l r) (arith-op + (eval l) (eval r))]
      [(Sub l r) (arith-op - (eval l) (eval r))]
      [(Mul l r) (arith-op * (eval l) (eval r))]
      [(Div l r) (arith-op / (eval l) (eval r))]
      [(With bound-id named-expr bound-body)
       (eval (subst bound-body
                    bound-id
                    (eval named-expr)))]
      [(Id name) (error 'eval "free identifier: ~s" name)]
      [(Fun bound-id bound-body) expr]
      [(Call fun-expr arg-expr)
       (let([fval (eval fun-expr)])
         (cases fval
           [(Fun bound-id bound-body)
            (eval (subst bound-body
                         bound-id
                         (eval arg-expr)))]
           [else (error 'eval "`call' expects a function, got: ~s" fval)]))]
     [(Bool b)  expr]
     [(Equal l r) (logic-op = (eval l) (eval r))]
     [(Bigger l r) (logic-op > (eval l) (eval r))]
     [(Smaller l r) (logic-op < (eval l) (eval r))]
     [(If l m r)
      (let ([fval (eval l)])
        (if(flang->bool fval) (eval m) (eval r)))]
     [(Not exp) (Bool(not(flang->bool (eval exp))))])) 

#|
  The function receives String and returns Number|Boolean|FLANG.
  way of thinking:
  Nun returns num , Bool returns boolean and ELSE will return FLANG.
|#
(: run : String -> (U Number Boolean FLANG))
;; evaluate a FLANG program contained in a string
(define (run str)
(let ([result (eval (parse str))])
(cases result
[(Num n) n]
[(Bool b) b]
[else result])))


;; tests
  (test (run "{call {fun {x} {+ x 1}} 4}")=> 5)
(test (run "{{fun {x} {+ x 1}} 4}")=error> "bad syntax in")
  (test (run "{with {add3 {fun {x} {+ x 3}}}
                {call add3 1}}")
        => 4)
    (test (run "{with {add3 {fun {x} {+ x 3}}}
                {with {add1 {fun {x} {+ x 1}}}
                  {with {x 3}
                    {call add1 {call add3 x}}}}}") => 7)



;; tests  - task4
(test (run "True") => true)
(test (run "{not True}") => false)
(test (run "{> 3 44}") => false)
(test (run "{if {- 3 3} {then-do 4} {else-do 5}}") => 4)
(test (run "{with {x 8} {if {> x 0} {then-do {/ 2 x}} {else-do x}}}") => 1/4)
(test (run "{with {x 0} {if {> x 0} {then-do {/ 2 x}} {else-do x}}}") => 0)
(test (run "{if {> 2 1} {then-do True} {else-do {+ 2 2}}}") => true)
(test (run "{with {c True} {if c {then-do {> 2 1}} {else-do 2}}}") => true)
(test (run "{with {foo {fun {x} {if {< x 2} {then-do x} {else-do {/ x 2}}}}} foo}") => (Fun 'x (If (Smaller (Id 'x) (Num 2)) (Id 'x) (Div (Id 'x) (Num 2)))))
(test (run "{with {x 0} {if {> x 0} {/ 2 x} x}}") =error> "parse-sexpr: bad `if' syntax in (if (> x 0) (/ 2 x) x)")
(test (run "true") =error> "eval: free identifier: true")
(test (run "{< false 5}") =error> "eval: free identifier: false")
(test (run "{< False 5}") =error> "Num->number: expected a number, got: #(struct:Bool #f)")
(test (run "{with {{x x} x}}") =error> "bad `with' syntax in")
(test (run "{= 5 20}") => false)
(test (run "{with {x 0} {if {> x 0} {then-do {* 2 x}} {else-do x}}}") => 0)
(test (run "{call {fun {x} {* x 1}} 4}") => 4)
(test (run "{call {fun {x} {- x 1}} 4}") => 3)
(test (run "{with {x 2} {if {= x 4} {then-do {* 2 x}} {else-do x}}}") => 2)
(test (run "{with {x 2} {if {< x 7} {then-do {* 10 x}} {else-do x}}}") => 20)
(test (run "{with {x 2} {if {not{< x 8}} {then-do {* 9 x}} {else-do x}}}") => 2)
(test (run "{with {x 2} {if {x} {then-do {* 9 x}} {else-do x}}}") =error> "parse-sexpr: bad syntax in (x)")
(test (run "{with {x 5} {with {y x} y}}") => 5)
(test (run "{with {x 5} {with {x x} x}}") => 5)
(test (run "{call {fun {4} {- x 1}} x}") =error> "parse-sexpr: bad `fun' syntax in (fun (4) (- x 1))")
(test (run "{call {fun {4} {- x 1}}{- x 2}}") =error> "parse-sexpr: bad `fun' syntax in (fun (4) (- x 1))") 
(test (run "{call {fun {4} {- x 1}} 2}") =error> "parse-sexpr: bad `fun' syntax in")
(test (run "{call {with {x 5} {with {x x} x}} 5}") =error> "eval: `call' expects a function, got: #(struct:Num 5)")
(test (run "{with {x 4} {if {= x 4} {then-do True} {else-do x}}}") => #t)
(test (run "{with {x 3} {if {not{< x 1}} {then-do {* 6 x}} {else-do x}}}") => 18)








