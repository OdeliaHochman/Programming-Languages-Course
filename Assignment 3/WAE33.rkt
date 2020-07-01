  #lang pl

#|
   This part took us a few hours because we did all the steps and tried to understand each step individually
   and also we did tests for every step. But this part was not very difficult.
   In this part we have been assisted by lectures, exercises and the internet.
   This part made it clear to us the material we learned in the lectures and in the exercises.
|#



   #| BNF for the MUWAE language:
       <MUWAE> ::= <num>
               | { + <MUWAE> <MUWAE> }
               | { - <MUWAE> <MUWAE> }
               | { * <MUWAE> <MUWAE> }
               | { / <MUWAE> <MUWAE> }
               | { with { <id> <MUWAE> } <MUWAE> }
               | <id>
               | {sqrt <MUWAE>}
  |#

  ;; MUWAE abstract syntax trees
  (define-type MUWAE
    [Num  (Listof Number)]
    [Add  MUWAE MUWAE]
    [Sub  MUWAE MUWAE]
    [Mul  MUWAE MUWAE]
    [Div  MUWAE MUWAE]
    [Id   Symbol]
    [With Symbol MUWAE MUWAE]
    [Sqrt MUWAE])

#|
  The function receives Sexpr and returns MUWAE.
|#
(: parse-sexpr : Sexpr -> MUWAE)
  ;; to convert s-expressions into MUWAEs
  (define (parse-sexpr sexpr)
    (match sexpr
      [(number: n)(Num(list n))]
      [(symbol: name) (Id name)]
      [(cons 'with more)
       (match sexpr
         [(list 'with (list (symbol: name) named) body)
          (With name (parse-sexpr named) (parse-sexpr body))]
         [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
      [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list 'sqrt exp) (Sqrt (parse-sexpr exp))]
      [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))


#|
  The function receives string and returns MUWAE.
|#

  (: parse : String -> MUWAE)
  ;; parses a string containing a MUWAE expression to a MUWAE AST
  (define (parse str)
    (parse-sexpr (string->sexpr str)))

  #| Formal specs for `subst':
     (`N' is a <num>, `E1', `E2' are <MUWAE>s, `x' is some <id>, `y' is a
     *different* <id>)
        N[v/x]                = N
        {+ E1 E2}[v/x]        = {+ E1[v/x] E2[v/x]}
        {- E1 E2}[v/x]        = {- E1[v/x] E2[v/x]}
        {* E1 E2}[v/x]        = {* E1[v/x] E2[v/x]}
        {/ E1 E2}[v/x]        = {/ E1[v/x] E2[v/x]}
        y[v/x]                = y
        x[v/x]                = v
        {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
        {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
        {sqrt E1}[v/x]    = {Sqrt E1[v/x]}
  |#


#|
  The function receives MUWAE, Symbol and MUWAE and returns MUWAE.
|#
  (: subst : MUWAE Symbol MUWAE -> MUWAE)
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
      [(Sqrt exp)(Sqrt (subst exp from to))]
      [(Id name) (if (eq? name from) to expr)]
      [(With bound-id named-expr bound-body)
       (With bound-id
             (subst named-expr from to)
             (if (eq? bound-id from)
               bound-body
               (subst bound-body from to)))]))

 

  #| Formal specs for `eval':
       eval(N)         = N
       eval({+ E1 E2}) = eval(E1) + eval(E2)
       eval({- E1 E2}) = eval(E1) - eval(E2)
       eval({* E1 E2}) = eval(E1) * eval(E2)
       eval({/ E1 E2}) = eval(E1) / eval(E2)
       eval(id)        = error!
       eval({with {x E1} E2}) = eval(E2[eval(E1)/x])
       eval({sqrt E1}) = eval(E1) Sqrt
  |#


#|
  The function receives MUWAE and returns a list of numbers.
|#
  (: eval : MUWAE -> (Listof Number))
  ;; evaluates MUWAE expressions by reducing them to numbers
  (define (eval expr)
    (cases expr
      [(Num n) n]
      [(Add l r) (bin-op + (eval l) (eval r))]
      [(Sub l r) (bin-op - (eval l) (eval r))]
      [(Mul l r) (bin-op * (eval l) (eval r))]
      [(Div l r) (bin-op / (eval l) (eval r))]
      [(Sqrt e) (sqrt+ (eval e))]
      [(With bound-id named-expr bound-body)
       (eval (subst bound-body
                    bound-id
                    (Num (eval named-expr))))]
      [(Id name) (error 'eval "free identifier: ~s" name)]))


#|
  The function receives and returns a list of numbers. 
  We used the site - stackoverflow.
|#
(: sqrt+ : (Listof Number) -> (Listof Number))
;; a version of `sqrt' that takes a list of numbers, and return a list
;; with twice the elements, holding the two roots of each of the inputs;
;; throws an error if any input is negative.
(define (sqrt+ ns)
  (cond [(null? ns) ns]
        [(< (first ns) 0) (error 'ns "`sqrt' requires a nonnegative input ~s" ns)]
        [else (cons (sqrt (first ns))
                    (cons (* -1 (sqrt (first ns)))
                          (sqrt+ (rest ns))))]))


#|
  The function receives string and returns a list of number.
|#
  (: run : String -> (Listof Number))
  ;; evaluate a MUWAE program contained in a string
  (define (run str)
    (eval (parse str)))





#|
  The function receives function(Number Number -> Number) two list of numbers and returns a list of numbers.
|#
(: bin-op : (Number Number -> Number) (Listof Number) (Listof Number) -> (Listof Number))
;; applies a binary numeric function on all combinations of numbers from
;; the two input lists, and return the list of all of the results
(define (bin-op op ls rs)
(: helper : Number (Listof Number) -> (Listof Number))
(define (helper l rs)
(: f : Number -> Number)
(define (f n)
(op l n))
(map f rs))
(if (null? ls) null
(append (helper (first ls) rs) (bin-op op (rest ls) rs))))




  ;; tests
  (test (run "5") => '(5))
  (test (run "{+ 5 5}") => '(10))
  (test (run "{with {x {+ 5 5}} {+ x x}}") => '(20))
  (test (run "{with {x 5} {+ x x}}") => '(10))
  (test (run "{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}") => '(14))
  (test (run "{with {x 5} {with {y {- x 3}} {+ y y}}}") => '(4))
  (test (run "{with {x 5} {+ x {with {x 3} 10}}}") => '(15))
  (test (run "{with {x 5} {+ x {with {x 3} x}}}") => '(8))
  (test (run "{with {x 5} {+ x {with {y 3} x}}}") => '(10))
  (test (run "{with {x 5} {with {y x} y}}") => '(5))
  (test (run "{with {x 5} {with {x x} x}}") => '(5))
  (test (run "{with {x 1} y}") =error> "free identifier")
  (test (run "{with {{x x} x}}") =error> "bad `with' syntax in")
  (test (run "{with {x 5} {* x {with {x 3} x}}}") => '(15))
  (test (run "{with {x 9} {/ x {with {x 3} x}}}") => '(3))
  (test (run "{8 2}") =error> "bad syntax in")
  (test (run "{with {x 9} {sqrt x}}") => '(3 -3))
  (test (run "{sqrt 25}") => '(5 -5))
  (test (run "{sqrt 9}") => '(3 -3))
  (test (run "{sqrt 0}") => '(0 0))
  (test (run "{sqrt -1}") =error> "`sqrt' requires a nonnegative input")
  (test (run "{+ {sqrt 1} 3}") => '(4 2))
  (test (run "{+ {/ {+ {sqrt 1} 3} 2} {sqrt 100}}") => '(12 -8 11 -9))
  (test (run "{sqrt {+ 16 {* {+ 1 {sqrt 1}} {/ 9 2}}}}") => '(5 -5 4 -4))





;----------------------------Part B-------------------------------------

#|

<WAE> ::= <num> 
   | {+ <WAE> <WAE>}
   | {-  <WAE> <WAE>}
   | {* <WAE> <WAE>}
   | {/ <WAE> <WAE>}
   | {with {<id> <WAE>} <WAE>}
   | <id>

|#



(define-type WAE
  [NumW Number]
  [AddW WAE WAE]
  [SubW WAE WAE]
  [MulW WAE WAE]
  [DivW WAE WAE]
  [IdW Symbol]
  [WithW Symbol WAE WAE])



(: parse-sexprW : Sexpr -> WAE) 
(define (parse-sexprW sexpr)
  (match sexpr
    [(number: n) (NumW n)]
    [(symbol: name) (IdW name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (WithW name (parse-sexprW named) (parse-sexprW body))]
       [else (error 'parse-sexprW "bad `with' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (AddW (parse-sexprW lhs) (parse-sexprW rhs))]
    [(list '- lhs rhs) (SubW (parse-sexprW lhs) (parse-sexprW rhs))]
    [(list '* lhs rhs) (MulW (parse-sexprW lhs) (parse-sexprW rhs))]
    [(list '/ lhs rhs) (DivW (parse-sexprW lhs) (parse-sexprW rhs))]
    [else (error 'parse-sexprW "bad syntax in ~s" sexpr)]))

(: parseW : String -> WAE)
(define (parseW str)
  (parse-sexprW (string->sexpr str)))



#| Formal specs for `subst':
   (`N' is a <num>, `E1', `E2' are <WAE>s, `x' is some <id>,
   `y' is a *different* <id>)
      N[v/x]                = N
      {+ E1 E2}[v/x]        = {+ E1[v/x] E2[v/x]}
      {- E1 E2}[v/x]        = {- E1[v/x] E2[v/x]}
      {* E1 E2}[v/x]        = {* E1[v/x] E2[v/x]}
      {/ E1 E2}[v/x]        = {/ E1[v/x] E2[v/x]}
      y[v/x]                = y
      x[v/x]                = v
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
|#



(: substW : WAE Symbol WAE -> WAE)
(define (substW expr from to)
  (cases expr
    [(NumW n) expr]
    [(AddW l r) (AddW (substW l from to) (substW r from to))]
    [(SubW l r) (SubW (substW l from to) (substW r from to))]
    [(MulW l r) (MulW (substW l from to) (substW r from to))]
    [(DivW l r) (DivW (substW l from to) (substW r from to))]
    [(IdW name) (if (eq? name from) to expr)]
    [(WithW bound-id named-expr bound-body)
     (WithW bound-id
           (substW named-expr from to)
           (if (eq? bound-id from)
               bound-body
               (substW bound-body from to)))]))



#|
    The function 'freeInstanceList' receives an abstract syntax tree (WAE) and returns list of symbols.

    The function 'evalHelper' receives an abstract syntax tree (WAE) and list of symbols and returns list of symbols.
    The function 'evalHelper' consumes an abstract syntax tree (WAE) and returns null if there are no free instance,
    and a list of all the free instances otherwise (multiple occurrences of the same identifier are appear in the list).

    we used the structure of eval and subs functions.
|#

(: freeInstanceList  : WAE -> (Listof Symbol))
  (define (freeInstanceList expr)
    (: evalHelper : WAE (Listof Symbol) -> (Listof Symbol))
    (define (evalHelper exp listSym)
    (cases exp
      [(NumW num) listSym]
      [(AddW l r) (append (evalHelper l listSym) (evalHelper r listSym))]
      [(SubW l r) (append (evalHelper l listSym) (evalHelper r listSym))]
      [(MulW l r) (append (evalHelper l listSym) (evalHelper r listSym))]
      [(DivW l r) (append (evalHelper l listSym) (evalHelper r listSym))]
      [(WithW bound-id named-expr bound-body)
              (evalHelper (substW bound-body bound-id named-expr) listSym)]
      [(IdW name) (cons name listSym)]))
  (evalHelper expr '()))






;;tests
(test (freeInstanceList (parseW "w")) => '(w))
(test (freeInstanceList (parseW "{with {xxx 2} {with {yyy 3} {+ {- xx y} z}}}")) => '(xx y z))
(test (freeInstanceList (WithW 'x (NumW 2) (AddW (IdW 'x) (NumW 3)))) => '())
(test (freeInstanceList (parseW "{+ z {+ x z}}")) => '(z x z))
(test (freeInstanceList (parseW "{* a {+ 2 z}}")) => '(a z))
(test (freeInstanceList (parseW "{with {a 2} {with {m 3} {/ {- a z} m}}}")) => '(z))
(test (freeInstanceList (parseW "{with {x 4} {with {y 5} {- {* x y} m}}}")) => '(m))
(test (freeInstanceList (parseW "{+ z {x z}}")) =error>"bad syntax in")
(test (freeInstanceList (parseW "{with {a 2} {{m 3} {/ {- a z} m}}}"))=error>"bad syntax in")
(test (freeInstanceList (parseW "{with}"))=error>"bad `with' syntax in")
(test (freeInstanceList (WithW 'x (NumW 2) (AddW (IdW 'y) (NumW 3)))) => '(y))
(test (freeInstanceList (WithW 'x (NumW 2) (SubW (NumW 4) (NumW 3)))) => '())
(test (freeInstanceList (WithW 'x (NumW 2) (SubW (IdW 'x) (NumW 3)))) => '())
(test (freeInstanceList (WithW 'x (NumW 2) (WithW 'y (NumW 2) (SubW (IdW 'x) (NumW 3))))) => '())
(test (freeInstanceList (WithW 'x (NumW 2) (WithW 'x (NumW 5) (SubW (IdW 'x) (NumW 3))))) => '())
