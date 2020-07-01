#lang pl

#|
   Q1:

|#


; The ROL BNF and Parsing code:

;; Defining two new types
(define-type BIT = (U 0 1))
(define-type Bit-List = (Listof BIT))

;; The actual interpreter
#| BNF for the RegE language:
<ROL> ::=  {reg-len = <num> <RegE>}
<RegE> ::= {and <RegE> <RegE>}
          |{or <RegE> <RegE>}
          |{shl <RegE>}
          |{<Bits>}
<Bits> ::= {0 | 1 | 0 <Bits> | 1 <Bits>}

|#
;; RegE abstract syntax trees
(define-type RegE
[Reg Bit-List]
[And RegE RegE]
[Or RegE RegE]
[Shl RegE])

;; Next is a technical function that converts (casts)
;; (any) list into a bit-list. We use it in parse-sexpr.
(: list->bit-list : (Listof Any) -> Bit-List)
;; to cast a list of bits as a bit-list
(define (list->bit-list lst)
(cond [(null? lst) null]
[(eq? (first lst) 1)(cons 1 (list->bit-list (rest lst)))]
[else (cons 0 (list->bit-list (rest lst)))]))

#|
   The function receives Sexpr and returns RegE.
   It converts the main s-expression into ROL.
   we check if the reg-len bigger than 0  if so it calls to parse-sexpr-RegL function, else return an error.
|#
(: parse-sexpr : Sexpr -> RegE)
;; to convert the main s-expression into ROL
(define (parse-sexpr sexpr)
(match sexpr
[(list 'reg-len '= (number: n) bitList)
 (if(> n 0)
     (parse-sexpr-RegL bitList n)
     (error 'parse-sexpr "wrong number of bits in ~s" bitList))] ;; remember to make sure specified register length is at least 1
[else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))


#|
  The function receives Sexpr ,Number and returns RegE.
  It converts s-expressions into RegEs.
  it checks if the reg-len equal to length list
  if so it calls to Reg and list->bit-list  function inside
  and converter to bit list, else return an error. or checks if its match to another pattern.

|#
(: parse-sexpr-RegL : Sexpr Number -> RegE)
;; to convert s-expressions into RegEs
(define (parse-sexpr-RegL sexpr reg-len)
(match sexpr
[(list (and a (or 1 0)) ... )
 (if(equal? reg-len (length a))
     (Reg(list->bit-list a))
     (error 'parse-sexpr "wrong number of bits in ~s" a))]

[(list 'and reg1 reg2 ) (And(parse-sexpr-RegL reg1 reg-len)(parse-sexpr-RegL reg2 reg-len))]
[(list 'or reg1 reg2) (Or(parse-sexpr-RegL reg1 reg-len)(parse-sexpr-RegL reg2 reg-len))]
[(list 'shl regExp) (Shl(parse-sexpr-RegL regExp reg-len))]
[else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))


(: parse : String -> RegE)
;; parses a string containing a RegE expression to a RegE AST
(define (parse str)
(parse-sexpr (string->sexpr str)))

;; tests
(test (parse "{ reg-len = 4 {1 0 0 0}}") => (Reg '(1 0 0 0)))
(test (parse "{ reg-len = 4 {shl {1 0 0 0}}}") => (Shl (Reg '(1 0 0 0))))
(test (parse "{ reg-len = 4 {and {shl {1 0 1 0}} {shl {1 0 1 0}}}}") => (And (Shl (Reg '(1 0 1 0))) (Shl (Reg '(1 0 1 0)))))
(test (parse "{ reg-len = 4 { or {and {shl {1 0 1 0}} {shl {1 0 0 1}}} {1 0 1 0}}}") => (Or (And (Shl (Reg '(1 0 1 0))) (Shl (Reg '(1 0 0 1)))) (Reg '(1 0 1 0))))
(test (parse "{ reg-len = 2 { or {and {shl {1 0}} {1 0}} {1 0}}}") => (Or (And (Shl (Reg '(1 0))) (Reg '(1 0))) (Reg '(1 0))))
(test (parse "{ reg-len = 4 {or {1 1 1 1} {0 1 1}}}") =error> "wrong number of bits in")
(test (parse "{ reg-len = 0 {}}") =error> "wrong number of bits in")
(test (parse "{ reg = 4 {or {1 1 1 1} {0 1 1}}}") =error> "bad syntax in")
(test (parse "{ reg = 4 {+ {1 1 1 1} {0 1 1 1}}}" ) =error> "bad syntax in")
(test (parse "{ reg = 4 {mul {1 1 1} {0 1 1}}}" ) =error> "bad syntax in")
(test (parse "{ reg = 3 {and {1 1 1}}}" ) =error> "bad syntax in")
(test (parse "{ reg = 1 {}}" ) =error> "bad syntax in")
(test (parse "{ reg = 8 5}" ) =error> "bad syntax in")
(test (parse "{ reg-len = 4 {and { 1 2 0 1} {0 2 1 1}}}") =error> "bad syntax in")


#|

  Q1.C:




--------------------------------------------------------------------------------------------------------
1. reg-len = 4 {shl {1 0 1 0}}}  :

       |
       | --> reg-len 
       |
       | -->  =      
<ROL> :|
       | -->  <num>  --> 4
       |
       | --> <RegE>  --> shl
       |             
       |             --> <RegE>  --> Reg
       |
       |                         --> <Bits> --> 1
       |
       |                                    --> <Bits>  --> 0
       |
       |                                                --> <Bits>  --> 1
       |
       |                                                            --> <Bits>  --> 0


-----------------------------------------------------------------------------------------------------------

2. reg-len = 2 {or {1 1} {0 1}}}   :

       |
       | --> reg-len 
       |
       | -->  =      
<ROL> :|
       | -->  <num>  --> 2
       |
       | --> <RegE>  --> or
       |             
       |             --> <RegE>  --> Reg
       |
       |                         --> <Bits> --> 1
       |
       |                                    --> <Bits>  --> 1
       |
       |
       |                                                           
       |             --> <RegE>  --> Reg
       |
       |                         --> <Bits> --> 0
       |
       |                                    --> <Bits>  --> 1
       |
       |                                               


--------------------------------------------------------------------------------------------------------------

3. reg-len = 2 { or {and {shl {1 0}} {1 0}} {1 0}}}  :


       |
       | --> reg-len 
       |
       | -->  =      
<ROL> :|
       | -->  <num>  --> 2
       |
       | --> <RegE>  --> or
       |             
       |             --> <RegE> --> and
       |                
       |                        --> <RegE> --> shl 
       |
       |                                   --> <RegE> --> Reg
       |
       |                                              --> <Bits> --> 1
       |                                                           
       |                                                         --> <Bits> --> 0
       |
       |                         --> <RegE> --> Reg
       |
       |                                    --> <Bits> --> 1
       |                                                           
       |                                               --> <Bits> --> 0
       |
       |             --> <RegE> --> Reg
       |
       |                        --> <Bits> --> 1
       |                                                           
       |                                   --> <Bits> --> 0
       |
       |                                    
       | 

-------------------------------------------------------------------------------------------------------------


|#


#|
  Q2:
  1.a : After about 15 minutes, I realized what the problem was with the help of examples given in the assignment (valid|invalid).
  1.b: I used the additional explanations given and from there I took the answer.
  2: It took me several days to solve the question. I did not understand the question.
     After the additional explanations are given in the assignment and after a friend explained the question to me, I understood what to do.
     The writing "MAE, AE, GET-AE" was clear except "SET".
     I tried to write "SET" more narrowly but I couldn't so I wrote it explicitly and in a long way.
  2.1:In the section I had to write 3 different MAE expressions and derive them:
      The derivation itself was simple and took me a few minutes but because I had not yet learned Automatic 2 I needed direction from a friend how to do the derivation. 




 1.a. Basically there are 2 problems:
      First problem - "get" is activate before "set" and according to the examples in this assignment it is invalid.
      Second problem - the two "set" are on the same "level" ({}) so it is unclear which "set" happens first and this affects the "get".
      for example:  set1 --> set2 --> get      get =set2
                    set2 --> set1 --> get      get =set1


 1.b. Each expression will have a number of actions like AE but there will only be one "set" action that appears as the leftmost action. (The idea is taken from section 5 of "Rules for writing the BNF" in the assignment).
      In addition, maybe you can just add more brackets and it will be clear which SET happens first.

 2.
     <AE> ::=  <num> 
             | {+ <AE> <AE>}
             | {- <AE> <AE>} 
             | {* <AE> <AE>} 
             | {/ <AE> <AE>}


    <GET-AE> ::=  <num>
                 | {get}
                 | {+ <GET-AE> <GET-AE>}
                 | {- <GET-AE> <GET-AE>} 
                 | {* <GET-AE> <GET-AE>} 
                 | {/ <GET-AE> <GET-AE>}



   ;;options for the start sequence

   <MAE> ::=   {seq <SET>}
              | {seq <AE>}

                                                   
    ;;options for different set                    ;; examples:
    <SET> ::=   {set <AE>}                         ;; {set {+ 8 7}}         
              | {set <GET-AE>}                     ;; {set {* {* get get} {* get get}}}
              | {{set <GET-AE>} <SET>}             ;; {{set {+ get 7}} {set {+ get get}}}   --> Of course this is only valid as part of a larger phrase
              | {{set <AE>} <SET> <GET-AE> }       ;; {set {+ 8 7}} {set {* get get}} {/ get 2}}
              | {{set <AE>} <GET-AE>}              ;; {set {+ 8 {* 6 5}} {/ get 2}}


----------------------------------------------------------------------------------------------------------------------------------------------------------

   MAE expressions :

  1. {seq {set {+ 20 932}}{set {* get get}} {/ get 73}}

     <MAE> --> {seq <SET>} --> {seq {{set <AE>} <SET> <GET-AE> }} --> {seq {{set <AE>} {set <GET-AE>} <GET-AE> }}

     --> {seq {set{+ <AE> <AE>} {set{* <GET-AE> <GET-AE>}} {/ <GET-AE> <GET-AE>} }}

     --> {seq {{set{+ <num> <num>}} {set{* get get}} {/ get <num>}}}

     --> {seq {{set{+ 20 932}} {set{* get get}} {/ get 73} }}
    


 2. {seq {set {+ 209 {* 32 60}}} {/ get 73}}

    <MAE> --> {seq <SET>} --> {seq {{set <AE>} <GET-AE>}}

    --> {seq {{set {+ <AE> <AE>}} {/ <GET-AE> <GET-AE>}}} --> {seq {{set {+ <AE> {* <AE> <AE>}}} {/ <GET-AE> <GET-AE>}}}

    --> {seq {{set {+ <num> {* <num> <num>}}} {/ <get> <num>}}}

    --> {seq {{set {+ 209 {* 32 60}}} {/ <get> 73}}}



 3. {seq {- {/ 20 93} 260 }}

    <MAE> --> {seq <AE>} --> {seq {- <AE> <AE> }} --> {seq {- {/ <AE> <AE>} <AE> }}

    --> {seq {- {/ <num> <num>} <num> }}

    --> {seq {- {/ 20 93} 260 }}


|#






#|
   Q3:
   
   The function sum-of-squares receives a list of numbers and returns the sum of the squared numbers in the list.
   In this function I used foldl ,map and square functions.
   This function has an internal function called square.

   The function square receives a number and returns a number.
   The function calculates squared of the number.

 
|#

( : sum-of-squares : (Listof Number) -> Number)
(define (sum-of-squares numberList)
  ( : square : Number -> Number)
  (define (square number)
  (* number number))
  (foldl + 0 (map square numberList)))


;;tests - sum-of-squares
(test (sum-of-squares '(5)) => 25)
(test (sum-of-squares '(7)) => 49)
(test (sum-of-squares '(-3)) => 9)
(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '(4 8)) => 80)
(test (sum-of-squares '()) => 0)
(test (sum-of-squares '(1 2 3 4)) => 30)



#|
  Q4:


|#



#|
   We define a binary tree as a something that is either a Leaf holding a number,
   or a Node that contains a binary tree on the left and one on the right. 
|#

(define-type BINTREE
  [Node BINTREE BINTREE]
  [Leaf Number])



#|
   The function takes in a numeric function f and a binary tree, and returns a tree with the same shape but using f(n) for values in its leaves.
   The function calls the constructors:
   If it is Leaf - Calls the constructor Leaf and activates the function on the number.
   If it is a Node - the Node constructor sents more nodes to it in a recursive way
|#
( : tree-map : (Number -> Number) BINTREE -> BINTREE)
(define (tree-map function tree)
  (cases tree
    [(Leaf number) (Leaf (function number))]
    [(Node bintree1 bintree2)(Node (tree-map function bintree1)(tree-map function bintree2))]))



;;tests - tree-map
(test (tree-map add1 (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))) => (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))
(test (tree-map sub1 (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))) => (Node (Leaf 0) (Node (Leaf 1) (Leaf 2))))
(test (tree-map abs (Node (Leaf -1) (Node (Leaf -2) (Leaf -3)))) => (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))))
(test (tree-map add1 (Leaf 1)) => (Leaf 2))
(test (tree-map sub1 (Leaf -5)) => (Leaf -6))
(test (tree-map abs (Leaf -9)) => (Leaf 9))
(test (tree-map add1 (Node (Node (Leaf 2) (Leaf 3)) (Node (Leaf 2) (Leaf 3)))) => (Node (Node (Leaf 3) (Leaf 4)) (Node (Leaf 3) (Leaf 4))))
(test (tree-map sub1 (Node (Node (Leaf 2) (Leaf 3)) (Node (Leaf 2) (Leaf 3)))) => (Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 1) (Leaf 2))))
(test (tree-map abs (Node (Node (Leaf -2) (Leaf -3)) (Node (Leaf -2) (Leaf -3)))) => (Node (Node (Leaf 2) (Leaf 3)) (Node (Leaf 2) (Leaf 3))))





#|
  
  The function recives three values: 
  the combiner function (a function of two arguments), the leaf function (a function of a single number argument), and the BINTREE value to process
  and returns the value after the function has been operated .
  tree-fold is a polymorphic function.

|#

(: tree-fold : (All (A) (A A -> A) (Number -> A) BINTREE -> A ))
(define (tree-fold function leafF tree)
  (cases tree
    [(Leaf number) (leafF number)]
    [(Node bintree1 bintree2) (function (tree-fold  function leafF bintree1) (tree-fold function leafF bintree2)) ]))




(: tree-flatten : BINTREE -> (Listof Number)) ;; flattens a binary tree to a list of its values in
;; left-to-right order
(define (tree-flatten tree) (tree-fold (inst append Number) (inst list Number) tree))


;;tests - tree-fold
(test (tree-flatten (Node (Leaf 4) (Node (Leaf 2) (Leaf 8)))) => (list 4 2 8))
(test (tree-fold + sub1 (Node (Leaf 1)(Leaf 3))) => 2)
(test (tree-fold + add1 (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))) => 9)
(test (tree-fold + abs (Node (Leaf -1) (Node (Leaf -4) (Leaf -7)))) => 12)
(test (tree-fold - sub1 (Node (Leaf 4) (Node (Leaf 6) (Leaf 3)))) => 0)
(test (tree-fold - add1 (Node (Leaf 0) (Node (Leaf -5) (Leaf 3)))) => 9)
(test (tree-fold - abs (Node (Leaf -1) (Node (Leaf -4) (Leaf -7)))) => 4)
(test (tree-fold * sub1 (Node (Leaf 4)(Leaf 3))) => 6)
(test (tree-fold / add1 (Node (Leaf 7)(Leaf 3))) => 2)
(test (tree-fold + add1 (Leaf 5)) => 6)
(test (tree-fold - sub1 (Leaf 9)) => 8)




#|
  The function receives a tree and returns a tree that is its mirror image.
  The function also uses 2 functions:
  1. tree-fold - I have already exercised upstairs
  2. Auxiliary function - switch-nodes which switches between BINTREEs.
|#

(: tree-reverse : BINTREE -> BINTREE)
(define (tree-reverse bintree)
   (tree-fold switch-nodes Leaf bintree))


#|
  The function receives 2 BINTREE and returns a BINTREE.
  The function switches between the trees in Node.

|#
(: switch-nodes : BINTREE BINTREE -> BINTREE)
(define (switch-nodes bintree1 bintree2)
  (Node bintree2 bintree1))


;;tests - tree-reverse , switch-nodes 
(test (reverse (tree-flatten (Node (Leaf 4)(Leaf 3)))) => (tree-flatten(tree-reverse(Node (Leaf 4)(Leaf 3)))))
(test (reverse (tree-flatten (Leaf 5))) => (tree-flatten(tree-reverse(Leaf 5))))
(test (reverse (tree-flatten (Node (Leaf 5) (Node (Leaf 6) (Leaf 7))))) => (tree-flatten(tree-reverse(Node (Leaf 5) (Node (Leaf 6) (Leaf 7))))))
(test (tree-reverse(Node (Leaf 8) (Node (Leaf 1) (Leaf 5)))) => (Node (Node(Leaf 5)(Leaf 1))(Leaf 8)))
(test (tree-reverse(Node (Leaf 0)(Leaf -4))) => (Node (Leaf -4)(Leaf 0)))
(test (tree-reverse(Leaf 5)) => (Leaf 5))
(test (switch-nodes(Leaf 5) (Leaf 2)) =>(Node(Leaf 2) (Leaf 5)))
(test (switch-nodes (Node (Leaf 8) (Node (Leaf 1) (Leaf 5)))(Leaf 2)) => (Node(Leaf 2)(Node (Leaf 8) (Node(Leaf 1)(Leaf 5)))))















(test (parse "{ reg-len = 4 {1 0 0 0}}") => (Reg '(1 0 0 0)))

(test (parse "{ reg-len = 4 {shl {1 0 0 0}}}") => (Shl (Reg '(1 0 0 0))))

(test (parse "{ reg-len = 4 {and {shl {1 0 1 0}} {shl {1 0 1 0}}}}") => (And (Shl (Reg '(1 0 1 0))) (Shl (Reg '(1 0 1 0)))))

(test (parse "{ reg-len = 4 {or {and {shl {1 0 1 0}} {shl {1 0 0 1}}} {1 0 1 0}}}") => (Or (And (Shl (Reg '(1 0 1 0))) (Shl (Reg '(1 0 0 1)))) (Reg '(1 0 1 0))))

(test (parse "{ reg-len = 2 {or {and {shl {1 0}} {1 0}} {1 0}}}") => (Or (And (Shl (Reg '(1 0))) (Reg '(1 0))) (Reg '(1 0))))

(test (parse "{ reg-len = 4 {or {1 1 1 1} {0 1 1}}}") =error> "wrong number of bits in")

(test (parse "{ reg-len = 10 {1 0 0 0 1 1 1 1 1 1}}") => (Reg '(1 0 0 0 1 1 1 1 1 1)))

(test (parse "{ reg-len = 3 {and {shl {1 0 1}} {shl {1 0 1}}}}") => (And (Shl (Reg '(1 0 1))) (Shl (Reg '(1 0 1)))))

(test (parse "{ reg-len = 3 {or {shl {1 0 1}} {shl {1 0 1}}}}") => (Or (Shl (Reg '(1 0 1))) (Shl (Reg '(1 0 1)))))

(test (sum-of-squares '(1 2 3)) => 14)

(test (sum-of-squares '(1)) => 1)

(test (sum-of-squares '(1.1 2 3)) => 14.21)

(test (sum-of-squares '(-1 2 3)) => 14)

(test (tree-map add1 (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))) => (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))

(test (tree-map exp (Node (Leaf 0) (Leaf 0))) => (Node (Leaf 1) (Leaf 1)))

(test (tree-map sin (Leaf 0)) => (Leaf 0))

(test (tree-flatten (Leaf 0)) => '(0))

(test (tree-flatten (Node (Leaf 2) (Leaf 3))) => '(2 3))

(test (tree-reverse (Node (Leaf 2) (Leaf 3))) => (Node (Leaf 3) (Leaf 2)))

(test (tree-reverse (Leaf 0)) => (Leaf 0))

(test (tree-reverse (Node (Leaf 2.1) (Leaf 3.1))) => (Node (Leaf 3.1) (Leaf 2.1)))

