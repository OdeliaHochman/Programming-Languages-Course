#lang pl




#|
   Q1:

   The function receives a list of strings ,search for the first string that has a suffix "pl" and returns that string. 
   The function checks the first string from the list.
   If the string contains the suffix then the function returns that string.
   Otherwise, it returns to the recursion function the rest of the list that does not include the first string.
   If we reach the end of the list and the string is not found, the function returns false.
|#

(: plSuffixContained : (Listof String) -> (U String Boolean))
(define(plSuffixContained stringList)
  (if( null? stringList)
     #f
     (if(and (>=(string-length(first stringList)) 2)( equal?(substring(first stringList)(-(string-length(first stringList)) 2)) "pl"))
       (first stringList)                                                  
       ( plSuffixContained (rest stringList)))))


;;test - plSuffixContained
(test (plSuffixContained '("pllp" "plyy" "ppp" "lpTT" "lol")) => false)
(test (plSuffixContained '("")) => false)
(test (plSuffixContained '("abcp" "p" "plnm" "nnplpl")) => "nnplpl")
(test (plSuffixContained '("abc" "def" "minkm" "plkim" "cvbrhp")) => false)
(test (plSuffixContained '("plnmg" "plplp" "mkbpl")) => "mkbpl")
(test (plSuffixContained '("p" "l" "pl")) => "pl")
(test (plSuffixContained '("aaa" "" "bbb" "jkpl" "")) => "jkpl")

 (test (plSuffixContained '("plll" "yyplyy" "pppl" "lpTT" "lol")) => "pppl")


#|
   Q2.1:


   The write-poly function receives a list of numbers and returns a string (polynom).
   This function use tail-recursion - helper function. The helper function receives string,number and list of numbers and returns string.
   The helper function consumes a list of coefficients (numbers) 𝑎1,𝑎2,…,𝑎𝑛 and returns the polynomial (in a reversed order of coefficients) "𝑎1𝑥𝑛+𝑎2𝑥𝑛−1+⋯+𝑎𝑛".
   If the list has only zeros then the string("poly") will be 0. If the list is empty poly will be "".
  
|#

(: write-poly : (Listof Number) -> String)
(define(write-poly list)
  (: helper : String Number (Listof Number) -> String)
  (define(helper poly lenList listHelper)
    (if(null? listHelper)
      (if(equal? "" poly)
         "0"
         (if(equal?(substring poly 0 1) "+")
           (substring poly 1)
           poly))
     (helper (string-append poly(monom (first listHelper) (-(length listHelper)1)))(-(length listHelper) 1) (rest listHelper))))
  (if(null? list)
     ""
  (helper "" (length list) list)))
 

#|
    This function is called from the helper function.
    The function receives two numbers (a number from the beginning of the list and a number that represent the power) and returns a string.
    The function checks for conditions such as: positive or negative number, which power etc. and then creates a monom string
    (+/- num x ^ power).
|#
(: monom : Number Number -> String)
(define(monom num power)  
  (cond
    [(and(> power 1)(> num 0)) (string-append "+" (number->string num) "x^" (number->string power)) ]
    [(and(= power 1)(> num 0)) (string-append "+" (number->string num) "x")]
    [(and(= power 0)(> num 0)) (string-append "+" (number->string num))]
    [(and(> power 1)(< num 0)) (string-append (number->string num) "x^" (number->string power)) ]
    [(and(= power 1)(< num 0)) (string-append (number->string num) "x")]
    [(and(= power 0)(< num 0)) (string-append (number->string num))]
    [else ""]))


  

;;tests - write-poly
(test (write-poly '(3 6)) => "3x+6")
(test (write-poly '(3 2 6)) => "3x^2+2x+6")
(test (write-poly '()) => "")
(test (write-poly '(7 8 9 10)) => "7x^3+8x^2+9x+10")
(test (write-poly '(0 0 0 0)) => "0")
(test (write-poly '(0 1 2 3)) => "1x^2+2x+3")
(test (write-poly '(1 0 2 3)) => "1x^3+2x+3")
(test (write-poly '(1 2 0 3)) => "1x^3+2x^2+3")
(test (write-poly '(1 2 3 0)) => "1x^3+2x^2+3x")
(test (write-poly '(7 0 0 0)) => "7x^3")
(test (write-poly '(0 8 0 0)) => "8x^2")
(test (write-poly '(0 0 9 0)) => "9x")
(test (write-poly '(0 0 0 6)) => "6")
(test (write-poly '(-6 2 3 8)) => "-6x^3+2x^2+3x+8")
(test (write-poly '(6 -2 3 8)) => "6x^3-2x^2+3x+8")
(test (write-poly '(6 2 -3 8)) => "6x^3+2x^2-3x+8")
(test (write-poly '(6 2 3 -8)) => "6x^3+2x^2+3x-8")
(test (write-poly '(6 -2 -3 0)) => "6x^3-2x^2-3x")




#|
   Q2.2:
 
   The function receives a number and a list of numbers and returns number.
   This function use tail-recursion - computeHelper function.
   The computeHelper function receives two numbers and a list of numbers and returns number.
   The computeHelper function take the first organ from the list, calculates the "monom" for a given number and summarize it in argument sum.  
   I used function -"expt" to compute the monom
  (firstOrganList * givenNumber^ power).To understand the expt function I used the racket site.
   power = length list-1 in recursion.
   When the list is empty the function returns arg sum.
   if the given number is 0 the function return 0.

|#
(: compute-poly : Number (Listof Number)-> Number)
(define (compute-poly num numList )
  (: computeHelper : Number Number (Listof Number)-> Number )
  (define(computeHelper number sum numbList)
    (if(null? numbList)
       sum
       (if(= number 0)
          0
          (computeHelper number (+ sum (* (first numbList) (expt number (-(length numbList) 1)))) (rest numbList)))))
  (if(null? numList)
     0
     (computeHelper num 0 numList)))
  


;;tests - compute-poly
(test (compute-poly 2 '()) => 0)
(test (compute-poly 2 '(3 2 6)) => 22)
(test (compute-poly 3 '(4 3 -2 0)) => 129)
(test (compute-poly 0 '(1 2 3 4)) => 0)
(test (compute-poly 4 '(0 0 0 0)) => 0)
(test (compute-poly 7 '(0 2 0 2)) => 100)
(test (compute-poly 5 '(1 0 4 0)) => 145)
(test (compute-poly 6 '(-5 0 0 7)) => -1073)
(test (compute-poly 1 '(0 -3 2 8)) => 7)
(test (compute-poly 9 '(4 5)) => 41)
(test (compute-poly 3 '(5)) => 5)
 


#|
   Q3:
   In this question, I was assisted by lectures, exercises and internet.

|#

#|
   Q3.1:

   A new type was define called KeyStack.
   Each element in the stack will be keyed with a symbol ,string and keyStack.
   (symbol=key , string =value , keyStack = EmptyKS or KeyStackPush).
   There are two constructors:
   EmptyKS - doesn't receive any arguments.
   KeyStackPush- receives Symbol,String and KeyStack.
|#
(define-type KeyStack
  [EmptyKS]
  [KeyStackPush Symbol String KeyStack])

#|
   Q3.2:
  
   The Push operation basically calls for the constructor - KeyStackPush.
   The operation receives symbol ,string , keyStack and returns extended keyStack.
|#

(: Push : Symbol String KeyStack -> KeyStack)
(define (Push symb str keySt)
  (KeyStackPush symb str keySt))

#|
   Q3.3:

   The search operation receives a symbol (key) and a keyed-stack and return the first (LIFO, last in first out) value that is keyed accordingly.
   If the key does not appear in the original stack, it returns false.

|#
(: search-stack : Symbol KeyStack -> (U String #f))
(define (search-stack symb keySt)
  (cases keySt
    [(KeyStackPush symbo str keySta)(if(equal? symb symbo) str (search-stack symbo keySta))]
    [else #f]))

#|
   Q3.4:
 
   The pop operation receives keyStack and 
   returns - keyStack (without its first keyed) or false if the original stack was empty. 
|#

(: pop-stack : KeyStack -> (U KeyStack #f))
(define (pop-stack kStack)
  (cases kStack
    [(KeyStackPush symb str keySt) keySt]
    [else #f]))


;;tests - EmptyKS, Push, search-stack, pop-stack
(test (EmptyKS) => (EmptyKS))
(test (Push 'b "B" (Push 'a "A" (EmptyKS))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))) => (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))
(test (Push 'c "CCC" (Push 'h "H" (EmptyKS))) => (Push 'c "CCC" (Push 'h "H" (EmptyKS))))
(test (search-stack 'a (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => "AAA")
(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => #f)
(test (search-stack 'm (Push 'j "JJJ" (Push 's "SS" (Push 'z "Z" (EmptyKS))))) => #f)
(test (search-stack 'b (EmptyKS)) => #f)
(test (pop-stack (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (pop-stack (Push 'g "G"(EmptyKS))) => (EmptyKS))
(test (pop-stack (Push 'a "AAA" (Push 'b "B"(EmptyKS)))) =>(Push 'b "B"(EmptyKS)))
(test (pop-stack (EmptyKS)) => #f)



 
 

#|
   Q4:

|#


;functions "is-odd?" and "is-even?" use each other.

(: is-odd? : Natural -> Boolean)
#|
The function receives a natural number and returns boolean.
The function checks if the number is odd.
if the natural number is zero then it returns false, otherwise, it calls to "is-even?" function and sends to him the natural number minus one.
|#
(define (is-odd? x)
  (if (zero? x)
      false
      (is-even? (- x 1))))

(: is-even? : Natural -> Boolean)
#|
The function receives a natural number and returns boolean.
The function checks if the number is even.
if the natural number is zero then it returns true, otherwise, it calls to "is-odd?" function and sends to him the natural number minus one.
|#
(define (is-even? x)
  (if (zero? x)
      true
      (is-odd? (- x 1))))

;; tests --- is-odd?/is-even?
(test (not (is-odd? 12)))
(test (is-even? 12))
(test (not (is-odd? 0)))
(test (is-even? 0))
(test (is-odd? 1))
(test (not (is-even? 1)))

(: every? : (All (A) (A -> Boolean) (Listof A) -> Boolean))
#|
This function is like a template type.
function every? - receives 2 arguments:
The first argument is a function from A to boolean (returns boolean).
The second argument is a list of A and returns boolean.
(All A) - indicates to us that the type of A that will be selected in the first argument(function) is also the one that will be selected in the other arguments that have A.
The function checks some boolean conditions on the first organ in the list and recursively returns the rest of the list to the same function.
|#
(define (every? pred lst)
  (or (null? lst)
      (and (pred (first lst))
           (every? pred (rest lst)))))


;; An example for the usefulness of this polymorphic function
(: all-even? : (Listof Natural) -> Boolean)
#|
The function receives a list of natural numbers and returns boolean.
The function checks if all the numbers in the list are even by using a "helper" function - "every?" 
|#
(define (all-even? lst)
  (every? is-even? lst))

;; tests
(test (all-even? null))
(test (all-even? (list 0)))
(test (all-even? (list 2 4 6 8)))
(test (not (all-even? (list 1 3 5 7))))
(test (not (all-even? (list 1))))
(test (not (all-even? (list 2 4 1 6))))

(: every2? : (All (A B) (A -> Boolean) (B -> Boolean) (Listof A) (Listof B) -> Boolean))
#|
The function receives:
1. function from A to boolean (returns boolean).
2. function from B to boolean (returns boolean).
3. list of A and list of B and it returns boolean.
A and B need to be of the same type because "All". 
Length list A equal to length list B.
This function is similar to the function -"every?". 
The difference between "every?" and "every2?" is that the boolean result in function- "every2?"  depends on both lists A and B.
|#
(define (every2? pred1 pred2 lst1 lst2)
  (or (null? lst1) ;; both lists assumed to be of same length
      (and (pred1 (first lst1))
           (pred2 (first lst2))
           (every2? pred1 pred2 (rest lst1) (rest lst2)))))







