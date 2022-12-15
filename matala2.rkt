#lang pl 2
#|=================Q1==================|#
#|
 <SE> ::= <String>
         | <Chars>
         | <Num>
 
 <D> ::= 0|1|2|3|4|5|6|7|8|9 - one of 10 options
 
 <Chars> :: = #\<D> | #\<D> <Chars> - #\2 #\9
 
 <Num> ::= <D>
          | <D> <Num>
          |{string-length <String> }- can represent the string length
 
 <String> ::= <Num>
             |{ string-append <String> <String>} - (string-append(#\2 #\2) "0")
             |{ string-append <String> <Chars>}  - (string-append("2066" #\8)
             |{ string-append <String>}          
             |{ string-insert <String> <Chars> <Num>}- (string-insert "2066" #\8 4)
             |{ number->string <Num>} - string length

 =========part b=========

 ( string-append "45" ( number->string ( string-length "003" ) ))
 <SE> = > <String>
        > < {string-append <String> <String>}
        > < {string-append <Num> <String>}
        > < {string-append <D> <String>}
        > < {string-append "4<D>" <String>}
        > < {string-append "45" <String>}
        > < {string-append "45" number->string <Num>}
        > < {string-append "45" number->string ( string-length <String>)}
        > < {string-append "45" number->string ( string-length "003")}
      
 

 ( number->string ( string-length "0033344" ) )
 <SE> = > <String>
        > {number->string (<Num>)}
        > {number->string ( string-length <String> )}
        > {number->string ( string-length "0033344")}
     

 (string-length "2066")
 <SE> = > <String>
        > <Num>
        > {string-length <String>}
        > {string-length "2066"}
    

|#



#|================Q2==================|#

#|
we were asked to write a sum of square function which takes a list of numbers as input
and produces a number which is the sum of the squares of all of the numbers in the list
for that we built the function 'sqr' which receives a number as input and returns its square.
we also used the 'foldl' which generalizes some iteration functions. It uses the
per-element function to both process an element and combine it with the “current” value,
so the per-element function takes an extra first argument.
Also, a starting “current” value must be provided before the lists.
|#
(: sqr : Number -> Number)
(define (sqr x) (* x x))

(: sum_of_square : (Listof Number) -> Number)
(define (sum_of_square lst)
(foldl + 0 (map sqr lst)))

;======tests=======
(test (sum_of_square '(1 2 3)) => 14)
(test (sum_of_square '(0 0 0)) => 0)
(test (sum_of_square '(0 1 2)) => 5)
(test (sum_of_square '(10 10 10 10)) => 400)



#|================Q3==================|#

 
#|createPolynomial - this func takes a list of Numbers, buiding a polydrom and in the and giving the total sum of the calc of the polydrom.|#
#|
 poly : x - a given number , power -a power value that is raised in each round by 1,
accum - and the accumulator that will hold the final value for us at each stage
if we will get an empty list we will return the accum value, else we will use a tail recursion on the rest of the list
that the pow value will up in 1 every time
|#


#|3.a|#
(: createPolynomial : (Listof Number) -> (Number -> Number)) 
(define (createPolynomial coeffs) 
  (: poly : (Listof Number) Number Integer Number -> Number) 
  (define (poly argsL x power accum)
    (if (null? argsL) accum
        (poly (rest argsL) x (+ 1 power)
              (+ accum (* (first argsL) (expt x power))))))
  (: polyX : Number -> Number)
  (define (polyX x)
    (poly coeffs x 0 0))
    polyX)

 
(define p2345 (createPolynomial '(2 3 4 5))) 
(test (p2345 0) =>  
   (+ (* 2 (expt 0 0)) (* 3 (expt 0 1)) (* 4 (expt 0 2)) (* 5 
(expt 0 3)))) 
(test (p2345 4) =>  
   (+ (* 2 (expt 4 0)) (* 3 (expt 4 1)) (* 4 (expt 4 2)) (* 5 (expt 4 3)))) 
(test (p2345 11) => (+ (* 2 (expt 11 0)) (* 3 (expt 11 1)) (* 4 (expt 11 2)) (* 5 (expt 11 3)))) 
 
 
(define p536 (createPolynomial '(5 3 6))) 
(test (p536 11) => (+ (* 5 (expt 11 0)) (* 3 (expt 11 1)) (* 6 
(expt 11 2)))) 
 
(define p_0 (createPolynomial '())) 
(test (p_0 4) => 0)




#|3.b.i|#

#| 
  The grammar:
    
    <PLANG> ::= {{poly <AEs>} {<AEs>}}
    
    <AEs>   ::=  <AE>
               | <AE> <AEs>
    
    <AE>    ::=  <num>
               | {+ <AE> <AE>}
               | {- <AE> <AE>}
               | {* <AE> <AE>}
               | {/ <AE> <AE>}
  |#


#|3.b.ii|#
#|we were asked to fill in the blink in the "parse" func
 we fill by match conditions func:
 condition1 - if the list is empty.
 condition2 - if we have a list but don't have the next object.
 condition3 - if what we have is right and we have all the object that we need,work on the first and then add the rest of the list.
 else - all the conditions beside the conditions that were mentioned.
 |#
(define-type PLANG 
    [Poly (Listof AE) (Listof AE)]) 
 
  (define-type AE 
    [Num  Number] 
    [Add  AE AE] 
    [Sub  AE AE] 
    [Mul  AE AE] 
    [Div  AE AE]) 
 
  (: parse-sexpr : Sexpr -> AE) 
  ;; to convert s-expressions into AEs 
  (define (parse-sexpr sexpr) 
    (match sexpr 
      [(number: n)    (Num n)] 
      [(list '+ lhs rhs) (Add (parse-sexpr lhs)(parse-sexpr rhs))] 
      [(list '- lhs rhs) (Sub (parse-sexpr lhs)(parse-sexpr rhs))] 
      [(list '* lhs rhs) (Mul (parse-sexpr lhs)(parse-sexpr rhs))] 
      [(list '/ lhs rhs) (Div (parse-sexpr lhs)(parse-sexpr rhs))] 
      [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

  (: parse : String -> PLANG) 
  ;; parses a string containing a PLANG expression to a PLANG AST
(define (parse str) 
    (let ([code (string->sexpr str)]) 
      (match code
      [(list (cons 'poly '()) (list rest ...)) (error 'parse "at least one coefficient is required in ~s" code)]
      [(list (cons 'poly first) '()) (error 'parse "at least one point is required in ~s" code)]
      [(list (cons 'poly first) (list rest ...)) (Poly (map parse-sexpr first) (map parse-sexpr rest))]
      [else (error 'parse "bad syntax in ~s" code)]))) 

 
(test (parse "{{poly 1 2 3} {1 2 3}}") => (Poly (list (Num 1) (Num 2) (Num 3)) (list (Num 1) (Num 2) (Num 3))))
(test (parse "{{poly } {1 2} }") =error> "parse: at least one coefficient is required in ((poly) (1 2))") 
(test (parse "{{poly 1 2} {} }") =error> "parse: at least one point is required in ((poly 1 2) ())")
 

 


#|3.b.iii|#
;; evaluates AE expressions to numbers
#|we want to create an eval-poly to live the AE eval untouched
 so we want to create a polydrom by using the createPolynomial func
 but createPolynomial func need to get a list of numbers -
 to give that we use a map whit eval on the r and the l
|#
(: eval : AE -> Number)
(define (eval expr) 
  (cases expr 
      [(Num n) n] 
      [(Add l r) (+ (eval l) (eval r))] 
      [(Sub l r) (- (eval l) (eval r))] 
      [(Mul l r) (* (eval l) (eval r))] 
      [(Div l r) (/ (eval l) (eval r))]))


  (: eval-poly : PLANG -> (Listof Number)) 
  (define (eval-poly p-expr) 
    (cases p-expr
      [(Poly r l)(map(createPolynomial
                      (map eval r))(map eval l))]))
 
  (: run : String -> (Listof Number)) 
  ;; evaluate a FLANG program contained in a string 
  (define (run str) 
    (eval-poly (parse str)))

(test (run "{{poly 1 2 3} {1 2 3}}") => '(6 17 34)) 
(test (run "{{poly 4 2 7} {1 4 9}}") => '(13 124 589)) 
(test (run "{{poly 1 2 3} {1 2 3}}") => '(6 17 34)) 
(test (run "{{poly 4/5 } {1/2 2/3 3}}") => '(4/5 4/5 4/5)) 
(test (run "{{poly 2 3} {4}}")  => '(14)) 
(test (run "{{poly 1 1 0} {-1 3 3}}") => '(0 4 4))  
(test (run "{{poly {/ 4 2} {- 4 1}} {{- 8 4}}}") => '(14)) 
(test (run "{{poly {+ 0 1} 1 {* 0 9}} {{- 4 5} 3 {/ 27 9}}}") => '(0 4 4)) 

