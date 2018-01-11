;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname expr-test-S4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; Note: this file is written in advanced-beginner
; (because the test-functions do sequences of I/O, rather than return values).


(require "S4.rkt")
(require "scanner.rkt")
;(require "expr-test-helpers.rkt")
(require rackunit)

;;;;;;;;;;;;;;;;;;; TEST CASES: S0 ;;;;;;;;;;;;;;;;


; Some expressions to test in a non-automated way:
(check-equal? (parse! (create-scanner "34")) 34)
(check-equal? (parse! (create-scanner "-34")) -34)
(check-equal? (parse! "34") 34)
(check-equal? (parse! "|34|") (make-paren 34))
(check-equal? (parse! "?ad 3 4") (make-binop "ad" 3 4))
(check-equal? (parse! "?ad |34| ?mu 3 4")
              (make-binop "ad" (make-paren 34)  (make-binop "mu" 3 4)))
(check-equal? (parse! "(3) !0 7 : 9")
              (make-ifnz 3 7 9))
(check-equal? (parse! "(|34|) !0 ?ad |34| ?mu 3 4 : (3) !0 7 : 9")
              (make-ifnz (make-paren 34)
                         (make-binop "ad" (make-paren 34)  (make-binop "mu" 3 4))
                         (make-ifnz 3 7 9)))

(check-equal? (eval 34) 34)
(check-equal? (eval (parse! "|34|")) 34)
(check-equal? (eval (parse! "?ad 3 4"))  7)
(check-equal? (eval (parse! "?su 3 4")) -1)
(check-equal? (eval (parse! "? mu 3 4")) 12)
(check-equal? (eval (parse! "(3) !0 4 : 5")) 4)
(check-equal? (eval (parse! "(0) !0 4 : 5")) 5)
(check-equal? (eval (parse! "(?su 2 2) !0 ?ad 1 2 : ?ad 3 4")) 7)
(check-equal? (eval (parse! "(0) !0 ?ad 3 4 : 5")) 5)

(check-equal? (expr->string (parse! "34")) "34")
(check-equal? (expr->string (parse! "|34|")) "|34|")
(check-equal? (expr->string (parse! "?ad 3 4")) "?ad 3 4")
(check-equal? (expr->string (parse! "?su 3 4")) "?su 3 4")
(check-equal? (expr->string (parse! "? mu   3   4")) "?mu 3 4")
(check-equal? (expr->string (parse! "(3) !0 4 : 5")) "(3) !0 4 : 5")
(check-equal? (expr->string (parse! "(0) !0 ?ad 3 4 : 5")) "(0) !0 ?ad 3 4 : 5")


(define e0 "43")
(define e1 "|43|")
(define e2 "?ad 4 3")
(define e3 "||?ad 4 |3|||")
(define e4 "?ad |43| ?mu 42 3")

;;; we can automate checking that parse! is the (right)inverses of expr->string:
(for-each (λ(e) (check-equal? (expr->string (parse! e))
                              e))
          (list e0 e1 e2 e3 e4))
; `for-each` is like map except that it discards the result from each function-call;
; it is suitable for functions which are called solely for a side-effect.
; (`test-all` is such a function.)

;;; Though we also want to check that e0..e4 eval to 43,43,7,7,169 respectively.
(for-each (λ(e v) (check-equal? (eval (parse! e)) v))
          (list e0 e1 e2 e3  e4)
          (list 43 43  7  7 169))


;;; The above is a promising start, to automating tests.
;;; Okay, we'll generalize the above to a more complete test-harness.
;;; One thing, is that we don't want to have two parallel-lists;
;;; instead keep a list of pairs.

;;; Three sorts of tests we want to make, for many different exprs:
(check-equal? (parse! "?ad 4 3") (make-binop "ad"  4  3))
(check-equal? (eval (parse! "?ad 4 3")) 7)
(check-equal? (expr->string (parse! "?ad 4 3"))
              "?ad 4 3")




;>>>S2
; some tests involving Id's, since we can't eval those, so don't include in harness:
;
(check-equal? (parse! "x") "x") ; Id
(check-equal? (parse! "? ad numPeople 3") (make-binop "ad" "numPeople" 3))
(check-equal? (expr->string "x") "x") ; Id
(check-equal? (expr->string (make-binop "ad" "numPeople" 3)) "?ad numPeople 3")


; Data Def'n:  a `S-example` is a list of length two or length three:
; '[str val]      (where val is the expected result `(eval (parse! str))`, or
; '[str val expr] (as above, but `expr` is the internal (struct) representation of `(parse! str)`).

; A list of S-examples;
; The last line of this file runs two-or-four tests on each S-example.
;
; BE AWARE of the comma preceding the constructors; it's necessary to actually call it.
; See explanation at http://www.radford.edu/~itec380/2017fall-ibarland/Lectures/backquote.html
;
(define tests
  `{["7" 7 7]
    ["?ad 3 4" 7 ,(make-binop "ad" 3 4)]
    ["?mu 3   4" 12 ,(make-binop "mu" 3 4 )]
    ["?ad ?ad 3  4 ?mu  3   4  " 19]
    ["(0) !0 1 : 2" 2 ,(make-ifnz 0 1 2)]
    ["(1) !0 1 : 2" 1 ,(make-ifnz 1 1 2)]
    ["(?ad 3 -3) !0  1 :  2" 2 ,(make-ifnz (make-binop "ad"  3 -3) 1 2)]
    ["(?ad ( (0) !0 1 : 2 ) !0 3 : 4 -3) !0 1 : 2"
     2
     ,(make-ifnz (make-binop "ad" (make-ifnz (make-ifnz 0 1 2) 3 4) -3) 1 2)]
    
    ;>>>S1
    ["?mo 3.0 4.0" 3]
    ["?mo  ?ad 5.0 6.0  3.0" 2]
    ["?mo 8.1 3.0" 2.1]
    ["?mo 8.0 3.1" 1.8]
    ["?mo -8.1 3.0" 0.9]
    ["?mo -8.0 3.1" 1.3]
    ["?mo 8.1 -3" -0.9]
    ["?mo 8.0 -3.1" -1.3]
    ["?mo -8.1 -3.0" -2.1]
    ["?mo -8.0 -3.1" -1.8]
    ["?mo 8.0   2.0" 0]
    ["?mo -8.0  2.0" 0]
    ["?mo 8.0  -2.0" 0]
    ["?mo -8.0 -2.0" 0]
    ["?mo 8.0   3.0" 2]
    ["?mo -8.0  3.0" 1]
    ["?mo 8.0  -3.0" -1]
    ["?mo -8.0 -3.0" -2]

    ;>>>S1: ifgt
    ["( 3 >? 2 ) = 1 : 0" 1 ,(make-ifgt 3 2 1 0)]  
    ["( 2 >? 3 ) = 1 : 0" 0 ,(make-ifgt 2 3 1 0)]
    ["( ?ad 2 3  >?  (2) !0 3 : 9 ) = ?mu 2 3 : ?ad 3 4" 
     6       
     ,(make-ifgt (make-binop "ad" 2 3) (make-ifnz 2 3 9) (make-binop "mu" 2 3) (make-binop "ad" 3 4))]
    ["( ?ad 2 3  >?  (0) !0 3 : 9 ) = ?mu 2 3 : ?ad 3 4"
     7
     ,(make-ifgt (make-binop "ad" 2 3) (make-ifnz 0 3 9) (make-binop "mu" 2 3) (make-binop "ad" 3 4))]

    ;>>>S2: let
    ["let x 99 in 50" 50 ,(make-let-expr "x" 99 50)]  
    ["let x 99 in x" 99 ,(make-let-expr "x" 99 "x")]
    ["let x 99 in ?su x 9" 90 ,(make-let-expr "x" 99 (make-binop "su" "x" 9))] 
    ["let x 99 in 50" 50 ,(make-let-expr "x" 99 50)]           

    ["let x 99 in let z 9 in ?su x z"
     90
     ,(make-let-expr "x" 99 (make-let-expr "z" 9 (make-binop "su" "x" "z")))]
    ["let x ?mu 33 3 in let z ?ad x 1 in ?mu x z"
     9900
     ,(make-let-expr "x" (make-binop "mu" 33 3)
                     (make-let-expr "z" (make-binop "ad" "x" 1)
                                    (make-binop "mu" "x" "z")))]

    ;>>>S3, taken from hw09, #1a-e.
    ["let y 3 in let x 5 in ?ad x y" 8]
    ["let y 3 in let x y in ?ad x y" 6]
    ["let x 5 in let y 3 in ?ad let x y in ?ad x y x" 11]
    ["let x 5 in |let x ?ad x 1 in ?ad x 2|" 8]
    ["let y let z 4 in |let y 99 in z|       in |let z 5 in ?ad |let z 10 in y| ?ad y z|"  13]
    ["let y let z 4 in |let y 99 in z|       in |let z 5 in ?ad |let z 10 in z| ?ad y z|"  19]
    ["let y let z 4 in |let y 99 in y|       in |let z 5 in ?ad |let z 10 in y| ?ad y z|" 203]
    ["let y let z 4 in |let y 99 in ?ad y z| in |let z 5 in ?ad |let z 10 in y| ?ad y z|" 211]
    ["let x 3 in |let y 4 in ?ad x |let x 5 in ?ad x y||" ,(+ 3 (+ 5 4))]


    ;>>> S4: func-expr, func-apply-expr
    ; the constant function 17
    ["func x -> 17" ,(make-func-expr "x" 17)]
    ["let seventeenF func x -> 17 in seventeenF" ,(make-func-expr "x" 17)]
    ; sqr
    ["func x -> ?mu x x" ,(make-func-expr "x" (make-binop "mu" "x" "x"))]
    ["let sqr func x -> ?mu x x in sqr" ,(make-func-expr "x" (make-binop "mu" "x" "x"))]

    ; make-adder
    ["func n -> func m -> ?ad m n"
     ,(make-func-expr "n" (make-func-expr "m" (make-binop "ad" "m" "n")))]
    ["let makeAdder func n -> func m -> ?ad m n in makeAdder"
     ,(make-func-expr "n" (make-func-expr "m" (make-binop "ad" "m" "n")))]
    
    ; factorial function commented out-- won't work until S6
    ;["let fact func n -> (n) !0 call fact ?su n 1 : 1 in fact"]

    ;>>> S4: func-apply-expr -- the above examples both with and without naming the function
    ; the constant function 17
    ["call func x -> 17(5)" 17]
    ["let seventeenF func x -> 17 in call seventeenF(5)" 17]
    ; sqr
    ["call func x -> ?mu x x(5)" 25]
    ["let sqr func x -> ?mu x x in call sqr(5)" 25]
    
    ; make-adder
    ["call call func n -> func m -> ?ad m n(5)(7)" 12]
    ["let makeAdder func n -> func m -> ?ad m n in call call makeAdder(5)(7)" 12]
    })
;
; For info on backquote, see documentation and/or:
;   http://www.radford.edu/itec380/2017fall-ibarland/Lectures/backquote.html






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; helper functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following functions should really be in a separate file, and exported from there.
;; However, putting this in to 'Si-test-harness.rkt' becomes mildly problematic:
;; since it calls 'eval', 'parse!' as provided in S0, it needs to require S0.rkt.
;; So it would need to be updated for every expr-test-Si.rkt, changing *nothing* but the 'require'.
;; The actual solution would be using "units":
;; http://docs.racket-lang.org/reference/creatingunits.html?q=unit#%28form._%28%28lib._racket%2Funit..rkt%29._unit%29%29
;;
;; But rather than add that level of indirection for a student-assignment, we'll just repeat
;; this code inside each expr-test-Si.rkt.


; my-check-equal? : any, any, string -> boolean
; If the two values aren't equal, print an error message.
; If they are equal (and `print-on-success`), a "." gets printed, just to show progress.
;
(define my-check-equal?
  (local {(define test# 0)
          (define print-on-success #true)}
      (λ (actual expected err-msg)
        (begin (set! test# (add1 test#))
               (if (equal? actual expected)
                   (when print-on-success (printf ".~a" (if (zero? (modulo test# 5)) " " "")))
                   (printf "\ntest #~a failed:\n~a\n" test# err-msg))
               ;(check-equal? actual expected) ; Use `check-equal?` to additionally-but-redundantly manage test cases.
               (equal? actual expected) ; return this boolean
               ))))

; string->tokens : string -> (listof string)
; Given a string, return a list of tokens (for the scanner, as configured).
;
(define (string->tokens str)
  ; We don't just use scheme's built-in `read`, because our string might contain semicolons etc.
  
  ; Make a local helper-function named `convert : scanner -> string`
  ; See also: "named-let"   https://docs.racket-lang.org/guide/let.html#%28part._.Named_let%29
  (letrec {[convert (λ (scnr)
                       (if (eof-object? (peek scnr))
                           empty
                           (cons (pop! scnr) (convert scnr))))]}
                           ; N.B. We RELY on left-to-right eval here:
                           ; the pop happens before looping back.
    (convert (create-scanner str))))
;; TODO Ian: move this to `scanner.rkt`





; test-internal-representation : S-example -> void?
; Test that t parses to the correct internal tree-representation (if provided)
;
(define (test-internal-representation t)
  (when (>= (length t) 3)
    (my-check-equal? (parse! (first t))
                     (third t)
                     (format "Parsing     ~v\nresulted in ~v\ninstead of  ~v\nas expected."
                             (first t) (parse! (first t)) (third t)))))

; test-eval : S-example -> void?
; Test that the S-example `eval`s to what it should.
;
(define (test-eval t)
  (my-check-equal? (eval (parse! (first t)))
                   (second t)
                   (format "Program    ~v\neval'd to  ~v\ninstead of ~v\nas expected."
                           (first t) (eval (parse! (first t))) (second t))))


; test-parse-inverse-of-to-string : S-example -> void?
; Test that `parse` and `expr->string` are inverses of each other:
;    `parse` is a right-inverse: for a string `s`,  (expr->string (parse s)) = s, and
;    `parse` is a left- inverse: for a tree `expr`, (parse (expr->string expr)) = expr.
; Note that spaces between tokens in a string is ignored, so they're not *quite* exact inverses.
;
; Also, other tests are redundant with checking the left-inverse,
; but we still check it to be independent of other code.
;
(define (test-parse-inverse-of-to-string t)
  (begin (my-check-equal? (string->tokens (expr->string (parse! (first t))))
                          (string->tokens (first t))
                          (format "Parsing ~v then converting back to string gave ~v."
                                  (first t) (parse! (first t))))
         (when (>= (length t) 3)
           (my-check-equal? (parse! (expr->string (third t)))
                            (third t)
                            (format "Converting ~v to string and re-parsing it gave ~v."
                                    (third t) (expr->string (third t)))))))


; test-all : S-example -> void?
; Make sure that t meets the following properties:
;   i. Parsing the string results in the expected internal representation (*)
;  ii. Check that parsing the string and then to-string'ing the result
;      gives back the initial string
; iii. Check that to-string'ing the internal representation and then parsing
;      that resulting string gives back the initial internal representation (*)
;  iv. check that eval'ing the (parsed) string gives the expected value.
;
; (*) steps i,iii can only be performed if the S-example contained all three values.
;     If it only contained a string and a value, then only *two* tests get performed.
;     This affects the test-number reported, should a later test fail.
;
(define (test-all t)
  (begin (test-internal-representation t)
         (test-parse-inverse-of-to-string t) ; N.B. counts as two tests
         (test-eval t)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; run the tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(printf "Running S-test-harness:\n")
(for-each test-all tests)  ; This line actually invokes the checker
(printf "\nS-test-harness complete; ~v expressions each tested two-or-four ways.\n" (length tests))
; `for-each` is like map except that it discards the result from each function-call;
; it is suitable for functions which are called solely for a side-effect.
; (`test-all` is such a function.)

