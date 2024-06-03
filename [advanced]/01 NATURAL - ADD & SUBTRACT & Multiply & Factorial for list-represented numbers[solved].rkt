;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |01 NATURAL - ADD & SUBTRACT [solved]|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ================
;; Data Defs

;; NATURAL is one of:
;; - empty
;; - (cons "!" NATURAL)
;; interp. a natural number represented in number of "!" in a list, where 0 is empty
(define N0 empty)
(define N1 (cons "!" N0))
(define N2 (cons "!" N1))
(define N3 (cons "!" N2))
(define N4 (cons "!" N3))
(define N5 (cons "!" N4))
(define N6 (cons "!" N5))
(define N7 (cons "!" N6))
(define N8 (cons "!" N7))

;; These are the primitives that operate NATURAL:
(define (ZERO? n) (empty? n))
(define (ADD1 n) (cons "!" n))
(define (SUB1 n) (rest n))

#;
(define (fn-for-NATURAL n)
  (cond [(ZERO? n) (...)]
        [else
        (... n
       (fn-for-NATURAL (SUB1 n)))]))

;; Template rules used:
;; one of: 2 cases:
;; - atomic non-distinct: boolean/ empty idk
;; - compound: (cons "!" NATURAL)
;; self-reference: (SUB1 n) is NATURAL

;; added n since it is commonly used

;; ===============
;; Functions

;; Function 1: sum two lists (add their members; "!" & prefix the empty as a trigger for the end of the recursive self-referencing function, as entailment of the end of the list.. ya know

;; NATURAL NATURAL -> NATURAL
;; adds up the numbers of both NATURAL lists.
(check-expect (sum N0 N0) empty)
(check-expect (sum N1 N0) N1)
(check-expect (sum N0 N1) N1)
(check-expect (sum N1 N2) N3)
(check-expect (sum N1 N3) N4)
(check-expect (sum N2 N3) N5)

; (define (sum a b) empty) ;stub

(define (sum a b)
  (cond [(ZERO? b) a]
        [else
         (sum (ADD1 a) (SUB1 b))]))

;; Function 2: Subtract!

;; NATURAL NATURAL -> NATURAL
;; produce new list as the subtraction of the 2nd List from the 1st List

(check-expect (subtract N0 N0) N0) ;empty
(check-expect (subtract N1 N1) N0) ;equal
(check-expect (subtract N2 N0) N2) ;a > b
(check-expect (subtract N5 N2) N3) ;a >> b

(check-expect (subtract N0 N2) (cons "-" N2)) ;subtraction = negative number
(check-expect (subtract N2 N5) (cons "-" N3)) ;subtraction = negative number

;(define (subtract a b) N0) ;stub

;<used Template from NATURAL>
(define (subtract a b)
  (cond [(ZERO? b) a]
        [(ZERO? a) (cons "-" b)]
        [else
         (subtract (SUB1 a) (SUB1 b))]))

;; Function 3: Factorial

;; NATURAL -> NATURAL
;; produces the Factorial result of given NATURAL
(check-expect (FACT N0) N1) ; 0! = 1
(check-expect (FACT N1) N1)

(check-expect (FACT N3) N6)

;(define (FACT n) (cons "!" empty)) ;stub

(define (FACT n)
  (cond [(ZERO? n) N1]
        [else
        (TIMES n
       (FACT (SUB1 n)))]))
;;<used Template from NATURAL Data Def>

;; Function 4; Helper Function: multiply!

;; NATURAL NATURAL -> NATURAL
;; multiplies the two numbers (doesn't put into account the "-" so don't bother check-expecting that at least at this !!! point)
(check-expect (TIMES N0 N0) N0)
(check-expect (TIMES N1 N0) N0)
(check-expect (TIMES N0 N1) N0)
(check-expect (TIMES N4 N0) N0)


(check-expect (TIMES N1 N2) N2)
(check-expect (TIMES N2 N1) N2)
(check-expect (TIMES N4 N2) N8)

;(define (TIMES a b) empty) ;stub

(define (TIMES a b)
  (cond [(or (ZERO? a) (ZERO? b)) N0]
;        [(ZERO? (subtract b N1)) N0]
        [else
        (sum a (TIMES a (SUB1 b)))]))

