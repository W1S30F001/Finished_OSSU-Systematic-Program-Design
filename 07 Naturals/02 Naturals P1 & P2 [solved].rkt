;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |02 Naturals P1 & P2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; =================
;; Data Defs
;; don't think it matters designing data for that but since this is for Naturals Module + practice let's do it
;; this is more like interval, since the cap number is set on fn call

;; Natural is one of:
;; - 0
;; - (add1 Natural)
;; interp. arbitrary-sized list of numbers from 0 to n

(define listn0 0)
(define listn1 (add1 listn0))
(define listn99 (add1 98)) ;should be listn98 but i'm not gonna write allat

#;
(define (fn-for-NaturalList n)
  (cond [(zero? n) (...)]
        [else (... n
              (fn-for-Natural (sub1 n)))]))
;; Template rules used:
;; - one of: 2 cases:
;; - atomic N0N-DiStInCt!!: 0
;; - compound: (add1 Natural)
;; self-reference: (sub1 n) is Natural
;; ... combination
;; n   contribution of the (first Natural) i.e. given n

  
;; a better data type fit is Interval in my opinion, since n is set on function call

;; Natural is [0, n]
;; number set

(define nx10 10)

(define (fn-for-NaturalInterval nx)
  (... nx))
;; Template rules used:
;; Atomic non-distinct: Interval

;; ===================
;; Functions

;; Function 1: Image decreasing

;; Natural -> Image
;; Image of numbers decreasing from n to zero
(check-expect (ImageNatural 0) (text (number->string 0) 24 "white"))

(check-expect (ImageNatural 2) (beside (text (number->string 2) 24 "white")
                                       (text (number->string 1) 24 "white")
                                       (text (number->string 0) 24 "white")))

(check-expect (ImageNatural 2) (beside (text (number->string 2) 24 "white")
                                       (beside (text (number->string 1) 24 "white")     ;redundantly repeating (beside ...) recursively is fine i guess..?
                                               (text (number->string 0) 24 "white"))))

;(define (ImageNatural n) (text (number->string 0) 24 "white")) ;stub

(define (ImageNatural n)
  (cond [(zero? n) (text (number->string 0) 24 "white")]
        [else (beside (text (number->string n) 24 "white")
                      (ImageNatural (sub1 n)))]))

;<used template from Natural>





;; Function 2: Odd > stdout

;; Natural number n -> Natural
;; consumes Natural number n & produces a list of all the odd numbers from n to zero

(check-expect (Oddout 0) empty)
(check-expect (Oddout 1) (cons 1 empty))
(check-expect (Oddout 2) (cons 1 empty))
(check-expect (Oddout 3) (cons 3 (cons 1 empty)))


;(define (Oddout n) empty) ;stub

(define (Oddout n)
  (cond [(zero? n) empty]
        [else (if (not (= (remainder n 2) 0))
              (cons n (Oddout (sub1 n)))
              (Oddout (sub1 n)))]))

;;<used Template from Natural>




























