;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |00 Naturals Add & list|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ================
;; Data Defs
;; Natural is one of:
;; - 0
;; - (add1 Natural)
;; interp. a natural number that isn't exactly a list whenever really confined arbitray type data to lists, yes the number can be extrapolated into a list which is why can actually apply the arbitrary one of: rule to it as it has a base 0 and a incrementingly masturbative function inside of it.


(define n0 0)
(define n1 (add1 n0))
(define n2 (add1 n1))
#;
(define (fn-for-Natural n)
  (cond [(zero? n) (...)]
        [else
          (... n
              (fn-for-natural (sub1 n)))]))
;; Tempalte rules used:
;; - one of:
;; - atomic non-distinct: 0   ;(1) base
;; - compound: (add1 Natural)
;; self-reference: (sub1 n) is Natural
;; (2) n is "contribution of first"? (3) ... is gon be the "combination"

;; Function 1
;; Natural -> Natural
;; sums all the numbers adding up to n; (n + (n - 1) ... + 0)
(check-expect (sum 0) 0)
(check-expect (sum 1) (+ 1 0))
(check-expect (sum 2) (+ 2 1 0))
(check-expect (sum 3) (+ 3 2 1 0))

;(define (sum n) 0) ;stub

(define (sum n)
  (cond [(zero? n) 0]
        [else
          (+ n
             (sum (sub1 n)))]))

; used templated from Natural

;; Function 2
;; Natural -> ListOfNatural
;; consumes number n & makes up a list of numbers in [0, n]

(check-expect (listN 0) empty)
(check-expect (listN 1) (cons 1 empty))
(check-expect (listN 2) (cons 2 (cons 1 empty)))

;(define (listN n) empty) ;stub

(define (listN n)
  (cond [(zero? n) empty]
        [else
          (cons n
              (listN (sub1 n)))]))







