;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |tuition graph|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; ===================
;; Constants

(define BAR-WIDTH 30)
(define BAR-COLOUR "lightblue")

(define TEXT-SIZE 24)
(define TEXT-COLOUR "black")

(define Y-SCALING 1/200)

;; ==================
;; Data Definitions

(define-struct school (name tuition))
;; school is (make-school String Natural)
;; interp. (make-school name tuition) name of the school as well as tuition
;;         name stands for school name
;;         tuition represents International Student tuition in USD

(define school01 (make-school "SC1" 15000))
(define school02 (make-school "Yale" 50000))

(define (fn-for-school s)
  (... (school-name s)
       (school-tuition s)))
;; Template rules used:
;; - Compound Data: 2 fields: (make-school String Natural)

;; --------------------

;; ListOfSchools is one of:
;; - empty
;; - (cons school ListOfSchools)

;; interp. List containing schools

(define LOS01 (cons school01 (cons school02 empty)))

(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (fn-for-school (first los))
              (fn-for-los (rest lost)))]))
  
;; Template rules used:
;; one of: 2 cases:
;; - atomic distinct: empty
;; - compound: (cons School ListOfSchools)
;; reference: (first los) /is school/
;; self-reference: (rest los) is ListOfSchools


;; =============================
;; Functions

;; chart
;; ListOfSchools -> Image
;; interp. display a bar-type chart each bar height represents school tuition & school name in front of bar

(check-expect (chart empty) (square 0 "solid" "white"))
(check-expect (chart (cons (make-school "Yale" 12000) empty)) (beside/align "bottom"
                                                                            (overlay/align "center" "bottom"
                                                                                           (rotate 90 (text (school-name school02) TEXT-SIZE TEXT-COLOUR))
                                                                                           (rectangle BAR-WIDTH (* 12000 Y-SCALING) "outline" "black")
                                                                                           (rectangle BAR-WIDTH (* 12000 Y-SCALING) "solid" BAR-COLOUR))
                                                                            (square 0 "solid" "white")))

(check-expect (chart (cons (make-school "Yale" 20000) (cons school01 empty))) (beside/align "bottom"
                                                                                            (overlay/align "center" "bottom"
                                                                                                           (rotate 90 (text (school-name school02) TEXT-SIZE TEXT-COLOUR))
                                                                                                           (rectangle BAR-WIDTH (* 20000 Y-SCALING) "outline" "black")
                                                                                                           (rectangle BAR-WIDTH (* 20000 Y-SCALING) "solid" BAR-COLOUR))
                                                                                            (overlay/align "center" "bottom"
                                                                                                           (rotate 90 (text (school-name school01) TEXT-SIZE TEXT-COLOUR))
                                                                                                           (rectangle BAR-WIDTH (* (school-tuition school01) Y-SCALING) "outline" "black")
                                                                                                           (rectangle BAR-WIDTH (* (school-tuition school01) Y-SCALING) "solid" BAR-COLOUR))
                                                                                            (square 0 "solid" "white")))


; (define (chart los) (square 0 "solid" "white")) ;stub

; <used tempalte from ListOfSchool>
(define (chart los)
  (cond [(empty? los) (square 0 "solid" "white")]
        [else
         (beside/align "bottom"
                       (bar (first los))
                       (chart (rest los)))]))

; bar: helper function for chart
; school -> Image
; consume school name & tuition and produce a bar representing tuition fee with school name on it

(check-expect (bar (make-school "" 0)) (overlay/align "center" "bottom"
                                                      (rotate 90 (text "" TEXT-SIZE TEXT-COLOUR))
                                                      (rectangle BAR-WIDTH (* 0 Y-SCALING) "outline" "black")
                                                      (rectangle BAR-WIDTH (* 0 Y-SCALING) "solid" BAR-COLOUR)))
(check-expect (bar school01)  (overlay/align "center" "bottom"
                                             (rotate 90 (text (school-name school01) TEXT-SIZE TEXT-COLOUR))
                                             (rectangle BAR-WIDTH (* (school-tuition school01) Y-SCALING) "outline" "black")
                                             (rectangle BAR-WIDTH (* (school-tuition school01) Y-SCALING) "solid" BAR-COLOUR)))
; (define (bar school) (square 0 "solid" "white")) ;stub

;<used Template from school>

(define (bar s)
  (overlay/align "center" "bottom"
                 (rotate 90 (text (school-name s) TEXT-SIZE TEXT-COLOUR))
                 (rectangle BAR-WIDTH (* (school-tuition s) Y-SCALING) "outline" "black")
                 (rectangle BAR-WIDTH (* (school-tuition s) Y-SCALING) "solid" BAR-COLOUR)))

