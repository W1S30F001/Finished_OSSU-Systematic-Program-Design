;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname for-encapsulation-exercise) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Person -> ListOfString
;; produce a list of the names of the persons under 20
(define-struct person (name age children))


(define P1 (make-person "N1" 5 empty))
(define P2 (make-person "N2" 25 (list P1)))
(define P3 (make-person "N3" 15 empty))
(define P4 (make-person "N4" 45 (list P3 P2)))


(check-expect (names-under-20 P1) (list "N1"))

(check-expect (names-under-20 P2) (list "N1"))
(check-expect (names-under-20 P4) (list "N3" "N1"))

(define (names-under-20 p)
  (local [(define (names-under-20--person p)
            (if (< (person-age p) 20)
                (cons (person-name p)
                      (names-under-20--lop (person-children p)))
                (names-under-20--lop (person-children p))))
      
          (define (names-under-20--lop lop)
            (cond [(empty? lop) empty]
                  [else
                   (append (names-under-20--person (first lop))
                           (names-under-20--lop (rest lop)))]))]
(names-under-20--person p)))

;;  ============== sort =========== no mutual recursion

;; ListOfNumber -> ListOfNumber
;; sort the numbers in lon in increasing order
(check-expect (sorrt empty) empty)
(check-expect (sorrt (list 1)) (list 1))
(check-expect (sorrt (list 1 2 3)) (list 1 2 3))
(check-expect (sorrt (list 2 1 3)) (list 1 2 3))
(check-expect (sorrt (list 3 2 1)) (list 1 2 3))

(define (sorrt lon)
  (local [(define (sort-lon lon)
  (cond [(empty? lon) empty]
        [else
         (insert (first lon)
                 (sort-lon (rest lon)))]))
(define (insert n lon)
  (cond [(empty? lon) (cons n empty)]
        [else
         (if (> (first lon) n)
             (cons n lon)
             (cons (first lon) (insert n (rest lon))))]))]
    (sort-lon lon)))

