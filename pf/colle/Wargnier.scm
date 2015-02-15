#!r6rs
(import (rnrs base) (rnrs io simple) (rnrs lists))

;;Pierre Wargnier Sujet 2

(define (writeln/return x)
  (write x)
  (newline)
  x)

(define examples
  '(("Methanol" (C . 1) (H . 3) (O . 1) (H . 1)) ("Methane" (C . 1) (H . 4))
    ("Acetylene" (C . 2) (H . 2)) ("Ethylene" (C . 2) (H . 4))
    ("Ethane" (C . 2) (H . 6)) ("Cyclopropane" (C . 3) (H . 6)) 
    ("Propane" (C . 3) (H . 8)) ("Cyclobutane" (C . 4) (H . 8))
    ("Butane" (C . 4) (H . 10)) ("Cyclopropane" (C . 3) (H . 6))
    ("Pentane" (C . 5) (H . 12)) ("Hexane" (C . 6) (H . 14))
    ("Heptane" (C . 7) (H . 16)) ("Octane" (C . 8) (H . 18))))

(define name car)


;;La fonction include-into

(define (included-into? l1 l2)
  (or (null? l1)
      (and (member (car l1) l2)
           (included-into? (cdr l1) l2))))

(writeln/return (included-into? '(9 12 2014) '(12 9 10 2014)))
(writeln/return (included-into? '(9 12 2014) '(10 9 2014)))

;; La fonction same-elts
(define (same-elts? l0 l1)
  (and (included-into? l0 l1) 
       (included-into? l1 l0)))

(writeln/return (same-elts? '(9 12 2014) '(12 9 10 2014)))
(writeln/return (same-elts? '(9 12 2014) '(12 9 2014)))

;; La fonction included-into-ne
(define (included-into-ne? l0 l1)
  (and (included-into? l0 l1)
       (not (same-elts? l0 l1))))

(writeln/return (included-into-ne? '(9 12 2014) '(12 9 10 2014)))
(writeln/return (included-into-ne? '(9 12 2014) '(12 9 2014)))

;; La fonction intesection
(define (intersection l0 l1)
  (if (not (null? l0))
      (if (member (car l0) l1)
          (cons (car l0) (intersection (cdr l0) l1))
          (intersection (cdr l0) l1))
      ()))
   
(writeln/return (intersection '(9 12 2014) '(10 12 2015)))

;; La fonction union
(define (union l0 l1)
  (if (null? l1)
      l0
      (if (member (cdr l1) l0)
          (union l0 (cdr l1))
          (union (cons (car l1) l0) (cdr l1)))))

(writeln/return (union '(9 12 2014) '(10 12 2015)))


;; La fonction difference
(define (difference l0 l1)
  (if (null? l0)
     ()
     (if (member (car l0) l1)
         (difference (cdr l0) l1)
         (cons (car l0) (difference (cdr l0) l1)))))
(writeln/return (difference '(9 12 2014) '(10 12 2015)))


;;La fonction all-difference
(define (all-different? l)
  (or (null? l)
      (and (not (member (car l) (cdr l)))
           (all-different? (cdr l)))))

(writeln/return (all-different? '(9 12 2014)))
(writeln/return (all-different? '(9 12 9 2014)))

;; la fonction delta
(define (delta l0 l1)
  (union (difference l0 l1) (difference l1 l0)))

(writeln/return (delta '(9 12 2014) '(10 12 2015)))

;;la fonction note-list
(define note-list '(do re mi fa sol la si))

;;la fonction check-chord
(define (check-chord l)
  (if (null? l)
      #t
      (if (not (member (car l)  note-list))
          #f
          (check-chord (cdr l)))))

(writeln/return (check-chord '(do mi sol si-dieze)))
(writeln/return (check-chord '(do mi sol si)))

;; la fonction same-chord
(define (same-chord? ch0 ch1)
  (and (included-into? ch0 ch1) (included-into? ch1 ch0)))
(writeln/return (same-chord? '(do mi sol) '(sol do mi)))
(writeln/return (same-chord? '(do mi fa) '(sol do mi)))

;(define (same-position? cho ch1)
 ; (if (and (null? ch0) (null? ch1))
  ;    #t
           
                 