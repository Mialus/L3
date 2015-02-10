#!r6rs

(import (rnrs))          ;  Importation en vrac de tous les modules de la
                         ;  bibliothèque initiale : nous verrons plus tard
                         ;  comment procéder plus précisément. Comme vous
                         ;  l'avez deviné, ceci est un commentaire.
(define (volume-at t v l)
  (* v (+ 1 (* l t))))

(write (volume-at 30 1 1e-5))

(newline)

(define (body-volume-at l)
  (lambda (t v)
    (volume-at t v l)))
(write ((body-volume-at 1e-5) 30 1))

(newline)

(define (gas-volume-at t v)
   ((body-volume-at (/ 1 273)) t v))

  (write (gas-volume-at 30.0 1))

(newline)

(define gas-volume-at-2
  (body-volume-at (/ 1 273)))

(write (gas-volume-at-2 30.0 1))

(newline)

(define (in-inches m y f i)
  (+ (* (+ (* (+ (* 1760 m) y) 3) f) 12) i))

(define (inches-to-meters i)
  (* 0.0254 i))
(define (british-to-metric m y f i)
         (inches-to-meters(in-inches m y f i)))
(write(british-to-metric 3 61 2 10))

(newline)
    
(define (to-metric n1 n2 n3 m0)
  (lambda (u1 u2 u3 u4)
      (* (+ (* (+ (* (+ (* n1 u1) u2) n2) u3) n3) u4) m0)))

(define british-to-metric-v2
  (to-metric 1760 3 12 0.0254))

(write(british-to-metric-v2 3 61 2 10))

(newline)

(define british-to-metric-v3
  (to-metric 24 60 60 1))

(write(british-to-metric-v3 1 3 47 48))

(newline)

;;cf tp2
(define (writeln/return x)
  (write x)
  (newline)
  x)

(define (mystery n p)
  (define (rec-mystery n0 p0)
    (if (zero? (mod n0 p0)) (rec-mystery (div n0 p0) p0) n0))
 ;; Principal call:
  (if (or (zero? p) (zero? n)) n (rec-mystery n p)))

(write(mystery 21 7))

(newline)

(define (hamming-number n)
  (= 1 (mystery (mystery (mystery n 2) 3) 5)))

(write(hamming-number 14))

(newline)

(define (alternate-digit-sum n p)
  (let ((q (div n 10))
           (r (mod n 10)))
        (cond ((zero? n) 0)
              (p (+ (alternate-digit-sum q #f) r))
              (else (- (alternate-digit-sum q #t) r)))))
        
(write (alternate-digit-sum 154 #f))

(newline)

(define (divisible-par-11 n)
  (let ((n1 (abs n)))
    (or (zero? n1)
        (and (>= n1 10)
             (divisible-par-11 alternate-digit-sum n1 #f)))))

(write (divisible-par-11 154))