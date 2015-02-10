#!r6rs

(import (rnrs))          ;  Importation en vrac de tous les modules de la
                         ;  bibliothèque initiale : nous verrons plus tard
                         ;  comment procéder plus précisément. Comme vous
                         ;  l'avez deviné, ceci est un commentaire.

(write (+ 2014 (* (+ 22 1) (/ 14 (- 9 2)))))  ;  (*)
(newline)
(write "Hello world!")
(newline)
(display "Hello world!")
(newline)
(write ((lambda (x y) (* x y y)) 2014 2013))  ;  (**)
(newline)

(define crash
  ;;  (***) Pour bien montrer que Scheme n'évalue pas sous la forme spciale
  ;;  "lambda".
  (lambda () (/ 0)))

(define derive-wrt
  (lambda (f1 h)
    (lambda (x) (/ (- (f1 (+ x h)) (f1 x)) h))))

(write ((derive-wrt (lambda (x) (* x x)) 0.0001) 2))
(newline)
(write ((derive-wrt + 0.0001) 2))  ;  (****)

(define fact
  (lambda (n) (if (zero? n) 1 (* n (fact (- n 1))))))

(newline)
(write (fact 6))
(newline)

(define pi
  (* 4 (atan 1)))

(+ (* 4.1 9.8) (/ (* 2 pi) 3)) 
(+ (exp pi) (log (sin 2.1)) (sin (+ (/ 1 pi) pi)))

(define average
  (lambda(x y)
    (/ (+ x y) 2)))

(define close-enough?
  (lambda( x y e)
    (< (abs (- x y)) e)))
    
(define (look-for-root f1 negp posp e)
    (let* ((m (average negp posp))
           (im (f1 m)))
      (if (= (close-enough? negp posp e) #f)
      (if (= f1(m) 0) 
          (m)
          (if (> f1(m) 0)
              (look-for-root(f1 negp (m) e)) 
              (look-for-root(f1 posp (m) e))
          )
       )
      (m)
     )
    )
  )
  
  
(define (look-for-root2 f n p e)
    (let* ((m (average n p))
           (im (f m)))
      (cond ((close-enough? n p e) m)
            ((< 0 im) (look-for-root2 f m p e))
            ((> 0 im) (look-for-root2 f n m e))
            (else m)
      )
      )
)

(define (dichotomy f1 r1 r2 eps)
  (if (and (<= (f1 r1) 0) (>= (f1 r2) 0))
      (look-for-root f1 r1 r2 eps)
      (begin
        (display "impossible")
        #f
        )
      )
  )