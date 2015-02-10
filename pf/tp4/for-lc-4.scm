#!r6rs
(import (rnrs base) (rnrs io simple) (rnrs lists))

(define (writeln/return x)
  (write x)
  (newline)
  x)

(define (butlast-and-last l)
  ;;  Rappelons qu'ici, "l" est une liste lin�aire non vide.
  (let ((first (car l))
	(rest (cdr l)))
    (if (null? rest)
	(values '() first)
	(call-with-values (lambda () (butlast-and-last rest))
	  (lambda (butlast-0 last-0)
	    (values (cons first butlast-0) last-0))))))

(define (butlast-and-last-v2 l)
  ;;  Seconde d�finition de cette fonction, en utilisant non pas la fonction
  ;;  "call-with-values", mais la forme sp�ciale "let-values".
  (let ((first (car l))
	(rest (cdr l)))
    (if (null? rest)
	(values '() first)
	(let-values (((butlast-0 last-0) (butlast-and-last-v2 rest)))
	  (values (cons first butlast-0) last-0)))))

(define (palindrome? l)
  (if (null? l)
      #t
      (let ((rest (cdr l)))
	(if (null? rest)
	    #t
	    (let-values (((butlast-0 last-0) (butlast-and-last rest)))
	      (and (equal? (car l) last-0) (palindrome? butlast-0)))))))

(define (take-while p1? l)
  (if (null? l)
      '()
      (let ((first (car l)))
	(if (p1? first) (cons first (take-while p1? (cdr l))) '()))))

(define (drop-while p1? l)
  (if (or (null? l) (not (p1? (car l)))) l (drop-while p1? (cdr l))))

(writeln/return (call-with-values (lambda () (values 'L 3)) cons))

(writeln/return (let ((a 'sure)
		      (b 'here)
		      (x 'little)
		      (y 'players))
		  (let-values (((a b) (values x y))
			       ((x y) (values a b)))
		    (list a b x y))))

(writeln/return (let ((a 'sure)
		      (b 'here)
		      (x 'little)
		      (y 'players))
		  (let*-values (((a b) (values x y))
				((x y) (values a b)))
		    (list a b x y))))

(writeln/return (let-values (((first) (car '(very little players)))
			     ((the-quotient the-remainder)
			      (div-and-mod 2000 9)))
		  (cons* the-quotient the-remainder first)))

(define (span? p? l)
  (if (null? l) (values '() '())
      (let ((first (car l)))
        (if (p? first)
            (let-values (((l1 l2)(span? p? (cdr l))))
            (values (cons first l1) l2))
        (values '() l)))))

(let-values (((l1 l2) (span? even? '(2 4 5 4 5 6))))
  (writeln/return l1)
  (writeln/return l2))

(define (map-3-functions f1 g1 h1 l)
  (if (null? l) (values '() '() '())
      (let ((first (car l)))
        (let-values (((l1 l2 l3) (map-3-functions
                                  f1 g1 h1 (cdr l))))
          (values (cons (f1 first) l1)
                  (cons (g1 first) l2)
                  (cons (h1 first) l3))))))
(let-values (((l1 l2 l3)
              (map-3-functions
               (lambda (x) (+ x 1))
               (lambda (x) (+ x 2))
               (lambda (x) (+ x 3))
               '(1 2 3 4 5))))
  (writeln/return l1)
  (writeln/return l2)
  (writeln/return l3))

;;preparation Tp noté

(define (included-into? l1 l2)
  (or (null? l1)
      (and (member (car l1) l2)
           (included-into? (cdr l1) l2))))

(writeln/return (included-into? '(1 2 3) '(4 2 6 3)))

(define (same-elts? l0 l1)
  (and (included-into? l0 l1) (included-into? l1 l0)))
(writeln/return (same-elts? '(2 12 2014) '(2014 12 2)))

;tefreito version
;(define (same-elts? l1 l2)
;(and (included-into? l1 l2)
;     (included-into? l2 l1))))
;

(define (find-tail? p? l)
  (cond ((null? l) #f)
        ((p? (car l)) l)
        (else (find-tail? p? (cdr l)))))

(writeln/return (find-tail? zero? '( 1 2 0 4 3)))

(define (assq-ff x l)
  (find-tail? (lambda (association)
                (eq? x
                     (car association)))
              l))

(writeln/return (assq-ff 'L '((M . 1) (L . 3) (M . 2))))

      