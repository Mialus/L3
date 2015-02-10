#!r6rs
(import (rnrs base) (rnrs io simple) (rnrs lists) (rnrs unicode))

(define (writeln/return x)
  (write x)
  (newline)
  x)

(define beer-a-list
  '(("Altbier" Germany ale 4.75) ("Amos" France lager 4.6)
    ("Cardinal" Swiss lager 4.8) ("Chimay" Belgium lambic 9.)
    ("Ciney" Belgium ale 7.) ("Fosters" Australia lager 5.)
    ("Gauloise" Belgium ale 9.) ("Guinness" Ireland stout 4.3)
    ("Harbin" China lager 4.8) ("Harp" Ireland lager 6.2)
    ("Hoegaarden" Belgium lambic 4.9) ("Kriek" Belgium lambic 3.5)
    ("Krombacher" Germany lager 4.8) ("London Pride" United-Kingdom ale 4.7)
    ("Orval" Belgium ale 6.2) ("Pilsner Urquell" Czech-Republic lager 4.4)
    ("Tsingtao" China lager 6.) ("Yanjing" China lager 4.5)))

(define (adjoin x l)
  (if (equal? (member x l) #f)
      (cons x l)
      l))
(writeln/return (adjoin 2012 '(13 12 2013)))
(writeln/return (adjoin 2013 '(13 12 2013)))


(define (get-those-that p1? l f1)
  (if (null? l)
      '()
      (if (p1? (car l))
          (cons (f1 (car l)) (get-those-that p1? (cdr l) f1))
          (get-those-that p1? (cdr l) f1))))
(writeln/return (get-those-that even? '() (lambda (x) (+ x 1))))
(writeln/return (get-those-that even? '(13 12 2014) (lambda (x) (+ x 1))))

(define (check-ordered? l p1?)
  (or (null? l)
      (null? (cdr l))
      (and (p1? (car l) (cadr l))
           (check-ordered? (cdr l) p1?))))
(writeln/return (check-ordered? '(13 12 2013) <=))
(writeln/return (check-ordered? '(12 12 2013) <))
(writeln/return (check-ordered? '(12 12 2013) <=))

(define beer-name car)
(writeln/return (beer-name (car beer-a-list)))

(define beer-country cadr)
(writeln/return (beer-country (car beer-a-list)))

(define beer-category caddr)
(writeln/return (beer-category (car beer-a-list)))

(define beer-abv cadddr)
(writeln/return (beer-abv (car beer-a-list)))

(define (get-beer-name l)
  (if (null? l)
      '()
      (map beer-name beer-a-list)))
(writeln/return (get-beer-name beer-a-list))

(writeln/return (check-ordered? (get-beer-name beer-a-list) string<?))

(define (look-4 s l)
  (if (null? l)
      #f
      (if (string-ci=? s (beer-name (car l)))
          (car l)
          (look-4 s (cdr l)))))
(writeln/return (look-4 "HaRbIn" beer-a-list))
(writeln/return (look-4 "1664" beer-a-list))

(writeln/return (get-those-that (lambda?  '(13 12 2014) (lambda (x) (+ x 1))))
