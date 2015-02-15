#!r6rs

(import (rnrs))

(define (writeln/return x)
  (write x)
  (newline)
  x)

;;  Start

;; (writeln/return (cons 0 1))
;; (writeln/return (cons 0.1 0.1))
;; (writeln/return (cons (cons 0.1 0.1) 1))
;; (writeln/return (cons 0.1 '()))
;; (writeln/return (cons '() 0.1))
;; (writeln/return (cons '() '()))

(define current-year 2014)
(define next-year (+ current-year 1))
(define academic-year (cons current-year next-year))
(define academic-year-0 (cons current-year 'next-year))
(define academic-year-1 (cons 'current-year 'next-year))
(define operations (cons + -))
(define symbols (cons '+ '-))
(define two-numbers (cons 0 1))
(define something (cons (cons 0.1 0.1) 1))

;;  (writeln/return academic-year)
;;  (writeln/return (quote academic-year))
;;  (writeln/return 'academic-year)
;;  (writeln/return 'undefined)
;;  (writeln/return undefined)
;;  (writeln/return +)
;;  (writeln/return '+)
;;  (writeln/return (0 . 1))
;;  (writeln/return '(0 . 1))
;;  (writeln/return  (+ . 1))
;;  (writeln/return '(+ . 1))
;;  (writeln/return (car academic-year))
;;  (writeln/return (cdr academic-year))
;;  (writeln/return (car next-year))
;;  (writeln/return academic-year-0)
;;  (writeln/return (cdr academic-year-0))
;;  (writeln/return academic-year-1)
;;  (writeln/return ((car operations) (car two-numbers) (cdr two-numbers)))
;;  (writeln/return ((cdr symbols) (cdr two-numbers) (car two-numbers)))
;;  (writeln/return (cons (cdr symbols) (car two-numbers)))
;;  (writeln/return (cons (cdr operations) (cdr two-numbers)))
;;  (writeln/return ((cdr operations) (cdr two-numbers)))
;;  (writeln/return (car something))
;;  (writeln/return (cdr (car something)))
;;  (writeln/return (car (cdr something)))

;; Playing with Pairs

(define (pair-op f2 v w)
  (cons (f2 (car v) (car w)) (f2 (cdr v) (cdr w)))) 

  (writeln/return (pair-op + '(30 . 10) '(5 . 11)))
  (writeln/return (pair-op - '(30 . 10) '(5 . 11)))

(define (diagonal i)
  (if(<= i 0)
     '(0 . 0)
     (let* ((diag (diagonal (- i 1)))
           (p (car diag))
           (q (cdr diag)))
          (if (zero? q)
              (cons 0 (+ p 1))
              (cons (+ p 1)(- q 1))))))
  
  (writeln/return (diagonal 4))
;;  Lists

;;  (writeln/return '(A . ((String . ()) . ((() . of) . (Pearls . ())))))
;;  (writeln/return '((In . ()) . (() . the) . (Mood . ())))
  (writeln/return
   '((Walk . ((() . ()) . on)) . ((() . the) . ((Wild . (Side . **)) . ()))))

  (writeln/return
   (cons (list 'Walking 'on 'the 'Moon) '()))

  (writeln/return (cons 'Keep (cons 'it '(up))))

  (writeln/return
   (append (list 'Just 'a)
                  (cons 'cast (cons 'away '()))))

  (writeln/return
   (append (list 'I 'hope) (append '(that) '(someone))
                  (cons 'gets '(my))))

  (writeln/return
   (cons 'Message
                  (append '((in a)) (list 'Bottle))))

;;;  Unknown information

(define unknown '??)

(define (unknown? x)
  (eq? x unknown))

;;;  Constructor

(define (mk-recording key-0 title-0 author-list-0 year-0 publisher-0 
		      key-list-0) 
  ;; SYMBOL,STRING,LIST-of[SYMBOL]|,INTEGER|,SYMBOL|,LIST-of[SYMBOL] -> JAZZ-R
  (list key-0 title-0 author-list-0 year-0 publisher-0 key-list-0))

(define key car)
(define title cadr)
(define (authors record)
  (list-ref record 2))

;(define year (list-ref record 3))
;;;  Examples

(define miles-davis-r
  (list (mk-recording 'd0 "On the Corner" unknown 1972 unknown '(c40))
	(mk-recording 'd1 "New-York Girl" unknown 1972 unknown '(c40))
	(mk-recording 'd2 "Thinkin' One Thing and Doin' Another" unknown 1972
		      unknown '(c40))
	(mk-recording 'd3 "One and One" unknown 1972 unknown '(c40))
	(mk-recording 'd4 "Well You Needn't" '(Thelonius-Monk) 1954
		      'Blue-Ribbon-Music '(c41))
	(mk-recording 'd5 "Love for Sale" '(Cole-Porter) 1958 'Chappell '(c41))
	(mk-recording 'd6 "Something Else" '(Miles-Davis) 1958 'EMI '(c41))
	(mk-recording 'd7 "Dear Old Stockholm" '(Stan-Getz P-Golly) 1952
		      'Windswept-Pacific-Music '(c41))
	(mk-recording 'd8 "Black Satin" unknown 1972 unknown '(c40))
	(mk-recording 'd9 "Mr. Freedom X" unknown 1972 unknown '(c40))
	(mk-recording 'd10 "Helen Butte" unknown 1972 unknown '(c40))
	(mk-recording 'd11 "Boplicity" '(Cleo-Henry) 1949 'Campbell '(c40))
	(mk-recording 'd12 "Ray's Idea" '(Ray-Bonner W-G-Fuller) 1953 'Bosworth
		      '(c41))
	(mk-recording 'd13 "Yesterdays" '(Kern Harbach) 1952 'Universal '(c42))
	(mk-recording 'd14 "Vote for Miles" unknown 1972 unknown '(c40))
	(mk-recording 'd15 "Deception" '(Miles-Davis) 1950 'Sony '(c41))
	(mk-recording 'd16 "Israel" '(John-Carisi) 1949 'EMI '(c41))
	(mk-recording 'd17 "How Deep is the Ocean" '(Irving-Berlin) 1952 'EMI
		      '(c41))
	(mk-recording 'd18 "Kelo" '(Jay-Jay-Johnson) 1953 'Kensington-Music
		      '(c41))
	(mk-recording 'd19 "Woody 'n' You" '(Dizzy-Gillepsie) 1952 'Chappell
		      '(c41))
	(mk-recording 'd20 "Chance It" '(Oscar-Petitford) 1952 'Orpheus-Music
		      '(c41))))

(define stan-getz-r
  (list (mk-recording 'g30 "Autumn Leaves" '(Kosma Prevert) unknown
		      'Peter-Maurice-Co-Ltd '(c44))
	(mk-recording 'g31 "Nature Boy" '(Abbon) unknown 'Chappell '(c44))))


(writeln/return (authors (car miles-davis-r)))

(define (unique-keys? l)
  (if (null? l)
      #t
      (let* ((rest (cdr l))
             (l2 (map key rest)))
        (if (not (member (key (car l)) l2))
            (unique-keys? rest)
            #t))))

;;(define (unike-keys2? l)
;;  (or (null? l)
;;      (let* ((rest (cdr l))
;;             (l2 (map key rest)))
;;        (a


;;;  Making groups from a non-empty linear list.

(define (make-groups l0)
  ;; LIST -> LIST-of[GROUP]
  (let ((first (car l0))
	(rest (cdr l0)))
    (if (null? rest)
	;; Une liste linéaire comportant un seul groupe à un élément :
	(list (list first))
	(let ((next-groups (make-groups rest)))
	  ;; Notez que "next-groups" n'est pas la liste vide.
	  (if (<= first (car rest))
	      ;; Le premier élément de "l0" rejoint le premier groupe dans la
	      ;; liste résultat :
	      (cons (cons first (car next-groups)) (cdr next-groups))
	      ;; Création d'un nouveau groupe, placé en tête des groupes
	      ;; existants :
	      (cons (list first) next-groups))))))

;;;  Merging groups

;;  (define (merge-groups group-list)
;;    (cond ((null? group-list) ***fill-in***)
;;          ((null? (cdr group-list)) (list ***fill-in***))
;;          (else (cons ***fill-in*** ***fill-in***))))

;;;  Examples for testing the merge sort procedure.

(define cp-list-example
  '((5 . 5) (6 . 0) (3 . 3) (6 . 1) (7 . 7) (1 . 1) (6 . 2)))

(define simple-list-example (map car cp-list-example))

;;  (writeln/return simple-list-example)
;;  (writeln/return (mergesort simple-list-example))
;;  (writeln/return (mergesort-v2 simple-list-example <=))
;;  (writeln/return (mergesort-v2 simple-list-example >=))

;;;  Dates

(define dates
  ;; Les éléments de cette liste sont tous des dates, données suivant le format
  ;; (<jour> <mois> <année>).
  '((9 11 1998) (11 11 1997) (1 1 1900) (17 11 1997) (29 2 2000) (25 12 1999)))

;;  (writeln/return (<arithmetical? 2014 2015 (lambda () 'ko)))
;;  (writeln/return (<arithmetical? 2014 2014 (lambda () 'ok)))
;;  (writeln/return (<arithmetical? 2015 2014 (lambda () (/ 0))))

;;;  Composers

(define composers
  '((Schoenberg 1874 1951) (Gorecki 1933 2010) (Messiaen 1908 1992)
    (Varese 1883 1965) (Rozsa 1907 1995) (Boulez 1925 **) (Artiomov 1940 **)
    (Webern 1883 1945) (Penderecki 1933 **) (Schnittke 1934 1998)
    (Berg 1883 1935) (Svetlanov 1928 2000) (Jaume 1940 **)))

;(define (get-key x0 sel l)
;(cond ((null? l) #f)
;((eq? x0 (sel (car l)) ((key (car l)))
;	(else (get-key x0 sel (cdr l)))))))
	
;(writeln/return (get-key "kelo" title miles-davis-r))
;(writeln/return (get-key "keloo" title miles-davis-r))

;(define (get-key2 x0 sel l)
 ; (let* ((l2 (map (lambda(x) (list (sel x) (key x))) l))
  ;  (res (assoc x0 l2)))
   ;(if res
   ;     (cadr res)
   ;     #f)))
  ;(and cadr res)))

;(writeln/return (get-key2 "kelo" title miles-davis-r))
;(writeln/return (get-key2 "keloo" title miles-davis-r))

;(define (those-that p? l f)
 ; (cond ((null? l) '())
   ;     ((p? (car )) (cons (f (car l)) (those-that p? (car l) f)))
  ;      'else (those-that p? (cdr l) f)))

(define cd-list-example '((5 . 5) (6 . 0) (3. 3) (6 .1) (7.7) (1 . 1) (6 . 2)))
;(define simple-list-example (map car cp-list-example))

(define (merge2group g1 g2)
  (cond ((null? g1) g2)
        ((null? g2) g1)
        ((< (car g1) (car g2)) (cons (car g1)
                                     (merge2group (cdr g1) g2)))
(else (cons (car g2) (merge2group g1 (cdr g2))))))

(writeln/return (merge2group '(1 2 4 8) '(0 4 6)))

(define (merge-groups group-list)
  (cond ((null? group-list) '())
        ((null? (cdr group-list)) (list (car group-list)))
        (else (cons (merge2group (car group-list) (cadr group-list))
                    (merge-groups (cddr group-list))))))

(writeln/return (merge-groups '((1 2 4 8) (0 4 6) (10 20) (1 5) (3 5))))
(writeln/return (merge-groups '((1 2 4 8) (0 4 6) (10 20) (1 5))))

(define (iter-merge-groups group-list)
  (if (null? (cdr group-list))
      (car group-list)
      (iter-merge-groups (merge-groups group-list))))

(writeln/return (iter-merge-groups
                 '((1 2 4 8) (0 4 6) (10 20) (1 5) (3 5))
                 ))

(define (mergesort-v2 l rel)
  (define (make-groups l0)
    ;; LIST -> LIST-of[GROUP]
    (let ((first (car l0))
          (rest (cdr l0)))
      (if (null? rest)
          ;; Une liste linéaire comportant un seul groupe à un élément :
          (list (list first))
          (let ((next-groups (make-groups rest)))
            ;; Notez que "next-groups" n'est pas la liste vide.
            (if (rel first (car rest))
                ;; Le premier élément de "l0" rejoint le premier groupe dans la
                ;; liste résultat :
                (cons (cons first (car next-groups)) (cdr next-groups))
                ;; Création d'un nouveau groupe, placé en tête des groupes
                ;; existants :
                (cons (list first) next-groups))))))
  (define (merge-two-groups g1 g2)
    (cond ((null? g1) g2)
          ((null? g2) g1)
          ((rel (car g1) (car g2)) (cons (car g1) (merge-two-groups (cdr g1) g2)))
          (else (cons (car g2) (merge-two-groups g1 (cdr g2))))))
  (define (merge-groups group-list)
    (cond ((null? group-list) '())
          ((null? (cdr group-list)) (list (car group-list)))
          (else (cons (merge-two-groups (car group-list) (cadr group-list))
                      (merge-groups (cddr group-list))))))
  (define (iter-merge-groups group-list)
    (cond ((null? (cdr group-list)) (car group-list))
          (else (iter-merge-groups (merge-groups group-list)))))
  (let ((l2 (make-groups l)))
    (iter-merge-groups l2)))

;let((j1 (car d1))
 ;   (m1 (cadr d1))
  ;  (a1 (caddr d1))
   ; (j2 (car d2))
    ;(m2 (cadr d2))
;    (a2 (caddr d2))
 ; (or (< a1 a2)
  ;    (and (= a1 a2)
   ;        (or (< m1 m2))
    ;           (and (= m1 m2) (<= j1 j2))))))))

(define (<arithmetical? x y thunk)
  (or (< x y)
      (and (= x y) (thunk))))

(define (<=dates-v2? d1 d2)
  (let ((j1 (car d1))(m1 (cadr d1))(a1 (cadr d1))
        (j2 (car d2))(m2 (cadr d2))(a2 (caddr d2)))
    (<arithmetical?
     a1 a2
     (lambda ()
       (<arithmetical?
        m1 m2
        (lambda () (<= j1 j2)))))))

(writeln/return (mergesort-v2 dates <=dates-v2?))

(define (<=** x1 x2)
  (or (and (not(equal? x1 '**))(not (equal? x2 '**))
           (<= x1 x2))
      (equal? x2 '**)))

(define composers
'((Schoenberg 1874 1951) (Gorecki 1933 2010) (Messiaen 1908 1992)
(Varese 1883 1965) (Rozsa 1907 1995) (Boulez 1925 **)
(Artiomov 1940 **) (Webern 1883 1945) (Penderecki 1933 **)
(Schnittke 1934 1998) (Berg 1883 1935) (Svetlanov 1928 2000)
(Jaume 1940 **)))

(define (rel? r1 r2)
  (or (unknown? (year r1))
      (and (not (unknown?