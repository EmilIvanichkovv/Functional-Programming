

(define (accumulate op nv a b term next)
  (if(> a b)
      nv
     (op (term a) (accumulate op nv (next a) b term next))))


;Функция която чертае долната част на квадрат със броя на символите нужни за този квадрат.
(define (bottom p)
  (define (id x) x)
  (define (1+ x) (+ x 1))
  (define (op x y)
    (if(= x 1)(display #\┘))
    (if(= x p) (display #\└))
    (if (and (> x 1) (< x p)) (display #\─)))                             
  (accumulate op 2 1 p id 1+))

;Функция която чертае горната част на квадрат със броя на символите нужни за този квадрат.
(define (top p)
  (define (id x) x)
  (define (1+ x) (+ x 1))
  (define (op x y)
    (if(= x 1)(display #\┐))
    (if(= x p) (display #\┌))
    (if (and (> x 1) (< x p)) (display #\─)))                             
  (accumulate op 2 1 p id 1+))

;Функции които чертаят лявата и дясната страни на по-външните квадрати.
(define(rightSide q)
  (define (id x) x)
  (define (1+ x) (+ x 1))
  (define (op x y)
     (display #\│)
     (display #\space))
  (accumulate op 1 1 q id 1+))
 
(define(leftSide q)
  (define (id x) x)
  (define (1+ x) (+ x 1))
  (define (op x y)
  (display #\space)
    (display #\│))
  (accumulate op  (display #\‎) 1 q id 1+));Забележка, използвам 'Празен символ за дъно' ???


;Функции които чертаят страните на квадратите

(define(printBottomSide n)
  (define max (+ 3 (* 4 n)))
  (define (id x) x)
  (define (1+ x) (+ x 1))
  (define  (op x y)
    (display #\newline)
    (rightSide (- x 1))
    (bottom(- max (* 4 x)))
    (leftSide (- x 1)))
  (accumulate op 0 1 n id 1+))


(define(printTopSide n)
  (define max (+ 3 (* 4 n)))
  (define (idReverse x) (- n (- x 1)))
  (define (1+ x) (+ x 1))
  (define  (op x y)
    (cond((= x 1)
          (rightSide (- x 1))
           (top(- max (* 4 x)))
           (leftSide (- x 1)))
         (else (display #\newline)
               (rightSide (- x 1))
               (top(- max (* 4 x)))
               (leftSide (- x 1)))))
  (accumulate op 0 1 n idReverse 1+))

(define (square n)
  (printTopSide n)
   (printBottomSide n))







