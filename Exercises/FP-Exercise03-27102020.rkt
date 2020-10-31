;EXERCISE 3 | ACCUMULATE |

;deffinition of Accumulate
(define (accumulate op nv a b term next)
  (if(> a b)
      nv
     (op (term a) (accumulate op nv (next a) b term next))))

;help functions:
(define (id x) x)
(define (1+ x) (+ x 1))
(define(complement p?)
  (lambda(x) (not(p? x))))

;example 1
(define (sum a b)
  (accumulate + 0 a b id 1+))

;task 1
(define (!! n)
  (accumulate * 1
              (if (odd? n) 1 2) n
              id
              (lambda (x) (+ x 2))))



;task 2
(define (fact n)
  (accumulate * 1 1 n id 1+))
(define(nchk n k)
  (/ (fact n) (*(fact(- n k)) (fact k))))

;task 3
;(nchk n k) = n!/(n-k)!k! but also:
;(nchk n k) = n*(n-1)*...*(n-(k-1)) / k*(k-10*...*1,
;so we can group them:
(define (nchk* n k)
  (accumulate * 1
              0 (- k 1)
              (lambda (i) (/ (- n i) (- k i)))
              1+))

;task 4.1 - accumulate
(define(2^ n)
  (accumulate * 1 1 n (lambda (x) 2) 1+))
;term always return 2, then we * n times 2
;task 4.2 - nchk
(define (2^* n)
  (accumulate + 0 0 n (lambda (k) (nchk* n k)) 1+))

;task 5
;all?
(define (all? p? a b)
  (accumulate (lambda (x y) (and x y)) #t
                a b
                p?
                1+))
;any? via Demorgan
(define(any? p? a b)
  (not (all?(complement p?) a b)))
;explanation:
; 1 2 3 4 5
;
;(and(p? 1) (and(p? 2) (and(p? 3)...))) Goes through all
;members of the interval

;example 3:
;"Smart" version of accumulate wich does not go through
;every member of the interval. Saves time and memory
;(usefull when we have 'and' and at lest one #f)
(define(allSmartVersion? p? a b)
 (if(> a b) #t
      (and ( p? a)
           (allSmartVersion? p? (+ a 1) b))))

;task 5 1/2
; Понякога е по-удобно да обходим целия
; интервал и да филтрираме някои от числата, вместо
; да мислим сложни начини за прескачане от едно на друго
(define (filter-accum p? op nv a b term next)
  (cond ((> a b) nv)
        ((p? a) (op (term a)
                    (filter-accum p? op nv (next a) b term next)))
        (else       (filter-accum p? op nv (next a) b term next))))

(define (!!* n)
  (filter-accum (if(even? n) even? odd?)
                * 1
                1 n
                id 1+))
;explanation:
;1    2    3    4     5
;(term1)   (term 3)

;task 6
(define(divisors-sum n)
  (filter-accum (lambda (x) (zero? (remainder n x)))
          + 0
          1 n
          id 1+))
;without 'filter-accum':
(define(divisors-sum* n)
 (accumulate + 0
             1 n
              ;"фалшива" функция за общ член: делителите се
              ;трансформират в себе си, а другите числа в 0
             (lambda (x) (if (zero? (remainder n x)) x 0))
             1+))

;task 7 - same as example 2
; Брои за колко от числата в интервала
; [a;b] е изпълнен предиката p?
(define(count p? a b)
  (accumulate + 0 a b (lambda (x) (if (p? x) 1 0)) 1+))

;task 8
; Вече имаме няколко начина да проверим дали число е просто
; Използваме accumulate, макар и индиректно (няма нужда да пишем едно и също по два пъти)
;with count:
(define (prime? n)
 (and (> n 1)
      (zero? (count (lambda (x) (zero? (remainder n x)))
              2
              (sqrt n)))))
;with all/any
(define (prime?* n)
  (and(> n 1)
      (not(any? (lambda (x) (zero? (remainder n x)))
                2
                (sqrt n)))))

;task 8*
(define (compose f g)
  (lambda (x) (f (g x))))
(define (repeat n f)
  (accumulate compose id 1 n (lambda (x) f) 1+))



  