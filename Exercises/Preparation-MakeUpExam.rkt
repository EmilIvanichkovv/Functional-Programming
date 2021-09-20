
#lang racket

(define ^2 (lambda (x) (* x x))) ; (define (^2 x) (* x x))
(define 1+ (lambda (x) (+ x 1)))
(define id (lambda (x) x))
(define (^3 x) (* x x x))

; Упражнение 1:
; Зад.1
(define (interval-sum a b)
  (define (helper start sum)
    (if (> start b) sum
        (helper (+ start 1) (+ sum start))))
  (helper a 0))

; Зад.2
(define (fast-expt x n)
  (define (sq x) (* x x))
  (cond ((= n 0) 1)
        ((even? n) (sq (fast-expt x (quotient n 2))))    ;   (x^(n/2))^2
        (else (* x (sq (fast-expt x (quotient n 2))))))) ; x*(x^(n/2))^2

; Зад.3
(define (last-digit n)
  (remainder n 10))

(define (count-digit d n)
  (define (helper newN count)
    (cond ((= newN 0) count)
          ((= d (last-digit newN))
             (helper (quotient newN 10) (+ count 1)))
          (else (helper (quotient newN 10) count))))
  (if (= n 0) (if (= d 0) 1 0)
      (helper n 0)))

; (count-digit 3 132)
; (helper 132 0)
; (helper 13 0)
; (helper 1 1)
; (helper 0 1)
; -> 1

; Зад.4
(define (reverse-int n)
  (define (helper newN res)
    (if (= newN 0) res
        (helper (quotient newN 10)
                (+ (* res 10) (last-digit newN)))))
  (helper n 0))

; (reverse-int 12345)
; (helper 12345 0)
; (helper 1234 5)
; (helper 123 54)
; (helper 12 543)
; (helper 1 5432)
; (helper 0 54321)

; Зад.5
(define (palindrome? n)
  (= n (reverse-int n)))

; Зад.6
(define (divisors-sum n)
  (define (helper i sum)
    (cond ((> i n) sum)
          ((= 0 (remainder n i))
             (helper (+ i 1) (+ sum i)))
          (else (helper (+ i 1) sum))))
  ;(if (and (positive? n)
  ;         (integer? n))
      (helper 1 0)
  ;    #f)
)

;зад.7
(define (perfect? n) (= n (- (divisors-sum n) n)))

;зад.8
(define (prime? n)
  (define sqrtn (sqrt n))
  (define (helper i)
    (cond ((> i sqrtn) #t)
          ((= 0 (remainder n i)) #f)
          (else (helper (+ 1 i)))))
  (and (> n 1) (helper 2)))


;зад.9
(define (increasing? n)
  (cond ((< n 10) #t) ; 
        ((< (last-digit n) (last-digit (quotient n 10))) #f)
        (else (increasing? (quotient n 10)))))

; Упражнение 2:

; зад.1
(define (toBinary n)
  (if (= n 0) 0
      (+ (remainder n 2) (* 10 (toBinary (quotient n 2))))))

; Зад.2
(define (toDecimal n)
  (if (= n 0) 0
      (+ (remainder n 10) (* 2 (toDecimal (quotient n 10))))))

; Зад.3
(define (constantly c)
  (lambda (x) c))

; Зад.4
(define (flip f)
  (lambda (x y) (f y x)))

; Зад.5
(define (compliment p)
  (lambda (x) (not(p x))))

; Зад.6
(define (compose f g)
  (lambda (x) (f(g x))))

; Зад.7
(define (repeat n f)
  (if (= n 0) id
  (compose f (repeat (- n 1) f))))

; Зад.10
(define (twist k f g )
  (repeat (/ k 2) (compose f g)))
(define (twist* k f g)
  (if (= k 0) id
      (lambda (x) (f (g ((twist* (- k 2) f g) x))))
      ; алтернативно - (compose f (compose g (twist ...)))
))

; Упражнение 3:
; Зад.0
(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a)(accumulate op nv (next a) b term next))))

; Зад.1
(define (!! n)
  (accumulate *
              1
              (if (odd? n) 1 2)
              n
              id
              (lambda (x) (+ x 2))))
              
; Зад.2
(define (fact n) (accumulate * 1 1 n id 1+))
(define (nchk n k)
  (/ (fact n) (* (fact - n k) (fact k))))

; Зад.3
(define (nchk* n k)
  (accumulate * 1
              0 (- k 1)
              (lambda (i) (/ (- n i) (- k i)))
              1+))

; Зад.4
(define (2^ n)
  (accumulate * 1 1 n (lambda (x) 2) 1+))

; Зад.5
(define (all? p a b)
  (accumulate (lambda (x y) (and x y)) #t a b p 1+))

(define (any?* p a b)
  (accumulate (lambda (x y) (or x y)) #f a b p 1+))

; Зад. 5 1/2:
(define (filter-accum p? op nv a b term next)
  (cond ((> a b) nv)
        ((p? a) (op (term a)
                    (filter-accum p? op nv (next a) b term next)))
        (else       (filter-accum p? op nv (next a) b term next))))

(define (!!* n)
  (filter-accum (if (even? n) even? odd?)
                * 1
                1 n
                id 1+))

; Зад.6
(define (divisors-sum* n)
  (filter-accum (lambda (a) (= 0(remainder n a))) + 0 1 n id 1+))
(define (divisors-sum** n)
  (accumulate + 0
              1 n
              (lambda (x) (if (zero? (remainder n x)) x 0))
              1+))

; Зад.7
(define (count p? a b)
  (filter-accum p? + 0 a b (lambda(x) 1) 1+))
(define (count* p? a b)
  (accumulate + 0 a b (lambda (x) (if (p? x) 1 0)) 1+))

; Упражнение 4&5:
(define head car)
(define tail cdr)
(define (filter p? lst)
  (cond ((null? lst) '())
        ((p? (head lst)) (cons (head lst)
                               (filter p? (tail lst))))
        (else (filter p? (tail lst)))))

; Зад.1
(define (take n lst)
  (if (or (= n 0) (null? lst))
      '()
      (cons (head lst)(take (- n 1)(tail lst)))))

(define (drop n lst)
  (if (or (= n 0) (null? lst))
      lst
      (drop (- n 1) (tail lst))))

; Зад.2
(define (all-lst? p? lst)
  (if (null? lst)
      #t
      (and (p? (head lst)) (all-lst? p? (tail lst)))))

(define (any-lst? p? lst)
  (if (null? lst)
      #f
      (or (p? (head lst)) (all-lst? p? (tail lst)))))

; Зад.3
(define (zip lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (cons (head lst1) (head lst2)) (zip (tail lst1) (tail lst2)))))

; Зад.4
(define (zipWith op lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (op (head lst1) (head lst2)) (zipWith op (tail lst1) (tail lst2)))))

; Зад.5
(define (sorted? lst)
  (if (or (null? lst) (null? (tail lst)))
      #t
      (and (> (head lst) (head (tail lst))) (sorted? (tail lst)))))

; Зад.6
(define (uniques lst)
  (if (null? lst)
      '()
      (cons (head lst) (uniques
                        (filter (lambda (x) (not (equal? x (head lst))))
                                (tail lst))))))

; ---
(define (foldr op nv lst)
  (if (null? lst)
      nv
      (op (head lst)
          (foldr op nv (tail lst)))))

(define (length-foldr lst)
  (foldr (lambda (el res) (+ 1 res))
         0
         lst))
(define (map-foldr f lst)
  (foldr (lambda (el res) (cons (f el) res))
         '()
         lst))
(define (filter-foldr p? lst)
  (foldr (lambda (el res)
           (if (p? el)
               (cons el res)
               res))
         '()
         lst))
(define (uniques*** lst)
  (foldr (lambda (el res)
           (if (member el res) res (cons el res)))
         '()
         lst))
; Зад.7
(define (insert val lst)
  (cond ((null? lst) (list val))
        ((< (head lst) val) (cons (head lst) (insert val (tail lst))))
        (else (cons val lst))))

; Зад.8
(define (insertion-sort lst)
  (foldr insert '() lst))

; Зад.9
(define (isSubinterval i1 i2)
  (and (>= (head i1) (head i2))
       (<= (tail i1) (tail i2))))

(define (intervalSize i) (- (tail i) (head i)))

(define (maxIntervalSize list)
  (if (null? list)
      0
      (max (intervalSize (head list)) (maxIntervalSize (tail list)))))

(define (>-interval i1 i2)
  (if (> (intervalSize i1) (intervalSize i2)) i1 i2))

(define (maxInterval list)
  (foldr >-interval (head list) (tail list)))

(define (longest-interval-subsets lst)
  (define max (maxInterval lst))
  (filter (lambda (x) (isSubinterval x max))
          lst))

; Зад.10
(define (compose-n . fns)
  (foldr compose (lambda (x) x) fns))

; Зад.11
(define (possibleValues f lst)
  (uniques (map f lst)))

(define (group-by f lst)
  (define values (possibleValues f lst))
  (define (group* f lst values)
    (if (or (null? lst)(null? values))
         '()
         (cons
          (cons
           (head values)
           (filter (lambda (x) (equal? (f x) (head values))) lst))
          (group* f lst (tail values)))))
  (group* f lst values))
            

(define (group-by2 f lst)
  (define returned (uniques (map f lst)))
  (define (elements-for x)
    (filter (lambda (el) (equal? x (f el))) lst))
  (map (lambda (x)
         (list x (elements-for x)))
       returned))

; Зад.12
(define (zipWith** f . lsts)
  (if (or (null? lsts) (any-lst? null? lsts))
      '()
      (cons (apply f (map head lsts))
            (apply zipWith** f (map tail lsts)))))



(define (smt f . lsts)
  (apply f (map head lsts)))


; Упражнение 6:

(define m '((1 2 3 4 5 6)
            (2 3 4 5 8 9)
            (0 2 6 4 8 2)
            (5 2 8 4 9 0)))
(define m1 '((3 4 5) (6 7 8)))
(define (head-rows m) (head m))
(define (head-cols m) (map head m))
(define (tail-rows m) (tail m))
(define (tail-cols m) (map tail m))
(define (null-m? m) (or (null? m) (null? (head m))))

; Зад.1
(define (sub-matrix i1 j1 i2 j2 m)
  (define (sub-row i1 i2 lst)(take (- i2 1)(drop (- i1 1) lst)))
  (map (lambda(x) (sub-row i1 i2 x)) (take (- j2 1)(drop (- j1 1) m))))

; Зад.2
(define (foldr-matrix op-rows nv-rows op-elems nv-elems m)
  (foldr op-rows nv-rows
         (map (lambda(x)(foldr op-elems nv-elems x)) m)))

(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3))
           (tree? (cadr t))
           (tree? (caddr t))))
(define empty-tree '())
(define (make-tree root left right) (list root left right))      ; не искаме просто (define make-tree list) - защо?
(define (make-leaf root) (make-tree root empty-tree empty-tree)) ; за удобство
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)

(define test-tree
  (make-tree 10
             (make-tree 7
                        (make-leaf 10)
                        (make-leaf 2))
             (make-tree 3
                        (make-tree 4
                                   (make-leaf 1)
                                   (make-leaf 2))
                        empty-tree)))

; Зад.4
(define (tree-sum t)
  (if (empty-tree? t)
      0
      (+ (root-tree t)
         (tree-sum (left-tree t))
         (tree-sum (right-tree t)))))

; Зад.5
(define (tree-level k t)
  (cond [(empty-tree? t) '()] ; не забравяме дъното
        [(zero? k) (list (root-tree t))]
        [else (append (tree-level (- k 1) (left-tree t))
                      (tree-level (- k 1) (right-tree t)))]))
; Зад.6
(define (height t)
    (if (empty-tree? t)
        0
        (+ 1 (max (height (left-tree t))
                  (height (right-tree t))))))
            
(define (all-levels t)
  (define (helper n)  
    (cond ((empty-tree? t) '())
          ((> n (height t)) '())
          (else (cons (cons n (list(tree-level n t)))
                      (helper (+ 1 n))))))
  (helper 0))


(define (all-levels* t)
  (map (lambda (i) (tree-level i t)) (range 0 (height t))))

; Зад.7
(define(tree-map f t)
  (if (empty-tree? t) t
      (make-tree (f (root-tree t))
                 (tree-map f (left-tree t))
                 (tree-map f (right-tree t)))))

; Зад.8
(define(tree->list t)
  (if (empty-tree? t)
      '()
      (append (tree->list (left-tree t))
              (list(root-tree t))
              (tree->list (right-tree t)))))

;-----------------------
(define (bst-insert val t)
  (cond [(empty-tree? t) (make-leaf val)]
        [(< val (root-tree t))
         (make-tree (root-tree t)
                    (bst-insert val (left-tree t))
                    (right-tree t))]
        [else (make-tree (root-tree t)
                         (left-tree t)
                         (bst-insert val (right-tree t)))]))

; Зад.4
(define (tree-sort lst)
  (foldr bst-insert empty-tree lst))



