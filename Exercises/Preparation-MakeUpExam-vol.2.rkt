#lang racket
(define (last-digit n) (remainder n 10))
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

; zad 3
(define (count-digit d n)
  (define (helper cnt n)
    (cond ((= n 0) cnt)
          ((= (last-digit n) d)
           (helper (+ 1 cnt) (quotient n 10)))
          (else (helper cnt (quotient n 10)))))
  
  (helper 0 n))

; zad 4
(define (rev-int n)
  (define (helper cur newN)
    (if (= newN 0)
        cur
        (helper (+ (last-digit newN) (* 10 cur)) (quotient newN 10))))
  (helper 0 n))

; zad 5
(define (palindrome? n)
  (= (rev-int n) n))

; zad 6
(define (divisors-sum n)
  (define (helper i sum)
    (cond ((> i n) sum)
          ((= 0 (remainder n i)) (helper (1+ i) (+ sum i)))
          (else (helper (1+ i) sum))))
  (helper 1 0))

; zad 7
(define (perfect? n)
  (define sum (- (divisors-sum n) n))
  (= sum n))

;zad 8
(define (prime? n)
  (define sqrtN (sqrt n))
  (define (helper i)
    (cond ((> i sqrtN) #t)
          ((= 0 (remainder n i)) #f)
          (else (helper (1+ i)))))
   (and (> n 1) (helper 2)))

;zad 9
(define (increasing? n)
    (cond ((< n 10) #t)
          ((< (last-digit n) (last-digit (quotient n 10))) #f)
          (else (increasing? (quotient n 10)))))

;zad 10
(define (toBinary n)
  (if (= n 0)
      0
      (+ (remainder n 2) (* 10 (toBinary (quotient n 2))))))
;zad 11
(define (toDecimal n)
  (if (= n 0)
      0
      (+ (remainder n 10) (* 2 (toDecimal (quotient n 10))))))

;Uprajneni 2
;zad 3
(define (constantly c)
  (lambda (x) c))

;zad4
(define (flip f)
  (lambda (x y) (f y x)))
;zad 5
(define (complement p)
  (lambda (x) (not (p x))))
;zad 6
(define (compose f g)
  (lambda (x) (f(g x))))
;zad 7
(define (repeat n f)
  (if (= 0 n) id
      (compose f (repeat (- n 1) f))))
;zad10
(define (twist k f g)
  (define half (/ k 2))
  (repeat half (compose f g)))

; uprajnenie3
(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a)
          (accumulate op nv (next a) b term next))))
;zad 1
(define (!! n)
  (accumulate *
              1
              (if (odd? n) 1 2)
              n
              id
              (lambda (x) (+ 2 x))))
;zad 4
(define (2^* n)
  (accumulate *
              1
              1
              n
              (lambda (x) 2)
              1+))
;zad 5
(define (all? p? a b)
  (accumulate (lambda (x y) (and x y))
              #t
              a
              b
              p?
              1+))
(define (any? p? a b)
  (accumulate (lambda (x y) (or x y))
              #f
              a
              b
              p?
              1+))
;zad 5 1/2
(define (filter-accum p? op nv a b term next)
  (cond ((> a b) nv)
        ((p? a) (op (term a)
                    (filter-accum p? op nv (next a) b term next)))
        (else       (filter-accum p? op nv (next a) b term next))))


;зад 6
(define (divisors-sum* n)
  (filter-accum (lambda (a) (= 0 (remainder n a)))
                +
                0
                1
                n
                id
                1+))
;zad 7
(define (trueForCount p? a b)
  (filter-accum p?
                +
                0
                a
                b
                (lambda (x) 1)
                +1))
; Упражнение 4&5:
(define head car)
(define tail cdr)
; zad 1
(define (take* n lst)
  (if (or (= 0 n) (null? lst))
      '()
      (cons (head lst) (take* (- n 1) (tail lst)))))
(define (drop* n lst)
  (if (or (= 0 n) (null? lst))
      lst
      (drop* (- n 1) (tail lst))))
;zad 2
(define (listAll? p? lst)
  (if (null? lst) #t
      (and (p? (head lst)) (listAll? p? (tail lst)))))
(define (listAny? p? lst)
  (if (null? lst) #f
      (or (p? (head lst)) (listAll? p? (tail lst)))))

;zad3
(define (zip lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (cons (head lst1) (head lst2))
            (zip (tail lst1) (tail lst2)))))
;zad 4
(define (zipWith f lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (f (head lst1) (head lst2)) (zipWith f (tail lst1) (tail lst2)))))
;zad 5
(define (sorted? lst)
  (cond ((or (null? lst) (null? (tail lst))) #t)
        ((< (head lst) (head(tail lst))) #f)
        (else (sorted? (tail lst)))))
;zad 6
(define (filter p? lst)
  (if (null? lst)
      '()
      (if (p? (head lst))
          (cons (head lst) (filter p? (tail lst)))
          (filter p? (tail lst)))))
(define (uniques lst)
  (if (null? lst)
      '()
      (cons
       (head lst)
       (uniques (filter
                 (lambda (x) (not (equal? (head lst) x)))
                 (tail lst))))))
      


;zad 7
(define (insert val lst)
  (if (null? lst)
      (cons val lst)
      (if (< val (head lst))
          (cons val lst)
          (cons (head lst) (insert val (tail lst))))))

;zad8
(define (insertion-sort lst)
  (define (helper list res)
    (if (null? list)
        res
        (helper (tail list) (insert (head list) res))))
  (helper lst '()))
;----------
(define (foldr op nv lst)
  (if (null? lst) nv
      (op (head lst)
          (foldr op nv (tail lst)))))

(define (length-foldr lst)
  (foldr
         (lambda (x res) (1+ res))
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

;zad8 with foldr
(define (insertion-sort* lst)
  (foldr insert '() lst))
;zad9
(define (interval-length lst)
  (- (tail lst) (head lst)))

(define (biggestInterval list)
  (define updatedList (map-foldr interval-length list))
  (define biggestIntervalSize
    (foldr
     (lambda (x res) (max x res))
     0
     updatedList))
  (filter-foldr
   (lambda (x) (= biggestIntervalSize (interval-length x)))
   list))

(define (subInterval? lst1 lst2)
  (and (>= (head lst1) (head lst2))
       (<= (tail lst1) (tail lst2))))

(define (longest-interval-subsets list)
  (define bI (biggestInterval list))
  (filter
   (lambda (x) (subInterval? x (head bI)))
   list))

;zad 11
(define (possibleResults f lst)
  (uniques (map f lst)))
(define (group-by f lst)
  (define returned (possibleResults f lst))
  (define (elemets-for-x x)
    (filter (lambda (el) (equal? (f el) x))
    lst))
  (map (lambda (x) (list x (elemets-for-x x))) returned))

;zad 12

(define (zipWith* f . lsts)
  (if (or (null? lsts) (listAny? null? lsts))
      '()
      (cons (apply f (map head lsts))
            (apply zipWith* f (map tail lsts)))))

; uprajnenie 6
(define (head-rows m) (head m))
(define (head-cols m) (map head m))
(define (tail-rows m) (tail m))
(define (tail-cols m) (map tail m))
(define (null-m? m) (or (null? m) (null? (head m))))

(define m '((1 2 3 4 5 6)
            (2 3 4 5 8 9)
            (0 2 6 4 8 2)
            (5 2 8 4 9 0)))

; zad 1
(define (sub-matrix i1 j1 i2 j2 lst)
  (define m1 (take* j2 (drop* j1 lst)))
  (map (lambda (x) (take* i2 (drop* i1 x)))
       m1))
;zad 2
(define (foldr-matrix op-rows nv-rows op-elems nv-elems m)
  (foldr op-rows nv-rows
         (map (lambda (x) (foldr op-elems nv-elems x)) m)))

(define empty-tree '())
(define (make-tree root left right)(list root left right))
(define (make-leaf root) (make-tree root empty-tree empty-tree))
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3)
           (tree? left-tree)
           (tree? right-tree))))
(define empty-tree? null?)

(define test-tree
  (make-tree 20
             (make-tree 7
                        (make-leaf 10)
                        (make-leaf 2))
             (make-tree 300
                        (make-tree 4
                                   (make-leaf 1)
                                   (make-leaf 2))
                        empty-tree)))
(define isBTS
  (make-tree 2
           (make-tree 1
                   empty-tree
                   empty-tree)
           (make-leaf 3)))

;zad 4
(define (tree-sum t)
  (if (empty-tree? t)
      0
      (+ (root-tree t) (tree-sum  (left-tree t)) (tree-sum (right-tree t)))))

;zad 5
(define (tree-level k t)
  (cond ((empty-tree? t) '())
        ((= k 0) (list (root-tree t)))
        (else (append (tree-level (- k 1) (left-tree t))
                      (tree-level (- k 1) (right-tree t))))))
;zad6
(define (height t)
  (cond ((empty-tree? t) 0)
        (else (+ 1 (max (height (left-tree t))
                        (height (right-tree t)))))))
(define (all-levels t)
  (define h (- (height t) 1))
  (define (helper cur)
    (cond
      ((empty-tree? t) '())
      ((> cur h)'())
      (else(cons (cons cur (list(tree-level cur t)))
                 (helper (+ 1 cur))))))
  (helper 0))
;zad 7
(define (map-tree f t)
  (cond ((empty-tree? t) '())
        (else (make-tree (f (root-tree t))
                         (map-tree f (left-tree t))
                         (map-tree f (right-tree t))))))
;zad 8
(define (tree->list t)
  (if (empty-tree? t)
      '()
      (append 
              (tree->list (left-tree t))
              (list (root-tree t))
              (tree->list (right-tree t)))))

;zad 9
(define (bts-insert val t)
  (cond ((empty-tree? t) (make-leaf val))
        ((< val (root-tree t))
         (make-tree (root-tree t) (bts-insert val (left-tree t)) (right-tree t)))
        (else
         (make-tree (root-tree t) (left-tree t) (bts-insert val (right-tree t))))))
;zad 10
(define (tree-sort t)
  (define lst (tree->list t))
  (define ordered-tree (foldr bts-insert '() lst))
  (tree->list ordered-tree))

;zad 11
(define (valid-bts? t)
  (define curTree (tree->list t))
  (define sortedTree (tree-sort t))
  (equal? curTree sortedTree))

;zad 12
(define (is-leaf? t)
  (cond ((empty-tree? t) #f)
        (else (and (empty-tree? (left-tree t))
                   (empty-tree? (right-tree t))))))
(define (prune t)
  (cond ((empty-tree? t) '())
        ((is-leaf? t) '())
        (else (make-tree (root-tree t)
                         (prune (left-tree t))
                         (prune (right-tree t))))))


(define (prune* t)
  (cond [(is-leaf? t) empty-tree] ; няма наследници (листо)
        [(empty-tree? (right-tree t)) ; само ляв наследник
         (make-tree (root-tree t)
                    (prune* (left-tree t))
                    empty-tree)]
        [(empty-tree? (left-tree t)) ; само десен наследник
         (make-tree (root-tree t)
                    empty-tree
                    (prune* (right-tree t)))]
        [else (make-tree (root-tree t) ; два наследника
                         (prune* (left-tree t))

                         (prune* (right-tree t)))]))


;zad 13
(define (bloom t)
  (cond ((empty-tree? t) '())
        ((is-leaf? t) (make-tree (root-tree t)
                                 (make-leaf (root-tree t))
                                 (make-leaf (root-tree t))))
        (else (make-tree (root-tree t)
                         (bloom (left-tree t))
                         (bloom (right-tree t))))))

;zad 14
(define (getMin t)
  (cond ((empty-tree? t) 1111)
        ((empty-tree? (left-tree t))
                      (apply min (tree->list (right-tree t))))
        ((empty-tree? (right-tree t))
                      (apply min (tree->list (left-tree t))))
        (else
         (min (apply min (tree->list (left-tree t)))
              (apply min (tree->list (right-tree t)))))))

(define (getMax t)
   (cond ((empty-tree? t) 1111)
        ((empty-tree? (left-tree t))
                      (apply max (tree->list (right-tree t))))
        ((empty-tree? (right-tree t))
                      (apply max (tree->list (left-tree t))))
        (else
         (max (apply max (tree->list (left-tree t)))
              (apply max (tree->list (right-tree t)))))))

(define (av a b) (/ (+ a b) 2))
(define (avg t)
  (cond ((or (is-leaf? t)) t)
        ((empty-tree? t) t)
        (else
         (make-tree (av (getMin t) (getMax t))
                    (avg (left-tree t))
                    (avg (right-tree t))))))

;prajnenie 7
;zad 9*
(define (path-code t)
  (define (helper res)
    (cond ((empty-tree? t) res)
          ((root-tree t) (1+ res))
          ((left-tree t) (+ (* 10 res) 1))
          ((right-tree t) (+ (* 10 res) 1))))
  (helper 0))
        

