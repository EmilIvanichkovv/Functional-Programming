#lang racket/base
(define head car)
(define tail cdr)
(define (1+ x) (+ 1 x))

;Списъци


;4.1
; Да се напишат функциите (take n lst) и (drop n lst),
; който съответно взимат или премахват първите n елемента на списък:
(define (take n lst)
  (if (= n 0)
      '()
      (append (list (head lst)) (take (- n 1) (tail lst)))))

(define (drop n lst)
  (if (> n 0)
      (drop (- n 1) (tail lst))
      lst))

;4.2
; Да се напишат функциите (all? p? lst) и (any? p? lst),
; които проверяват съответно дали всички или някои елементи на даден списък изпълняват предиката p?
(define (all? p lst)
  (cond [(null? lst) #t]
        [else(and (p (head lst))
                  (all? p (tail lst)))]))

(define (any? p lst)
  (cond [(null? lst) #f]
        [else (or (p (head lst))
                  (any? p (tail lst)))]))

;4.3
; Да се напише функция (zip lst1 lst2), която приема два списъка
; и връща списък от наредени двойки от техните съответни елементи:
(define(zip lst1 lst2)
  (cond [(or (null? lst1) (null? lst2)) '()]
        [else (cons (cons (head lst1) (head lst2))
                    (zip (tail lst1) (tail lst2)))]))

;4.4
; Да се напише функция (zipWith f lst1 lst2), която връща списъка, получен от прилагането на f
; върху съответните елементи на двата списъка lst1 и lst2.
(define(zipWith f lst1 lst2)
  (cond [(or (null? lst1) (null? lst2)) '()]
        (else (cons (f (head lst1) (head lst2))
                    (zipWith f (tail lst1) (tail lst2))))))

;4.5
; Да се напише функция (sorted? lst), която проверява дали списък е сортиран в ненамаляващ ред.
(define (sorted? lst)
  (cond [(or (null? lst) (null? (tail lst))) #t]
        [else(and (<= (head lst) (head (tail lst)))
              (sorted? (tail lst)))]))

;4.6
; Да се напише функция (uniques lst), която оставя само уникалните стойности в даден списък.
; Можете да проверявате за еднаквост с equal? за най-сигурно.
(define (contains elem lst)
  (cond [(null? lst) #f]
        [(equal? elem (head lst)) #t]
        [else (contains elem (tail lst))]))

(define (uniques lst)
  (cond  [(or (null? lst) (null? (tail lst))) lst]
         [(contains (head lst) (tail lst)) (uniques (tail lst))]
         [else (cons (head lst) (uniques (tail lst)))]))
             
;4.7
; Да се напише функция (insert val lst), която вмъква стойността val на правилното
; място в сортирания в ненамаляващ ред списък lst:
(define (insertHelper val lst result)
  (cond [(null?  lst) (append result (list val))]
        [else(if (>= val (head lst))
                 (insertHelper val (tail lst) (append result (list(head lst))))
                 (append result (list val) lst))]))
     
(define (insert val lst)
  (cond [(null?  lst) (list val)]
        [else (insertHelper val lst '())]))

;4.8
; Да се напише функция (insertion-sort lst), която прави точно това, което подсказва името ѝ:
(define (insertionSort lst)
  (define (insertionSort* lst result)
    (cond [(null? lst) result]
          [else (insertionSort* (tail lst) (insert (head lst) result))]))
  (insertionSort* lst '()))

;4.9
; Да се напише функция longest-interval-subsets, която по даден списък от интервали il
; връща нов списък, който съдържа всички интервали от il,
; които са подинтервали на най-дългия интервал в списъка.
(define(interval-length interval)
  (- (tail interval) (head interval)))

(define (longest-interval il)
  (cond [(null? il) '()]
        [(null? (tail il)) (head il)]
        [(> (interval-length (head il)) (interval-length(longest-interval (tail il)))) (head il)]
        [else  (longest-interval (tail il))]))

(define (isSubset set1 set2)
  (if (null? set1)
      #f
      (if(and (<= (head set2) (head set1))
              (>= (tail set2) (tail set1)))
         #t
         #f)))
             
(define (longest-interval-subsets il)
  (define maxSet (longest-interval il))
    (define (longest-interval-subsets* il res)
    (cond [(null? il) res]
          [(isSubset (head il) maxSet)(longest-interval-subsets* (tail il) (append res (list(head il))))]
          [else (longest-interval-subsets* (tail il) res)]))
  (longest-interval-subsets* il '()))

;4.10
;4.11
;4.12

; foldr
(define (foldr op nv lst)
  (if (null? lst) nv
      (op (head lst)
          (foldr op nv (tail lst)))))

; Examples for foldr:
(define (lenght* lst)
  (foldr (lambda (el res) (+ res 1))
         0
         lst))
(define (map* f lst)
  (foldr (lambda (el res) (cons (f el) res))
         '()
         lst))
(define (filter* p lst)
  (foldr (lambda (el res) (if (p el)
                              (cons el res)
                              res))
         '()
         lst))


;5.6
(define (uniques* lst)
  (foldr (lambda (el res) (if (member el res) res (cons el res)))
         '()
         lst))

;5.8
(define (insertionSort* lst)
  (foldr insert
         '()
         lst))

;5.9
(define (intervalLength interval)
  (- (tail interval) (head interval)))

(define (>forIntervals int1 int2)
  (if (> (intervalLength int1) (intervalLength int2)) int1 int2))

(define (longestInterval intList)
  (foldr >forIntervals (head intList) (tail intList)))

(define (isSubset* set1 set2)
  (and (<= (head set2) (head set1))
       (>= (tail set2) (tail set1))))

(define (longest-interval-subset* il)
  (define longestSet (longestInterval il))
  (filter (lambda (el) (isSubset el longestSet)) il))

;5.10
; Да се напише функция (compose . fns),
; която приема произволен брой функции като аргументи и връща тяхната композиция:
(define (compose f g)
  (lambda (x) (f (g x))))
(define (id x) x)
(define (sq x) (* x x))
(define (^3 x) (* x x x))

;(define (compose-n fns) ; Би се извиквало така (compose-n (list sq 1+))
(define (compose-n . fns) ; Би се извиквало така (compose-n sq 1+ ^3) (може да се каже че съставя списък)
  (foldr compose id fns))

;5.11*
; Да се напише функция (group-by f lst), която групира елементите на списъка lst по стойността,
; която f връща за тях:
(define (group-by f lst)
 (map (lambda (x) (list x (filter (lambda (el) (equal? x (f el))) lst)))
      (uniques (map f lst))))

; cleaner
(define (group-by* f lst)
  (define cases (uniques (map f lst)))
  (define (toBeGroupedBy case)  (filter (lambda (el) (equal? case (f el))) lst))
  (map (lambda (case) (list case (toBeGroupedBy case))) cases))
         
;5.12*
(define (zipWith* f . lsts)
  (if (or (null? lsts) (any? null? lsts))
      '()
      (cons (apply f (map head lsts))
            (apply zipWith* f (map tail lsts)))))


;Матрици

(define (head-rows m) (head m))
(define (head-cols m) (map head m))
(define (tail-rows m) (tail m))
(define (tail-cols m) (map tail m))
(define (null-m? m) (or (null? m) (null? (head m))))

(define matrix (list (list 1 2 3 4 5 6 7 8 9)
                     (list 4 5 6 7 8 9 1 2 3)
                     (list 7 8 9 1 2 3 4 5 6)
                     (list 2 3 4 5 6 7 8 9 0)))

;6.1
; Да се напише функция (sub-matrix i1 j1 i2 j2 m),
; която намира подматрицата на m със зададени горен ляв и долен десен ъгъл (i1,j1) и (i2,j2), съответно.
(define (sub-matrix x1 y1 x2 y2 m)
  (define (helper x1 x2 subMatrix)
    (map (lambda (x) (take (1+ (- x2 x1)) (drop x1 x)))
         subMatrix))
  (helper x1 x2 (take (1+(- y2 y1)) (drop y1 m))))
        




  
