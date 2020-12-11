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
        
;6.2
; Напишете функция (foldr-matrix op-rows nv-rows op-elems nv-elems m), която свива матрицата m,
; като прилага двуместната функция op-elems върху елементите във всеки ред, с начална стойност nv-elems.
; Резултатът от свиването на всеки от редовете се насъбира от двуместната функция op-rows, с начална стойност
; nv-rows
(define (foldr-matrix op-rows nv-rows op-elems nv-elems m)
  (define newM (map (lambda (row) (foldr op-elems nv-elems row)) m))
  (foldr op-rows nv-rows newM))
(define (foldr-matrix* op-rows nv-rows op-elems nv-elems m)
  (if (null? m)
      nv-rows
      (op-rows (foldr op-elems nv-elems (head m))
               (foldr-matrix* op-rows nv-rows op-elems nv-elems (tail m)))))

;6. oт Дренски
; Дали матрица е валидно судоку
; Число да не се повтаря в ред и стълб
(define (uniques? lst)
  (define uniqueList (uniques lst))
  (if (null? lst)
      #t
      (= (length lst) (length uniqueList))))
(define (sudoku? m)
  (if (null-m? m)
      #t
      (and (uniques? (head-rows m))
           (uniques? (head-cols m))
           (sudoku? (tail-rows m))
           (sudoku? (tail-cols m)))))

;7.1,1/3
; Дадена е матрица m от числа и списък ps от едноместни числови предикати.
; Да се напише функция (find-submatrix ps m), която намира най-голямата квадратна подматрица
; на m такава, че всеки неин елемент удовлетворява поне един от предикатите в списъка ps.
(define (fromTo int1 int2)
  (if (> int1 int2)
      '()
      (cons int1 (fromTo (+ 1 int1) int2))))

(define (pCheckElem ps elem)
  (if (null? ps )
      #f
      (or ((head ps) elem)
           (pCheckElem (tail ps) elem))))

(define (pCheckList ps lst)
  (if (null? lst)
      #t
      (and (pCheckElem ps (head lst))
           (pCheckList ps (tail lst)))))

(define(pCheckMatrix ps m)
  (if (null-m? m)
      #t
      (and (pCheckList ps (head-rows m))
           (pCheckList ps (head-cols m))
           (pCheckMatrix ps (tail-rows m))
           (pCheckMatrix ps (tail-cols m)))))

; funciq za vs zymojni gorni levi koordinati
(define (allUpLeftCoords m)
  (define numRows (length  m))
  (define numCols (length (head m)))
  (apply append (map (lambda (row) 
                     (map (lambda (col) (list row col)) 
                          (fromTo 0 (- numCols 1))))
                   (fromTo 0 (- numRows 1)))))
; da napravq kvadratni podmatrici
(define (allCoords m)
  (define numRows (length  m))
  (define numCols (length (head m)))
  
  (define (maxFarCoord coord)
    (min (- numRows (head coord)) (- numCols (head(tail coord)))))
  
  (define (addExtention coord n)
    (map (lambda (x) (+ x n)) coord))
  (apply append (map (lambda (curr)
                        (map (lambda (fuck) (list curr (addExtention curr fuck)))
                             (fromTo 0 (- (maxFarCoord curr) 1))))
                        (allUpLeftCoords m))))
;..............
;7.2
; Два реда на матрица наричаме “линейно (не)зависими”, ако елементите на единия ред (не) могат да се получат
; при умножение на елементите на другия ред с едно и също число.
; Да се дефинира функция (dependent l1 l2), която проверява дали
; списъците от числа с еднаква ненулева дължина l1 и l2 са линейно зависими.
(define (dependent l1 l2)
  (define (dependent* l1 l2 q)
    (if (null? l1)
        #t
        (if (= (/ (head l1) (head l2)) q)
            (dependent* (tail l1) (tail l2) q)
            #f)))
  (if (= (length l1) (length l2))
      (dependent* (tail l1) (tail l2) (/ (head l1) (head l2)))
      #f))

;7.2,1/2
; “Псевдоранг” на матрица наричаме максималния брой нейни редове, които два по два са линейно независими.
; Да се дефинира функция pseudorank m, която пресмята псевдоранга на дадена матрица m от положителни цели числа.
(define (checkFollowing list m)
  (if (null-m? m)
      #f
      (or (dependent list (head m))
           (checkFollowing list (tail m)))))

(define (pseudorank m)
  (define (pseudorankCounter m res)
    (if (null-m? m) res
        (if (checkFollowing (head m) (tail m))
            (pseudorankCounter (tail m) res)
            (pseudorankCounter (tail m) (1+ res)))))
  (pseudorankCounter m 0))
      
      

; ДВУИЧНИ ДЪРВЕТА

(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3))
           (tree? (cadr t))
           (tree? (caddr t))))

(define empty-tree '())
(define (make-tree root left right) (list root left right))      
(define (make-leaf root) (make-tree root empty-tree empty-tree))

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
(define balanced-tree
  (make-tree 10
             (make-tree 5
                        (make-leaf 1)
                        (make-leaf 6))
             (make-tree 11
                        (make-leaf 12)
                        empty-tree)))

;6.4
; Да се напише функция (tree-sum t), която намира сумата на всички елементи на дървото t.
(define (tree-sum t)
    (if (empty-tree? t)
        0
        (+ (root-tree t)
           (tree-sum (left-tree t))
           (tree-sum (right-tree t)))))

;6.5
; Да се напише функция (tree-level k t), която връща списък от
; всички стойности във възли на дълбочина k (тоест разстояние k от корена).
(define (tree-level k t)
  (cond
    [(empty-tree? t) '()]
    [(= k 0)(list (root-tree t))]
    [else(append
          (tree-level (- k 1) (left-tree t))
          (tree-level (- k 1) (right-tree t) ))]))

;6.6
; Да се напише функция (all-levels t), която връща списък от всички нива на дървото t,
; започвайки от нулевото надолу.
(define (height t)
  (if (empty-tree? t)
      0
      (max (+ 1 (height (left-tree t)))
           (+ 1 (height (right-tree t))))))

(define (all-levels t)
  (define (all-levels* t cur res)
    (cond [(empty-tree? t) '()]
          [(= cur (height t)) res]
          [else (all-levels* t (1+ cur) (append res (list(tree-level cur t))))]))
  (all-levels* t 0 '()))

;6.7
; Да се напише функция (tree-map f t), която map-ва функцията f на всички стойности в дървото t.
(define (tree-map f t)
  (if (empty-tree? t)
      '()
      (make-tree (f (root-tree t)) (tree-map f (left-tree t)) (tree-map f (right-tree t)))))
      
;6.8
; Да се напише функция (tree->list t), която връща списък от всички елементи на дървото,
; получени при обхождане ляво-корен-дясно.

(define (tree->list t)
  (if (empty-tree? t)
      '()
      (append (tree->list (left-tree t))
              (list (root-tree t))
              (tree->list (right-tree t)))))

;6.9
; Да се напише функция (bst-insert val t), която вмъква стойността val в двоичното наредено дърво t
(define(bst-insert val t)
  (if (empty-tree? t)
      (make-leaf val)
      (if (< val (root-tree t))
          (make-tree (root-tree t)(bst-insert val (left-tree t)) (right-tree t))
          (make-tree (root-tree t)(left-tree t)(bst-insert val (right-tree t))))))

;6.10
; Да се напише функция (tree-sort lst), която сортира списъка lst, използвайки предишните две функции.
(define (tree-sort lst)
  (define (sortListWithTree lst tree)
    (if (null? lst)
        tree
        (sortListWithTree (tail lst) (bst-insert (head lst) tree))))
  (define newTree (sortListWithTree lst '()))
  (tree->list newTree))

;6.11
; Да се напише функция (valid-bst? t), която проверява дали дървото t е валидно двоично наредено дърво.
(define (checkForLeft curr  t)
  (if (empty-tree? t)
      #t
      (and (>= curr (root-tree t))
           (checkForLeft curr (left-tree t))
           (checkForLeft curr (right-tree t)))))
(define (checkForRight curr  t)
  (if (empty-tree? t)
      #t
      (and (< curr (root-tree t))
           (checkForRight curr (left-tree t))
           (checkForRight curr (right-tree t)))))


(define (valid-bts? t)
  (if (or (empty-tree? t) (empty-tree? (left-tree t)) (empty-tree? (right-tree t)))
      #t
      (and (checkForLeft (root-tree t) (left-tree t))
           (checkForRight (root-tree t) (right-tree t))
           (valid-bts? (left-tree t))
           (valid-bts? (right-tree t)))))

;6.12
; Да се напише функция (prune t), която премахва всички листа в дървото t.
(define (isLeaf t)
  (and (empty-tree? (left-tree t)) (empty-tree? (right-tree t))))
(define (prune t)
  (if (empty-tree? t)
      '()
      (if (isLeaf t)
          '()
          (make-tree (root-tree t)(prune (left-tree t)) (prune (right-tree t))))))
      
;6.13
; Да се напише функция (bloom t), която заменя всяко листо със стойност x със следното дърво:
(define (bloom t)
  (if (empty-tree? t)
      '()
      (if (isLeaf t)
          (make-tree (root-tree t)(make-leaf (root-tree t))(make-leaf (root-tree t)))
          (make-tree (root-tree t)(bloom (left-tree t)) (bloom (right-tree t))))))
;6.14
; Да се напише функция (avg t), която заменя всяка стойност във възлите на дадено дърво със средно-аритметичното
; на максималната и минималната стойност в поддървото с корен съотвеетния възел.
(define (avg* t)
  (define treeElems (tree->list t))
  (define avarage (/ (foldr + 0 treeElems) (length treeElems)))
  avarage)

(define (maxEl t)
  (if(empty-tree? t)
     -1000
     (max (root-tree t)
          (maxEl (left-tree t))
          (maxEl (right-tree t)))))

(define (minEl t)
  (if(empty-tree? t)
     1000
     (min (root-tree t)
          (minEl (left-tree t))
          (minEl (right-tree t)))))

(define (avgReal t)
  (/ (+ (maxEl t) (minEl t)) 2))

(define (avg t)
  (if (empty-tree? t)
      '()
      (make-tree (avgReal t) (avg (left-tree t)) (avg (right-tree t)))))

  
