#lang racket

; УПРАЖНЕНИЕ 6 | Структури от данни |


;--------------------------------------------------------
;                    Записки
;--------------------------------------------------------


(define head car)
(define tail cdr)
; map, flter, foldr, any?, all?, drop, take, dropWhile, takeWhile

;(list-ref lst index) - връща елемента на позиция index

(define (dropWhile p? lst)
  (cond [ (null? lst) '()]
        [(p? (head lst)) (dropWhile p? (tail list))]
        [else lst]))

;--------------------------------------------------------
;                    Матрици
;--------------------------------------------------------

(define (head-rows m) (head m))
(define (head-cols m) (map head m))
(define (tail-rows m) (tail m))
(define (tail-cols m) (map tail m))
(define (null-m? m) (or (null? m) (null? (head m))))

; Като списък от своите редове

; 1 2 3    '((1 2 3)
; 4 5 6 ->   (4 5 6)
; 7 8 9      (7 8 9))

(define m '((1 2 3 4 5 6)
            (2 3 4 5 8 9)
            (0 2 6 4 8 2)
            (5 2 8 4 9 0)))

; Обхождане по редове - тривиално
; Обхождане по колони

; (range 1 5) - Прави списък с числата в интервала '(1 2 3 4)


;----------------------------------------------------------------------
;                    ЗАДАЧИ
;----------------------------------------------------------------------


; Задача 1
; Пример:
;  1 2 3 4 5 6
;  2 3(4 5 8)9
;  0 2(6 4 8)2
;  5 2 8 4 9 0
; (sub-matrix 1 2 3 5 ...) -> '((4 5 8) (6 4 8))

; Първо ограничаваме редовете с които ще работим
; После за всеки ред ограничаваме елементите от кой стълб ни трябват
; Правим помощна функция за 'ограничаване' и използвайки drop и take
(define (sub-range i j lst)
  (drop (take lst j) i))

(define (sub-matrix i1 j1 i2 j2 m)
  (let [(the-rows (sub-range i1 i2 m))]
    (map (lambda (row) (sub-range j1 j2 row)) the-rows)))


; Задача 2
(define (foldr-matrix op-rows nv-rows op-elems nv-elems m)
  (foldr op-rows nv-rows
         (map (lambda (row) (foldr op-elems nv-elems row)) m)))
    

; Зад.3
; Упътване: да се имплементира функция, извършваща Гаусова елиминация:
; '(( 2  3  6)     '((2   3  6)
;   ( 1  0  5)  ->   (0 -3/2 2)
;   (-2  5 -4))      (0   8  2))




;----------------------------------------------------------------------
;                    ДВОИЧНИ ДЪРВЕТА
;----------------------------------------------------------------------


; За удобство и консистентност ще използваме
; следните "стандартни" функции за работа с дървета:

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

; Пример:
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

; Функция за височината на дърво
(define (height t)
  (if (empty-tree? t) 0
      (+ 1 (max (height (left-tree t))
                (height (right-tree t))))))




;----------------------------------------------------------------------
;                    ЗАДАЧИ
;----------------------------------------------------------------------

; Задача 4
(define (tree-sum t)
  (cond [(empty-tree? t) 0]
        [else (+ (root-tree t)
                 (tree-sum (left-tree t))
                 (tree-sum (right-tree t)))]))

; Задача 5
(define (tree-level k t)
  (cond [(empty-tree? t) '()] ; не забравяме дъното
        [(zero? k) (list (root-tree t))]
        [else (append (tree-level (- k 1) (left-tree t))
                      (tree-level (- k 1) (right-tree t)))]))


; Задача 6
; Нестандартно решение:
(define (all-levels t)
  (map (lambda (i) (tree-level i t))(range 0 (height t))))

; Рекурсивно решение
; Алтернативно решение със сливане на списъците,
; получени то двете поддървета.
; Помощна функция, аналогична на zipWith

(define (merge-levels lst1 lst2)
  (cond [(null? lst1) lst2]
        [(null? lst2) lst1]
        [else (cons (append (head lst1) (head lst2))
                    (merge-levels (tail lst1) (tail lst2)))]))

(define (all-levels* t)
  (if (empty-tree? t) '()
      (cons (list (root-tree t))
            (merge-levels (all-levels* (left-tree t))
                          (all-levels* (right-tree t))))))

; Задача 7
(define (tree-map f t)
  (if (empty-tree? t) t
      (make-tree (f (root-tree t))
                 (tree-map f (left-tree t))
                 (tree-map f (right-tree t)))))
                 


; Задача 8
(define (tree->list t)
  (if (empty-tree? t) '()
      (append (tree->list (left-tree t))
              (list (root-tree t))
              (tree->list (right-tree t)))))







