#lang racket

  
;УПРАЖНЕНИЕ 4 | СПИСЪЦИ |

;--------------------------------------------------------
;                    Записки
;--------------------------------------------------------


;Наредена двойка:
; (cons 4 "iei")
; (define p (cons 4 "iei")
; (car p) -> първи елемент
; (cdr p) -> втори елемент
;Наредена тройка:
; (define q (cons 2 (cons 3 #t)))
; (car (cdr q)) за достъпване на по вътрешен елемнт
; (cadr q) - вложени извиквания ( от ляво на дясно)
; (cddr q) - втори елемнт на втория елемент
;Атом за празен списък:
; '()

;Списък - наредена двойка от 1 елемент (глава)
;и списък с останалите елементи ) (опашка)

;Пример
; (cons 2 (cons 3 (cons 5 '())))
; '(2 3 5) - (за хардкодване)"цитиране" 
; (list (+ 2 2) 3 5) - за по-сложни списъци

;Вградени функции:
; (null? l) - Проверява дали списък е празен
; (lenght l) - Дължина
; (reverse l) - Обръща списък
; (member l) - Проверява дали нещо се съдържа в списък и връща
;  подсписъкът в който се съдържа
; (map f l) - Прилага функцията над всеки от елементите и връща
;  нов списък със приложена функцията
; (filter p? list) - Прилага предиката над списъка и връща списък
;  за които той е верен от елементите
; (append l1 l2) - Слепя два списъка
; (equal? l1 l2) - Сравнява два списъка


;Наши Дефиниции:

;Lenght:
 (define (length? lst)
   (if (null? lst) 0
       (+ 1 (length (cdr lst)))))

 (define (legth?* lst)
  (define (helper lst res)
    (if (null? lst)
        res
        (helper (tail lst) (+ res 1))))
 (helper lst 0))

;Member:
 (define (member? x lst)
  ; (cond ((null? lst) #f)
  ;       (( = (head lst) x) lst)
  ;       (else (member? x (tail lst)))))
  ;      или
   (and (not (null? lst))
        (or (= (head lst) x)
            (member? x (tail lst)))))

;Map:
 (define (map? f lst)
   (if (null? lst) '()
       (cons (f (head lst))
             (map? f (tail lst)))))

 (define (map* f lst)
  (define (helper lst res)
    (if (null? lst)
        res
        (helper (tail lst)
                 ;(cons (f (head lst)) res))))  Така новия би бил на обратно
                (append res (list (f (head lst)))))))
 (helper lst '()))


;Filter:
 (define (filter? p? lst)
   (cond ((null lst) '())
         ((p? (head lst)) (cons (head lst)
                                (filter p? (tail lst))))
         (else (filter p? (tail lst)))))
                         
        

;Помощни функции:
(define head car) ;Приема списък и връща първия елемент
(define tail cdr) ;Приема списък и връща всичко без първото

;----------------------------------------------------------------------
;                    ЗАДАЧИ
;----------------------------------------------------------------------


;Задача 1
(define (take n lst)
  (if (or (= n 0) (null? lst))
      '()
      (cons (head lst) (take (- n 1) (tail lst)))))
      
(define (drop n lst)
  (if (or (= n 0) (null? lst))
      lst
      (drop (- n 1) (tail lst))))

;Задача 2
(define (all? p? lst)
  (if (or (null? lst)
          (and (p? (head lst))
               (all? p? (tail lst))))
      #t
      #f))

(define (any? p? lst)
  (and (not (null? lst))
      (or (p? (head lst))
          (any? p? (tail lst)))))

;Задача 3
(define (zip lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (cons (head lst1) (head lst2))
            (zip (tail lst1) (tail lst2)))))
;Задача 4
(define (zipWith f lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (f (head lst1) (head lst2))
            (zipWith f (tail lst1) (tail lst2)))))

;Задачи 3 и 4 от Дренски
(define (zipWith* f lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (f (head lst1) (head lst2))
            (zipWith* f (tail lst1) (tail lst2)))))

(define (zip* lst1 lst2)
  (zipWith* cons lst1 lst2))

;Задача 5
(define (sorted? lst)
  (or (null? lst)
      (null? (tail lst))
      (and (<= (head lst) (head (tail lst)))
           (sorted? (tail lst)))))

; Пример: ако трябва да използваме дължината на
; подадения списък на всяка итерация.
   ;(define (neshto lst)
   ;  (define (helper lst n) ; Инварианта: (length lst) = n
   ;    (...)) ; Важно - да поддържаме инвариантата при рекурсивните извиквания
   ;  (helper lst (length lst)))

          
;--------------------------------------------------------------------------------          
;УПРАЖНЕНИЕ 5 | СПИСЪЦИ - ПРОДЪЛЖЕНИЕ

;--------------------------------------------------------
;                    Записки
;--------------------------------------------------------

;Foldr
; Обобщено "насъбиране" на елементи от списък:
; "комбинираме" главата на дадения списък с
; резултата от насъбирането на опашката му.
(define (foldr op nv lst)
  (if (null? lst) nv
      (op (head lst)
          (foldr op nv (tail lst)))))

;Примерни Приложения:
(define (maximum lst)
  (foldr max (head lst) (tail lst)))
  ;(foldr max -inf.0 lst))

(define (length** lst)
  (foldr (lambda (el res) (+ 1 res))
         0
         lst))

(define (map** f lst)
  (foldr (lambda (el res) (cons (f el) res))
         '()
         lst))

(define (filter* p? lst)
  (foldr (lambda (el res)
           (if (p? el) (cons el res) res))
         '()
         lst))

         
;----------------------------------------------------------------------
;                    ЗАДАЧИ
;----------------------------------------------------------------------



;Задача 6
(define (uniques lst)
  (if (null? lst)
      '()
      (let [(rest (uniques (tail lst)))] 
      (if (member (head lst) rest)
          rest
          (cons (head lst) rest)))))

; По-просто рекурсивно решение използващо filter
(define (uniques* lst)
  (if (null? lst)
      '()
      (cons (head lst)
            (uniques* (filter (lambda (x) (not (equal? x (head lst))))
                             (tail lst))))))

; Итеративно решение
(define (uniques** lst)
  (define (helper lst res)
    (cond [(null? lst) res]
          [(member (head lst) res) (helper (tail lst) res)]
          [else (helper (tail lst) (cons (head lst) res))]))
  (helper lst '()))

; Решение чрез foldr
(define (uniques*** lst)
  (foldr (lambda (el res)
           (if (member el res) res (cons el res)))
         '()
         lst))


;Задача 7
(define (insert val lst)
  (cond [(null? lst) (list val)]
        [(> val (head lst)) (cons (head lst) (insert val (tail lst)))]
        [else (cons val lst)]))

;Задача 8
;Insertion Sort
(define (insertion-sort lst)
  (foldr insert '() lst))
;QuickSort
(define (quicksort lst)
  (if (or (null? lst) (null? (tail lst)))
      lst
      (let [(pivot (head lst))
            (rest (tail lst))]
      (append (quicksort (filter (lambda (x) (< x pivot))
                                rest))
              (list pivot)
              (quicksort (filter (lambda (x) (>= x pivot))
                                 rest))))))



; Зад.9
; дали i1 е подинтервал на i2
(define (sub? i1 i2)
  (and (>= (head i1) (head i2))
       (<= (tail i1) (tail i2))))
; дължина на интервал
(define (int-length i)
  (- (tail i) (head i)))
; като >, но за интервали :)
(define (>-int i1 i2)
  (if (> (int-length i1) (int-length i2)) i1 i2))
; максимален по дължина интервал
(define (max-interval lst)
  ; При търсене на мин/макс взимаме първия елемент
  ; като първоначален и обхождаме останалите.
  (foldr >-int (head lst) (tail lst)))

(define (longest-interval-subsets lst)
  (define longest (max-interval lst)) ; за да не го преизчисляваме непрекъснато
  (filter (lambda (i) (sub? i longest)) lst))



;Задача 10
(define (compose f g)
  (lambda (x) (f (g x))))
(define (1+ x) (+ x 1))
(define (sq x) (* x x))
(define (^3 x) (* x x x))

;(define (compose-n fns) ; compose-n (list sq 1+)
(define (compose-n . fns) ; compose-n sq 1+
  (foldr compose (lambda (x) x) fns))

;(define (myFunction x y . rest))

; (compose-n sq 1+ ^3)
; (foldr compose id '(sq 1+ ^3))
; (compose sq (foldr compose id '(1+ ^3))
; (compose sq (compose 1+ (foldr compose id '(^3)))
; ...
; (compose sq (compose 1+ (compose ^3 id)))
; (compose sq (compose 1+ (lambda (x) (^3 (id x)))
; Тук ползваме, че ((lambda (x) (f x)) y) <=> (f y)
; (compose sq (lambda (x) (1+ (^3 (id x)))
; (sq (1+ (^3 (id x))))


;Задача 11
(define (group-by f lst)
  (define returned (uniques (map f lst)))
  (define (elements-for x)
    (filter (lambda (el) (equal? x (f el))) lst))
  (map (lambda (x)
         (list x (elements-for x)))
       returned))


; Зад.12
; Функция, която приема произволен брой аргументи, но поне един (!)
; apply is our best friend here
(define (zipWith** f . lsts)
  (if (or (null? lsts) (any? null? lsts))
      '()
      (cons (apply f (map head lsts))
            (apply zipWith* f (map tail lsts)))))

  











       


