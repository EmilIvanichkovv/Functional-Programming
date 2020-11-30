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
 ;(define (length? lst)
 ;  (if (null? lst) 0
 ;      (+ 1 (length (cdr lst)))))

 ;(define (legth* lst)
 ; (define (helper lst res)
 ;   (if (null? lst)
 ;       res
 ;       (helper (tail lst) (+ res 1))))
 ;(helper lst 0))

;Member:
 ;(define (member? x lst)
 ; ; (cond ((null? lst) #f)
 ; ;       (( = (head lst) x) lst)
 ; ;       (else (member? x (tail lst)))))
 ;       или
 ;  (and (not (null? lst))
 ;       (or (= (head lst) x)
 ;           (member? x (tail lst)))))

;Map:
 ;(define (map? f lst)
 ;  (if (null? lst) '()
 ;      (cons (f (head lst))
 ;            (map? f (tail lst)))))

 ;(define (map* f lst)
 ; (define (helper lst res)
 ;   (if (null? lst)
 ;       res
 ;       (helper (tail lst)
                 ;(cons (f (head lst)) res))))  Така новия би бил на обратно
 ;               (append res (list (f (head lst)))))))
 ;(helper lst '()))


;Filter:
 ;(define (filter? p? list)
 ;  (cond ((null lst) '())
 ;        ((p? (head lst)) (cons (head lst)
 ;                               (filter p? (tail lst))))
 ;        (else (filter p? (tail lst)))))
                         
        

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

          
          
;УПРАЖНЕНИЕ 5 | СПИСЪЦИ - ПРОДЪЛЖЕНИЕ


      
      


















