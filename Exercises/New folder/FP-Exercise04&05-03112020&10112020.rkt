
#lang racket

;УПРАЖНЕНИЕ 4 | СПИСЪЦИ |

;--------------------------------------------------------
;                    Записки
;--------------------------------------------------------

; 2 3 5

; cons
;2   cons
;   3   cons
;      5     '()

;cat - глава
;cdr - опашка

;Начини за създаване
;(cons 2(cons 3(cons 5 '())))
;'(2 3 5)
;(list 2 3 5)
;(list (+ 2 2) 3 5) -> 4 3 5
;'(2 #f "yey") е валиден списък

;Вградени функции:
;(null? l) проверява празен ли е?
;(length l) Връща дължината.
;(reverse l) - обръща го
;(member x l)- ако х го няма връща лъжа
;             ако го има връща подсписъка в който се съдържа
;(map f l) - изпълнява функцията f върху всеки елемент от l
;(append x y) - слепя два списъка
;(equal? x y) - сравняване



;               За Олеснение:
(define head car);приема списък от неща и връща първото
(define tail cdr);приема списък от неща и връща списък от вс без първото



;;     AKO ТРЯБВА НИЕ ДА ГИ ДЕФИНИРЕАМЕ ТЕ БИХА ИЗГЛЕДАЛ ТАКА:

;(define(length 1st)
 ;  (if(null? lst)0
 ;     (+1 (lenght(crd 1st)))))

;(define(member? x lst)
  ;cond((null? lst)#f)
  ;    ((= (head lst) x) #t)
  ;    ( else(member? x (tail lst)))))
  ;           или
  ;(and (not (null? lst))
  ;      (or (= (head 1st) x)
  ;           (member? x(tail lst)))))


;(define (map f lst)
 ; (if (null? lst) '()
  ;    (cons (f (head 1st))
   ;         (map f (tail lst)))))


;Функция която филтрира - проверява условие и връща списък с
;елементите които го изпълняват
(define (filter p? lst)
  (cond ((null? lst) '())
        ((p?(head lst))(cons (head lst)
                            (filter p? (tail lst))))
        (else (filter p? (tail lst)))))

;Итеративна версия за ф-я за дължина
(define (length* lst)
  (define (helper lst res)
    (if (null? lst) res
        (helper (tail lst) (+ res 1))))
    (helper lst 0))

;----------------------------------------------------------------------
;                    ЗАДАЧИ
;----------------------------------------------------------------------
;Задача1
(define (take n lst)
  (if(or(= n 0)(null? lst))
     '()
     (cons (head lst)
            (take (- n 1) (tail lst)))))
(define (drop n lst)
  (if (or (= n 0) (null? lst))
      lst
      (drop (- n 1) (tail lst))))

;Задача 3
(define (zip lst1 lst2)
  (if (or (null? lst1) (null lst2))
      '()
      (cons (cons (head lst1) (head lst2))
            (zip ((tail lst1) (tail lst2))))))

;Задача 4 ???

;Задача 5
;note: не може да достъпваме head & tail на пазни списъци!!!
;за това проверяваме дали списъка не е с 0 или 1 елемнти
;ако е -> True. В противен случай рекурсивно...
(define (sorted? lst)
  (if (or(null? lst)(null? (tail lst)))
      ;if(< (lenth lst) 2)) Много лоша практика заради рекурсията
      #t
      (and (<= (head lst) (head (tail lst)))
           (sorted? (tail lst)))))

;Ако ни е нужна дължина по време на обработването
;ни трябва помощтна функция
;(define (smt lst)
;  (define (helper lst n) ; Инварианта (lenth lst = n)
;    (...));Важно - да поддържаме инвариантата при рекурсивните извиквания
;  (helper lst(lenth lst)))


;-------------------------------------------------------------------------
;                           ПРОДЪЛЖЕНИЕ 10.11.2020
;--------------------------------------------------------------------------

;задача 6

(define (uniques lst)
  (if (null? lst) '()
      (let [ (rest (uniques (tail lst)))]
        (if (member (read lst) rest)
            rest
            (cons (head lst) rest)))))

(define (uniques* lst)
  (if (null? lst) '()
      (cons (head lst)
            (uniques* (filter (lambda (x) (not (equal? x (head lst))))
                              (tail lst))))))
(define (uniques** lst)
  (define (helper lst res)
    (cond [(null? lst) res]
          [(member (head lst) res) (helper (tail lst) res)]
          [else (helper (tail lst) (cons (head lst) res))]))
  (helper lst '()))        


;Функция Сгъване на списък
(define (foldr op nv lst)
  (if (null? lst) nv
      (op (head lst)
          (foldr op nv (tail lst)))))

;функция за намиране на максимален елемент чрез foldr
(define (maximum lst)
  (foldr max (head lst) (tail lst)))

;функция за намиране на уникалните елементи чрез foldr
(define (uniques*** lst)
  (foldr (lambda (el res)
           (if (member el res) res (cons el res)))
         '()
         lst))
;функция за намиране на дължината чрез foldr
(define (length** lst)
  (foldr (lambda (el res)
           (+ 1 res))
         0
         lst))

;функция за мапване на функция върху елементите на списък чрез foldr
(define (map** f lst)
  (foldr (lambda (el res)
           (cons (f el) res))
         '()
         lst))

;функция за филтриране на елементите на списък чрез foldr
(define (filter* p? lst)
  (foldr (lambda (el res)
           (if (p? el) (cons el res) res))
         '()
         lst))


;задача 7
(define (insert val lst)
  (cond [(null? lst)(list val)]
        [(> val (head lst))(cons (head lst) (insert val (tail lst)))]
        [else (cons val lst)]))

;задача 8 - Insertion Sort
(define (insertion-sort lst)
  (foldr insert
         '()
         lst))

; Quick Sort
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

;задача 10
(define(compose f g)
  (lambda (x) (f (g x))))
(define (1+ x) (+ x 1))
(define (sq x) (* x x))

;задача 11*
(define (group-by f lst)
  ;локална стойност
  (define returned (uniques* (map f lst)))
  ;локална ф-я за махане на дубликатите
  (define (elements-for x)
    (filter (lambda (el) (equal? x (f el))) lst))
  (map (lambda (x)
         (list x (elements-for x)))
       returned))

;(define (compose n fns); (compose-n (list sq 1+))
(define (compose-n . fns); (compose-n sq 1+)
  (foldr compose (lambda (x) x) fns))



;to do:
; -Notes for task6
















  