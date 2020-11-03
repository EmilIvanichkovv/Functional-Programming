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
           (sorted (tail lst))))

;Ако ни е нужна дължина по време на обработването
;ни трябва помощтна функция
(define (smt lst)
  (define (helper lst n) ; Инварианта (lenth lst = n)
    (...))
  (helper lst(lenth lst)))



















  