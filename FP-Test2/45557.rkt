;
; СУ "Св. Климент Охридски"
; Факултет по математика и информатика
; Курс Функционално програмиране 2020/21
; Контролно 2
; 2020-12-12
;
; Име: Емил Иваничков 
; ФН: 45557
; Специалност: Информатика
; Курс: 3
; Административна група: 2
; Начален час на контролното: 9:45
;

#lang racket/base
(provide (all-defined-out)) 


; Това е примерна функция. Можете да напишете решението си на нейното място.
(define (sample-function x) x)

(define notBalanced (list 5
      (list 22
            (list 2 '() '())
            (list 6 '() '()))
      (list 1
            '()
            (list 3
                  (list 111
                        (list 12
                              (list 5 '() '())
                              '())
                        '())
                  '()))))

(define balanced(list 5
                      (list 22
                            (list 2 '() '())
                            (list 6 '() '()))
                      (list 1
                            '()
                            (list 3
                                  (list 111 '() '())
                                  '()))))

(define head car)
(define tail cdr)
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define (1+ x) (+ x 1))


;Задача1:
(define (abs x)
  (if (< x 0 )
      (- 0 x)
      x))

(define (elCount tree)
  (cond [(null? tree) 0]
        [else (+ 1 (elCount (left-tree tree))
                   (elCount (right-tree tree)))]))

(define(weight-balanced? tree)
  (if (null? tree)
      #t
      (if (and
           
           (<= (abs(- (elCount (left-tree tree)) (elCount (right-tree tree)))) 1)
           (weight-balanced? (left-tree tree))
           (weight-balanced? (left-tree tree)))
          #t
          #f)))
;Задача2:
(define res (list (list 12345 "a" 5)
                  (list 12345 "b" 6)
                  (list 12345 "b" 6)
                  (list 54321 "a" 5)
                  (list 54321 "b" 6)
                  (list 54321 "a" 5)
                  (list 54321 "b" 6)
                  (list 54321 "c" 6)))
; филтър по предмет
(define (sbjFilter sbj res)
  (filter (lambda (x) (equal? (head(tail x)) sbj)) res))
;изважда фн
(define (fn lst)
  (if (null? lst)
      '()
      (map (lambda (x)  (head x)) lst)))
;оставя уникалните фн
(define (uniques lst)
  (foldr (lambda (x res) (if (member x res)
                             res
                           (cons x res))) '() lst))
;брои явяванията
(define (timesCount fn lst)
  (foldr (lambda (x res) (if (equal? (head x) fn)
                             (1+ res)
                             res))
         0
         lst))


(define (attempts subj res)
  
  (map (lambda (a) (append (cons (uniques(fn(sbjFilter subj a)))
                                 (map (lambda (x) (timesCount x (sbjFilter subj a)))
                                      (uniques(fn(sbjFilter subj a))))))) res))
  
          
  



















