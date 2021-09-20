;
; СУ "Св. Климент Охридски"
; Факултет по математика и информатика
; Курс Функционално програмиране 2020/21
; Поправителна сесия 2020/21
;
; Име: Емил Иваничков 
; ФН: 45557
; Специалност: Информатика
; Курс: 3
; Административна група: 2
; Дата: 16.08.2021
; Начален час на контролното за вашата група: 9:00 
;

#lang racket/base
(define empty-tree '())
(define (make-tree root left right)
  (list root left right))
(define (make-leaf root)
  (list root empty-tree empty-tree))
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)
(define (leaf? tree)
  (and (not (null? (root-tree tree)))
       (null? (left-tree tree))
       (null? (right-tree tree))))

(define head car)
(define tail cdr)

(define testTree
  (make-tree "птица"
             (make-tree "лети"
                        (make-tree "голямо"
                                   (make-leaf "кондор")
                                   (make-leaf "колибри"))
                        (make-leaf "пингвин"))
             (make-tree "насекомо"
                        (make-tree "лети"
                                   (make-leaf "муха")
                                   (make-leaf "мравка"))
                        (make-tree "голямо"
                                   (make-leaf "мечка")
                                   empty-tree))))

(define propsBoboar (list  (cons "плува" #t)
                           (cons "голямо" #f)
                           (cons "птица" #f)
                           (cons "кафяво" #t)
                           (cons "насекомо" #f) ))
(define notValidStraus (list (cons"птица" #t)
                             (cons"лети" #f)))

; A) За решението на задачата ще използвам по горе дефинираната имплеметация на дърво
; чрез списъци. Листата му че са животните, а останалите възли ще са свойства.
; Първият подсписък на всеки списък ще представлява лявата част на дървото, а вторият - дясната
; Б)
(define (allAnimals tree)
  (define (helper nTree res)
    (cond ((empty-tree? nTree) res)
          ((leaf? nTree) (cons (root-tree nTree) res))
          (else (append (helper  (left-tree nTree) res)
                        (helper  (right-tree nTree) res)))))
  (helper tree '()))

(define (animalChar animal tree)
  (define (helper ntree res)
    (cond ((empty-tree? ntree) '())
          ((equal? (root-tree ntree) animal) res)
          (else (append (helper (left-tree ntree) (append res (list(list (root-tree ntree) #t))))
                        (helper (right-tree ntree) (append res (list(list (root-tree ntree) #f))))))))
  (helper tree '()))

(define (allAnimalChar tree)
  (define all-animals (allAnimals tree))
  (map (lambda (x) (list x (animalChar x tree))) all-animals))

(define (filterForOneProp lst prop)
  (filter (lambda (x) (member (list prop #t) (cadr x))) lst))

(define (all-with tree props)
  (define all-animals  (allAnimalChar tree))
  (define (helper props res)
    (if (null? props)
        res
        (helper (cdr props) (filterForOneProp res (car props)))))
  (helper props all-animals))

; В)
(define (addProps props animal tree)
  (cond ((null? props) tree)
        ((empty-tree? tree)
         (newAnimalSubtree animal props  ))
        ((assoc (root-tree tree) props)
         (if (equal? (cdr (assoc(root-tree tree) props)) #t)
             (make-tree (root-tree tree)
                        (addProps (remove (assoc (root-tree tree) props) props) animal (left-tree tree))
                        (right-tree tree))
             
             (make-tree (root-tree tree)
                        (left-tree tree)
                        (addProps (remove (assoc (root-tree tree) props) props) animal (right-tree tree)))))
        (else #f)))

             
(define (newAnimalSubtree animal props)
  (cond (( null? props ) animal)
        (else   (if (equal? (tail(head props)) #t)
                    (make-tree (head (head props))
                               (newAnimalSubtree  animal (tail props) )
                               empty-tree)
                    
                     (make-tree (head (head props))
                                 empty-tree
                                 (newAnimalSubtree animal (tail props) ))))))

(define (validation props animal tree)
  (cond 
    ((member animal (allAnimals tree)) #f)
    ((and (null? props)
          (not (empty-tree? tree))) #f)
    ((not (empty-tree? tree))
     (if (equal? (tail(head props)) #t)
              (validation  (tail props) animal (left-tree tree))
              (validation  (tail props) animal (right-tree tree))))
    (else #t)))
               
(define (add-animal animal props tree)
  (if (validation props animal tree)
      (addProps props animal tree)
      #f))
         
