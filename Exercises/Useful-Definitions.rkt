#lang racket/base

; Usefull functions
; Lists
(define head car)
(define tail cdr)

(define (any? p? lst)
  (if (null? lst)
      #f
      (or (p? (head lst))
          (any? p? (tail lst)))))

(define (all? p? lst)
  (if (null? lst)
      #t
      (and (p? (head lst))
           (all? p? (tail lst)))))

(define (take n lst)
  (if (= n 0)
      '()
      (cons (head lst) (take (- n 1) (tail lst)))))

(define (drop n lst)
  (if (> n 0)
      (drop (- n 1) (tail lst))
      lst))

;Matrises
(define matrix (list (list 1 2 3 4 5 6 7 8 9)
                     (list 4 5 6 7 8 9 1 2 3)
                     (list 7 8 9 1 2 3 4 5 6)
                     (list 2 3 4 5 6 7 8 9 0)))

(define (head-rows m)(head m))
(define (head-cols m) (map head m))
(define (tail-rows m) (tail m))
(define (tail-cols m)(map tail m))
(define (null-m? m) (or (null? m) (null? (head m))))

;Trees
(define (root-tree t)(car t))
(define (left-tree t) (cadr t))
(define (right-tree t) (caddr t))
(define (empty-tree? t) (null? t))

(define empty-tree '())
(define (make-tree root left right) (list root left right))
(define (make-leaf x)(make tree x empty-tree empty-tree))

(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3))
           (tree? (cadr t))
           (tree? (caddr t))))





      