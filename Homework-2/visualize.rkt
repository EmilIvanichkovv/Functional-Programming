#lang racket/base

(define empty-tree '())
(define (make-tree root left right) (list root left right))     
(define (make-leaf root) (make-tree root empty-tree empty-tree))
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)
(define (leaf? tree)(and (empty-tree? (left-tree tree)) (empty-tree? (right-tree tree))))
(define (height t) ;връща височината на дървото
  (if (empty-tree? t) 0
      (+ 1 (max (height (left-tree t)) (height (right-tree t))))))


(define test-tree
  (make-tree 5
             (make-tree 7
                        (make-leaf 2)
                        (make-leaf 6))
             (make-tree 1
                        empty-tree
                        (make-tree 3
                                   (make-leaf 9)
                                   empty-tree)
                        )))
(define test-tree2
  (make-tree 5
             (make-tree 7
                        (make-leaf 2)
                        (make-leaf 6))
             (make-tree 1
                        (make-tree 3
                                   (make-leaf 4)
                                   (make-leaf 5))
                        (make-tree 3
                                   (make-leaf 9)
                                   empty-tree)
                        )))



(define (maxWidth tree)
(define (tree-level-size tree k)
  (define (tree-level k tree)
    (cond [(empty-tree? tree) '()] ;не забравяме дъното
          [(zero? k) (list (root-tree tree))]
          [else (append (tree-level (- k 1) (left-tree tree))
                        (tree-level (- k 1) (right-tree tree)))]))
  (length (tree-level k tree)))

  (define (maxW tree k)
    (cond ((= k -1) 0)
          (else (max
                 (tree-level-size tree k)
                 (maxW tree (- k 1))))))
  (maxW tree (height tree)))

(define (display- k)
  (if (= k 0)
      (display "")
      (and

       (display #\-)
       (display- (- k 1)))))

(define (displaySpaces k)
  (if (= k 0)
      (display "")
      (and

       (display #\space)
       (displaySpaces (- k 1)))))

(define (drawFirstLine tree)
  (cond [(empty-tree? (right-tree tree))
         (display (root-tree tree))
         (display #\newline)]
        [else
         (display (root-tree tree))
         (display- (+ 1 (max (height (right-tree tree)) (maxWidth (left-tree tree)))))
         (drawFirstLine (right-tree tree))]))

(define (followingLine tree)
  (cond [(leaf? tree) (display #\newline)]
        [(empty-tree? (right-tree tree))(display #\|)(display #\newline)]
                                        
        [(empty-tree? (left-tree tree))
         (display #\space)
         (displaySpaces (+ 1 (max (height (right-tree tree)) (maxWidth (left-tree tree)))))
         (followingLine (right-tree tree))]
        [else
         (display #\|)
         (displaySpaces (+ 1 (max (height (right-tree tree)) (maxWidth (left-tree tree)))))
         (followingLine (right-tree tree))]))

(define (k->fL tree k)
  (if (= k 0) (display "")
      (and (followingLine tree)
       (k->fL tree (- k 1)))))       

(define (f tree)
  (cond [(empty-tree? tree) (display "")]
        [else(and (drawFirstLine tree)
                  (k->fL tree (+ 1 (max (height (right-tree tree)) (maxWidth (left-tree tree)))))
                  
                  (f (left-tree tree)))]))