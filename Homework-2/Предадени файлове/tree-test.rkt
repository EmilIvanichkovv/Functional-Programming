#lang racket/base

(define (head lst) (car lst))
(define (tail lst) (cdr lst))

(require rackunit rackunit/gui)
(require "tree.rkt")

; Tests for tree?
(define (correct-tree expression)
  (test-true
   (string-append "Expression ' " expression " ' is correct tree!")
   (tree? expression)))

(define (incorrect-tree expression)
  (test-false
   (string-append "Expression ' " expression " ' is not correct tree!")
   (tree? expression)))

; Tests for balanced?
(define (correct-balanced-tree tree)
  (test-true
   (string-append "Tree ' " (tree->string tree) " ' is balanced!")
   (balanced? tree)))

(define (not-correct-balanced-tree tree)
  (test-false
   (string-append "Tree ' " (tree->string tree) " ' is not balanced!")
   (balanced? tree)))

; Tests for ordered?
(define (correct-ordered-tree tree)
  (test-true
   (string-append "Tree ' " (tree->string tree) " ' is ordered!")
   (ordered? tree)))

(define (not-correct-ordered-tree tree)
  (test-false
   (string-append "Tree ' " (tree->string tree) " ' is not ordered!")
   (ordered? tree)))

; Tests for tree->string
(define (correct-tree->string tree str)
  (test-true
   (string-append (tree->string tree) " and " str " should be equal!")
   (equal? (tree->string tree) str)))

(define (not-correct-tree->string tree str)
  (test-false
   (string-append (tree->string tree) " and " str " should not be equal!")
   (equal? (tree->string tree) str)))

; Tests for tree->stream
(define (listConvertor lst)
  (define (listConvertor* lst res)
    (cond [(empty-tree? lst) res]
          [(empty-tree? (tail lst)) (listConvertor* (tail lst) (string-append res (number->string (head lst))))]
          [else (listConvertor* (tail lst) (string-append res (number->string (head lst)) " "))]))
  (listConvertor* lst ""))

(define (correct-tree->streamt tree order lst)
  (test-true
   (cond[(equal? order 'inorder)
         (string-append "Inorder " (tree->string tree) " must be: (" (listConvertor lst) ")")]
        [(equal? order 'preorder)
         (string-append "Preorder " (tree->string tree) " must be: (" (listConvertor lst) ")")]
        [(equal? order 'postorder)
        (string-append "Postorder " (tree->string tree) " must be: (" (listConvertor lst) ")")]
        [else (string-append "Error!")])
   (equal? (tree->stream tree order) lst)))

(define (not-correct-tree->stream tree order lst)
  (test-false
   (cond[(equal? order 'inorder)
         (string-append "Inorder " (tree->string tree) " and (" (listConvertor lst) ") should not be equal")]
        [(equal? order 'preorder)
         (string-append "Preorder " (tree->string tree) " and (" (listConvertor lst) ") should not be equal")]
        [(equal? order 'postorder)
        (string-append "Postorder " (tree->string tree) " and (" (listConvertor lst) ") should not be equal")]
        [else (string-append "Error!")])
   (equal? (tree->stream tree order) lst)))



(test/gui
 (test-suite
  "Expression Tests: "
  (correct-tree "*")
  (correct-tree "{1 * *}")
  (correct-tree "{10 * *}")
  (correct-tree "{9{99**}*}")
  (correct-tree "{9*{99**}}")
  (correct-tree "{15{22**}{5**}}")
  (correct-tree "{111 {3 {8 * *} {6 * {99  * *}}} {900 * {2 {3 * *} *}}}")
  (incorrect-tree " ")
  (incorrect-tree "5 * *")
  (incorrect-tree "{* * *}")
  (incorrect-tree "{* 2 *}")
  (incorrect-tree "6 6 *}")
  (incorrect-tree "{6 6 *")
  (incorrect-tree "{4 22 *}")
  (incorrect-tree "{658 *}")
  (incorrect-tree "{5 * 100}")
  (incorrect-tree "{5 22 100}")
  (incorrect-tree "{9 99**}*}")
  (incorrect-tree "{9*{99**}")
  (incorrect-tree "{6 {*} *}")
  (incorrect-tree "{  15  {  2  * *  }  {  5 * * }  }   ")
  (incorrect-tree "  {  15  {  2  * *  }  {  5 * * }  }"))

 (test-suite
  "Tests for balanced? "
  (correct-balanced-tree '())
  (correct-balanced-tree '(1 (1 () ()) ()) )
  (correct-balanced-tree '(1 () (1 () ())) )
  (correct-balanced-tree '(1 (1 () ()) (1 () ())) )
  (correct-balanced-tree '(1  (1  (1 () ())  (1 () ()))  (1  (1 () ())  (1 () ()))))
  (not-correct-balanced-tree '(1 (1 (1 () ()) ())()) )
  (not-correct-balanced-tree '(1 () (1 (1 () ())())) )
  (not-correct-balanced-tree '(1 (1 (1 () ()) () ) (1 (1 (1 () () ) () ) () ) ) ))

 (test-suite
  "Tests for ordered? " 
  (correct-ordered-tree '() )
  (correct-ordered-tree '(1 () ()) )
  (correct-ordered-tree '(2 (1 () ()) (3 () ())))
  (correct-ordered-tree '(9 (7 (6 () ()) (8 () ())) (13 (11 () ()) ())) )
  (not-correct-ordered-tree '(1 (1 () ()) (1 () ())) )
  (not-correct-ordered-tree '(112 (2224 () ()) ()) )
  (not-correct-ordered-tree '(20 (12 () ()) (12 () ())) )
  (not-correct-ordered-tree '(65 (54 () ()) (222 () (3 () ()))) )
  (not-correct-ordered-tree '(5 (123 (5 () ()) (7 () ())) (57 (41 () ()) ())) ))

 (test-suite
  "Tree to string:"
  (correct-tree->string '() "*")
  (correct-tree->string '(1(1 () ()) ()) "{1 {1 * *} *}")
  (correct-tree->string '(1 (1 () ()) (1 ()())) "{1 {1 * *} {1 * *}}")
  (not-correct-tree->string '(1(1 () ()) ()) "{1{1 * *} *}")
  (not-correct-tree->string '(1(1 () ()) ()) "{1 {1 * *} *}    ")
  (not-correct-tree->string '(1 (1 () ()) (1 ()())) "{1 {1 * *}{1 * *}}"))

 (test-suite
  "Tests for tree->stream: "
  (correct-tree->streamt '() 'inorder '())
  (correct-tree->streamt '() 'postorder '())
  (correct-tree->streamt '() 'preorder '())
  (correct-tree->streamt '(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ())))
                         'inorder '(2 22 6 5 1 111 3))
  (correct-tree->streamt '(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ())))
                         'postorder '(2 6 22 111 3 1 5))
  (correct-tree->streamt '(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ())))
                         'preorder '(5 22 2 6 1 3 111))
  (not-correct-tree->stream '(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ())))
                            'inorder '(5 22 2 6 1 3 111))
  (not-correct-tree->stream '(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ())))
                            'postorder '(2 22 6 5 1 111 3))
  (not-correct-tree->stream '(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ())))
                            'preorder '(2 6 22 111 3 1 5))

  (not-correct-tree->stream '(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ())))
                            inorder '(5 22 2 6 1 3 111))
  )
 )
   
         

