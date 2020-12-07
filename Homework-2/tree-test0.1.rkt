#lang racket/base
(require rackunit rackunit/gui)
(require "tree0.1.rkt")

(define (tree-correct str)
  (test-true
   (string-append "Expression ' " str " ' should be correct tree!")
   (tree? str)))

(define (tree-incorrect str)
  (test-false
   (string-append "Expression ' " str " ' should not be correct tree!")
   (tree? str)))


(test/gui
 (test-suite
  "Expression validation "
  (tree-correct "*")
  (tree-correct "{1 * *}")
  (tree-correct "{10 * *}")
  (tree-correct "{9{99**}*}")
  (tree-correct "{9*{99**}}")
  (tree-correct "{15{22**}{5**}}")
  (tree-correct "{111 {3 {8 * *} {6 * {99  * *}}} {900 * {2 {3 * *} *}}}")
  (tree-incorrect " ")
  (tree-incorrect "5 * *")
  (tree-incorrect "{* * *}")
  (tree-incorrect "{* 2 *}")
  (tree-incorrect "6 6 *}")
  (tree-incorrect "{6 6 *")
  (tree-incorrect "{4 22 *}")
  (tree-incorrect "{658 *}")
  (tree-incorrect "{5 * 100}")
  (tree-incorrect "{5 22 100}")
  (tree-incorrect "{9 99**}*}")
  (tree-incorrect "{9*{99**}")
  (tree-incorrect "{6 {*} *}")
  (tree-correct "{  15  {  2  * *  }  {  5 * * }  }   ")
  (tree-correct "  {  15  {  2  * *  }  {  5 * * }  }")))
  ;(tree-incorrect "{2 * *}"))) ;???????

(define (balanced-tree-correct tree)
  (test-true
   (string-append "Tree " (tree->string tree) "should be balanced!")
   (balanced? tree)))

(define (not-balanced-tree-correct tree)
  (test-false
   (string-append "Tree " (tree->string tree) "should not be balanced!")
   (balanced? tree)))

(test/gui
 (test-suite
  "Balanced tree validation "
  (balanced-tree-correct '())
  (balanced-tree-correct '(2 (3 () ()) ()) )
  (balanced-tree-correct '(2 () (3 () ())) )
  (balanced-tree-correct '(2 (3 () ()) (3 () ())) )
  (not-balanced-tree-correct '(2 (3 (4 () ())()) ()))
  (not-balanced-tree-correct '(2 () (3 (4 () ())())) )))

(define (ordered-tree-correct tree)
  (test-true
   (string-append "Tree " (tree->string tree) "should be ordered!")
   (ordered? tree)))

(define (not-ordered-tree-correct tree)
  (test-false
   (string-append "Tree " (tree->string tree) "should not be ordered!")
   (ordered? tree)))

(test/gui
 (test-suite
  "Ordered tree validation "
  (ordered-tree-correct '() )
  (ordered-tree-correct '(1 () ()) )
  (ordered-tree-correct '(20 (13 (9 () ()) (14 () ())) (23 (21 () ()) ())) )
  (not-ordered-tree-correct '(20 (13 (9 () ()) (14 () ())) (23 (19 () ()) ())) )
  (not-ordered-tree-correct '(1 (2 () ()) ()) )
  (not-ordered-tree-correct '(2 (1 () ()) (1 () ())) )))


