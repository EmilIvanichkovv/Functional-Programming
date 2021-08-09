#lang racket/base

(require racket/stream)
(provide (all-defined-out))

; Помощни функции:
; За дървета
(define empty-tree '())
(define (make-tree root left right) (list root left right))     
(define (make-leaf root) (make-tree root empty-tree empty-tree))
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define (leaf? tree)(and (empty-tree? (left-tree tree)) (empty-tree? (right-tree tree))))
(define empty-tree? null?)

;  За конвертиране на низове
; От низ към число
(define(fromCharToNumber c)
 (cond ((char=? c #\0) 0)
       ((char=? c #\1) 1)
       ((char=? c #\2) 2)
       ((char=? c #\3) 3)
       ((char=? c #\4) 4)
       ((char=? c #\5) 5)
       ((char=? c #\6) 6)
       ((char=? c #\7) 7)
       ((char=? c #\8) 8)
       ((char=? c #\9) 9)))

(define (stringToNumber string)
  (define (stringToNumber* string index)
    (if(= index (string-length string))
       0
       (+
        (* (expt 10 (- (string-length string) (+ index 1))) (fromCharToNumber (string-ref string index)))
        (stringToNumber* string (+ 1 index)))))
  (stringToNumber* string 0))

; От число към низ
(define (fromNumberToString c)
  (cond
    ((= c 1) "1")
    ((= c 2) "2")
    ((= c 3) "3")
    ((= c 4) "4")
    ((= c 5) "5")
    ((= c 6) "6")
    ((= c 7) "7")
    ((= c 8) "8")
    ((= c 9) "9")
    ((= c 0) "0")))

(define (numberToString number)
  (define (numberToString* number)
    (if (= number 0)
        ""
        (string-append
         (numberToString* (quotient number 10))
         (fromNumberToString (remainder number 10)))))
  (if (= number 0)
      "0"
      (numberToString* number)))

; За символи
(define (char-digit? c)
  (and
   (char>=? c #\0)
   (char<=? c #\9)))
(define (char-space? c)
  (if (char=? c #\space)
      #t
      #f))
(define (char-operation? c)
  (or (char=? c #\+)
      (char=? c #\-)
      (char=? c #\/) (char=? c #\*)(char=? c #\^)))
(define (char-open-bracket? c)
  (if (char=? c #\{)
      #t
      #f))
(define (char-close-bracket? c)
  (if (char=? c #\})
      #t
      #f))
(define (char-star? c)
  (if (char=? c #\*)
      #t
      #f))


; Други:
(define (1+ x) (+ x 1))



; Помощна функция за проверка дали символен низ е коректно представяне на дърво. 
; Използвам променлива counter която определя "състояние" и спрямо него извършва дадена функционалност
(define (valid-string? string index counter)
  (cond
    [(= index (string-length string)) #f]
    ;преди отваряне на първа срещната скоба
    [(= counter 0) (cond [(char-open-bracket? (string-ref string index)) (valid-string? string (1+ index) (1+ counter))]
                         [else #f])]
        
    ;след първа срещната скоба търсим само цифра и пропускаме спейсовете
    [(= counter 1) (cond [(char-space? (string-ref string index)) (valid-string? string (1+ index) counter)]
                         [(char-digit? (string-ref string index)) (valid-string? string (1+ index) (1+ counter))]
                         [else #f])]
        
    ;след първа срещната цифра тръсим цифра или звезда и пропускаме спейсовете
    [(= counter 2) (cond [(char-digit? (string-ref string index)) (valid-string? string (1+ index) counter)]
                         [(char-space? (string-ref string index)) (valid-string? string (1+ index) (1+ counter))]
                         [(char-star? (string-ref string index)) (valid-string? string (1+ index) (1+ (1+ counter)))]
                         [(char-open-bracket? (string-ref string index))
                          (if (valid-string? string index 0)
                              (valid-string? string
                                             (1+ (valid-string? string index 0))
                                             (1+ (1+ counter)))
                              #f)]     
                         [else #f])]
        
    ;при срещане на повече неразделени цифри търсим звезда и пропускаме спейсовете
    [(= counter 3) (cond [(char-space? (string-ref string index)) (valid-string? string (1+ index) counter)]
                         [(char-star? (string-ref string index)) (valid-string? string (1+ index) (1+ counter))]
                         [(char-open-bracket? (string-ref string index))
                          (if (valid-string? string index 0)
                              (valid-string? string
                                             (1+ (valid-string? string index 0))
                                             (1+ counter))
                              #f)]
                         [else #f])]
        
    ;при звезда търсим втора звезда и пропускаме спейсовете
    [(= counter 4) (cond [(char-space? (string-ref string index)) (valid-string? string (1+ index) counter)]
                         [(char-star? (string-ref string index)) (valid-string? string (1+ index) (1+ counter))]
                         [(char-open-bracket? (string-ref string index))
                          (if (valid-string? string index 0)
                              (valid-string? string
                                             (1+ (valid-string? string index 0))
                                             (1+ counter))
                              #f)]
                         [else #f])]
        
    ;търсим затваряща скоба и пропускаме спейсовете
    [(= counter 5) (cond [(char-space? (string-ref string index)) (valid-string? string (1+ index) counter)]
                         [(char-close-bracket? (string-ref string index)) index]
                         [else #f])]
    ))


; Функция за проверява дали подаденият ѝ като аргумент символен низ е коректно представяне на дърво.                               
(define (tree? str)
  (cond
    [(= (string-length str) 0) #f]
    [(= (string-length str) 1)
     (if(char-star? (string-ref str 0))
        #t
        #f)]
    [else (if (equal? (valid-string? str 0 0) (- (string-length str) 1))
              #t
              #f)]))

;-----------------------------------------------------------------------------------------------------------------
; Помощни функции за създаване на дърво от стринг
; Функция за определяне на идекса на съответана затваряща скоба по зададена отваряща
(define (closeBracketIndex string index br)
  (cond [(= br 0) (- index 1)]
        [(char-open-bracket? (string-ref string index))
         (closeBracketIndex string (1+ index) (1+ br))]
        [(char-close-bracket? (string-ref string index))
         (closeBracketIndex string (1+ index) (- br 1))]
        [else (closeBracketIndex string (1+ index) br)]))

; Функция за създаване на корена на дървото
(define (making-root string)
  (define (making-root* string index)
    (if (char-digit? (string-ref string index))
        (making-root* string (1+ index))
        (substring string 1 index)))
  (making-root* string 1))

; Функция за създаване на лявото поддърво на дървото
(define (making-leftSub string)
  (define (making-leftSub* string index)
    (cond [(char-star? (string-ref string index))
           "*"]
          [(char-open-bracket? (string-ref string index))
           (substring string
                      index
                      (1+ (closeBracketIndex string (1+ index) 1)))]
          [else (making-leftSub* string (1+ index))]))
  (making-leftSub* string (1+ (string-length (making-root string)))))

; Функция за създаване на дясното поддърво на дървото
(define (making-rightSub string)
  (define (making-rightSub* string index)
    (cond [(char-star? (string-ref string index))
           "*"]
          [(char-open-bracket? (string-ref string index))
           (substring string
                      index
                      (1+ (closeBracketIndex string (1+ index) 1)))]
          [else (making-rightSub* string (1+ index))]))
  (making-rightSub* string (+ (string-length (making-root string))
                              (1+ (string-length (making-leftSub string))))))

; Фунция за конвертиране на низа в лист
(define (string->list string lst)
  (append lst
          (list (stringToNumber (making-root string)))         
          (if (equal? (making-leftSub string) "*")
              (append lst (list '()))
              (append lst (list (string->list (making-leftSub string) lst))))
          (if (equal? (making-rightSub string) "*")
              (append lst (list '()))
              (append lst (list (string->list (making-rightSub string) lst))))))
  
; Функция за премахване на спейсовете от подаден низ
(define (space-remover str index)
  (cond [(= index (string-length str)) str]
        [(char-space? (string-ref str index))
         (space-remover (string-append
                         (substring str 0 index)
                         (substring str (+ 1 index) (string-length str)))
                        index)]
        [else (space-remover str (+ 1 index))]))
        
(define (remove-spaces str)
  (space-remover str 0))  


; Функцията за създаване на дърво от стринг
(define (string->tree str)
  (if (not (tree? str))
      #f
      (let [(cleanStr (remove-spaces str))]
        (string->list cleanStr '()))))



;-------------------------------------------------------------------------------------
; Помощни функции за функцията която проверява дали двоичното дърво е балансирано по височина.
; Функция за височината
(define (height t)
  (if (empty-tree? t) 0
      (+ 1 (max (height (left-tree t))
                (height (right-tree t))))))
; Функция за модул
(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

; Функция която проверява дали двоичното дърво е балансирано по височина. 
(define (balanced? t)
  (cond
    [(empty-tree? t) #t]
    [else(and (<= (abs(- (height (left-tree t)) (height (right-tree t)))) 1);za tekusht vryh
              (balanced? (left-tree t))
              (balanced? (right-tree t)))]))


;--------------------------------------------------------------------------------------
; Помощни функции за функцията която проверява дали двоичното дърво tree е двоично наредено дърво.
; Функция която проверява дали всички стойности в лявото поддърво са <= от корена
(define (leftOrdered? tree root)
  (cond
    [(empty-tree? tree) #t]
    [else (and (>= root (root-tree tree))
               (leftOrdered? (left-tree tree) root)
               (leftOrdered? (right-tree tree) root))]))

; Функция която проверява дали всички стойности в дясното поддърво са > от корена
(define (rightOrdered? tree root)
  (cond
    [(empty-tree? tree) #t]
    [else (and (< root (root-tree tree))
               (rightOrdered? (left-tree tree) root)
               (rightOrdered? (right-tree tree) root))]))

; Функция която проверява дали двоичното дърво tree е двоично наредено дърво. 
(define (ordered? tree)
  (cond
    [(empty-tree? tree) #t]
    [else (and (leftOrdered? (left-tree tree) (root-tree tree))
               (rightOrdered? (right-tree tree) (root-tree tree))
               (ordered? (left-tree tree))
               (ordered? (right-tree tree)))]))


;----------------------------------------------------------------------
; Функция която преобразува двоичното дърво tree до неговото представяне като символен низ.
(define (tree->string tree)
  (if (empty-tree? tree)
      "*"
      (string-append
       "{"
       (numberToString (root-tree tree))
       " "
       (tree->string (left-tree tree))
       " "
       (tree->string (right-tree tree))
       "}")))

;----------------------------------------------------------------------
; Функция за: Корен -> Ляво -> Дясно
(define (preorder tree)
  (cond [(empty-tree? tree) '()]
        [else (append (list (root-tree tree))
                      (preorder (left-tree tree))
                      (preorder (right-tree tree)))]))
; Функция за: Ляво -> Корен -> Дясно
(define (inorder tree)
  (cond [(empty-tree? tree)'()]
        [else (append (inorder (left-tree tree))
                      (list (root-tree tree))
                      (inorder (right-tree tree)))]))
; Функция за: Ляво -> Дясно -> Корен
(define (postorder tree)
  (cond [(empty-tree? tree) '()]
        [else(append (postorder (left-tree tree))
                     (postorder (right-tree tree))
                     (list (root-tree tree)))]))

; Функция която преобразува двоичното дърво tree до поток от неговите елементи.
(define (tree->stream tree order)
  (cond
    [(equal? order 'preorder)(stream-first(stream(preorder tree)))]
    [(equal? order 'inorder)(stream-first(stream(inorder tree)))]
    [(equal? order 'postorder)(stream-first(stream(postorder tree)))]
    [else #f]))

;-------------------------------------------------------------------------------
;Опит за функцията за визуализиране двоичното дърво
(define (maxWidth tree)
(define (tree-level-size tree k)
  (define (tree-level k tree)
    (cond [(empty-tree? tree) '()]
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

(define (followingLineKTimes tree k)
  (if (= k 0) (display "")
      (and (followingLineKTimes tree)
       (followingLineKTimes tree (- k 1)))))       

(define (visualize tree)
  (cond [(empty-tree? tree) (display "")]
        [else(and (drawFirstLine tree)
                  (followingLineKTimes tree (+ 1 (max (height (right-tree tree)) (maxWidth (left-tree tree)))))
                  
                  (visualize (left-tree tree)))]))




