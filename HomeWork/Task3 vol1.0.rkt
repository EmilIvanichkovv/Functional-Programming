;Помощни функции

(define (char-digit? c)
  (and
   (char>=? c #\0)
   (char<=? c #\9)))

(define (char-spaces? c)
  (if (char=? c #\space) #t #f))

(define (char-op? c)
  (or (char=? c #\+)
      (char=? c #\-)
      (char=? c #\/)
      (char=? c #\*)
      (char=? c #\^)))

(define(beginSymbolsCheck expr index)
  (cond ((= index (string-length expr)) #t)
        ((char-op? (string-ref expr index)) #f)
        ((char-digit? (string-ref expr index)) #t)
        ;((char-spaces? (string-ref expr index)) #t)
        (else (beginSymbolsCheck expr (+ 1 index)))))


(define(endSymbolsCheck expr index)
  (cond ((< (- index 1) 0) #t)
        ((char-digit? (string-ref expr (- index 1))) #t)
        ((char-op? (string-ref expr (- index 1))) #f)
        (else (endSymbolsCheck expr (- index 1)))))

(define(checkFollowinForOp expr index)
  (cond ((= index (string-length expr)) #f)
        ((char-op? (string-ref expr index)) #t)
        ((char-digit? (string-ref expr index)) #f)
        (else (checkFollowinForOp expr (+ 1 index)))))

(define(onlySpacesAferFinalDigit expr index)
  (cond ((= index (string-length expr)) #t)
        ((equal?(char-spaces? (string-ref expr index)) #f) #f)
        (else (onlySpacesAferFinalDigit expr (+ 1 index)))))

(define(afterNumberCheck* expr index)
  (cond ((= (string-length expr) 0) #t )
        ((= (+ 1 index) (string-length expr)) #t)
        ((and (char-digit? (string-ref expr index))
              (char-digit? (string-ref expr (+ 1 index))))
              (afterNumberCheck* expr (+ index 1)))
        
        ((and (char-digit? (string-ref expr index))
              (equal?(char-digit? (string-ref expr (+ 1 index))) #f)
              (equal? (onlySpacesAferFinalDigit expr (+ 1 index)) #t))
         #t)
         
        ((and (char-digit? (string-ref expr index))
              (equal?(char-digit? (string-ref expr (+ 1 index))) #f)
              (equal? (checkFollowinForOp expr (+ 1 index)) #f))
         #f) 
        (else (afterNumberCheck* expr (+ index 1)))))


(define(afterOperationCheck expr index)
   (cond ((= (string-length expr) 0) #t )
         ((= (+ 1 index) (string-length expr)) #t)
         ((and (char-op?(string-ref expr index))
               (equal? (checkFollowinForOp expr (+ 1 index)) #t))
               #f)
         (else (afterOperationCheck expr (+ index 1)))
         ))
         

         
;;------------------------------------------------------------
(define(expr-valid? expr)
(cond ((equal? (beginSymbolsCheck expr 0) #f) #f)
      ((equal? (endSymbolsCheck expr (string-length expr)) #f) #f)
      ((equal? (afterNumberCheck* expr 0) #f) #f)
       ((equal? (afterOperationCheck expr 0) #f) #f)
      (else #t)
      ))
;---------------------------------------------------------------

;
(define (numberOfDigits y)
  (if (= y 0) 0
      (+ 1 (numberOfDigits (quotient y 10)))))

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

(define (stringToNumber string index)
  (if(= index (string-length string))
     0
     (+
      (* (expt 10 (- (string-length string) (+ index 1))) (fromCharToNumber (string-ref string index)))
       (stringToNumber string (+ 1 index)))))

(define(numberToString number)
  (if (= number 0)
      "0"
      (numberToString* number)
      ))

(define(numberToString* number)
  (if(= number 0)
     ""
(cond
    ((= (remainder number 10) 0) (string-append (numberToString*(quotient number 10)) "0"))
    ((= (remainder number 10) 1) (string-append (numberToString*(quotient number 10)) "1"))
    ((= (remainder number 10) 2) (string-append (numberToString*(quotient number 10)) "2"))
    ((= (remainder number 10) 3) (string-append (numberToString*(quotient number 10)) "3"))
    ((= (remainder number 10) 4) (string-append (numberToString*(quotient number 10)) "4"))
    ((= (remainder number 10) 5) (string-append (numberToString*(quotient number 10)) "5"))
    ((= (remainder number 10) 6) (string-append (numberToString*(quotient number 10)) "6"))
    ((= (remainder number 10) 7) (string-append (numberToString*(quotient number 10)) "7"))
    ((= (remainder number 10) 8) (string-append (numberToString*(quotient number 10)) "8"))
    ((= (remainder number 10) 9) (string-append (numberToString*(quotient number 10)) "9")))))


(define(prevNumber expr index index2)
  (cond ((= index 0) (substring expr index index2))
        ((char-op? (string-ref expr (- index 1)))(substring expr index index2))
        (else (prevNumber expr (- index 1) index2))))


(define(nextNumber expr index index2)
  (cond ((= (+ 1 index2) (string-length expr)) (substring expr (+ 1 index)  (+ 1 index2)))
        ((char-op? (string-ref expr (+ index2 1)))(substring expr  (+ 1 index)  (+ 1 index2)))
        (else (nextNumber expr index (+ index2 1)))))


(define (indexOf^ expr index)
  (cond ((= index (string-length expr)) #t);tova znachi che nqma simvol za stepen
        ((char=? (string-ref expr index) #\^) index)
        ((indexOf^ expr (+ 1 index)))))

(define(calculatePower expr)
  (cond ((equal? (indexOf^ expr 0) #t) expr)
        (else
         (expt (stringToNumber (prevNumber expr (indexOf^ expr 0) (indexOf^ expr 0)) 0)
               (stringToNumber (nextNumber expr (indexOf^ expr 0) (indexOf^ expr 0)) 0)))))

(define(append^ expr)
  (cond ((equal? (indexOf^ expr 0) #t) expr)
        (else
         (string-append
           (substring expr
                      0
                      (- (string-length expr)
                         (+(string-length(prevNumber expr (indexOf^ expr 0) (indexOf^ expr 0)))
                           2)))
           
           (numberToString(calculatePower expr))
           
  
           )
           )
         ))


      
 
     
  