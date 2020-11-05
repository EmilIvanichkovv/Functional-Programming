;Помощни функции

(define (char-digit? c)
  (and
   (char>=? c #\0)
   (char<=? c #\9)))

(define (char-space? c)
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
        ((equal?(char-space? (string-ref expr index)) #f) #f)
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
         

         

(define(expr-valid? expr)
(cond ((equal? (beginSymbolsCheck expr 0) #f) #f)
      ((equal? (endSymbolsCheck expr (string-length expr)) #f) #f)
      ((equal? (afterNumberCheck* expr 0) #f) #f)
       ((equal? (afterOperationCheck expr 0) #f) #f)
      (else #t)
      ))

      
 
     
  


















