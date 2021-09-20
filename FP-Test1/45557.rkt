;
; СУ "Св. Климент Охридски"
; Факултет по математика и информатика
; Курс Функционално програмиране 2020/21
; Контролно 1
; 2020-11-07
;
; Начален час на контролното: 9:00
; Име: Емил Иваничков
; ФН: 45557
; Специалност: Информатика
; Курс: 3
; Административна група: 2 / G
;

;12 % 2 0
;6 % 2 0
;3 % 2 1
;1 % 2 1

(define (toBinary n)
  (if ( = n 0) 0
      (+ (remainder n 2) (* 10 (toBinary (quotient n 2))))))

;1010 не работи правилно
(define (toDecimal n)
  (if ( = n 0) 0
      (+ (remainder n 10) (expt 10 (toBinary (quotient n 2))))))

;1101010 4 10000

(define (positionCheck s n)
  (if
   (= (remainder(quotient (toBinary s) (expt 10 n)) 10) 0)
   #f
   #t))
      

;1010 , 1 -> 1010
;1010 , 5 ->101010

; A)

(define (nset-add s elem)
  (if
   (equal? (positionCheck s elem) #t)
   s
  (toDecimal(+ (toBinary s) (expt 10  elem)))))
(define (nset-remove s elem)
  (if (equal? (positionCheck s elem) #f)
      s
      (toDecimal(- (toBinary s) (expt 10  elem)))))

;Б)
(define(nset-size s)
  (if (= s 0)
      0
      (+ (remainder (toBinary s) 10) (nset-size (quotient (toBinary s) 10)))))
;проверка дали (positionCheck s (nset-size s))
;


      
  


         