
; помощни функции

;Функция която конвертира множество от десетичен в двуичен вид
(define (convertToBinary dSet)
  (if (= dSet 0) 0
      (+ (remainder dSet 2) (* 10 (convertToBinary (quotient dSet 2))))))

;Функция която конвертира множество от двуичен в десетичен вид
(define (convertToDecimal n)
  (if (= n 0) 0
      (+ (remainder n 10) (* 2 (convertToDecimal (quotient n 10))))))


;функция за проверка на позицията 
(define (positionCheck binaryNumber digit)
  (remainder (quotient binaryNumber (expt 10 digit)) 2))

;функции които конвертират " фалшиво " множество в истинско (в двуичен вид)
(define (intersectConverter fakeSet)
     (if (= fakeSet 0) 0
         (+ (if (= (remainder fakeSet 10) 2) 1 0) (* 10 (intersectConverter (quotient fakeSet 10))))))

(define (unionConverter fakeSet)
     (if (= fakeSet 0) 0
         (+ (if (or (= (remainder fakeSet 10) 2) (= (remainder fakeSet 10) 1)) 1 0) (* 10 (unionConverter (quotient fakeSet 10))))))

;функция за разликата на две множества
(define (differenceConverter set1 set2)
  (if (= set1 0)
      0
      (+ (if (= (remainder set1 10) (remainder set2 10))
             0
             (remainder set1 10))
         (* 10 (differenceConverter (quotient set1 10) (quotient set2 10))))))

;функция която дава броя на цифрите на двуично число
(define (digitsCounter x)
  (if (= x 0) 0
      (+ 1 (digitsCounter (quotient x 10)))))

;функция която дава броя на едениците на двуично число
(define (counterOf1s x)
  (if (= x 0)
      0
      (+ (remainder x 2) (counterOf1s (quotient x 10)))))



;Добавяне на елемент към множеството
(define (set-add set element)
 (if (> 0 element)
      #f
     (if (> element (digitsCounter (convertToBinary set)))
         (convertToDecimal (+ (expt 10 element) (convertToBinary set)))
         (if (= (positionCheck (convertToBinary set) element) 0)
             (convertToDecimal (+ (expt 10 element) (convertToBinary set)))
             set))))

;Премахване на елемент от множеството
(define (set-remove set element)
   (if (or (> element (digitsCounter (convertToBinary set)))
           (> 0 element)
           (= (positionCheck (convertToBinary set) element) 0))
       #f
       (convertToDecimal (- (convertToBinary set) (expt 10  element)))))

;Проверка дали елемент принадележи на множество
(define (set-contains set element)
  (if (> 0 element)
      #f
      (if  (= (positionCheck (convertToBinary set) element) 1)
           #t
           #f)))
;Проверка дали множество е празно
(define (set-empty set)
  (if (= (digitsCounter (convertToBinary set)) 0)
              #t
              #f
              ))

;Проверка на размер на дадено множество
(define (set-size set)
  (counterOf1s (convertToBinary set)))

;Сечение на две множества
(define (set-intersect s1 s2)
  (define psevdoSum (+ (convertToBinary s1) (convertToBinary s2)))
   (convertToDecimal (intersectConverter psevdoSum)))

;Ообединение на две множества
(define (set-union s1 s2)
   (define psevdoSum (+ (convertToBinary s1) (convertToBinary s2)))
     (convertToDecimal (unionConverter psevdoSum)))

;Разлика на две множества
(define (set-difference s1 s2)
        (convertToDecimal (differenceConverter (convertToBinary s1) (convertToBinary s2))))
        


