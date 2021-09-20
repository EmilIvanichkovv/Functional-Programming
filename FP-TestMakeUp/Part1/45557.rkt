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
; Начален час на контролното за вашата група: 8:00 
;

#lang racket
(require racket/stream)

(define (takeSize n)
  (if (= n 0) 1
      (* 10 (takeSize (quotient n 10)))))
(define(contains n m)
  (define nSize (takeSize n))
  (if (= m 0) #f
      (or (= n (remainder m nSize))
          (contains n (quotient m 10)))))


(define (nats-from n)
  (stream-cons n (nats-from (+ n 1))))
(define (run p n)
(cond [(> n 0) (printf "~a: ~a\n" n (stream-first p))
(run (stream-rest p) (- n 1))]
[else (display "End\n")]))


(define (stream-filter pred? s)
  (cond
    [(stream-empty? s)
     empty-stream]
    [(pred? (stream-first s))
     (stream-cons (stream-first s)
                  (stream-filter pred? (stream-rest s)))]
    [else (stream-filter pred? (stream-rest s))]
    ))


(define (patern-stream n)
 (stream-filter (lambda (x) (contains n x)) (nats-from n)))