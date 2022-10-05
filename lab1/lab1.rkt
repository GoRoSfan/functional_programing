#lang racket

;=================================Task 1==========================
(define (task1 n)
  (define (get_recurtion_depth n i)
    (if (= n 1)
        i
        (get_recurtion_depth (quotient n 2) (+ i 1))
      ))
  (get_recurtion_depth n 0)
  )

(display "Please input number of people in circle\n")
(define amount (string->number (read-line)))

(printf "Recurtion depth: ~a\n"
        (task1 amount))

;=================================Task 2==========================
(define (task2 n)
  (if (= n 1)
      "YES"
      (if (= (remainder n 2) 0)
          (task2 (/ n 2))
          "NO")))

(display "Please input number\n")
(define n (string->number (read-line)))

(task2 n)