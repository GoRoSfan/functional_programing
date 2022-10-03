#lang racket

;=================================Task 1==========================
(define (task1 n)
  (define (iteration n i)
    (define (get_circle_last n i)
      (cond
        [(= n 1) (list 1 i)]
        [(= (remainder n 2) 0) (list (- (* 2 (car (get_circle_last (/ n 2) i))) 1) (+ (car (cdr (get_circle_last (/ n 2) i))) 1))]
        [else (list (+ (* 2 (car (get_circle_last (quotient n 2) (+ i 1)))) 1) (+ (car (cdr (get_circle_last (quotient n 2) i))) 1))]
        )
      )
    (define x (car (get_circle_last n i)))
    (define y (car (get_circle_last x i)))
    (cond
      [(= x 1) (car (cdr (get_circle_last n i)))]
      [(= x y) (car (cdr (get_circle_last x i)))]
      [else (iteration x (+ i (car (cdr (get_circle_last x i)))))] 
   
      ))
  (iteration n 0))

(display "Please input number of people in circle\n")
(define amount (string->number (read-line)))

(printf "Recurtion depth: ~a\n"
        (task1 amount))

;=================================Task 2==========================
(define (task2 n)
  (if (= n 1)
      "is power of 2"
      (if (= (remainder n 2) 0)
          (task2 (/ n 2))
          "is not power of 2")))

(display "Please input number\n")
(define n (string->number (read-line)))

(printf "~a ~a\n"
        n
        (task2 n))