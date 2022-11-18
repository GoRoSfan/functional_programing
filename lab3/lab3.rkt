#lang racket

;----------------Task 1--------------
"Task 1"
(define (abs x)
  (if (positive? x )
       x
      (- x)))

(define (close-enough? x y)
      (< (abs (- x y)) 0.001))

(define (funcd x)
  (+ 2 (sin x))
)

(define (func x)
  (- (* 2 x) (cos x))
)

(define (newton x)
  (let ((res (- x (/ (func x) (funcd x)))))
    (if (close-enough? res x)
        res
        (newton res)
    )
  )
)

(define (simple-i x)
  (let ((res (- x (* 0.3519 (func x)))))
    (if (close-enough? res x)
        res
        (simple-i res)
    )
  )
)

"Delta"
(- (newton 1) (simple-i 1))

;-------------------Task 2-------------
"Task 2"
(define (func2 x)
  (sin (- x (cos x)))
)

"Simpsona"
(+ ((lambda (x h) (* (/ h 3) (+ (func2 x) (* 4 (func2 (+ x h))) (func2 (+ x (* 2 h))))))0 (/ pi 4))
   ((lambda (x h) (* (/ h 3) (+ (func2 x) (* 4 (func2 (+ x h))) (func2 (+ x (* 2 h))))))(/ pi 2) (/ pi 4))
)

"Right rectangles"
(* (/ pi 4) (+ (func2 (/ pi 4))(func2 (/ pi 2))(func2 (/ (* 3 pi) 4))(func2 pi)))






