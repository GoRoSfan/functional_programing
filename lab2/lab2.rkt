#lang racket

;=================================Task 1==========================
(define (factorial n)
  (if (<= n 1)
    1
    (* n (factorial (- n 1)))
  )
)

(define (sine x n)
  (cond ((not (null? n))
         (cond ((< 25 n)
                0)
               (else (- (/ (expt x n) (factorial n))
                        (sine x (+ 2 n))))))
        (else (- x (sine x 3))))
)

(define (cose x n)
  (cond ((not (null? n))
         (cond ((< 25 n)
                0)
               (else (- (/ (expt x n) (factorial n))
                        (cose x (+ 2 n))))))
        (else (- 1 (cose x 2))))
)

(define (task1_taylor x)
  (cond
    [(and (>= x -2) (< x 0)) (/ (- (/ (sine x 1) (cose x 0)) (/ x 2))
                                (/ (sine (* x 2) 1) (cose (* x 0) 1)))
    ]
    [(and (> x 0) (<= x 2)) (- (/ (sine (+ x 2) 1) (cose (+ x 2) 0))
                               (expt (/ (sine x 1) (cose x 0)) 2))]
    [else "Function does not defined"]
  )
)

(define (task1_common x)
  (cond
    [(and (>= x -2) (< x 0)) (/ (- (tan x) (/ x 2))
                                (tan (* x 2)))
    ]
    [(and (> x 0) (<= x 2)) (- (tan (+ x 2))
                               (expt (tan x) 2))]
    [else "Function does not defined"]
  )
)

"Delta"
(- (task1_taylor 2.0) (task1_common 2.0))


;=================================Task 2==========================
(define (task2 n i)
  (if (>= i n)
    n 
    (sqrt(+ (* i 3) (task2 n (+ i 1))))
  )
)

(task2 1000000 1)




