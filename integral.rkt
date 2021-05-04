#lang racket

(define (sum a b next term)
 (if (> a b) 0
  (+ (term a) (sum (next a) b next term))
  )
)
(define (inc n) (+ n 1))
(define (simpson-integral f a b n)
  (let ([h (/ (- b a) n)])
    (* (/ h 3) (sum 0 n inc (lambda (x) (*
        (if (> x 0) (if (= (remainder x 2) 0) 2 4) 1) 
        (f (+ a (* x h)))
      )))
   )
  )
)
