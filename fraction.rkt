#lang racket
(require racket/local)

(define (k-fraction d n k)
  (local ([iter x]) (
    if (= x k) (+ (d x) (/ (n x) (d x))
    (+ (d x) (/ (n (+ x 1)) (iter (+ x 1))))
  )))
  (/ (n 1) (iter 1))
)
