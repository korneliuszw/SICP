(define (good-enough? x y ) (< (abs (- x y)) 0.0001))
(define (avg x y) (/ (+ x y)) 2)
(define (half-interval f neg pos) (
  (let (mid) (avg neg pos))
  (if (good-enough? neg pos) mid (
    (let mid_func (f mid))
    (cond (< mid_func 0) (half-interval f mid_func pos))
    (cond (> mid_func 0) (half-interval f neg mid_func))
    (else mid)
                                  ))
                               ))

