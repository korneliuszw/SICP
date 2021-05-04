;;; Margin of fixed point error
(define tolerance 0.0001)
;;; Fixed point is x for which f(x) = x
(define (fixed-point f first-guess)
 (define (enough? x y)
    (< (abs(- x y)) tolerance)
  )
  (define (try guess)
    (let ((next (f guess)))
      (if (enough? guess next) next
        (try next)
       )
     )
   )
  (try first-guess)
)
;;; (x + y) / 2
(define (average x y) (/ (+ x y) 2))
;;; Average dump is an average of x and f(x)
(define (avg-damp f) (
  lambda (x) (average x (f x))
))
#|
Square root can be found by calculating an average of x and some guess until a tolerance is met
sqrt(x) = y
y = [y + (x/y)] / 2
until |y^2 - x| < z
where
x - number which root we search
y - guess
z - tolerance (0.0001)

This function does by calculating fixed point of an average dump of x, giving 1.0 as initial guess
f(y) = (x + (x / y)) / 2
sqrt(x) = f(f(f(f...(f(1.0)))))
|#
(define (sqrt x) 
  (fixed-point (avg-damp (lambda (y) (/ x y))) 1.0)
)
(define dx 0.0001)
;;; Derivate of a function f(x) is [(f(x + dx) - f(x)) / dx] where dx is a very small value, smaller = more accurate
;;; Derviate is used to calculate change of a function (wikipedia definition, didn't study Calculus yet)
(define (deriv g) 
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx))
)
;;; Newton transformation function f(x) = x - [g(x) / Dg(x)] where Dg is a derivate of function g
(define (newton-func g)
  (lambda (x) (- x (/ (g x) ((deriv g) x))))
 )
;;; Newton methods finds x for which g(x) = 0 and x is a fixed point of newton transformation of g
(define (newton-method g guess)
  (fixed-point (newton-func g) guess)
)
(define (square x) (* x x))
;;; We can calculate sqrt of x using newton method with g(x) = x^2 - y  sqrt(4) = 2 g(2) = 2^2 - 4 = 4 -4 = 0
(define (newton-sqrt x)
  (newton-method (lambda (y) (- (square y) x)) 1.0)
 )
(define (compose f g)
  (lambda (x) (f (g x)))
)
(define (inc x) (+ x 1))
(define (continual f n)
  (define (next g now)
    (if (> now n) g 
    (next (compose f g) (+ now 1))
    )
  )
  (next f 2)
 )

