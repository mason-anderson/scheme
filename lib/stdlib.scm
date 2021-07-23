(define id (lambda (x) x))
(define curry (lambda (f x) (lambda (y) (f x y))))
(define compose (lambda (f g) (lambda (x) (f (g x)))))
(define flip (lambda (f) (lambda (x y) (f y x))))

(define foldl (lambda (f accum lst)
  (if (null? lst)
      accum
      (foldl f (f accum (car lst)) (cdr lst)))))
(define foldr (lambda (f accum lst)
  (if (null? lst)
      accum
      (f (car lst) (foldr f accum (cdr lst))))))
(define unfold (lambda (f init pred)
  (if (pred init)
      (cons init '())
      (cons init (unfold f (f init) pred)))))
(define fold (lambda (f a l) (foldl f a l)))

(define length (lambda (lst)        (fold (lambda (x y) (+ x 1)) 0 lst)))
(define append (lambda (lst  lsts)  (foldr (flip (curry foldr cons)) lst lsts)))
(define reverse (lambda (list) (cdr (foldl (flip cons) '() list))))

(define map (lambda (f lst)      (foldr (lambda (x y) (cons (f x) y)) '() lst)))
(define filter (lambda (pred lst)   (foldr (lambda (x y) (if (pred x) (cons x y) y)) '() lst)))
(define reduce (lambda (f a l) (fold f a l)))

(define max (lambda (x  num-list) (fold (lambda (y z) (if (> y z) y z)) x (cons 0 num-list))))
(define min (lambda (x  num-list) (fold (lambda (y z) (if (< y z) y z)) x (cons 536870911 num-list))))

(define fact (lambda (n) (if (== n 0) 1 (* n (fact (- n 1))) ) ))
