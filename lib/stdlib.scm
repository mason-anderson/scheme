(define test 7)
(define id (lambda (x) x))

(define foldl (lambda (func accum lst)
  (if (null? lst)
      accum
      (foldl func (func accum (car lst)) (cdr lst)))))
(define foldr (lambda (func accum lst)
  (if (null? lst)
      accum
      (func (car lst) (foldr func accum (cdr lst))))))
(define unfold (lambda (func init pred)
  (if (pred init)
      (cons init '())
      (cons init (unfold func (func init) pred)))))
(define fold (lambda (f a l) (foldl f a l)))

(define reduce (lambda (f a l) (fold f a l)))
(define length (lambda (lst)        (fold (lambda (x y) (+ x 1)) 0 lst)))
(define append (lambda (lst  lsts)  (foldr (flip (curry foldr cons)) lst lsts)))
(define reverse (lambda (list) (cdr (foldl (flip cons) '() list))))

(define map (lambda (func lst)      (foldr (lambda (x y) (cons (func x) y)) '() lst)))
(define filter (lambda (pred lst)   (foldr (lambda (x y) (if (pred x) (cons x y) y)) '() lst)))

(define max (lambda (x  num-list) (fold (lambda (y z) (if (> y z) y z)) x (cons 0 num-list))))
(define min (lambda (x  num-list) (fold (lambda (y z) (if (< y z) y z)) x (cons 536870911 num-list))))

(define fact (lambda (n) (if (== n 0) 1 (* n (fact (- n 1))) ) ))
