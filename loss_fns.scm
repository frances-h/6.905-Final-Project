

(define (d_squared-error targets outputs)
	(matrix:- targets outputs))


(define (d_absolute-error targets outputs)
	(matrix:sgn (matrix:- targets outputs)))