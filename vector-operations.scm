;;; This is a generic library of basic vector and matrix operations.

#| Vector Operations |#
(define vector:+ (simple-generic-procedure 'vector:+ 2 #f))
(define-generic-procedure-handler vector:+
  (all-args 2 vector?)
  (lambda (a b)
    (vector-map (lambda (x y) (+ x y)) a b)))

(define-generic-procedure-handler vector:+
  (any-arg 2 number? vector?)
  (lambda (a b)
    (if (number? a)
	(vector-map (lambda (x) (+ x a)) b)
	(vector-map (lambda (x) (+ x b)) a))))

(define vector:- (simple-generic-procedure 'vector:- 2 #f))
(define-generic-procedure-handler vector:-
  (all-args 2 vector?)
  (lambda (a b)
    (vector-map (lambda (x y) (- x y)) a b)))

(define-generic-procedure-handler vector:-
  (any-arg 2 number? vector?)
  (lambda (a b)
    (if (number? a)
	(vector-map (lambda (x) (- a x)) b)
	(vector-map (lambda (x) (- x b)) a))))

(define vector:* (simple-generic-procedure 'vector:* 2 #f))
;;; elementwise vector multiplication
(define-generic-procedure-handler vector:*
  (all-args 2 vector?)
  (lambda (a b)
    (vector-map (lambda (x y) (* x y)) a b)))

(define-generic-procedure-handler vector:*
  (any-arg 2 number? vector?)
  (lambda (a b)
    (if (number? a)
	(vector-map (lambda (x) (* x a)) b)
	(vector-map (lambda (x) (* x b)) a))))

;;; dot product between two vectors
(define (vector:dot a b)
  (fold-left + 0 (vector->list (vector-map (lambda (x y) (* x y)) a b))))

#| Preliminary Matrix Operation Setup |#
(define (matrix? object)
  (and (vector? object)
       (vector-every (lambda (elt)
		       (or (matrix? elt)
			   (and (vector? elt)
				(vector-every (lambda (x)
						(number? x))
					      elt))))
	      object)))

(register-predicate! matrix? 'matrix)

(define (get-matrix-dims matrix)
  (append (list (vector-length matrix))
	  (if (matrix? (vector-first matrix))
	      (get-matrix-dims (vector-first matrix))
	      (list (vector-length (vector-first matrix))))))


(define (generate-matrix num-rows #!optional num-cols num-layers fill)
  (let ((fill (if (default-object? fill) #f fill))
	(num-cols (if (default-object? num-cols) num-rows num-cols)))
    (if (or (default-object? num-layers) (= 0 num-layers))
	(make-initialized-vector num-rows (lambda (x) (make-vector num-cols fill)))
	(make-initialized-vector num-layers
				 (lambda (x)
				   (make-initialized-vector
				    num-rows
				    (lambda (y)
				      (make-vector num-cols fill))))))))

#| matrix-ref -- accessor for matrices, including intervals |#
;;; for 2d matrices
(define matrix-ref (simple-generic-procedure 'matrix-ref 3 #f))
;;; for 3d matrices
(define matrix-ref (simple-generic-procedure 'matrix-ref 4 #f))
;;; getting specific elements
(define-generic-procedure-handler matrix-ref
  (match-args matrix?
	      number?
	      number?)
  (lambda (matrix row col)
    (vector-ref (vector-ref matrix row) col)))

(define-generic-procedure-handler matrix-ref
  (match-args matrix?
	      number?
	      number?
	      number?)
  (lambda (matrix row col layer)
    (matrix-ref (vector-ref matrix layer) row col)))

;;; getting part of a row [start->end)
(define-generic-procedure-handler matrix-ref
  (match-args matrix?
	      number?
	      pair?)
  (lambda (matrix row col-interval)
    (let ((col-start (car col-interval))
	  (col-end (if (list? (cdr col-interval))
		       (second col-interval)
		       (cdr col-interval))))
      (subvector (vector-ref matrix row) col-start col-end))))
		 
(define-generic-procedure-handler matrix-ref
  (match-args matrix?
	      number?
	      pair?
	      number?)
  (lambda (matrix row col-interval layer)
    (matrix-ref (vector-ref matrix layer) row col-interval)))

;;; getting part of a col [start->end)
(define-generic-procedure-handler matrix-ref
  (match-args matrix?
	      pair?
	      number?)
  (lambda (matrix row-interval col)
    (let ((row-start (car row-interval))
	  (row-end (if (list? (cdr row-interval))
		       (second row-interval)
		       (cdr row-interval))))
      (vector-map (lambda (row) (vector-ref row col))
		  (subvector matrix row-start row-end)))))

(define-generic-procedure-handler matrix-ref
  (match-args matrix?
	      pair?
	      number?
	      number?)
  (lambda (matrix row-interval col layer)
    (matrix-ref (vector-ref matrix layer) row-interval col)))

;;; getting part of a layer (depth column) [start, end)
(define-generic-procedure-handler matrix-ref
  (match-args matrix?
	      number?
	      number?
	      pair?)
  (lambda (matrix row col layer-interval)
    (let ((layer-start (car layer-interval))
	  (layer-end (if (list? (cdr layer-interval))
			 (second layer-interval)
			 (cdr layer-interval))))
      (vector-map (lambda (layer) (matrix-ref layer row col))
		  (subvector matrix layer-start layer-end)))))
		
;;; getting 2d submatrix [start->end)
(define-generic-procedure-handler matrix-ref
  (match-args matrix?
	      pair?
	      pair?)
  (lambda (matrix row-interval col-interval)
    (let ((row-start (car row-interval))
	  (row-end (if (list? (cdr row-interval))
		       (second row-interval)
		       (cdr row-interval)))
	  (col-start (car col-interval))
	  (col-end (if (list? (cdr col-interval))
		       (second col-interval)
		       (cdr col-interval))))
      (vector-map (lambda (row) (subvector row col-start col-end))
		  (subvector matrix row-start row-end)))))

(define-generic-procedure-handler matrix-ref
  (match-args matrix?
	      pair?
	      pair?
	      number?)
  (lambda (matrix row-interval col-interval layer)
    (matrix-ref (vector-ref matrix layer) row-interval col-interval)))

;;; getting 3d submatrix [start->end)
(define-generic-procedure-handler matrix-ref
  (match-args matrix?
	      pair?
	      pair?
	      pair?)
  (lambda (matrix row-interval col-interval layer-interval)
    (let ((layer-start (car layer-interval))
	  (layer-end (if (list? (cdr layer-interval))
			 (second layer-interval)
			 (cdr layer-interval))))
      (vector-map (lambda (layer)
		    (matrix-ref layer row-interval col-interval))
		  (subvector matrix layer-start layer-end)))))


#| Matrix Operations |#
(define (transpose to-transpose)
  ;;; add assert that verifies all row vectors are the same length
  (let* ((original-dims (get-matrix-dims to-transpose))
	 (transposed-vector (generate-matrix (second original-dims)
					     (first original-dims))))
    (let col-loop ((col-index 0))
      (if (= col-index (first original-dims)) #t
	  (begin
	    (let row-loop ((row-index 0))
	      (if (= row-index (second original-dims))#t
		  (begin
		    (vector-set! (vector-ref transposed-vector row-index)
				 col-index
				 (matrix-ref to-transpose col-index row-index))
		    (row-loop (+ 1 row-index)))))
	    (col-loop (+ 1 col-index)))))
    transposed-vector))

(define matrix:+ (simple-generic-procedure 'matrix:+ 2 #f))
(define-generic-procedure-handler matrix:+
  (all-args 2 matrix?)
  (lambda (a b)
    (vector-map (lambda (x y)
		  (if (matrix? x)
		      (matrix:+ x y)
		      (vector:+ x y))) a b)))
    
(define-generic-procedure-handler matrix:+
  (any-arg 2 number? matrix?)
  (lambda (a b)
    (if (number? a)
	(vector-map (lambda (x) (if (matrix? x)
				    (matrix:+ x a)
				    (vector:+ x a))) b)
	(vector-map (lambda (y) (if (matrix:? y)
				    (matrix:+ y a)
				    (vector:+ y a)) a)))))

(define matrix:- (simple-generic-procedure 'matrix:- 2 #f))
(define-generic-procedure-handler matrix:-
  (all-args 2 matrix?)
  (lambda (a b)
    (vector-map (lambda (x y)
		  (if (matrix? x)
		      (matrix:- x y)
		      (vector:- x y))) a b)))
    
(define-generic-procedure-handler matrix:-
  (any-arg 2 number? matrix?)
  (lambda (a b)
    (if (number? a)
	(vector-map (lambda (x) (if (matrix? x)
				    (matrix:- a x )
				    (vector:- a x))) b)
	(vector-map (lambda (y) (if (matrix:? y)
				    (matrix:- y b)
				    (vector:- y b)) a)))))

(define matrix:* (simple-generic-procedure 'matrix:* 2 #f))
(define-generic-procedure-handler matrix:*
  (all-args 2 matrix?)
  (lambda (a b)
    (let* ((a-dims (get-matrix-dims a))
	   (b-dims (get-matrix-dims b))
	   (dot-product (generate-matrix (first a-dims)
					 (second b-dims))))
      (let b-col-loop ((col-index-b 0))
	(if (= col-index-b (second b-dims)) #t
	    (begin
	      (let a-row-loop ((row-index-a 0))
		(if (= row-index-a (first a-dims)) #t
		    (begin
		      (vector-set! (vector-ref dot-product row-index-a)
				   col-index-b
				   (vector:dot (vector-ref a row-index-a)
					       (matrix-ref b
							   (cons 0 (car b-dims))
							   col-index-b )))
		      (a-row-loop (+ 1 row-index-a)))))
	      (b-col-loop (+ 1 col-index-b)))))
      dot-product)))

(define-generic-procedure-handler matrix:*
  (any-arg 2 number? matrix?)
  (lambda (a b)
    (if (number? a)
	(vector-map (lambda (row) (if (matrix? row)
				      (matrix:* row a)
				      (vector:* row a))) b)
	(vector-map (lambda (row) (if (matrix? row)
				      (matrix:* row a)
				      (vector:* row a)) b)))))

(define (matrix:element_mul a b)
  ;;; assert shape of a = shape of b
  (vector-map (lambda (x y) (vector:* x y)) a b))
