;;; utils to define:
#|
generate-initial-weights
vector:+
vector:dot
transpose
|# 

;;; generate randomized weights in the range of -1/(sqrt num-cells) to 1/(sqrt num-cells) (uniform)
;;; Might add optional argument later to specify different distribution types
(define (generate-normalized-random num-cells)
  (+ (/ (* 2.0 (random 1000)) (* (sqrt num-cells) 1000))
     (/ -1 (sqrt num-cells))))
  
(define (generate-initial-weights input-size output-size)
  (let ((weights (make-vector input-size)))
    (let x-loop ((i 0)
		 (x (make-vector output-size)))
      (if (= i input-size) weights
	  (begin
	    (let y-loop ((j 0))
	      (if (= j output-size) #t
		  (begin
		    (vector-set! x j (generate-normalized-random input-size))
		    (y-loop (+ j 1)))))
	      (vector-set! weights i x)
	      (x-loop (+ 1 i)
		      (make-vector output-size)))))
    weights))


#| VECTOR OPERATIONS |#

(define (vector:dot a b)
  ;;; assert a and b equal length
  (fold-left + 0
	     (map (lambda (x y) (* x y))
		  (vector->list a)
		  (vector->list b))))

(define (vector:+ a b)
  (vector-map (lambda (x y) (+ x y)) a b))

#| MATRIX OPERATIONS |#


(define (generate-empty-matrix num-rows num-cols)
  (make-initialized-vector num-rows (lambda (x) (make-vector num-cols))))


(define (get-col-vector col-index matrix)
  (let ((col-vector (make-vector (vector-length matrix))))
    (let row-loop ((row-index 0))
      (if (= row-index (vector-length matrix)) #t
	  (begin
	    (vector-set! col-vector
			 row-index
			 (vector-ref (vector-ref matrix row-index)
				     col-index))
	    (row-loop (+ 1 row-index)))))
    col-vector))

(define (transpose to-transpose)
  ;;; add assert that verifies all row vectors are the same length
  (let ((transposed-vector (generate-empty-matrix (vector-length (vector-first
								  to-transpose))
						  (vector-length to-transpose))))
    (let col-loop ((col-vector-index 0))
      (if (= col-vector-index (vector-length to-transpose)) #t
	  (begin
	    (let row-loop ((row-index 0))
	      (if (= row-index (vector-length (vector-first to-transpose))) #t
		  (begin
		    (vector-set! (vector-ref transposed-vector row-index)
				 col-vector-index
				 (vector-ref (vector-ref to-transpose
							 col-vector-index)
					     row-index))
		    (row-loop (+ 1 row-index)))))
	    (col-loop (+ 1 col-vector-index)))))
    transposed-vector))  

 ;;; add assert that verifies that len col a = len row b for every col in a and row in b
  ;;; num-rows = (vector-length (vector-first a))
  ;;; num-cols = (vector-length b)
(define (matrix:* a b)
  (let ((dot-product (generate-empty-matrix (vector-length a)
					    (vector-length (vector-first b)))))
    (let b-col-loop ((col-index-b 0))
      (if (= col-index-b (vector-length (vector-first b))) #t
	  (begin
	    (let a-row-loop ((row-index-a 0))
	      (if (= row-index-a (vector-length a)) #t
		  (begin
		    (vector-set! (vector-ref dot-product
					     row-index-a)
				 col-index-b
				 (vector:dot (vector-ref a row-index-a)
					     (get-col-vector col-index-b b)))
		    (a-row-loop (+ 1 row-index-a)))))
	    (b-col-loop (+ 1 col-index-b)))))
    dot-product))

(define (matrix:+ a b) ;;2d matrix
  ;;; assert shape of a = shape of b
  (let ((sum (make-vector (vector-length a))))
    (let row-loop ((row-index 0))
      (if (= row-index (vector-length a)) #t
	  (begin
	    (vector-set! sum
			 row-index
			 (vector:+ (vector-ref a row-index)
				   (vector-ref b row-index))) 
	    (row-loop (+ 1 row-index)))))
    sum))
