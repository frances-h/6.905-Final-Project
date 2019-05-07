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
  (+ (/ (* 2 (random 1000)) (* (sqrt num-cells) 1000))
     (/ -1 (sqrt num-cells))))

(define (generate-initial-weights input-size output-size)
  (let ((weights (generate-empty-matrix input-size output-size)))
    (let x-loop ((i 0))
      (if (= i input-size) weights
	  (begin
	    (let y-loop ((j 0))
	      (if (= j output-size) #t
		  (begin
		    (vector-set! (vector-ref weights i)
				 j
				 (generate-normalized-random input-size))
		    (y-loop (+ j 1)))))
	    (x-loop (+ 1 i)))))
    weights))

(define (generate-filters num-filters filter-size in-channels)
  (vector->list (make-initialized-vector
   num-filters
   (lambda (x)
     (make-initialized-vector in-channels
			      (lambda (y)
				(generate-initial-weights filter-size
							  filter-size)))))))

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

(define (get-submatrix input-matrix out-size center-x center-y)
  (let ((output (make-vector out-size))
	(x-begin (- center-x (- (/ out-size 2) 1/2)))
	(x-end (+ center-x (+ 1/2 (/ out-size 2)))))
    (let row-loop ((out-row-index 0)
		   (input-row-index (- center-y (- (/ out-size 2) 1/2))))
      (if (= out-row-index out-size) output
	  (begin
	    (vector-set! output
			 out-row-index
			 (subvector (vector-ref input-matrix input-row-index)
				    x-begin
				    x-end))
	    (row-loop (+ 1 out-row-index) (+ 1 input-row-index)))))
    output))

(define (transpose to-transpose)
  ;;; add assert that verifies all row vectors are the same length
  (let ((transposed-vector (generate-matrix (vector-length (vector-first
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

(define (matrix:dot a b)
  (fold-left + 0
	     (map (lambda (x y) (vector:dot x y))
		  (vector->list a)
		  (vector->list b))))

(define (matrix:* a b)
  (let ((dot-product (generate-matrix (vector-length a)
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

(define (matrix:+ a b)
  ;;; assert shape of a = shape of b
  (if (number? a) (vector-map (lambda (x)
				(vector-map (lambda (y)
					      (+ a y))
					    x))
			      b)
      (let ((sum (make-vector (vector-length a))))
	(let row-loop ((row-index 0))
	  (if (= row-index (vector-length a)) #t
	      (begin
		(vector-set! sum
			     row-index
			     (vector:+ (vector-ref a row-index)
				       (vector-ref b row-index)))
		(row-loop (+ 1 row-index)))))
	sum)))


#| Convolution things |#

(define (simple-convolve-2d 2d-input filter filter-size stride)
  ;; assume squares :(
  ;; assume filter size is odd (to find central pixel)
  (let ((output (generate-matrix (+ 1 (/ (- (vector-length 2d-input)
					    filter-size) stride))))
	(initial-index (- (/ filter-size 2) 1/2)))
    (let y-loop ((y-pos initial-index)
		 (output-y-index 0))
      (if (>= y-pos (- (vector-length 2d-input) initial-index)) #t
	  (begin
	    (let x-loop ((x-pos initial-index)
			 (output-x-index 0))
	      (begin
		(if (>= x-pos (- (vector-length 2d-input) initial-index)) #t
		    (begin
		      (vector-set! (vector-ref output output-y-index)
				   output-x-index
				   (matrix:dot filter
					       (get-submatrix 2d-input
							   filter-size
							   x-pos y-pos)))
		      (x-loop (+ x-pos stride) (+ 1 output-x-index))))))
	    (y-loop (+ y-pos stride) (+ 1 output-y-index)))))
    output))

(define (convolve-2d input weights num-filters filter-size stride)
  (let ((output (generate-matrix num-filters 1)))
    (let out-channel-loop ((out-index 0))
      (if (= out-index num-filters) #t
	  (begin
	    (let in-channel-loop ((in-index 0)
				  (layer-sum 0))
	      (if (= in-index (vector-length input))
		  (vector-set! output out-index layer-sum)
		  (in-channel-loop (+ 1 in-index) 
				   (matrix:+ layer-sum
					     (simple-convolve-2d
					      (vector-ref input in-index)
					      (vector-ref (list-ref weights
								    out-index)
							  in-index)
					      filter-size
					      stride)))))
	    (out-channel-loop (+ 1 out-index)))))
    output))


(define test-input (vector
		    #(#(0 0 0 0 0 0 0)
		      #(0 0 1 0 0 1 0)
		      #(0 1 2 1 0 2 0)
		      #(0 1 1 2 1 1 0)
		      #(0 1 0 0 1 1 0)
		      #(0 1 1 0 0 1 0)
		      #(0 0 0 0 0 0 0))
		    #(#(0 0 0 0 0 0 0)
		      #(0 2 0 0 1 2 0)
		      #(0 0 1 1 2 0 0)
		      #(0 2 1 0 0 1 0)
		      #(0 0 2 2 0 2 0)
		      #(0 2 1 1 0 0 0)
		      #(0 0 0 0 0 0 0))
		    #(#(0 0 0 0 0 0 0)
		      #(0 0 2 1 0 1 0)
		      #(0 1 0 0 1 1 0)
		      #(0 2 2 1 2 0 0)
		      #(0 0 1 1 0 1 0)
		      #(0 0 0 1 0 0 0)
		      #(0 0 0 0 0 0 0))))

(define filters (list
		 #(#(#(-1 0 -1)
		     #(-1 1 -1)
		     #(-1 0 1))
		   #(#(0 1 1)
		     #(0 1 0)
		     #(0 1 0))
		   #(#(1 -1 1)
		     #(0 1 1)
		     #(-1 0 1)))
		 #(#(#(1 0 -1)
		     #(1 -1 -1)
		     #(1 -1 -1))
		   #(#(-1 1 0)
		     #(-1 1 0)
		     #(-1 -1 -1))
		   #(#(1 -1 0)
		     #(0 -1 1)
		     #(-1 -1 1)))))

(define bias (vector
	      #(#(1 1 1)
		#(1 1 1)
		#(1 1 1))
	      #(#(0 0 0)
		#(0 0 0)
		#(0 0 0))))
		

		     
		   
		   
