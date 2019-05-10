;;; generate randomized weights in the range of -1/(sqrt num-cells) to 1/(sqrt num-cells) (uniform)
;;; Might add optional argument later to specify different distribution types
(define (generate-normalized-random num-cells)
  (+ (/ (* 2 (random 1000)) (* (sqrt num-cells) 1000))
     (/ -1 (sqrt num-cells))))

(define (generate-initial-weights input-size output-size)
  (let ((weights (generate-matrix input-size output-size)))
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
				(generate-initial-weights (first filter-size)
							  (second filter-size))))))))

(define (apply-filter filter submatrix)
  (fold-left + 0
	     (map (lambda (x y) (* x y))
		  (vector->list a)
		  (vector->list b))))

#| Convolution things |#
(define (simple-convolve-2d 2d-input filter filter-size stride)
  (let* ((input-dims (get-matrix-dims 2d-input))
	 (output-rows (+ 1 (/ (- (first input-dims) (first filter-size))
			      (first stride))))
	 (output-cols (+ 1 (/ (- (second input-dims) (second filter-size))
			      (second stride))))
	 (output (generate-matrix output-rows output-cols)))
    (let row-loop ((row-index 0)
		   (out-row-index 0))
      (if (> row-index (- (first input-dims) (first filter-size))) #t
	  (begin
	    (let col-loop ((col-index 0)
			   (out-col-index 0))
	      (if (> col-index (- (second input-dims) (second filter-size))) #t
		  (begin
		    (vector-set! (vector-ref output out-row-index)
				out-col-index
				(apply-filter
				 filter
				 (matrix-ref 2d-input
					     (list row-index
						   (+ (first filter-size)
						      row-index))
					     (list col-index
						   (+ (second filter-size)
						      col-index)))))
		    (col-loop (+ (second stride) col-index) (+ 1 out-col-index)))))
	    (row-loop (+ (first stride) row-index) (+ 1 out-row-index)))))
    output))			

(define (convolve-2d input weights num-filters filter-size stride)
  (let* ((filter-size (if (number? filter-size)
			  (list filter-size filter-size)
			  filter-size))
	 (stride (if (number? stride) (list stride stride) stride))
	 (output (generate-matrix num-filters 1)))
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

(define (sigmoid x)
	(/ (exp x) (+ 1 (exp x))))

(define (dir_sigmoid x)
	(* (sigmoid x) (- 1 (sigmoid x))))

(define (vector:sigmoid a)
  (vector-map (lambda (x) (sigmoid x )) a ))

(define (vector:dir_sigmoid a)
  (vector-map (lambda (x) (dir_sigmoid x )) a ))

(define (matrix:sigmoid a)
  (let ((sum (make-vector (vector-length a))))
    (let row-loop ((row-index 0))
      (if (= row-index (vector-length a)) #t
	  (begin
	    (vector-set! sum
			 row-index
			 (vector:sigmoid (vector-ref a row-index)
				   ))
	    (row-loop (+ 1 row-index)))))
    sum))

(define (matrix:dir_sigmoid a)
  (let ((sum (make-vector (vector-length a))))
    (let row-loop ((row-index 0))
      (if (= row-index (vector-length a)) #t
	  (begin
	    (vector-set! sum
			 row-index
			 (vector:dir_sigmoid (vector-ref a row-index)
				   ))
	    (row-loop (+ 1 row-index)))))
    sum))

(define (bypass x)
	x)


(define (d_squared-error targets outputs)
	(matrix:- targets outputs))

(define a #(#(0 1 1 1 0 0 0)
	    #(0 0 1 1 1 0 0)
	    #(0 0 0 1 1 1 0)
	    #(0 0 0 1 1 0 0)
	    #(0 0 1 1 0 0 0)
	    #(0 1 1 0 0 0 0)
	    #(1 1 0 0 0 0 0)))
(define b #(#(1 0 1)
	    #(0 1 0)
	    #(1 0 1)))

(define 2d-test #(#(0 0 0 0 0 0 0)
		  #(0 0 1 0 0 1 0)
		  #(0 1 2 1 0 2 0)
		  #(0 1 1 2 1 1 0)
		  #(0 1 0 0 1 1 0)
		  #(0 1 1 0 0 1 0)
		  #(0 0 0 0 0 0 0)))

(define test-filter #(#(-1 0 -1)
		      #(-1 1 -1)
		      #(-1 0 1)))

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


(define (matrix:ones_2d A)
	(apply generate-matrix (append (get-matrix-dims A) (list 0 1)))) ; (list #layers fill )








;;; ReLU



(define (ReLU x)
	(if (> x 0)
		x
		0))

(define (dir_ReLU x)
	(if (> x 0)
		1
		0))

(define (vector:ReLU a)
  (vector-map (lambda (x) (ReLU x )) a ))

(define (vector:dir_ReLU a)
  (vector-map (lambda (x) (dir_ReLU x )) a ))

(define (matrix:ReLU a)
  (let ((sum (make-vector (vector-length a))))
    (let row-loop ((row-index 0))
      (if (= row-index (vector-length a)) #t
	  (begin
	    (vector-set! sum
			 row-index
			 (vector:ReLU (vector-ref a row-index)
				   ))
	    (row-loop (+ 1 row-index)))))
    sum))

(define (matrix:dir_ReLU a)
  (let ((sum (make-vector (vector-length a))))
    (let row-loop ((row-index 0))
      (if (= row-index (vector-length a)) #t
	  (begin
	    (vector-set! sum
			 row-index
			 (vector:dir_ReLU (vector-ref a row-index)
				   ))
	    (row-loop (+ 1 row-index)))))
    sum))

