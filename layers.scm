
(define layer?
  (make-bundle-predicate 'layer))



(define (make-fully-connected input-size output-size use-bias activation_fn d_act_fn)

  (let ((weights (generate-initial-weights input-size output-size))
  (bias (generate-initial-weights output-size 1)))

    (define (forward inputs)
      (if use-bias
    (activation_fn (matrix:+ (matrix:* (transpose weights) inputs) bias))
    (activation_fn (matrix:*(transpose weights) inputs))))

    (define (update-weights! new-weights)
      (set! weights new-weights))

    (define (update-bias! new-bias)
      (set! bias new-bias))

    (define (get-weights)
      weights)

    (define (get-bias)
      bias)

    (define (d_activation x)
      (d_act_fn x))

    (bundle layer? forward update-weights!  update-bias! get-weights get-bias d_activation)))


(define (make-conv2d in-channels
		     num-filters
		     filter-size
		     use-bias
		     stride
		     padding
		     pad-value)
  (let ((weights (generate-filters num-filters filter-size in-channels))
	(bias (generate-initial-weights num-filters 1)))
    (define (forward inputs)
      (let ((convolution (convolve-2d inputs
				      weights
				      num-filters
				      filter-size
				      stride)))
	(if use-bias
	    (let channel-loop ((channel-index 0)
			       (biased-conv (make-vector num-filters)))
	      (if (= channel-index num-filters) biased-conv
		  (begin
		    (vector-set! biased-conv
				channel-index
				(matrix:+ (vector-ref (vector-ref bias
								  channel-index)
						      0)
					  (vector-ref convolution channel-index)))
		    (channel-loop (+ 1 channel-index) biased-conv))))
	    convolution)))

    (define (update-weights! new-weights)
      (set! weights new-weights))

    (define (update-bias! new-bias)
      (set! bias new-bias))

    (define (get-weights)
      weights)

    (define (get-bias)
      bias)

    (bundle layer? forward update-weights!  update-bias! get-weights get-bias)))
