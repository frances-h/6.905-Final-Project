;; TODO: Generic way to make a layer + give details
;;; Layers we would want:
;;     -fully-connected
;;     -convolutional
;;     -max-pool
;;     -dropout

#| for every loss function, need to define dloss |#

(define layer?
  (make-bundle-predicate 'layer))

(define (make-fully-connected input-size output-size use-bias)
  (let ((weights (generate-initial-weights input-size output-size))
	(bias (generate-initial-weights 1 output-size)))

    (define (forward inputs)
      (if use-bias
	  (matrix:+ (vector:dot inputs (transpose weights)) bias)
	  (matrix:* inputs (transpose weights))))

    (define (update-weights! new-weights)
      (set! weights new-weights))

    (define (update-bias! new-bias)
      (set! bias new-bias))

    (bundle layer? forward update-weights! update-bias!)))
