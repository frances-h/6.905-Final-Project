;; TODO: Generic way to make a layer + give details
;;; Layers we would want:
;;     -fully-connected
;;     -convolutional
;;     -max-pool
;;     -dropout

#| for every loss function, need to define dloss |#
(load "utils.scm")

(define layer?
  (make-bundle-predicate 'layer))

(define (make-fully-connected input-size output-size use-bias)
  (let ((weights (generate-initial-weights input-size output-size))
	(bias (generate-initial-weights output-size 1)))

    (define (forward inputs)
      (if use-bias
	  (matrix:+ (matrix:* (transpose weights) inputs) bias)
	  (matrix:*(transpose weights) inputs)))

    (define (update-weights! new-weights)
      (set! weights new-weights))

    (define (update-bias! new-bias)
      (set! bias new-bias))

    (define (print-weights)
      (newline)
      (display weights))

    (define (print-bias)
      (newline)
      (display bias))


    (bundle layer? forward update-weights! update-bias! print-weights print-bias)))
