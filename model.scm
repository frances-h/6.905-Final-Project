;;; Overall make-model definition


(define model?
  (make-bundle-predicate 'model))


(define (make-model layers)

  (define (forward-layers inputs)
    ;;; Starts with input and goes through output 
    (let ((layer-outs (list inputs)))
      (for-each (lambda (layer)
		  (append! layer-outs
			   (list (layer 'forward (car layer-outs))))) layers)
      layer-outs))

  (define (forward inputs)
    (last (forward-layers inputs)))

  (define (backwards! loss-func targets layer-outs learning-rate)
   (let backwards-loop ((i (length layers)) (errors (loss-func targets (last layer-outs))))
      ;;; calculate gradients

      (let* ((layer (list-ref layers (- i 1))) 
            (scaled_errors (matrix:* learning-rate errors))
            (cur_layer_outs (list-ref layer-outs i))
            (d_activation_cur_layer_outs (layer `d_activation cur_layer_outs))
            (gradients  (matrix:element_mul scaled_errors d_activation_cur_layer_outs) )
            (deltas (transpose(matrix:* gradients (transpose (list-ref layer-outs (- i 1)))))))


      (layer `update-weights! (matrix:+ (layer `get-weights) deltas))
      (layer `update-bias! (matrix:+ (layer `get-bias) gradients))
    )

      (if (= i 1) 
        '()
        (backwards-loop (- i 1) (matrix:* (transpose ((list-ref layers (- i 2)) `get-weights)) errors)))
      ))

 

    (define (get-layers)
      layers)

  (bundle model? forward-layers forward backwards! get-layers))


  