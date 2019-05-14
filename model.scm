;;; Overall make-model definition


(define model?
  (make-bundle-predicate 'model))


(define (make-model layers)

  (define (forward-layers inputs)
    ;;; Starts with input and goes through output 
    (let ((layer-outs (list inputs)))
      (for-each (lambda (layer)
		  (append! layer-outs
			   (list (layer 'forward (last layer-outs))))) layers)
      layer-outs))

  (define (forward inputs)
    (last (forward-layers inputs)))

  (define (backwards! loss-func targets layer-outs learning-rate)
   (let backwards-loop ((i (length layers)) (errors (loss-func targets (last layer-outs))))
      ;;; calculate gradients

      (let* (
             (layer (list-ref layers (- i 1))) 
             (new_errors (layer `layer-backwards! errors learning-rate i layer-outs)))

        (if (= i 1) 
          '()
          (backwards-loop (- i 1) new_errors)
      ))))

 

  (define (get-layers)
    layers)

   

  (bundle model? forward-layers forward backwards! get-layers ))


  