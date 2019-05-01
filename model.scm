;;; Overall make-model definition

(define (make-model layers)

  (define (forward inputs)
    (let ((a inputs))
      (for-each (lambda (layer)
		  (append! a (list (layer 'forward a)))) layers)
      a))

  (define (backward loss-func output-list)
    ;; output-list to start at last layer and go to first
    (let ((a-list (reverse output-list))
      (for-each (lambda (layer)
		  (set-a!)
		  (reverse layers)

		  )))))


		
