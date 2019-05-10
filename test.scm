
;;;

(load "load")


(define net (make-model (list
			 (make-fully-connected 1 6  #t
					       matrix:ReLU
					       matrix:dir_ReLU)
			 (make-fully-connected 6 6  #t
					       matrix:ReLU
					       matrix:dir_ReLU)
			 (make-fully-connected 6 6  #t
					       matrix:ReLU
					       matrix:dir_ReLU)
		
			 (make-fully-connected 6 1 #t
					       bypass
					       matrix:ones_2d)


			 )))

(define input_data   #(#(#(1))
		       #(#(2))
		       #(#(3)) 
		       #(#(4))
		       #(#(5))
		       #(#(6))
		       #(#(7 ))))

(define output_data  #(#(#(3))
		       #(#(2))
		       #(#(1))
		       #(#(0))
		       #(#(1))
		       #(#(2))
		       #(#(3))))

(define N (vector-length input_data))



(let training-loop (( i 0)  (data_num 0)  )
  (if (= 0 (modulo i 100))
      (pp i)
      )
  (if (= i 1000)
      '()
      (begin
	(net `backwards! d_squared-error 
	     (vector-ref output_data data_num)
	     (net `forward-layers 
		  (vector-ref input_data data_num) )
		  0.001)
	(training-loop (+ i 1) (random N) ))))

(pp "done")
(vector-map (lambda (i) (net `forward i)) input_data)