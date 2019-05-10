
;;;

(load "load")


(define net (make-model (list
			 (make-fully-connected 1 3  #t
					       matrix:ReLU
					       matrix:dir_ReLU)
			 (make-fully-connected 3 3  #t
					       matrix:ReLU
					       matrix:dir_ReLU)
		
			 (make-fully-connected 3 1 #t
					       bypass
					       matrix:ones_2d)


			 )))

(define input_data   #(#(#(1))
		       #(#(1.5))
		       #(#(2)) 
		       #(#(2.5))
		       #(#(3))
		       #(#(3.5))
		       #(#(4 ))))

(define output_data  #(#(#(1))
		       #(#(2.25))
		       #(#(4))
		       #(#(6.25))
		       #(#(9))
		       #(#(12.25))
		       #(#(16))))

(define N (vector-length input_data))



(let training-loop (( i 0)  (data_num 0)  )
  (if (= i 10000)
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