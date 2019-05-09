;;;

(load "load")


(define net (make-model (list
			 (make-fully-connected 1 4  #t
					       matrix:sigmoid
					       matrix:dir_sigmoid)
			 (make-fully-connected 4 1 #t
					       bypass
					       matrix:ones_2d)


			 )))





(let training-loop (( i 0))
  (if (= i 100)
      '()
      (begin
	(net `backwards! d_squared-error #(#(1)) 
	     (net `forward-layers #(#(1))) 0.2)

	(pp "*************")
	(pp (net `forward #(#(1))))

	(training-loop (+ i 1)))))


