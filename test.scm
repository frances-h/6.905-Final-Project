;;;

(load "model.scm")
(load "layers.scm")
(load "utils.scm")
(load "loss_fns.scm")


(define net (make-model (list
			 (make-fully-connected 1 4  #t
					       matrix:sigmoid
					       matrix:dir_sigmoid)
			 (make-fully-connected 4 1 #t
					       bypass
					       matrix:ones)


			 )))





(let training-loop (( i 0))
  (if (= i 1000)
      '()
      (begin
	(net `backwards! d_squared-error #(#(1)) 
	     (net `forward-layers #(#(1))) 0.2)

	(net `backwards! d_squared-error #(#(2)) 
	     (net `forward-layers #(#(2))) 0.2)
	(pp "*************")
	(pp (net `forward #(#(1))))
	(pp (net `forward #(#(2))))

	(training-loop (+ i 1)))))


