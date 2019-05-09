

; A single instance of MSE Loss


(define (SE_Loss input target)
 		(* (- input target) (- input target)))

(define (SE_Loss_dir input target)
 		(* 2 (- input target)))
