#lang racket
;ecuacion recta 
;(findEq '(4 6) '(2 2))
;(findEq '(2 4) '(4 2))

(define (findEq A B)
  (local
    (
     (define x1 (first A))
     (define y1 (second A))
     (define x2 (first B))
     (define y2 (second B))
     (define m (/ (- y2 y1) (- x2 x1) ))
     (define b (- y1 (* m x1)))
     (define y '() )
     
    )
   (if (< b 0)
       (set! y (list m'(* x) (+ b)) )
       (set! y (list m'(* x) '(+) (+ b))) 

   )
   ;(set! y (list m'(* x) (+ b)) ) 
   (display "y=" )
   
   (display y)
  )

)
