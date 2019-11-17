#lang racket
;ecuacion recta
;(findEq '(x1 y1) '(x2 y2) numaevaluar)
;(findEq '(4 6) '(2 2) 6)
;(findEq '(2 4) '(4 2) 2)

(define (findEq A B Z)
  (local
    (
     (define x1 (first A))
     (define y1 (second A))
     (define x2 (first B))
     (define y2 (second B))
     (define m (/ (- y2 y1) (- x2 x1) )) ; se calcula m
     (define b (- y1 (* m x1))) ; se calcula b para ello se utliza y1 y x1
     (define y '() )
     
    )
   (if (< b 0)
       (set! y (list m '(* x) (+ b)) ) ;si b es negativo
       (set! y (list m '(* x) '+ (+ b))) ;si b es positivo

   )
   (display "y=")
   (display y)
   (newline)
   (display "Evaluada: ")
   (display (+ (* Z m) b) ); se evalua ecuacion
  )

)
