#lang racket

;1
;ejercicio 1

;Si queremos integrar 5
;Ejemplo input es (integral 5)

;Si queremos integrar X
;Ejemplo input es (integral 'x)

;Si queremos integrar X^3
;Ejemplo input es (integral '((1 3)))

;Si queremos integrar 5*X^3 + 4*X^2 + 3*X^1
;Ejemplo input es (integral '((5 3) (4 2) (3 1)))

;Si queremos integrar 5^x
;Ejemplo input es (integral '((5 'x)))

;Si queremos integrar 5^(x^3)
;Ejemplo de input es (integral '((5 ('((1 3))))))

(define (integral L)
  (cond
    [(empty? L) (void)]
    [(number? L) (display (string-append "Integral = " (number->string L) "x"))]
    [(equal? L 'x) (display "(x^2)/2")]
    [else (display "Integral = ")
          (mostrar-int L)
          ])
  )

;(string-append  (number->string (first (first L))))

(define (mostrar-int L)
  (cond
    [(empty? L) (display "c")]
    [(equal? (second (first L)) ''x) (display (string-append (number->string(first(first L))) "^x" "/ln" (number->string(first (first L))) "+c" ))]
    [(list? (second (first L))) (display (mostrar-int (second (first L))))]
    [else
     (display "(")
     (display (first (first L)))
     (display (string-append "*x^" (number->string (+ (second (first L)) 1)) ")"))
     (display "/")
     (display (number->string(+ (second (first L)) 1)))
     (display "+")
     (mostrar-int (rest L))
    ]
  )
)


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
