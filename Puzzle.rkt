(require (lib "graphics.ss" "graphics"))
(open-graphics)
(define w (open-viewport "prueba" 1300 690))

;MENU
(define (pintarmenu)
  (begin
    ((draw-pixmap w) "HitmanFondo.jpg" (make-posn 0 0))
    ((draw-pixmap w) "Rompecraneos.png" (make-posn 210 5))
    ((draw-pixmap w) "Nivel.png" (make-posn 60 300))
    ((draw-pixmap w) "Facil.png" (make-posn 240 400))
    ((draw-pixmap w) "Dificil.png" (make-posn 240 480))
    ))
;DEFINE UNA FUNCION PARA MANEJAR EL MENU
(define (botones click)
  (cond
    ((and (<= 240 (posn-x (mouse-click-posn click)) 390)(<= 400 (posn-y (mouse-click-posn click)) 450)) (nivelfacil 80 20 163))
    ((and (<= 240 (posn-x (mouse-click-posn click)) 390)(<= 480 (posn-y (mouse-click-posn click)) 530)) (niveldificil 60 20 131))
    ((and (<= 240 (posn-x (mouse-click-posn click)) 390)(<= 560 (posn-y (mouse-click-posn click)) 610)));(niveldificil 60 20 131))
    (else (botones (get-mouse-click w)))
    ))
;__________________________________________________________________________________________________________________________________________________ 


;PINTA EL FONDO Y SUS COMPONENTES NIVEL FACIL
(define (pintarfondo1)
  (begin
    ((draw-pixmap w) "Muñeco.png" (make-posn 0 0))
    ((draw-pixmap w) "Inicio.png" (make-posn 880 550))
    ((draw-pixmap w) "Universo.jpg" (make-posn 620 20))
    ))

;SE DEFINE FUNCION QUE PINTA EL TABLERO DONDE SE UBICARAN LAS FICHAS
(define (pintarcuadro1 px py lado)
  (begin
   ((draw-rectangle w) (make-posn px py) lado lado (make-rgb 1 1 1))))

(define (pintarfila1 px py lado n)
  (if (> n 0)
      (begin
        (pintarcuadro1 px py lado)
        (pintarfila1 (+ px lado) py lado (- n 1)) 
        )))

(define (pintartablero1 px py lado n)
  (if (> n 0)
      (begin
        (pintarfila1 px py lado 3)
        (pintartablero1 px (+ py lado) lado  (- n 1)) 
        )))

;COPIA LA IMAGEN
(define (pintarFicha1 ficha px py)
  (cond
    ((= ficha 21) ((draw-pixmap w) "Imagen1.jpg" (make-posn px py)))
    ((= ficha 22) ((draw-pixmap w) "Imagen2.jpg" (make-posn px py)))
    ((= ficha 23) ((draw-pixmap w) "Imagen3.jpg" (make-posn px py)))
    ((= ficha 24) ((draw-pixmap w) "Imagen4.jpg" (make-posn px py)))
    ((= ficha 25) ((draw-pixmap w) "Imagen5.jpg" (make-posn px py)))
    ((= ficha 26) ((draw-pixmap w) "Imagen6.jpg" (make-posn px py)))
    ((= ficha 27) ((draw-pixmap w) "Imagen7.jpg" (make-posn px py)))
    ((= ficha 28) ((draw-pixmap w) "Imagen8.jpg" (make-posn px py)))
    ((= ficha 29) ((draw-pixmap w) "Imagen9.jpg" (make-posn px py)))
    ((= ficha 30) ((draw-pixmap w) "Imagen10.jpg" (make-posn px py)))
    ((= ficha 31) ((draw-pixmap w) "Imagen11.jpg" (make-posn px py)))
    ((= ficha 32) ((draw-pixmap w) "Imagen12.jpg" (make-posn px py)))
    ))

;UBICA UNA FICHA EN LA COLUMNA Y FILA DEL TABLERO DADAS
(define (moverFicha1 ficha col fila px py lado)
  (pintarFicha1 ficha (+ (* (- col 1) lado) (+ px 1)) (+ (* (- fila 1) lado) (+ py 1))))

;UBICA GRAFICAMENTE LAS FICHAS EN EL TABLERO EN PANTALLA
(define (ubicarFichas1 px py lado)
  (begin
     (moverFicha1 29 1 1 px py lado)
     (moverFicha1 27 2 1 px py lado) 
     (moverFicha1 24 3 1 px py lado) 
     (moverFicha1 22 1 2 px py lado) 
     (moverFicha1 21 2 2 px py lado)
     (moverFicha1 23 3 2 px py lado) 
     (moverFicha1 25 1 3 px py lado) 
     (moverFicha1 28 2 3 px py lado)
     (moverFicha1 32 3 3 px py lado)
     (moverFicha1 30 1 4 px py lado)
     (moverFicha1 26 2 4 px py lado)
     (moverFicha1 31 3 4 px py lado)
     ))

;CREA UN VECTOR DE 20 CASILLAS PARA LAS FICHAS
(define (crearJuego1 T)
  (begin
    (vector-set! T 0 29)
    (vector-set! T 1 27)
    (vector-set! T 2 24)
    (vector-set! T 3 22)
    (vector-set! T 4 21)
    (vector-set! T 5 23)
    (vector-set! T 6 25)
    (vector-set! T 7 28)
    (vector-set! T 8 32)
    (vector-set! T 9 30)
    (vector-set! T 10 26)
    (vector-set! T 11 31)
    T
    ))

(define (realizarMovimiento1 c1 f1 c2 f2 T px py lado posn1 posn2)
  (begin
  ;COPIA EN LA POSICIION DESTINO LA INFORMACION DEL VECTOR ORIGEN
     (vector-set! T (+ (- c2 1) (* 3 (- f2 1))) posn1)
    ;COPIA EN LA POSICION ORIGEN LA INFORMACION DEL VECTOR DESTINO
    (vector-set! T (+ (- c1 1) (* 3 (- f1 1))) posn2)
    ;PONE LA IMAGEN DE LA FICHA QUE SE MUEVE EN LA POSISCIO DESTINO EN PANTALLA
    (moverFicha1 (vector-ref T (+ (- c2 1) (* 3 (- f2 1)))) c2 f2 px py lado)
    ;PONE LA IMAGEN DEL LA FICHA QUE NO SE MUEVE EN LA POSICION ORIGEN EN PANTALLA
    (moverFicha1 (vector-ref T (+ (- c1 1) (* 3 (- f1 1)))) c1 f1 px py lado)
    ))


(define (validarJugada1 c1 f1 c2 f2 T px py lado)
  (realizarMovimiento1 c1 f1 c2 f2 T px py lado (vector-ref T (+ (- c1 1) (* 3 (- f1 1)))) (vector-ref T (+ (- c2 1) (* 3 (- f2 1)))))
    )
    
;PULSAR 2
(define (pulsar21 c1 f1 p2 T px py lado)
  (begin
    (display (posn-x p2)) 
    (newline)
    (display (posn-y p2))
    (newline)
    (display (+ 1 (quotient (- (posn-x p2) px) lado)))
    (newline)
    (display (+ 1 (quotient (- (posn-y p2) py) lado)))
    (newline)
    (validarJugada1 c1 f1 (+ 1 (quotient (- (posn-x p2) px) lado))  (+ 1 (quotient (- (posn-y p2) py) lado)) T px py lado)
    ))

;PULSAR 1
(define (pulsar11 p1 T px py lado)
  (begin
;    (display (posn-x p1)) 
;    (newline)
;    (display (posn-y p1))
;    (newline)
;    (display (+ 1 (quotient (- (posn-x p1) px) lado)))
;    (newline)
;    (display (+ 1 (quotient (- (posn-y p1) px) lado)))
;    (newline)
    (pulsar21 (+ 1 (quotient (- (posn-x p1) px) lado))  (+ 1 (quotient (- (posn-y p1) px) lado)) (mouse-click-posn (get-mouse-click w)) T px py lado)
    ))

;DEFINE EL NUMERO DE MOVIMIENTOS 
(define (juego1 c px py lado T)
  (if (< c 20)
      (begin
         (pulsar11  (mouse-click-posn (get-mouse-click w)) T px py lado)
         (juego1 (+ c 1) px py lado T)
     )))

;EJECUTA EL NIVEL FACIL   
(define (nivelfacil px py lado)
  (begin
    (pintarfondo1)
    (pintartablero1 px py lado 4)
    (ubicarFichas1 px py lado)
    (juego1 1 px py lado (crearJuego1 (make-vector 20 0)))
    ))
;__________________________________________________________________________________________________________________________________________________
;PINTA EL FONDO Y SUS COMPONENTES NIVEL DIFICIL
(define (pintarfondo)
  (begin
    ((draw-pixmap w) "Rider.jpg" (make-posn 0 0))
    ((draw-pixmap w) "Inicio.png" (make-posn 880 550))
    ((draw-pixmap w) "Rossi.jpg" (make-posn 620 20))
    ))

;SE DEFINE FUNCION QUE PINTA EL TABLERO DONDE SE UBICARAN LAS FICHAS
(define (pintarcuadro px py lado)
  (begin
   ((draw-rectangle w) (make-posn px py) lado lado (make-rgb 1 1 1))))

(define (pintarfila px py lado n)
  (if (> n 0)
      (begin
        (pintarcuadro px py lado)
        (pintarfila (+ px lado) py lado (- n 1)) 
        )))

(define (pintartablero px py lado n)
  (if (> n 0)
      (begin
        (pintarfila px py lado 4)
        (pintartablero px (+ py lado) lado  (- n 1)) 
        )))

;COPIA LA IMAGEN
(define (pintarFicha ficha px py)
  (cond
    ((= ficha 1) ((draw-pixmap w) "Rossi1.jpg" (make-posn px py)))
    ((= ficha 2) ((draw-pixmap w) "Rossi2.jpg" (make-posn px py)))
    ((= ficha 3) ((draw-pixmap w) "Rossi3.jpg" (make-posn px py)))
    ((= ficha 4) ((draw-pixmap w) "Rossi4.jpg" (make-posn px py)))
    ((= ficha 5) ((draw-pixmap w) "Rossi5.jpg" (make-posn px py)))
    ((= ficha 6) ((draw-pixmap w) "Rossi6.jpg" (make-posn px py)))
    ((= ficha 7) ((draw-pixmap w) "Rossi7.jpg" (make-posn px py)))
    ((= ficha 8) ((draw-pixmap w) "Rossi8.jpg" (make-posn px py)))
    ((= ficha 9) ((draw-pixmap w) "Rossi9.jpg" (make-posn px py)))
    ((= ficha 10) ((draw-pixmap w) "Rossi10.jpg" (make-posn px py)))
    ((= ficha 11) ((draw-pixmap w) "Rossi11.jpg" (make-posn px py)))
    ((= ficha 12) ((draw-pixmap w) "Rossi12.jpg" (make-posn px py)))
    ((= ficha 13) ((draw-pixmap w) "Rossi13.jpg" (make-posn px py)))
    ((= ficha 14) ((draw-pixmap w) "Rossi14.jpg" (make-posn px py)))
    ((= ficha 15) ((draw-pixmap w) "Rossi15.jpg" (make-posn px py)))
    ((= ficha 16) ((draw-pixmap w) "Rossi16.jpg" (make-posn px py)))
    ((= ficha 17) ((draw-pixmap w) "Rossi17.jpg" (make-posn px py)))
    ((= ficha 18) ((draw-pixmap w) "Rossi18.jpg" (make-posn px py)))
    ((= ficha 19) ((draw-pixmap w) "Rossi19.jpg" (make-posn px py)))
    ((= ficha 20) ((draw-pixmap w) "Rossi20.jpg" (make-posn px py)))
    ))

;UBICA UNA FICHA EN LA COLUMNA Y FILA DEL TABLERO DADAS
(define (moverFicha ficha col fila px py lado)
  (pintarFicha ficha (+ (* (- col 1) lado) (+ px 1)) (+ (* (- fila 1) lado) (+ py 1))))

;UBICA GRAFICAMENTE LAS FICHAS EN EL TABLERO EN PANTALLA
(define (ubicarFichas px py lado)
  (begin
     (moverFicha 18 1 1 px py lado)
     (moverFicha 20 2 1 px py lado) 
     (moverFicha 5 3 1 px py lado) 
     (moverFicha 3 4 1 px py lado) 
     (moverFicha 7 1 2 px py lado)
     (moverFicha 4 2 2 px py lado) 
     (moverFicha 19 3 2 px py lado) 
     (moverFicha 1 4 2 px py lado)
     (moverFicha 15 1 3 px py lado)
     (moverFicha 17 2 3 px py lado)
     (moverFicha 10 3 3 px py lado)
     (moverFicha 14 4 3 px py lado)
     (moverFicha 6 1 4 px py lado)
     (moverFicha 8 2 4 px py lado)
     (moverFicha 12 3 4 px py lado)
     (moverFicha 9 4 4 px py lado)
     (moverFicha 13 1 5 px py lado)
     (moverFicha 11 2 5 px py lado) 
     (moverFicha 16 3 5 px py lado) 
     (moverFicha 2 4 5 px py lado) 
     ))

;CREA UN VECTOR DE 20 CASILLAS PARA LAS FICHAS
(define (crearJuego T)
  (begin
    (vector-set! T 0 18)
    (vector-set! T 1 20)
    (vector-set! T 2 5)
    (vector-set! T 3 3)
    (vector-set! T 4 7)
    (vector-set! T 5 4)
    (vector-set! T 6 19)
    (vector-set! T 7 1)
    (vector-set! T 8 15)
    (vector-set! T 9 17)
    (vector-set! T 10 10)
    (vector-set! T 11 14)
    (vector-set! T 12 6)
    (vector-set! T 13 8)
    (vector-set! T 14 12)
    (vector-set! T 15 9)
    (vector-set! T 16 13)
    (vector-set! T 17 11)
    (vector-set! T 18 16)
    (vector-set! T 19 2)
    T
    ))

;REALIZA LOS MOVIMIENTOS DADOS
(define (realizarMovimiento c1 f1 c2 f2 T px py lado posn1 posn2)
  (begin
    ;COPIA EN LA POSICIION DESTINO LA INFORMACION DEL VECTOR ORIGEN
     (vector-set! T (+ (- c2 1) (* 4 (- f2 1))) posn1)
    ;COPIA EN LA POSICION ORIGEN LA INFORMACION DEL VECTOR DESTINO
    (vector-set! T (+ (- c1 1) (* 4 (- f1 1))) posn2)
    ;PONE LA IMAGEN DE LA FICHA QUE SE MUEVE EN LA POSISCIO DESTINO EN PANTALLA
    (moverFicha (vector-ref T (+ (- c2 1) (* 4 (- f2 1)))) c2 f2 px py lado)
    ;PONE LA IMAGEN DEL LA FICHA QUE NO SE MUEVE EN LA POSICION ORIGEN EN PANTALLA
    (moverFicha (vector-ref T (+ (- c1 1) (* 4 (- f1 1)))) c1 f1 px py lado)
    ))

;VALIDA LA JUGADA
(define (validarJugada c1 f1 c2 f2 T px py lado)
  (realizarMovimiento c1 f1 c2 f2 T px py lado (vector-ref T (+ (- c1 1) (* 4 (- f1 1)))) (vector-ref T (+ (- c2 1) (* 4 (- f2 1)))))
    )
    
;PULSAR 2
(define (pulsar2 c1 f1 p2 T px py lado)
  (begin
    (display (posn-x p2)) 
    (newline)
    (display (posn-y p2))
    (newline)
    (display (+ 1 (quotient (- (posn-x p2) px) lado)))
    (newline)
    (display (+ 1 (quotient (- (posn-y p2) py) lado)))
    (newline)
    (validarJugada c1 f1 (+ 1 (quotient (- (posn-x p2) px) lado))  (+ 1 (quotient (- (posn-y p2) py) lado)) T px py lado)
    ))

;PULSAR 1
(define (pulsar1 p1 T px py lado)
  (begin
;    (display (posn-x p1)) 
;    (newline)
;    (display (posn-y p1))
;    (newline)
;    (display (+ 1 (quotient (- (posn-x p1) px) lado)))
;    (newline)
;    (display (+ 1 (quotient (- (posn-y p1) px) lado)))
;    (newline)
    (pulsar2 (+ 1 (quotient (- (posn-x p1) px) lado))  (+ 1 (quotient (- (posn-y p1) px) lado)) (mouse-click-posn (get-mouse-click w)) T px py lado)
    ))

;DEFINE EL NUMERO DE MOVIMIENTOS
(define (juego c px py lado T)
  (if (< c 24)
      (begin
         (pulsar1  (mouse-click-posn (get-mouse-click w)) T px py lado)
         (juego (+ c 1) px py lado T)
     )))

;EJECUTA EL NIVEL DIFICIL  
(define (niveldificil px py lado)
  (begin
    (pintarfondo)
    (pintartablero px py lado 5)
    (ubicarFichas px py lado)
    (juego 1 px py lado (crearJuego (make-vector 20 0)))
    ))


;___________________________________________________________________________________________________________________________________________________

;DEFINE EL EL JUEGO TOTAL 
(define (puzzle)
  (begin
    (pintarmenu)
    (botones (get-mouse-click w))
    ))

(puzzle)