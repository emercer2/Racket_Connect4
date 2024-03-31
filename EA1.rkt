#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)


(define-struct world [indicator pLst])

(define initialPLst (list (list "black" "black" "black" "black" "black" "black" "black") (list "black" "black" "black" "black" "black" "black" "black") (list "black" "black" "black" "black" "black" "black" "black") (list "black" "black" "black" "black" "black" "black" "black") (list "black" "black" "black" "black" "black" "black" "black") (list "black" "black" "black" "black" "black" "black" "black")))
(define initial-world (make-world (make-posn 60 20) initialPLst))
(define curPlayer "yellow")
(define win '())

(define (draw-game world)
  (cond
    [(equal? win '())
     (place-image
      (getPSetup 0 0 (world-pLst world))
      240 235
      (place-image 
       (isosceles-triangle 30 300 "solid" curPlayer)
       (posn-x (world-indicator world))
       (posn-y (world-indicator world))
       (rectangle 470 445 "solid" "black"))
      )
    ]
    [else
     (place-image
       (displayText)
       240 235
       (rectangle 470 445 "solid" "black"))
    ]
  )
)

(define (mouse-fn world x y event)
  ;left 0 right 500
  (define setx x)
  (cond
    [(< x 90) (set! setx 60)]
    [(and (< x 150) (> x 90)) (set! setx 120)]
    [(and (< x 210) (> x 150)) (set! setx 180)]
    [(and (< x 270) (> x 210)) (set! setx 240)]
    [(and (< x 330) (> x 270)) (set! setx 300)]
    [(and (< x 390) (> x 330)) (set! setx 360)]
    [(> x 390) (set! setx 420)]
  )
  (cond
    [(string=? "move" event)
     (make-world (make-posn setx 20) (world-pLst world))
    ]
    [(string=? "button-up" event)
     (cond
     [(equal? win '())
      (define pos x)
      (define row y)
      (cond
        [(<= x 90) (set! pos 0)]
        [(and (<= x 150) (> x 90)) (set! pos 1)]
        [(and (<= x 210) (> x 150)) (set! pos 2)]
        [(and (<= x 270) (> x 210)) (set! pos 3)]
        [(and (<= x 330) (> x 270)) (set! pos 4)]
        [(and (<= x 390) (> x 330)) (set! pos 5)]
        [(> x 390) (set! pos 6)]
      )
      ;iterate throgh rows through rows to find next non-black token (set color) also changes cur player
      (define newPlst (placePeice pos (world-pLst world) 0))
     
      (make-world (make-posn setx 20) newPlst)
      ]
     [else
      (set! win '())
      (make-world (make-posn 60 20) initialPLst)]
    )]
    [else world]
  )
)

  
(define (placePeice pos Plst curRow)
  (cond
    [(equal? curRow 6) Plst]
    [(equal? (list-ref (list-ref Plst curRow) pos) "black")
     ;set row in position
     (set! Plst (list-set Plst curRow (list-set (list-ref Plst curRow) pos curPlayer)))
     (set! win (assessWin Plst curRow pos))
     (cond
       [(equal? curPlayer "yellow") (set! curPlayer "red")]
       [else
        (set! curPlayer "yellow")
        ]
       )
     Plst
    ]
    [else (placePeice pos Plst (+ curRow 1))]
  )
)

(define (assessWin Plst row pos)
  (cond
    [(>= (horizontalWin Plst row 0 0) 4) curPlayer]
    [(>= (verticalWin Plst 0 pos 0) 4) curPlayer]
    [(>= (rightDiagonalWin Plst row pos 0 0) 4) curPlayer]
    [(>= (leftDiagonalWin Plst row pos 0 0) 4) curPlayer]
    [(fullboard Plst 0 0) "Tie"]
    [else null]
  )
)

(define (horizontalWin Plst row pos counter)
  (cond
    [(equal? counter 4) 4]
    [(equal? pos 7) counter]
    [(equal? (list-ref (list-ref Plst row) pos) curPlayer) (horizontalWin Plst row (+ pos 1) (+ counter 1))]
    [else (horizontalWin Plst row (+ pos 1) 0)]
  )
)

(define (verticalWin Plst row pos counter)
  (cond
    [(equal? counter 4) 4]
    [(equal? row 6) counter]
    [(equal? (list-ref (list-ref Plst row) pos) curPlayer) (verticalWin Plst (+ row 1) pos (+ counter 1))]
    [else (verticalWin Plst (+ row 1) pos 0)]
  )
)

(define (rightDiagonalWin Plst row pos strtF counter)
  (cond
    [(equal? pos 7) counter]
    [(equal? row -1) counter]
    [(equal? strtF 0)
     (cond
       [(and (not (or (equal? row 5) (equal? pos 0))) (equal? (list-ref (list-ref Plst (+ row 1)) (- pos 1)) curPlayer))
           (+ 0 (rightDiagonalWin Plst (+ row 1) (- pos 1) strtF counter));move back one if room and that peice is curPlayer
       ]
       [else (+ 0 (rightDiagonalWin Plst row pos 1 counter))];sets strtF to 1
     )
    ]
    [else
     (cond
       [(equal? (list-ref (list-ref Plst row) pos) curPlayer)
        (rightDiagonalWin Plst (- row 1) (+ pos 1) strtF (+ counter 1))
        ]
       [else counter]
      )
    ]
  )
)

(define (leftDiagonalWin Plst row pos strtF counter)
  (cond
    [(equal? pos -1) counter]
    [(equal? row -1) counter]
    [(equal? strtF 0)
     (cond
       [(and (not (or (equal? row 5) (equal? pos 6))) (equal? (list-ref (list-ref Plst (+ row 1)) (+ pos 1)) curPlayer))
           (leftDiagonalWin Plst (+ row 1) (+ pos 1) strtF counter);move back one if room and that peice is curPlayer
       ]
       [else (leftDiagonalWin Plst row pos 1 counter)];sets strtF to 1
     )
    ]
    [else
     (cond
       [(equal? (list-ref (list-ref Plst row) pos) curPlayer)
        (leftDiagonalWin Plst (- row 1) (- pos 1) strtF (+ 1 counter))
        ]
       [else counter]
      )
    ]
  )
)

(define (fullboard Plst row pos)
  (cond
    [(equal? row 6) true]
    [(equal? (list-ref (list-ref Plst row) pos) "black") false]
    [(equal? pos 6) (fullboard Plst (+ row 1) 0)]
    [else (fullboard Plst row (+ pos 1))]
  )
)
  
(define (getPSetup row pos Plst)
  (cond
    [(equal? row 6) (rectangle 430 370 "solid" "blue")]
    [else
     (define x 0)
     (define y 0)
     (cond
       [(equal? row 0) (set! y 150)]
       [(equal? row 1) (set! y 90)]
       [(equal? row 2) (set! y 30)]
       [(equal? row 3) (set! y -30)]
       [(equal? row 4) (set! y -90)]
       [(equal? row 5) (set! y -150)]
     )
     (cond
       [(equal? pos 0) (set! x -180)]
       [(equal? pos 1) (set! x -120)]
       [(equal? pos 2) (set! x -60)]
       [(equal? pos 3) (set! x 0)]
       [(equal? pos 4) (set! x 60)]
       [(equal? pos 5) (set! x 120)]
       [(equal? pos 6) (set! x 180)]
     )
     (define newRow row)
     (define newPos (+  pos 1))
     (cond
       [(equal? pos 6)
         (set! newRow (+ row 1))
         (set! newPos 0)
       ]
     )
     (underlay/offset (getPSetup newRow newPos Plst) x y (circle 25 "solid" (list-ref (list-ref Plst row) pos)))
    ]
  )
)

(define (displayText)
  (cond
    [(equal? win "red") (text "Red Wins!" 24 win)]
    [(equal? win "yellow") (text "Yellow Wins!" 24 win)]
    [(equal? win "Tie") (text "Tie" 24 "white")]
  )
)

(define (connect4)
  (big-bang
    initial-world
    [to-draw draw-game]
    [on-mouse mouse-fn]))

(connect4)