#lang racket

(require graphics/graphics)
(require racket)

(include "graphics-nm.rkt")

;Main Menu

(define (main f)
  (open-graphics)
  (define main-windw (open-viewport "Main Menu" 700 700))
  (define main-mouse-click-posn 0)
  
  (if (equal? f "open") 
      (begin
        ((draw-pixmap main-windw) "nine-men.jpg" (make-posn 0 0))
        (sleep/yield 2)
        ((draw-pixmap main-windw) "blue-back.jpg" (make-posn 0 0))
        ((draw-pixmap main-windw) "nine-top.png" (make-posn 200 0))
        ((draw-pixmap main-windw) "new-game.jpg" (make-posn 50 200))
        ((draw-pixmap main-windw) "instructions.png" (make-posn 500 300))
        ((draw-pixmap main-windw) "credits.png" (make-posn 50 400))
        ((draw-pixmap main-windw) "exit.jpg" (make-posn 500 500))
        )
      (begin 
        ((draw-pixmap main-windw) "blue-back.jpg" (make-posn 0 0))
        ((draw-pixmap main-windw) "nine-top.png" (make-posn 200 0))
        ((draw-pixmap main-windw) "new-game.jpg" (make-posn 50 200))
        ((draw-pixmap main-windw) "instructions.png" (make-posn 500 300))
        ((draw-pixmap main-windw) "credits.png" (make-posn 50 400))
        ((draw-pixmap main-windw) "exit.jpg" (make-posn 500 500))))

          
  
  (define (helper)
    (set! main-mouse-click-posn (mouse-click-posn (get-mouse-click main-windw)))
    (cond [(and (> (posn-x main-mouse-click-posn) 50) (< (posn-x main-mouse-click-posn) 200) (> (posn-y main-mouse-click-posn) 200) (< (posn-y main-mouse-click-posn) 350))
           (begin
             (close-viewport main-windw)
             (new))]
          [(and (> (posn-x main-mouse-click-posn) 500)(< (posn-x main-mouse-click-posn) 650) (> (posn-y main-mouse-click-posn) 300) (< (posn-y main-mouse-click-posn) 450))
           (begin 
             (close-viewport main-windw)
             (instructions))]
          [(and (> (posn-x main-mouse-click-posn) 50) (< (posn-x main-mouse-click-posn) 200) (> (posn-y main-mouse-click-posn) 400) (< (posn-y main-mouse-click-posn) 550))
           (begin 
             (close-viewport main-windw)
             (credits))]
          [(and (> (posn-x main-mouse-click-posn) 500) (< (posn-x main-mouse-click-posn) 650) (> (posn-y main-mouse-click-posn) 500) (< (posn-y main-mouse-click-posn) 650))
           (begin
             (close-viewport main-windw)
             (close-graphics))]
          [else (helper)]))
  (helper))



;Instructions

(define (instructions)
  (define instruct-windw (open-viewport "How To Play?" 700 700))
  (define ins-mouse-click-posn 0)
  (begin
    ((draw-pixmap instruct-windw) "how-to-play.gif" (make-posn 0 0))
    ((draw-pixmap instruct-windw) "back2.png" (make-posn 270 550)))
  (define (helper)
    (set! ins-mouse-click-posn (mouse-click-posn (get-mouse-click instruct-windw)))
    (cond [(and (> (posn-x ins-mouse-click-posn) 270) (< (posn-x ins-mouse-click-posn) 420) (> (posn-y ins-mouse-click-posn) 550) (< (posn-y ins-mouse-click-posn) 650))
           (begin
             (close-viewport instruct-windw)
             (main "opened"))]
          [else (helper)]))
  (helper))

;Credits
 
(define (credits)
  (define credits-windw (open-viewport "Credits" 700 700))
  (define credits-mouse-click-posn 0)
  (begin
    ((draw-pixmap credits-windw) "credits2.jpg" (make-posn 0 0))
    ((draw-pixmap credits-windw) "back.png" (make-posn 300 550))
    )
    (define (helper)
    (set! credits-mouse-click-posn (mouse-click-posn (get-mouse-click credits-windw)))
    (cond [(and (> (posn-x credits-mouse-click-posn) 300) (< (posn-x credits-mouse-click-posn) 450) (> (posn-y credits-mouse-click-posn) 550) (< (posn-y credits-mouse-click-posn) 650))
           (begin
             (close-viewport credits-windw)
             (main "opened"))]
          [else (helper)]))
  (helper))
    
          
           
    