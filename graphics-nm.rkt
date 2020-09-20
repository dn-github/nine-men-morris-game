#lang racket

(require graphics/graphics)
(require racket)

(define (new)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;<LIST COMPREHENSION>;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (concat l) (foldr append `() l))
  
  (define-syntax lc
    (syntax-rules (: <- *)
      [(lc expr : var <- drawn-from) (map (lambda (var) expr) drawn-from)]
      [(lc expr : * guard) (if guard (list expr) `())]
      [(lc expr : * guard  qualifier ...) 
       (concat (lc (lc expr : qualifier ...) : guard))]
      [(lc expr : var <- drawn-from  qualifier ...) 
       (concat (lc (lc expr :  qualifier ... ) : var <- drawn-from))]))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;<VECTOR DEFINITIONS>;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (make-2d-vector r c initial)
    (build-vector r (lambda (x) (make-vector c initial))))
  
  (define (2d-vector-ref vec r c)
    (vector-ref (vector-ref vec r) c))
  
  (define (2d-vector-set! vec r c val)
    (vector-set! (vector-ref vec r) c val))
  
  ;(define (vector-append v1 v2)
  ;  (list->vector (append (vector->list v1) (vector->list v2))))
  ;
  ;
  ;(define (vector-filter pred v1)
  ;  (list->vector (filter pred (vector->list v1))))
  ;
  ;(define (vector-drop v1 pos)
  ;  (if (> pos (vector-length v1)) `fail
  ;      (if (= pos 0) v1
  ;          (vector-drop (list->vector (cdr (vector->list v1))) (- pos 1)))))
  
  
  (define human-white-pos 
    (make-vector 9 (cons -1 -1)))
  
  ;(vector-set! human-white-pos 0 (cons 0 0))
  ;(vector-set! human-white-pos 1 (cons 0 1))
  ;(vector-set! human-white-pos 2 (cons 0 3))
  ;(vector-set! human-white-pos 3 (cons 0 6))
  ;(vector-set! human-white-pos 4 (cons 1 0))
  ;(vector-set! human-white-pos 5 (cons 1 2))
  ;(vector-set! human-white-pos 6 (cons 1 4))
  ;(vector-set! human-white-pos 7 (cons 1 5))
  ;(vector-set! human-white-pos 8 (cons 2 7))
  
  ;;Its a 9X1 matrix with a cons element in each position.Cons element will be (cons -1 -1) if the coin has not come onto the board till now.
  ;;(cons -2 -2) if the coin is dead.(cons x y) x=(0 to 7) y=(0 to 7) 
  (define computer-black-pos 
    (make-vector 9 (cons -1 -1)))
  
  ;(vector-set! computer-black-pos 0 (cons 0 2))
  ;(vector-set! computer-black-pos 1 (cons 0 4))
  ;(vector-set! computer-black-pos 2 (cons 0 5))
  ;(vector-set! computer-black-pos 3 (cons 0 7))
  ;(vector-set! computer-black-pos 4 (cons 1 1))
  ;(vector-set! computer-black-pos 5 (cons 1 3))
  ;(vector-set! computer-black-pos 6 (cons 1 6))
  ;(vector-set! computer-black-pos 7 (cons 1 7))
  ;;(vector-set! computer-black-pos 8 (cons 2 0))
  
  
  ;;Its a 3X8 matrix with a string element in its each position. String will be "EMPTY" if that positon is empty or ("HW0" to "HW8" and "CB0" to "CB8")
  (define board-status
    (make-2d-vector 3 8 "EMPTY"))
  
  ;(2d-vector-set! board-status 0 2 "CB0")
  ;(2d-vector-set! board-status 0 4 "CB1")
  ;(2d-vector-set! board-status 0 5 "CB2")
  ;(2d-vector-set! board-status 0 7 "CB3")
  ;(2d-vector-set! board-status 1 1 "CB4")
  ;(2d-vector-set! board-status 1 3 "CB5")
  ;(2d-vector-set! board-status 1 6 "CB6")
  ;(2d-vector-set! board-status 1 7 "CB7")
  ;;(2d-vector-set! board-status 2 3 "CB8")
  ;(2d-vector-set! board-status 0 0 "HW0")
  ;(2d-vector-set! board-status 0 1 "HW1")
  ;(2d-vector-set! board-status 0 3 "HW2")
  ;(2d-vector-set! board-status 0 6 "HW3")
  ;(2d-vector-set! board-status 1 0 "HW4")
  ;(2d-vector-set! board-status 1 2 "HW5")
  ;(2d-vector-set! board-status 1 4 "HW6")
  ;(2d-vector-set! board-status 1 5 "HW7")
  ;(2d-vector-set! board-status 2 7 "HW8")
  
  (define selected-piece 
    (cons "POS" "COIN-ID"))
  
  (define count -1)
  
  (define mycount 0)
  
  (define reference (build-vector 9 (lambda (x) x)))
  
  (vector-set! reference 0 #\0)
  (vector-set! reference 1 #\1)
  (vector-set! reference 2 #\2)
  (vector-set! reference 3 #\3)
  (vector-set! reference 4 #\4)
  (vector-set! reference 5 #\5)
  (vector-set! reference 6 #\6)
  (vector-set! reference 7 #\7)
  (vector-set! reference 8 #\8)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define helper-list1
    (list (cons 2 5)
          (cons 0 0)
          (cons 2 7)
          (cons 0 1)
          (cons 0 2)
          (cons 0 3)
          (cons 0 4)
          (cons 0 5)
          (cons 0 6)
          (cons 0 7)
          (cons 1 0)
          (cons 1 1)
          (cons 1 2)
          (cons 1 3)
          (cons 1 4)
          (cons 1 5)
          (cons 1 6)
          (cons 1 7)
          (cons 2 0)
          (cons 2 1)
          (cons 2 2)
          (cons 2 3)
          (cons 2 4)
          (cons 2 6)))
  
  
  
  
  (define helper-list
    (list (cons (cons 0 0) (list (cons 0 1) (cons 0 7)))
          (cons (cons 0 1) (list (cons 0 0) (cons 0 2) (cons 1 1)))
          (cons (cons 0 2) (list (cons 0 1) (cons 0 3)))
          (cons (cons 0 3) (list (cons 0 2) (cons 0 4) (cons 1 3)))
          (cons (cons 0 4) (list (cons 0 3) (cons 0 5)))
          (cons (cons 0 5) (list (cons 0 4) (cons 0 6) (cons 1 5)))
          (cons (cons 0 6) (list (cons 0 5) (cons 0 7)))
          (cons (cons 0 7) (list (cons 0 0) (cons 0 6) (cons 1 7)))
          (cons (cons 1 0) (list (cons 1 1) (cons 1 7)))
          (cons (cons 1 1) (list (cons 1 0) (cons 1 2) (cons 0 1) (cons 2 1)))
          (cons (cons 1 2) (list (cons 1 1) (cons 1 3)))
          (cons (cons 1 3) (list (cons 1 2) (cons 1 4) (cons 0 3) (cons 2 3)))
          (cons (cons 1 4) (list (cons 1 3) (cons 1 5)))
          (cons (cons 1 5) (list (cons 1 4) (cons 1 6) (cons 0 5) (cons 2 5)))
          (cons (cons 1 6) (list (cons 1 5) (cons 1 7)))
          (cons (cons 1 7) (list (cons 1 0) (cons 1 6) (cons 0 7) (cons 2 7)))
          (cons (cons 2 0) (list (cons 2 1) (cons 2 7)))
          (cons (cons 2 1) (list (cons 2 0) (cons 2 2) (cons 1 1)))
          (cons (cons 2 2) (list (cons 2 1) (cons 2 3)))
          (cons (cons 2 3) (list (cons 2 2) (cons 2 4) (cons 1 3)))
          (cons (cons 2 4) (list (cons 2 3) (cons 2 5)))
          (cons (cons 2 5) (list (cons 2 4) (cons 2 6) (cons 1 5)))
          (cons (cons 2 6) (list (cons 2 5) (cons 2 7)))
          (cons (cons 2 7) (list (cons 2 0) (cons 2 6) (cons 1 7)))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;<GRAPHICS>;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define vec-pos (make-2d-vector 3 8 (cons -1 -1)))
  
  (2d-vector-set! vec-pos 0 0 (cons 0 0))
  (2d-vector-set! vec-pos 0 1 (cons 270 0))
  (2d-vector-set! vec-pos 0 2 (cons 550 0))
  (2d-vector-set! vec-pos 0 3 (cons 550 275))
  (2d-vector-set! vec-pos 0 4 (cons 550 540))
  (2d-vector-set! vec-pos 0 5 (cons 270 540))
  (2d-vector-set! vec-pos 0 6 (cons 0 540))
  (2d-vector-set! vec-pos 0 7 (cons 0 275))
  (2d-vector-set! vec-pos 1 0 (cons 60 60))
  (2d-vector-set! vec-pos 1 1 (cons 270 60))
  (2d-vector-set! vec-pos 1 2 (cons 480 60))
  (2d-vector-set! vec-pos 1 3 (cons 480 275))
  (2d-vector-set! vec-pos 1 4 (cons 480 470))
  (2d-vector-set! vec-pos 1 5 (cons 270 470))
  (2d-vector-set! vec-pos 1 6 (cons 60 470))
  (2d-vector-set! vec-pos 1 7 (cons 60 275))
  (2d-vector-set! vec-pos 2 0 (cons 130 130))
  (2d-vector-set! vec-pos 2 1 (cons 270 130))
  (2d-vector-set! vec-pos 2 2 (cons 410 130))
  (2d-vector-set! vec-pos 2 3 (cons 410 275))
  (2d-vector-set! vec-pos 2 4 (cons 410 400))
  (2d-vector-set! vec-pos 2 5 (cons 270 400))
  (2d-vector-set! vec-pos 2 6 (cons 130 400))
  (2d-vector-set! vec-pos 2 7 (cons 130 275))
  
  (define (outhelper vec r c)
    (if (> r 2) "Done"
        (if (> c 7) (outhelper vec (+ r 1) 0)
            (cond [(equal? (2d-vector-ref vec r c) "HW")
                   (begin 
                     ((draw-pixmap windw) "white-1.gif" (make-posn (car (2d-vector-ref vec-pos r c)) (cdr (2d-vector-ref vec-pos r c))))
                     (outhelper vec r (+ c 1)))]
                  [(equal? (2d-vector-ref vec r c) "CB")
                   (begin 
                     ((draw-pixmap windw) "black-1.gif" (make-posn (car (2d-vector-ref vec-pos r c)) (cdr (2d-vector-ref vec-pos r c))))
                     (outhelper vec r (+ c 1)))]
                  [(equal? (2d-vector-ref vec r c) "HHW")
                   (begin 
                     ((draw-pixmap windw) "highlight-1.gif" (make-posn (car (2d-vector-ref vec-pos r c)) (cdr (2d-vector-ref vec-pos r c))))
                     (outhelper vec r (+ c 1)))]
                  
                  [else (outhelper vec r (+ c 1))]))))
  
  
  (define (main2 vec i)
    
    
    (define (helper r c)
      (if (> r 2) "Done"
          (if (> c 7) (helper (+ r 1) 0)
              (cond [(equal? (substring (2d-vector-ref vec r c) 0 2) "HW")
                     (begin 
                       ((draw-pixmap windw) "white-1.gif" (make-posn (car (2d-vector-ref vec-pos r c)) (cdr (2d-vector-ref vec-pos r c))))
                       (helper r (+ c 1)))]
                    [(equal? (substring (2d-vector-ref vec r c) 0 2) "CB")
                     (begin 
                       ((draw-pixmap windw) "black-1.gif" (make-posn (car (2d-vector-ref vec-pos r c)) (cdr (2d-vector-ref vec-pos r c))))
                       (helper r (+ c 1)))]
                    [(equal? (substring (2d-vector-ref vec r c) 0 3) "HHW")
                     (begin 
                       ((draw-pixmap windw) "highlight-1.gif" (make-posn (car (2d-vector-ref vec-pos r c)) (cdr (2d-vector-ref vec-pos r c))))
                       (helper r (+ c 1)))]
                    
                    [else (begin 
                            ((clear-pixel windw) (make-posn (car (2d-vector-ref vec-pos r c)) (cdr (2d-vector-ref vec-pos r c))))
                            (helper r (+ c 1)))]))))
    
    (define (invalid)
      
      (define frame (new frame%
                         [label "Example"]
                         [width 100]
                         [height 40]))
      
      (define msg (new message% [parent frame]
                       [label "Invalid Selection"]))
      
      (send frame show #t))
    
    (define (coordinates)
      (define mouse-click-pos 0)
      (begin 
        (set! mouse-click-pos (mouse-click-posn (get-mouse-click windw)))
        (cond [(and (< (posn-x mouse-click-pos) 39) (< (posn-y mouse-click-pos) 39)) (cons 0 0)]
              [(and (< (posn-x mouse-click-pos) 315) (> (posn-x mouse-click-pos) 270) (< (posn-y mouse-click-pos) 45) (> (posn-y mouse-click-pos) 0)) (cons 0 1)]
              [(and (< (posn-x mouse-click-pos) 600) (> (posn-x mouse-click-pos) 550) (< (posn-y mouse-click-pos) 45) (> (posn-y mouse-click-pos) 0)) (cons 0 2)]
              [(and (< (posn-x mouse-click-pos) 600) (> (posn-x mouse-click-pos) 550) (< (posn-y mouse-click-pos) 320) (> (posn-y mouse-click-pos) 275)) (cons 0 3)]
              [(and (< (posn-x mouse-click-pos) 600) (> (posn-x mouse-click-pos) 550) (< (posn-y mouse-click-pos) 600) (> (posn-y mouse-click-pos) 545)) (cons 0 4)]
              [(and (< (posn-x mouse-click-pos) 315) (> (posn-x mouse-click-pos) 270) (< (posn-y mouse-click-pos) 600) (> (posn-y mouse-click-pos) 545)) (cons 0 5)]
              [(and (< (posn-x mouse-click-pos) 39) (> (posn-x mouse-click-pos) 0) (< (posn-y mouse-click-pos) 600) (> (posn-y mouse-click-pos) 545)) (cons 0 6)]
              [(and (< (posn-x mouse-click-pos) 39) (> (posn-x mouse-click-pos) 0) (< (posn-y mouse-click-pos) 320) (> (posn-y mouse-click-pos) 275)) (cons 0 7)]
              [(and (< (posn-x mouse-click-pos) 105) (> (posn-x mouse-click-pos) 65) (< (posn-y mouse-click-pos) 110) (> (posn-y mouse-click-pos) 65)) (cons 1 0)]
              [(and (< (posn-x mouse-click-pos) 315) (> (posn-x mouse-click-pos) 270) (< (posn-y mouse-click-pos) 110) (> (posn-y mouse-click-pos) 65)) (cons 1 1)]
              [(and (< (posn-x mouse-click-pos) 525) (> (posn-x mouse-click-pos) 480) (< (posn-y mouse-click-pos) 110) (> (posn-y mouse-click-pos) 65)) (cons 1 2)]
              [(and (< (posn-x mouse-click-pos) 525) (> (posn-x mouse-click-pos) 480) (< (posn-y mouse-click-pos) 320) (> (posn-y mouse-click-pos) 275)) (cons 1 3)]
              [(and (< (posn-x mouse-click-pos) 525) (> (posn-x mouse-click-pos) 480) (< (posn-y mouse-click-pos) 500) (> (posn-y mouse-click-pos) 450)) (cons 1 4)]
              [(and (< (posn-x mouse-click-pos) 315) (> (posn-x mouse-click-pos) 270) (< (posn-y mouse-click-pos) 500) (> (posn-y mouse-click-pos) 450)) (cons 1 5)]
              [(and (< (posn-x mouse-click-pos) 110) (> (posn-x mouse-click-pos) 65) (< (posn-y mouse-click-pos) 500) (> (posn-y mouse-click-pos) 450)) (cons 1 6)]
              [(and (< (posn-x mouse-click-pos) 105) (> (posn-x mouse-click-pos) 65) (< (posn-y mouse-click-pos) 320) (> (posn-y mouse-click-pos) 275)) (cons 1 7)]
              [(and (< (posn-x mouse-click-pos) 185) (> (posn-x mouse-click-pos) 135) (< (posn-y mouse-click-pos) 185) (> (posn-y mouse-click-pos) 135)) (cons 2 0)]
              [(and (< (posn-x mouse-click-pos) 315) (> (posn-x mouse-click-pos) 270) (< (posn-y mouse-click-pos) 185) (> (posn-y mouse-click-pos) 135)) (cons 2 1)]
              [(and (< (posn-x mouse-click-pos) 454) (> (posn-x mouse-click-pos) 410) (< (posn-y mouse-click-pos) 185) (> (posn-y mouse-click-pos) 135)) (cons 2 2)]
              [(and (< (posn-x mouse-click-pos) 454) (> (posn-x mouse-click-pos) 410) (< (posn-y mouse-click-pos) 320) (> (posn-y mouse-click-pos) 275)) (cons 2 3)]
              [(and (< (posn-x mouse-click-pos) 454) (> (posn-x mouse-click-pos) 410) (< (posn-y mouse-click-pos) 450) (> (posn-y mouse-click-pos) 405)) (cons 2 4)]
              [(and (< (posn-x mouse-click-pos) 315) (> (posn-x mouse-click-pos) 270) (< (posn-y mouse-click-pos) 450) (> (posn-y mouse-click-pos) 405)) (cons 2 5)]
              [(and (< (posn-x mouse-click-pos) 185) (> (posn-x mouse-click-pos) 135) (< (posn-y mouse-click-pos) 450) (> (posn-y mouse-click-pos) 405)) (cons 2 6)]
              [(and (< (posn-x mouse-click-pos) 185) (> (posn-x mouse-click-pos) 135) (< (posn-y mouse-click-pos) 320) (> (posn-y mouse-click-pos) 275)) (cons 2 7)]
              [else "Invalid Selection" ]
              )))
    
    (begin 
      
      ((draw-pixmap windw) "morris.gif" (make-posn 0 0))
      
      (if (= i 1) (begin (helper 0 0) (coordinates))
          (helper 0 0)
          )))
  
  (define (main1 pair)
    (begin (2d-vector-set! board-status (car pair) (cdr pair) "HW")
           (main1 (main2 board-status))))
  
  (define (placing-position-checking circle-id-pair)
    (if (equal? circle-id-pair "Invalid Selection") (placing-position-checking (main2 board-status 1))
        (let ((val-at-circle (2d-vector-ref board-status (car circle-id-pair) (cdr circle-id-pair))))
          (cond [(equal? count 8) (selecting-to-move-position-checking circle-id-pair)]
                [(not (equal? val-at-circle "EMPTY")) (placing-position-checking (main2 board-status 1))] ;;Afterwards include error statement.
                [else (updating-by-placing circle-id-pair)]))))
  
  (define (updating-by-placing final-pos)
    (begin (set! count (+ count 1))
           (set! selected-piece (cons final-pos (string-append "HW" (string (vector-ref reference count)))))
           (2d-vector-set! board-status (car final-pos) (cdr final-pos) (cdr selected-piece))
           (vector-set! human-white-pos (string->number (string (string-ref (cdr selected-piece) 2))) final-pos)
           (if (not (checking-for-mill final-pos `human board-status)) (begin (main2 board-status 2)
                                                                              (let ((most-imp (best-move-generator)))
                                                                                (if (number? (car most-imp)) (begin 
                                                                                                               (2d-vector-set! board-status (car most-imp) (cdr most-imp) (string-append "CB" (string (vector-ref reference mycount))))
                                                                                                               (vector-set! computer-black-pos mycount most-imp)
                                                                                                               (set! mycount (+ mycount 1)))
                                                                                    (begin (2d-vector-set! board-status (caar most-imp) (cdar most-imp) (string-append "CB" (string (vector-ref reference mycount))))
                                                                                           (vector-set! computer-black-pos mycount (car most-imp))
                                                                                           (main2 board-status 2)
                                                                                           (vector-set! human-white-pos (string->number (string (string-ref (2d-vector-ref board-status (caadr most-imp) (cdadr most-imp)) 2))) (cons -2 -2))
                                                                                           (2d-vector-set! board-status (caadr most-imp) (cdadr most-imp) "EMPTY")
                                                                                           (set! mycount (+ mycount 1)))))
                                                                              
                                                                              (placing-position-checking (main2 board-status 1)))
               (selecting-to-remove1-position-checking (main2 board-status 1))
               )))
  
  (define (checking-for-mill final-pos player board-status)
    (let ((loop-no (car final-pos))
          (circle-no (cdr final-pos)))
      (cond [(= circle-no 0) (if (or (checking-mill-D-type loop-no player board-status) (checking-mill-A-type loop-no player board-status)) #t #f)] 
            [(= circle-no 2) (if (or (checking-mill-A-type loop-no player board-status) (checking-mill-B-type loop-no player board-status)) #t #f)]  
            [(= circle-no 4) (if (or (checking-mill-B-type loop-no player board-status) (checking-mill-C-type loop-no player board-status)) #t #f)]  
            [(= circle-no 6) (if (or (checking-mill-C-type loop-no player board-status) (checking-mill-D-type loop-no player board-status)) #t #f)]  
            [(= circle-no 1) (if (or (checking-mill-A-type loop-no player board-status) (checking-mill-E-type 1 player board-status)) #t #f)]  
            [(= circle-no 3) (if (or (checking-mill-B-type loop-no player board-status) (checking-mill-E-type 3 player board-status)) #t #f)]  
            [(= circle-no 5) (if (or (checking-mill-C-type loop-no player board-status) (checking-mill-E-type 5 player board-status)) #t #f)]  
            [(= circle-no 7) (if (or (checking-mill-D-type loop-no player board-status) (checking-mill-E-type 7 player board-status)) #t #f)])))
  
  (define (checking-mill-A-type loop-no player board-status)
    (if (equal? player `human) (and (equal? #\H (string-ref (2d-vector-ref board-status loop-no 0) 0))
                                    (equal? #\H (string-ref (2d-vector-ref board-status loop-no 1) 0))
                                    (equal? #\H (string-ref (2d-vector-ref board-status loop-no 2) 0)))
        (and (equal? #\C (string-ref (2d-vector-ref board-status loop-no 0) 0))
             (equal? #\C (string-ref (2d-vector-ref board-status loop-no 1) 0))
             (equal? #\C (string-ref (2d-vector-ref board-status loop-no 2) 0)))))
  
  (define (checking-mill-B-type loop-no player board-status)
    (if (equal? player `human) (and (equal? #\H (string-ref (2d-vector-ref board-status loop-no 2) 0))
                                    (equal? #\H (string-ref (2d-vector-ref board-status loop-no 3) 0))
                                    (equal? #\H (string-ref (2d-vector-ref board-status loop-no 4) 0)))
        (and (equal? #\C (string-ref (2d-vector-ref board-status loop-no 2) 0))
             (equal? #\C (string-ref (2d-vector-ref board-status loop-no 3) 0))
             (equal? #\C (string-ref (2d-vector-ref board-status loop-no 4) 0)))))
  
  (define (checking-mill-C-type loop-no player board-status)
    (if (equal? player `human) (and (equal? #\H (string-ref (2d-vector-ref board-status loop-no 4) 0))
                                    (equal? #\H (string-ref (2d-vector-ref board-status loop-no 5) 0))
                                    (equal? #\H (string-ref (2d-vector-ref board-status loop-no 6) 0)))
        (and (equal? #\C (string-ref (2d-vector-ref board-status loop-no 4) 0))
             (equal? #\C (string-ref (2d-vector-ref board-status loop-no 5) 0))
             (equal? #\C (string-ref (2d-vector-ref board-status loop-no 6) 0)))))
  
  (define (checking-mill-D-type loop-no player board-status)
    (if (equal? player `human) (and (equal? #\H (string-ref (2d-vector-ref board-status loop-no 6) 0))
                                    (equal? #\H (string-ref (2d-vector-ref board-status loop-no 7) 0))
                                    (equal? #\H (string-ref (2d-vector-ref board-status loop-no 0) 0)))
        (and (equal? #\C (string-ref (2d-vector-ref board-status loop-no 6) 0))
             (equal? #\C (string-ref (2d-vector-ref board-status loop-no 7) 0))
             (equal? #\C (string-ref (2d-vector-ref board-status loop-no 0) 0)))))
  
  (define (checking-mill-E-type circle-no player board-status)
    (if (equal? player `human) (and (equal? #\H (string-ref (2d-vector-ref board-status 0 circle-no) 0))
                                    (equal? #\H (string-ref (2d-vector-ref board-status 1 circle-no) 0))
                                    (equal? #\H (string-ref (2d-vector-ref board-status 2 circle-no) 0)))
        (and (equal? #\C (string-ref (2d-vector-ref board-status 0 circle-no) 0))
             (equal? #\C (string-ref (2d-vector-ref board-status 1 circle-no) 0))
             (equal? #\C (string-ref (2d-vector-ref board-status 2 circle-no) 0)))))
  
  (define (selecting-to-remove1-position-checking circle-id-pair)
    (if (equal? circle-id-pair "Invalid Selection") (selecting-to-remove1-position-checking (main2 board-status 1))
        (let ((val-at-circle (2d-vector-ref board-status (car circle-id-pair) (cdr circle-id-pair))))
          (cond [(not (equal? (string-ref val-at-circle 0) #\C)) (selecting-to-remove1-position-checking (main2 board-status 1))];;Include error statement.
                [else (if (checking-for-mill circle-id-pair `computer board-status) (selecting-to-remove1-position-checking (main2 board-status 1))
                          (updating-by-removing1 circle-id-pair))]))))
  
  (define (updating-by-removing1 circle-id-pair)
    (let ((loop-no (car circle-id-pair))
          (circle-no (cdr circle-id-pair)))
      (begin (vector-set! computer-black-pos (string->number (string (string-ref (2d-vector-ref board-status loop-no circle-no) 2))) (cons -2 -2))
             (2d-vector-set! board-status loop-no circle-no "EMPTY")
             (begin (main2 board-status 2)
                    (let ((most-imp (best-move-generator)))
                      (if (number? (car most-imp)) (begin 
                                                     (2d-vector-set! board-status (car most-imp) (cdr most-imp) (string-append "CB" (string (vector-ref reference mycount))))
                                                     (vector-set! computer-black-pos mycount most-imp)
                                                     (set! mycount (+ mycount 1)))
                          (begin (2d-vector-set! board-status (caar most-imp) (cdar most-imp) (string-append "CB" (string (vector-ref reference mycount))))
                                 (vector-set! computer-black-pos mycount (car most-imp))
                                 (main2 board-status 2)
                                 (vector-set! human-white-pos (string->number (string (string-ref (2d-vector-ref board-status (caadr most-imp) (cdadr most-imp)) 2))) (cons -2 -2))
                                 (2d-vector-set! board-status (caadr most-imp) (cdadr most-imp) "EMPTY")
                                 (set! mycount (+ mycount 1)))))
                    
                    (placing-position-checking (main2 board-status 1))))))
  
  (define (selecting-to-move-position-checking circle-id-pair)
    (if (equal? circle-id-pair "Invalid Selection") (selecting-to-move-position-checking (main2 board-status 1))
        (let ((val-at-circle (2d-vector-ref board-status (car circle-id-pair) (cdr circle-id-pair))))
          (cond [(not (equal? (string-ref val-at-circle 0) #\H)) (selecting-to-move-position-checking (main2 board-status 1))];;Include error statement
                [else (updating-by-selecting circle-id-pair)]))))
  
  (define (updating-by-selecting circle-id-pair)
    (let ((loop-no (car circle-id-pair))
          (circle-no (cdr circle-id-pair)))
      (begin (set! selected-piece (cons circle-id-pair (2d-vector-ref board-status loop-no circle-no)))
             (2d-vector-set! board-status loop-no circle-no (string-append "H" (cdr selected-piece)))
             (moving-position-checking (main2 board-status 1)))))
  
  (define (moving-position-checking circle-id-pair)
    (if (equal? circle-id-pair "Invalid Selection") (moving-position-checking (main2 board-status 1))
        (let ((val-at-circle (2d-vector-ref board-status (car circle-id-pair) (cdr circle-id-pair))))
          (cond [(not (equal? val-at-circle "EMPTY")) (begin (2d-vector-set! board-status (car (car selected-piece)) (cdr (car selected-piece)) (cdr selected-piece))
                                                             (selecting-to-move-position-checking (main2 board-status 1)))]
                [else (cond [(not (checking-adjacent-pos circle-id-pair)) (begin (2d-vector-set! board-status (car (car selected-piece)) (cdr (car selected-piece)) (cdr selected-piece))
                                                                                 (selecting-to-move-position-checking (main2 board-status 1)))]
                            [else (updating-by-moving circle-id-pair)])]))))
  
  (define (checking-adjacent-pos final-pos)
    (let* ((init-pos (car selected-piece))
           (init-loop-no (car init-pos))
           (init-circle-no (cdr init-pos))
           (final-loop-no (car final-pos))
           (final-circle-no (cdr final-pos)))
      (cond [(equal? init-loop-no final-loop-no) (if (equal? (abs (- init-circle-no final-circle-no)) 1)
                                                     #t (if (or (and (= init-circle-no 0) (= final-circle-no 7)) (and (= init-circle-no 7) (= final-circle-no 0)))
                                                            #t #f))]
            [(and (equal? (abs (- init-loop-no final-loop-no)) 1) (or (= init-circle-no final-circle-no 1) (= init-circle-no final-circle-no 3) 
                                                                      (= init-circle-no final-circle-no 5) (= init-circle-no final-circle-no 7))) #t]
            [else #f])))
  
  (define (updating-by-moving final-pos)
    (begin (2d-vector-set! board-status (car final-pos) (cdr final-pos) (cdr selected-piece))
           (2d-vector-set! board-status (car (car selected-piece)) (cdr (car selected-piece)) "EMPTY")
           (vector-set! human-white-pos (string->number (string (string-ref (cdr selected-piece) 2))) final-pos)
           (if (not (checking-for-mill final-pos `human board-status))
               (if (= (cdr (alivepieces)) (cdr (blocked-pieces))) (you-win) (begin (main2 board-status 2)
                                                                                   (let ((most-imp (best-move-generator)))
                                                                                     (if (number? (car (list-ref most-imp 0))) (begin 
                                                                                                                                 (2d-vector-set! board-status (car (list-ref most-imp 1)) (cdr (list-ref most-imp 1)) (2d-vector-ref board-status (car (list-ref most-imp 0)) (cdr (list-ref most-imp 0))))
                                                                                                                                 (vector-set! computer-black-pos (string->number (string (string-ref (2d-vector-ref board-status (car (list-ref most-imp 0)) (cdr (list-ref most-imp 0))) 2))) (list-ref most-imp 1))
                                                                                                                                 (2d-vector-set! board-status (car (list-ref most-imp 0)) (cdr (list-ref most-imp 0)) "EMPTY")
                                                                                                                                 (if (= (car (alivepieces)) (car (blocked-pieces))) (you-lost) (selecting-to-move-position-checking (main2 board-status 1))))
                                                                                         (begin (2d-vector-set! board-status (car (list-ref (list-ref most-imp 0) 1)) (cdr (list-ref (list-ref most-imp 0) 1)) (2d-vector-ref board-status (car (list-ref (list-ref most-imp 0) 0)) (cdr (list-ref (list-ref most-imp 0) 0))))
                                                                                                (vector-set! computer-black-pos (string->number (string (string-ref (2d-vector-ref board-status (car (list-ref (list-ref most-imp 0) 0)) (cdr (list-ref (list-ref most-imp 0) 0))) 2))) (list-ref (list-ref most-imp 0) 1))
                                                                                                (2d-vector-set! board-status (car (list-ref (list-ref most-imp 0) 0)) (cdr (list-ref (list-ref most-imp 0) 0)) "EMPTY")
                                                                                                (main2 board-status 2)
                                                                                                (vector-set! human-white-pos (string->number (string (string-ref (2d-vector-ref board-status (car (list-ref most-imp 1)) (cdr (list-ref most-imp 1))) 2))) (cons -2 -2))
                                                                                                (2d-vector-set! board-status (car (list-ref most-imp 1)) (cdr (list-ref most-imp 1)) "EMPTY")
                                                                                                (if (or (= (car (alivepieces)) (car (blocked-pieces))) (= 2 (no-of-coins `human))) 
                                                                                                    (you-lost) (selecting-to-move-position-checking (main2 board-status 1))))))))                                              
               
               (if (= 3 (no-of-coins `computer)) (you-win) 
                   (selecting-to-remove2-position-checking (main2 board-status 1))))))
  
  
  (define (alivepieces)
    (cons (foldr + 0 (map (lambda (x) (if (equal? x (cons -2 -2)) 0 1)) (vector->list human-white-pos)))
          (foldr + 0 (map (lambda (x) (if (equal? x (cons -2 -2)) 0 1)) (vector->list computer-black-pos)))))
  
  ;returns number of free movement any coin can make
  (define (free-movements coin-id cur-pos)
    
    (define extended-board (build-vector 3 (lambda(x) (let ((m (vector-ref board-status x))) (vector-append (vector-drop m 7) m  (vector (vector-ref m 0)))))))
    
    (define (check-surrounding pair)
      (if (< (cdr pair) 0) 0
          (if (= (remainder (cdr pair) 2) 0) (adjacent-vacant pair)
              (+ (adjacent-vacant pair) (upar-niche-vacant pair)))))
    
    (define (adjacent-vacant pair)  ; number of adjacent vacant place    
      (let ([a (substring (2d-vector-ref extended-board (car pair) (cdr pair)) 0 2)]
            [b (substring (2d-vector-ref extended-board (car pair) (+ 2 (cdr pair))) 0 2)]) 
        (if (and (or (equal? a "HW") (equal? a "CB")) (or (equal? b "HW") (equal? b "CB"))) 0
            (if (or (or (equal? a "HW") (equal? a "CB")) (or (equal? b "HW") (equal? b "CB"))) 1 2))))
    
    (define (upar-niche-vacant pair) ; number of upar niche vacant place
      (let ([a (if (= (car pair) 0) "HW" (substring (2d-vector-ref extended-board (- (car pair) 1) (+ 1 (cdr pair))) 0 2))]
            [b (if (= (car pair) 2) "HW" (substring (2d-vector-ref extended-board (+ 1 (car pair)) (+ (cdr pair) 1)) 0 2))])
        (if (and (or (equal? a "HW") (equal? a "CB")) (or (equal? b "HW") (equal? b "CB"))) 0
            (if (or (or (equal? a "HW") (equal? a "CB")) (or (equal? b "HW") (equal? b "CB"))) 1 2))))
    
    
    (if (not (equal? (or (cons -2 -2) (cons -1 -1)) (vector-ref cur-pos (string->number (string (string-ref coin-id 2))))))
        (check-surrounding (vector-ref cur-pos (string->number (string (string-ref coin-id 2)))))
        0))
  
  ;this function gives the number of blocked pieces (cons white black)
  
  (define (blocked-pieces)
    (define i 0)
    (define j 0)
    
    (let ((temp1 (reverse (lc (begin (set! i (add1 i)) i) : x  <- `("HW0" "HW1" "HW2" "HW3" "HW4" "HW5" "HW6" "HW7" "HW8")
                              * (and (= (free-movements x human-white-pos) 0)(>= (cdr (vector-ref human-white-pos (string->number (string (string-ref x 2))))) 0)))))
          (temp2 (reverse (lc (begin (set! j (add1 j)) j) : x  <- `("CB0" "CB1" "CB2" "CB3" "CB4" "CB5" "CB6" "CB7" "CB8")
                              * (and (= (free-movements x computer-black-pos) 0)(>= (cdr (vector-ref computer-black-pos  (string->number (string (string-ref x 2))))) 0))))))
      
      (cons (if (equal? temp1 `()) 0 (car temp1)) (if (equal? temp2 `()) 0 (car temp2)))))
  
  
  
  (define (selecting-to-remove2-position-checking circle-id-pair)
    (if (equal? circle-id-pair "Invalid Selection") (selecting-to-remove2-position-checking (main2 board-status 1));;Include Error Statement
        (let ((val-at-circle (2d-vector-ref board-status (car circle-id-pair) (cdr circle-id-pair))))
          (cond [(not (equal? (string-ref val-at-circle 0) #\C)) (selecting-to-remove2-position-checking (main2 board-status 1))];;Include error statement.
                [else (if (checking-for-mill circle-id-pair `computer board-status) (selecting-to-remove2-position-checking (main2 board-status 1))
                          (updating-by-removing2 circle-id-pair))]))))
  
  (define (updating-by-removing2 circle-id-pair)
    (let ((loop-no (car circle-id-pair))
          (circle-no (cdr circle-id-pair)))
      (begin (vector-set! computer-black-pos (string->number (string (string-ref (2d-vector-ref board-status loop-no circle-no) 2))) (cons -2 -2))
             (2d-vector-set! board-status loop-no circle-no "EMPTY")
             (if (= (cdr (alivepieces)) (cdr (blocked-pieces))) (you-win) (begin (main2 board-status 2)
                                                                                 (let ((most-imp (best-move-generator)))
                                                                                   (if (number? (car (list-ref most-imp 0))) (begin 
                                                                                                                               (2d-vector-set! board-status (car (list-ref most-imp 1)) (cdr (list-ref most-imp 1)) (2d-vector-ref board-status (car (list-ref most-imp 0)) (cdr (list-ref most-imp 0))))
                                                                                                                               (vector-set! computer-black-pos (string->number (string (string-ref (2d-vector-ref board-status (car (list-ref most-imp 0)) (cdr (list-ref most-imp 0))) 2))) (list-ref most-imp 1))
                                                                                                                               (2d-vector-set! board-status (car (list-ref most-imp 0)) (cdr (list-ref most-imp 0)) "EMPTY")
                                                                                                                               (if (= (car (alivepieces)) (car (blocked-pieces))) (you-lost) (selecting-to-move-position-checking (main2 board-status 1))))
                                                                                       (begin (2d-vector-set! board-status (car (list-ref (list-ref most-imp 0) 1)) (cdr (list-ref (list-ref most-imp 0) 1)) (2d-vector-ref board-status (car (list-ref (list-ref most-imp 0) 0)) (cdr (list-ref (list-ref most-imp 0) 0))))
                                                                                              (vector-set! computer-black-pos (string->number (string (string-ref (2d-vector-ref board-status (car (list-ref (list-ref most-imp 0) 0)) (cdr (list-ref (list-ref most-imp 0) 0))) 2))) (list-ref (list-ref most-imp 0) 1))
                                                                                              (2d-vector-set! board-status (car (list-ref (list-ref most-imp 0) 0)) (cdr (list-ref (list-ref most-imp 0) 0)) "EMPTY") 
                                                                                              (main2 board-status 2)
                                                                                              (vector-set! human-white-pos (string->number (string (string-ref (2d-vector-ref board-status (car (list-ref most-imp 1)) (cdr (list-ref most-imp 1))) 2))) (cons -2 -2))
                                                                                              (2d-vector-set! board-status (car (list-ref most-imp 1)) (cdr (list-ref most-imp 1)) "EMPTY")
                                                                                              (if (or (= (car (alivepieces)) (car (blocked-pieces))) (= 2 (no-of-coins `human))) 
                                                                                                  (you-lost) (selecting-to-move-position-checking (main2 board-status 1)))))))))))
  
  
  
  
  (define (no-of-coins player)
    (if (equal? player `computer) (counter computer-black-pos 8 9)
        (counter human-white-pos 8 9)))
  
  (define (counter vec index ans)
    (if (= index 0) (if (equal? (cons -2 -2) (vector-ref vec 0)) (- ans 1) ans)
        (if (equal? (cons -2 -2) (vector-ref vec index)) (counter vec (- index 1) (- ans 1)) (counter vec (- index 1) ans))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;<END OF GRAPHICS>;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;<BEST MOVE GENERATOR>;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (pos-eval-func-phase1 cur-board-status cur-human-pos cur-comp-pos player closed-morris)
    
    ;;extended-vector 3*8 to 3*9
    (define board1 (build-vector 3 (lambda(x) (let ((m (vector-ref cur-board-status x))) (vector-append  m  (vector (vector-ref m 0)))))))
    
    ;total number complete mills (cons white black)
    (define (no-of-mills)
      
      (define (all-same-element v) 
        (= (length (filter (lambda(x) (equal? (substring x 0 2) (substring (list-ref (vector->list v) 0) 0 2))) (vector->list v))) (length (vector->list v)))) 
      
      (define (no-of-mills-in-loop) 
        (define (helper x y i j)
          (if (and (= i 2) (= j 8)) (cons x y)
              (if (= j 8) (begin (set! i (+ i 1))
                                 (set! j 0) 
                                 (helper x y i j))
                  (let ([r (build-vector 3 (lambda(x) (2d-vector-ref board1 i (+ x j))))])
                    (if (all-same-element r) (begin (if (equal? (substring (vector-ref r 0) 0 2) "HW") (set! x (+ x 1))
                                                        (if (equal? (substring (vector-ref r 0) 0 2) "CB") (set! y (+ y 1))
                                                            (set! x (+ x 0))))
                                                    (helper x y i (+ j 2)))
                        (helper x y i (+ j 2)))))))
        (helper 0 0 0 0))
      
      (define (no-of-mills-on-side) 
        (define (helper  x y k)
          (if (= k 9) (cons x y)
              (let ([r (build-vector 3 (lambda(x) (2d-vector-ref board1 x k)))])
                (if (all-same-element r) (begin (if (equal? (substring (vector-ref r 0) 0 2) "HW") (set! x (+ x 1))
                                                    (if (equal? (substring (vector-ref r 0) 0 2) "CB") (set! y (+ y 1))
                                                        (set! x (+ x 0))))
                                                (helper  x y (+ k 2)))
                    (helper  x y (+ k 2))))))
        (helper 0 0 1))
      
      (let ([a (no-of-mills-on-side)]
            [b (no-of-mills-in-loop)])
        (cons (+ (car a) (car b)) (+ (cdr a) (cdr b)))))
    
    ;returns number of corner pieces (cons white black)  
    (define (corner-pieces) 
      (define (helper  i)
        (if (= i 8) (vector)
            (vector-append (build-vector 3 (lambda(x) (2d-vector-ref cur-board-status x i))) (helper (+ i 2)))))
      
      (let ([v (helper 0)])
        (cons (vector-length (vector-filter (lambda(x) (equal? (substring x 0 2) "HW")) v))
              (vector-length (vector-filter (lambda(x) (equal? (substring x 0 2) "CB")) v)))))
    
    
    ;returns number of pieces on intersection (cons white black)
    (define (intersection-pieces) 
      (let ([v (build-vector 4 (lambda(x) (2d-vector-ref cur-board-status 1 (+ 1 (* 2 x)))))])
        (cons (vector-length (vector-filter (lambda(x) (equal? (substring x 0 2) "HW")) v))
              (vector-length (vector-filter (lambda(x) (equal? (substring x 0 2) "CB")) v)))))
    
    ;returns number of pieces on side (cons white black)
    (define (side-pieces) 
      (let ([v (vector-append (build-vector 4 (lambda(x) (2d-vector-ref cur-board-status 0 (+ 1 (* 2 x)))))
                              (build-vector 4 (lambda(x) (2d-vector-ref cur-board-status 2 (+ 1 (* 2 x))))))])
        (cons (vector-length (vector-filter (lambda(x) (equal? (substring x 0 2) "HW")) v))
              (vector-length (vector-filter (lambda(x) (equal? (substring x 0 2) "CB")) v)))))
    
    ;returns number of free movement any coin can make
    
    (define (free-movements coin-id cur-pos)
      
      (define extended-board (build-vector 3 (lambda(x) (let ((m (vector-ref cur-board-status x))) (vector-append (vector-drop m 7) m  (vector (vector-ref m 0)))))))
      
      (define (check-surrounding pair)
        (if (< (cdr pair) 0) 0
            (if (= (remainder (cdr pair) 2) 0) (adjacent-vacant pair)
                (+ (adjacent-vacant pair) (upar-niche-vacant pair)))))
      
      (define (adjacent-vacant pair)  ; number of adjacent vacant place    
        (let ([a (substring (2d-vector-ref extended-board (car pair) (cdr pair)) 0 2)]
              [b (substring (2d-vector-ref extended-board (car pair) (+ 2 (cdr pair))) 0 2)]) 
          (if (and (or (equal? a "HW") (equal? a "CB")) (or (equal? b "HW") (equal? b "CB"))) 0
              (if (or (or (equal? a "HW") (equal? a "CB")) (or (equal? b "HW") (equal? b "CB"))) 1 2))))
      
      (define (upar-niche-vacant pair) ; number of upar niche vacant place
        (let ([a (if (= (car pair) 0) "HW" (substring (2d-vector-ref extended-board (- (car pair) 1) (+ 1 (cdr pair))) 0 2))]
              [b (if (= (car pair) 2) "HW" (substring (2d-vector-ref extended-board (+ 1 (car pair)) (+ (cdr pair) 1)) 0 2))])
          (if (and (or (equal? a "HW") (equal? a "CB")) (or (equal? b "HW") (equal? b "CB"))) 0
              (if (or (or (equal? a "HW") (equal? a "CB")) (or (equal? b "HW") (equal? b "CB"))) 1 2))))
      
      
      (if (not (equal? (or (cons -2 -2) (cons -1 -1)) (vector-ref cur-pos (string->number (string (string-ref coin-id 2))))))
          (check-surrounding (vector-ref cur-pos (string->number (string (string-ref coin-id 2)))))
          0))
    
    ;this function gives the number of blocked pieces (cons white black)
    
    (define (blocked-pieces)
      (define i 0)
      (define j 0)
      
      (let ((temp1 (reverse (lc (begin (set! i (add1 i)) i) : x  <- `("HW0" "HW1" "HW2" "HW3" "HW4" "HW5" "HW6" "HW7" "HW8")
                                * (and (= (free-movements x cur-human-pos) 0)(>= (cdr (vector-ref cur-human-pos (string->number (string (string-ref x 2))))) 0)))))
            (temp2 (reverse (lc (begin (set! j (add1 j)) j) : x  <- `("CB0" "CB1" "CB2" "CB3" "CB4" "CB5" "CB6" "CB7" "CB8")
                                * (and (= (free-movements x cur-comp-pos) 0)(>= (cdr (vector-ref cur-comp-pos  (string->number (string (string-ref x 2))))) 0))))))
        
        (cons (if (equal? temp1 `()) 0 (car temp1)) (if (equal? temp2 `()) 0 (car temp2)))))
    
    ;;no. of coins (cons white black)
    (define (no-of-coins)
      (cons (counter cur-human-pos 8 9)
            (counter cur-comp-pos 8 9)))
    
    (define (counter vec index ans)
      (if (= index 0) (if (equal? (cons -2 -2) (vector-ref vec 0)) (- ans 1) ans)
          (if (equal? (cons -2 -2) (vector-ref vec index)) (counter vec (- index 1) (- ans 1)) (counter vec (- index 1) ans))))
    
    ;A “2 pieces configuration” refers to having 2 pieces on one line (and the other place empty, so morris possibility).this is an important consideration for starting phase
    (define (no-of-2-pieces-configuration)
      
      (define (2-same-element-1-empty v) (and (equal? (vector-filter (lambda(x) (equal? x "EMPTY")) v) #("EMPTY"))
                                              (or (= (vector-length (vector-filter (lambda(x) (equal? (substring x 0 2) "HW")) v)) 2)
                                                  (= (vector-length (vector-filter (lambda(x) (equal? (substring x 0 2) "CB")) v)) 2))))
      
      (define (no-of-2-pieces-configuration-in-loop)
        (define (helper  x y i j)
          (if (and (= i 2) (= j 8)) (cons x y)
              (if (= j 8) (begin (set! i (+ i 1))
                                 (set! j 0) 
                                 (helper  x y i j))
                  (let ([r (build-vector 3 (lambda(x) (2d-vector-ref board1 i (+ x j))))])
                    (if (2-same-element-1-empty r) (begin (if (equal? (vector-filter (lambda(x) (equal? (substring x 0 2) "CB")) r) #()) (set! x (+ x 1))
                                                              (set! y (+ y 1)))
                                                          (helper  x y i (+ j 2)))
                        (helper  x y i (+ j 2)))))))
        (helper  0 0 0 0))  
      
      (define (no-of-2-pieces-configuration-on-side) 
        (define (helper  x y k)
          (if (= k 9) (cons x y)
              (let ([r (build-vector 3 (lambda(x) (2d-vector-ref board1 x k)))])
                (if (2-same-element-1-empty r) (begin (if (equal? (vector-filter (lambda(x) (equal? (substring x 0 2) "CB")) r) #()) (set! x (+ x 1))
                                                          (set! y (+ y 1)))
                                                      (helper  x y (+ k 2)))
                    (helper  x y (+ k 2))))))
        (helper  0 0 1))
      
      
      (let ([a (no-of-2-pieces-configuration-in-loop)]
            [b (no-of-2-pieces-configuration-on-side)])
        (cons (+ (car a) (car b)) (+ (cdr a) (cdr b)))))
    
    
    ;this function gives three piece configuration 
    (define (no-of-3-pieces-configuration)
      
      (define (3-pieces-configuration-side-wise)
        
        (define (2-same-element-1-empty v) (and (equal? (vector-filter (lambda(x) (equal? x "EMPTY")) v) #("EMPTY"))
                                                (or (= (vector-length (vector-filter (lambda(x) (equal? (substring x 0 2) "HW")) v)) 2)
                                                    (= (vector-length (vector-filter (lambda(x) (equal? (substring x 0 2) "CB")) v)) 2))))
        
        (define (helper x y i j)
          (if (= j 8) (helper x y (+ i 1) 0)
              (if (= i 3) (cons x y)    
                  (let* ([r1 (build-vector 3 (lambda(x) (2d-vector-ref board1 i (modulo (+ x j) 8))))]
                         [r2 (build-vector 3 (lambda(x) (2d-vector-ref board1 i (modulo (+ x 2 j) 8))))]
                         [t (build-vector 5 (lambda(x) (2d-vector-ref board1 i (modulo (+ x j) 8))))]
                         [p1 (2-same-element-1-empty r1)]
                         [p2 (2-same-element-1-empty r2)])
                    (if (and p1 p2) (if (= (vector-length (vector-filter (lambda(x) (equal? (substring x 0 2) "HW")) t)) 3) (helper (+ x 1) y i (+ j 2))
                                        (if (= (vector-length (vector-filter (lambda(x) (equal? (substring x 0 2) "CB")) t)) 3) (helper x (+ y 1) i (+ j 2)) (helper x y i (+ j 2))))
                        (helper x y i (+ j 2)))))))    
        (helper 0 0 0 0))
      
      (define (3-pieces-configuration-cross-wise)
        
        (define (2-same-element-1-empty v) (and (equal? (vector-filter (lambda(x) (equal? x "EMPTY")) v) #("EMPTY"))
                                                (or (= (vector-length (vector-filter (lambda(x) (equal? (substring x 0 2) "HW")) v)) 2)
                                                    (= (vector-length (vector-filter (lambda(x) (equal? (substring x 0 2) "CB")) v)) 2))))
        
        
        (define (helper x y i j)
          (if (= j 8) (helper x y (+ i 1) 0)
              (if (= i 3) (cons x y)    
                  (let* ([r1 (build-vector 3 (lambda(x) (2d-vector-ref board1 i (modulo (+ x j) 8))))]
                         [r2 (build-vector 3 (lambda(x) (2d-vector-ref board1 x (modulo (+ 1 j) 8))))]
                         [t (vector-append r1 r2)]
                         [p1 (2-same-element-1-empty r1)]
                         [p2 (2-same-element-1-empty r2)])
                    (if (and p1 p2 (not (equal? (vector-ref r1 1) "EMPTY"))) (if (= (vector-length (vector-filter (lambda(x) (equal? (substring x 0 2) "HW")) t)) 4) (helper (+ x 1) y i (+ j 2))
                                                                                 (if (= (vector-length (vector-filter (lambda(x) (equal? (substring x 0 2) "CB")) t)) 4) (helper x (+ y 1) i (+ j 2)) (helper x y i (+ j 2))))
                        (helper x y i (+ j 2)))))))    
        (helper 0 0 0 0))
      
      (let ([a (3-pieces-configuration-side-wise)]
            [b (3-pieces-configuration-cross-wise)])
        (cons (+ (car a) (car b)) (+ (cdr a) (cdr b)))))
    
    (define (seedha pair)
      (- (car pair) (cdr pair)))
    
    (define (ulta pair)
      (- (cdr pair) (car pair)))
    
    ; (begin (display closed-morris)
    ;     (display (no-of-mills))
    ;(display (corner-pieces))
    ;(display (intersection-pieces))
    ;(display (side-pieces))
    ;(display (blocked-pieces))
    ;(display (no-of-coins))
    ;(display (no-of-2-pieces-configuration))
    ;    (display (no-of-3-pieces-configuration)))
    
    (if (equal? player `human) (+ (* closed-morris 18) (* 26 (seedha (no-of-mills))) (* 5 (seedha (intersection-pieces))) (* 2 (seedha (side-pieces))) (seedha (blocked-pieces)) (* 6 (seedha (no-of-coins))) (* 12 (seedha (no-of-2-pieces-configuration))) (* 7 (seedha (no-of-3-pieces-configuration))))
        (+ (* closed-morris 18) (* 26 (ulta (no-of-mills))) (* 5 (ulta (intersection-pieces))) (* 2 (ulta (side-pieces))) (ulta (blocked-pieces)) (* 6 (ulta (no-of-coins))) (* 12 (ulta (no-of-2-pieces-configuration))) (* 7 (ulta (no-of-3-pieces-configuration)))))
    )
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  (define (pos-eval-func-phase2 cur-board-status cur-human-pos cur-comp-pos player closed-morris)
    
    ;;extended-vector 3*8 to 3*9
    (define board1 (build-vector 3 (lambda(x) (let ((m (vector-ref cur-board-status x))) (vector-append m  (vector (vector-ref m 0)))))))
    
    ;total number complete mills (cons white black)
    (define (no-of-mills)
      
      (define (all-same-element v) 
        (= (vector-length (vector-filter (lambda(x) (equal? (substring x 0 2) (substring (vector-ref v 0) 0 2))) v)) (vector-length v))) 
      
      (define (no-of-mills-in-loop) 
        (define (helper x y i j)
          (if (and (= i 2) (= j 8)) (cons x y)
              (if (= j 8) (begin (set! i (+ i 1))
                                 (set! j 0) 
                                 (helper x y i j))
                  (let ([r (build-vector 3 (lambda(x) (2d-vector-ref board1 i (+ x j))))])
                    (if (all-same-element r) (begin (if (equal? (substring (vector-ref r 0) 0 2) "HW") (set! x (+ x 1))
                                                        (if (equal? (substring (vector-ref r 0) 0 2) "CB") (set! y (+ y 1))
                                                            (set! x (+ x 0))))
                                                    (helper x y i (+ j 2)))
                        (helper x y i (+ j 2)))))))
        (helper 0 0 0 0))
      
      (define (no-of-mills-on-side) 
        (define (helper  x y k)
          (if (= k 9) (cons x y)
              (let ([r (build-vector 3 (lambda(x) (2d-vector-ref board1 x k)))])
                (if (all-same-element r) (begin (if (equal? (substring (vector-ref r 0) 0 2) "HW") (set! x (+ x 1))
                                                    (if (equal? (substring (vector-ref r 0) 0 2) "CB") (set! y (+ y 1))
                                                        (set! x (+ x 0))))
                                                (helper  x y (+ k 2)))
                    (helper  x y (+ k 2))))))
        (helper 0 0 1))
      
      (let ([a (no-of-mills-on-side)]
            [b (no-of-mills-in-loop)])
        (cons (+ (car a) (car b)) (+ (cdr a) (cdr b)))))
    
    ;returns number of free movement any coin can make
    
    (define (free-movements coin-id cur-pos)
      
      (define extended-board (build-vector 3 (lambda(x) (let ((m (vector-ref cur-board-status x))) (vector-append (vector-drop m 7) m  (vector (vector-ref m 0)))))))
      
      (define (check-surrounding pair)
        (if (< (cdr pair) 0) 0
            (if (= (remainder (cdr pair) 2) 0) (adjacent-vacant pair)
                (+ (adjacent-vacant pair) (upar-niche-vacant pair)))))
      
      (define (adjacent-vacant pair)  ; number of adjacent vacant place    
        (let ([a (substring (2d-vector-ref extended-board (car pair) (cdr pair)) 0 2)]
              [b (substring (2d-vector-ref extended-board (car pair) (+ 2 (cdr pair))) 0 2)]) 
          (if (and (or (equal? a "HW") (equal? a "CB")) (or (equal? b "HW") (equal? b "CB"))) 0
              (if (or (or (equal? a "HW") (equal? a "CB")) (or (equal? b "HW") (equal? b "CB"))) 1 2))))
      
      (define (upar-niche-vacant pair) ; number of upar niche vacant place
        (let ([a (if (= (car pair) 0) "HW" (substring (2d-vector-ref extended-board (- (car pair) 1) (+ 1 (cdr pair))) 0 2))]
              [b (if (= (car pair) 2) "HW" (substring (2d-vector-ref extended-board (+ 1 (car pair)) (+ (cdr pair) 1)) 0 2))])
          (if (and (or (equal? a "HW") (equal? a "CB")) (or (equal? b "HW") (equal? b "CB"))) 0
              (if (or (or (equal? a "HW") (equal? a "CB")) (or (equal? b "HW") (equal? b "CB"))) 1 2))))
      
      
      (if (not (equal? (or (cons -2 -2) (cons -1 -1)) (vector-ref cur-pos (string->number (string (string-ref coin-id 2))))))
          (check-surrounding (vector-ref cur-pos (string->number (string (string-ref coin-id 2)))))
          0))
    
    ;this function gives the number of blocked pieces (cons white black)
    
    (define (blocked-pieces)
      (define i 0)
      (define j 0)
      
      (let ((temp1 (reverse (lc (begin (set! i (add1 i)) i) : x  <- `("HW0" "HW1" "HW2" "HW3" "HW4" "HW5" "HW6" "HW7" "HW8")
                                * (and (= (free-movements x cur-human-pos) 0)(>= (cdr (vector-ref cur-human-pos (string->number (string (string-ref x 2))))) 0)))))
            (temp2 (reverse (lc (begin (set! j (add1 j)) j) : x  <- `("CB0" "CB1" "CB2" "CB3" "CB4" "CB5" "CB6" "CB7" "CB8")
                                * (and (= (free-movements x cur-comp-pos) 0)(>= (cdr (vector-ref cur-comp-pos  (string->number (string (string-ref x 2))))) 0))))))
        
        (cons (if (equal? temp1 `()) 0 (car temp1)) (if (equal? temp2 `()) 0 (car temp2)))))
    
    ;;no. of coins (cons white black)
    (define (no-of-coins)
      (cons (counter cur-human-pos 8 9)
            (counter cur-comp-pos 8 9)))
    
    (define (counter vec index ans)
      (if (= index 0) (if (equal? (cons -2 -2) (vector-ref vec 0)) (- ans 1) ans)
          (if (equal? (cons -2 -2) (vector-ref vec index)) (counter vec (- index 1) (- ans 1)) (counter vec (- index 1) ans))))
    
    
    ;A “2 pieces configuration” refers to having 2 pieces on one line (and the other place empty, so morris possibility).this is an important consideration for starting phase
    (define (no-of-2-pieces-configuration)
      
      (define (2-same-element-1-empty v) (and (equal? (vector-filter (lambda(x) (equal? x "EMPTY")) v) #("EMPTY"))
                                              (or (= (vector-length (vector-filter (lambda(x) (equal? (substring x 0 2) "HW")) v)) 2)
                                                  (= (vector-length (vector-filter (lambda(x) (equal? (substring x 0 2) "CB")) v)) 2))))
      
      (define (no-of-2-pieces-configuration-in-loop)
        (define (helper  x y i j)
          (if (and (= i 2) (= j 8)) (cons x y)
              (if (= j 8) (begin (set! i (+ i 1))
                                 (set! j 0) 
                                 (helper  x y i j))
                  (let ([r (build-vector 3 (lambda(x) (2d-vector-ref board1 i (+ x j))))])
                    (if (2-same-element-1-empty r) (begin (if (equal? (vector-filter (lambda(x) (equal? (substring x 0 2) "CB")) r) #()) (set! x (+ x 1))
                                                              (set! y (+ y 1)))
                                                          (helper  x y i (+ j 2)))
                        (helper  x y i (+ j 2)))))))
        (helper  0 0 0 0))  
      
      (define (no-of-2-pieces-configuration-on-side) 
        (define (helper  x y k)
          (if (= k 9) (cons x y)
              (let ([r (build-vector 3 (lambda(x) (2d-vector-ref board1 x k)))])
                (if (2-same-element-1-empty r) (begin (if (equal? (vector-filter (lambda(x) (equal? (substring x 0 2) "CB")) r) #()) (set! x (+ x 1))
                                                          (set! y (+ y 1)))
                                                      (helper  x y (+ k 2)))
                    (helper  x y (+ k 2))))))
        (helper  0 0 1))
      
      
      (let ([a (no-of-2-pieces-configuration-in-loop)]
            [b (no-of-2-pieces-configuration-on-side)])
        (cons (+ (car a) (car b)) (+ (cdr a) (cdr b)))))
    
    ;;
    (define noofcoins (no-of-coins))
    (define blockedpieces (blocked-pieces))
    ;;
    (define alivepieces
      (cons (foldr + 0 (map (lambda (x) (if (equal? x (cons -2 -2)) 0 1)) (vector->list cur-human-pos)))
            (foldr + 0 (map (lambda (x) (if (equal? x (cons -2 -2)) 0 1)) (vector->list cur-comp-pos)))))
    
    
    ;;Winning Condition
    (define winning-configuration
      (- (if (or (= 2 (cdr noofcoins)) (= (cdr alivepieces) (cdr blockedpieces))) 1 0)
         (if (or (= 2 (car noofcoins)) (= (car alivepieces) (car blockedpieces))) 1 0)))
    
    
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;< >;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;returns the number of double morris (cons white black)
    (define (double-morris)
      
      
      (define (2-same-element-1-empty v) (and (equal? (vector-filter (lambda(x) (equal? x "EMPTY")) v) #("EMPTY"))
                                              (or (= (vector-length (vector-filter (lambda(x) (equal? (substring x 0 2) "HW")) v)) 2)
                                                  (= (vector-length (vector-filter (lambda(x) (equal? (substring x 0 2) "CB")) v)) 2))))
      
      (define (all-same-element v) 
        (= (length (filter (lambda(x) (equal? (substring x 0 2) (substring (list-ref (vector->list v) 0) 0 2))) (vector->list v))) (length (vector->list v)))) 
      
      
      
      (define (double-morris-type1)
        (define (helper x y i j)
          (if (= j 8) (cons x y)
              (if (= i 2) (helper x y 0 (+ j 2))    
                  (let* ([brd board1]
                         [r1 (build-vector 3 (lambda(x) (2d-vector-ref brd i (modulo (+ x j) 8))))]
                         [r2 (build-vector 3 (lambda(x) (2d-vector-ref brd(+ 1 i) (modulo (+ x j) 8))))]
                         [p1 (and (2-same-element-1-empty r1) (equal? (vector-ref r1 1) "EMPTY"))]
                         [p2 (and (2-same-element-1-empty r2) (equal? (vector-ref r2 1) "EMPTY"))]
                         [q1 (all-same-element r1)]
                         [q2 (all-same-element r2)]
                         [t (vector-append r1 r2)])
                    (if (or (and p1 q2) (and p2 q1)) (if (= (vector-length (vector-filter (lambda(x) (equal? (substring x 0 2) "HW")) t)) 5) (helper (+ x 1) y (+ 1 i) j)
                                                         (if (= (vector-length (vector-filter (lambda(x) (equal? (substring x 0 2) "CB")) t)) 5) (helper x (+ y 1) (+ 1 i) j) (helper x y (+ i 1) j)))
                        (helper x y (+ 1 i) j))))))
        (helper 0 0 0 0))
      
      (define (double-morris-type2)
        (define (helper x y i j)
          (if (= j 9) (cons x y)
              (if (= i 3) (helper x y 0 (+ j 2))
                  (let* ([brd board1]
                         [main-vec (build-vector 3 (lambda(x) (2d-vector-ref brd x j)))]
                         [r1 (build-vector 3 (lambda(x) (2d-vector-ref brd i (modulo (- (- j 1) x) 8))))]
                         [r2 (build-vector 3 (lambda(x) (2d-vector-ref brd i (modulo (+ (+ j 1) x) 8))))]
                         [p (and (2-same-element-1-empty main-vec) (equal? (vector-ref main-vec i) "EMPTY"))]
                         [p1 (and (2-same-element-1-empty r1) (equal? (vector-ref r1 0) "EMPTY"))]
                         [p2 (and (2-same-element-1-empty r2) (equal? (vector-ref r2 0) "EMPTY"))]
                         [q (all-same-element main-vec)]
                         [q1 (all-same-element r1)]
                         [q2 (all-same-element r2)]     
                         [t1 (vector-append main-vec r1)]
                         [t2 (vector-append main-vec r2)]
                         [hwt1 (= (vector-length (vector-filter (lambda(x) (equal? (substring x 0 2) "HW")) t1)) 5)]
                         [cbt1 (= (vector-length (vector-filter (lambda(x) (equal? (substring x 0 2) "CB")) t1)) 5)]
                         [hwt2 (= (vector-length (vector-filter (lambda(x) (equal? (substring x 0 2) "HW")) t2)) 5)]
                         [cbt2 (= (vector-length (vector-filter (lambda(x) (equal? (substring x 0 2) "CB")) t2)) 5)])
                    (if (and (or (and p q1) (and p1 q)) (or (and p q2) (and p2 q))) (cond [(and hwt1 hwt2) (helper (+ x 2) y (+ 1 i) j)]
                                                                                          [(or (and cbt1 hwt2) (and hwt1 cbt2)) (helper (+ x 1) (+ y 1) (+ 1 i) j)]
                                                                                          [(and cbt1 cbt2) (helper x (+ y 2) (+ 1 i) j)]
                                                                                          [(or hwt1 hwt2) (helper (+ x 1) y (+ 1 i) j)]
                                                                                          [(or cbt1 cbt2) (helper x (+ 1 y) (+ 1 i) j)]
                                                                                          [else (helper x y (+ 1 i) j)])
                        (if (or (and p q1) (and p1 q)) (cond [hwt1 (helper (+ x 1) y (+ 1 i) j)]
                                                             [cbt1 (helper x (+ 1 y) (+ 1 i) j)]
                                                             [else (helper x y (+ 1 i) j)])   
                            (if (or (and p q2) (and p2 q)) (cond [hwt2 (helper (+ x 1) y (+ 1 i) j)]
                                                                 [cbt2 (helper x (+ 1 y) (+ 1 i) j)]
                                                                 [else (helper x y (+ 1 i) j)])
                                (helper x y (+ 1 i) j))))))))
        (helper 0 0 0 1))
      
      
      (let ([a (double-morris-type1)]
            [b (double-morris-type2)])
        (cons (+ (car a) (car b)) (+ (cdr a) (cdr b)))))
    
    (define (seedha pair)
      (- (car pair) (cdr pair)))
    
    (define (ulta pair)
      (- (cdr pair) (car pair)))
    
    ;(begin 
    ; (display closed-morris)
    ;         (display (no-of-mills))
    ;         (display blockedpieces)
    ;         (display alivepieces)
    ;         (display noofcoins)
    ;         (display (no-of-2-pieces-configuration))
    ;         (display winning-configuration))
    ; (display (double-morris))
    
    (if (equal? player `human) (+ (* closed-morris 14) (* 43 (seedha (no-of-mills))) (* 10 (seedha blockedpieces)) (* 8 (seedha noofcoins))  (* 1086 winning-configuration) (* 6 (seedha (no-of-2-pieces-configuration))) (* 42 (seedha (double-morris))))
        (+ (* closed-morris 14) (* 43 (ulta (no-of-mills))) (* 10 (ulta blockedpieces)) (* 8 (ulta noofcoins)) (* 1086 winning-configuration ) (* 6 (ulta (no-of-2-pieces-configuration))) (* 42 (ulta (double-morris)))))
    )
  
  
  
  
  
  
  
  
  (define (make-vec vec)
    (define v (make-2d-vector 3 8 #f))
    
    (define (helper i j)
      (cond [(= i 3) v]
            [(= j 7) (begin (2d-vector-set! v i j (2d-vector-ref vec i j))
                            (helper (+ i 1) 0))]
            [else (begin (2d-vector-set! v i j (2d-vector-ref vec i j))
                         (helper i (+ j 1)))]))
    (helper 0 0))
  
  
  (define (make-vec1 vec)
    (define v (make-vector 9 #f))
    
    (define (helper i)
      (if (= i 9) v
          (begin (vector-set! v i (vector-ref vec i))
                 (helper (+ i 1)))))
    (helper 0))
  
  
  
  (define (possible-board-generators1 cur-board-status cur-pos player mycount1)
    (let ((temp-string (string-append (if (equal? player `computer) "CB" "HW") (string (vector-ref reference mycount1)))))
      (map (lambda (x) (let ((trial-vec (make-vec cur-board-status))
                             (trial-vec1 (make-vec1 cur-pos)))
                         (begin (2d-vector-set! trial-vec (car x) (cdr x) temp-string)
                                (vector-set! trial-vec1 mycount1 x)
                                (list trial-vec trial-vec1 (checking-for-mill x player trial-vec) x)))) (lc x : x <- helper-list1 *(equal? "EMPTY" (2d-vector-ref cur-board-status (car x) (cdr x)))))))
  
  ;(trace possible-board-generators1)
  
  
  (define (possible-board-removers1 cur-board-status cur-pos player)
    (filter (lambda (x) (not (equal? x `()))) (map (lambda (x) (if (or (equal? x (cons -2 -2)) (equal? x (cons -1 -1)) (checking-for-mill x player cur-board-status)) 
                                                                   `() 
                                                                   (let ((trial-vec (make-vec cur-board-status))
                                                                         (trial-vec1 (make-vec1 cur-pos))) 
                                                                     (begin (2d-vector-set! trial-vec (car x) (cdr x) "EMPTY")
                                                                            (vector-set! trial-vec1 (string->number (string (string-ref (2d-vector-ref cur-board-status (car x) (cdr x)) 2))) (cons -2 -2))
                                                                            (list trial-vec trial-vec1 x))))) (vector->list cur-pos))))
  ;(trace possible-board-removers1)
  
  
  (define (possible-moves-generator cur-board-status cur-pos)
    (concat (lc (lc (append (list (car z)) (list w)) : w <- (cdr z) * (equal? "EMPTY" (2d-vector-ref cur-board-status (car w) (cdr w)))) : 
                z <- (map (lambda (x) (car x)) (lc (lc y : y <- helper-list *(equal? x (car y))) : x <- (vector->list cur-pos) * (not (or (equal? x (cons -1 -1)) (equal? x (cons -2 -2)))))))))
  
  (define (possible-board-generators cur-board-status cur-pos lst-pos-moves)
    (map (lambda (x) (let ((trial-vec (make-vec cur-board-status))
                           (trial-vec1 (make-vec1 cur-pos))) 
                       (begin (2d-vector-set! trial-vec (caar x) (cdar x) "EMPTY")
                              (2d-vector-set! trial-vec (caadr x) (cdadr x) (2d-vector-ref cur-board-status (caar x) (cdar x)))
                              (set! trial-vec1 (list->vector (map (lambda (y) (if (equal? y (car x)) (cadr x) y)) (vector->list cur-pos))))
                              (list trial-vec trial-vec1 (checking-for-mill (cons (caadr x) (cdadr x)) (if (equal? (string-ref (2d-vector-ref cur-board-status (caar x) (cdar x)) 0) #\C) `computer `human) trial-vec) x)))) lst-pos-moves))
  
  ;(trace possible-board-generators)
  
  
  (define (possible-board-removers cur-board-status cur-pos player)
    (filter (lambda (x) (not (equal? x `()))) (map (lambda (x) (if (or (equal? x (cons -2 -2)) (equal? x (cons -1 -1)) (checking-for-mill x player cur-board-status)) 
                                                                   `() 
                                                                   (let ((trial-vec (make-vec cur-board-status))
                                                                         (trial-vec1 (make-vec1 cur-pos))) 
                                                                     (begin (2d-vector-set! trial-vec (car x) (cdr x) "EMPTY")
                                                                            ;(display cur-board-status)
                                                                            ;(display trial-vec)
                                                                            ;(display x)
                                                                            ;(display cur-pos)
                                                                            (vector-set! trial-vec1 (string->number (string (string-ref (2d-vector-ref cur-board-status (car x) (cdr x)) 2))) (cons -2 -2))
                                                                            ;(display trial-vec1)
                                                                            (list trial-vec trial-vec1 x))))) (vector->list cur-pos))))
  
  ;(trace possible-board-removers)
  
  (define (best-move-generator)
    
    (define (helper1 cur-board-status cur-human-pos cur-comp-pos player i pred closed-morris mycount1)
      
      
      ;;no. of coins (cons white black)
      (define (no-of-coins)
        (cons (counter cur-human-pos 8 9)
              (counter cur-comp-pos 8 9)))
      
      (define (counter vec index ans)
        (if (= index 0) (if (equal? (cons -2 -2) (vector-ref vec 0)) (- ans 1) ans)
            (if (equal? (cons -2 -2) (vector-ref vec index)) (counter vec (- index 1) (- ans 1)) (counter vec (- index 1) ans))))
      
      
      (if (= mycount1 9) (helper2 cur-board-status cur-human-pos cur-comp-pos player 3 pred closed-morris)
          
          (if (equal? pred #t) 
              (if (equal? player `computer) 
                  (let ((lst (possible-board-removers1 cur-board-status cur-human-pos `human))) 
                    (if (equal? `() lst) (helper1 cur-board-status cur-human-pos cur-comp-pos `human (- i 1) #f closed-morris (+ mycount1 1))
                        (if (= i 2) (min3 (map (lambda (x) (cons (list-ref x 2) (helper1 (list-ref x 0) (list-ref x 1) cur-comp-pos `human (- i 1) #f #t (+ mycount1 1)))) lst))
                            (if (odd? i) (max2 (map (lambda (x) (helper1 (list-ref x 0) (list-ref x 1) cur-comp-pos `human (- i 1) #f closed-morris (+ mycount1 1))) lst))
                                (min2 (map (lambda (x) (helper1 (list-ref x 0) (list-ref x 1) cur-comp-pos `human (- i 1) #f closed-morris (+ mycount1 1))) lst))))))
                  
                  (let ((lst (possible-board-removers1 cur-board-status cur-comp-pos `computer))) 
                    (if (equal? `() lst) (helper1 cur-board-status cur-human-pos cur-comp-pos `computer (- i 1) #f closed-morris mycount1)
                        (if (= i 2) (min3 (map (lambda (x) (helper1 (list-ref x 0) cur-human-pos (list-ref x 1) `computer (- i 1) #f #t mycount1)) lst))
                            (if (odd? i) (max2 (map (lambda (x) (helper1 (list-ref x 0) cur-human-pos (list-ref x 1) `computer (- i 1) #f closed-morris mycount1)) lst))
                                (min2 (map (lambda (x) (helper1 (list-ref x 0) cur-human-pos (list-ref x 1) `computer (- i 1) #f closed-morris mycount1)) lst)))))))      
              
              
              (if (equal? player `computer)
                  
                  (cond [(= i 2) (let* ((lst (possible-board-generators1 cur-board-status cur-comp-pos `computer mycount1))
                                        (tempo (map (lambda (x) (if (list-ref x 2) (helper1 (list-ref x 0) cur-human-pos (list-ref x 1) `computer i #t #t mycount1)
                                                                    (helper1 (list-ref x 0) cur-human-pos (list-ref x 1) `human (- i 1) #f closed-morris (+ mycount1 1)))) lst))
                                        (tempo1 (min4 tempo)))
                                   (if (number? (list-ref tempo tempo1)) (list-ref (list-ref lst tempo1) 3) (list (list-ref (list-ref lst tempo1) 3) (car (list-ref tempo tempo1)))))]
                        
                        [(= i 0) (if (equal? closed-morris #t) (pos-eval-func-phase1 cur-board-status cur-human-pos cur-comp-pos `human 1)
                                     (pos-eval-func-phase1 cur-board-status cur-human-pos cur-comp-pos `human 0))]
                        
                        [else (let ((lst (possible-board-generators1 cur-board-status cur-comp-pos `computer mycount1)))
                                (if (or (equal? `() lst) (equal? 2 (cdr (no-of-coins)))) (if (odd? i) -1500 1500) 
                                    (if (odd? i) (max2 (map (lambda (x) (if (list-ref x 2) (helper1 (list-ref x 0) cur-human-pos (list-ref x 1) `computer i #t closed-morris mycount1)
                                                                            (helper1 (list-ref x 0) cur-human-pos (list-ref x 1) `human (- i 1) #f closed-morris (+ mycount1 1)))) lst))
                                        (min2 (map (lambda (x) (if (list-ref x 2) (helper1 (list-ref x 0) cur-human-pos (list-ref x 1) `computer i #t closed-morris mycount1)
                                                                   (helper1 (list-ref x 0) cur-human-pos (list-ref x 1) `human (- i 1) #f closed-morris (+ mycount1 1)))) lst)))))])
                  
                  
                  (cond [(= i 2) (let* ((lst (possible-board-generators1 cur-board-status cur-human-pos `human mycount1))
                                        (tempo (map (lambda (x) (if (list-ref x 2) (helper1 (list-ref x 0) (list-ref x 1) cur-comp-pos `human i #t #t mycount1)
                                                                    (helper1 (list-ref x 0) (list-ref x 1) cur-comp-pos `computer (- i 1) #f closed-morris mycount1))) lst))
                                        (tempo1 (min4 tempo)))
                                   
                                   (if (number? (list-ref tempo tempo1)) (list-ref lst tempo1) (cons (car (list-ref tempo tempo1)) (cdr (list-ref lst tempo1)))))]
                        
                        [(= i 0) (if (equal? closed-morris #t) (pos-eval-func-phase1 cur-board-status  cur-human-pos cur-comp-pos `computer 1)
                                     (pos-eval-func-phase1 cur-board-status  cur-human-pos cur-comp-pos `computer 0))]
                        
                        [else (let ((lst (possible-board-generators1 cur-board-status cur-human-pos `human mycount1)))
                                (if (or (equal? `() lst) (equal? 2 (car (no-of-coins)))) (if (odd? i) -1500 1500) 
                                    (if (odd? i) (max2 (map (lambda (x) (if (list-ref x 2) (helper1 (list-ref x 0) (list-ref x 1) cur-comp-pos `human i #t closed-morris mycount1)
                                                                            (helper1 (list-ref x 0) (list-ref x 1) cur-comp-pos `computer (- i 1) #f closed-morris mycount1))) lst))
                                        (min2 (map (lambda (x) (if (list-ref x 2) (helper1 (list-ref x 0) (list-ref x 1) cur-comp-pos `human i #t closed-morris mycount1)
                                                                   (helper1 (list-ref x 0) (list-ref x 1) cur-comp-pos `computer (- i 1) #f closed-morris mycount1))) lst)))))])))))
    
    
    
    
    (define (helper2 cur-board-status cur-human-pos cur-comp-pos player i pred closed-morris)
      
      ;;no. of coins (cons white black)
      (define (no-of-coins)
        (cons (counter cur-human-pos 8 9)
              (counter cur-comp-pos 8 9)))
      
      (define (counter vec index ans)
        (if (= index 0) (if (equal? (cons -2 -2) (vector-ref vec 0)) (- ans 1) ans)
            (if (equal? (cons -2 -2) (vector-ref vec index)) (counter vec (- index 1) (- ans 1)) (counter vec (- index 1) ans))))
      
      (if (equal? pred #t) 
          (if (equal? player `computer) 
              (let ((lst (possible-board-removers cur-board-status cur-human-pos `human))) 
                (if (equal? `() lst) (helper2 cur-board-status cur-human-pos cur-comp-pos `human (- i 1) #f closed-morris) 
                    (if (= i 4) (min3 (map (lambda (x) (cons (list-ref x 2) (helper2 (list-ref x 0) (list-ref x 1) cur-comp-pos `human (- i 1) #f #t))) lst))
                        (if (odd? i) (max2 (map (lambda (x) (helper2 (list-ref x 0) (list-ref x 1) cur-comp-pos `human (- i 1) #f closed-morris)) lst))
                            (min2 (map (lambda (x) (helper2 (list-ref x 0) (list-ref x 1) cur-comp-pos `human (- i 1) #f closed-morris)) lst))))))
              
              (let ((lst (possible-board-removers cur-board-status cur-comp-pos `computer))) 
                (if (equal? `() lst) (helper2 cur-board-status cur-human-pos cur-comp-pos `computer (- i 1) #f closed-morris)
                    (if (= i 4) (min3 (map (lambda (x) (helper2 (list-ref x 0) cur-human-pos (list-ref x 1) `computer (- i 1) #f #t)) lst))
                        (if (odd? i) (max2 (map (lambda (x) (helper2 (list-ref x 0) cur-human-pos (list-ref x 1) `computer (- i 1) #f closed-morris)) lst))
                            (min2 (map (lambda (x) (helper2 (list-ref x 0) cur-human-pos (list-ref x 1) `computer (- i 1) #f closed-morris)) lst)))))))      
          
          
          (if (equal? player `computer)
              
              (cond [(= i 4) (let* ((lst (possible-board-generators cur-board-status cur-comp-pos
                                                                    (possible-moves-generator cur-board-status cur-comp-pos)))
                                    (tempo (map (lambda (x) (if (list-ref x 2) (helper2 (list-ref x 0) cur-human-pos (list-ref x 1) `computer i #t #t)
                                                                (helper2 (list-ref x 0) cur-human-pos (list-ref x 1) `human (- i 1) #f closed-morris))) lst))
                                    (tempo1 (min4 tempo)))
                               (if (number? (list-ref tempo tempo1)) (list-ref (list-ref lst tempo1) 3) (list (list-ref (list-ref lst tempo1) 3) (car (list-ref tempo tempo1)))))]
                    
                    [(= i 0) (if (equal? closed-morris #t) (pos-eval-func-phase2 cur-board-status cur-human-pos cur-comp-pos `human 1)
                                 (pos-eval-func-phase2 cur-board-status cur-human-pos cur-comp-pos `human 0))]
                    
                    [else (let ((lst (possible-board-generators cur-board-status cur-comp-pos
                                                                (possible-moves-generator cur-board-status cur-comp-pos))))
                            (if (or (equal? `() lst) (equal? 2 (cdr (no-of-coins)))) (if (odd? i) -1500 1500) 
                                (if (odd? i) (max2 (map (lambda (x) (if (list-ref x 2) (helper2 (list-ref x 0) cur-human-pos (list-ref x 1) `computer i #t closed-morris)
                                                                        (helper2 (list-ref x 0) cur-human-pos (list-ref x 1) `human (- i 1) #f closed-morris))) lst))
                                    (min2 (map (lambda (x) (if (list-ref x 2) (helper2 (list-ref x 0) cur-human-pos (list-ref x 1) `computer i #t closed-morris)
                                                               (helper2 (list-ref x 0) cur-human-pos (list-ref x 1) `human (- i 1) #f closed-morris))) lst)))))])
              
              
              (cond [(= i 4) (let* ((lst (possible-board-generators cur-board-status cur-human-pos
                                                                    (possible-moves-generator cur-board-status cur-human-pos)))
                                    (tempo (map (lambda (x) (if (list-ref x 2) (helper2 (list-ref x 0) (list-ref x 1) cur-comp-pos `human i #t closed-morris)
                                                                (helper2 (list-ref x 0) (list-ref x 1) cur-comp-pos `computer (- i 1) #f closed-morris))) lst))
                                    (tempo1 (min4 tempo)))
                               
                               (if (number? (list-ref tempo tempo1)) (list-ref lst tempo1) (cons (car (list-ref tempo tempo1)) (cdr (list-ref lst tempo1)))))]
                    
                    [(= i 0) (if (equal? closed-morris #t) (pos-eval-func-phase2 cur-board-status  cur-human-pos cur-comp-pos `computer 1)
                                 (pos-eval-func-phase2 cur-board-status  cur-human-pos cur-comp-pos `computer 0))]
                    
                    [else (let ((lst (possible-board-generators cur-board-status cur-human-pos
                                                                (possible-moves-generator cur-board-status cur-human-pos))))
                            (if (or (equal? `() lst) (equal? 2 (car (no-of-coins)))) (if (odd? i) -1500 1500) 
                                (if (odd? i) (max2 (map (lambda (x) (if (list-ref x 2) (helper2 (list-ref x 0) (list-ref x 1) cur-comp-pos `human i #t closed-morris)
                                                                        (helper2 (list-ref x 0) (list-ref x 1) cur-comp-pos `computer (- i 1) #f closed-morris))) lst))
                                    (min2 (map (lambda (x) (if (list-ref x 2) (helper2 (list-ref x 0) (list-ref x 1) cur-comp-pos `human i #t closed-morris)
                                                               (helper2 (list-ref x 0) (list-ref x 1) cur-comp-pos `computer (- i 1) #f closed-morris))) lst)))))]))))
    
    
    (if (< mycount 9)  
        ;(trace helper1)
        (helper1 board-status human-white-pos computer-black-pos `computer 2 #f #f mycount)
        
        ;(trace helper2)
        (helper2 board-status human-white-pos computer-black-pos `computer 4 #f #f))
    )
  
  (define (max1 lst)
    (let ((temp (max2 lst)))
      (max-helper temp lst 0)))
  
  (define (min1 lst)
    (let ((temp (min2 lst)))
      (max-helper temp lst 0)))
  
  (define (max-helper mem lst index)
    (if (= mem (car lst)) index
        (max-helper mem (cdr lst) (+ index 1))))
  
  (define (max2 lst)
    (if (equal? `() (cdr lst)) (car lst)
        (max (car lst) (max2 (cdr lst)))))
  
  (define (min2 lst)
    (if (equal? `() (cdr lst)) (car lst)
        (min (car lst) (min2 (cdr lst)))))
  
  (define (min3 lst)
    (let ((temp (map (lambda (x) (cdr x)) lst)))
      (list-ref lst (min1 temp))))
  
  (define (min4 lst)
    (let ((temp (map (lambda (x) (if (number? x) x (cdr x))) lst)))
      (min1 temp)))
  
  
  ;(trace pos-eval-func-phase2)
  ;(trace max1)
  ;(trace max2)
  
;  (define (you-lost)
;    
;    (define windw (open-viewport "LOSER!!" 400 594))
;    
;    ((draw-pixmap windw) "lose1.png" (make-posn 0 0))
;    ((draw-pixmap windw) "click.jpg" (make-posn 400 0))
;    )
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;< >;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;<END OF BEST MOVE GENERATOR>;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (begin
    (open-graphics)
    
    (define windw (open-viewport "Nine Men's Morris" 600 600))
    ((draw-pixmap windw) "morris.gif" (make-posn 0 0))
    
    (outhelper board-status 0 0)
    
    (placing-position-checking (main2 board-status 1)))
  
  
  )

(define (you-lost)
  
  (open-graphics)
  (define windw (open-viewport "LOSER!!" 400 768))
  
  ((draw-pixmap windw) "lose-2.png" (make-posn 0 0))
  ((draw-pixmap windw) "click.png" (make-posn 0 400))
  
  (define (helper)
    
    (define mouse-click-pos 0)
    (define new-mouse-click-pos 0)
    
    (begin 
      (set! mouse-click-pos (mouse-click-posn (get-mouse-click windw)))
      (if (> (posn-y mouse-click-pos) 400)
          (begin 
            ((draw-pixmap windw) "lose-1.png" (make-posn 0 0))
            ((draw-pixmap windw) "exit2.jpg" (make-posn 0 400))
            
            (set! new-mouse-click-pos (mouse-click-posn (get-mouse-click windw)))
            (if (> (posn-y mouse-click-pos) 400)
                (begin
                  (close-viewport windw)
                  (close-graphics)
                  )
                (helper)))
          (you-lost))))
  (helper))

(define (you-win)
  
  (open-graphics)
  (define windw (open-viewport "WINNER!" 400 768))
  
  ((draw-pixmap windw) "you-win.jpg" (make-posn 0 0))
  ((draw-pixmap windw) "click2.jpg" (make-posn 0 400))
  
  (define (helper)
    
    (define mouse-click-pos 0)
    (define new-mouse-click-pos 0)
    
    (begin 
      (set! mouse-click-pos (mouse-click-posn (get-mouse-click windw)))
      (if (> (posn-y mouse-click-pos) 400)
          (begin 
            ((draw-pixmap windw) "i-will-be-back.jpg" (make-posn 0 0))
            ((draw-pixmap windw) "hasta.jpg" (make-posn 0 300))
            
            (set! new-mouse-click-pos (mouse-click-posn (get-mouse-click windw)))
            (if (> (posn-y mouse-click-pos) 300)
                (begin
                  (close-viewport windw)
                  (close-graphics)
                  )
                (helper)))
          (you-win))))
  (helper))
