#lang typed/racket

(require (only-in typed/racket/gui/base put-file get-file))
(require typed/test-engine/racket-tests)

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")

;; data definitions

(define-struct (Some a)
  ([value : a]))

(define-type (Optional a)
  (U 'None (Some a)))

(define-type Stone
  (U 'black 'white))

(define-struct LogicalLoc
  ([col : Integer]
   [row : Integer]))

(define-type Board
  (Vectorof (Vectorof (Optional Stone))))

(define-struct Go
  ([board : Board]
   [next-to-play : Stone]
   [history : (Listof Board)]
   [last-turn-place : (Optional LogicalLoc)]
   [last-turn-opp-captures : (Listof LogicalLoc)]
   [last-turn-self-captures : (Listof LogicalLoc)]
   [consecutive-passes : Integer]))

(define-struct PhysicalLoc
  ([x-offset-from-left : Integer]
   [y-offset-from-top  : Integer]))

(define-struct BoardSpec
  ([background-color : Image-Color]
   [cell-size-pixels : Integer]
   [margin-pixels : Integer]
   [stone-radius-pixels : Integer]))

(define-struct World
  ([spec : BoardSpec]
   [game : Go]
   [status-message : String]
   [black-tenths : Integer]
   [white-tenths : Integer]
   [hover : (Optional LogicalLoc)]))

(define-struct Outcome
  ([black  : Integer]
   [white  : Integer]
   [winner : (U Stone 'draw)]))

;; ======

;; convert logical location to physical
;; - raise error if logical loc is off the board
(: logical->physical : LogicalLoc Integer BoardSpec -> PhysicalLoc)
(define (logical->physical lloc dim spec)
  (match lloc
    [(LogicalLoc col row)
     (if (or (< col 0) (< row 0) (>= col dim) (>= row dim))
         (error "off the board")
         (match spec
           [(BoardSpec _ cell margin _)
            (PhysicalLoc (+ margin (* cell col))
                         (+ margin (* cell (- dim row 1))))]))]))

(check-expect
 (logical->physical (LogicalLoc 0 0) 5 (BoardSpec 'tan 10 10 4))
 (PhysicalLoc 10 50))

(check-expect
 (logical->physical (LogicalLoc 0 1) 5 (BoardSpec 'tan 10 10 4))
 (PhysicalLoc 10 40))

(check-expect
 (logical->physical (LogicalLoc 1 1) 5 (BoardSpec 'tan 10 10 4))
 (PhysicalLoc 20 40))

(check-expect
 (logical->physical (LogicalLoc 4 4) 5 (BoardSpec 'tan 10 10 4))
 (PhysicalLoc 50 10))

;; Euclidean distance formula
(: distance : Integer Integer Integer Integer -> Real)
(define (distance x0 y0 x1 y1)
  (local {(define dx (- x0 x1))
          (define dy (- y0 y1))}
    (sqrt (+ (* dx dx) (* dy dy)))))

(check-expect (distance 0 0 10 0) 10)
(check-expect (distance 3 0 0 4) 5)

;; snap-to-grid moves a number on a number line to its nearest "grid line."
;; That is, it assumes something like this, with "m...m" meaning margin:
;; mmmmmG----G----G----G
;; The unknown is rounded to the nearest G.
;; The arguments to this function are as follows: 
;; - the first argument is margin size
;; - the second argument is the size of the gap between grid lines
;; - the third argument is the unknown that needs to be mapped
;; Negatives should not arise in practice, but they are mapped to the leftmost
;; G. There is no right end to the number line -- it extends forever.
(: snap-to-grid : Integer Integer Real -> Integer)
(define (snap-to-grid margin gap-size x)
  (if (>= x 0)
      (+ margin (* gap-size (exact-round (/ (- x margin) gap-size))))
      margin))

(check-expect (snap-to-grid 5 1 4.9) 5)
(check-expect (snap-to-grid 5 1 5.1) 5)
(check-expect (snap-to-grid 5 1 5.6) 6)
(check-expect (snap-to-grid 5 1 5.9) 6)
(check-expect (snap-to-grid 5 1 9.9) 10)
(check-expect (snap-to-grid 5 5 4.9) 5)
(check-expect (snap-to-grid 5 5 5.9) 5)
(check-expect (snap-to-grid 5 5 9.9) 10)
(check-expect (snap-to-grid 5 5 -99999) 5)

;; on-the-board? checks if a logical location is on the board, given dimension
(: on-the-board? : Integer LogicalLoc -> Boolean)
(define (on-the-board? dim lloc)
  (match lloc
    [(LogicalLoc x y) (and (<= 0 x (sub1 dim)) (<= 0 y (sub1 dim)))]))

;; convert physical location to logical, if within stone radius of a stone
;; location
(: physical->logical : PhysicalLoc Integer BoardSpec -> (Optional LogicalLoc))
(define (physical->logical ploc dim spec)
  (match ploc
    [(PhysicalLoc x y)
     (match spec
       [(BoardSpec _ cell margin stone)
        (local
          {(define nearx (snap-to-grid margin cell x))
           (define neary (snap-to-grid margin cell y))
           (define lloc-candidate
             (LogicalLoc (quotient (- nearx margin) cell)
                         (quotient (- (* (sub1 dim) cell) (- neary margin))
                                   cell)))}
          (if (and (on-the-board? dim lloc-candidate)
                   (<= (distance x y nearx neary) stone))
              (Some lloc-candidate)
              'None))])]))

(check-expect
 (physical->logical (PhysicalLoc 10 50) 5 (BoardSpec 'tan 10 10 4))
 (Some (LogicalLoc 0 0)))

(check-expect
 (physical->logical (PhysicalLoc 20 50) 5 (BoardSpec 'tan 10 10 4))
 (Some (LogicalLoc 1 0)))

(check-expect
 (physical->logical (PhysicalLoc 20 40) 5 (BoardSpec 'tan 10 10 4))
 (Some (LogicalLoc 1 1)))
        
(check-expect
 (physical->logical (PhysicalLoc 50 10) 5 (BoardSpec 'tan 10 10 4))
 (Some (LogicalLoc 4 4)))

;; ======

;; The alphabet without the letter I.
(: alphabet-no-i (Listof Char))
(define alphabet-no-i
  (string->list "ABCDEFGHJKLMNOPQRSTUVWXYZ"))

;; convert the column index to a string label
;; 0 => "A", ..., 24 => "Z", 25 => "AA", ...
(: column->string : Integer -> String)
(define (column->string n)
  (make-string (add1 (quotient n 25))
               (list-ref alphabet-no-i (remainder n 25))))

(check-expect (column->string 0) "A")
(check-expect (column->string 24) "Z")
(check-expect (column->string 25) "AA")
(check-expect (column->string 26) "BB")

;; produce a string label for a logical location
;; ex: (logical->string (LogicalLoc 0 0)) => "A1"
(: logical->string : LogicalLoc -> String)
(define (logical->string lloc)
  (match lloc
    [(LogicalLoc col row)
     (string-append (column->string col) (number->string (add1 row)))]))

(check-expect (logical->string (LogicalLoc 0 0))  "A1")
(check-expect (logical->string (LogicalLoc 1 0))  "B1")
(check-expect (logical->string (LogicalLoc 0 1))  "A2")
(check-expect (logical->string (LogicalLoc 25 0)) "AA1")

;; return either Some stone or None by inspecting the board
(: board-ref : Go LogicalLoc -> (Optional Stone))
(define (board-ref g lloc)
  (match* (g lloc)
    [((Go gob _ _ _ _ _ _) (LogicalLoc lx ly))
     (vector-ref (vector-ref gob lx) ly)]))

(check-expect
 (board-ref
  (Go
   (vector
    (vector 'None 'None (Some 'white) 'None)
    (vector 'None (Some 'black) (Some 'black) 'None)
    (vector 'None (Some 'white) 'None 'None)
    (vector 'None 'None 'None 'None))
   'black
   (list) 'None '() '() 0)
  (LogicalLoc 1 1))
 (Some 'black))

;; flip to the next player
(: adv : Stone -> Stone)
(define (adv s)
  (match s
    ['white 'black]
    ['black 'white]))

;; sets the location of a stone on the board
(: board-set! : Go LogicalLoc (Optional Stone) -> Void)
(define (board-set! g lloc stone)
  (match* (g lloc)
    [((Go gob _ _ _ _ _ _) (LogicalLoc lx ly))
     (vector-set! (vector-ref gob lx) ly stone)]))

;; test of board-set!
(define test-go
  (Go
   (vector
    (vector 'None 'None (Some 'white) 'None)
    (vector 'None (Some 'black) (Some 'black) 'None)
    (vector 'None (Some 'white) 'None 'None)
    (vector 'None 'None 'None 'None))
   'black
   (list) 'None '() '() 0))

(board-set! test-go (LogicalLoc 0 1) (Some 'black))

(check-expect
 (board-ref test-go (LogicalLoc 0 1))
 (Some 'black))

;; decides if two vectors are equal
(: vector=? : (Vectorof (Optional Stone))
   (Vectorof (Optional Stone)) -> Boolean)
(define (vector=? v1 v2)
  (local
    {(: helper : (Vectorof (Optional Stone))
        (Vectorof (Optional Stone)) Integer -> Boolean)
     (define (helper ve1 ve2 index)
       (if (= index (vector-length ve1))
           #t
           (match* ((vector-ref ve1 index) (vector-ref ve2 index))
             [('None (Some _)) #f]
             [((Some _) 'None) #f]
             [('None 'None)
              (helper ve1 ve2 (add1 index))]
             [((Some a) (Some b))
              (if (symbol=? a b)
                  (helper ve1 ve2 (add1 index))
                  #f)])))}
    (helper v1 v2 0)))

(check-expect
 (vector=? (vector 'None (Some 'black))
           (vector 'None (Some 'black))) #t)

(check-expect
 (vector=? (vector 'None (Some 'black))
           (vector 'None 'None)) #f)

; decides if two Board are equal
(: board=? : Board Board -> Boolean)
(define (board=? b1 b2)
  (local
    {(: helper : (Vectorof (Vectorof (Optional Stone)))
        (Vectorof (Vectorof (Optional Stone))) Integer -> Boolean)
     (define (helper bo1 bo2 index)
       (if (= index (vector-length b1))
           #t
           (match (vector=? (vector-ref bo1 index)
                            (vector-ref bo2 index))
             [#t (helper bo1 bo2 (add1 index))]
             [#f #f])))}
    (helper b1 b2 0)))

(check-expect
 (board=?
  (vector
   (vector (Some 'black) 'None (Some 'white))
   (vector (Some 'white) (Some 'black) (Some 'white))
   (vector 'None 'None (Some 'white)))
  (vector
   (vector (Some 'black) 'None (Some 'white))
   (vector (Some 'white) (Some 'black) (Some 'white))
   (vector 'None 'None (Some 'white)))) #t)

(check-expect
 (board=?
  (vector
   (vector (Some 'black) 'None (Some 'white))
   (vector (Some 'white) (Some 'black) (Some 'white))
   (vector 'None (Some 'black) (Some 'white)))
  (vector
   (vector (Some 'black) 'None (Some 'white))
   (vector (Some 'white) (Some 'black) (Some 'white))
   (vector 'None 'None (Some 'white)))) #f)

;; creates an identical copy of the given board
(: board-copy : Board -> Board)
(define (board-copy b)
  (build-vector (vector-length b)
                (lambda ([x : Integer])
                  (vector-copy (vector-ref b x)))))

(check-expect
 (board-copy
  (vector
   (vector (Some 'black) 'None (Some 'white))
   (vector (Some 'white) (Some 'black) (Some 'white))
   (vector 'None 'None (Some 'white))))
 (vector
  (vector (Some 'black) 'None (Some 'white))
  (vector (Some 'white) (Some 'black) (Some 'white))
  (vector 'None 'None (Some 'white))))

;; put-stone-at:
;; Return (Some go+), where go+ includes the new stone, if possible.
;; Return 'None if location is already occupied.
;; Raise an error if it's not the turn to place that stone.
(: put-stone-at : LogicalLoc Stone Go -> (Optional Go))
(define (put-stone-at lloc s g)
  (match g
    [(Go gob next his lp loc lsc cp)
     (if (not (symbol=? s next))
         (error (string-append "It's not " (symbol->string s) "'s turn."))
         (match (board-ref g lloc)
           [(Some _) 'None]
           ['None
            (begin
              (board-set! g lloc (Some s))
              (Some (Go gob (adv next)
                        (cons (board-copy gob) his) lp loc lsc cp)))
            ]))]))

;; flip the next player, and leave all else about game the same
(: pass : Go -> Go)
(define (pass g)
  (match g
    [(Go gob next his lp loc lsc cp)
     (Go gob (adv next) (cons (board-copy gob) his) 'None '() '() (add1 cp))]))

;; read the next player out of the go struct
(: next-player : World -> Stone)
(define (next-player w)
  (match w
    [(World _ (Go _ next _ _ _ _ _) _ _ _ _) next]))

;; this applies pass to the go struct within the world and changes the status
;; message
(: pass/world : World -> World)
(define (pass/world w)
  (World (World-spec w)
         (pass (World-game w))
         (string-append (capstone (next-player w)) " passed.")
         (World-black-tenths w)
         (World-white-tenths w)
         (World-hover w)))

;; ======

;; decides if two (Optional Stone) are equal
(: optional-stone-equal? : (Optional Stone) (Optional Stone) -> Boolean)
(define (optional-stone-equal? s1 s2)
  (match* (s1 s2)
    [('None 'None) #t]
    [('None _) #f]
    [(_ 'None) #f]
    [((Some a) (Some b))
     (if (symbol=? a b) #t #f)]))

(check-expect
 (optional-stone-equal? 'None (Some 'black)) #f)

(check-expect
 (optional-stone-equal? (Some 'white) (Some 'black)) #f)

(check-expect
 (optional-stone-equal? 'None 'None) #t)

;; returns a list of LogicalLoc which are the neighbors of a given LogicalLoc
(: neighbors : Board LogicalLoc -> (Listof LogicalLoc))
(define (neighbors board lloc)
  (match lloc
    [(LogicalLoc lx ly)
     (local {(define dim (sub1 (vector-length board)))}
       (cond
         [(and (= lx 0) (= ly 0))
          (list
           (LogicalLoc (add1 lx) ly)
           (LogicalLoc lx (add1 ly)))]
         [(and (= lx 0) (= ly dim))
          (list
           (LogicalLoc lx (sub1 ly))
           (LogicalLoc (add1 lx) ly))]
         [(and (= lx dim) (= ly 0))
          (list
           (LogicalLoc (sub1 lx) ly)
           (LogicalLoc lx (add1 ly)))]
         [(and (= lx dim) (= ly dim))
          (list
           (LogicalLoc (sub1 lx) ly)
           (LogicalLoc lx (sub1 ly)))]
         [(= lx 0)
          (list
           (LogicalLoc (add1 lx) ly)
           (LogicalLoc lx (sub1 ly))
           (LogicalLoc lx (add1 ly)))]
         [(= ly 0)
          (list
           (LogicalLoc (sub1 lx) ly)
           (LogicalLoc (add1 lx) ly)
           (LogicalLoc lx (add1 ly)))]
         [(= lx dim)
          (list
           (LogicalLoc (sub1 lx) ly)
           (LogicalLoc lx (sub1 ly))
           (LogicalLoc lx (add1 ly)))]
         [(= ly dim)
          (list
           (LogicalLoc (sub1 lx) ly)
           (LogicalLoc (add1 lx) ly)
           (LogicalLoc lx (sub1 ly)))]
         [else
          (list
           (LogicalLoc (sub1 lx) ly)
           (LogicalLoc (add1 lx) ly)
           (LogicalLoc lx (sub1 ly))
           (LogicalLoc lx (add1 ly)))]))]))

(check-expect (neighbors
               (vector
                (vector 'None 'None (Some 'white) 'None)
                (vector 'None (Some 'black)
                        (Some 'black) (Some 'white))
                (vector 'None 'None (Some 'black) 'None)
                (vector 'None 'None 'None 'None))
               (LogicalLoc 2 3))
              (list (LogicalLoc 1 3) (LogicalLoc 3 3) (LogicalLoc 2 2)))

;; returns a list of LogicalLoc which are the neighbors with the same color
;; of a given LogicalLoc
(: same-color-neighbor : Board LogicalLoc (Listof LogicalLoc)
   -> (Listof LogicalLoc))
(define (same-color-neighbor board lloc l)
  (match* (lloc l)
    [((LogicalLoc lx ly) '()) '()]
    [((LogicalLoc lx ly) (cons (LogicalLoc nx ny) tl))
     (if (optional-stone-equal?
          (vector-ref (vector-ref board lx) ly)
          (vector-ref (vector-ref board nx) ny))
         (cons (LogicalLoc nx ny)
               (same-color-neighbor board lloc tl))
         (same-color-neighbor board lloc tl))]))

(check-expect
 (same-color-neighbor
  (vector
   (vector 'None 'None (Some 'white) 'None)
   (vector 'None (Some 'black)
           (Some 'black) (Some 'white))
   (vector 'None 'None (Some 'black) 'None)
   (vector 'None 'None 'None 'None))
  (LogicalLoc 1 2)
  (list (LogicalLoc 0 2) (LogicalLoc 2 2)
        (LogicalLoc 1 1) (LogicalLoc 1 3)))
 (list (LogicalLoc 2 2) (LogicalLoc 1 1)))

;; returns a list of LogicalLoc which are the neighbors with different color
;; of a given LogicalLoc
(: diff-color-neighbor : Board LogicalLoc (Listof LogicalLoc)
   -> (Listof LogicalLoc))
(define (diff-color-neighbor board lloc l)
  (match* (lloc l)
    [((LogicalLoc lx ly) '()) '()]
    [((LogicalLoc lx ly) (cons (LogicalLoc nx ny) tl))
     (match (vector-ref (vector-ref board nx) ny)
       ['None (diff-color-neighbor board lloc tl)]
       [_
        (if (not (optional-stone-equal?
                  (vector-ref (vector-ref board lx) ly)
                  (vector-ref (vector-ref board nx) ny)))
            (cons (LogicalLoc nx ny)
                  (diff-color-neighbor board lloc tl))
            (diff-color-neighbor board lloc tl))])]))

(check-expect
 (diff-color-neighbor
  (vector
   (vector 'None 'None (Some 'white) 'None)
   (vector 'None (Some 'black)
           (Some 'black) (Some 'white))
   (vector 'None 'None (Some 'black) 'None)
   (vector 'None 'None 'None 'None))
  (LogicalLoc 1 2)
  (list (LogicalLoc 0 2) (LogicalLoc 2 2)
        (LogicalLoc 1 1) (LogicalLoc 1 3)))
 (list (LogicalLoc 0 2) (LogicalLoc 1 3)))

;; returns a list of LogicalLoc which are the unmarked same-color neighbors
;; a given LogicalLoc
(: unmarked-same-color-neighbor-helper : Board LogicalLoc (Listof LogicalLoc)
   (Listof LogicalLoc) -> (Listof LogicalLoc))
(define (unmarked-same-color-neighbor-helper board lloc l marked)
  (match l
    ['() '()]
    [(cons hd tl)
     (if (member hd marked)
         (unmarked-same-color-neighbor-helper board lloc tl marked)
         (cons hd
               (unmarked-same-color-neighbor-helper board lloc tl marked)))]))

;; returns a list of LogicalLoc which are the unmarked same-color neighbors
;; a given LogicalLoc
(: unmarked-same-color-neighbor : Board LogicalLoc (Listof LogicalLoc) ->
   (Listof LogicalLoc))
(define (unmarked-same-color-neighbor board lloc marked)
  (unmarked-same-color-neighbor-helper
   board lloc (same-color-neighbor board lloc (neighbors board lloc)) marked))

(check-expect
 (unmarked-same-color-neighbor
  (vector
   (vector 'None 'None (Some 'white) 'None)
   (vector 'None (Some 'black)
           (Some 'black) (Some 'white))
   (vector 'None 'None (Some 'black) 'None)
   (vector 'None 'None 'None 'None))
  (LogicalLoc 1 2)
  (list
   (LogicalLoc 1 1)
   (LogicalLoc 0 2)))
 (list (LogicalLoc 2 2)))

;; decideds if a LogicalLoc has 'None (liberties) around it
;; l is the output of the neighbor function
(: liberties?-helper : Board (Listof LogicalLoc) -> Boolean)
(define (liberties?-helper board l)
  (match l
    ['() #f]
    [(cons hd tl)
     (match hd
       [(LogicalLoc lx ly)
        (match (vector-ref (vector-ref board lx) ly)
          ['None #t]
          [_ (liberties?-helper board tl)])])]))

(check-expect
 (liberties?-helper
  (vector
   (vector 'None (Some 'black) (Some 'black) 'None)
   (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
   (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
   (vector 'None (Some 'black) (Some 'black) 'None))
  (list (LogicalLoc 0 0) (LogicalLoc 0 2) (LogicalLoc 1 1))) #t)

(check-expect
 (liberties?-helper
  (vector
   (vector 'None (Some 'black) (Some 'black) 'None)
   (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
   (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
   (vector 'None (Some 'black) (Some 'black) 'None))
  (list (LogicalLoc 0 1) (LogicalLoc 1 0)
        (LogicalLoc 1 2) (LogicalLoc 2 1))) #f)

;; decideds if a LogicalLoc has 'None (liberties) around it
(: liberties? : Board LogicalLoc -> Boolean)
(define (liberties? b lloc)
  (liberties?-helper b (neighbors b lloc)))

(check-expect
 (liberties?
  (vector
   (vector 'None (Some 'black) (Some 'black) 'None)
   (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
   (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
   (vector 'None (Some 'black) (Some 'black) 'None)) (LogicalLoc 0 1)) #t)

;; identify the chains to be captured 
(: identify-chain : Board (Listof LogicalLoc) (Listof LogicalLoc)
   -> (Optional (Listof LogicalLoc)))
(define (identify-chain board to-explore marked)
  (match to-explore
    ['() (Some marked)]
    [(cons e tl)
     (if (liberties? board e)
         'None
         (local
           {(define m
              (unmarked-same-color-neighbor board e marked))}
           (identify-chain board 
                           (append tl m)
                           (append marked m))))]))

(check-expect
 (identify-chain
  (vector
   (vector 'None (Some 'black) (Some 'black) 'None)
   (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
   (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
   (vector 'None (Some 'black) (Some 'black) 'None))
  (list (LogicalLoc 2 1))
  (list (LogicalLoc 2 1)))
 (Some (list (LogicalLoc 2 1) (LogicalLoc 1 1)
             (LogicalLoc 2 2) (LogicalLoc 1 2))))
 
(check-expect
 (identify-chain
  (vector
   (vector 'None (Some 'black) (Some 'black) 'None)
   (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
   (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
   (vector 'None (Some 'black) (Some 'black) 'None))
  (list (LogicalLoc 0 1))
  (list (LogicalLoc 0 1))) 'None)

;; ======

;; Given the current state of the game, decides if the player whose turn it
;; currently is placing a stone at the specified location is a legal move
(: legal-move? : Go LogicalLoc -> Boolean)
(define (legal-move? go lloc)
  (match* (go lloc)
    [((Go board next his lp loc lsc cp) (LogicalLoc lx ly))
     (if (two-passes? go) #f
         (match (board-ref go lloc)
           [(Some g) #f] ;; location already occupied
           ['None
            (match (apply-move
                    (Go (board-copy board) next his lp loc lsc cp) lloc)
              [go-result
               (if (member (Go-board go-result) his) #f #t)])]))]))
 
(check-expect
 (legal-move?
  (Go
   (vector
    (vector 'None 'None (Some 'white) 'None)
    (vector 'None (Some 'black) (Some 'black) 'None)
    (vector 'None (Some 'white) 'None 'None)
    (vector 'None 'None 'None 'None))
   'black
   (list
    (vector
     (vector (Some 'black) 'None (Some 'white) 'None)
     (vector 'None (Some 'black) (Some 'black) 'None)
     (vector 'None (Some 'white) 'None 'None)
     (vector 'None 'None 'None 'None))) 'None '() '() 0)
  (LogicalLoc 0 0)) #f)

(check-expect
 (legal-move?
  (Go
   (vector
    (vector 'None 'None (Some 'white) 'None)
    (vector 'None (Some 'black) (Some 'black) 'None)
    (vector 'None (Some 'white) 'None 'None)
    (vector 'None 'None 'None 'None))
   'black
   (list) 'None '() '() 0)
  (LogicalLoc 0 0)) #t)

(check-expect
 (legal-move?
  (Go
   (vector
    (vector 'None 'None (Some 'white) 'None)
    (vector 'None (Some 'black) (Some 'black) 'None)
    (vector 'None (Some 'white) 'None 'None)
    (vector 'None 'None 'None 'None))
   'black
   (list) 'None '() '() 2)
  (LogicalLoc 0 0)) #f)

;; changes a location on board to 'None
(: change-to-none : Board LogicalLoc -> Board)
(define (change-to-none b lloc)
  (match lloc
    [(LogicalLoc lx ly)
     (begin
       (vector-set! (vector-ref b lx) ly 'None)
       b)]))

(check-expect
 (change-to-none
  (vector
   (vector 'None 'None (Some 'white) 'None)
   (vector 'None (Some 'black) (Some 'black) 'None)
   (vector 'None (Some 'white) 'None 'None)
   (vector 'None 'None 'None 'None))
  (LogicalLoc 0 2))
 (vector '#(None None None None)
         (vector 'None (Some 'black) (Some 'black) 'None)
         (vector 'None (Some 'white) 'None 'None)
         '#(None None None None)))

;; removes a list of stones from the board
(: remove-chains : Go (Listof LogicalLoc) -> Go)
(define (remove-chains go l)
  (match* (go l)
    [((Go board next his lp loc lsc cp) '()) go]
    [((Go board next his lp loc lsc cp) (cons hd tl))
     (remove-chains
      (Go (change-to-none board hd) next his lp loc lsc cp) tl)]))

(check-expect
 (remove-chains
  (Go
   (vector
    (vector 'None (Some 'black) (Some 'black) 'None)
    (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
    (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
    (vector 'None (Some 'black) (Some 'black) 'None))
   'black '() 'None '() '() 0)
  (list (LogicalLoc 2 1) (LogicalLoc 1 1)
        (LogicalLoc 2 2) (LogicalLoc 1 2)))
 (Go
  (vector
   (vector 'None (Some 'black) (Some 'black) 'None)
   (vector (Some 'black) 'None 'None (Some 'black))
   (vector (Some 'black) 'None 'None (Some 'black))
   (vector 'None (Some 'black) (Some 'black) 'None))
  'black '() 'None '() '() 0))

;; takes in the list of all the chains and removes them
(: apply-move-helper : Go (Listof LogicalLoc) -> Go)
(define (apply-move-helper go l)
  (remove-chains go l))

;; delete all the repeated elements in a list
(: delete-repeat : (Listof LogicalLoc) -> (Listof LogicalLoc))
(define (delete-repeat l)
  (match l
    ['() '()]
    [(cons hd tl)
     (if (member hd tl)
         (delete-repeat tl)
         (cons hd (delete-repeat tl)))]))

(check-expect
 (delete-repeat
  (list
   (LogicalLoc 0 2)
   (LogicalLoc 0 1)
   (LogicalLoc 1 1)
   (LogicalLoc 0 0)
   (LogicalLoc 2 1)
   (LogicalLoc 1 0)
   (LogicalLoc 2 0)
   (LogicalLoc 2 2)
   (LogicalLoc 2 2)
   (LogicalLoc 2 1)
   (LogicalLoc 1 1)
   (LogicalLoc 2 0)
   (LogicalLoc 0 1)
   (LogicalLoc 1 0)
   (LogicalLoc 0 0)
   (LogicalLoc 0 2)
   (LogicalLoc 1 1)
   (LogicalLoc 0 1)
   (LogicalLoc 2 1)
   (LogicalLoc 1 0)
   (LogicalLoc 0 0)
   (LogicalLoc 0 2)
   (LogicalLoc 2 0)
   (LogicalLoc 2 2)))
 (list (LogicalLoc 1 1) (LogicalLoc 0 1)
       (LogicalLoc 2 1) (LogicalLoc 1 0)
       (LogicalLoc 0 0) (LogicalLoc 0 2)
       (LogicalLoc 2 0) (LogicalLoc 2 2)))

;; returns a list of all the locations of all the stones 
(: list-of-chains : Board (Listof LogicalLoc)
   -> (Listof LogicalLoc))
(define (list-of-chains b l)
  (match l
    ['() '()]
    [(cons hd tl)
     (match (identify-chain b (list hd) (list hd))
       ['None (list-of-chains b tl)]
       [(Some c)
        (delete-repeat
         (append c
                 (list-of-chains b tl)))])]))

(check-expect
 (list-of-chains
  (vector
   (vector 'None (Some 'black) (Some 'black) 'None)
   (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
   (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
   (vector 'None (Some 'black) (Some 'black) 'None))
  (list (LogicalLoc 1 1)))
 (list (LogicalLoc 1 1) (LogicalLoc 2 1)
       (LogicalLoc 1 2) (LogicalLoc 2 2)))

;; check for and complete self-capture process
(: self-capture : Go LogicalLoc -> Go)
(define (self-capture g lloc)
  (match (identify-chain (Go-board g) (list lloc) (list lloc))
    ['None g]
    [(Some l) (remove-chains g l)]))

(check-expect
 (self-capture
  (Go
   (vector
    (vector 'None (Some 'black) (Some 'black) 'None)
    (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
    (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
    (vector 'None (Some 'black) (Some 'black) 'None))
   'white '() 'None '() '() 0)
  (LogicalLoc 1 1))
 (Go
  (vector
   (vector 'None (Some 'black) (Some 'black) 'None)
   (vector (Some 'black) 'None 'None (Some 'black))
   (vector (Some 'black) 'None 'None (Some 'black))
   (vector 'None (Some 'black) (Some 'black) 'None))
  'white '() 'None '() '() 0))

;; duplicate of apply-move
(: apply-move-duplicate : Go LogicalLoc -> Go) 
(define (apply-move-duplicate go lloc)
  (match* (go lloc)
    [((Go board next his lp loc lsc cp) (LogicalLoc lx ly))
     (local
       {(: copy : Board)
        (define copy (board-copy board))
        
        (define locations-to-identify
          (diff-color-neighbor board lloc (neighbors board lloc)))
        
        (: update : Go -> Go)
        (define (update g)
          (match g
            [(Go b next his lp loc lsc cp)
             (Go b (adv next) (cons copy his)
                 (Some lloc)
                 loc lsc
                 0)]))}

       ;; assuming legal-moved has been checked 
       (begin
         (board-set! go lloc (Some next))
         (apply-move-helper go
                            (list-of-chains board locations-to-identify))
         (self-capture go lloc)
         (Go board (adv next) (cons copy his)
             (Some lloc) '() '() 0)))]))

;; apply a move
(: apply-move : Go LogicalLoc -> Go) 
(define (apply-move go lloc)
  (match* (go lloc)
    [((Go board next his lp loc lsc cp) (LogicalLoc lx ly))
     (local
       {(: copy : Board)
        (define copy (board-copy board))

        (: copy2 : Board)
        (define copy2 (board-copy board))
        
        (: update : Go -> Go)
        (define (update g)
          (match g
            [(Go b next his lp loc lsc cp)
             (Go b (adv next) (cons copy his)
                 (Some lloc)
                 loc lsc
                 0)]))}

       ;; assuming legal-moved has been checked 
       (begin
         (board-set! go lloc (Some next))
         (local
           {(define locations-to-identify
              (diff-color-neighbor (Go-board go) lloc (neighbors board lloc)))
            (define last-opp-captures
              (list-of-chains board locations-to-identify))
            (define last-self-captures
              (match (identify-chain
                      (Go-board
                       (apply-move-helper
                        (Go (board-copy board) next his lp loc lsc cp)
                        (list-of-chains board locations-to-identify)))
                      (list lloc) (list lloc))
                ['None '()]
                [(Some c) c]))}
           
           (apply-move-helper go
                              (list-of-chains board locations-to-identify))
           (self-capture go lloc)
           (Go board (adv next) (cons copy his)
               (Some lloc) last-opp-captures last-self-captures 0))))]))

(check-expect
 (apply-move
  (Go
   (vector
    (vector 'None (Some 'black) (Some 'black) 'None)
    (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
    (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
    (vector 'None (Some 'black) 'None 'None))
   'black '() 'None '() '() 0)
  (LogicalLoc 3 2))
 (Go
  (vector
   (vector 'None (Some 'black) (Some 'black) 'None)
   (vector (Some 'black) 'None 'None (Some 'black))
   (vector (Some 'black) 'None 'None (Some 'black))
   (vector 'None (Some 'black) (Some 'black) 'None))
  'white
  (list
   (vector
    (vector 'None (Some 'black) (Some 'black) 'None)
    (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
    (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
    (vector 'None (Some 'black) 'None 'None)))
  (Some (LogicalLoc 3 2))
  (list (LogicalLoc 2 2) (LogicalLoc 1 2)
        (LogicalLoc 2 1) (LogicalLoc 1 1)) '() 0))

(check-expect
 (apply-move
  (Go
   (vector
    (vector 'None (Some 'black) (Some 'black) 'None)
    (vector (Some 'black) 'None (Some 'white) (Some 'black))
    (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
    (vector 'None (Some 'black) (Some 'black) 'None))
   'white '() 'None '() '() 0)
  (LogicalLoc 1 1))
 (Go
  (vector
   (vector 'None (Some 'black) (Some 'black) 'None)
   (vector (Some 'black) 'None 'None (Some 'black))
   (vector (Some 'black) 'None 'None (Some 'black))
   (vector 'None (Some 'black) (Some 'black) 'None))
  'black
  (list
   (vector
    (vector 'None (Some 'black) (Some 'black) 'None)
    (vector (Some 'black) 'None (Some 'white) (Some 'black))
    (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
    (vector 'None (Some 'black) (Some 'black) 'None)))
  (Some (LogicalLoc 1 1)) '()
  (list (LogicalLoc 1 1) (LogicalLoc 2 1)
        (LogicalLoc 1 2) (LogicalLoc 2 2)) 0))

(check-expect
 (apply-move
  (Go
   (vector
    (vector (Some 'black) (Some 'black) (Some 'black))
    (vector (Some 'black) (Some 'black) 'None)
    (vector (Some 'black) (Some 'black) (Some 'black)))
   'white '() 'None '() '() 0)
  (LogicalLoc 1 2))
 (Go (vector '#(None None None)
             (vector 'None 'None (Some 'white))
             '#(None None None))
     'black
     (list
      (vector
       (vector (Some 'black) (Some 'black) (Some 'black))
       (vector (Some 'black) (Some 'black) 'None)
       (vector (Some 'black) (Some 'black) (Some 'black))))
     (Some (LogicalLoc 1 2))
     (list (LogicalLoc 1 1) (LogicalLoc 0 1)
           (LogicalLoc 2 1) (LogicalLoc 1 0)
           (LogicalLoc 0 0) (LogicalLoc 0 2)
           (LogicalLoc 2 0) (LogicalLoc 2 2))
     '() 0))

;; ======

;; check board-spec for non-ridiculosity
(: valid-board-spec? : BoardSpec -> Boolean)
(define (valid-board-spec? spec)
  (match spec
    [(BoardSpec bg cell margin stone)
     (and (positive? cell)
          (positive? margin)
          (positive? stone)
          (< stone (quotient cell 2))
          (> margin stone))]))

(check-expect
 (valid-board-spec?
  (BoardSpec 'moccasin 30 7 8)) #f)

(check-expect
 (valid-board-spec?
  (BoardSpec 'moccasin 30 22 8)) #t)

;; ======

;; increase the time of the current player and total time
(: react-to-tick (World -> World))
(define (react-to-tick w)
  (match w
    [(World spec (Go b s his ltp ltoc ltsc cp)
            msg btime wtime h)
     (if (two-passes? (World-game w)) w
         (match s
           ['black
            (World spec (Go b s his ltp ltoc ltsc cp)
                   msg (+ 1 btime) wtime h)]
           ['white
            (World spec (Go b s his ltp ltoc ltsc cp)
                   msg btime (+ 1 wtime) h)]))]))

(check-expect
 (react-to-tick
  (World
   (BoardSpec 'moccasin 30 22 8)
   (Go
    (vector
     (vector 'None (Some 'black) (Some 'black) 'None)
     (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
     (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
     (vector 'None (Some 'black) 'None 'None))
    'black '() 'None '() '() 0)
   "Welcome to go!"
   108
   211
   'None))
 (World
  (BoardSpec 'moccasin 30 22 8)
  (Go
   (vector
    (vector 'None (Some 'black) (Some 'black) 'None)
    (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
    (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
    (vector 'None (Some 'black) 'None 'None))
   'black '() 'None '() '() 0)
  "Welcome to go!"
  109
  211
  'None))

;; format the time into forms like 0:00.0
(: format-time : Integer -> String)
(define (format-time tenth-seconds)
  (local
    {(define seconds (quotient tenth-seconds 10))
     (define minutes (quotient seconds 60))}
    (string-append
     (number->string minutes)
     ":"
     (if (< (quotient (- seconds (* minutes 60)) 10) 1)
         (string-append "0"
                        (number->string (- seconds (* minutes 60))))
         (number->string (- seconds (* minutes 60))))
     "."
     (number->string (- tenth-seconds (* 10 seconds))))))

(check-expect (format-time 21109) "35:10.9")
(check-expect (format-time 1880) "3:08.0")

;; time string for black
(: black-timer : World -> String)
(define (black-timer w)
  (local {(define black-time (World-black-tenths w))}
    (string-append
     "Black total time: "
     (format-time black-time))))

(check-expect
 (black-timer
  (World
   (BoardSpec 'moccasin 30 22 8)
   (Go
    (vector
     (vector 'None (Some 'black) (Some 'black) 'None)
     (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
     (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
     (vector 'None (Some 'black) 'None 'None))
    'black '() 'None '() '() 0)
   "Welcome to go!"
   108
   211
   'None))
 "Black total time: 0:10.8")

;; time string for white
(: white-timer : World -> String)
(define (white-timer w)
  (local {(define white-time (World-white-tenths w))}
    (string-append
     "White total time: "
     (format-time white-time))))

(check-expect
 (white-timer
  (World
   (BoardSpec 'moccasin 30 22 8)
   (Go
    (vector
     (vector 'None (Some 'black) (Some 'black) 'None)
     (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
     (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
     (vector 'None (Some 'black) 'None 'None))
    'black '() 'None '() '() 0)
   "Welcome to go!"
   108
   211
   'None))
 "White total time: 0:21.1")

;; time string for whole game
(: total-timer : World -> String)
(define (total-timer w)
  (local
    {(define black-time (World-black-tenths w))
     (define white-time (World-white-tenths w))
     (define total-time (+ black-time white-time))}
    (string-append
     "Total time: "
     (format-time total-time))))

(check-expect
 (total-timer
  (World
   (BoardSpec 'moccasin 30 22 8)
   (Go
    (vector
     (vector 'None (Some 'black) (Some 'black) 'None)
     (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
     (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
     (vector 'None (Some 'black) 'None 'None))
    'black '() 'None '() '() 0)
   "Welcome to go!"
   108
   211
   'None))
 "Total time: 0:31.9")

;; ======

;; report whether the last two moves have been passes,
;; indicating that the game is now over
(: two-passes? : Go -> Boolean)
(define (two-passes? go)
  (if (>= (Go-consecutive-passes go) 2) #t #f))

(check-expect
 (two-passes?
  (Go
   (vector
    (vector 'None (Some 'black) (Some 'black) 'None)
    (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
    (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
    (vector 'None (Some 'black) 'None 'None))
   'black '() 'None '() '() 2)) #t)

(check-expect
 (two-passes?
  (Go
   (vector
    (vector 'None (Some 'black) (Some 'black) 'None)
    (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
    (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
    (vector 'None (Some 'black) 'None 'None))
   'black '() 'None '() '() 1)) #f)

;; ======

;; equivalent to liberties?-helper
(: different-color?-helper : Stone Board (Listof LogicalLoc) -> Boolean)
(define (different-color?-helper s board l)
  (match l
    ['() #f]
    [(cons hd tl)
     (match hd
       [(LogicalLoc lx ly)
        (match (vector-ref (vector-ref board lx) ly)
          ['None (different-color?-helper s board tl)]
          [(Some st)
           (if (symbol=? st s)
               (different-color?-helper s board tl)
               #t)])])]))

;; equivalent to liberties?
(: different-color? : Stone Board LogicalLoc -> Boolean)
(define (different-color? s b lloc)
  (different-color?-helper s b (neighbors b lloc)))

(check-expect
 (different-color?
  'black
  (vector
   (vector 'None (Some 'black) (Some 'black) 'None)
   (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
   (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
   (vector 'None (Some 'black) 'None 'None))
  (LogicalLoc 3 2)) #t)

(check-expect
 (different-color?
  'black
  (vector
   (vector 'None (Some 'black) (Some 'black) 'None)
   (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
   (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
   (vector 'None (Some 'black) 'None 'None))
  (LogicalLoc 0 0)) #f)

;; returns all the neighbors that are 'None and unmarked
(: unmarked-none-neighbor : Board LogicalLoc (Listof LogicalLoc) ->
   (Listof LogicalLoc))
(define (unmarked-none-neighbor b lloc marked)
  (local
    {(: lp : Board (Listof LogicalLoc) (Listof LogicalLoc) ->
        (Listof LogicalLoc))
     (define (lp b neigh mark)
       (match neigh
         ['() '()]
         [(cons hd tl)
          (match hd
            [(LogicalLoc lx ly)
             (if (member hd mark)
                 (lp b tl mark)
                 (match (vector-ref (vector-ref b lx) ly)
                   ['None (cons hd (lp b tl mark))]
                   [_ (lp b tl mark)]))])]))}
    (lp b (neighbors b lloc) marked)))

(check-expect
 (unmarked-none-neighbor
  (vector
   (vector 'None (Some 'black) (Some 'black) 'None)
   (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
   (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
   (vector 'None (Some 'black) 'None 'None))
  (LogicalLoc 3 3) '()) (list (LogicalLoc 3 2)))

;; identify all territories of a color of stones
(: identify-territory :
   Stone Board (Listof LogicalLoc) (Listof LogicalLoc) ->
   (Optional (Listof LogicalLoc)))
(define (identify-territory s board to-explore marked)
  (match to-explore
    ['() (Some marked)]
    [(cons e tl)
     (if (different-color? s board e)
         'None
         (local
           {(define m (unmarked-none-neighbor board e marked))}
           (identify-territory s board
                               (append tl m)
                               (append marked m))))]))

(check-expect
 (identify-territory
  'black
  (vector
   (vector 'None 'None 'None 'None 'None 'None)
   (vector 'None 'None (Some 'black) 'None 'None 'None)
   (vector 'None (Some 'black) 'None (Some 'black) 'None 'None)
   (vector 'None (Some 'black) 'None (Some 'black) 'None 'None)
   (vector 'None 'None (Some 'black) 'None 'None 'None)
   (vector 'None 'None 'None 'None 'None 'None))
  (list (LogicalLoc 0 0))
  (list (LogicalLoc 0 0)))
 (Some
  (list
   (LogicalLoc 0 0) (LogicalLoc 1 0) (LogicalLoc 0 1) (LogicalLoc 2 0)
   (LogicalLoc 1 1) (LogicalLoc 0 2) (LogicalLoc 3 0) (LogicalLoc 0 3)
   (LogicalLoc 4 0) (LogicalLoc 1 3) (LogicalLoc 0 4) (LogicalLoc 5 0)
   (LogicalLoc 4 1) (LogicalLoc 1 4) (LogicalLoc 0 5) (LogicalLoc 5 1)
   (LogicalLoc 2 4) (LogicalLoc 1 5) (LogicalLoc 5 2) (LogicalLoc 3 4)
   (LogicalLoc 2 5) (LogicalLoc 5 3) (LogicalLoc 4 4) (LogicalLoc 3 5)
   (LogicalLoc 4 3) (LogicalLoc 5 4) (LogicalLoc 4 5) (LogicalLoc 5 5))))

;; remove a list from another list
(: remove-list : (Listof LogicalLoc) (Listof LogicalLoc) -> (Listof LogicalLoc))
(define (remove-list to-remove large-list)
  (match to-remove
    ['() large-list]
    [(cons hd tl)
     (remove-list tl (remove hd large-list))]))

;; infinite loop inside
;; remove the results of (identify-territory (first listlloc) ...) and recurse
(: all-the-territory-helper : Stone Board (Listof LogicalLoc) ->
   (Listof LogicalLoc))
(define (all-the-territory-helper s board listnone)
  (match listnone
    ['() '()]
    [(cons hd tl)
     (match (identify-territory s board (list hd) (list hd))
       ['None (all-the-territory-helper s board tl)]
       [(Some chains)
        (append
         chains
         (all-the-territory-helper s board (remove-list chains listnone)))])]))

;; return all the 'None LogicalLoc on the board
(: none-locs : Board Integer Integer -> (Listof LogicalLoc))
(define (none-locs b lx ly)
  (local
    {(define dim (vector-length b))}
    (cond
      [(= lx dim) '()]
      [(= ly dim) (none-locs b (add1 lx) 0)]
      [else
       (match (vector-ref (vector-ref b lx) ly)
         ['None
          (cons (LogicalLoc lx ly)
                (none-locs b lx (add1 ly)))]
         [_ (none-locs b lx (add1 ly))])])))

;; shows all the territory for a color of stones
;; example: 'black b -> all the 'None territories of 'black on the board
(: all-the-territory : Stone Board -> (Listof LogicalLoc))
(define (all-the-territory s board)
  (all-the-territory-helper s board (none-locs board 0 0)))

;; number of intersections occupied by a color of stones
(: num-territory-occupied : Stone Board -> Integer)
(define (num-territory-occupied s board)
  (local
    {(define dim (vector-length board))
     (: lp : Stone Board Integer Integer -> Integer)
     (define (lp s b lx ly)
       (cond
         [(= lx dim) 0]
         [(= ly dim) (lp s b (add1 lx) 0)]
         [else
          (match (vector-ref (vector-ref b lx) ly)
            ['None (lp s b lx (add1 ly))]
            [(Some st)
             (if (symbol=? st s)
                 (+ 1 (lp s b lx (add1 ly)))
                 (lp s b lx (add1 ly)))])]))}
    (lp s board 0 0)))

(check-expect
 (num-territory-occupied
  'black
  (vector
   (vector 'None 'None 'None 'None 'None 'None)
   (vector 'None 'None (Some 'black) 'None 'None 'None)
   (vector 'None (Some 'black) 'None (Some 'black) 'None 'None)
   (vector 'None (Some 'black) 'None (Some 'black) 'None 'None)
   (vector 'None 'None (Some 'black) 'None 'None 'None)
   (vector 'None 'None 'None 'None 'None 'None))) 6)

;; reports the outcome of the game:
;; the area controlled by each player and the winner
(: outcome : Go -> Outcome)
(define (outcome go)
  (match go
    [(Go board _ _ _ _ _ _)
     (local
       {(define black-area
          (+ (num-territory-occupied 'black board)
             (length (all-the-territory 'black board))))
        (define white-area
          (+ (num-territory-occupied 'white board)
             (length (all-the-territory 'white board))))}
       (Outcome
        black-area
        white-area
        (cond
          [(> black-area white-area) 'black]
          [(> white-area black-area) 'white]
          [else 'draw])))]))

(check-expect
 (outcome
  (Go
   (vector
    (vector 'None 'None 'None 'None 'None 'None)
    (vector 'None 'None (Some 'black) 'None 'None 'None)
    (vector 'None (Some 'black) 'None (Some 'black) 'None 'None)
    (vector 'None (Some 'black) 'None (Some 'black) 'None 'None)
    (vector 'None 'None (Some 'black) 'None 'None 'None)
    (vector 'None 'None 'None 'None 'None 'None))
   'black
   '()
   'None
   '()
   '()
   3))
 (Outcome 36 0 'black))

;; ======
;; save-and-load functions

;; convert a text representation of an Integer to an Integer
;; raise an error if the string is not a number
;; return the integer part of the resulting number only
;; (this is intended only to be used with integers)
(: string->integer : String -> Integer)
(define (string->integer s)
  (local
    {(define conv : (U Complex False) (string->number s))}
    (if (complex? conv) 
        (exact-round (real-part conv))
        (error "string->integer: invalid integer"))))

;; turns a Board into a String
(: board->string : Board -> String)
(define (board->string b)
  (local
    {(define dim (vector-length b))
     (: lp : Board Integer Integer -> String)
     (define (lp board lx ly)
       (cond
         [(= lx dim) ""]
         [(and (= ly dim) (= lx (sub1 dim)))
          (lp board (add1 lx) 0)]
         [(= ly dim)
          (string-append
           "|"
           (lp board (add1 lx) 0))]
         [else
          (string-append
           (match (vector-ref (vector-ref board lx) ly)
             ['None "_"]
             [(Some 'black) "*"]
             [(Some 'white) "o"])
           (lp board lx (add1 ly)))]))}
    (lp b 0 0)))

(check-expect
 (board->string
  (vector
   (vector 'None 'None 'None)
   (vector 'None 'None 'None)
   (vector 'None 'None 'None)))
 "___|___|___")

(check-expect
 (board->string
  (vector
   (vector (Some 'black) (Some 'black) 'None)
   (vector 'None (Some 'white) 'None)
   (vector 'None (Some 'white) 'None)))
 "**_|_o_|_o_")

;; turns a listof Board into String
(: history->string : (Listof Board) -> String)
(define (history->string l)
  (match l
    [(cons hd '()) (board->string hd)]
    [(cons hd tl)
     (string-append
      (board->string hd)
      "!"
      (history->string tl))]))

(check-expect
 (history->string
  (list
   (vector
    (vector (Some 'black) (Some 'black) 'None)
    (vector 'None (Some 'white) 'None)
    (vector 'None (Some 'white) 'None))
   (vector
    (vector 'None 'None 'None)
    (vector 'None 'None 'None)
    (vector 'None 'None 'None))))
 "**_|_o_|_o_!___|___|___")

;; turns a Go into String
(: go->string : Go -> String)
(define (go->string go)
  (match go
    [(Go board next his lp loc lsc cp)
     (string-append
      (match next
        ['black "*"]
        ['white "o"])
      "~"
      (board->string board)
      "~"
      (history->string his)
      "~"
      (number->string cp))]))

(check-expect
 (go->string
  (Go
   (vector
    (vector (Some 'black) (Some 'black) 'None)
    (vector (Some 'black) (Some 'white) 'None)
    (vector 'None (Some 'white) 'None))
   'white
   (list
    (vector
     (vector (Some 'black) (Some 'black) 'None)
     (vector 'None (Some 'white) 'None)
     (vector 'None (Some 'white) 'None))
    (vector
     (vector 'None 'None 'None)
     (vector 'None 'None 'None)
     (vector 'None 'None 'None)))
   (Some (LogicalLoc 1 0))
   '() '() 0))
 "o~**_|*o_|_o_~**_|_o_|_o_!___|___|___~0")

;; turns a World into a savable String
(: world->string : World -> String)
(define (world->string w)
  (match w
    [(World spec go _ btime wtime _)
     (string-append
      (number->string btime)
      "@"
      (number->string wtime)
      "@"
      (go->string go))]))

(check-expect
 (world->string
  (World
   (BoardSpec 'moccasin 30 22 8)
   (Go
    (vector
     (vector (Some 'black) (Some 'black) 'None)
     (vector (Some 'black) (Some 'white) 'None)
     (vector 'None (Some 'white) 'None))
    'white
    (list
     (vector
      (vector (Some 'black) (Some 'black) 'None)
      (vector 'None (Some 'white) 'None)
      (vector 'None (Some 'white) 'None))
     (vector
      (vector 'None 'None 'None)
      (vector 'None 'None 'None)
      (vector 'None 'None 'None)))
    (Some (LogicalLoc 1 0))
    '() '() 0)
   "Black moves to B1"
   1082
   773
   (Some (LogicalLoc 0 2))))
 "1082@773@o~**_|*o_|_o_~**_|_o_|_o_!___|___|___~0")

;; turns a String into a Board
(: string->board : String -> Board)
(define (string->board s)
  (local
    {(define lcolumns (string-split s "|"))
     (define dim (length lcolumns))
     (define result (empty-board dim))
     
     (: small-lp : String Integer Integer Board -> Void)
     (define (small-lp st lx ly b)
       (if (= ly dim) (void)
           (begin
             (vector-set!
              (vector-ref b lx) ly
              (match (substring st ly (add1 ly))
                ["o" (Some 'white)]
                ["*" (Some 'black)]
                ["_" 'None]))
             (small-lp st lx (add1 ly) b))))
     
     (: big-lp : (Listof String) Integer Board -> Void)
     (define (big-lp l lx b)
       (if (= lx dim) (void)
           (begin
             (small-lp (list-ref l lx) lx 0 b)
             (big-lp l (add1 lx) b))))}
    
    (begin (big-lp lcolumns 0 result) result)))

(check-expect
 (string->board "**_|*o_|_o_")
 (vector (vector (Some 'black) (Some 'black) 'None)
         (vector (Some 'black) (Some 'white) 'None)
         (vector 'None (Some 'white) 'None)))

(check-expect
 (string->board "___|___|___")
 '#(#(None None None) #(None None None) #(None None None)))

;; turns a String into a Listof Board
(: string->history : String -> (Listof Board))
(define (string->history s)
  (local
    {(define lboard (string-split s "!"))
     (: lp : (Listof String) -> (Listof Board))
     (define (lp l)
       (match l
         ['() '()]
         [(cons hd tl)
          (cons (string->board hd)
                (lp tl))]))}
    (lp lboard)))

(check-expect
 (string->history "**_|_o_|_o_!___|___|___")
 (list
  (vector (vector (Some 'black) (Some 'black) 'None)
          (vector 'None (Some 'white) 'None)
          (vector 'None (Some 'white) 'None))
  '#(#(None None None) #(None None None) #(None None None))))

;; turns a String into a Go
(: string->go : String -> Go)
(define (string->go s)
  (local
    {(define l (string-split s "~"))}
    (Go
     (string->board (list-ref l 1))
     (match (list-ref l 0)
       ["o" 'white]
       ["*" 'black])
     (string->history (list-ref l 2))
     'None '() '()
     (string->integer (list-ref l 3)))))

(check-expect
 (string->go "o~**_|*o_|_o_~**_|_o_|_o_!___|___|___~0")
 (Go
  (vector (vector (Some 'black) (Some 'black) 'None)
          (vector (Some 'black) (Some 'white) 'None)
          (vector 'None (Some 'white) 'None))
  'white
  (list
   (vector (vector (Some 'black) (Some 'black) 'None)
           (vector 'None (Some 'white) 'None)
           (vector 'None (Some 'white) 'None))
   '#(#(None None None) #(None None None) #(None None None)))
  'None '() '() 0))

(define test-string1
  (string-append
   "58@64@o~___________________|___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|_________*_________|"
   "________*_*________|_________*o________|___________________|"
   "___________________|___________________|___________________|"
   "___________________~___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|___________________|"
   "_________*_________|_________o*________|_________*o________|"
   "___________________|___________________|___________________|"
   "___________________|___________________!___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|_________*_________|_________o*________|"
   "_________*_________|___________________|___________________|"
   "___________________|___________________|___________________!"
   "___________________|___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|_________*_________|"
   "_________o*________|___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________!___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|___________________|"
   "_________*_________|_________o*________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________!___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|_________*_________|_________o_________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|___________________!"
   "___________________|___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|_________*_________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________!___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________~0"))

(define test-string2
  (string-append
   "58@64@o~___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|_________*_________|"
   "________*_*________|_________*o________|___________________|"
   "___________________|___________________|___________________|"
   "___________________~___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|___________________|"
   "_________*_________|_________o*________|_________*o________|"
   "___________________|___________________|___________________|"
   "___________________|___________________!___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|_________*_________|_________o*________|"
   "_________*_________|___________________|___________________|"
   "___________________|___________________|___________________!"
   "___________________|___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|_________*_________|"
   "_________o*________|___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________!___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|___________________|"
   "_________*_________|_________o*________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________!___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|_________*_________|_________o_________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|___________________!"
   "___________________|___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|_________*_________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________!___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________|___________________|"
   "___________________|___________________~0"))

;; tests if the saved String is correctly formatted
(: string-correctformat? : String -> Boolean)
(define (string-correctformat? s)
  (local
    {(define world (string-split s "@"))
     (define go (string-split (list-ref (string-split s "@") 2) "~"))}
    (if (and (= (length world) 3)
             (= (length go) 4)) #t #f)))

(check-expect (string-correctformat? test-string1) #t)

;; tests if all the Boards are squares
(: board-square? : String -> Boolean)
(define (board-square? s)
  (local
    {(define go (list-ref (string-split s "@") 2))
     (define cboard (list-ref (string-split go "~") 1))
     (define hisboard (string-split (list-ref (string-split go "~") 2) "!"))
     (: single-board-square? : String -> Boolean)
     (define (single-board-square? ss)
       (if
        (=
         (length (string-split ss "|"))
         (string-length (list-ref (string-split ss "|") 0)))
        #t #f))
     (: his-board-square? : (Listof String) -> Boolean)
     (define (his-board-square? l)
       (match l
         ['() #t]
         [(cons hd tl)
          (if (single-board-square? hd)
              (his-board-square? tl) #f)]))}
    (if (and (single-board-square? cboard)
             (his-board-square? hisboard)) #t #f)))

(check-expect (board-square? test-string1) #t)
 
;; turns a String into a World
(: string->world : BoardSpec String -> World)
(define (string->world bs s)
  (local
    {(define l (string-split s "@"))}
    (cond
      [(not (string-correctformat? s))
       (error (string-append "string->world: the String in the loaded"
                             "file is not correctly formatted"))]
      [(not (board-square? s))
       (error (string-append "string->world: the current board or some "
                             "board in history is not a square"))]
      [else
       (World
        bs
        (string->go (list-ref l 2))
        "Welcome to Go!"
        (string->integer (list-ref l 0))
        (string->integer (list-ref l 1))
        'None)])))

(check-error
 (string->world (BoardSpec 'moccasin 30 22 8) test-string2)
 (string-append "string->world: the current board or some "
                "board in history is not a square"))

;; prompt the user for an output file location
;; then, save the game to that file
;; do nothing if the user cancels
(: save-game! : World -> Void)
(define (save-game! w)
  (local
    {(define path : (U Path False) (put-file))}
    (if (path? path)
        (local
          {(define op : Output-Port (open-output-file path))}
          (begin (write-string (world->string w) op)
                 (close-output-port op)))
        (void))))

;; ask the user to choose a file
;; then load an in-progress game from that file
;; use the provided BoardSpec to make a new World
;; raise an error if the user cancels or if something goes wrong
(: load-game : BoardSpec -> World)
(define (load-game bs)
  (local
    {(define path : (U Path False) (get-file))}
    (if (path? path)
        (local
          {(define ip : Input-Port (open-input-file path))
           (define w : World
             (string->world bs (port->string (open-input-file path))))}
          (begin (close-input-port ip) w))
        (error "load-game: user cancelled"))))

;; ======

;; generates an empty board with no stones on it
(: empty-board : Integer -> Board)
(define (empty-board dim)
  (local
    {(: none : (Optional Stone))
     (define none 'None)}
    (build-vector
     dim  
     (lambda ([x : Integer])
       (make-vector dim none)))))

(check-expect
 (empty-board 5)
 '#(#(None None None None None)
    #(None None None None None)
    #(None None None None None)
    #(None None None None None)
    #(None None None None None)))

;; build a new game of given dim, with default board spec if none is provided
(: initial-world : Integer (Optional BoardSpec) -> World)
(define (initial-world dim opt-spec)
  (local
    {(define sp
       (match opt-spec
         [(Some spec) spec]
         ['None (BoardSpec 'moccasin 24 16 4)]))}
    (World sp
           (Go (empty-board dim) 'black '() 'None '() '() 0)
           "Welcome to Go!"
           0 0 'None)))

(check-expect
 (initial-world 3 'None)
 (World
  (BoardSpec 'moccasin 24 16 4)
  (Go '#(#(None None None)
         #(None None None)
         #(None None None))
      'black '() 'None '() '() 0)
  "Welcome to Go!"
  0 0 'None))

;; draw the grid only, for overlay later
(: draw-grid : Integer Integer -> Image)
(define (draw-grid dim cell)
  (local {(define c (square cell 'outline 'black))
          (define row (foldr beside empty-image (make-list (sub1 dim) c)))}
    (foldr above empty-image (make-list (sub1 dim) row))))

"eyeball tests: draw-grid"
(draw-grid 2 10)
(draw-grid 3 12)
(draw-grid 19 8)

;; draw the row labels that go on the right edge of the board
;; 1 goes at the bottom, and numbers increase going up
(: row-labels : World -> Image)
(define (row-labels w)
  (match w
    [(World (BoardSpec _ cell margin _) (Go gob _ _ _ _ _ _) _ _ _ _)
     (above (square (max 0 (- margin (quotient cell 2))) 'solid 'white)
            (foldr (lambda ([row-label : String] [img : Image])
                     (above (overlay (text row-label 11 'black)
                                     (square cell 'outline 'white))
                            img))
                   empty-image
                   (build-list
                    (vector-length gob)
                    (lambda ([i : Integer])
                      (number->string (- (vector-length gob) i))))))]))

"eyeball tests: row-labels"
(row-labels (initial-world 3 'None))

;; draw column labels to go along the bottom of the board
(: column-labels : World -> Image)
(define (column-labels w)
  (match w
    [(World (BoardSpec _ cell margin _) (Go gob _ _ _ _ _ _) _ _ _ _)
     (foldr (lambda ([column-label : String] [img : Image])
              (beside (overlay (text column-label 10 'black)
                               (square cell 'outline 'white))
                      img))
            empty-image
            (build-list (vector-length gob) column->string))]))

"eyeball-tests: column-labels"
(column-labels (initial-world 19 'None))

;; produce the message panel that will appear at the bottom of the display
(: message-panel : World -> Image)
(define (message-panel w)
  (match w
    [(World (BoardSpec bg cell margin _) (Go gob next _ _ _ _ cp) stat _ _ _)
     (local
       {(define msg : String
          (string-append stat
                         (if (symbol=? next 'black) " (b)" " (w)")))
        (define panel : Image
          (rectangle (+ (* cell (sub1 (vector-length gob)))
                        (* 2 margin)) 40 'solid bg))}
       (if (two-passes? (World-game w))
           (overlay
            (above
             (beside
              (text "The game is over!  " 12 'black)
              (match (outcome (World-game w))
                [(Outcome _ _ 'draw)
                 (text "The result is a draw!  " 12 'black)]
                [(Outcome _ _ 'black)
                 (text "Black wins!  " 12 'black)]
                [(Outcome _ _ 'white)
                 (text "White wins!  " 12 'black)])
              (match (outcome (World-game w))
                [(Outcome b w _)
                 (text
                  (string-append
                   "Black gains " (number->string b) " scores." "  "
                   "White gains " (number->string w) " scores.") 12 'black)]))
             (text (string-append
                    (black-timer w)
                    "   "
                    (white-timer w)
                    "   "
                    (total-timer w)) 13 'black))
            panel)
           (overlay
            (above
             (text msg 12 'black)
             (text (string-append
                    (black-timer w)
                    "   "
                    (white-timer w)
                    "   "
                    (total-timer w)) 13 'black))
            panel)))]))

"eyeball-tests: message-panel"
(message-panel
 (World
  (BoardSpec 'moccasin 30 22 8)
  (Go
   (vector
    (vector 'None (Some 'black) (Some 'black) 'None)
    (vector (Some 'black) 'None 'None (Some 'black))
    (vector (Some 'black) 'None 'None (Some 'black))
    (vector 'None (Some 'black) (Some 'black) 'None))
   'white
   (list
    (vector
     (vector 'None (Some 'black) (Some 'black) 'None)
     (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
     (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
     (vector 'None (Some 'black) 'None 'None)))
   'None '() '() 0)
  "Black moved to D3."
  114 514 'None))
     
;; draw the current state of the game, including labels and status message
(: draw : World -> Image)
(define (draw w)
  (match w
    [(World (BoardSpec bg cell margin stone)
            (Go gob next his lp loc lsc cp) stat
            btime wtime hover)
     (local
       {(define dim (vector-length gob))
        (define msg-panel (message-panel w))
        (define wood (square (+ (* 2 margin) (* cell (sub1 dim))) 'solid bg))
        (define empty-board (overlay (draw-grid dim cell) wood))
        (define board-side
          (square (+ (* 2 margin)
                     (* cell (sub1 dim))) 'outline 'white))
        (: draw-stone : Stone -> Image)
        (define (draw-stone s)
          (circle stone 'solid s))
        
        (define ghost-stone (circle stone 128 next))
        (: draw-ghost-stone : LogicalLoc Integer BoardSpec -> Image)
        (define (draw-ghost-stone lloc dim (World-spec w))
          (local
            {(define ploc (logical->physical lloc dim
                                             (BoardSpec bg cell margin stone)))
             (define board-side
               (square (+ (* 2 margin)
                          (* cell (sub1 dim))) 'outline 'white))}
            (place-image ghost-stone
                         (PhysicalLoc-x-offset-from-left ploc)
                         (PhysicalLoc-y-offset-from-top ploc)
                         board-side)))

        (define last-placed-stone
          (circle (/ stone 2) 'solid 'red))
        (: draw-last-placed : LogicalLoc Integer BoardSpec -> Image)
        (define (draw-last-placed lloc dim (World-spec w))
          (local
            {(define ploc (logical->physical lloc dim
                                             (BoardSpec bg cell margin stone)))
             (define board-side
               (square (+ (* 2 margin)
                          (* cell (sub1 dim))) 'outline 'white))}
            (place-image last-placed-stone
                         (PhysicalLoc-x-offset-from-left ploc)
                         (PhysicalLoc-y-offset-from-top ploc)
                         board-side)))

        (: draw-last-oppo-captured :
           Image-Color (Listof LogicalLoc) Image -> Image)
        (define (draw-last-oppo-captured color listlloc img)
          (foldr (lambda ([lloc : LogicalLoc] [i : Image])
                   (match (logical->physical lloc dim (World-spec w))
                     [(PhysicalLoc x y)
                      (place-image
                       (text "x" 20 color)
                       x y i)]))
                 img listlloc))

        (: draw-last-self-captured :
           Image-Color (Listof LogicalLoc) Image -> Image)
        (define (draw-last-self-captured color listlloc img)
          (local
            {(define board-side
               (square (+ (* 2 margin)
                          (* cell (sub1 dim))) 'outline 'white))}
            (foldr (lambda ([lloc : LogicalLoc] [i : Image])
                     (match (logical->physical lloc dim (World-spec w))
                       [(PhysicalLoc x y)
                        (place-image
                         (text "x" 20 color)
                         x y i)]))
                   img listlloc))) 
        
        (: draw-on-board : Stone Integer Integer Image -> Image)
        (define (draw-on-board s lx ly b)
          (cond
            [(= lx dim) b]
            [(= ly dim)
             (draw-on-board s (add1 lx) 0 b)]
            [else
             (match (board-ref (Go gob next his lp loc lsc cp)
                               (LogicalLoc lx ly))
               ['None (draw-on-board s lx (add1 ly) b)]
               [(Some s)
                (match (logical->physical
                        (LogicalLoc lx ly) dim (World-spec w))
                  [(PhysicalLoc x y)
                   (place-image (draw-stone s) x y
                                (draw-on-board s lx (add1 ly) b))])])]))}
       
       (beside/align
        "top"
        (above (overlay
                (match lp
                  ['None empty-image]
                  [(Some loc)
                   (draw-last-placed loc dim (World-spec w))])
                (match hover
                  ['None empty-image]
                  [(Some loc)
                   (match (legal-move? (World-game w) loc)
                     [#f empty-image]
                     [#t (draw-ghost-stone loc dim (World-spec w))])])
                
                (draw-last-oppo-captured next loc board-side)
                (draw-last-oppo-captured (adv next) lsc board-side)
                
                (draw-on-board 'white 0 0
                               (draw-on-board 'black 0 0 empty-board)))
               (column-labels w)
               msg-panel)
        (row-labels w)))]))

"eyeball tests: draw"
(draw (initial-world  8 'None))
(draw (initial-world  8 (Some (BoardSpec 'lightgreen 22 30 3))))
(draw (initial-world 10 'None))
(draw (initial-world 19 'None))

;; capstone -- return capitalized player color :-)
(define (capstone s)
  (match s
    ['black "Black"]
    ['white "White"]))

;; Place stone on click, if location is unoccupied.
(: react-to-mouse : World Integer Integer Mouse-Event -> World)
(define (react-to-mouse w x y e)
  (match w
    [(World spec (Go gob next his lp loc lsc cp) orig-msg btime wtime hover)
     (local
       {(define dim (vector-length gob))}
       (match e
         ["button-down"
          (match (physical->logical (PhysicalLoc x y) dim spec)
            ['None (World spec (Go gob next his lp loc lsc cp)
                          "This move is illegal." btime wtime hover)]
            [(Some lloc)
             (match (legal-move? (Go gob next his lp loc lsc cp) lloc)
               [#f (World spec (Go gob next his lp loc lsc cp)
                          "This move is illegal." btime wtime hover)]
               [#t (World spec
                          (apply-move (Go gob next his lp loc lsc cp) lloc)
                          (string-append (capstone next)
                                         " moved to "
                                         (logical->string lloc)
                                         ".")
                          btime wtime hover)])])]
         ["move"
          (World spec (Go gob next his lp loc lsc cp)
                 orig-msg btime wtime
                 (physical->logical (PhysicalLoc x y) dim spec))]
         [_ w]))]))

(check-expect
 (react-to-mouse
  (World
   (BoardSpec 'moccasin 30 22 8)
   (Go
    (vector
     (vector 'None (Some 'black) (Some 'black) 'None)
     (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
     (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
     (vector 'None (Some 'black) 'None 'None))
    'black '() 'None '() '() 0)
   "Welcome to go!"
   114 514 'None)
  112 52
  "button-down")
 (World (BoardSpec 'moccasin 30 22 8)
        (Go (vector (vector 'None (Some 'black) (Some 'black) 'None)
                    (vector (Some 'black) 'None 'None (Some 'black))
                    (vector (Some 'black) 'None 'None (Some 'black))
                    (vector 'None (Some 'black) (Some 'black) 'None))
            'white
            (list
             (vector (vector 'None (Some 'black) (Some 'black) 'None)
                     (vector (Some 'black) (Some 'white) (Some 'white)
                             (Some 'black))
                     (vector (Some 'black) (Some 'white) (Some 'white)
                             (Some 'black))
                     (vector 'None (Some 'black) 'None 'None)))
            (Some (LogicalLoc 3 2))
            (list (LogicalLoc 2 2) (LogicalLoc 1 2) (LogicalLoc 2 1)
                  (LogicalLoc 1 1)) '() 0)
        "Black moved to D3." 114 514 'None))

;; Pass on "p" or "P".
(: react-to-keyboard : World String -> World)
(define (react-to-keyboard w key)
  (match key
    [(or "P" "p") (pass/world w)]
    [(or "S" "s") (begin (save-game! w) w)]
    [(or "L" "l")
     (match (load-game (World-spec w))
       [(World _ (Go board _ _ _ _ _ _) _ _ _ _)
        (if
         (= (vector-length (Go-board (World-game w)))
            (vector-length board))
         (load-game (World-spec w))
         (error
          (string-append "The dimensions of the loaded game don't match "
                         "those of the already-running game.")))])]
    [_ w]))

(check-expect
 (react-to-keyboard
  (World
   (BoardSpec 'moccasin 30 22 8)
   (Go
    (vector
     (vector 'None (Some 'black) (Some 'black) 'None)
     (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
     (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
     (vector 'None (Some 'black) 'None 'None))
    'black '() 'None '() '() 0)
   "Welcome to go!"
   114 514 'None) "p")
 (World (BoardSpec 'moccasin 30 22 8)
        (Go (vector (vector 'None (Some 'black) (Some 'black) 'None)
                    (vector (Some 'black) (Some 'white) (Some 'white)
                            (Some 'black))
                    (vector (Some 'black) (Some 'white) (Some 'white)
                            (Some 'black))
                    (vector 'None (Some 'black) 'None 'None))
            'white (list (vector (vector 'None (Some 'black)
                                         (Some 'black) 'None)
                                 (vector (Some 'black) (Some 'white)
                                         (Some 'white) (Some 'black))
                                 (vector (Some 'black) (Some 'white)
                                         (Some 'white) (Some 'black))
                                 (vector 'None (Some 'black) 'None 'None)))
            'None '() '() 1) "Black passed." 114 514 'None))

;; Play the game given dimension and board spec.
;; Raise error if dimension is less than 2 or spec is invalid.
(: play : Integer BoardSpec -> World)
(define (play dim spec)
  (cond
    [(< dim 2) (error "dimension must be at least 2")]
    [(not (valid-board-spec? spec)) (error "invalid board spec")]
    [else (big-bang (initial-world dim (Some spec)) : World
            [to-draw draw]
            [on-key react-to-keyboard]
            [on-mouse react-to-mouse]
            [on-tick react-to-tick 1/10])]))

(play 19 (BoardSpec 'moccasin 30 22 8))
      
(test)