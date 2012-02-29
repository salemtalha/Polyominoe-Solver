#lang racket
(require "a10.rkt")

;;Contract: flatten: lstofAny -> lstofAny
;;Purpose: Takes a list and "flattens" it to a depth of 1
;;Examples:
(check-expect (flatten (list (list 1 2 3))) (list 1 2 3))
(check-expect (flatten (list 1 2 (list 3 4 5))) (list 1 2 3 4 5))

(define (flatten lst)
           (cond [(empty? lst) empty]
                 [(cons? (first lst)) 
                  (append (first lst) (flatten (rest lst)))]
                 [else
                  (cons (first lst) (flatten (rest lst)))]))

;;Tests:
(check-expect (flatten empty) empty)
(check-expect (flatten (list (list 1) (list 2) (list 3))) (list 1 2 3))

;; A Grid is a (ne-listof (ne-listof Character))

;; A Pos is a (make-pos Int Int)
(define-struct pos (x y))

;; A State is a (make-state Grid (listof Grid))
(define-struct state (puzzle pieces))

;;Contract: build-2dlist: Nat Nat (X X -> Y)
;;Purpose: Takes a width and height and creates an array
;;consisting of a function applied to the corresponding 
;;coordinates of each item
;;Examples:
(check-expect (build-2dlist 3 2 +) (list (list 0 1 2) (list 1 2 3)))
(check-expect (build-2dlist 3 2 -) (list (list 0 1 2) (list -1 0 1)))

(define (build-2dlist width height f)
  (build-list height (lambda (y)
                       (build-list width (lambda (x) (f x y))))))

(check-expect (build-2dlist 1 1 *) (list (list 0)))
;;;;;;;;;;;;;;;;;;
;;Contract: all-positions: Nat Nat -> listofPos
;;Purpose: Takes a width and height and outputs the 
;;coordinates of a w x h array.
;;Examples:
(check-expect (all-positions 1 1) (list (make-pos 0 0)))
(check-expect (all-positions 0 0) empty)

(define (all-positions w h)
  (local ([define posns (build-2dlist w h (lambda (x y) (make-pos x y)))])
    (flatten posns)))

;;Tests:
(check-expect (all-positions 3 2) (list (make-pos 0 0) (make-pos 1 0) (make-pos 2 0)
                                        (make-pos 0 1) (make-pos 1 1) (make-pos 2 1)))

;;;;;;;;;;;;;;;;;;

;;Contract: get-column: Grid -> listofAny
;;Purpose: Outputs the first item of every column in the grid
;;Examples:
(check-expect (get-column (list (list 1 2 3) (list 4 5 6) (list 7 8 9))) (list 1 4 7))
(check-expect (get-column (list (list 1))) (list 1))

(define (get-column grid)
  (foldr (lambda (x y) (cons (first x) y)) empty grid))

;;Tests:
(check-expect (get-column (list (list 1) (list 2) (list 3))) (list 1 2 3))
(check-expect (get-column (list (list "great" 2 4) (list "wow" 4 5) (list "phew" 3))) (list "great" "wow" "phew"))

;;Contract: rm-col: Grid -> Grid
;;Purpose: Removes a column from a Grid
;;Examples:
(check-expect (rm-col (list (list 1 2 3)
                            (list 4 5 6)
                            (list 6 7 8)))
              (list (list 2 3)
                    (list 5 6)
                    (list 7 8)))
(define (rm-col grid)
  (map (lambda (x) (rest x)) grid))

;;Tests:

;;Contract: rotate: Grid -> Grid
;;Purpose: Takes a grid and rotates it 90 degrees
;;Examples:
(check-expect (rotate (list (list #\.))) (list (list #\.)))

(define (rotate grid)
  (cond [(empty? (first grid)) empty]
        [else
         (cons (get-column (reverse grid))
               (rotate (rm-col grid)))]))
;;Tests:
(check-expect (rotate (list (list #\q #\q #\q)
                            (list #\. #\q #\.)
                            (list #\. #\q #\.))) (list (list #\. #\. #\q)
                                                       (list #\q #\q #\q)
                                                       (list #\. #\. #\q)))

;;Contract: remove-duplicates: listofAny -> listofAny
;;Purpose: Takes a list of items and outputs a list of the unique elements
;;Examples:
(check-expect (remove-duplicates (list 1 2 2)) (list 1 2))
(check-expect (remove-duplicates (list 1)) (list 1))

(define (remove-duplicates log)
  (foldr (lambda (x y) (cons x (filter (lambda (z) (not (equal? x z))) y))) empty log))

;;Tests:
(check-expect (remove-duplicates (list 1 2 3 1 2 3)) (list 1 2 3))
(check-expect (remove-duplicates (list 1 3 2 3)) (list 1 3 2))


;;Contract: all-orientations: Grid -> listofGrid
;;Purpose: Takes in a grid and outputs all unique rotations and 
;;reflections thereof
;;Examples:
(check-expect (all-orientations (list (list 1 1)
                                      (list 1 1))) (list (list (list 1 1)
                                                               (list 1 1))))
(check-expect (all-orientations (list (list 1 1)
                                      (list 1 1)
                                      (list 1 1)))
              (list (list (list 1 1)
                          (list 1 1)
                          (list 1 1)) (list (list 1 1 1)
                                            (list 1 1 1))))

(define (all-orientations grid)
  (local [(define (all-configs grid counter)
            (cond [(= counter 4) empty]
                  [else
                   (cons grid (all-configs (rotate grid) (add1 counter)))]))
          (define first4rots (all-configs grid 0))
          (define scnd4rots (all-configs (reverse grid) 0))]
  (remove-duplicates (append first4rots scnd4rots))))

;;Tests:
(check-expect (all-orientations (list (list #\. #\q)
                                      (list #\. #\q)))
              (list
               (list (list #\. #\q) (list #\. #\q))
               (list (list #\. #\.) (list #\q #\q))
               (list (list #\q #\.) (list #\q #\.))
               (list (list #\q #\q) (list #\. #\.))))

;;;;;;;;;;;;;;;;;;;;
;;Contract: first-empty-pos: Grid -> (union pos empty)
;;Purpose: Takes a grid and outputs the position of the first empty
;;item in the grid.
;;Examples:
(check-expect (first-empty-pos (list (list #\. #\. #\.))) (make-pos 0 0))
(check-expect (first-empty-pos (list (list #\q #\q #\q)
                                     (list #\q #\. #\q))) (make-pos 1 1))
                                           
(define (first-empty-pos grid)
  (local [(define lst (flatten grid))
           (define width (length (first grid)))
           (define height (length grid))
           (define posns (all-positions width height))
           (define (compare lst posns)
             (cond [(empty? lst) false]
                   [(equal? (first lst) #\.)
                    (first posns)]
                   [else
                    (compare (rest lst) (rest posns))]))]
    (compare lst posns)))

;;Tests:
(check-expect (first-empty-pos (list (list 1 1 1)
                                     (list 2 2 2)
                                     (list 3 3 #\.))) (make-pos 2 2))

;;;;;;;;;;;;;;;;;;;;;;;
;;Contract: add-row: Grid -> Grid
;;Purpose: Takes a grid and adds an empty row to it
;;Examples:
(check-expect (add-row (list (list 1 2 3) (list 4 5 6)))
              (list (list #\. #\. #\.)
                    (list 1 2 3)
                    (list 4 5 6)))
(define (add-row grid)
  (cons (build-list (length (first grid))  (lambda (x) #\.)) grid))

;;Tests:
(check-expect (add-row (list (list 1 1 1 1)))
              (list (list #\. #\. #\. #\.)
                    (list 1 1 1 1)))
(check-expect (add-row (list (list #\q)))
              (list (list #\.) (list #\q)))

;;Contract: rm-row: Grid -> Grid
;;Purpose: Takes a Grid and removes a row from it
;;Examples:
(check-expect (rm-row (list (list 1 2 3))) empty)

(define (rm-row grid) (rest grid))

;;Tests:
(check-expect (rm-row (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
              (list (list 4 5 6) (list 7 8 9)))

;;Contract: add-col Grid -> Grid
;;Purpose: Takes a Grid and adds an empty Column to it
;;Examples:
(check-expect (add-col (list (list 1 2 3) 
                             (list 4 5 6)
                             (list 7 8 9)))
              (list (list #\. 1 2 3)
                    (list #\. 4 5 6)
                    (list #\. 7 8 9)))
                         
(define (add-col grid)
  (map (lambda (x) (cons #\. x)) grid))

;;Tests:
(check-expect (add-col (list (list 1 2))) (list (list #\. 1 2)))
(check-expect (add-col (list (list 1) (list 2) (list 3)))
              (list (list #\. 1)
                    (list #\. 2)
                    (list #\. 3)))

;;Contract: translate: Grid Pos -> Grid
;;Purpose: Translates a grid by given coordinates
;;Examples:
(check-expect (translate (list (list #\q #\q)) (make-pos 0 0))
              (list (list #\q #\q)))
(check-expect (translate (list (list #\a #\a)) (make-pos 1 1))
              (list (list #\. #\. #\.)
                    (list #\. #\a #\a)))

(define (translate grid pos)
  (local [(define x (pos-x pos))
          (define y (pos-y pos))
          (define (shift grid f counter result)
            (cond [(= counter 0) result]
                  [else
                   (shift (f grid) f (sub1 counter) (f grid))]))
          (define xshift (shift grid (cond [(< x 0) rm-col]
                                           [else add-col]) (abs x) grid))]
    (shift xshift (cond [(< y 0) rm-row]
                        [else add-row]) (abs y) xshift)))

;;Tests:
(check-expect (translate (list (list #\a #\a)) (make-pos -1 0))
              (list (list #\a)))
                        
;;Contract: compare-row: listofChar listofChar -> listofChar
;;Purpose: Takes two rows to be compared and outputs the result of 
;;"overlaying" one row over the other.
;;Examples:
(check-expect (compare-row (list #\. #\. #\.) (list #\q #\w #\s))
              (list #\q #\w #\s))
(check-expect (compare-row (list #\. #\.) (list #\. #\.))
              (list #\. #\.))

(define (compare-row baserow toprow)
  (cond [(empty? baserow) empty]
        [(empty? toprow)
         (cons (first baserow) (compare-row (rest baserow) empty))]
        [(equal? (first toprow) #\.)
         (cons (first baserow)
               (compare-row (rest baserow) (rest toprow)))]
        [else
         (cons (first toprow)
               (compare-row (rest baserow) (rest toprow)))]))

;;Examples:
(check-expect (compare-row (list #\q) (list #\.))
              (list #\q))

;;Contract: superimpose: Grid Grid Pos -> Grid
;;Purpose: Takes a base grid and another grid with which 
;;it shifts by pos and then overlays it on the base grid
;;Examples:
(check-expect (superimpose (list (list #\a #\.)) (list (list #\a)) (make-pos 1 0))
              (list (list #\a #\a)))

(define (superimpose base top pos)
  (local [(define shftdtop (translate top pos))
          (define (impose base top)
            (cond [(empty? base) empty]
                  [(empty? top) base]
                  [else
                   (cons (compare-row (first base) (first top))
                         (impose (rest base) (rest top)))]))]
    (impose base shftdtop)))

;;Tests:
(check-expect (superimpose (list (list #\y #\. #\. #\. #\.)
                                 (list #\y #\y #\y #\. #\.)
                                 (list #\. #\y #\. #\. #\.))
                           (list (list #\p #\p)
                                 (list #\p #\p)
                                 (list #\p #\.)) (make-pos 0 0))
              (list (list #\p #\p #\. #\. #\.)
                    (list #\p #\p #\y #\. #\.)
                    (list #\p #\y #\. #\. #\.)))

(check-expect (superimpose (list (list #\y #\. #\. #\. #\.)
                                 (list #\y #\y #\y #\. #\.)
                                 (list #\. #\y #\. #\. #\.)) 
                           (list (list #\p #\p)
                                 (list #\p #\p)
                                 (list #\p #\.)) (make-pos 4 1))
              (list (list #\y #\. #\. #\. #\.)
                    (list #\y #\y #\y #\. #\p)
                    (list #\. #\y #\. #\. #\p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Contract: can-impose?: Grid Grid Pos -> Boolean
;;Purpose: Determines whether or not a superposition will be legal or not.
;;this means all squares lie on a previously empty, in bounds position and that
;;one of the squares occupied is the first empty square of the base
;;Examples:
(check-expect (can-impose? (list (list #\q)) (list (list #\y)) (make-pos 0 0))
              false)
(check-expect (can-impose? (list (list #\.)) (list (list #\q)) (make-pos 0 0))
              true)

(define (can-impose? base top pos)
  (local [(define firstempty (first-empty-pos base))
          (define (numempty grid)
            (foldr (lambda (a b) 
                     (+ (foldr (lambda (x y) (+ (cond [(equal? x #\.) 1]
                                                      [else 0]) y)) 0 a) b)) 0 grid))
          (define initempty (numempty base))
          (define filledtop (- (* (length (first top)) (length top)) (numempty top)))
          (define superposition (superimpose base top pos))]
    (cond [(and 
            (= (- initempty filledtop) (numempty superposition))
            (not (equal? firstempty (first-empty-pos superposition))))
             true]
          [else false])))
;;Tests
(check-expect (can-impose? (list (list #\. #\.)
                                 (list #\. #\p))
                           (list (list #\q #\q)
                                 (list #\q #\.)) (make-pos 0 0)) true)


;;Contract: legal-offsets: Grid Grid -> listofPos
;;Purpose: Takes a puzzle and a piece and outputs the list of 
;;translations that would still yield a legal superimposition
;;Examples: 
(check-expect (legal-offsets (list (list #\. #\.)
                                   (list #\. #\.)) (list (list #\q #\q)))
              (list (make-pos 0 0)))
(check-expect (legal-offsets (list (list #\. #\. #\.)) (list (list #\q)))
              (list (make-pos 0 0)))
                                   
                                                         
               
(define (legal-offsets base top)
  (local [(define posns (all-positions (length (first base)) (length base)))
          (define (imposes base top posns)
            (cond [(empty? posns) empty]
                  [(can-impose? base top (first posns))
                   (cons (first posns)
                         (imposes base top (rest posns)))]
                  [else
                   (imposes base top (rest posns))]))]
    (imposes base top posns)))

;;Tests:
(check-expect (legal-offsets (list (list #\a #\q #\.)) (list (list #\v))) (list (make-pos 2 0)))
(check-expect (legal-offsets (list (list #\a #\a)
                                   (list #\a #\.)) (list (list #\q))) (list (make-pos 1 1)))

;;Contract: single-orient: State Grid Grid -> (listofState)
;;Purpose: Outputs the result of trying every offset of a given piece at a given orientation on 
;;the given State
;;Examples:
(check-expect (single-orient (make-state (list (list #\. #\.)
                                              (list #\. #\.)) (list (list (list #\q)))) (list (list (list #\q))) (list (list (list #\q))))
              (list (make-state (list (list #\q #\.)
                                      (list #\. #\.)))))
              
(define (single-orient s top piece)
  (local [(define offsets (legal-offsets (state-puzzle s) top))
          (define (orient base top offsets)        
            (cond [(empty? offsets) empty]
                  [else
                   (cons (make-state (superimpose base top (first offsets)) (remove piece (state-pieces s))) 
                         (orient base top (rest offsets)))]))]
    (orient (state-puzzle s) top offsets)))


;;Tests:
(check-expect (single-orient (make-state (list (list #\. #\q #\.)
                                               (list #\. #\q #\.)
                                               (list #\. #\q #\.)) (list (list (list #\y)
                                                                               (list #\y)
                                                                               (list #\y)))) (list (list #\y)
                                                                               (list #\y)
                                                                               (list #\y)) (list (list #\y)
                                                                               (list #\y)
                                                                               (list #\y)))
              (list (make-state (list (list #\y #\q #\.) (list #\y #\q #\.) (list #\y #\q #\.)) empty)))
              
;;Contract: all-orient: State Grid -> listofState
;;Purpose: Outputs all legal moves for all rotations and translations 
;;of the given piece
;;Examples:
(check-expect (all-orient (make-state (list (list #\. #\. #\.)
                                            (list #\. #\q #\.)
                                            (list #\. #\q #\.)) (list (list (list #\y)
                                                                            (list #\y)
                                                                            (list #\y)))) (list (list #\y)
                                                                                                (list #\y)
                                                                                                (list #\y)))(list
 (make-state (list (list #\y #\. #\.) (list #\y #\q #\.) (list #\y #\q #\.)) empty)
 (make-state (list (list #\y #\y #\y) (list #\. #\q #\.) (list #\. #\q #\.)) empty)))
              

(define (all-orient s piece)
  (local [(define orients (all-orientations piece))
          (define (orient-all base orients)                  
            (cond [(empty? orients) empty]
                  [else
                   (append (single-orient s (first orients) piece)
                           (orient-all base (rest orients)))]))]
    (orient-all (state-puzzle s) orients)))

;;Tests:
(check-expect (all-orient (make-state (list (list #\. #\q #\.)
                              (list #\. #\q #\.)
                              (list #\. #\q #\.)) (list (list (list #\y)
                                                              (list #\y)
                                                              (list #\y)))) (list (list #\y)
                                                                                  (list #\y)
                                                                                  (list #\y)))
              (list (make-state (list (list #\y #\q #\.) (list #\y #\q #\.) (list #\y #\q #\.)) empty)))
              
            
                               
    
;;Contract: State -> listofState
;;Purpose: Takes in an initial state and returns all possible neighbors which yield a valid 
;;superimposition, this includes all reflections and translations of each piece.
(define (neighbours s)
  (local [(define perms (state-pieces s))
          (define (neighbor s perms)
                   (cond [(empty? perms) empty]
                         [else
                          (append (all-orient s (first perms))
                                  (neighbor s (rest perms)))]))]
    (neighbor s perms)))

;; solve-puzzle: Grid (listof Grid) Symbol -> (union (listof String) false)
;; Solve a polyomino puzzle, given the initial empty (or partially filled 
;; in) grid, a set of pieces that must be placed, and a Symbol indicating
;; what visualization style to use.  Legal viz styles are 'interactive
;; (draw every step of the search), 'at-end (just draw the solution, if one
;; is found), or 'offline (don't draw anything).  Produce either the solved
;; Grid (converted to a list of Strings, just for convenience) or false if
;; no solution exists.
;;
;; You don't need to modify this function at all.  It is provided for you
;; so that you can test your puzzle solving algorithm interactively.  If
;; you decide you want to write check-expect tests using solve-puzzle
;; (which you don't have to do, but can if you want), be sure to pass in
;; 'offline for viz-style.

;; Examples:
;; (The examples are not provided in check-expect form.  They're meant to
;; demonstrate typical uses of the function, but we don't want to them to
;; open interactive visualizations every time you start the program.)

;; Solve offline (i.e. work like a normal Scheme function).
;(solve-puzzle
;  (strlist->grid '("...." "...." "...." "...." "...." "...."))
;  (cons '((#\L #\L) (#\. #\L)) (cons '((#\O)) tetrominoes-uc))
;  'offline)

;; Display the result graphically, if a solution is found.
;(solve-puzzle
;  (strlist->grid '("...." "...." "...." "...." "...." "...."))
;  (cons '((#\L #\L) (#\. #\L)) (cons '((#\O)) tetrominoes-uc))
;  'at-end)

;; Display every step of the search as it progresses.
;(solve-puzzle
;  (strlist->grid '("...." "...." "...." "...." "...." "...."))
;  (cons '((#\L #\L) (#\. #\L)) (cons '((#\O)) tetrominoes-uc))
;  'interactive)

(define (solve-puzzle grid polys viz-style)
  (local
    [(define result
       (search 
        (lambda (S) (empty? (state-pieces S)))
        neighbours
        (cond
          [(symbol=? viz-style 'interactive)
           (lambda (S) (draw-grid (state-puzzle S)))]
          [else false])
        (make-state grid polys)))
     
     (define maybe-last-draw
       (cond
         [(and (state? result)
               (symbol=? viz-style 'at-end))
          (draw-grid (state-puzzle result))]
         [else false]))]
    (cond
      [(boolean? result) result]
      [else (map list->string (state-puzzle result))]))) 
