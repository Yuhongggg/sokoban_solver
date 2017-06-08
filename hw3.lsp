; Yuhong Wang
; UID: 004270644

;
; CS161 HW3: Sokoban
;
; *********************
;    READ THIS FIRST
; *********************
;
; All functions that you need to modify are marked with 'EXERCISE' in their
; header comments. This file also contains many helper functions. You may call
; any of them in your functions.
;
; Do not modify a-star.lsp.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any
; node. That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your
; heuristic functions.  Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on
; memory. So, it may crash on some hard sokoban problems and there is no easy
; fix (unless you buy Allegro). Of course, other versions of Lisp may also crash
; if the problem is too hard, but the amount of memory available will be
; relatively more relaxed. Improving the quality of the heuristic will mitigate
; this problem, as it will allow A* to solve hard problems with fewer node
; expansions. In either case, this limitation should not significantly affect
; your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition
; (which will affect your score).
;  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time.
;
(defun reload ()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star ()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all ()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
(defun goal-test (s)
  (cond ((null s) t); return true, if every row has been tested
  		((atom s) (not (isBox s))); return false if a box is detected
  		(t (and (goal-test (car s)) (goal-test (cdr s))))
  );end cond
);end defun

; EXERCISE: Modify this function to return the list of
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
;
; If you want to use it, you will need to set 'result' to be
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
;
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
;

; helper function for get-square 
(defun get-column (s col)
	(cond ((null s) wall)
		  ((< col 0) wall); return wall for negetiva col number
		  ((= col 0) (car s))
		  (t (get-column (cdr s) (- col 1)))); end cond
);end defun

; return the integer content of state S at square (r,c)
(defun get-square (s row col)
	(cond ((null s) wall)
		  ((< row 0) wall);return wall for negative row number
		  ((= row 0) (get-column (car s) col))
		  (t (get-square (cdr s) (- row 1) col)));end cond
);end defun

; helper function for set-square
(defun set-column (s col n)
	(cond ((null s) nil)
		  ((null col) nil)
		  ((< col 0) nil)
		  ((= col 0) (cons n (cdr s)))
		  (t (cons (car s) (set-column (cdr s) (- col 1) n)))
	);end cond
);end defun


; set the (c,r) square in s to n.
; return nil for invalid coordinate number
(defun set-square (s row col n)
	(cond ((null s) nil)
		  ((null row) nil)
		  ((null col) nil)
		  ((< row 0) nil)
		  ((= row 0) (append (list (set-column (car s) col n)) 
		  					 (cdr s)))
		  (t (append (list (car s)) 
		  	  	 	 (set-square (cdr s) (- row 1) col n)))
	);end cond
);end defun

; helper function for try-move
(defun get-next-position (row col dir)
	(cond ((null dir) nil)
		  ((null col) nil)
		  ((null row) nil)
		  ((equal dir 'UP) (list (- row 1) col))
		  ((equal dir 'DOWN) (list (+ row 1) col))
		  ((equal dir 'LEFT) (list row (- col 1)))
		  ((equal dir 'RIGHT) (list row (+ col 1)))
	);end cond; set the next position (row, col) after move
);end defun

; helper function
; If box at (row, col) could be move to dir by one square, then move it. else return nil.
(defun move-box (s row col dir)
	(let* ((next-pos (get-next-position row col dir))
		   (next-row (car next-pos))
		   (next-col (cadr next-pos))
		   (square (get-square s next-row next-col))
		   ); end initialization
		(cond ((null square) nil)
			  ((isBlank square) (set-square (restore-a-square s row col) next-row next-col box))
			  ((isStar square) (set-square (restore-a-square s row col) next-row next-col boxstar))
			  (t nil)
		);end cond
	);end let
);end defun

; helper function;
; restore a square,
; if a square is a keeper or a box, this function will change it to blank,
; if a square is a keeperstar or boxstar, this function will change it to a star.
(defun restore-a-square (s row col)
	(let ((square (get-square s row col))
		 ); end initialization
		(cond ((null square) nil)
			  ((or (isBox square) (isKeeper square)) 
					(set-square s row col blank))
				;if current square is a box of a keeper, then change it to blank
			  ((or (isBoxStar square) (isKeeperStar square)) 
			  		(set-square s row col star))
			  	;if current square is a boxStart or a boxKeeper, then change it to a star
		);end cond
	);end let
);end defun


(defun try-move (s dir)
	(let* ((pos (getKeeperPosition s 0))
		  (col (car pos))
		  (row (cadr pos))
		  (next-pos (get-next-position row col dir))
		  (next-row (car next-pos))
		  (next-col (cadr next-pos))
		  (square (get-square s next-row next-col))
		  ); end initialization
			;pos is the next position (row, col). square is the contenet at next position

		(cond ((null square) nil)
			  ((isWall square) nil);return nil if next position is wall
			  ((isBlank square) (set-square (restore-a-square s row col) next-row next-col keeper)
			  );end isBlank section. set the next square to keeper and restore the current square
			  ((isBox square)
			  			(set-square (restore-a-square (move-box s next-row next-col dir) 
			  										  row col);end restore-a-square 
			  						next-row next-col keeper);end set-square
			  );end isBox section.
			  ((isBoxStar square)
			  			(set-square (restore-a-square (move-box s next-row next-col dir) 
			  										  row col);end restore-a-square 
			  						next-row next-col keeperstar);end set-square
			  );end isBoxStar section.
			  ((isStar square) (set-square (restore-a-square s row col) next-row next-col keeperstar)
			  );end isStar section. 
		);end cond
	);end let
);end defun


(defun next-states (s)
  (let*  ((result (list (try-move s 'UP) (try-move s 'RIGHT) (try-move s 'DOWN) (try-move s 'LEFT))))
	    (cleanUpList result);end
   );end let
); end defun

; EXERCISE: Modify this function to compute the trivial
; admissible heuristic.
;
(defun h0 (s)
	0
  )

; EXERCISE: Modify this function to compute the
; number of misplaced boxes in s.
; This heuristic is admissible, because if there are n misplaced boxes, we need to push at least n times
; to clean all misplaces boxes. Therefore, the heuristic will not overestimate the cost to
; reach the goal state from current state. At the same time, the less misplaced boxes the closer we are 
; to the goal state.

(defun h1 (s)
	(cond ((null s) 0)
		  ((atom s) (if (isBox s) 1 0))
		  (t (+ (h1 (car s)) (h1 (cdr s)))));end cond
)

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this
; function to compute an admissible heuristic value of s.
;
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the
; running time of a function call.
;

; I tried two ways to find my heuristic:
; the first one is h2, which calculated the sum of manhattan distance from all boxes to their nearest goal, ignoring the walls.
; It is obviously admissible.
; the second one is h3. In h3, I used a BFS to search for the length of closest path from boxes to goals.
; this heuristic decreased a large amount of nodes to be expanded, but the BFS itself also take a large amount of time.
; So finally, i decided to use h2 only.
; And both h2 and h3 calculated the deadlocks. I only checked simple deadlocks where boxes are at the corners. If any deadlocks
; detected, the heuristic function will return a number larger than 4000.
(defun h004270644 (s)
	(h2 s)
); end defun

; helper function:
; return a list contains the position of all boxes.
(defun get-boxes (s row col width height)
	(cond ((null s) nil)
		  ((= col width) (get-boxes (rest s) (+ row 1) 0 width height))
		  ((isBox (car (car s))) 
		  	; if the first is box or boxstar, append it to the return list
		  		(append (list (list row col)) 
							 (get-boxes (append (list (rest (car s))) (rest s)) 
		  								row (+ col 1) width height); end get-box recursive call
		  		); end append
		  ); end isBox
		  	;default, if the first is not a box or boxstar, call with the rest of the list
		  (t (get-boxes (append (list (rest (car s))) (rest s)) row (+ col 1) width height))
	); end cond
); end defun


; helper function:
; return a list contains the position of all goals.
(defun get-goals (s row col width height)
	(cond ((null s) nil)
		  ((= col width) (get-goals (rest s) (+ row 1) 0 width height))
		  ((or (isStar (car (car s))) (isBoxStar (car (car s))) (isKeeperStar (car (car s)))) 
		  	; if the first is goal of any kind, append it to the return list
		  		(append (list (list row col)) 
							 (get-goals (append (list (rest (car s))) (rest s)) 
		  								row (+ col 1) width height); end get-box recursive call
		  		); end append
		  ); end isBox
		  	;default, if the first is not a box or boxstar, call with the rest of the list
		  (t (get-goals (append (list (rest (car s))) (rest s)) row (+ col 1) width height))
	); end cond
); end defun


;calculated the sum of manhattan distance from boxes to nearest goals.
(defun h2 (s)
	(let ((boxes (get-boxes s 0 0 (length (car s)) (length s)))
		  (goals (get-goals s 0 0 (length (car s)) (length s)))
		  (player (list (cadr (getKeeperPosition s 0)) (car (getKeeperPosition s 0))))
		 );end initialization
		;(+ (sum-pushes s boxes) (dead-lock s boxes))
		(+ (sum-distance boxes goals)  (dead-lock s boxes))
	); end let
); end defun


; if a box is push to the corner, return 5000
(defun dead-lock (s boxes)
	(cond ((null boxes) 0)
		  (t (let* ((row (car (car boxes)))
		 	  		(col (second (car boxes)))
		 	  		(up-square (get-square s (- row 1) col))
		 	  		(down-square (get-square s (+ row 1) col))
		 	  		(left-square (get-square s row (- col 1)))
		 	  		(right-square (get-square s row (+ col 1)))
		   		   ); end initialization					
					(cond ((and (or (isWall up-square) (isWall down-square))
							    (or (isWall left-square) (isWall right-square)))
			  					   4000
			  			  ); if box get to a corner, return 4000
			  			  (t (dead-lock s (rest boxes)))
			  		);end cond
			  );end let
		  ); end t
	); end cond
); end defun


(defun min-distance (box goals)
	(cond ((null goals) 4000)
		  (t (let  ((distance (+ (abs (- (car box) (car (car goals)))) 
			  				     (abs (- (second box) (second (car goals))))))
		  		   ); end initialization
		  		(min distance (min-distance box (rest goals)))
		  	 ); end let
		  )
	); end cond
); end defun


(defun sum-distance (boxes goals)
		(cond ((null boxes) 0)	
			  (t (+ (min-distance (car boxes) goals)
			  		(sum-distance (cdr boxes) goals)))
	); end cond
); end defun


; this heuristic function used BFS.
(defun h3 (s)
	(let ((boxes (get-boxes s 0 0 (length (car s)) (length s)))
		  (goals (get-goals s 0 0 (length (car s)) (length s)))
		  (player (list (cadr (getKeeperPosition s 0)) (car (getKeeperPosition s 0))))
		 );end initialization
		(+ (sum-pushes s boxes) (dead-lock s boxes))
	); end let
); end defun

; This is a BFS, which calculates the min distance of the path from box to goal
(defun BFS (s box)
	;(printstate s)
	(cond ((null box) 0)
		  (t (let* ((row (caar box))
		  			(col (cadar box))
		  			(score (caddar box))
		  			; generating the children
		  			(up (append (get-next-position row col 'UP) (list (+ score 1))))
		  			(down (append (get-next-position row col 'DOWN) (list (+ score 1))))
		  			(right (append (get-next-position row col 'RIGHT) (list (+ score 1))))
		  			(left (append (get-next-position row col 'LEFT) (list (+ score 1))))
		  			(square (get-square s row col))
		  			);end initialization
		  			;(print box)
		  		(cond ((or (isStar square) (isBoxStar square) (isKeeperStar square)) score)
		  			  ((isWall square) (BFS s (cdr box)))
		  			  (t (BFS (set-square s row col wall)
		  			  			(append (cdr box) (list up) (list down) (list right) (list left))
		  			  	 )
		  			  )
		  		);end cond
		  	 );end let
		  );end t
	);end cond
); end defun

; sum of min-pushes of all boxes calculated by BFS
(defun sum-pushes (s boxes)
	(cond ((null boxes) 0)
		  (t (+ (BFS s (list (append (car boxes) (list 0))))
		  	    (sum-pushes s (cdr boxes)))
		  )
	); end cond
); end defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.  Each problem can be visualized by calling
 | (printstate <problem>).  Problems are ordered roughly by their difficulties.
 | For most problems, we also privide 2 additional number per problem: 1) # of
 | nodes expanded by A* using our next-states and h0 heuristic.  2) the depth of
 | the optimal solution.  These numbers are located at the comments of the
 | problems. For example, the first problem below was solved by 80 nodes
 | expansion of A* and its optimal solution depth is 7.
 |
 | Your implementation may not result in the same number of nodes expanded, but
 | it should probably give something in the same ballpark. Allegros for the solution
 | depth, any admissible heuristic must make A* return an optimal solution. So,
 | the depths of the optimal solutions provided could be used for checking
 | whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible
 | to solve without a good heuristic!
 |
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
