

(load "lp.l")
;;=========================GLOBAL-USE VARIABLES================================
(defvar row '(A B C D E F G H I J K L M N O P Q R S))
(defvar column '(A B C D E F G H I J K L M N O P Q R S))
(setf row-starters '(19 38 57 76 95 114 133 152 171 190 209 228 247 266 285 304 323 342 361))
(setf (symbol-plist 'coordinates)'(A 0 B 1 C 2 D 3 E 4 F 5 G 6 H 7 I 8 J 9 K 10 L 11 M 12 N 13 O 14 P 15 Q 16 R 17 S 18))
(defvar limit-check '(15 16 17 18 34 35 36 37 53 54 55 56 72 73 74 75 91 92 93 94 110 111 112 113 129 130 131 132 148 149 150 151 167 168 169 170 186 187 188 189 205 206 207 208 224 225 226 227 243 244 245 246 262 263 264 265 281 282 283 284 300 301 302 303 319 320 321 322 338 339 340 341 357 358 359 360))
(defvar limit-check-2 '(0 1 2 3 19 20 21 22 38 39 40 41 57 58 59 60 76 77 78 79 95 96 97 98 114 115 116 117 133 134 135 136 152 153 154 155 171 172 173 174 190 191 192 193 209 210 211 212 228 229 230 231 247 248 249 250 266 267 268 269 285 286 287 288 304 305 306 307 323 324 325 326 342 343 344 345))
(defmacro timing (&body forms)
    (let ((real1 (gensym))
	    (real2 (gensym))
	    (run1 (gensym))
	    (run2 (gensym))
	    (result (gensym)))
    `(let* ((,real1 (get-internal-real-time))
	      (,run1 (get-internal-run-time))
	      (,result (progn ,@forms))
	      (,run2 (get-internal-run-time))
	      (,real2 (get-internal-real-time)))
	 (format *debug-io* ";;; Computation took:~%")
	 (format *debug-io* ";;;  ~f seconds of real time~%"
		 (/ (- ,real2 ,real1) internal-time-units-per-second))
	 (format t ";;;  ~f seconds of run time~%"
		 (/ (- ,run2 ,run1) internal-time-units-per-second))
	 ,result)))
;;=========================Task 1: board setup=================================

;;making an empty board, in the form of a loooong list (19*19 = 361)
(defmethod make-new-board (&aux boardList x y)
  (setf boardList ())
    (dotimes (x (length row))
      (dotimes (y (length column))
	(setf boardList (snoc '- boardList))
	)
      )
   ; (generate-initial-movelist)
     boardList
  )
(defmethod generate-movelist()
  (generate-initial-movelist)
  )

;;making a 2D array board
(defmethod new-board (&aux board)
  (setf board (make-array '(19 19)))
  )


;;displaying/visualizing the board with labels 
(defmethod display-b ((board list)&aux x)
  (format t "~%")
  (dolist (label column)
    (format t "~A " label)
    )
  (format t "~%~%")
  (dotimes (x (length board))
    (if (member x row-starters)
	(format t  " ~A ~%~A " (nth(- ( / x 19) 1) row) (nth x board))
      (format t "~A " (nth x board))
    )
    )
  (format t " ~A~%" 'S)
  )

;;==========================Task2: Putting Down Pieces=========================
;;needs revision
(defmethod put ((board list) player-symbol rowpos columnpos &aux xIndex yIndex)

  (if (eq (out-of-bound board rowpos columnpos)nil)
      (let ()
	(if (empty board rowpos columnpos)
	    (let ()
	      (setf xIndex (get 'coordinates rowpos))
	      (setf yIndex (get 'coordinates columnpos))
	      (setf listIndex (+ (* xIndex 19) yIndex))
	     
	      (setf pair (cons rowpos columnpos))
	     ; (setf *move-list* (remove pair *move-list*))
	      
	      (setf board (rep-elt board listIndex player-symbol ))
	      )
	  (let()
	    (format t "A piece already exist at this position.")
	    (setf board board)))    
	)
    (let ()
      (format t "Invalid position.")
      (setf board board)
      )
    )
  )
	      
    
 
  

;;assuming the given position is NOT out-of-bound, check whether it is epty
  (defun empty (board  rowpos columnpos)
    (eq (get-piece board rowpos columnpos) '-)    
    )
;;check whether a given position is out of bound
  (defun out-of-bound (board rowpos columnpos)
    (or (eq (member rowpos row) nil)(eq (member columnpos column) nil))
    )
     

;;get the symbol that is at the given position
(defmethod get-piece ((board list) row column &aux xIndex yIndex symbol)
  (setf xIndex (get 'coordinates row))
  (setf yIndex (get 'coordinates column))
  (setf listIndex (+ (* xIndex 19) yIndex))
  (setf symbol '-)
  (if
      (not (eq (nth listIndex board) '-))
      (setf symbol (nth listIndex board))
    )
  symbol
  )

;;replacing an element in a list in an non-destructive way.
(defun rep-elt (list &optional n new)
  (cond ((null n)
	 list)
	((> n 0)
	 (cons (car list)(rep-elt (cdr list)(1- n)new)))
	((eq n 0)
	 (cons new (cdr list))
	 )
	)
  )

(defmethod task2demo ()
  (format t "~%Making a new empty board and displaying it:~%")
  (setf b (make-new-board))
  (display-b b)
  (format t "~%Putting down a X piece at position 'A 'A~%")
  (setf b (put b 'X 'A 'A))
  (display-b b)
  (format t "~%Putting down ab O piece at position 'S 'S ~%")
  (setf b (put b 'O 'S 'S))
  (display-b b)
  (format t "~%Trying to put down a X piece at the position where the O piece already exist -- should fail and board state does not change ~%")
  (setf b (put b 'X 'S 'S))
  (display-b b)
  (format t "~%Trying to put a piece at position 'X 'X which is out of bound. Board state should not change:~%")
  (setf b (put b 'S 'Z 'Z))
  (display-b b)

  )
;;==========================Task3:DetectingTheGoalState========================
(defmethod check-goal ((board list) &aux result x)
  (setf result nil)
  (loop for x from 0 to (length board)
	do
	   (if (check-individual-win board x)
	       (let ()
		 (setf result t)
	       (return))
	       )
	   )
  
result
)

(defmethod check-individual-win ((board list) pos)
  (or (check-horizontal board pos)
      (check-vertical board pos)
      (check-right-diagonal board pos)
      (check-left-diagonal board pos)
      )
  )

(defmethod check-horizontal ((board list) pos &aux currentsymbol secondIndex secondsymbol thirdIndex thirdsymbol fourthIndex fourthsymbol fifthIndex fifthsymbol)
  (setf currentsymbol (nth pos board))
  (setf secondIndex (+ pos 1))(setf secondsymbol (nth secondIndex board))
  (setf thirdIndex (+ pos 2))(setf thirdsymbol (nth thirdIndex board))
  (setf fourthIndex (+ pos 3))(setf fourthsymbol (nth fourthIndex board))
  (setf fifthIndex (+ pos 4))(setf fifthsymbol (nth fifthIndex board))
   (and (same currentsymbol secondsymbol thirdsymbol fourthsymbol fifthsymbol)(limit pos))
  )

;;same symbol
(defun same (symbol1 symbol2 symbol3 symbol4 symbol5)
  (and
   (not (eq symbol1 '-))
   (not (eq symbol1 'nil))
   (eq symbol1 symbol2)(eq symbol1 symbol3)(eq symbol1 symbol4)(eq symbol1 symbol5)
   (eq symbol2 symbol3)(eq symbol2 symbol4)(eq symbol2 symbol5)
   (eq symbol3 symbol4)(eq symbol3 symbol4)
   (eq symbol4 symbol5))
  )

(defun limit (startingIndex)
  (cond
   ((member startingIndex limit-check) nil)
   (t
    T)
   )
  )

(defun limit-2 (startingIndex)
  (cond
   ((member startingIndex limit-check-2) nil)
   (t
    T)
   )
  )
(defmethod check-vertical ((board list) pos &aux currentsymbol secondIndex secondsymbol thirdIndex thirdsymbol fourthIndex fourthsymbol fifthIndex fifthsymbol)
  (setf currentsymbol (nth pos board))
  (setf secondIndex (+ pos 19))(setf secondsymbol (nth secondIndex board))
  (setf thirdIndex (+ pos 38))(setf thirdsymbol (nth thirdIndex board))
  (setf fourthIndex (+ pos 57))(setf fourthsymbol (nth fourthIndex board))
  (setf fifthIndex (+ pos 76))(setf fifthsymbol (nth fifthIndex board))
  (and (same currentsymbol secondsymbol thirdsymbol fourthsymbol fifthsymbol)(limit pos))
  )

(defmethod check-right-diagonal ((board list) pos &aux currentsymbol secondIndex secondsymbol thirdIndex thirdsymbol fourthIndex fourthsymbol fifthIndex fifthsymbol)
  (setf currentsymbol (nth pos board))
  (setf secondIndex (+ pos 19 1))(setf secondsymbol (nth secondIndex board))
  (setf thirdIndex (+ pos 38 2))(setf thirdsymbol (nth thirdIndex board))
  (setf fourthIndex (+ pos 57 3))(setf fourthsymbol (nth fourthIndex board))
  (setf fifthIndex (+ pos 76 4))(setf fifthsymbol (nth fifthIndex board))
   (and (same currentsymbol secondsymbol thirdsymbol fourthsymbol fifthsymbol)(limit pos))
   )

(defmethod check-left-diagonal ((board list) pos &aux currentsymbol secondIndex secondsymbol thirdIndex thirdsymbol fourthIndex fourthsymbol fifthIndex fifthsymbol)
  (setf currentsymbol (nth pos board))
  (setf secondIndex (- (+ pos 19)1) )(setf secondsymbol (nth secondIndex board))
  (setf thirdIndex (-(+ pos 38) 2 ))(setf thirdsymbol (nth thirdIndex board))
  (setf fourthIndex (-(+ pos 57) 3 ))(setf fourthsymbol (nth fourthIndex board))
  (setf fifthIndex (-(+ pos 76) 4))(setf fifthsymbol (nth fifthIndex board))
  (and (same currentsymbol secondsymbol thirdsymbol fourthsymbol fifthsymbol)(limit-2 pos))
  )

(defmethod task3demo ()
  (setf b (make-new-board))
  (format t "Made a New Board~%")
  (format t "~%Purposely putting down five horizontal consecutive pieces...~%")
  (setf b (put b 'x 'b 'b))
  (setf b (put b 'x 'b 'c))
  (setf b (put b 'x 'b 'd))
  (setf b (put b 'x 'b 'e))
  (setf b (put b 'x 'b 'f))
  (display-b b)
  (format t "Checking winning state : ~A~%" (check-goal b))

  (setf b (make-new-board))

  (format t "Made a New Board~%")
  (format t "~%Purposely putting down five vertical consecutive pieces...~%")
  (setf b (put b 'x 'b 'b))
  (setf b (put b 'x 'c 'b))
  (setf b (put b 'x 'd 'b))
  (setf b (put b 'x 'e 'b))
  (setf b (put b 'x 'f 'b))
  (display-b b)
  (format t "Checking winning state : ~A~%" (check-goal b))

  (setf b (make-new-board))
  (format t "Made a New Board~%")
  (format t "Purposely putting down five left-diagonally consecutive pieces...~%")
  (setf b (put b 'x 'd 'e))
  (setf b (put b 'x 'e 'd))
  (setf b (put b 'x 'f 'c))
  (setf b (put b 'x 'g 'b))
  (setf b (put b 'x 'h 'a))
  (display-b b)
  (format t "Checking winning state : ~A~%" (check-goal b))
  (format t "~%")

  (setf b (make-new-board))
  (format t "Made a New Board~%")
  (format t"~%Purposely putting down five right-diagonally consecutive pieces...~%")
  (setf b (put b 'x 'd 'd))
  (setf b (put b 'x 'e 'e))
  (setf b (put b 'x 'f 'f))
  (setf b (put b 'x 'g 'g))
  (setf b (put b 'x 'h 'h))
  (display-b b)
  (format t "Checking winning state : ~A~%" (check-goal b))
  (format t "~%")

  (setf b (make-new-board))
  (format t "Made a New Board~%")
  (format t "~%Purposely putting down no consecutive pieces...~%")
  (setf b (put b 'x 's 'd))
  (setf b (put b 'x 'h 'g))
  (setf b (put b 'x 's 'i))
  (setf b (put b 'x 'a 'a))
  (setf b (put b 'x 'b 'd))
  (display-b b)
  (format t "Checking winning state : ~A~%" (check-goal b))
  (format t "~%")

  
  (format t "~%Continue putting down some pieces...~%")
  (setf b (put b 'o 'h 'h))
  (setf b (put b 'x 'h 'i))
  (setf b (put b 'o 'g 'g))
  (setf b (put b 'o 'h 'f))
  (setf b (put b 'o 'g 'f))
  (setf b (put b 'o 'i 'g))
  (setf b (put b 'x 'i' h))
  (display-b b)
  (format t "Checking winning state : ~A~%" (check-goal b))
  (format t "~%")

  (format t "~%Continue putting down some pieces...~%")
  (setf b (put b 'o 'l 's))
  (setf b (put b 'o 'm 'a))
  (setf b (put b 'o 'm 'b))
  (setf b (put b 'o 'm 'c))
  (setf b (put b 'o 'm 'd))
  (display-b b)
  (format t "Checking winning state : ~A~%" (check-goal b))
  (format t "~%")

   (format t "~%Continue putting down some pieces...~%")
  (setf b (put b 'o 'n 'c))
  (setf b (put b 'o 'o 'b))
  (setf b (put b 'o 'p 'a))
  (setf b (put b 'o 'p 's))
  ;(setf b (put b 'o 'm 'd))
  (display-b b)
  (format t "Checking winning state : ~A~%" (check-goal b))
  (format t "~%")
  
  )


;;===========================TASK4:Minimax Evaluation==========================
;(defvar max-depth 3)
;;a list containing all current possible moves
;;called in task2's put method; variable would be updated if a piece has been successfully placed 
;(setf *move-list* '())

;;generate the move list when the board was initially created. - 19*19 moves
;;each position is an available move
;;called in task1's make-new-board method
(defun generate-initial-movelist (&aux *move-list*)
  (setf *move-list* '())
  (dotimes (x (length row))
    (dotimes (y (length column))
      (setf xx (nth x row))
      (setf yy (nth y column))
      (setf pair (cons xx yy))
      (setf *move-list* (cons pair *move-list*))      
      )
    )
  *move-list*
  )

(defclass node()
  (
   (current-board :accessor board :initarg :node)
   (parent :accessor board-parent :initarg :parent)
   (successor :accessor board-successor :initarg :successor)
   (operator :accessor operator :initarg :operator)
   (score :accessor board-score :initform '0 :initarg :score)
   (moves :accessor avai-moves :initarg :moves)
   (history :accessor play-history :initarg :history)
   )
  )

(defmethod new-node (&aux board movelist)
  (setf board (make-new-board))
;  (setf movelist (generate-initial-movelist))
  (make-instance 'node
		 :node board
		 :parent nil
		 :successor nil
		 :operator nil
		 :moves (generate-initial-movelist)
		 :score 0
		 :history '()
		 )
  )

(defmethod display-n ((n node))
 (display-b (board n))
  )


(defun opposite (player)
  (if (eq player 'x) 'o
    'x)
  )

;;trace the minimaxing tree back up to get the next best move
;;this method returns the entire updated board with the best move
(defmethod next-best-move ((node node) depth max-player current-player &aux b)
  (setf e1 (board-evaluation node current-player))
  (setf e2 (board-evaluation node (opposite current-player)))
  (cond
   ;;his worse -when we both have high score but his score is better
   ((and (>= e2 100000)(>= e1 100000)(> e2 e1))(setf b (minimax node depth (opposite max-player) current-player)))
   ;;my best - when we both have high scores but mine is better
   ((and (>= e2 100000)(>= e1 100000)(> e1 e2)) (setf b (minimax node depth max-player current-player)))
   ;;his worst - when he is at advantage and I am not
   ((and (>= e2 100000)(< e1 100000))(setf b (minimax node depth (opposite max-player) current-player)))
   ;;my best - when im at advantage and he is not
   ((and (>= e1 100000)(< e2 100000))(setf b (minimax node depth max-player current-player)))
   
   (t
   ;;o.w. my best
    (setf b (minimax node depth max-player current-player))))

  (loop while  (not (eq (board-parent b) node))
	do
	(if ( eq (board-parent b) nil) (setf b b)
	  (let ()

	    (setf b (board-parent b))
	    )
	  )
	)
  b
  )


;;establishes a tree
(defmethod minimax ((node node) depth max-player current-player &aux best-board childnodes board cn)
  (cond
   ( (= (length (avai-moves node) ) 361) (minimax (apply-move (cons 'j 'j) node current-player) (- depth 1) max-player (opposite current-player)))
   ( (>= ( board-score node) 1000000000) node)
 ; (( = (length (avai-moves node)) 361)(apply-move (cons 'j 'j) node current-player))
  ((= depth 0) node)
;  (setf best-score 0)
  ((eq max-player current-player)

   (let()
   ;  (format t  "they are the same")
    ; (print (eq max-player current-player))
     (setf best-board (new-node))
     (setf (board-score best-board) -10000000)
   ;  (setf childnodes (generate-child-nodes n current-player))
     (setf childnodes (generate-neighbor-child-nodes node current-player))
     
  (loop for cn in childnodes
	do
	(setf board (minimax cn (- depth 1)  max-player (opposite current-player) ))
;	(if (eq current-player max-player)
;	    (let()
	(if (>= (board-score board) (board-score best-board))
	    (setf best-board board))
	)
;	  (let()
;	    (if (<= score best-score)(setf best-score score)))
					;	    )
  best-board
	)
  )
  ((not (eq max-player current-player))
   (let ()
    ; (format t "they are not the same")
   ;  (print (not (eq max-player current-player)))
	  (setf best-board (new-node))
	  (setf (board-score best-board) 10000000)
	  (setf childnodes (generate-neighbor-child-nodes node current-player))

	  (loop for cn in childnodes
		
		do
	;	(format t "calling ~A and ~A ~%" max-player (opposite current-player))
		(setf board (minimax cn (- depth 1)  max-player (opposite current-player)))

		(if (<= (board-score board)(board-score best-board))
		    (setf best-board board)))
	  best-board
	  )
	)
  ))



(defmethod maximum ((node1 node)(node2 node)&aux score1 score2)
  (if (eq node1 nil) (setf score1 0)
    (setf score1 (board-score node1)))
  (if (eq node2 nil) (setf score2 0)
    (setf score2 (board-score node2)))
  (if (>= score1 score2) node1
    node2)
  
  )

(defmethod minimum ((node1 node)(node2 node)&aux score1 score2)
  (if (eq node1 nil) (setf score1 0)
    (setf score1 (board-score node1)))
  (if (eq node2 nil) (setf score2 0)
    (setf score2 (board-score node2)))
  (if (>= score1 score2) node2
    node1)
  )



;;generate the next possible boards 
(defmethod generate-child-nodes ((n node) player &aux childList x i)
  (setf childList '())
  (setf moves (avai-moves n))
 ; (list moves)
  (loop for x in moves
	do
	(list x)
	(setf i (apply-move x n player))
	(setf childList (cons i childList))
	
	)
  childList
  )

;;cut down the searching space
(defmethod generate-neighbor-child-nodes ((n node) player &aux childList x i)
  (setf childList '())
  (setf moves (generate-neighbors n))
  (loop for x in moves
	do
	(setf i (apply-move x n player))
	(setf childList (cons i childList))
	(length childList)
	)
  childList
  )


(defmethod apply-move (move (n node) player &aux updated rowsymbol columnsymbol)
  
  (setf updated (copy-node n)) 
  (setf rowsymbol (car move))
  (setf columnsymbol (cdr move))
  (setf (board updated) (put (board updated) player rowsymbol columnsymbol))
  (setf (board-parent updated) n)
  (setf (operator updated) move)
  (setf (avai-moves updated)(take-from move (avai-moves n)))
  (setf (board-score updated)(board-evaluation updated player))
  (setf (play-history updated)(snoc move (play-history n)))
  updated
  
  )

(defmethod copy-node ((n node))
  (make-instance 'node
		 :node (board n)
		 :parent(board-parent n)
		 :operator (operator n)
		 :moves (avai-moves n)
		 :score (board-score n)
		 :successor (board-successor n)
		 :history (play-history n)
		 )
  )

;;prints out the info about a node.
(defmethod info ((n node))
  (format t "===================================INFO===============================~%")
  (format t "Node:~A~%" n)
  (format t "Current Board:")
  (format t "~A" (display-b (board n)))
  (format t "~%Length of the available moves for this board is: ~A~%" (length (avai-moves n)))
  (format t "Parent Board:~%~A" (board-parent n))
  (format t "~%Node Derived from parent with the move: ~A~%" (operator n))
  (format t "Board Eval Score in X persepctive :~A~%" (board-evaluation n 'x))
  (format t "Board Eval Score in O persepctive :~A~%" (board-evaluation n 'o))
  (format t "Move history:~A~%" (play-history n))
  (format t "==============================END INFO================================~%") 
  )
(defmethod task4.1demo ()
  (format t "Making a new node -- the root node.~%")
  (setf n (new-node))
  (setf move (cons 'a 'a))
  (info n)
  (format t "Applying move ~A to the board" move)
  (format t "~%")
  (setf n2 (apply-move move n 'x))
  (info n2)
  (format t"Generating child nodes for ~%" )
  (setf node-list (generate-child-nodes n 'x))
  (format t "There are ~A child nodes.~%" (length node-list))
  (setf n3 (nth 0 node-list))
  (format t "The first node in the child node list:~%")
  (info n3)
  (format t "Generating child nodes for the first child node of the root node~A ~%" n)
  (setf node-list2 (generate-child-nodes n3 'o))
  (format t "There are ~A child nodes for the first child node of the root node.~%" (length node-list2))
  (setf n4 (nth 0 node-list2))
  (format t "The first node from its child node list info:~%")
  (info n4)

  )


(defmethod task4.2demo()
  (setf n (new-node))
  (setf m (minimax n 1 'x 'x))
  (display-n m)

  )

(defmethod get-horizontal ((node node) pos)
  (setf list '())
  (setf board (board node))
  (if (limit pos)
      (let ()
	(setf list (snoc (nth pos board) list))
	(setf list (snoc (nth (+ pos 1) board) list))
	(setf list (snoc (nth (+ pos 2) board) list))
	(setf list (snoc (nth (+ pos 3) board) list))
	(setf list (snoc (nth (+ pos 4) board) list))
	)
    )
  list
  )

(defmethod get-horizontal-6 ((node node ) pos)
  (setf list '())
  (setf board (board node))
  (if (limit-h-d-6 pos)
      (let ()
		(setf list (snoc (nth pos board) list))
	(setf list (snoc (nth (+ pos 1) board) list))
	(setf list (snoc (nth (+ pos 2) board) list))
	(setf list (snoc (nth (+ pos 3) board) list))
	(setf list (snoc (nth (+ pos 4) board) list))
	(setf list (snoc (nth (+ pos 5) board) list))
	)
    )
  list
  )


(defmethod limit3 (pos)
  (not (and (<= pos 360) (>= pos 285)))
  )
(defmethod limit6 (pos)
  (not (and (<= pos 360) (>= pos 266)))
  )

(defmethod limit-h-d-6 (pos)
  (not (or 
  (and (>= pos 14)(<= pos 18))
  (and (>= pos 33)(<= pos 37))
  (and (>= pos 52)(<= pos 56))
  (and (>= pos 71)(<= pos 75))
  (and (>= pos 90)(<= pos 94))
  (and (>= pos 109)(<= pos 113))
  (and (>= pos 128)(<= pos 132))
  (and (>= pos 147)(<= pos 151))
  (and (>= pos 166)(<= pos 170))
  (and (>= pos 185)(<= pos 189))
  (and (>= pos 204)(<= pos 208))
  (and (>= pos 223)(<= pos 227))
  (and (>= pos 242)(<= pos 246))
  (and (>= pos 261)(<= pos 265))
  (and (>= pos 280)(<= pos 284))
  (and (>= pos 299)(<= pos 303))
  (and (>= pos 318)(<= pos 322))
  (and (>= pos 337)(<= pos 341))
  (and (>= pos 356)(<= pos 360))
  ))

  )


(defmethod limit-h-dl-6 (pos)
  (not (or 
  (and (>= pos 0)(<= pos 4))
  (and (>= pos 19)(<= pos 23))
  (and (>= pos 38)(<= pos 42))
  (and (>= pos 57)(<= pos 61))
  (and (>= pos 76)(<= pos 80))
  (and (>= pos 95)(<= pos 99))
  (and (>= pos 114)(<= pos 118))
  (and (>= pos 133)(<= pos 137))
  (and (>= pos 152)(<= pos 156))
  (and (>= pos 171)(<= pos 175))
  (and (>= pos 190)(<= pos 194))
  (and (>= pos 209)(<= pos 213))
  (and (>= pos 228)(<= pos 232))
  (and (>= pos 247)(<= pos 251))
  (and (>= pos 266)(<= pos 270))
  (and (>= pos 285)(<= pos 289))
  (and (>= pos 304)(<= pos 308))
  (and (>= pos 323)(<= pos 327))
  (and (>= pos 342)(<= pos 346))
  ))

  )

(defmethod get-vertical ((node node) pos)
  (setf list '())
  (setf board (board node))
  (if (limit3 pos)
      (let ()
	(setf list (snoc (nth pos board) list))
	(setf list (snoc (nth (+ pos 19) board) list))
	(setf list (snoc (nth (+ pos 38) board) list))
	(setf list (snoc (nth (+ pos 57) board) list))
	(setf list (snoc (nth (+ pos 76) board) list))
	)
    )
  list
  )
(defmethod get-vertical-6 ((node node )pos)
  (setf list '())
  (setf board (board node))
  (if (limit6 pos)
      (let ()
	(setf list (snoc (nth pos board) list))
	(setf list (snoc (nth (+ pos 19) board) list))
	(setf list (snoc (nth (+ pos 38) board) list))
	(setf list (snoc (nth (+ pos 57) board) list))
	(setf list (snoc (nth (+ pos 76) board) list))
	(setf list (snoc (nth (+ pos 95) board) list))
	)
    )
  list
  )
	
(defun getindex (pos1 pos2)
  (setf xIndex (get 'coordinates pos1))
  (setf yIndex (get 'coordinates pos2))
  (+ (* xIndex 19) yIndex)
  )

(defmethod get-r-diagonal((node node) pos)
  (setf list '())
  (setf board (board node))
  (if (limit pos)
      (let ()
	(setf list (snoc (nth pos board) list))
	(setf list (snoc (nth (+ pos 20) board) list))
	(setf list (snoc (nth (+ pos 40) board) list))
	(setf list (snoc (nth (+ pos 60) board) list))
	(setf list (snoc (nth (+ pos 80) board) list))
	)
    )
  )

(defmethod get-r-diagonal-6 ((node node) pos)
  (setf list '())
  (setf board (board node))
  (if (limit-h-d-6 pos)
      (let ()
	(setf list (snoc (nth pos board) list))
	(setf list (snoc (nth (+ pos 20) board) list))
	(setf list (snoc (nth (+ pos 40) board) list))
	(setf list (snoc (nth (+ pos 60) board) list))
	(setf list (snoc (nth (+ pos 80) board) list))
	(setf list (snoc (nth (+ pos 100) board) list))
	)
    )

  )
(defmethod get-l-diagonal((node node) pos)
  (setf list '())
  (setf board (board node))
  (if (limit pos)
      (let ()
	(setf list (snoc (nth pos board) list))
	(setf list (snoc (nth (+ pos 18) board) list))
	(setf list (snoc (nth (+ pos 36) board) list))
	(setf list (snoc (nth (+ pos 54) board) list))
	(setf list (snoc (nth (+ pos 72) board) list))
	)
    )
  )

(defmethod get-l-diagonal-6((node node) pos)
  (setf list '())
  (setf board (board node))
  (if (limit-h-dl-6 pos)
      (let ()
	(setf list (snoc (nth pos board) list))
	(setf list (snoc (nth (+ pos 18) board) list))
	(setf list (snoc (nth (+ pos 36) board) list))
	(setf list (snoc (nth (+ pos 54) board) list))
	(setf list (snoc (nth (+ pos 72) board) list))
	(setf list (snoc (nth (+ pos 90) board) list))
	)
    )
  )

(defmethod openthree ((board list) pos player)
  (if (and (and (eq (nth 0 board) '-)(eq (nth 4 board) '-))
      (and (eq (nth 1 board) player)
	   (eq (nth 2 board) player)
	   (eq (nth 3 board) player)))
      t
    nil)
  )

;;takes in a length 6 list
;;x o o o o - or - o o o o x
(defmethod cappedfour ((board list) pos player)
  (or
   (and (eq (nth 0 board) '-)
       (eq (nth 1 board) player)
       (eq (nth 2 board) player)
       (eq (nth 3 board) player)
       (eq (nth 4 board) player)
       (eq (nth 5 board) (opposite player)))
   (and (eq (nth 0 board) (opposite player))
       (eq (nth 1 board) player)
       (eq (nth 2 board) player)
       (eq (nth 3 board) player)
       (eq (nth 4 board) player)
       (eq (nth 5 board) '-))
  )
  )

;;takes in a length 5 list
;; -ooox or xooo-
(defmethod cappedthree ((board list) pos player)
  (or 
   (and (eq (nth 0 board) '-)
       (eq (nth 1 board) player)
       (eq (nth 2 board) player)
       (eq (nth 3 board) player)
       (eq (nth 4 board) (opposite player)))
  (and (eq (nth 0 board) (opposite player))
       (eq (nth 1 board) player)
       (eq (nth 2 board) player)
       (eq (nth 3 board) player)
       (eq (nth 4 board) '-)))
  )


(defmethod gappedthree ((board list) pos player)
  (or
   (and
    (eq (nth 0 board) '-)
    (eq (nth 1 board) player)
    (eq (nth 2 board) player)
    (eq (nth 3 board) '-)
    (eq (nth 4 board) player)
    (eq (nth 5 board) '-)
    )
   (and
    (eq (nth 0 board) '-)
    (eq (nth 1 board) player)
    (eq (nth 2 board) '-)
    (eq (nth 3 board) player)
    (eq (nth 4 board) player)
    (eq (nth 5 board) '-)
    )
   )
  )

(defmethod gappedfour ((board list) pos player)
  (or
   (and
    (eq (nth 0 board) player)
    (eq (nth 1 board) player)
    (eq (nth 2 board) player)
    (eq (nth 3 board) '-)
    (eq (nth 4 board) player))
   (and
    (eq (nth 0 board) player)
    (eq (nth 1 board) '-)
    (eq (nth 2 board) player)
    (eq (nth 3 board) player)
    (eq (nth 4 board) player)
    )
   )
  )

(defmethod con-five ((board list) pos player)
  (if (same (nth 0 board) (nth 1 board) (nth 2 board) (nth 3 board) (nth 4 board))
      t
    nil))

(defmethod gapped-2-2 ((board list) pos player)
  (and
   (eq (nth 0 board) player)
   (eq (nth 1 board) player)
   (eq (nth 2 board) '-)
   (eq (nth 3 board) player)
   (eq (nth 4 board) player)
   )
  )

(defmethod openfour ((board list) pos player)
  (if
      (and
      (eq (nth 0 board) '-)
      (eq (nth 5 board) '-)
      (eq (nth 1 board) player)
      (eq (nth 2 board) player)
      (eq (nth 3 board) player)
      (eq (nth 4 board) player))
      t
    nil
    )
  )




(defmethod generate-neighbors ((node node) &aux neighborlist n)
  (setf neighborlist '())
  (setf initial-movelist (generate-initial-movelist))
  (setf current-avai-movelist (avai-moves node))
  (setf np (remove-list current-avai-movelist initial-movelist))
  (dolist (x np)
    (dolist (y current-avai-movelist)
      (if (and (not (member y neighborlist))
	       (distance-check x y))
	       
	  (setf neighborlist (snoc y neighborlist)))))
  neighborlist
)



(defmethod task4.2demo ()
  (setf n (new-node))
  (setf move (cons 'a 'a))
  (setf n (apply-move move n 'x))
  (generate-neighbors n)
  (setf n (apply-move (cons 'i 'i) n 'o))
  (generate-neighbors n)
  (setf n (apply-move (cons 'b 'b) n 'o))
  (list (generate-neighbors n))
  (print (length (generate-neighbors n)))
  (generate-neighbor-child-nodes n 'x)
  
  )

(defun remove-list (list-to-remove target-list)
  (remove-if (lambda (element)
               (member element list-to-remove
                       :test #'equal))
             target-list))


(defun distance-check ( (p1 list)( p2 list) &aux p1x p1y p2x p2y)
  (setf p1x (get 'coordinates (car p1)))
  (setf p1y (get 'coordinates (cdr p1)))
  (setf p2x (get 'coordinates (car p2)))
  (setf p2y (get 'coordinates (cdr p2)))
  (and 
  (< (abs (- p1x p2x)) 2)
  (< (abs (- p1y p2y)) 2))
  )

(defun distance-to-center ((p1 list))
			   (setf p1x (get 'coordinates (car p1)))
			   (setf p1y (get 'coordinates (cdr p1)))
  
  (sqrt (+ (expt (abs (- p1x 9)) 2)
	   (expt (abs (- p1y 9)) 2)))
  )

(defmethod get-board-total ()
  (setf score 0)
 ; (print score)
  (loop for x in row
	do
;	(print x)
	(loop for y in column
	      do
	     ; (print y)
	      (setf score ( + (distance-to-center (cons x y)) score))
	      )
	)
  score
  )
(defmethod get-total-distance ( (board list) player)
  (setf score 0)
 ; (print score)
  (loop for x in row
	do
;	(print x)
	(loop for y in column
	      do
					; (print y)
	      
	      (setf piece (get-piece board x y))
	      (if (not (eq piece nil))
		  (let()
		    (if (eq piece player)
			(setf score ( + (distance-to-center (cons x y)) score)
			      )
	      )
		    )
		)
	      )
	)
  score
  )

			   

(defmethod check-methods ()
  (setf n (new-node))
  (setf n (apply-move (cons 'j 'j) n 'x))
  (setf n (apply-move (cons 'i 'j) n 'x))
  (setf n (apply-move (cons 'i 'k) n 'x))
  (setf n (apply-move (cons 'h 'l) n 'x))
  (setf n (apply-move (cons 'g 'm) n 'x))
  
  (print (get-l-diagonal-6 n 108))


  (info n)
  )

(defmethod board-evaluation ((node node) player )
 ; (print player)
  (setf finalscore 0)
  (if (= (length (avai-moves node)) 361)
      0
    (let ()
      (setf *openthree* 0)
      (setf *cappedthree* 0)
  (setf *cappedfour* 0)
  (setf *openfour* 0)
  (setf *gappedthree* 0)
  (setf *gappedfour* 0)
  (setf *gapped-2-2* 0)
  (setf *win* 0)
  (setf board (board node))
  (setf initial-movelist (generate-initial-movelist))
  (setf current-avai-movelist (avai-moves node))
  (setf np (remove-list current-avai-movelist initial-movelist))
  (setf neighbor-list (generate-neighbors node))
  (setf evaluationlist (append neighbor-list np))
;  (print evaluationlist)

;  (format t "evaluation list:")
;  (print evaluationlist)
  (loop for x in evaluationlist
	do


	(setf xSymbol (nth 0 x))
;	(print xSymbol)
	(setf ySymbol (cdr x))
;	(print ySymbol)
	(setf small-5-lists '())
	(setf small-6-lists '())
	(setf pos (getIndex xSymbol ySymbol))
       
	(setf v5 (get-vertical node pos ))

	;;get all the different directions list
	(setf small-5-lists (snoc v5 small-5-lists))
	(setf v6 (get-vertical-6 node pos))
	(setf small-6-lists (snoc v6 small-6-lists))
	(setf h5 (get-horizontal node pos))
	(setf small-5-lists (snoc h5 small-5-lists))
	(setf h6 (get-horizontal node pos ))
	(setf small-6-lists (snoc h6 small-6-lists))
	(setf rd5 (get-r-diagonal node pos))
	(setf small-5-lists (snoc rd5 small-5-lists))
	(setf rd6 (get-r-diagonal-6 node pos))
	(setf small-6-lists (snoc rd6 small-6-lists))
	(setf ld5 (get-l-diagonal node pos))
	(setf small-5-lists (snoc ld5 small-5-lists))
	(setf ld6 (get-l-diagonal-6 node pos))
	(setf small-6-lists (snoc ld6 small-6-lists))
	(setf gh6 (get-horizontal-6 node pos))
	(setf small-6-lists (snoc gh6 small-6-lists))
	(setf gv6 (get-vertical-6 node pos))
	(setf small-6-lists (snoc gv6 small-6-lists))
	
  (loop for 5-list in small-5-lists
	do
	(if
	    (openthree 5-list pos player)
	    (setf *openthree* (+ *openthree* 1)))

	(if
	    (cappedthree 5-list pos player)
	    (setf *cappedthree* (+ *cappedthree* 1)))
	(if
	    (gappedfour 5-list pos player)
	    (setf *gappedfour* (+ *gappedfour* 1)))
	(if
	    (gapped-2-2 5-list pos player)
	    (setf *gapped-2-2* (+ *gapped-2-2* 1)))
	(if
	   
	    (con-five 5-list pos player)
	    (setf *win* (+ *win* 1))
)
	(if
	    (gappedthree 5-list pos player)
	    (setf *gappedthree* (+ *gappedthree* 1)))

	)
	
  (loop for 6-list in small-6-lists
	do
	(if
	    (cappedfour 6-list pos player)
	    (setf *cappedfour* (+ *cappedfour* 1)))
	(if
	    (openfour 6-list pos player)
	    (setf *openfour* (+ *openfour* 1)))

	 )

  
  )

  (setf totaldistance 0)
  (setf totaldistance  (get-total-distance (board node) player))
  (setf totaldistance (- 2621.2173 totaldistance))
  
;  (format t "number of openfour~A~%" *openfour*)
;  (format t "number of openthree~A~%" *openthree*)
 ; (format t "number of cappedfour~A~%" *cappedfour*)
;  (format t "totol distance: ~A~%" totaldistance)
			       ;	  (format t "winning state~A~%" *win*)
				;	(print small-5-lists)
				;	  (format t "number of five in a row~A~%" *win*)
  ;;2621.2173

  (setf finalscore ( + totaldistance (* *win* 10000000)
		       (* *openfour* 1000000)(* *openthree* 100000)
		       (* *cappedfour* 100050)(* *gappedthree* 100000)
		       (* *gappedfour* 100050)(* *gapped-2-2* 100050)
		       (* *cappedthree* 10000)))
  ;; capped three 1000

   )

    )
  finalscore
  )


  (defmethod final-board-evaluation ((node node) player &aux score)
    (setf score 0)
    (setf max-score (board-evaluation node player))
    (setf min-score (board-evaluation node (opposite player)))
    (setf score (- max-score min-score))
    score
    )
(defmethod task4.3demo ()
  (format t "Making a new node....")
  (setf n (new-node))
  (format t "Information about this node...:~%")
  (info n)

  (format t "Adding some piece to the current game board of this node...and displaying it~%")
  (setf n (apply-move (cons 'j 'i) n 'x))

  (setf n (apply-move (cons 'a 'a) n 'o))
  (setf n (apply-move (cons 'a 'b) n 'x))
  (setf n (apply-move (cons 'a 'c) n 'o))
  (setf n (apply-move (cons 'a 'd) n 'x))
   (setf n (apply-move (cons 'r 'r) n 'o))
   (setf n (apply-move (cons 'j 'j) n 'x))
    (setf n (apply-move (cons 'i 'a) n 'o))
    (setf n (apply-move (cons 'j 'k) n 'x))
    (setf n (apply-move (cons 'j 'b) n 'o))
    (setf n (apply-move (cons 'j 'l) n 'x))
     (format t "Current board evaluation score in o's persepctive is :~A ~%" (board-evaluation n 'o))
     (setf n (apply-move (cons 'a 'i) n 'o))
     (display-n n)
     (format t "O's Perspective suuposed to be :~A~%" (board-evaluation n 'o))
					; (setf n (apply-move (cons 'a 'e) n 'o))
  (format t "Current board evaluation score in x's perspective is :~A ~%" (board-evaluation n 'x))
  (format t "Current board evaluation score in o's persepctive is :~A ~%" (board-evaluation n 'o))
 
  )

;;demo play
(defmethod play ()
  (setf n (new-node))
  (dolist  (player ' (x o x o x o x o x o x o x o x o x o) )
 ; (cond
  ; ((eq player 'x)
    (setf n (next-best-move n 2 player player))
    (info n)
;  )

))

;;============================TASK6:modelingplayers============================
(defclass player()
  (
   (name :accessor player-name :initarg :name :initform 'Teemo)
   (playersymbol :accessor player-symbol :initarg :playersymbol :initform 'O)
   )
  )

(defclass machine-player (player)())

(defclass human-player (player)())



;;gameplay of two random machine players ...
;;@param num is optional. num dominates the total number of moves the gameply will generate
(defmethod demo-machine-machine (&optional num &aux n gameover x o currentplayer count )
  (setf n (new-node))
  (setf gameover nil) 
  (setf x (make-instance 'machine-player ))
  (setf o (make-instance 'machine-player ))
  (setf currentplayer x)
  (setf (player-symbol x) 'X)
  (setf (player-symbol o) 'O)
  (setf (player-name o) 'Sam)
  (setf count 0 )
  (format t "~A will be playing against ~A.~%  ~A will start with symbol X.~% "
	  (player-name x) (player-name o) (player-name x))
  (display-n n)
  (if (eq num nil) (setf num 361))
  (loop while (and (eq gameover nil)(<= count num))
	do
;	(display-n n)
	
	(setf count (+ count 1))
	(format t "~%~% ~A is thinking......might take awhile......~%" (player-name currentplayer))
	(setf n (next-best-move n 2 (player-symbol currentplayer) (player-symbol currentplayer)))
	(display-n n)
	(format t "~% ~A made a move at position ~A. " (player-name currentplayer) (operator n))
	(if
	    (eq (player-symbol currentplayer) 'x)
	    (setf currentplayer o)
	  (setf currentplayer x)
	  )

	 (if (or ( check-goal (board n) ) (= (length (avai-moves n)) 0))
	     (setf gameover t)
	   )
	 )
  (if (check-goal (board n))
      (let ()
	(setf winner nil)
	(if
	    (eq (player-symbol currentplayer) 'x)
	    (setf winner o)
	  (setf winner x)
	  )
	(format t "~%Game Over! ~A won the game!" (player-name winner))

	)
    
    (format t "~%Game over! No one wins!")
    
    
    )
	(format t "~%Game move history: ~A " (play-history n))	
nil
  )

;;=================================TASK7=====================================

(defmethod demo-machine-human ()
  (setf n (new-node))
  (setf machine-player (make-instance 'machine-player))
  (setf human-player (make-instance 'human-player))
  (setf (player-symbol human-player) 'X)
  (format t "What would you like to be called?~%")
  (setf user-name (read))
 ; (print user-name)
  (setf (player-name human-player) user-name)
   (format t "============================================================================~%")
  (format t "Welcome, ~A ! ~%You will be playing against our machine player ~A. Your symbol will be X and ~A's symbol will be O.~%" user-name (player-name machine-player) (player-name machine-player))
  (format t "Here are some instructions for this Five in a Row game: ~%")
  (format t "The player who first obtains a consecutive of five identical pieces will win the game -- either vertically, horizontally or diagonally.~%")
  (format t "=============================================================================~%")
  (display-n n)
  (setf gameover nil)
  (setf list '())
;  (setf user-command nil)
  (setf current-player human-player)
  (loop while (eq gameover nil)
	do
;	(display-n n)
	(if (eq (player-symbol current-player) 'X)
	    (let ()
	      (format t "It is your turn:~%")
	      (format t "Please type in the row letter:~%")
	      (setf user-row (read))
	      (format t "Please type in the column letter:~%")
	      (setf user-column (read))
	      (setf n (apply-move (cons user-row user-column) n (player-symbol current-player)))
	      (display-n n)
	      (format t "You put down ~A~% " (cons user-row user-column))
	      (setf current-player machine-player)
	      )
	  (let ()
	    (format t "It is ~A's turn.~% " (player-name current-player))
	    (format t "~A is thinking... might take a while since ~A is not as smart as you are..." (player-name current-player)(player-name current-player))
	    (setf n (next-best-move n 2 (player-symbol machine-player) (player-symbol machine-player)))
	    (display-n n)
	    (format t "~A put down ~A~%" (player-name current-player)(operator n))
	    (setf current-player human-player)
	    )
	  )
	(if (or ( check-goal (board n) ) (= (length (avai-moves n)) 0))
	     (setf gameover t)
	  ))

  (if (check-goal (board n))
	    (let ()
	      (setf winner nil)
	      (if
		  (eq current-player machine-player)
		  (setf winner human-player)
		(setf winner machine-player)
		)
	 
	      (format t "~%Game Over! ~A won the game!" (player-name winner))
	      )
	  (format t "~%Game over! No one wins!")
	  )
  )


;;=================================TASK 8======================================


(defmethod demo8 ()

  (demo-machine-machine 3)
  )
