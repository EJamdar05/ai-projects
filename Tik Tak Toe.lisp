(defvar board '(* * * * * * * * * * * * * * * *)) 
(defvar test 1)

;function boardState will print out the board everytime it is called with the current state in which elements
;in the list are
(defun boardState ()
    ;(write )(write (car board)) (write (car board))
    (format t "|")(write (car board))(format t "|")(write (car(cdr board)))(format t "|")(write (car(cdr(cdr board))))(format t "|")(write (car(cdr(cdr(cdr board))))) (format t "|")(terpri)
    (format t "---------") (terpri)
    (format t "|")(write (car(cdr(cdr(cdr(cdr board)))))) (format t "|")(write (car(cdr(cdr(cdr(cdr(cdr board))))))) (format t "|")
    (write (car(cdr(cdr(cdr(cdr (cdr(cdr board)))))))) (format t "|")(write (car(cdr(cdr(cdr(cdr (cdr(cdr(cdr board))))))))) (format t "|") (terpri)
    (format t "---------") (terpri)
    (format t "|")(write (car(cdr(cdr(cdr(cdr (cdr(cdr(cdr(cdr board)))))))))) (format t "|") (write (car(cdr(cdr(cdr(cdr (cdr(cdr(cdr(cdr(cdr board))))))))))) (format t "|") 
    (write (car(cdr(cdr(cdr(cdr (cdr(cdr(cdr(cdr(cdr(cdr board)))))))))))) (format t "|") (write (car(cdr(cdr(cdr(cdr (cdr(cdr(cdr(cdr(cdr(cdr(cdr board))))))))))))) (format t "|") (terpri)
    (format t "---------") (terpri)
    (format t "|") (write (car(cdr(cdr(cdr(cdr (cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr board)))))))))))))) (format t "|") (write (car(cdr(cdr(cdr(cdr (cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr board))))))))))))))) (format t "|") 
    (write (car(cdr(cdr(cdr(cdr (cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr board)))))))))))))))) (format t "|") (write (car(cdr(cdr(cdr(cdr (cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr board))))))))))))))))) (format t "|") 
    (terpri)
)

;function RESULT will take a letter (either X or O) and a position on the board
;and atrtempt to insert it on the board. In addition, it will also then test to see
;if a winner is present and prints winner if one is found or draw if none exists 
(defun RESULT(letter pos)
    
    (setq blank '(* T X O)) ;common symbols (* = blank, T = true)


    ;first cond will get the position on the board in order to use it to
    ;assign the letter. First condition is for O aka the CPU since the calculated index starts at zero
    ;while the user provided one can give answers from 1-16 
    (cond 
        ((eq (car letter) (car(cdr(cdr(cdr blank)))))(setq position (nth pos board)))
        ((eq (car letter) (car(cdr(cdr blank))))(setq position (nth (- pos 1) board)))
    )
   ;test, test2 and test3 are variables to make it easier to type conditonals 
    (setq test (car (cdr blank))) ;symbol T
    (setq test2 (car (cdr(cdr blank))));symbol X
    (setq test3 (car (cdr(cdr(cdr blank))))) ;symbol O
    
    ;if position has a blank symbol (*), then add the letter to the board

    (cond 
        ((and (eq position (car blank)) (eq (car letter) (car(cdr(cdr(cdr blank))))) (setf (nth pos board) (car letter)) ))
        
        ((and (eq position (car blank)) (eq (car letter) (car(cdr(cdr blank)))) (setf (nth (- pos 1) board) (car letter))))
    )
    ;findWinner tests if a winner has been found and stores result (T or NIL)
    (setq findWinner (TERMINAL-TEST letter))
    ;checks for draw with terminaltest2
    (setq drawCheck (TERMINAL-TEST2 blank))

    
    (cond 
        ((and (eq test findWinner) (eq (car letter) test2) (print "Winner")1)) ;if we found a winner and it is the player, they win and returns 1
        ((and (eq test findWinner) (eq (car letter) test3) (print "CPU Wins") 2)) ;if a winner is found and it is the cpu, the cpu wins and returns 2
        ((eq drawCheck NIL) (print "Draw") 3) ;draw and returns 3
    )
    
    

)

;TERMINAL-TEST2 will check for a draw. So long as a blank symbol is found, then we do not have a draw
(defun TERMINAL-TEST2 (blank)
    (setq number 0)
    (loop for x in board
        do (cond ((eq (car blank) x)
            (return 1))))
)

;TERMINAL-TEST will take in a player's symbol and test if the boards config matches any of these tests cases
;returns T if a winning outcome was found NIL otherwise 
(defun TERMINAL-TEST (player)
    ;sq (square) and the number represents a number in the board to make conditionals easier to write
    (setq sq1 (car board)) 
    (setq sq2 (car(cdr board)))
    (setq sq3 (car(cdr(cdr board))))
    (setq sq4 (car(cdr(cdr(cdr board)))))
    (setq sq5 (car(cdr(cdr(cdr(cdr board))))))
    (setq sq6 (car(cdr(cdr(cdr(cdr(cdr board)))))))
    (setq sq7 (car(cdr(cdr(cdr(cdr (cdr(cdr board))))))))
    (setq sq8 (car(cdr(cdr(cdr(cdr (cdr(cdr(cdr board)))))))))
    (setq sq9 (car(cdr(cdr(cdr(cdr (cdr(cdr(cdr(cdr board))))))))))
    (setq sq10 (car(cdr(cdr(cdr(cdr (cdr(cdr(cdr(cdr(cdr board)))))))))))
    (setq sq11 (car(cdr(cdr(cdr(cdr (cdr(cdr(cdr(cdr(cdr(cdr board))))))))))))
    (setq sq12 (car(cdr(cdr(cdr(cdr (cdr(cdr(cdr(cdr(cdr(cdr(cdr board)))))))))))))
    (setq sq13 (car(cdr(cdr(cdr(cdr (cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr board))))))))))))))
    (setq sq14 (car(cdr(cdr(cdr(cdr (cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr board)))))))))))))))
    (setq sq15 (car(cdr(cdr(cdr(cdr (cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr board))))))))))))))))
    (setq sq16 (car(cdr(cdr(cdr(cdr (cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr board)))))))))))))))))
    
    ;test condittion uses the first element as an anchor to test if the other letters match what is in the first element
    (cond 
        ((and (eq sq1 sq2) (eq sq1 sq3) (eq sq1 sq4) (eq sq1 (car player)) T)) ;first horizontal row has the same letters
        ((and (eq sq5 sq6) (eq sq5 sq7) (eq sq5 sq8) (eq sq5 (car player)) T)) ;second horizontal row has the same letetrs
        ((and (eq sq9 sq10) (eq sq9 sq11) (eq sq9 sq12) (eq sq9 (car player)) T)) ;third horizontal row has the same letters
        ((and (eq sq13 sq14) (eq sq13 sq15) (eq sq13 sq16) (eq sq13 (car player)) T)) ;fourth horizontal row has the same letters
        ((and (eq sq1 sq5) (eq sq1 sq6) (eq sq1 sq13) (eq sq1 (car player)) T)) ;first vertical row has same letters
        ((and (eq sq2 sq6) (eq sq2 sq10) (eq sq2 sq14) (eq sq2 (car player)) T)) ;second vertical row has same letters
        ((and (eq sq3 sq7) (eq sq3 sq11) (eq sq3 sq15) (eq sq3 (car player)) T)) ;third vertical row has same letters
        ((and (eq sq4 sq8) (eq sq4 sq12) (eq sq4 sq16) (eq sq4 (car player)) T)) ;fourth vertical row has same letters
        ((and (eq sq4 sq7) (eq sq4 sq10) (eq sq4 sq13) (eq sq4 (car player)) T)) ;diagonal test 1
        ((and (eq sq1 sq6) (eq sq1 sq11) (eq sq1 sq16) (eq sq1 (car player)) T)) ;diagonal test 2
    )


)

;minmax function will give us tthe best action the CPU will take 
;by going through all possible actions with a limitation with the depth (to make sure 
;the search does not go on forever)
(defun minmax (board depth isMax player1 player2)
    (setq total 0) ;itterator through the board
    (cond 
        ((or (eq depth 0) (eq (TERMINAL-TEST player2) T) 1)) ;if depth is 0 and we have a victory returrn 1
        ((or (eq depth 0) (eq (TERMINAL-TEST player1) T) -1)) ;if depth is 0 and we have a victory returrn -1
        (t (TERMINAL-TEST2 *)) ;draw test
    )   



    (cond 
        ((eq isMax 1) ;if we are maximizing the player
            (setq bestScore -1) ;negative infinity score
            (loop for x in board
                do (cond ((eq x *)  (setq score (minmax board (- depth 1) -1 player1 player2)) (cond ((> score bestScore))(setq bestScore (score))))) ;if blank, call function recursivley and decrease value of depth and set bestScore
                                                                                                                                                      ;to recursive call if it is bigger
            ))
        (t
            (setq bestScore 1) ;not maximizing
            (loop for x in board
                do (cond ((eq x *) (setq score (minmax board (- depth 1) 1 player1 player2)) (cond ((< score bestScore))(setq bestScore (score)))));if blank, call function recursivley and decrease value of depth and set bestScore
                                                                                                                                                      ;to recursive call if it is smaller
                    
            ))
        )
        bestScore ;return best score
)

;function play is a loop that will run aprximatley 16 times and switches
;between cpu and player
(defun play (player player2)
    (setq number 0)
    (loop for x in board
        do (cond ((or (eq (computerMove  player player2 board) 2) (eq (userPrompt player) 1))  ;if computerMove gives us back 2, then we end the game, for player, 1 ends the game
            (return))))
)

;computer move will evaluate the best possible placement for the CPU and when generated, sends it to the results function
(defun computerMove (player1 player2 board)
    (setq bestMove 1) ;bestMove set to 1 initially (if set to 0, the CPU will occupy two spaces before and this will cause errors this was the only way it could act somewhat normally) 
                      ;NOTE: Giving the CPU the bestMove of 1 will cause the index at board 0 to not be occupied even if the player requests it
    (setq bestScore 0) ;bestScore set to 0
    (setq total 0) ;itterator
    (loop for x in board
        ;if the space at x is empty, add O to the board and calculate the score then reset the value to blank and see if the minmax produced a better move than the initial one, if so, bestmove is now at total (list index)
        do(cond ((eq x (car(cdr player2))) (setf (nth total board) (car player2)) (setq score (minmax board 5 0 player1 player2)) (setf (nth total board) (car(cdr player2))) (cond ((> score bestScore) (setq bestScore score) (setq bestMove total))) ))
        (incf total)
    )

    (RESULT player2 bestMove) ;place letter on board

)

;function userPrompt will ask user for a position and then put it on the board
(defun userPrompt (player)
    (boardState)
    (print "Enter a position for X: ")
    (setq position (read))
    (RESULT player position)
    
    
)


;misc var
(setq player '(X)) ;list with player symbol
(setq player2 '(O *)) ;list with cpu and blank symbol

(play player player2) ;play function called to start game


(terpri)
(boardState) ;last state printed at game end 

;Sample Run 1
;|*|O|*|*|
;---------
;|*|*|*|*|
;---------
;|*|*|*|*|
;---------
;|*|*|*|*|

;"Enter a position for X: " 13
;|*|O|O|*|
;---------
;|*|*|*|*|
;---------
;|*|*|*|*|
;---------
;|X|*|*|*|

;"Enter a position for X: " 14
;|*|O|O|O|
;---------
;|*|*|*|*|
;---------
;|*|*|*|*|
;---------
;|X|X|*|*|

;"Enter a position for X: " 15
;|*|O|O|O|
;---------
;|O|*|*|*|
;---------
;|*|*|*|*|
;---------
;|X|X|X|*|

;"Enter a position for X: " 16

;"Winner" 
;|*|O|O|O|
;---------
;|O|*|*|*|
;---------
;|*|*|*|*|
;---------
;|X|X|X|X|

;Sample Run 2
;|*|O|*|*|
;---------
;|*|*|*|*|
;---------
;|*|*|*|*|
;---------
;|*|*|*|*|

;"Enter a position for X: " 3
;|*|O|X|O|
;---------
;|*|*|*|*|
;---------
;|*|*|*|*|
;---------
;|*|*|*|*|

;"Enter a position for X: " 5
;|*|O|X|O|
;---------
;|X|O|*|*|
;---------
;|*|*|*|*|
;---------
;|*|*|*|*|

;"Enter a position for X: " 7
;|*|O|X|O|
;---------
;|X|O|X|O|
;---------
;|*|*|*|*|
;---------
;|*|*|*|*|

;"Enter a position for X: " 9
;|*|O|X|O|
;---------
;|X|O|X|O|
;---------
;|X|O|*|*|
;---------
;|*|*|*|*|

;"Enter a position for X: " 11
;|*|O|X|O|
;---------
;|X|O|X|O|
;---------
;|X|O|X|O|
;---------
;|*|*|*|*|

;"Enter a position for X: " 13

;"CPU Wins" 
;|*|O|X|O|
;---------
;|X|O|X|O|
;---------
;|X|O|X|O|
;---------
;|X|O|*|*|
