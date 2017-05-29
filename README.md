# five-in-a-row
A Five in a row/Gomoku board game implemented in Lisp from scratch. The rule of this board game is simple and is analogous to those of 
Tic-Tac-Toe -- the first player who connects five of the same pieces together wins. </br> 
The rule models both a `machine-to-machine` gameplay and a `machine-to-human` gameplay.</br>
Main heuristics used here are the Minimax Algorithm and a self-designed board evaluation function. </br>
`gomoku.l` - The file contains the gomoku game. </br>
`lp.l` - A Utility file contains a bunch of lisp processing functions. Only function snoc (by attaching an element to the end of a given list)
is used in `gomoku.l`. </br>

# Execution
You need to download `CLISP` for your own personal edition, and then use the following command lines: <br>
- Start CLISP environment: <br>
`clisp` </br>
- Load file by using: </br>
`(load "gomoku.l")` </br>
- To start a machine-to-machine gameplay use: </br>
`(demo-machine-machine)` </br>
****NOTE: </em> giving the function an optional parameter (number) takes control of how many moves in total a user want the program to perform.
For example, (demo-machine-machine 15) would lead to termination of the program after 15 performed moves (both machine players in total). </br>
- To start a machine-to-human gameplay use :</br>
`(demo-machine-human)`</br>

# Future Plans
1. Performance improvement. Cache gameplays to reduce processing time.</br>
2. Gameflow improvement. E.g. Cases when users type in invalid commands. </br>
3. GUI for easier interaction. </br>

# Links
For more information (slides and informal write-up) about the project and more detailed tasks breakdown, 
please see http://cs.oswego.edu/~yxia/coursework/csc466/project/index.html
