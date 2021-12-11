**Gomoku**

This project implements a Gomoku Terminal User Interface game in Haskell using the Brick library. This is a two player game, with two users sharing the same computer (no network). The game starts when the first player moves the cursor in order to choose a spot on the board to place the piece. Places can be pieced on the board using the spacebar. Players alternate turns and place white and black pieces on the board until one player wins. If the new piece that is placed creates a consecutive line of 5+ pieces in a row/column/diagonal, the corresponding player wins. Each player is allowed one move per round. Calculation of a possible winner happens after every turn, when a player places a piece.

**Playing Gomoku**

Running this game on the terminal interface:
1. git clone this repo to your local machine.
2. cd cse230
3. cabal build
4. cabal run gomoku


**Milestone 2**

We are still learning how to use Brick from its Guide, its demo programs, and some projects built on it.

The structure of our program is learnt from a Snake game: https://github.com/jtdaugherty/brick/blob/master/docs/samtay-tutorial.md

1. Data types:

`data Game` in `Main.hs` denotes the state of game. It is also the `s` in `App s e n` required by Brick. It is based on some other data types like `Piece` which stands for each piece already placed on the board, consisting of its owner and coordinator.

`App s e n` is required by Brick. In our case, `s` is just the `Game`. As for `e`, We think we don't need a custom event here, based by my understanding of ListDemo and VisibilityDemo. We are still learning about `n` for resources.

2. Transformation of state:

We have `moveCursor`, `placePiece`, `illegalPlace`, and `closeMsg` functions to transform a state. 

`moveCursor` changes the state by moving the cursor.

`placePiece` changes the state by placing a piece on an empty Cell. We will check whether someone wins in this step.

`illegalPlace` changes the state by displaying a warning message, saying the player can't place a piece on an occupied Cell.

`closeMsg` changes the state by deleting the warning message.

3. Constants:

There are some constants used, including the size of the board and how many pieces needed to win. We may allow the user to input them if we have time.

4. TODO:

We need to learn how the event is handled, how the cursor is implemented, and many other stuff.

We also need to learn how to build a program with cabal.

As the change of plan, the following will be implemented if we have time.
(1) allow the user to decide the size of board and winning condition; 
(2) allow the rotation of 1/4 of the board; 
(3) a very simple AI opponent who can block your pieces if you have 3 pieces in line.

*Group members*: Nitya Davarapalli (ndavarapalli@ucsd.edu), Ziheng Liu (zil060@ucsd.edu)
