*Group members*: Nitya Davarapalli (ndavarapalli@ucsd.edu), Ziheng Liu (zil060@ucsd.edu)

Running this game on the terminal interface:
1. git clone this repo to your local machine.
2. cd cse230
3. cabal build
4. cabal run gomoku


**Milestone 2**
(This section will be moved below other sections later.)

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



**Introduction**

This project intends to implement a Gomoku game in Haskell using the Brick library. There will be two players in this game, sharing the same computer (no network). In each round, a player can choose a spot on the board to place the piece, or rotate 1/4 of the board. If the new piece or the rotation creates a consecutive line of 5+ pieces in a row/column/diagonal, their owner wins. 

*Note*: To the best of our knowledge, this game has not been implemented in Brick before. Moreover, we have the rotating mechanism added to traditional Gomoku.

**Goals**
1. Display a 2N * 2N board with N input by user.
2. Move the cursor on the board by keyboard input.
3. Place a piece by keyboard input, with black or white color alternately.
4. Choose and rotate 1/4 of the board by keyboard input.
5. Only allow one movement for one player per round.
6. After placing a piece or rotating, calculate whether one player has won.


![How the rotate works](https://github.com/lzhfromustc/cse230/blob/main/doc/RotateGomoku.webp)
