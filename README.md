*Group members*: Nitya Davarapalli (ndavarapalli@ucsd.edu), Ziheng Liu (zil060@ucsd.edu)

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


![How the rotate works](https://github.com/lzhfromustc/cse230/doc/RotateGomoku.webp)
