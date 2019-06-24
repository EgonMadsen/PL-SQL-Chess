# PL-SQL-Chess
Chess Engine made in PL/SQL


PL_PIG_CHESS is a Chess Engine written in Oracle PL/SQL. (c) Egon Von Bech Madsen, Denmark

It works on Oracle databases only.

It contains 4 packages:
PL_PIG_CHESS_ENGINE
PL_PIG_CHESS_ENGINE_EVAL
PL_PIG_CHESS_DATA
PL_PIG_CHESS_INTERFACE 

TECHNICAL (PL/SQL)
PL/SQL is a quite restricted and slow language, not the perfect choice for a Chess Engine.
If You want top strength, don't use PIG-Chess, but install a Stockfish Engine at Your HOST OS.

Fair Performance is reached by intensive use of VARRAYS, SIMPLE_INTEGER and INLINE'able SET operators. 
And by NOT using SQL features. The many initiations of arrays happens only one time pr session/game.

TECHNICAL (CHESS)
Opening book
Static evaluation
quiescence deepening minimax algorithm with alpha/beta cut-off and sort for better pruning
Q-search (deeper selective search on killer-moves like check)
Leaf evaluation
Single CPU only
Supports 5 levels (from few seconds to several minutes pr move).
Rating: up to 1600
FEN and EPD is supported.

INSTALLATION
Install the 4 packages on a Oracle database:
1. PL_PIG_CHESS_DATA
2. PL_PIG_CHESS_ENGINE_EVAL
3. PL_PIG_CHESS_ENGINE
4. PL_PIG_CHESS_INTERFACE

USE:
1. Use SQL*PLUS or Toad/SQL-Developer with DBMS_OUTPUT activated
2. Use examples seen in PL_PIG_CHESS_INTERFACE

OR:
Make your own graphical (APEX?) interface...


