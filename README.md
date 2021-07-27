# PL/SQL Chess Engine v0.92 (download the newest [Release 1.0](https://github.com/EgonMadsen/PL-SQL-Chess/releases/tag/v1.0) under 'release')

This freeware chess engine, written in PL/SQL by Egon Von Bech Madsen for Oracle Database, consists of the following packages:

* PL\_PIG\_CHESS\_ENGINE
* PL\_PIG\_CHESS\_ENGINE\_EVAL
* PL\_PIG\_CHESS\_DATA
* PL\_PIG\_CHESS\_INTERFACE

## Technical Overview

### PL/SQL

PL/SQL is a quite restricted and slow language, not the perfect choice for a Chess Engine.
If You want top strength, don't use PL-PIG-Chess.

Fair Performance is reached by intensive use of VARRAYS, SIMPLE\_INTEGER and INLINEable SET operators. And by NOT using SQL features. The many initiations of arrays happens only one time pr session/game.

### Chess Engine

* Opening book
* Static evaluation
* quiescence deepening minimax algorithm with alpha/beta cut-off and sort for better pruning
* Q-search (deeper selective search on killer-moves like check)
* Leaf evaluation
* Single CPU only
* Supports 5 levels (from few seconds to several minutes pr move).
* Rating: up to 1600
* FEN and EPD is supported.

## INSTALLATION
Install the 4 packages on a Oracle database:

1. PL\_PIG\_CHESS\_ENGINE
2. PL\_PIG\_CHESS\_ENGINE\_EVAL
3. PL\_PIG\_CHESS\_DATA
4. PL\_PIG\_CHESS\_INTERFACE

USE:

1. Use SQL*PLUS or Toad/SQL-Developer with DBMS\_OUTPUT activated
2. Use examples seen in PL\_PIG\_CHESS\_INTERFACE

OR:
Make your own graphical (APEX?) interface...

## License

This is distributed under the Diceware License.
