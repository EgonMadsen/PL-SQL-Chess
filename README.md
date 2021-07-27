# PL/SQL Chess Engine v0.92 (download the newest [Release 1.0](https://github.com/EgonMadsen/PL-SQL-Chess/releases/tag/v1.0) under 'release')

This freeware chess engine, written in PL/SQL by Egon Von Bech Madsen for Oracle Database, consists of the following packages:

* PL\_PIG\_CHESS\_ENGINE
* PL\_PIG\_CHESS\_ENGINE\_EVAL
* PL\_PIG\_CHESS\_DATA
* PL\_PIG\_CHESS\_INTERFACE

## Technical Overview

### PL/SQL

PL/SQL is a quite restricted and slow language to use for a Chess Engine.
If You want top strength, don't use PL-PIG-Chess. Engines like StockFish (C language) is able to calculate around 100 times faster.

Fair Performance is reached by intensive use of VARRAYS, SIMPLE\_INTEGER and INLINEable SET operators. And by NOT using SQL features. The many initiations of arrays happens only one time pr session/game.

### Chess Engine

* Opening books (internal plus polyglot books)
* Static evaluation
* quiescence deepening minimax algorithm with alpha/beta cut-off and sort for better pruning
* Q-search deepening (deeper selective search on killer-moves like check, attacks, promotion, double-threats and pinning themes)
* Leaf (dynamic) evaluation
* Single CPU only
* Supports 5 levels (from few seconds to several minutes pr move).
* Rating: up to 1600 ( [v1.0](https://github.com/EgonMadsen/PL-SQL-Chess/releases/tag/v1.0) up to 1900)
* PGN, FEN, EPD and Polyglot open chess formats is supported.
* All code is original and made from the bottom, mostly based on widespread ideas though.
* Automated test-suite to tune weights and features for best rating (the most time-consuming and critical part of coding an engine) 

## INSTALLATION
Install the 4 packages on a Oracle database:

1. PL\_PIG\_CHESS\_ENGINE
2. PL\_PIG\_CHESS\_ENGINE\_EVAL
3. PL\_PIG\_CHESS\_DATA
4. PL\_PIG\_CHESS\_INTERFACE

USE:

1. Use SQL*PLUS or Toad/SQL-Developer with DBMS\_OUTPUT activated
2. Use examples seen in PL\_PIG\_CHESS\_INTERFACE

OR install the GUI PL\_PIG\_CHS\_GUI or
maybe make your own graphical (APEX?) interface...

## [License] (https://github.com/EgonMadsen/PL-SQL-Chess/blob/master/LICENSE)

This is distributed under the Diceware License.
