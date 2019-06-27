DROP PACKAGE PL_PIG_CHESS_INTERFACE;

CREATE OR REPLACE PACKAGE PL_PIG_CHESS_INTERFACE AS
--
-- PL_PIG_CHESS_INTERFACE v 0.92
--
-- Interface for play with PL_PIG_CHESS_ENGINE using DBMS_OUTPUT
--
--
-- EXAMPLES (DBMS_OUTPUT must be active):
--
-- Example 1, play black against engine (low level):
-- 1.       BEGIN NEW_GAME; END;
-- 2..n     BEGIN DO_MOVE('e7e5'); END;
--
-- Example 2, play white against engine (high level):
-- 1.       BEGIN NEW_GAME(0,6); END;
-- 2..n     BEGIN DO_MOVE('e2e4'); END;
--
-- Example 3, play engine (medium level) manually against engine (low level):
-- 1.       BEGIN NEW_GAME(4,2); END;
-- 2..n     BEGIN DO_BOTMOVE; END;
--
-- Example 4, play engine (medium level) against engine (low level):
-- 1.       BEGIN NEW_GAME(4,2); END;
-- 2        BEGIN DO_BOTGAME; END;
--
-- Example 5, Let engine find the best move in a given FEN position (high level):
-- 1.       BEGIN NEW_GAME(4,4,'7k/p7/1R5K/6r1/6p1/6P1/8/8 w - - 0 1'); END;
--
-- Example 6, run a test-suite (24 positions) on low level and see the result
-- 1.       BEGIN test_BKtest(2); END;
--
-- Example 7, run a test-suite (30 positions) on medium level and see the result
-- 1.       BEGIN test_ColditzTest(4); END;

PROCEDURE NEW_GAME(
  White INTEGER  DEFAULT 2,                -- 0=human, 2=low,  4=medium, 6=high (engine strength/timeusage)
  Black INTEGER  DEFAULT 0,                -- 0=human, 2=low,  4=medium, 6=high (engine strength/timeusage)
  STARTPOSITION      VARCHAR2 DEFAULT NULL,-- NULL= chess startposition 'RSLDMLSRBBBBBBBB                                bbbbbbbbrsldmlsrH'
  p_TheoryMode       INTEGER  DEFAULT 0,   -- 0 = no theory, 
                                           -- 1 = internal theory (random play 2 ply, 16 lines)
                                           -- 2 = internal opening book (random play of about 3000 opening-lines)   
  p_InteractionMode  INTEGER  DEFAULT 1    -- 0 = dbms_output (moves only)
                                           -- 1 = dbms_output (with positions)
                                           -- 2 = table (NOT IMPLEMENTED!)
                                           -- 3 = tables(asyncronous) (NOT IMPLEMENTED!)
  );
  
PROCEDURE DO_MOVE(fromto VARCHAR2); -- move in the form 'e2e4' or 'g1f3' as black 'e7e5' or 'g8f6'

PROCEDURE DO_BOTMOVE(OverruleLevel SIMPLE_INTEGER DEFAULT 0);

PROCEDURE DO_BOTGAME(maxmoves SIMPLE_INTEGER DEFAULT 200);

PROCEDURE SET_White(White INTEGER  DEFAULT 0); -- alter engine/player-selection for White

PROCEDURE SET_Black(Black INTEGER  DEFAULT 0); -- alter engine/player-selection for Black

PROCEDURE TAKEBACK_MOVE;--(not yet implemented!) takes back the last move (if engine, it will move again, possibly the same move)

PROCEDURE TAKEBACK_MOVES;--(not yet implemented!) takes back the last two moves (Player can retry another move)

---- for testing:

-- suites of test-positions:

  -- ARRAY          POSITIONS   TEST-SUITE IN EPD FORMAT:
  -------------------------------------------------------------------------------------
  -- bkTest	            24      The B-K test (henceforth BKT) Bratko-Kopec Testpositions
  -- MSquickTest	    24      The Quicktest by Michael Scheidl 
  -- THmyPosTest	    16      MY POSITIONAL TEST SUITE by Tony Hedlund
  -- SLendgameTest      20      Endgame testsuite Sune Larsson 2006 / John Nunn	
  -- CCRTest	        25      One Hour Test by Larry Kaufman, published 1994 (Kaufman Test)
  -- ColditzTest	    30      Colditz test suite by Ferdinand Mosca, CCC, December 30, 2016
  -- BBCTest	        42      Big Book Of Combinations
  -- ReinfeldTest      300      Reinfeld's (1945) 300 (tactical) positions; 
  -- LCTIITest	        35      LCT II (Louguet Chess Test II by Frdric Louguet in 1994) 
  -- SBDTest	       134      Silent but Deadly (sbd)
  -- STSTest	      1500      Strategic Test Suite, (STS) 15 suites x 100 positions:
  --   1  Undermining 
  --   2  Open Files and Diagonals, Rook on Open File
  --   3  Knight Outposts, Outposts
  --   4  Square Vacancy, Square Control
  --   5  Bishop vs Knight
  --   6  Re-Capturing, Captures
  --   7  Offer of Simplification, Material
  --   8  Advancement of f/g/h pawns, Pawn Storm
  --   9  Advancement of a/b/c Pawns, Pawn Storm
  --  10  Simplification
  --  11  Activity of the King, King Safety vs King Centra	
  --  12  Center Control
  --  13  Pawn Play in the Center, Pawn Center
  --  14  Queens and Rooks 7th Rank, Rook on Seventh		
  --  15  Avoid Pointless Exchange

PROCEDURE test_BKtest(       lvl NUMBER DEFAULT 2, poslow INTEGER DEFAULT 1 , poshigh INTEGER DEFAULT  24);

PROCEDURE test_MSquickTest(  lvl NUMBER DEFAULT 2, poslow INTEGER DEFAULT 1 , poshigh INTEGER DEFAULT  24);

PROCEDURE test_THmyPosTest(  lvl NUMBER DEFAULT 2, poslow INTEGER DEFAULT 1 , poshigh INTEGER DEFAULT  16);

PROCEDURE test_SLendgameTest(lvl NUMBER DEFAULT 2, poslow INTEGER DEFAULT 1 , poshigh INTEGER DEFAULT  20); 

PROCEDURE test_CCRTest(      lvl NUMBER DEFAULT 2, poslow INTEGER DEFAULT 1 , poshigh INTEGER DEFAULT  25);

PROCEDURE test_ColditzTest(  lvl NUMBER DEFAULT 2, poslow INTEGER DEFAULT 1 , poshigh INTEGER DEFAULT  30);

PROCEDURE test_BBCTest(      lvl NUMBER DEFAULT 2, poslow INTEGER DEFAULT 1 , poshigh INTEGER DEFAULT  42);

PROCEDURE test_ReinfeldTest( lvl NUMBER DEFAULT 2, poslow INTEGER DEFAULT 1 , poshigh INTEGER DEFAULT 300);

PROCEDURE test_LCTIITest(    lvl NUMBER DEFAULT 2, poslow INTEGER DEFAULT 1 , poshigh INTEGER DEFAULT  35);

PROCEDURE test_SBDTest(      lvl NUMBER DEFAULT 2, poslow INTEGER DEFAULT 1 , poshigh INTEGER DEFAULT 134);

PROCEDURE test_PIG(          lvl NUMBER DEFAULT 2, poslow INTEGER DEFAULT 1 , poshigh INTEGER DEFAULT   4);

PROCEDURE test_STSTest(suite NUMBER, lvl NUMBER DEFAULT 2, poslow INTEGER DEFAULT 1 , poshigh INTEGER DEFAULT 100);

PROCEDURE test1;

PROCEDURE test2;

END;
/
