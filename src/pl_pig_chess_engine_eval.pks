DROP PACKAGE PL_PIG_CHESS_ENGINE_EVAL;

CREATE OR REPLACE PACKAGE PL_PIG_CHESS_ENGINE_EVAL AS
--
--

-- LICENSE
--
-- PL_PIG_CHESS_ENGINE v 0.92,                  (c) Egon Von Bech Madsen, Denmark (ema@adm.ku.dk) 
--
-- This software is Diceware. Diceware is defined as Freware with one restriction:
--
--   For non-commercial use:
--      roll one dice until you get 6, then take a picture. That picture is your 10 year free license.
--
--   For commercial use of small or medium sized companies (Yearly revenue less than US$ 60 billion), like Oracle:
--      roll two dices until you get 6 6, then take a picture. That picture is your 10 year free license.
--
--   For commercial use of large sized companies (Yearly revenue more than US$ 60 billion), like Microsoft:
--      roll three dices until you get 6 6 6, then take a picture. That picture is your 10 year free license.
--
-- It is allowed to modify and share the code as long as this LICENSE-text is unmodified.
--
-- ****************************************************************************************
-- * Disclaimer (use at your own risk):                                               
-- * Any use of this software is strictly at your own risk, and I will not be liable for 
-- * any losses and damages in connection with the use of this software.
-- ****************************************************************************************
-- LICENSE <end>

-- PL_PIG_CHESS_ENGINE is a Chess Engine written in Oracle PL/SQL.
--
-- HISTORY
-- Migrated from the Engine of PIGbase4 v2.8, coded with M2Amiga Modula-2 v4.2 
--
-- The Engine is 3 packages:
-- PL_PIG_CHESS_ENGINE
-- PL_PIG_CHESS_ENGINE_EVAL
-- PL_PIG_CHESS_DATA
--
-- Also included is a package:
-- PL_PIG_CHESS_INTERFACE 
-- making it possible to play with the Engine and run some test-suites using DBMS_OUTPUT.

-- TECHNICAL (PL/SQL)
-- PL/SQL is a quite restricted and slow language, not the perfect choice for a Chess Engine.
-- If You want top strength, don't use PIG-Chess, but install a Stockfish Engine at Your HOST OS.
--
-- Fair Performance is reached by intensive use of VARRAYS, SIMPLE_INTEGER and INLINE'able SET operators. 
-- And by NOT using SQL features. The many initiations of arrays happens only one time pr session/game.
--
-- TECHNICAL (CHESS)
-- Opening book
-- Static evaluation
-- quiescence deepening minimax algorithm with alpha/beta cut-off and sort for better pruning
-- Q-search (deeper selective search on killer-moves like check)
-- Leaf evaluation
-- Single CPU only
-- Supports 5 levels (from few seconds to several minutes pr move).
-- Rating: up to 1600
-- FEN and EPD is supported.
--
-- INSTALLATION
-- Install the 4 packages on a Oracle database:
-- 1. PL_PIG_CHESS_DATA
-- 2. PL_PIG_CHESS_ENGINE_EVAL
-- 3. PL_PIG_CHESS_ENGINE
-- 4. PL_PIG_CHESS_INTERFACE
--
-- USE:
-- 1. Use SQL*PLUS or Toad/SQL-Developer with DBMS_OUTPUT activated
-- 2. Use examples seen in PL_PIG_CHESS_INTERFACE
--
-- OR:
-- Make your own graphical (APEX?) interface...




--
--To get best performance (depending of database and hardware, up to double speed):
--alter session set plsql_optimize_level =3; 
--alter package PL_PIG_CHESS_ENGINE_EVAL compile plsql_code_type=native plsql_debug=FALSE;
--
--fra Base:
  HvisTur   CONSTANT SIMPLE_INTEGER  := 110; /* Stilling[HvisTur] indicates if H (white) or S (Black) to move */
  MaxExtras CONSTANT SIMPLE_INTEGER  := 22; /* How many Extra PGN infos max (max 34) */
TYPE STILLINGTYPE is ARRAY(121) OF SIMPLE_INTEGER; /* in PL/SQL is offset 11, logical: -10..HvisTur: A1-H8 = 11-88, HvidSort='H' | 'S' CAP=BLACK */

--logical structure for a position :
--    RECORD
--      stilling :ARRAY(-1..11,-1..11] OF BRIK;
--      hvistur  :BOOLEAN;
--    END;
--  where 1,1 -> 8,8  A1 -> H8 , so 11-88 is A1-H1
--  surrounded with a double edge ('.'). 
--
--------------------------------
SkakBrainEvalDefCompilation CONSTANT varchar2(8) := '17';
-- naming according to danish names:
  ValueT  CONSTANT SIMPLE_INTEGER  := 475; --rook: danish: Tårn
  ValueR  CONSTANT SIMPLE_INTEGER  := 480; --rook (castling possible) danish: tårn (Rokade)
  ValueM  CONSTANT SIMPLE_INTEGER  :=9999; --king (castling possible) danish: Konge (Majestæt)
  ValueK  CONSTANT SIMPLE_INTEGER  :=9980; --king danish: Konge
  ValueD  CONSTANT SIMPLE_INTEGER  := 880; --Queen danish: Dronning
  ValueL  CONSTANT SIMPLE_INTEGER  := 300; --Bishop danish: Løber
  Value_S CONSTANT SIMPLE_INTEGER  := 280; --knight danish: Springer
  ValueB  CONSTANT SIMPLE_INTEGER  := 100; --pawn danish: Bonde
  ValueE  CONSTANT SIMPLE_INTEGER  := 100; --pawn (en-passant) danish: bonde (En-passant) 
-- special: the pieces will internally change names to implicit register castling and en-passant.
--               If capitalized it is a black piece else a white piece.
--               The advantage is simpler move-generation (like if rook og king is never moved 
--               then no need to check backwards moves, but king can go two to sides).
--               As bonus, a  position will be exact without need for ekstra castling/enpassant infos.

  pdSz  CONSTANT SIMPLE_INTEGER  := 3978;
TYPE pdType is varray(3978) of SIMPLE_INTEGER;--3978 needed for two-dimensional array ('B'..'t', 11..88)
-- pl/sql specifics:
-- arrays index is always 1..n and always needs to allocate members using extend. 
-- PLS_INTEGER is 32 bit integer (signed) 
-- SIMPLE_INTEGER is 32 bit integer with NOT NULL constraint
-- SIMPLE_INTEGER gives twice the speed if pl/SQL is compiled, not interpreted.
-- SIMPLE_INTEGER NEEDS to be initialized with value!
--
--TYPE ysy IS RECORD (a CHAR, b SIMPLE_INTEGER);
  Evals SIMPLE_INTEGER := 0;/* for current/last engine call */
  OpenGame BOOLEAN;		/* calculated by preprocessor 	*/
  EndGame    BOOLEAN;		/* calculated by preprocessor 	*/

  pd  pdType:= pdType(); --pointer allocates a liste with 0 elements
                         --pointer allocates a liste with 40 elements:
  pdw pdType:= pdType(); --pdType(0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0) ;
  pdb pdType:= pdType(); -- pointer is null

  ToFile   BOOLEAN;
  Depth    SIMPLE_INTEGER:= 0;
  matr     SIMPLE_INTEGER:= 0;
  posi     SIMPLE_INTEGER:= 0;
  wbonus   SIMPLE_INTEGER:= 0;
  bbonus   SIMPLE_INTEGER:= 0; 
  /* see Eval here, for testing only */
  
  KingAir         BOOLEAN := TRUE; -- safety strategy, make escape-space for king, if risk of mat on first row. Default TRUE
  defendersWeight SIMPLE_INTEGER := 5;-- default=5     motivate to make hole for king when few 1-line defenders and behind wall of 3 pawns. Only used when KingAir=TRUE.
  Follow          BOOLEAN := TRUE; -- mating strategy, make king follow opponent king.  Default TRUE
  PieceClear      BOOLEAN := FALSE;-- Clear some new calcs for king, default FALSE
  PieceClearEarly BOOLEAN := TRUE;-- Clear some new calcs for king at early point, default TRUE
  PieceDecr       BOOLEAN := FALSE;-- decrement all positional data with 50, default FALSE
  PawnOfficer     BOOLEAN := TRUE; -- if in front then prefer exchange of officers, else pawns. default TRUE
  
FUNCTION pdN(brik_n SIMPLE_INTEGER, felt SIMPLE_INTEGER) RETURN SIMPLE_INTEGER;

FUNCTION pdX(brik CHAR,           felt SIMPLE_INTEGER) RETURN SIMPLE_INTEGER;

PROCEDURE Initialize;
/* allocation and initializations, call on startup */
/* Is called one time for each engine call */

PROCEDURE PreProcess;
/* sets the pd array to default positional values */
/* Is called one time for each engine call */

PROCEDURE PreProcessor(stilling STILLINGTYPE);
/* adjusts the pd-array+OpenGame+EngGame according to the actual position. */
/* Is called one time for each engine call */

FUNCTION Eval(stilling STILLINGTYPE, Activity SIMPLE_INTEGER,
              Black BOOLEAN, alpha SIMPLE_INTEGER, beta SIMPLE_INTEGER) RETURN SIMPLE_INTEGER;
/* evaluates the actual postion using pd and position. */
/* Is called thousands of times for each engine call! */

END;
/
