DROP PACKAGE PL_PIG_CHESS_ENGINE;

CREATE OR REPLACE PACKAGE PL_PIG_CHESS_ENGINE AS
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

  SkakBrainDefCompilation CONSTANT VARCHAR2(8) := '51';
  MaxHalvTraek            CONSTANT SIMPLE_INTEGER :=  600; -- 180,200? max moves

-- results from last loaded EPD position, a loaded FEN position will clear them
EPD_BESTMOVE        VARCHAR2(80); --bm -- here we can expect several moves
EPD_SECONDARYMOVE   VARCHAR2(80); --sm -- here we can expect several moves
EPD_AVOIDMOVE       VARCHAR2(80); --am -- here we can expect several moves
EPD_ID              VARCHAR2(30); --id
EPD_COMMENT0        VARCHAR2(255);--c0: primary comment

SUBTYPE MOVETYPE IS SIMPLE_INTEGER;
MOVEnormal    CONSTANT MOVETYPE := 0;
MOVEenpassant CONSTANT  MOVETYPE := 1;
MOVErokade    CONSTANT  MOVETYPE := 2;
MOVEpat       CONSTANT  MOVETYPE := 4;
MOVEmat       CONSTANT  MOVETYPE := 8;
MOVEslag      CONSTANT  MOVETYPE := 16;
MOVEskak      CONSTANT  MOVETYPE := 32;
MOVEpromotion CONSTANT  MOVETYPE := 64;
MOVEx7        CONSTANT  MOVETYPE := 128;

SUBTYPE ATTRTYPE IS SIMPLE_INTEGER;
ATTRman    CONSTANT  ATTRTYPE := 1;
ATTRe1     CONSTANT  ATTRTYPE := 2;
ATTRe2     CONSTANT  ATTRTYPE := 4;
ATTRe4     CONSTANT  ATTRTYPE := 8;
ATTRa4     CONSTANT  ATTRTYPE := 16;
ATTRa5     CONSTANT  ATTRTYPE := 32;
ATTRa6     CONSTANT  ATTRTYPE := 64;
ATTRa7     CONSTANT  ATTRTYPE := 128;

  -- {mat}=skakmat (checkmate), {x7}=draw on repetition 
 --TYPE MOVETYPES IS (enpassant,rokade,pat,mat,slag,skak,promotion,x7); rokade=castling, pat=stalemate, slag=take, skak=check
 --TYPE  MOVETYPE IS SET OF MOVETYPES;
 --TYPE  ATTRTYPES IS (man,e1,e2,e4,a4,a5,a6,a7); -- lv1={e1} lv4={e4} lv7={e1,e2,e4} 
 --TYPE  ATTRTYPE IS SET OF ATTRTYPES;            -- a4 should be e8, to get lv 1-15  
					-- so use a5-a7 for xtras *)

  TYPE Xtyp IS VARRAY(3) of SIMPLE_INTEGER;
  TYPE XYtyp IS VARRAY(121) of Xtyp;
  tst XYtyp := XYtyp(Xtyp(1,23,0),Xtyp(2,3,4),Xtyp(2,3,3),Xtyp(2,3,4),Xtyp(2,3,5));

  TeoMaxPos    CONSTANT SIMPLE_INTEGER := 15000; -- max positions (in openingbook)       DYNAMIC
  TeoMaxTrk    SIMPLE_INTEGER          :=     0; -- max variant length                   CALCULATED
  TeoMaxVar    CONSTANT SIMPLE_INTEGER :=    20; -- max replies in a position            DYNAMIC
  TeoBook      SIMPLE_INTEGER          :=     2; 
      -- 0 = none, 
      -- 1 = Micro-book  30 x 2
      -- 2 = Mini-book  178 x (2->8)
  
  --TeoMax       CONSTANT SIMPLE_INTEGER :=    9; -- max positions (in micro-openingbook) 5 (now 9)
  --TeoMaxTrk    CONSTANT SIMPLE_INTEGER :=    5; -- max variant length                   5 (now 3)
  --TeoMaxVar    CONSTANT SIMPLE_INTEGER :=    6; -- max replies in the position          4 (now 6)

  stOff     CONSTANT SIMPLE_INTEGER  := 11; --offset for arrays Stilling
  vcxOff    CONSTANT SIMPLE_INTEGER  :=-64; --offset for arrays Stilling ASCII('A')..ASCII('T')
  vcyOff    CONSTANT SIMPLE_INTEGER  :=-31; --offset for arrays Stilling ASCII(' ')..ASCII('T')

  HvisTur   CONSTANT SIMPLE_INTEGER  := 110; -- Stilling[HvisTur] indikates if White ('H') or black ('S') to move 

  --TYPE STILLINGTYPE is VARRAY(121) OF CHAR(1); -- pl/sql offset=11                  -10..HvisTur: A1-H8 =11-88, HvidSort='H' | 'S' 
  TYPE STRINGG IS VARRAY(2000) OF CHAR(1);--ARRAY(0..2000)
  SUBTYPE STRINGptr is STRINGG;

  TYPE TRKDATA IS RECORD (
            Fra SIMPLE_INTEGER := 0,--from (ll-88)
            Til SIMPLE_INTEGER := 0,--to   (11-88)
            Typ MOVETYPE := 0,
            Vlu SIMPLE_INTEGER := 0 --evaluation value (100= a pawn)
            );
  TYPE TRAEKDATA IS  VARRAY(116) OF TRKDATA; -- max  116 possible moves  in a position

  TYPE STIL IS RECORD (
    DomOn SIMPLE_INTEGER := 0, -- -128 | 0 | 1  -- -128=don't change 		
    StyOv SIMPLE_INTEGER := 0, -- -128 | StyO   --    0=man, 1-9=machine (1,4,7) 	
    StyUn SIMPLE_INTEGER := 0, -- -128 | StyU   -- store for StyU 		
    Opad  SIMPLE_INTEGER := 0, -- -128 | 0 | 1  -- store for OpAd (BOOLEAN) norm=T (if board is reversed)
    TrkNr SIMPLE_INTEGER := 0,                    -- store for TraekNr (mivenumber)           	
    Late1 SIMPLE_INTEGER := 0,
    Late2 SIMPLE_INTEGER := 0,
    Late3 SIMPLE_INTEGER := 0,
    Still PL_PIG_CHESS_ENGINE_EVAL.STILLINGTYPE
    );

  TYPE STILLREC IS RECORD (
             Fra       SIMPLE_INTEGER := 0,
             Til       SIMPLE_INTEGER := 0, -- [0] uses start --to   (11-88)
             Tekst     STRINGptr          , -- more linies, split using \ or LF 
             Secs      SIMPLE_INTEGER := 0, -- Seconds used for this move 
             Attribs   ATTRTYPE       := 0, -- who did it? 
             Xtra      SIMPLE_INTEGER := 0  -- future 
           );
  TYPE SPIL IS VARRAY(500) OF STILLREC; -- [0]=start-position  0..!!!!!MaxHalvTraek

  TYPE retnTypeY IS VARRAY(63) OF SIMPLE_INTEGER;--ARRAY[-31..31] pl/sql offset=32
  TYPE retnType IS VARRAY(58) OF retnTypeY;      --ARRAY['B'..'t'] pl/sql offset=-65 CHR()

  TYPE TeoType IS VARRAY(1+ TeoMaxPos) OF PL_PIG_CHESS_ENGINE_EVAL.STILLINGTYPE;
  
  TYPE TeoTTypeY IS VARRAY(TeoMaxVar) OF TRKDATA; 
  TYPE TeoTType IS VARRAY(1+ TeoMaxPos) OF TeoTTypeY;

  ryOff     CONSTANT SIMPLE_INTEGER  :=  32; --offset for arrays retnTypeY
  rxOff     CONSTANT SIMPLE_INTEGER  := -65; --offset for arrays retnTypeX (char)
  
  FindCnt   SIMPLE_INTEGER :=0;
  QFindCnt  SIMPLE_INTEGER :=0;
    
    
  GameStartSeconds  SIMPLE_INTEGER :=0; 
  GameStartMicros   SIMPLE_INTEGER :=0;
  MoveStartSeconds  SIMPLE_INTEGER :=0;
  MoveStartMicros   SIMPLE_INTEGER :=0;
  stVsum   SIMPLE_INTEGER :=0;--: CARDINAL; 
  --Evals    SIMPLE_INTEGER :=0;--: LONGCARD; -- not used, use SkakBrainEval.Evals 
  Push     BOOLEAN;
  stilling  PL_PIG_CHESS_ENGINE_EVAL.STILLINGTYPE;
  startt    STIL;
  EatChar   CHAR(1);
  --
  retn  retnType := retnType();  --ARRAY['B'..'t'] OF ARRAY[-31..31] OF INTEGER; -- direction lookup table
  Teo   TeoType  := TeoType();   --ARRAY[0..TeoMaxPos] OF STILLINGTYPE; (TeoMax=5) OFFSET=1
  TeoT  TeoTType := TeoTType();  --ARRAY[0..TeoMaxPos],[1..TeoMaxVar] (TeoMaxVar=4) OF TRKDATA;OFFSET=1
  DefStill  PL_PIG_CHESS_ENGINE_EVAL.STILLINGTYPE;-- := PL_PIG_CHESS_ENGINE_EVAL.STILLINGTYPE();

  MaxTeori   SIMPLE_INTEGER :=0; -- MaxTeori (openingbook)
  TraekNr    SIMPLE_INTEGER :=0; -- 0..MaxTraek
  
  TFra  SIMPLE_INTEGER :=0;-- The top (recommended) move (from) by theory tree 
  TTil  SIMPLE_INTEGER :=0;-- The top (recommended) move (to) by theory tree 

FUNCTION STILLING_TO_EPD(stilling PL_PIG_CHESS_ENGINE_EVAL.STILLINGTYPE, operationlist VARCHAR2 DEFAULT NULL) RETURN VARCHAR2;
--Converts a position from internal array format to a string in EPD format
--Possible to add EPD operations. Example: 'bm Nf3; id "test1";'

FUNCTION FEN_EPD_TO_STR(FEN_EPD VARCHAR2) RETURN VARCHAR2;
-- converts a position in FEN or EPD format to POSITIONSTR format.

PROCEDURE still(stilling in out PL_PIG_CHESS_ENGINE_EVAL.STILLINGTYPE, 
                p_st char DEFAULT ''); --str65
-- setup a position from a string in POSITIONSTR format (st='' gives start-position). 
-- Converts to internal format if standard FEN or EPD format detected.
-- Internal format, but english will be OK, translated:'PpBbNnQqRrCc' to 'BbLlSsDdTtRr' and Color WB to HS (note: 'c' stands for rook with castling right)
-- example Internal english format (after 1.b4): 'CNBQMBNCPPPPPPPP                 e              p ppppppcnbqmbncW'
-- example EPD format:                           '7k/p7/1R5K/6r1/6p1/6P1/8/8 w - - bm Rb7; id "WAC.006";'

FUNCTION DoMoveOk(stilling in out PL_PIG_CHESS_ENGINE_EVAL.STILLINGTYPE, 
                  fra SIMPLE_INTEGER,--from (ll-88)
                  til SIMPLE_INTEGER,--to   (11-88)  
                  MoveTyp in out MOVETYPE) RETURN BOOLEAN;
-- complete check if move is ok, generates black/white moves using DoMove 
-- IF not DoMoveOk() then error.. -> generates a move if it's legal

PROCEDURE DoMoveC(stilling in out PL_PIG_CHESS_ENGINE_EVAL.STILLINGTYPE, 
                  fra SIMPLE_INTEGER, --from (ll-88)
                  til SIMPLE_INTEGER);--to   (11-88)
-- As DoMove, but NO MoveTyp information is required
-- Faster than DoMoveOK, used when the move is alread validated OK 

PROCEDURE DoMove(stilling in out PL_PIG_CHESS_ENGINE_EVAL.STILLINGTYPE, 
                 fra SIMPLE_INTEGER,--from (ll-88)
                 til SIMPLE_INTEGER,--to   (11-88) 
                 MoveTyp MOVETYPE);
-- Do (white/black) move, already validated OK (by GetNext) 
-- pawn-promotions: 20 less then Rook, 30 then kNight, 40 bishop
-- MoveTyp MUST be set to enpassant and castling (rokade) when relevant!!! (use DoMoveC) 

PROCEDURE GetNext(stilling in out PL_PIG_CHESS_ENGINE_EVAL.STILLINGTYPE, 
                  fra in out SIMPLE_INTEGER,--from (ll-88)
                  til in out SIMPLE_INTEGER,--to   (11-88)
                  retning in out SIMPLE_INTEGER,
                  MoveTyp in out MOVETYPE);
-- finds next legal (white/black) move in the position, fra=89 when no more
-- fra=fra-1 and til=89 for læsning af en briks muligheder; 

PROCEDURE Mirror(stilling in out PL_PIG_CHESS_ENGINE_EVAL.STILLINGTYPE);
-- Spejler stilling sort/hvid *)

PROCEDURE FindTrk(stilling in out PL_PIG_CHESS_ENGINE_EVAL.STILLINGTYPE, 
                  dybde SIMPLE_INTEGER, --Depth  (0,1,4,7,10,13)   
                  ekstra SIMPLE_INTEGER,--Depth-extra (not used)
                  Traek in out TRKDATA);--The move

PROCEDURE GetMove(stilling in out PL_PIG_CHESS_ENGINE_EVAL.STILLINGTYPE, 
                  t in out TRKDATA, 
                  MoveNr SIMPLE_INTEGER, 
                  Quick BOOLEAN);
-- Make a Move out of MoveNr, evt take is placed in EatChar (blstrdBLSTRD) else ' ' 
-- 1'st move =1, do the move if a OK move for MoveNr is found (fra<89) 
-- Quick, so drop check for checks *)

PROCEDURE GetMoveNr(stilling in out PL_PIG_CHESS_ENGINE_EVAL.STILLINGTYPE, 
                    p_fra SIMPLE_INTEGER,--from (ll-88)
                    p_til SIMPLE_INTEGER,--to   (11-88)  
                    MoveNr  in out SIMPLE_INTEGER, 
                    Quick BOOLEAN);
-- Make a MoveNr out of Move 

--PROCEDURE ClearHistory(cnt SIMPLE_INTEGER, 
--                       black BOOLEAN);
-- clears engine history data 3* array: cnt=1-999 (4) moves back ,black=colour 

--PROCEDURE AddHistory(stilling in out PL_PIG_CHESS_ENGINE_EVAL.STILLINGTYPE, 
--                     fra SIMPLE_INTEGER,
--                     til SIMPLE_INTEGER,
--                     vlu SIMPLE_INTEGER);
-- to add moves not made by engine to history used by engine 

PROCEDURE Initialize;

END;
/
