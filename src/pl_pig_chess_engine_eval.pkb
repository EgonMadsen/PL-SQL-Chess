DROP PACKAGE BODY PL_PIG_CHESS_ENGINE_EVAL;

CREATE OR REPLACE PACKAGE BODY PL_PIG_CHESS_ENGINE_EVAL AS
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


  ClosedE4 BOOLEAN;ClosedD4 BOOLEAN;ClosedE3 BOOLEAN;
  ClosedE5 BOOLEAN;ClosedD3 BOOLEAN;ClosedD5 BOOLEAN;
  LatePart BOOLEAN; --* LatePart of OpenGame or EndGame
  FirstW   BOOLEAN := FALSE; 

 /* PreProcess (static evaluation part 1, defaults): */
  RokeretBonus CONSTANT  SIMPLE_INTEGER  := 20; /* bonus for safe king */
  pd1080   CONSTANT  SIMPLE_INTEGER  := -9; /* edge */
  pd2070   CONSTANT  SIMPLE_INTEGER  := -2; /* near edge */
  pd3060   CONSTANT  SIMPLE_INTEGER  :=  3; /* large center */
  pd4050   CONSTANT  SIMPLE_INTEGER  :=  8; /* center*/
  pd1080qb CONSTANT  SIMPLE_INTEGER  :=  3; /* for queen and bishops */
  pd3060qb CONSTANT  SIMPLE_INTEGER  := -2; 
  pd3060qe CONSTANT  SIMPLE_INTEGER  :=  8; /* for queen in endgame */  
  pd1080k  CONSTANT  SIMPLE_INTEGER  :=  8; /* for king, edge*/
  pd2070k  CONSTANT  SIMPLE_INTEGER  := -5; /* for king, near edge*/
  pd3060k  CONSTANT  SIMPLE_INTEGER  :=-20; /* for king, large center */
  pd7R                  CONSTANT SIMPLE_INTEGER  := 20; /* rook on 7'th rank */ 
  pd8R                  CONSTANT SIMPLE_INTEGER  := 5;  /* rook on 8'th rank */ 
  pd6Q                  CONSTANT SIMPLE_INTEGER  :=  5;
  pd7Q                  CONSTANT SIMPLE_INTEGER  := 12; /* queen on 7'th rank */ 
  pd8Q                  CONSTANT SIMPLE_INTEGER  :=  5;
  pdkc12                CONSTANT SIMPLE_INTEGER  :=  9; /* penalty for king in centre */
  /*pdkc9                 CONSTANT SIMPLE_INTEGER  :=  7; */ /* penalty for king in centre */
  pdPcf                 CONSTANT SIMPLE_INTEGER  :=  9; /* Pawn center (c+f) bonus  */
  pdPde                 CONSTANT SIMPLE_INTEGER  := 17; /* Pawn center (d+e) bonus  */
  pdP                   CONSTANT SIMPLE_INTEGER  :=  8; /* pawn rank bonus :  6  7  8  9 start for below:   */
  pdPend                CONSTANT SIMPLE_INTEGER  := 11; /* pawn -"- endgame : 1, 3, 5, 7, 9, 11, 13, 15, 17 */
  PenalUndev            CONSTANT SIMPLE_INTEGER  := 12; /* Penalty for undelevoped pieces */

  /* PreProcessor (static evaluation part 2, adjustments according to position): */
  BishopPenalty         CONSTANT SIMPLE_INTEGER  :=  3; /* behind own pawns penalty */
  OpenGameValue         CONSTANT SIMPLE_INTEGER  :=  6; /* if FirstLineCount > 7*/
  EndGameValue          CONSTANT SIMPLE_INTEGER  := 10; /* if q*4+Q*4+r*2+R*2+n+N+b+B < */
  pdB5                  CONSTANT SIMPLE_INTEGER  :=  6; /* Bishop b5,g5 bonus/penalty */
  KingArea30            CONSTANT SIMPLE_INTEGER  := 30;
  KingArea20            CONSTANT SIMPLE_INTEGER  := 20;
  KingArea16            CONSTANT SIMPLE_INTEGER  := 16;
  KingArea12            CONSTANT SIMPLE_INTEGER  := 12;
  KingArea8             CONSTANT SIMPLE_INTEGER  :=  8;
  AroundKingBonus       CONSTANT SIMPLE_INTEGER  :=  5; /* by KingAdj: *1 *2 or *3 */
  PawnStrateg20         CONSTANT SIMPLE_INTEGER  := 15; /* strategical pawn moves when closed center */
  PawnStrateg10         CONSTANT SIMPLE_INTEGER  :=  8;
  BishopClosed          CONSTANT SIMPLE_INTEGER  := 15; /* penalty */
  BishopOpen            CONSTANT SIMPLE_INTEGER  :=  7;
  KnightClosed          CONSTANT SIMPLE_INTEGER  :=  7;
  RookFullOpenLine      CONSTANT SIMPLE_INTEGER  := 12;
  RookHalfOpenLine      CONSTANT SIMPLE_INTEGER  :=  8;
  EarlyKnight           CONSTANT SIMPLE_INTEGER  := 12; -- penalty 
  LateBishop            CONSTANT SIMPLE_INTEGER  :=  3; -- penalty 
  pawnh3a3attack        CONSTANT SIMPLE_INTEGER  :=  5;
  AvoidCastling         CONSTANT SIMPLE_INTEGER  :=  6; -- penalty 
  CastleBonus           CONSTANT SIMPLE_INTEGER  := 28;

-- Eval (dynamic evaluation):
  InMoveBonus                  CONSTANT SIMPLE_INTEGER := 10;
  --ActivityWeight               CONSTANT SIMPLE_INTEGER :=  0; --!!!!!!!!!!!!!!!!!!!!!! 1-5 (not used) !!!!!!*)
  TwoOnRow7                    CONSTANT SIMPLE_INTEGER :=350; --bonus for 2 advanced side by side pawns!
  TwoOnRow6                    CONSTANT SIMPLE_INTEGER :=150;
  TwoOnRow5                    CONSTANT SIMPLE_INTEGER := 50;
  RookFirstRowBehindPawn       CONSTANT SIMPLE_INTEGER :=  8; -- Penalty, 
  RookCornerNextToKing         CONSTANT SIMPLE_INTEGER := 30; -- Penalty, 
  RookCornerNextToPiece        CONSTANT SIMPLE_INTEGER :=  4; -- Penalty, 
  KingCloseRook                CONSTANT SIMPLE_INTEGER := 22; -- Penalty, (NOT endgame) king on c1 or f1 with rook behind 
  KingBehindPawns              CONSTANT SIMPLE_INTEGER := 12; --          (NOT endgame)
  QueenDeveloped               CONSTANT SIMPLE_INTEGER := 12; -- Penalty, (OpenGame)
  QueenBeforePawn              CONSTANT SIMPLE_INTEGER := 20; -- Penalty, (OpenGame)
  QueenBeforePawnBlockedBishop CONSTANT SIMPLE_INTEGER := 30; -- Penalty, (OpenGame)
  QueenBeforeBishop            CONSTANT SIMPLE_INTEGER := 15; -- Penalty, (OpenGame)
  BishopBeforePawn             CONSTANT SIMPLE_INTEGER := 13; -- Penalty, 
  BishopBehindPawn             CONSTANT SIMPLE_INTEGER :=  8; -- Penalty, 
  BishopProtectedByPawn        CONSTANT SIMPLE_INTEGER :=  8;
  kNightBeforePawn             CONSTANT SIMPLE_INTEGER := 10; -- Penalty, 
  kNightProtectedByPawn        CONSTANT SIMPLE_INTEGER :=  8;
  kNightpPawnInFront           CONSTANT SIMPLE_INTEGER := 12; -- blocks a opponent-pawn
  PawnNextToPawn               CONSTANT SIMPLE_INTEGER :=  8;
  PawnGuardsPawn               CONSTANT SIMPLE_INTEGER :=  5;
  PawnF3                       CONSTANT SIMPLE_INTEGER := 15; -- Penalty, (OpenGame)
  PawnDoubbledPawn             CONSTANT SIMPLE_INTEGER := 40; -- Penalty, (KingEndgame)
  knEndGamePrio                CONSTANT SIMPLE_INTEGER :=  6; --          (KingEndgame)
  knEndGameBprio               CONSTANT SIMPLE_INTEGER :=  6; --          (KingEndgame)
  SupportedPawnPenalty         CONSTANT SIMPLE_INTEGER := 50; -- penalty, (KingEndgame)
  SupportedPawn0911            CONSTANT SIMPLE_INTEGER := 50; --          (KingEndgame)
  SupportedPawn1020            CONSTANT SIMPLE_INTEGER := 20; --          (KingEndgame)
  SupportedPawn1921            CONSTANT SIMPLE_INTEGER := 80; --          (KingEndgame)
  BadDevelPenalty              CONSTANT SIMPLE_INTEGER := 17; -- penalty, (KingEndgame)
  BadDevelBishop               CONSTANT SIMPLE_INTEGER :=  6; -- penalty, (KingEndgame)
  BishopPairBonus              CONSTANT SIMPLE_INTEGER := 30; -- if half-open center, if full-open center then 2*
-- stuff:
  kOut     BOOLEAN;
  K_Out    BOOLEAN;
  stOff    CONSTANT SIMPLE_INTEGER  := 11; --offset for arrays Stilling
  p      SIMPLE_INTEGER:=0;
--
--FUNCTION pdX(brik SIMPLE_INTEGER, felt SIMPLE_INTEGER) RETURN SIMPLE_INTEGER IS
--BEGIN -- two-dimensional array ('B'..'t', 11..88) -> 1..3979
--  RETURN (brik - 66) * 78 + felt - 10; -- ASCII('B')=66
--END pdX;
--
--
--compare with simple_integer is double speed than compare with char(1).
--Numeric representation of pieces to get best possible speed:

wN CONSTANT SIMPLE_INTEGER  := 115;--ASCII('s');--White kNight
bN CONSTANT SIMPLE_INTEGER  :=  83;--ASCII('S');--Black kNight
wB CONSTANT SIMPLE_INTEGER  := 108;--ASCII('l');--White Bishop
bB CONSTANT SIMPLE_INTEGER  :=  76;--ASCII('L');--Black Bishop
wR CONSTANT SIMPLE_INTEGER  := 116;--ASCII('t');--White Rook
bR CONSTANT SIMPLE_INTEGER  :=  84;--ASCII('T');--Black Rook
wC CONSTANT SIMPLE_INTEGER  := 114;--ASCII('r');--White rook with Castling
bC CONSTANT SIMPLE_INTEGER  :=  82;--ASCII('R');--Black rook with Castling
wQ CONSTANT SIMPLE_INTEGER  := 100;--ASCII('d');--White Queen
bQ CONSTANT SIMPLE_INTEGER  :=  68;--ASCII('D');--Black Queen
wP CONSTANT SIMPLE_INTEGER  :=  98;--ASCII('b');--White Pawn
bP CONSTANT SIMPLE_INTEGER  :=  66;--ASCII('B');--Black Pawn
wE CONSTANT SIMPLE_INTEGER  := 101;--ASCII('e');--White pawn En-passant
bE CONSTANT SIMPLE_INTEGER  :=  69;--ASCII('E');--Black pawn En-passant
wK CONSTANT SIMPLE_INTEGER  := 107;--ASCII('k');--White King
bK CONSTANT SIMPLE_INTEGER  :=  75;--ASCII('K');--Black King
wM CONSTANT SIMPLE_INTEGER  := 109;--ASCII('m');--White Majesty (king w castling)
bM CONSTANT SIMPLE_INTEGER  :=  77;--ASCII('M');--Black Majesty (king w castling)
wA CONSTANT SIMPLE_INTEGER  :=  97;--ASCII('a');--White pieces all bigger than this
bA CONSTANT SIMPLE_INTEGER  :=  65;--ASCII('A');--Pieces all bigger than this + reference index
wT CONSTANT SIMPLE_INTEGER  :=  72;--ASCII('H');--Whites Turn (index hvistur)
bT CONSTANT SIMPLE_INTEGER  :=  83;--ASCII('S');--Blacks Turn (index hvistur)
spc CONSTANT SIMPLE_INTEGER :=  32;--ASCII(' ');--Space
edge CONSTANT SIMPLE_INTEGER:=  46;--ASCII('.');--Edge-marker

FUNCTION UPPER_n(n SIMPLE_INTEGER) RETURN SIMPLE_INTEGER IS
BEGIN
  IF n<wA THEN
    RETURN n;
  ELSE 
    RETURN n - 32;
  END IF;
END UPPER_n;
--
FUNCTION pdN(brik_n SIMPLE_INTEGER, felt SIMPLE_INTEGER) RETURN SIMPLE_INTEGER IS
BEGIN -- two-dimensional array (bP..wR, 11..88) -> 1..3978. Here index is calculated out of Numeric repræsentation (ascii value) of pieces.
  --RETURN (brik - 66) * 78 + felt - 10; -- ASCII(bP)=66  ASCII(wR)=116
  RETURN brik_n * 78 -5158 + felt; -- ASCII(bP)=66
END pdN;

FUNCTION pdX(brik CHAR, felt SIMPLE_INTEGER) RETURN SIMPLE_INTEGER IS
BEGIN -- two-dimensional array ('B'..'t', 11..88) -> 1..3978
  RETURN (ASCII(brik) - 66) * 78 + felt - 10; -- ASCII('B')=66
END pdX;

PROCEDURE WRT(s VARCHAR2) IS
BEGIN
  dbms_output.put_line(s);
END WRT;
--


PROCEDURE PreProcess IS
  n SIMPLE_INTEGER := 0;
  ix SIMPLE_INTEGER:= 0;
  m SIMPLE_INTEGER:= 0;
  t SIMPLE_INTEGER:= 0;
  v SIMPLE_INTEGER:= 0;
  f SIMPLE_INTEGER:= 0;
  --ch CHAR(1);
  ch_n SIMPLE_INTEGER:= 0; -- ASCII(CHAR);
BEGIN
  --pdw.extend(4000);
  p := 600;
  FOR ch_n IN bP..wR  LOOP  --clear all
    IF ch_n in (bP,bE,bR,bC,bN,bK,bM,bB,bQ, wP,wE,wR,wC,wN,wK,wM,wB,wQ) THEN
      ix := pdn(ch_n,0);
      FOR n in 1..8 LOOP  FOR m in 1..8 LOOP 
         pdw(ix + n*10+m) := 0;
         pdb(ix + n*10+m) := 0;
      END LOOP; END LOOP;
    END IF;
  END LOOP;
  --
  p := 601;
  FOR ch_n IN bP..bR  LOOP        /* positionel data, centrum bedst */
     p := 602;
     --ch := chr(ch_n);
     p := 604;
     CASE 
     WHEN ch_n in ( bP,bE,bR,bC,bN ) THEN
          p := 605;
          FOR n in 1..8 LOOP  FOR m in 1..8 LOOP 
            --p := 6052;
            --p := 6053;
            --WRT(chr(ch_n)||': '||to_char(n*10+m)||' '||pdN(ch_n,n*10+m));
            --p := 6054;
            --WRT(chr(ch_n)||n||m||' '||pdN(ch_,n*10+m));
            pdw(pdN(ch_n,n*10+m)):=pd1080;  
          END LOOP; END LOOP;
          p := 606;
          FOR n in 2..7 LOOP  FOR m in 2..7 LOOP pdw(pdN(ch_n,n*10+m)):=pd2070;  END LOOP; END LOOP;
          p := 607;
          IF ch_n in ( bP,bE,bN ) THEN -- Rooks not in center
            FOR n in 3..6 LOOP  FOR m in 3..6 LOOP pdw(pdN(ch_n,n*10+m)):=pd3060;  END LOOP; END LOOP;
            p := 610;
            FOR n in 4..5 LOOP  FOR m in 4..5 LOOP pdw(pdN(ch_n,n*10+m)):=pd4050;  END LOOP; END LOOP;
          END IF;
     WHEN ch_n in (bK,bM) THEN
          p := 620;
          IF Endgame THEN
            IF LatePart THEN
              FOR n in 1..8 LOOP  FOR m in 1..8 LOOP pdw(ix + n*10+m):=floor(-pd1080k/2);  END LOOP; END LOOP;
              FOR n in 2..7 LOOP  FOR m in 2..7 LOOP pdw(ix + n*10+m):=floor(-pd2070k/2);  END LOOP; END LOOP;
              FOR n in 3..6 LOOP  FOR m in 3..6 LOOP pdw(ix + n*10+m):=floor(-pd3060k/2);  END LOOP; END LOOP;
            END IF;
          ELSE
            FOR n in 1..8 LOOP  FOR m in 1..8 LOOP pdw(pdN(ch_n,n*10+m)):=pd1080k;  END LOOP; END LOOP;
            FOR n in 2..7 LOOP  FOR m in 2..7 LOOP pdw(pdN(ch_n,n*10+m)):=pd2070k;  END LOOP; END LOOP;
            FOR n in 3..6 LOOP  FOR m in 3..6 LOOP pdw(pdN(ch_n,n*10+m)):=pd3060k;  END LOOP; END LOOP;
          END IF;
      WHEN ch_n in (bB,bQ) THEN  /* løber,D ikke specielt i centrum, men fianciettering */
          p := 630;
          IF EndGame AND (ch_n=bQ) THEN t:=pd3060qe; ELSE t:=pd3060qb; END IF;
          FOR n in 1..8 LOOP  FOR m in 1..8 LOOP pdw(pdN(ch_n,n*10+m)):=pd1080qb;  END LOOP; END LOOP;
          FOR n in 3..6 LOOP  FOR m in 3..6 LOOP pdw(pdN(ch_n,n*10+m)):=t;  END LOOP; END LOOP;
      ELSE
          NULL;      
      END CASE;
  END LOOP;
  p := 639;
  IF OpenGame THEN
    p := 640;
    /* bonus to move pawns into the center */
    ix := pdN(bP,56); pdw(ix) := pdw(ix) + pdPcf;
    ix := pdN(bP,55); pdw(ix) := pdw(ix) + pdPde;
    ix := pdN(bP,54); pdw(ix) := pdw(ix) + pdPde;
    ix := pdN(bP,53); pdw(ix) := pdw(ix) + pdPcf;
    ix := pdN(bE,56); pdw(ix) := pdw(ix) + pdPcf;
    ix := pdN(bE,55); pdw(ix) := pdw(ix) + pdPde;
    ix := pdN(bE,54); pdw(ix) := pdw(ix) + pdPde;
    ix := pdN(bE,53); pdw(ix) := pdw(ix) + pdPcf;
    /* penalty to undeveloped pieces */
    ix := pdN(bN,82); pdw(ix) := pdw(ix) - PenalUndev;
    ix := pdN(bB,83); pdw(ix) := pdw(ix) - PenalUndev;
    ix := pdN(bQ,84); pdw(ix) := pdw(ix) - PenalUndev;
    ix := pdN(bB,86); pdw(ix) := pdw(ix) - PenalUndev;
    ix := pdN(bN,87); pdw(ix) := pdw(ix) - PenalUndev;
    ix := pdN(bP,74); pdw(ix) := pdw(ix) - PenalUndev;
    ix := pdN(bP,75); pdw(ix) := pdw(ix) - PenalUndev;
  ELSE
    p := 650;
  /* tårne (rook) + Dronning (queen) på (at) 7+8 række (row) bonus */
    FOR n IN 1..8 LOOP
      pdw(pdN(bR,20+n)):=pd7R;   
      pdw(pdN(bR,10+n)):=pd8R;
      pdw(pdN(bQ,30+n)):=pd6Q;
      pdw(pdN(bQ,20+n)):=pd7Q;
      pdw(pdN(bQ,10+n)):=pd8Q;
    END LOOP;
    p := 660;

    /* bønder (pawns) mere værd jo tættere de er på forvandling (promotion) 7, 9 */
    IF EndGame THEN t := pdPend; ELSE t := pdP; END IF;
    FOR n IN 2..7 LOOP
      v := (t-n)*(t-n);
      FOR m IN 1..8 LOOP
           f :=10*n+m;
           ix := pdN(bP,f); pdw(ix) := pdw(ix) + v;
           ix := pdN(bE,f); pdw(ix) := pdw(ix) + v;
       END LOOP;
    END LOOP;
  END IF;
  p := 670;

  /* Knights worth a bit less */
  IF EndGame THEN
    FOR n IN 1..8 LOOP
      FOR m IN 1..8 LOOP
        ix := pdN(bN,10*n+m); pdw(ix) := pdw(ix) -12;
      END LOOP;
    END LOOP;
  END IF;
  
  /* spejl af sort til hvid*/
  FOR ch_n IN wP..wR LOOP 
     --ch := chr(ch_n);
    CASE WHEN ch_n IN (wP,wE,wR,wC,wK,wM,wQ,wN,wB ) THEN
        FOR n in 1..8 LOOP
          FOR m in 1..8 LOOP
              pdw(pdN(ch_n,n+10*m)) :=pdw(pdN(UPPER_n(ch_n),99-n-10*m));
          END LOOP;
        END LOOP;
    ELSE
      NULL;
    END CASE;
  END LOOP;

  p := 680;


  /* konge (king) mere værd hvis i sikkerhed (secure) */
  IF not EndGame THEN
    p := 690;
    FOR m IN 1..2 LOOP
      FOR n IN 2..3 LOOP
         ix := pdN(wK,m*10+5-n);    pdw(ix) := RokeretBonus;-- castlingbonus
         ix := pdN(wK,m*10+5+n);    pdw(ix) := RokeretBonus;
         ix := pdN(bK,90-m*10+5-n); pdw(ix) := RokeretBonus;
         ix := pdN(bK,90-m*10+5+n); pdw(ix) := RokeretBonus;
      END LOOP;
      FOR n IN 4..6 LOOP
         ix := pdN(wK,m*10+n);      pdw(ix) := pdw(ix) - pdkc12;
         ix := pdN(bK,90-m*10+n);   pdw(ix) := pdw(ix) -pdkc12;
      END LOOP;
    END LOOP;
    /* 5-98: develop king-side bishops */
     ix := pdN(wB,16);      pdw(ix) := pdw(ix) - 3;
     ix := pdN(wB,86);      pdw(ix) := pdw(ix) - 3;
  END IF;
  p := 699;

  /*pd:=pdw;*/
  EXCEPTION
    WHEN OTHERS THEN
      dbms_output.put_line('Preprocess p='||p||': '||sqlerrm);
  END PreProcess;
  
  PROCEDURE PreProcessor(stilling STILLINGTYPE) IS
/* adjusts the pd-array+OpenGame+EngGame according..the actual position. */
/* Is called one time for each engine call */
    dd SIMPLE_INTEGER := 0; -- PL/SQL is not case sensitive
    tt SIMPLE_INTEGER := 0;
    ll SIMPLE_INTEGER := 0; 
    ss SIMPLE_INTEGER := 0;
    bbb SIMPLE_INTEGER := 0;
    defenders SIMPLE_INTEGER := 0;
    D SIMPLE_INTEGER := 0;
    T SIMPLE_INTEGER := 0;
    L SIMPLE_INTEGER := 0;
    S SIMPLE_INTEGER := 0;
    B SIMPLE_INTEGER := 0; 
    n SIMPLE_INTEGER := 0;
    m SIMPLE_INTEGER := 0;
    n_n SIMPLE_INTEGER := 0;-- BY 10 helper
    m_n SIMPLE_INTEGER := 0;-- BY 10 helper
    f SIMPLE_INTEGER := 0;
    FirstLineCount SIMPLE_INTEGER := 0; 
    wkk SIMPLE_INTEGER := 0;
    bkk SIMPLE_INTEGER := 0;
    kx SIMPLE_INTEGER := 0;
    ky SIMPLE_INTEGER := 0;
   
    x SIMPLE_INTEGER := 0;
    y SIMPLE_INTEGER := 0;
    y_n SIMPLE_INTEGER := 0;-- BY 10 helper
    v SIMPLE_INTEGER := 0;
    nm SIMPLE_INTEGER := 0;
    om SIMPLE_INTEGER := 0;
    sw SIMPLE_INTEGER := 0;
    ch_n SIMPLE_INTEGER := 0;
    nbb SIMPLE_INTEGER := 0;
    nB SIMPLE_INTEGER := 0;
    TYPE Rtyp is VARRAY(8)OF BOOLEAN;
    R  Rtyp := Rtyp(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE); -- stores TRUE if line open for (Black) rooks
    rr Rtyp := Rtyp(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE);
    NoWoffs BOOLEAN;--None White Officers
    NoBoffs BOOLEAN;
    ix SIMPLE_INTEGER:= 0;
PROCEDURE KingAdj(k SIMPLE_INTEGER) IS
  WpieceCnt SIMPLE_INTEGER := 0;
  BpieceCnt SIMPLE_INTEGER := 0;
  fc CHAR(1);
BEGIN p := 100;
  x:=MOD(k,10);
  y:=FLOOR(k/10);
  WpieceCnt:=0;
  BpieceCnt:=0;
  FOR n IN x-2..x+2 LOOP
    FOR m IN y-2..y+2 LOOP
      IF (n>0) AND (n<9) AND (m>0) AND (m<9) THEN 
        f:=n+10*m;
        IF stilling(stOff+ f)>spc THEN
          IF stilling(stOff+ f) = UPPER_n(stilling(stOff+ f)) THEN BpieceCnt:=BpieceCnt+1; ELSE WpieceCnt:= WpieceCnt+1; END IF;
        END IF;
      END IF;
    END LOOP;
  END LOOP;
  IF stilling(stOff+ k)=UPPER_n(stilling(stOff+ k)) THEN -- black king, then swap
    n:=BpieceCnt;
    BpieceCnt:=WpieceCnt;
    WpieceCnt:=n;
  END IF;
  IF WpieceCnt+BpieceCnt<10 THEN
    WpieceCnt := WpieceCnt+3;
  ELSIF WpieceCnt+BpieceCnt<13 THEN
    WpieceCnt := WpieceCnt+2;
  ELSIF WpieceCnt+BpieceCnt<17 THEN
    WpieceCnt := WpieceCnt+1;
  END IF;
  IF WpieceCnt-BpieceCnt>7 THEN
    ll:=0;
  ELSIF WpieceCnt-BpieceCnt>5 THEN
    ll:=AroundKingBonus;
  ELSIF WpieceCnt-BpieceCnt>4 THEN
    ll:=AroundKingBonus*2;
  ELSE
    ll:=AroundKingBonus*3;
  END IF;
  FOR n IN x-2..x+2 LOOP
    FOR m  IN y-2..y+2 LOOP
      IF (n>0) AND (n<9) AND (m>0) AND (m<9) THEN 
        f:=n+10*m;
        --WRT('KingAdj: '||f||' + '||ll);
        ix := pdN(bQ,f); Pdw(ix) := Pdw(ix) + ll;
        ix := pdN(bR,f); Pdw(ix) := Pdw(ix) + ll;
        ix := pdN(bB,f); Pdw(ix) := Pdw(ix) + ll;
        ix := pdN(bN,f); Pdw(ix) := Pdw(ix) + ll;
        ix := pdN(bP,f); Pdw(ix) := Pdw(ix) + ll;
        ix := pdN(wQ,f); Pdw(ix) := Pdw(ix) + ll;
        ix := pdN(wR,f); Pdw(ix) := Pdw(ix) + ll;
        ix := pdN(wB,f); Pdw(ix) := Pdw(ix) + ll;
        ix := pdN(wN,f); Pdw(ix) := Pdw(ix) + ll;
        ix := pdN(wP,f); Pdw(ix) := Pdw(ix) + ll;--rettet 'n' -> wP
      END IF;
    END LOOP;
  END LOOP;
   p := 199;
END KingAdj;--in PreProcessor
PROCEDURE PieceAdjust IS
BEGIN p := 200;
  IF PieceClear THEN
    FOR x IN 1..8 LOOP
      FOR y IN 1..8 LOOP
        v:=x+10*y;
        pdw(pdN(wK,v)):=0; --pdw(pdN(wK,v] DIV 2;
        pdw(pdN(bK,v)):=0; --pdw(pdN(bK,v] DIV 2;
      END LOOP;
    END LOOP;
  END IF;
  FOR x IN 1..8 LOOP
    FOR y IN 1..8 LOOP
      v:=x+10*y;
      IF stilling(stOff+ v)>spc THEN
        FOR n IN x-4..x+4 LOOP
          FOR m IN y-4..y+4 LOOP
            IF (n>0) AND (n<9) AND (m>0) AND (m<9) AND ((n<>x) OR (m<>y)) THEN 
              f:=n+10*m;
              ll:=FLOOR((26-ABS(y-m)*ABS(x-n)) /  2); -- 12,11,8,5
              IF stilling(stOff+ v)<>wK THEN ix := pdN(wK,f); Pdw(ix) := Pdw(ix) + ll; END IF; 
              IF stilling(stOff+ v)<>bK THEN ix := pdN(bK,f); Pdw(ix) := Pdw(ix) + ll; END IF; 
            END IF;
          END LOOP;
        END LOOP;
      END IF;
    END LOOP;
  END LOOP;
  IF PieceDecr THEN
    FOR x IN 1..8 LOOP 
      FOR y IN 1..8 LOOP 
        ix := pdN(wK,x+10*y); Pdw(ix) := Pdw(ix) - 50; --DEC(pdw(pdN(wK,x+10*y],50); 
        ix := pdN(bK,x+10*y); Pdw(ix) := Pdw(ix) - 50; --DEC(pdw(pdN(bK,x+10*y],50); 
      END LOOP; 
    END LOOP;
  END IF;
  p := 299;
END PieceAdjust;--in PreProcessor
PROCEDURE SetP(brik SIMPLE_INTEGER, distance SIMPLE_INTEGER, valuee SIMPLE_INTEGER) IS
  kx SIMPLE_INTEGER := 0;
  ky SIMPLE_INTEGER := 0;
  x SIMPLE_INTEGER := 0;
  y SIMPLE_INTEGER := 0;
BEGIN  p := 300;                 -- brik=white piece (adjusts for black too)
  kx:=MOD(bkk,  10);
  ky:=FLOOR(bkk / 10);
  FOR x IN kx-distance..kx+distance LOOP
    FOR y IN ky-distance..ky+distance LOOP
      IF (x>0) AND (x<9) AND (y>0) AND (y<9)
      AND ((ABS(x-kx)=distance) OR (ABS(y-ky)=distance)) THEN
        ix := pdN(brik,x+10*y); Pdw(ix) := Pdw(ix) + valuee;
      END IF;
    END LOOP;
  END LOOP;
  kx:=MOD (wkk, 10);
  ky:=FLOOR(wkk / 10);
  FOR x IN kx-distance..kx+distance LOOP
    FOR y IN ky-distance..ky+distance LOOP
      IF (x>0) AND (x<9) AND (y>0) AND (y<9)
      AND ((ABS(x-kx)=distance) OR (ABS(y-ky)=distance)) THEN
        ix := pdN(UPPER_n(brik),x+10*y); Pdw(ix) := Pdw(ix) + valuee;
      END IF;
    END LOOP;
  END LOOP;
  p := 399;
END SetP;-- in PreProcessor
BEGIN  p := 400;--PreProcessor
  --WRT('PREPROCESSOR');
  IF not FirstW THEN --one-time initialization (allocation+initial-values) of varray variables.
    Initialize;
  END IF;
--(*$ IF Test0 WRITELNF(s('PreProcessor...')); --(*$ ENDIF

  -- init variables
  dd:=0; tt:=0; ll:=0; ss:=0; bbb:=0; D:=0; T:=0; L:=0; S:=0; B:=0;
  ClosedE4:=FALSE; ClosedD4:=FALSE; ClosedE3:=FALSE;
  ClosedE5:=FALSE; ClosedD5:=FALSE; ClosedD3:=FALSE;

  FirstLineCount:=16; -- 16 pieces minus empty fields

  -- count pieces
  FOR n IN 11..88 LOOP
    CASE stilling(stOff+ n) 
    WHEN  wQ THEN  dd := dd + 1; -- white queen
    WHEN  bQ THEN  D  := D + 1;  -- black queen
    WHEN  wR THEN  tt := tt + 1; -- white rook
    WHEN  wC THEN  tt := tt + 1; -- white rook, castleable
    WHEN  bR THEN  T  := T + 1;  -- black rook
    WHEN  bC THEN  T  := T + 1;  -- black rook, castleable
    WHEN  wB THEN  ll := ll + 1; -- white bishop
    WHEN  bB THEN  L  := L +1;   -- black bishop
    WHEN  wN THEN  ss := ss + 1; -- white knight
    WHEN  bN THEN  S  := S + 1;  -- black knight
    WHEN  wP THEN  bbb:= bbb + 1; -- white pawn
    WHEN  wE THEN  bbb:= bbb + 1; -- white pawn, enpassantable 
    WHEN  bP THEN  B  := B + 1;  -- black pawn
    WHEN  bE THEN  B  := B + 1;  -- black pawn, enpassantable 
    WHEN  wK THEN  kOut  := n=15;-- white king 
    WHEN  wM THEN  kOut  := n=15;-- white king, castleable 
    WHEN  bK THEN  K_Out := n=85;-- black king 
    WHEN  bM THEN  K_Out := n=85;-- black king, castleable
    ELSE
      IF (n<19) OR (n>80) THEN FirstLineCount := FirstLineCount - 1; END IF;
    END CASE;
  END LOOP;
  NoBoffs :=  (S=0) AND (L=0) AND (D=0) AND (T=0);    --none black officers
  NoWoffs := (ss=0) AND (ll=0) AND (dd=0) AND (tt=0); --none white officers
  p := 410;
  -- set global flags (5 combinations):
  --  1. opengame, NOT latepart
  --  2. opengame, latepart
  --  3. NOT opengame, NOT endgame, latepart
  --  4. endgame, NOT latepart
  --  5. endgame, latepart
  OpenGame :=FirstLineCount>OpenGameValue;
  IF OpenGame THEN
    LatePart:=FirstLineCount<OpenGameValue+3;
    EndGame :=FALSE;
  ELSE
    EndGame  :=dd*3+D*3+tt*2+T*2+ss+S+ll+L < EndGameValue;
    IF EndGame THEN
      LatePart:=dd*3+D*3+tt*2+T*2+ss+S+ll+L < EndGameValue-4;
    END IF;
  END IF;

  IF EndGame and PieceClearEarly  THEN
    FOR x IN 1..8 LOOP
      FOR y IN 1..8 LOOP
        v:=x+10*y;
        pdw(pdN(wK,v)):=0; --pdw(pdN(wK,v] DIV 2;
        pdw(pdN(bK,v)):=0; --pdw(pdN(bK,v] DIV 2;
      END LOOP;
    END LOOP;
  END IF;
  
  -- set default positional values (in pd)
  PreProcess;
 
  -- White king positions calc
  IF stilling(stOff+ 15)=wM THEN 
    wkk:=15;
    IF NOT EndGame THEN
      ix := pdN(wK,17); pdw(ix) := pdw(ix) + CastleBonus+9;
      ix := pdN(wK,13); pdw(ix) := pdw(ix) + CastleBonus-9;
    END IF;
  ELSE
    wkk:=11;
    WHILE (stilling(stOff+ wkk)<>wK) AND (wkk<88) LOOP 
      wkk := wkk + 1; 
    END LOOP;
  END IF;
  --WRT('wkk='||wkk);
  
  
  -- Black king positions calc  
  IF stilling(stOff+ 85)=bM THEN
    p := 1441;
    bkk:=85;
    IF NOT EndGame THEN
      p := 1442;
      ix := pdN(bK,87); pdw(ix) := pdw(ix) + CastleBonus+9;
      ix := pdN(bK,83); pdw(ix) := pdw(ix) + CastleBonus-9;
    END IF;
  ELSE
    bkk:=88;
    p := 1444;
    WHILE (stilling(stOff+ bkk)<>bK) AND (bkk>11) LOOP 
      bkk := bkk -1; 
    END LOOP;
    p := 1448;
  END IF;
  --WRT('bkk='||bkk);
  
 
  -- Mating, 4-Double value of pawns to try to promote or Added value to
  -- get lonely king to the edge
  IF NoBoffs AND (B=0) THEN
    FOR x IN 1..8 LOOP
      FOR y IN 1..8 LOOP
        n:=x+10*y;
        IF bbb=0 THEN
          IF Follow THEN
            ix := pdn(bK,n);
            IF (y=1) OR (y=8) THEN -- on Y edge
               pdw(ix) := pdw(ix) -100;--WRT('B100');
            END IF;
            IF (x=1) OR (x=8) THEN -- on X edge
               pdw(ix) := pdw(ix) -100;--WRT('B100');
            END IF;
            IF (y=2) OR (y=7) THEN -- next to Y edge
               pdw(ix) := pdw(ix) -50;--WRT('B50');
            END IF;
            IF (x=2) OR (x=7) THEN -- next to X edge
               pdw(ix) := pdw(ix) -50;--WRT('B50');
            END IF;
            IF (y=3) OR (y=6)   THEN -- not in Y center
               pdw(ix) := pdw(ix) -25;--WRT('B25');
            END IF;
            IF (x=3) OR (x=6)  THEN -- not in X center
               pdw(ix) := pdw(ix) -25;--WRT('B25');
            END IF;
          ELSE
            IF (n<19) OR (n>70) OR (x=1) OR (x=8) THEN
              ix := pdX('K',n); pdw(ix) := pdw(ix) -60;
            ELSIF (n<29) OR (n>60) OR (x=2) OR (x=7) THEN
              ix := pdX('K',n); pdw(ix) := pdw(ix) -30;
            END IF;
          END IF;
        ELSE
          ix := pdN(wP,n); pdw(ix) := pdw(ix)*4;--WRT('B*4');
        END IF;
      END LOOP;
    END LOOP;
    IF Follow THEN
      kx:=MOD(bkk,  10);-- bonus for moving king towards the king to mate
      ky:=FLOOR(bkk / 10);    
      FOR x IN kx-2..kx+2 LOOP
        IF x>0 and x<9 THEN
          FOR y IN ky-2..ky+2 LOOP
            IF y>0 and y<9 THEN
              IF (y between ky-2 and ky+2) and (x between kx-2 and kx+2) THEN
                ix := pdn(wK,x+10*y);
                IF (y = ky-2 or y = ky+2) and (x = kx-2 or x = kx+2) THEN
                  pdw(ix) := pdw(ix) + 15;                 
                ELSE
                  pdw(ix) := pdw(ix) + 30;
                END IF;
--                WRT('bonus for moving king towards the black king to mate yx='||y||x);
              END IF;
            END IF;
          END LOOP;
        END IF;
      END LOOP;     
    END IF; 
    --
  END IF;
--
--  IF NoBoffs AND (B>0) AND (bbb>0) THEN
--    FOR x IN 1..8 LOOP
--      FOR y IN 1..8 LOOP
--        n:=x+10*y;
--        ix := pdN(wP,n); pdw(ix) := pdw(ix)*4;
--      END LOOP;
--    END LOOP;
--  END IF;

  IF NoWoffs AND (bbb=0) THEN
    FOR x in 1..8 LOOP
      FOR y in 1..8 LOOP
        n:=x+10*y;
        IF B=0 THEN
          IF Follow THEN
            ix := pdn(wK,n);
            IF (y=1) OR (y=8) THEN -- on Y edge
               pdw(ix) := pdw(ix) -100;--WRT('W100');
            END IF;
            IF (x=1) OR (x=8) THEN -- on X edge
               pdw(ix) := pdw(ix) -100;--WRT('W100');
            END IF;
            IF (y=2) OR (y=7) THEN -- next to Y edge
               pdw(ix) := pdw(ix) -50;--WRT('W50');
            END IF;
            IF (x=2) OR (x=7) THEN -- next to X edge
               pdw(ix) := pdw(ix) -50;--WRT('W50');
            END IF;
            IF (y=3) OR (y=6)   THEN -- not in Y center
               pdw(ix) := pdw(ix) -25;--WRT('W25');
            END IF;
            IF (x=3) OR (x=6)  THEN -- not in X center
               pdw(ix) := pdw(ix) -25;--WRT('W25');
            END IF;
          ELSE
            IF (n<19) OR (n>70) OR (x=1) OR (x=8) THEN
              ix := pdX('k',n); pdw(ix) := pdw(ix) -60;
            ELSIF (n<29) OR (n>60) OR (x=2) OR (x=7) THEN
              ix := pdX('k',n); pdw(ix) := pdw(ix) -30;
            END IF;
          END IF;
        ELSE
          ix := pdN(bP,n); pdw(ix) := pdw(ix)*4;--WRT('W*4');
        END IF;
      END LOOP;
    END LOOP;
    --
    IF Follow THEN
      kx:=MOD(wkk,  10);-- bonus for moving king towards the king to mate
      ky:=FLOOR(wkk / 10);    
      FOR x IN kx-2..kx+2 LOOP
        IF x>0 and x<9 THEN
          FOR y IN ky-2..ky+2 LOOP
            IF y>0 and y<9 THEN
              IF (y between ky-2 and ky+2) and (x between kx-2 and kx+2) THEN
                ix := pdn(bK,x+10*y);
                IF (y = ky-2 or y = ky+2) and (x = kx-2 or x = kx+2) THEN
                  pdw(ix) := pdw(ix) + 15;                 
                ELSE
                  pdw(ix) := pdw(ix) + 30;
                END IF;
--                WRT('bonus for moving king towards the white king to mate yx='||y||x);
              END IF;
            END IF;
          END LOOP;
        END IF;
      END LOOP;   
    END IF;   
    --
  END IF;
  p := 420;
  --
 -- IF NoWoffs AND (bbb>0) AND (B>0) THEN
 --   FOR x IN 1..8 LOOP
 --     FOR y IN 1..8 LOOP
 --       n:=x+10*y;
 --       ix := pdN(bP,n); pdw(ix) := pdw(ix)*4;
 --     END LOOP;
 --   END LOOP;
 -- END IF;

  -- calc pawn center type
  IF (stilling(stOff+ 44)=wP) AND (stilling(stOff+ 54)=bP) THEN--rettet 54=wP til 54=bP
    ClosedD4:=TRUE;
    IF (stilling(stOff+ 35)=wP) AND (stilling(stOff+ 45)=bP) THEN ClosedE3 :=TRUE; END IF;
    IF (stilling(stOff+ 55)=wP) AND (stilling(stOff+ 65)=bP) THEN ClosedE5 :=TRUE; END IF;
  ELSIF (stilling(stOff+ 45)=wP) AND (stilling(stOff+ 55)=bP) THEN--rettet 55=wP til 55=bP
    ClosedE4:=TRUE;
    IF (stilling(stOff+ 34)=wP) AND (stilling(stOff+ 44)=bP) THEN ClosedD3 :=TRUE; END IF;
    IF (stilling(stOff+ 54)=wP) AND (stilling(stOff+ 64)=bP) THEN ClosedD5 :=TRUE; END IF;
  END IF;
  
  p := 424;
  IF (stilling(stOff+ HvisTur)=bT) THEN -- 5-98
    IF NOT NoBoffs AND NoWoffs THEN --pawn endgame
      FOR x IN 1..8 LOOP
        FOR y IN 1..8 LOOP
          n:=x+10*y;
          pdw(pdN(bP,n)):=pdw(pdN(bP,n))*2;-- rettet fra pdb til pdw
        END LOOP;
      END LOOP;
    END IF;
  ELSE
    IF NOT NoWoffs AND NoBoffs THEN
       FOR x IN 1..8 LOOP
        FOR y IN 1..8 LOOP
          n:=x+10*y;
          pdw(pdN(wP,n)):=pdw(pdN(wP,n))*2;
        END LOOP;
      END LOOP;
    END IF;
  END IF;
  p := 426;

  -- rook open lines calc
  FOR x IN 1..8 LOOP
    rr(x):=TRUE;
    R(x):=TRUE;
    FOR y_n IN 2..7 LOOP
      y := y_n*10;
      IF (stilling(stOff+ y+x)=wP) OR (stilling(stOff+ y+x)=wE) THEN rr(x):=FALSE; END IF;
      IF (stilling(stOff+ y+x)=bP) OR (stilling(stOff+ y+x)=bE) THEN R(x):=FALSE; END IF;
    END LOOP;
  END LOOP;
 p := 430;

  IF NOT EndGame AND NOT (OpenGame AND NOT LatePart) AND KingAir THEN--!!!!!!!!!new may19: air for king in mid-game:
    IF wkk = 17 and stilling(stOff+ 26)=wP and stilling(stOff+ 27)=wP and stilling(stOff+ 28)=wP  THEN
      defenders :=0;
      FOR sc IN 11..16 LOOP
         IF stilling(stOff+ sc) in (wR,wQ) THEN defenders := defenders + 1; END IF;
      END LOOP;
      IF stilling(stOff+ 16) in (wB,wN)  THEN defenders := defenders + 1; END IF;   
      IF defenders < 2 THEN ix := pdN(wP,27); pdw(ix) := pdw(ix) + defendersWeight*4 - defenders*defendersWeight*2; pdw(ix+1) := pdw(ix+1) + defendersWeight*6 - defenders*defendersWeight*3; END IF;
    ELSIF wkk = 12 and stilling(stOff+ 21)=wP and stilling(stOff+ 22)=wP and stilling(stOff+ 23)=wP THEN
      defenders :=0;
      FOR sc IN 13..18 LOOP
         IF stilling(stOff+ sc) in (wR,wQ) THEN defenders := defenders + 1; END IF;
      END LOOP;
      IF stilling(stOff+ 13) in (wB,wN)  THEN defenders := defenders + 1; END IF;   
      IF defenders < 2 THEN ix := pdN(wP,22); pdw(ix) := pdw(ix) + defendersWeight*4 - defenders*defendersWeight*2; pdw(ix-1) := pdw(ix-1) + defendersWeight*6 - defenders*defendersWeight*3; END IF;    
    END IF;
  END IF;
  --

  IF NOT EndGame AND (wkk=15) OR OpenGame AND (( MOD(wkk,10)>3) OR ( MOD(wkk,10)<7)) THEN
    -- king castling eval (avoid a side?)
    f:=0; -- O-O-O
    IF (stilling(stOff+ 21)<>wP) AND (stilling(stOff+ 31)<>wP) THEN f := f + 1; END IF;
    IF (stilling(stOff+ 22)<>wP) AND (stilling(stOff+ 22)<>wB) AND (stilling(stOff+ 33)<>wB) THEN f := f + 1; END IF;
    IF stilling(stOff+ 23)<>wP THEN f := f + 1; END IF;
    FOR x IN 1..3 LOOP 
      IF R(x) THEN f := f + 1; END IF; 
    END LOOP;
    IF f>0 THEN 
      ix := pdN(wK,13); pdw(ix) := pdw(ix) - f*f*AvoidCastling;
    END IF;
    f:=0; -- O-O
    IF (stilling(stOff+ 28)<>wP) AND (stilling(stOff+ 38)<>wP) THEN f := f + 1; END IF;
    IF (stilling(stOff+ 27)<>wP) AND (stilling(stOff+ 27)<>wB)  AND (stilling(stOff+ 36)<>wB) THEN f := f + 1; END IF;
    IF stilling(stOff+ 26)<>wP THEN f := f + 1; END IF;
    FOR x IN 6..8 LOOP IF rr(x) THEN f := f +1; END IF; END LOOP;
    IF f>0 THEN
      ix := pdN(wK,17); pdw(ix) := pdw(ix) - f*f*AvoidCastling;
    ELSE
      IF (stilling(stOff+ 15)=wM) AND (stilling(stOff+ 16)=wB) AND (stilling(stOff+ 17)=wN) THEN
        -- undeveloped king-side penalty
        ix := pdN(wB,16); pdw(ix) := pdw(ix) - 8;
        ix := pdN(wN,17); pdw(ix) := pdw(ix) - 8;
      END IF; 
    END IF;
  END IF;
  --p := 439;
  --dbms_output.put_line(length(stilling(stOff+ 85)));
  
  p := 440;
    IF NOT EndGame AND NOT (OpenGame AND NOT LatePart) AND KingAir THEN-----------------new may19: air for king in mid-game:
    IF bkk = 87 and stilling(stOff+ 76)=bP and stilling(stOff+ 77)=bP and stilling(stOff+ 78)=bP  THEN
      defenders :=0;
      FOR sc IN 81..86 LOOP
         IF stilling(stOff+ sc) in (bR,bQ) THEN defenders := defenders + 1; END IF;
      END LOOP;
      IF stilling(stOff+ 86) in (bB,bN)  THEN defenders := defenders + 1; END IF;   
      IF defenders < 2 THEN ix := pdN(bP,77); pdw(ix) := pdw(ix) + defendersWeight*4 - defenders*defendersWeight*2; pdw(ix+1) := pdw(ix+1) + defendersWeight*6 - defenders*defendersWeight*3; END IF;
    ELSIF bkk = 82 and stilling(stOff+ 71)=bP and stilling(stOff+ 72)=bP and stilling(stOff+ 73)=bP THEN
      defenders :=0;
      FOR sc IN 83..88 LOOP
         IF stilling(stOff+ sc) in (bR,bQ) THEN defenders := defenders + 1; END IF;
      END LOOP;
      IF stilling(stOff+ 83) in (bB,bN)  THEN defenders := defenders + 1; END IF;   
      IF defenders < 2 THEN ix := pdN(bP,72); pdw(ix) := pdw(ix) + defendersWeight*4 - defenders*defendersWeight*2; pdw(ix-1) := pdw(ix-1) + defendersWeight*6 - defenders*defendersWeight*3; END IF;    
    END IF;
  END IF;
  --

  p := 442;
  IF NOT EndGame AND (bkk=85) OR OpenGame AND ((MOD(bkk,10)>3) OR (MOD(bkk,10)<7)) THEN
    p := 443;
    f:=0; -- O-O-O
    IF (stilling(stOff+ 71)<>bP) AND (stilling(stOff+ 61)<>bP) THEN f := f + 1; END IF;
    IF (stilling(stOff+ 72)<>bP) AND (stilling(stOff+ 72)<>bB) AND (stilling(stOff+ 63)<>bB) THEN f := f + 1; END IF;
    IF stilling(stOff+ 73)<>bP THEN f := f + 1; END IF;
    FOR x IN 1..3 LOOP IF rr(x) THEN f := f + 1; END IF; END LOOP;
    IF f>0 THEN 
      ix := pdN(bK,83); pdw(ix) := pdw(ix) - f*f*AvoidCastling;
    END IF;
    f:=0; -- O-O
    IF (stilling(stOff+ 78)<>bP) AND (stilling(stOff+ 68)<>bP) THEN f := f +1; END IF;
    IF (stilling(stOff+ 77)<>bP) AND (stilling(stOff+ 77)<>bB) AND (stilling(stOff+ 66)<>bB) THEN f := f +1; END IF;
    IF stilling(stOff+ 76)<>bP THEN  f := f +1; END IF;
    p := 446;
    FOR x IN 6..8 LOOP IF rr(x) THEN  f := f + 1; END IF; END LOOP;
    p := 447;
    IF f>0 THEN
      p := 448;
      ix := pdN(bK,87); pdw(ix) := pdw(ix) - f*f*AvoidCastling;
    ELSE
      IF (stilling(stOff+ 85)=bM) AND (stilling(stOff+ 86)=bB) AND (stilling(stOff+ 87)=bN) THEN -- undeveloped king-side penalty
        p := 449;
        ix := pdN(bB,86); pdw(ix) := pdw(ix) - 8;
        ix := pdN(bN,87); pdw(ix) := pdw(ix) - 8;
      END IF; 
    END IF;
  END IF;
 p := 450;
 
  -- adjust around opponent king (wk,bk) as better fields (mating)
  IF EndGame THEN
    -- attract the kings to areas with pieces, own AND opponents
    p := 452;
    PieceAdjust;
    p := 453;    
    SetP(wQ,0,KingArea30);
    p := 454;
    SetP(wQ,1,KingArea30);
    SetP(wQ,2,KingArea20);
    SetP(wQ,3,KingArea12);
    SetP(wQ,4,KingArea8);
    SetP(wR,0,KingArea30);
    SetP(wR,1,KingArea30);
    SetP(wR,2,KingArea20);
    SetP(wR,3,KingArea12);
    SetP(wR,4,KingArea8);
    SetP(wK,0,KingArea20);
    SetP(wK,1,KingArea20);
    SetP(wK,2,KingArea16);
    SetP(wK,3,KingArea12);
    SetP(wK,4,KingArea8);
  ELSE
    p := 460;
    IF (stilling(stOff+ 11)=wR) AND (wkk=12) OR (wkk=13) THEN -- rook in corner
      ix := pdN(wR,11); pdw(ix) := pdw(ix) - 10;
      ix := pdN(wR,21); pdw(ix) := pdw(ix) - 10;
      ix := pdN(wR,12); pdw(ix) := pdw(ix) - 10;
      ix := pdN(wK,14); pdw(ix) := pdw(ix) - FLOOR(RokeretBonus / 2) ;
      ix := pdN(wK,21); pdw(ix) := pdw(ix) + RokeretBonus+10;
      ix := pdN(wK,22); pdw(ix) := pdw(ix) + RokeretBonus+10;
      ix := pdN(wK,23); pdw(ix) := pdw(ix) + RokeretBonus;
      IF (stilling(stOff+ wkk+9)=wP) AND (stilling(stOff+ wkk+10)=wP) THEN
        ix := pdN(wP,31); pdw(ix) := pdw(ix) + RokeretBonus+10;
        ix := pdN(wP,32); pdw(ix) := pdw(ix) + RokeretBonus+10;
        ix := pdN(wP,33); pdw(ix) := pdw(ix) + RokeretBonus;
        ix := pdN(wP,41); pdw(ix) := pdw(ix) + RokeretBonus+10;
      END IF;
    END IF;
    IF (stilling(stOff+ 18)=wR) AND (wkk=17) OR (wkk=16) THEN -- rook in corner
      ix := pdN(wR,18); pdw(ix) := pdw(ix) - 10;
      ix := pdN(wR,28); pdw(ix) := pdw(ix) - 10;
      ix := pdN(wR,17); pdw(ix) := pdw(ix) - 10;
      ix := pdN(wK,15); pdw(ix) := pdw(ix) - FLOOR(RokeretBonus/2);
      ix := pdN(wK,28); pdw(ix) := pdw(ix) + RokeretBonus+10;
      ix := pdN(wK,27); pdw(ix) := pdw(ix) + RokeretBonus+10;
      ix := pdN(wK,26); pdw(ix) := pdw(ix) + RokeretBonus;
      IF (stilling(stOff+ wkk+11)=wP) AND (stilling(stOff+ wkk+10)=wP) THEN
        ix := pdN(wP,38); pdw(ix) := pdw(ix) + RokeretBonus+10;
        ix := pdN(wP,37); pdw(ix) := pdw(ix) + RokeretBonus+10;
        ix := pdN(wP,36); pdw(ix) := pdw(ix) + RokeretBonus;
        ix := pdN(wP,48); pdw(ix) := pdw(ix) + RokeretBonus+10;
      END IF;
    END IF;

    IF (stilling(stOff+ 81)=bR) AND (bkk=82) OR (bkk=83) THEN -- rook in corner
      ix := pdN(bR,81); pdw(ix) := pdw(ix) - 10;
      ix := pdN(bR,71); pdw(ix) := pdw(ix) - 10;
      ix := pdN(bR,82); pdw(ix) := pdw(ix) - 10;
      ix := pdN(bK,84); pdw(ix) := pdw(ix) - FLOOR(RokeretBonus/2);
      ix := pdN(bK,71); pdw(ix) := pdw(ix) + RokeretBonus+10;
      ix := pdN(bK,72); pdw(ix) := pdw(ix) + RokeretBonus+10;
      ix := pdN(bK,73); pdw(ix) := pdw(ix) + RokeretBonus;
      IF (stilling(stOff+ bkk-11)=bP) AND (stilling(stOff+ bkk-10)=bP) THEN
        ix := pdN(bP,61); pdw(ix) := pdw(ix) + RokeretBonus+10;
        ix := pdN(bP,62); pdw(ix) := pdw(ix) + RokeretBonus+10;
        ix := pdN(bP,63); pdw(ix) := pdw(ix) + RokeretBonus;
        ix := pdN(bP,51); pdw(ix) := pdw(ix) + RokeretBonus+10;
      END IF;
    END IF;
    
    IF (stilling(stOff+ 88)=bR) AND (bkk=87) OR (bkk=86) THEN -- rook in corner
      ix := pdN(bR,88); pdw(ix) := pdw(ix) - 10;
      ix := pdN(bR,78); pdw(ix) := pdw(ix) - 10;
      ix := pdN(bR,87); pdw(ix) := pdw(ix) - 10;
      ix := pdN(bK,85); pdw(ix) := pdw(ix) - FLOOR(RokeretBonus/2);
      ix := pdN(bK,78); pdw(ix) := pdw(ix) + RokeretBonus+16;
      ix := pdN(bK,77); pdw(ix) := pdw(ix) + RokeretBonus+16;
      ix := pdN(bK,76); pdw(ix) := pdw(ix) + RokeretBonus;
      IF (stilling(stOff+ bkk-9)=bP) AND (stilling(stOff+ bkk-10)=bP) THEN
        ix := pdN(bP,68); pdw(ix) := pdw(ix) + RokeretBonus+10;
        ix := pdN(bP,67); pdw(ix) := pdw(ix) + RokeretBonus+10;
        ix := pdN(bP,66); pdw(ix) := pdw(ix) + RokeretBonus;
        ix := pdN(bP,58); pdw(ix) := pdw(ix) + RokeretBonus+10;
      END IF;
    END IF;

    -- king positions adjust up around
    p := 470;
    KingAdj(wkk);
    KingAdj(bkk);
  END IF;

  -- if closed center, add value to strategical pawn moves, decrement bad bishop
  IF ClosedD4 THEN
    IF ClosedE5 THEN
      ix := pdN(wP,46); pdw(ix) := pdw(ix) + PawnStrateg10;
      ix := pdN(wP,56); pdw(ix) := pdw(ix) + PawnStrateg20;
      ix := pdN(bP,53); pdw(ix) := pdw(ix) + PawnStrateg20;
    END IF;
    IF ClosedE3 THEN
      ix := pdN(wP,43); pdw(ix) := pdw(ix) + PawnStrateg20;
      ix := pdN(bP,56); pdw(ix) := pdw(ix) + PawnStrateg10;
      ix := pdN(bP,46); pdw(ix) := pdw(ix) + PawnStrateg20;
    END IF;   
    IF ClosedE5 OR ClosedE3 THEN -- decrement white/black bad bishops on own half
      FOR m IN 1..4 LOOP
        FOR n IN 1..4 LOOP
          f:=10*m+n*2;
          IF MOD(m,2)=1 THEN
            ix := pdN(wB,f-1);  pdw(ix) := pdw(ix) - BishopClosed ;
            ix := pdN(bB,f+40); pdw(ix) := pdw(ix) - BishopClosed ;
          ELSE
            ix := pdN(wB,f);    pdw(ix) := pdw(ix) - BishopClosed ;
            ix := pdN(bB,f+39); pdw(ix) := pdw(ix) - BishopClosed ;
          END IF;
        END LOOP;
      END LOOP;
    END IF;
  END IF;
  p := 472;
  IF ClosedE4 THEN
    IF ClosedD5 THEN
      ix := pdN(wP,43); pdw(ix) := pdw(ix) + PawnStrateg10;
      ix := pdN(wP,53); pdw(ix) := pdw(ix) + PawnStrateg20;
      ix := pdN(bP,56); pdw(ix) := pdw(ix) + PawnStrateg20;
    END IF;
    IF ClosedD3 THEN
      ix := pdN(wP,46); pdw(ix) := pdw(ix) + PawnStrateg20;
      ix := pdN(bP,53); pdw(ix) := pdw(ix) + PawnStrateg10;
      ix := pdN(bP,43); pdw(ix) := pdw(ix) + PawnStrateg20;
    END IF;   
    IF ClosedD5 OR ClosedD3 THEN -- decrement white/black bad bishops on own half
      FOR m IN 1..4 LOOP
        FOR n IN 1..4 LOOP
          f:=10*m+n*2;
          IF MOD(m,2)=1 THEN
            ix := pdN(wB,f);    pdw(ix) := pdw(ix) - BishopClosed;
            ix := pdN(bB,f+39); pdw(ix) := pdw(ix) - BishopClosed;
          ELSE
            ix := pdN(wB,f-1);    pdw(ix) := pdw(ix) - BishopClosed;
            ix := pdN(bB,f+40);   pdw(ix) := pdw(ix) - BishopClosed;
          END IF;
        END LOOP;
      END LOOP;
    END IF;
  END IF;

  -- add value to bishops if open center
  IF NOT ClosedD4 AND NOT ClosedE4 THEN
    FOR n IN 11..88 LOOP
      ix := pdN(wB,n); pdw(ix) := pdw(ix) + BishopOpen;
      ix := pdN(bB,n); pdw(ix) := pdw(ix) + BishopOpen;
    END LOOP;
  END IF;

  -- add value to knights if closed center
  IF ClosedD4 AND (ClosedE3 OR ClosedE5) OR  ClosedE4 AND (ClosedD3 OR ClosedD5) THEN
    FOR n IN 11..88 LOOP
      ix := pdN(wN,n); pdw(ix) := pdw(ix) + KnightClosed;
      ix := pdN(bN,n); pdw(ix) := pdw(ix) + KnightClosed;
    END LOOP;
  END IF;

  -- rooks bonus on open lines
  FOR n IN 1..8 LOOP
    nbb:=0; nB:=0;
    FOR m IN 2..7 LOOP
      ch_n:=stilling(stOff+ 10*m+n);
      IF ch_n=wP THEN nbb := nbb+1; END IF;
      IF ch_n=bP THEN nbb := nbb+1; END IF;
    END LOOP;
    IF nbb=0 THEN
      IF nB=1 THEN f:=RookHalfOpenLine; ELSE f:=RookFullOpenLine; END IF;
      FOR m in 1..2 LOOP
        ix := pdN(wR,10*m+n); pdw(ix) := pdw(ix) + f;
      END LOOP;
      IF ((n=1) OR (n=8)) THEN 
        ix := pdN(wC,10+n); pdw(ix) := pdw(ix) + f;
      END IF;
    END IF;
    IF nB=0 THEN
      IF nbb=1 THEN f:=RookHalfOpenLine; ELSE f:=RookFullOpenLine; END IF;
      FOR m in 7..8 LOOP
        ix := pdN(bR,10*m+n); pdw(ix) := pdw(ix) + f;
      END LOOP;
      IF ((n=1) OR (n=8)) THEN 
        ix := pdN(bC,10+n); pdw(ix) := pdw(ix) + f;
      END IF;
    END IF;
  END LOOP;
  
  p := 480;
  IF OpenGame THEN
    p := 481;
    -- Bishop b/g5 bonus if not too early
    IF (stilling(stOff+ 33)=wN) THEN 
      ix := pdN(bB,42); pdw(ix) := pdw(ix) + pdB5;
    ELSE 
      ix := pdN(bB,42); pdw(ix) := pdw(ix) - FLOOR(pdB5/2);
    END IF;
    IF (stilling(stOff+ 63)=bN) THEN 
      ix := pdN(wB,52); pdw(ix) := pdw(ix) + pdB5;
    ELSE 
      ix := pdN(wB,52); pdw(ix) := pdw(ix) - FLOOR(pdB5/2);
    END IF;
    IF (stilling(stOff+ 36)=wN) THEN 
      ix := pdN(bB,47); pdw(ix) := pdw(ix) + pdB5;
    ELSE 
      ix := pdN(bB,47); pdw(ix) := pdw(ix) - FLOOR(pdB5/2);
    END IF;
    IF (stilling(stOff+ 66)=bN) THEN 
      ix := pdN(wB,57); pdw(ix) := pdw(ix) + pdB5;
    ELSE 
      ix := pdN(wB,57); pdw(ix) := pdw(ix) - FLOOR(pdB5/2);
    END IF;
    
    -- too early knights
    IF (stilling(stOff+ 44)=wP) AND (stilling(stOff+ 54)=spc) THEN
      IF stilling(stOff+ 63)=bN THEN
        ix := pdN(bP,54); pdw(ix) := pdw(ix) + EarlyKnight;
        ix := pdN(bP,65); pdw(ix) := pdw(ix) + EarlyKnight;
      ELSE
        ix := pdN(bN,63); pdw(ix) := pdw(ix) - EarlyKnight;
        IF stilling(stOff+ 75)=bP THEN 
          ix := pdN(bN,63); pdw(ix) := pdw(ix) - EarlyKnight;
        END IF;
      END IF;
    END IF;
    IF (stilling(stOff+ 54)=bP) AND (stilling(stOff+ 44)=spc) THEN 
      IF stilling(stOff+ 33)=wN THEN
        ix := pdN(wP,44); pdw(ix) := pdw(ix) + EarlyKnight;
        ix := pdN(wP,35); pdw(ix) := pdw(ix) + EarlyKnight;
      ELSE
        ix := pdN(wN,33); pdw(ix) := pdw(ix) - EarlyKnight;
        IF stilling(stOff+ 25)=wP THEN 
          ix := pdN(wN,33); pdw(ix) := pdw(ix) - EarlyKnight;
        END IF;
      END IF;
    END IF;
    IF (stilling(stOff+ 45)=wP) AND (stilling(stOff+ 55)=spc) THEN 
      IF stilling(stOff+ 66)=bN THEN
        ix := pdN(bP,55); pdw(ix) := pdw(ix) + EarlyKnight;
        ix := pdN(bP,64); pdw(ix) := pdw(ix) + EarlyKnight;
      ELSE
        ix := pdN(bN,66); pdw(ix) := pdw(ix) - EarlyKnight;
        IF stilling(stOff+ 74)=bP THEN 
          ix := pdN(bN,66); pdw(ix) := pdw(ix) - EarlyKnight;
        END IF;
      END IF;
    END IF;
    IF (stilling(stOff+ 55)=bP) AND (stilling(stOff+ 45)=spc) THEN 
      IF stilling(stOff+ 36)=wN THEN
        ix := pdN(wP,45); pdw(ix) := pdw(ix) + EarlyKnight;
        ix := pdN(wP,34); pdw(ix) := pdw(ix) + EarlyKnight;
      ELSE
        ix := pdN(wN,36); pdw(ix) := pdw(ix) - EarlyKnight;
        IF stilling(stOff+ 24)=wP THEN 
          ix := pdN(wN,36); pdw(ix) := pdw(ix) - EarlyKnight;
        END IF;
      END IF;
    END IF;
  END IF;
  
  -- too early bishops
  IF (stilling(stOff+ 33)=spc) AND (stilling(stOff+ 23)=wP) THEN
    ix := pdN(bB,42); pdw(ix) := pdw(ix) - EarlyKnight;
  END IF;
  IF (stilling(stOff+ 36)=spc) AND (stilling(stOff+ 26)=wP) THEN
    ix := pdN(bB,47); pdw(ix) := pdw(ix) - EarlyKnight;
  END IF;
  IF (stilling(stOff+ 63)=spc) AND (stilling(stOff+ 73)=bP) THEN
    ix := pdN(wB,52); pdw(ix) := pdw(ix) - EarlyKnight;
  END IF;
  IF (stilling(stOff+ 66)=spc) AND (stilling(stOff+ 76)=bP) THEN
    ix := pdN(wB,57); pdw(ix) := pdw(ix) - EarlyKnight;
  END IF;
  IF NOT EndGame THEN
    -- too late bishops
    ix := pdN(bB,86); pdw(ix) := pdw(ix) - LateBishop;
    ix := pdN(bB,83); pdw(ix) := pdw(ix) - LateBishop;
    ix := pdN(wB,13); pdw(ix) := pdw(ix) - LateBishop;
    ix := pdN(wB,16); pdw(ix) := pdw(ix) - LateBishop;
  END IF;

  -- h3
  IF (stilling(stOff+ 47)=bB) OR (stilling(stOff+ 47)=bN) THEN 
    ix := pdN(wP,38); pdw(ix) := pdw(ix) + pawnh3a3attack;
  END IF;
  IF (stilling(stOff+ 57)=wB) OR (stilling(stOff+ 57)=wN) THEN 
    ix := pdN(bP,68); pdw(ix) := pdw(ix) + pawnh3a3attack;
  END IF;
  IF (stilling(stOff+ 42)=bB) OR (stilling(stOff+ 42)=bN) THEN 
    ix := pdN(wP,31); pdw(ix) := pdw(ix) + pawnh3a3attack;--før 38, må være en fejl
  END IF;
  IF (stilling(stOff+ 52)=wB) OR (stilling(stOff+ 52)=wN) THEN 
    ix := pdN(bP,61); pdw(ix) := pdw(ix) + pawnh3a3attack;--før 68, må være en fejl
  END IF;
  
  p := 490;
  --  penalty for bishops behind own pawns
  FOR n_n IN 1..5 LOOP -- LOOP it for whites first 5 rows
    n := n_n*10;
    FOR m IN 1..8 LOOP
      IF stilling(stOff+ n+m)=wP THEN
        IF stilling(stOff+ n+10+m)=bP THEN v:=BishopPenalty*2; ELSE v:=BishopPenalty; END IF; 
        FOR f IN 1..m-1 LOOP -- to left edge
          IF 10*f<n THEN -- if on-board 
            ix := pdN(wB,n-10*f+f); pdw(ix) := pdw(ix) - v;
          END IF;
        END LOOP;
        FOR f IN 1..8-m LOOP -- to right edge
          IF 10*f<n THEN
            ix := pdN(wB,n-10*f+m+f); pdw(ix) := pdw(ix) - v;
          END IF;
        END LOOP;
      END IF;
    END LOOP;
  END LOOP;
  FOR n_n IN 4..8 LOOP -- LOOP it for blacks first 5 rows
    n := n_n * 10;
    FOR m IN 1..8 LOOP
      IF stilling(stOff+ n+m)=bP THEN 
        IF stilling(stOff+ n-10+m)=wP THEN v:=BishopPenalty*2; ELSE v:=BishopPenalty; END IF;
        FOR f IN 1..m-1 LOOP -- to left edge
          IF 10*f+n<90 THEN -- if on-board
            ix := pdN(bB,n+10*f+f); pdw(ix) := pdw(ix) - v;
          END IF;
        END LOOP;
        FOR f IN 1..8-m LOOP -- to right edge
          IF 10*f+n<90 THEN
            ix := pdN(bB,n+10*f+m+f); pdw(ix) := pdw(ix) - v;
          END IF;
        END LOOP;
      END IF;
    END LOOP;
  END LOOP;
  p := 492;
      
  -- mirror pdw to pdb
  FOR ch_n IN wP..wR LOOP 
    CASE 
      WHEN CH_n in ( wP,wE,wR,wC,wK,wM,wQ,wN,wB) THEN
      FOR n IN 1..4 LOOP 
        nm:=10*n;
        om:=90-nm;
        FOR m IN 1..8 LOOP
          nm:=nm+1;
          om:=om+1;
          pdb(pdN(ch_n,nm))        := pdw(pdN(UPPER_n(ch_n),om));
          pdb(pdN(ch_n,om))        := pdw(pdN(UPPER_n(ch_n),nm));
          pdb(pdN(UPPER_n(ch_n),nm)) := pdw(pdN(ch_n,om));
          pdb(pdN(UPPER_n(ch_n),om)) := pdw(pdN(ch_n,nm));
        END LOOP;
      END LOOP;
    ELSE 
      NULL;
    END CASE;
  END LOOP;
  p := 499;
  --dbms_output.put_line(p);
EXCEPTION
  WHEN OTHERS THEN
    dbms_output.put_line('PreProcessor p='||p||': '||sqlerrm);
    --raise;
END PreProcessor;


FUNCTION Eval(stilling STILLINGTYPE, Activity SIMPLE_INTEGER,
              Black BOOLEAN, alpha SIMPLE_INTEGER, beta SIMPLE_INTEGER) RETURN SIMPLE_INTEGER IS
/* evaluates the actual postion using pd and position. */
/* Is called thousands of times for each engine call! */


  n     SIMPLE_INTEGER := 0;
  m     SIMPLE_INTEGER := 0;
  res   SIMPLE_INTEGER := 0;
  kposs SIMPLE_INTEGER := 0;
  Kpos  SIMPLE_INTEGER := 0;
  x     SIMPLE_INTEGER := 0;
  y     SIMPLE_INTEGER := 0;
  y_n   SIMPLE_INTEGER := 0;
  inmv SIMPLE_INTEGER  := 0;   -- Positionel, matriel værdi
  brik           SIMPLE_INTEGER := 0;
  ch             SIMPLE_INTEGER := 0; -- ny
  KingEndGameW  BOOLEAN;
  KingEndGameB  BOOLEAN; -- if k or k+n endgame W/B
  knEndGameW    BOOLEAN;
  knEndGameB    BOOLEAN; -- if k+n endgame W/B      
  priW          SIMPLE_INTEGER := 0;
  priB          SIMPLE_INTEGER := 0; -- for k or kn endgame     
  TYPE pawnsType is varray(89) of BOOLEAN;
  Wpawns  pawnsType := pawnsType(); -- uses 11..89, but is 1..89
  Bpawns  pawnsType := pawnsType();
  pawnsInit BOOLEAN:=FALSE;
  TYPE pawnLineType is varray(8) of SIMPLE_INTEGER;
  WpL   pawnLineType := pawnLineType(); -- var SHORTINT
  BpL   pawnLineType := pawnLineType(); -- Pawns on line (iopb) 
  WpLc          SIMPLE_INTEGER := 0;
  BpLc          SIMPLE_INTEGER := 0; -- Pawns Total (if only pawns back) 
  HvidsTur      BOOLEAN;
  Slut          BOOLEAN;
  wBishops      SIMPLE_INTEGER := 0;
  bBishops      SIMPLE_INTEGER := 0;
  DebugS VARCHAR2(89);
  OpenCenterDegree SIMPLE_INTEGER :=2;
BEGIN
  HvidsTur := stilling(stOff+ HvisTur)<>bN;
  IF HvidsTur THEN 
    inmv:=InMoveBonus;
  ELSE
    inmv:=-InMoveBonus;
  END IF;
  Evals := Evals+1;
  posi  := 0;
  wbonus:= 0;
  bbonus:= 0;
  matr  := 0;
  -- based on static eval, how open is center pawn-structure
  IF  ClosedE4 AND (ClosedD3 or ClosedD5) OR  ClosedD4 AND (ClosedE3 or ClosedE5) THEN 
    OpenCenterDegree:= 0;
  ELSIF ClosedE4 and not ClosedD4 or ClosedD4 and not ClosedE4 THEN
    OpenCenterDegree:= 1;
  --ELSE   
  --  OpenCenterDegree:= 2;--default
  END IF;
  -- KingEndgame if just pawns and knights left. knEndGame if knights left.
  KingEndGameW:=TRUE; knEndGameW:=FALSE;
  KingEndGameB:=TRUE; knEndGameB:=FALSE;
  FOR n IN 11..88 LOOP
    brik:=stilling(stOff+ n);
    IF (brik>bA) THEN 
--WRT(brik);
      CASE WHEN brik IN
        (wR,wC) THEN 
                    matr:=matr+ValueT; KingEndGameW:=FALSE;
                    IF (n<50) THEN
                      IF (stilling(stOff+ n+10)=wP) OR (stilling(stOff+ n+20)=wP) THEN
                        wbonus:=wbonus-RookFirstRowBehindPawn;
                      END IF;
                    END IF;
                    IF n=11 THEN
                      IF brik=wC THEN matr:=matr+(ValueR-ValueT); END IF;
                      IF stilling(stOff+ 12)<>spc THEN
                        IF stilling(stOff+ 12)=wK THEN
                          wbonus:=wbonus-RookCornerNextToKing;
                        ELSE
                          wbonus:=wbonus-RookCornerNextToPiece;
                        END IF;
                      END IF;
                    ELSIF n=18 THEN
                      IF brik=wC THEN matr:=matr+(ValueR-ValueT); END IF;
                      IF stilling(stOff+ 17)<>spc THEN
                        IF stilling(stOff+ 17)=wK THEN
                          wbonus:=wbonus-RookCornerNextToKing;
                        ELSE
                          wbonus:=wbonus-RookCornerNextToPiece;
                        END IF;
                      END IF;
                    END IF;
        WHEN brik=wM  THEN 
                    matr:=matr+ValueM; kposs:=n;
        WHEN brik= wK     THEN 
                    matr:=matr+ValueK; kposs:=n;
                    IF NOT EndGame THEN
                      IF (n=16) AND ((stilling(stOff+ 18)=wR) OR (stilling(stOff+ 17)=wR))
                      OR (n=13) AND ((stilling(stOff+ 11)=wR) OR (stilling(stOff+ 12)=wR)) THEN
                        IF (n=16) AND (stilling(stOff+ 26)=wP) AND (stilling(stOff+ 27)=wP) 
                        OR (n=13) AND (stilling(stOff+ 23)=wP) AND (stilling(stOff+ 22)=wP) THEN
                          wbonus:=wbonus-KingCloseRook;
                        END IF;
                      ELSE
                        FOR m IN n+9..n+11 LOOP
                          IF stilling(stOff+ m)=wP THEN wbonus:=wbonus+12; END IF;
                        END LOOP;
                        wbonus:=wbonus-KingBehindPawns;
                      END IF;
                    END IF;
        WHEN brik= wQ     THEN matr:=matr+ValueD; KingEndGameW:=FALSE;
                    IF OpenGame THEN
                      IF n>28 THEN
                        wbonus:=wbonus-QueenDeveloped;
                        IF n>41 THEN
                          wbonus:=wbonus-12;
                        ELSE
                          IF (n=34) AND (stilling(stOff+ 24)=wP) THEN
                            wbonus:=wbonus-QueenBeforePawn;
                            IF (stilling(stOff+ 13)=wB) AND (stilling(stOff+ 22)=wP) THEN
                              wbonus:=wbonus-QueenBeforePawnBlockedBishop;
                            END IF;
                          ELSIF (n=35) AND (stilling(stOff+ 25)=wP) THEN
                            wbonus:=wbonus-QueenBeforePawn;
                            IF (stilling(stOff+ 16)=wB) AND (stilling(stOff+ 27)=wP) THEN
                              wbonus:=wbonus-QueenBeforePawnBlockedBishop;
                            END IF;
                          END IF;
                        END IF;
                      ELSIF (n=24) AND (stilling(stOff+ 13)=wB) AND (stilling(stOff+ 22)=wP)
                      OR    (n=25) AND (stilling(stOff+ 16)=wB) AND (stilling(stOff+ 27)=wP) THEN
                        wbonus :=wbonus-QueenBeforeBishop;
                      END IF;
                    END IF;
        WHEN brik= wB     THEN 
                    matr:=matr+ValueL; KingEndGameW:=FALSE;wBishops:=wBishops+1;
                    IF (n<39) AND (n>30) THEN IF stilling(stOff+ n-10)=wP THEN wbonus:=wbonus-BishopBeforePawn; END IF; END IF;
                    IF (n=13) AND (stilling(stOff+ 22)=wP) AND (stilling(stOff+ 24)<>spc) THEN wbonus:=wbonus-BishopBehindPawn; END IF;  
                    IF (n=16) AND (stilling(stOff+ 27)=wP) AND (stilling(stOff+ 25)<>spc) THEN wbonus:=wbonus-BishopBehindPawn; END IF;  
                    IF (stilling(stOff+ n-11)=wP) OR (stilling(stOff+ n-9)=wP) THEN wbonus:=wbonus+BishopProtectedByPawn; END IF;
        WHEN brik= wN     THEN 
                    matr:=matr+Value_S; knEndGameW:=TRUE;
                    IF (n<39) AND (n>30) THEN IF stilling(stOff+ n-10)=wP THEN wbonus:=wbonus-kNightBeforePawn; END IF; END IF;
                    IF (stilling(stOff+ n-11)=wP) OR (stilling(stOff+ n-9)=wP) THEN wbonus:=wbonus+kNightProtectedByPawn; END IF;
                    IF (stilling(stOff+ n+10)=bP) AND (stilling(stOff+ n+11)<>bP) AND (stilling(stOff+ n+9)<>bP) THEN
                      wbonus:=wbonus+kNightpPawnInFront;
                      IF (n>50) OR (stilling(stOff+ n+19)<>bP) AND (stilling(stOff+ n+21)<>bP) THEN 
                        wbonus:=wbonus+kNightpPawnInFront;
                        IF (n>60) OR (stilling(stOff+ n+29)<>bP) AND (stilling(stOff+ n+31)<>bP) THEN 
                          wbonus:=wbonus+kNightpPawnInFront;
                        END IF;
                      END IF;
                    END IF;
        WHEN brik IN (wP,wE) THEN matr:=matr+ValueB;
                    IF stilling(stOff+ n- 1)=wP THEN
                      IF EndGame THEN
                        IF n>70 THEN
                          wbonus:=wbonus+TwoOnRow7;
                        ELSIF n>60 THEN
                          wbonus:=wbonus+TwoOnRow6;
                        ELSIF n>50 THEN
                          wbonus:=wbonus+TwoOnRow5;
                        END IF;
                      END IF;
                      wbonus:=wbonus+PawnNextToPawn;
                    END IF;
                    IF stilling(stOff+ n+ 1)=wP THEN wbonus:=wbonus+PawnNextToPawn; END IF;
                    IF stilling(stOff+ n- 9)=wP THEN wbonus:=wbonus+PawnGuardsPawn; END IF;
                    IF stilling(stOff+ n-11)=wP THEN wbonus:=wbonus+PawnGuardsPawn; END IF;
                    IF stilling(stOff+ n-10)=wP THEN wbonus:=wbonus-PawnDoubbledPawn; END IF;
                    IF stilling(stOff+ n-20)=wP THEN wbonus:=wbonus-PawnDoubbledPawn; END IF;
        WHEN brik in (bR,bC) THEN 
                    matr:=matr-ValueT; KingEndGameB:=FALSE;
                    IF (n>40) THEN
                      IF (stilling(stOff+ n-10)=bP) OR (stilling(stOff+ n-20)=bP) THEN
                        bbonus:=bbonus-RookFirstRowBehindPawn;
                      END IF;
                    END IF;
                    IF n=81 THEN
                      IF brik=bC THEN matr:=matr+(ValueT-ValueR); END IF;
                      IF stilling(stOff+ 82)<>spc THEN
                        IF stilling(stOff+ 82)=bK THEN
                          bbonus:=bbonus-RookCornerNextToKing;
                        ELSE
                          bbonus:=bbonus-RookCornerNextToPiece;
                        END IF;
                      END IF;
                    ELSIF n=88 THEN
                      IF brik=bC THEN matr:=matr+(ValueT-ValueR); END IF;
                      IF stilling(stOff+ 87)<>spc THEN
                        IF stilling(stOff+ 87)=bK THEN
                          bbonus:=bbonus-RookCornerNextToKing;
                        ELSE
                          bbonus:=bbonus-RookCornerNextToPiece;
                        END IF;
                      END IF;
                    END IF;
        WHEN brik= bM     THEN 
                    matr:=matr-ValueM; Kpos:=n;
        WHEN brik= bK     THEN 
                    matr:=matr-ValueK; Kpos:=n;
                    IF NOT EndGame THEN
                      IF (n=86) AND ((stilling(stOff+ 88)=bR) OR (stilling(stOff+ 87)=bR))
                      OR (n=83) AND ((stilling(stOff+ 81)=bR) OR (stilling(stOff+ 82)=bR)) THEN
                        IF (n=86) AND (stilling(stOff+ 76)=bP) AND (stilling(stOff+ 77)=bP) 
                        OR (n=83) AND (stilling(stOff+ 73)=bP) AND (stilling(stOff+ 72)=bP) THEN
                          bbonus:=bbonus-KingCloseRook;
                        END IF;
                      ELSE
                        FOR m IN n-11..n-9 LOOP
                          IF stilling(stOff+ m)=bP THEN bbonus:=bbonus+KingBehindPawns; END IF;
                        END LOOP;
                        bbonus:=bbonus-KingBehindPawns;
                      END IF;
                    END IF;
        WHEN brik= bQ     THEN 
                    matr:=matr-ValueD; KingEndGameB:=FALSE;
                    IF OpenGame THEN
                      IF n<71 THEN
                        bbonus:=bbonus-12;
                        IF n<61 THEN
                           IF n<>51 THEN bbonus:=bbonus-QueenDeveloped; END IF;
                        ELSE
                          IF (n=64) AND (stilling(stOff+ 74)=bP) THEN
                            bbonus:=bbonus-QueenBeforePawn;
                            IF (stilling(stOff+ 83)=bB) AND (stilling(stOff+ 72)=bP) THEN
                              bbonus:=bbonus-QueenBeforePawnBlockedBishop;
                            END IF;
                          ELSIF (n=65) AND (stilling(stOff+ 75)=bP) THEN
                            bbonus:=bbonus-QueenBeforePawn;
                            IF (stilling(stOff+ 86)=bB) AND (stilling(stOff+ 77)=bP) THEN
                              bbonus:=bbonus-QueenBeforePawnBlockedBishop;
                            END IF;
                          END IF;
                        END IF;
                      ELSIF (n=74) AND (stilling(stOff+ 83)=bB) AND (stilling(stOff+ 72)=bP)
                      OR    (n=75) AND (stilling(stOff+ 86)=bB) AND (stilling(stOff+ 77)=bP) THEN
                        bbonus :=bbonus-QueenBeforeBishop;
                      END IF;
                    END IF;
        WHEN brik= bB     THEN 
                    matr:=matr-ValueL; KingEndGameB:=FALSE;bBishops:=bBishops+1;
                    IF (n>60) AND (n<69) THEN IF stilling(stOff+ n+10)=bP THEN bbonus:=bbonus-BishopBeforePawn; END IF; END IF;
                    IF (n=83) AND (stilling(stOff+ 72)=bP) AND (stilling(stOff+ 74)<>spc) THEN bbonus:=bbonus-BishopBehindPawn; END IF;  
                    IF (n=86) AND (stilling(stOff+ 77)=bP) AND (stilling(stOff+ 75)<>spc) THEN bbonus:=bbonus-BishopBehindPawn; END IF;  
                    IF (stilling(stOff+ n+11)=bP) OR (stilling(stOff+ n+9)=bP) THEN bbonus:=bbonus+BishopProtectedByPawn; END IF;
        WHEN brik= bN     THEN 
                    matr:=matr-Value_S; knEndGameB:=TRUE;
                    IF (n>60) AND (n<69) THEN IF stilling(stOff+ n+10)=bP THEN bbonus:=bbonus-kNightBeforePawn; END IF; END IF;
                    IF (stilling(stOff+ n+11)=bP) OR (stilling(stOff+ n+9)=bP) THEN bbonus:=bbonus+kNightProtectedByPawn; END IF;
                    IF (stilling(stOff+ n-10)=wP) AND (stilling(stOff+ n-11)<>wP) AND (stilling(stOff+ n-9)<>wP) THEN
                      bbonus:=bbonus+kNightpPawnInFront;
                      IF (n<31) OR (stilling(stOff+ n-19)<>wP) AND (stilling(stOff+ n-21)<>wP) THEN 
                        bbonus:=bbonus+kNightpPawnInFront;
                        IF (n<41) OR (stilling(stOff+ n-29)<>wP) AND (stilling(stOff+ n-31)<>wP) THEN 
                          bbonus:=bbonus+kNightpPawnInFront;
                        END IF;
                      END IF;
                    END IF;
        WHEN brik in (bP,bE) THEN 
                    matr:=matr-ValueB;
                    IF stilling(stOff+ n- 1)=bP THEN
                      IF EndGame THEN
                        IF n<29 THEN
                          bbonus:=bbonus+TwoOnRow7;
                        ELSIF n<39 THEN
                          bbonus:=bbonus+TwoOnRow6;
                        ELSIF n<49 THEN
                          bbonus:=bbonus+TwoOnRow5;
                        END IF;
                      END IF;
                      bbonus:=bbonus+PawnNextToPawn;
                    END IF;
                    IF stilling(stOff+ n+ 1)=bP THEN bbonus:=bbonus+PawnNextToPawn; END IF;
                    IF stilling(stOff+ n+ 9)=bP THEN bbonus:=bbonus+PawnGuardsPawn; END IF;
                    IF stilling(stOff+ n+11)=bP THEN bbonus:=bbonus+PawnGuardsPawn; END IF;
                    IF stilling(stOff+ n+10)=bP THEN bbonus:=bbonus-PawnDoubbledPawn; END IF;
                    IF stilling(stOff+ n+20)=bP THEN bbonus:=bbonus-PawnDoubbledPawn; END IF;
      ELSE
        NULL; 
      END CASE;
--(*$ IF Test0
--WRITEF(s('POSI (')+l(posi)+c(')'));
--(*$ENDIF
      IF brik=UPPER_n(brik) THEN 
        posi:=posi-pd(pdN(brik,n)); -- 99-n
      ELSE 
        posi:=posi+pd(pdN(brik,n)); -- UPPER_n(brik)
      END IF;
--(*$ IF Test0
--WRITELNF(s(' brik=')+c(brik)+s(' n=')+l(n)+s(' posi=')+l(posi));
--(*$ENDIF
    END IF;
  END LOOP;
  IF wBishops = 2 THEN wbonus:=wbonus+BishopPairBonus*OpenCenterDegree; END IF;
  IF bBishops = 2 THEN bbonus:=bbonus+BishopPairBonus*OpenCenterDegree; END IF;

  --short-cut experiment (no detailed eval if more than 300 better than nessecary
  -- Makes it just one % faster and serches 2% more positions. NO change in strength, so 
  -- Not a good complication:
  --IF matr<alpha-300 OR matr>beta+300 THEN
  --      IF Black THEN
  --        return -matr-posi-wbonus+bbonus -inmv;
  --       ELSE
  --        return  matr+posi+wbonus-bbonus +inmv;     
  --      END IF;
  --END IF;

  IF (KingEndGameW OR KingEndGameB) AND ABS(matr+posi+wbonus-bbonus) < 800 THEN --dont use energy to more exact endgame-evaluations in totally won/lost positions (100= one pawn)
    IF knEndGameW THEN priW:=knEndGamePrio; ELSE priW:=knEndGamePrio*2; END IF;
    IF knEndGameB THEN priB:=knEndGamePrio; ELSE priB:=knEndGamePrio*2; END IF;
    --WRT('KingEndgame '||CASE WHEN KingEndGameW THEN 'W' END||','||CASE WHEN KingEndGameB THEN bP END);
    --FOR yy IN 1..8 LOOP
    --  DebugS := edge;
    --  FOR xx IN 1..8 LOOP
    --    DebugS := DebugS||stilling(stOff+ xx+10*(9-yy));
    --  END LOOP;
    --  WRT(DebugS);
    --END LOOP;
    --WRT('matr='||matr||' wbonus='||wbonus||' bbonus='||bbonus||' posi='||posi); --matr=-1559 wbonus=-3 bbonus=24 posi=-33
    --WRT(stilling(stOff+ hvistur));
    IF NOT pawnsInit THEN
      Wpawns.extend(89);
      Bpawns.extend(89);
      WpL.extend(8);
      BpL.extend(8);
      pawnsInit := TRUE;
    END IF;
    
    FOR n IN 11..88 LOOP
      Wpawns(n):=FALSE;
      Bpawns(n):=FALSE;
    END LOOP;

    FOR x IN 1..8 LOOP WpL(x):=0; BpL(x):=0; END LOOP;
    WpLc:=0; BpLc:=0;
    -- mark squares with pawn defend
    FOR y IN 2..7 LOOP
      FOR x IN 1..8 LOOP
        n:=10*y+x;
        ch := stilling(stOff+ n);
        CASE 
         WHEN ch in (bP,bE) THEN 
                    IF KingEndGameB THEN
                      BpL(x) := BpL(x) + 1;
                      BpLc := BpLc + 1;
                      Bpawns(n-10):=TRUE;
                      IF NOT HvidsTur THEN 
                        Bpawns(n-11):=TRUE; -- 10 too!
                        Bpawns(n- 9):=TRUE;
                      END IF;
                      --FOR m IN 20+x..n-20 BY 10 LOOP
                      FOR m_n in 2..y-2 LOOP --testet grundígt!!!
                        m := m_n*10+x;
                        Bpawns(m)  :=TRUE;
                        Bpawns(m-1):=TRUE;
                        Bpawns(m+1):=TRUE;
                      END LOOP;
                    END IF;
        WHEN ch in (wP,wE)  THEN 
                    IF KingEndGameW THEN
                      WpL(x) := WpL(x) + 1;
                      WpLc := WpLc+1;
                      Wpawns(n+10):=TRUE;
                      IF HvidsTur THEN 
                        Wpawns(n+11):=TRUE; -- 89 too!
                        Wpawns(n+ 9):=TRUE;
                      END IF;
                      --FOR m IN n+20..70+x BY 10 LOOP
                      FOR m_n in 2+y..7 LOOP--testet grundígt!!!
                        m := m_n*10+x;
                        Wpawns(m)  :=TRUE;
                        Wpawns(m-1):=TRUE;
                        Wpawns(m+1):=TRUE;
                      END LOOP;
                    END IF;
        ELSE 
          NULL;
        END CASE;
      END LOOP;
    END LOOP;

    -- find first (nearest promotion) free white pawn
    IF KingEndGameB THEN
      Slut:=FALSE;
      --IF NOT knEndGameB AND (posi<0) THEN -- Check if only pawns on A or H file
      --  -- IF king in the corner then equal !!!!!!! VIRKER VIST IKKE!!!!!*)
      --  IF (BpLc=BpL(8)) AND ((kposs=17) OR (kposs=18) OR (kposs=27) OR (kposs=28)) 
      --  OR (BpLc=BpL(1)) AND ((kposs=12) OR (kposs=11) OR (kposs=22) OR (kposs=21)) THEN
      --    RETURN(0);
      --  END IF;
      --END IF;
      <<outer_y1>>
      FOR y_n IN 3..7 LOOP--FOR y IN 7..3 BY -1 
        y := 10 - y_n;
        FOR x IN 1..8 LOOP
          n:=10*y+x;
          IF (stilling(stOff+ n)=wP) OR (y=3) AND (stilling(stOff+ n-10)=wP) THEN
            IF NOT Bpawns(n) THEN
              IF (FLOOR(Kpos/10)<y) OR (ABS( MOD(Kpos,10-x))>8-y) THEN
                Slut:=TRUE; -- inside quadrant
              ELSIF NOT knEndGameB THEN
                IF (n>60) THEN
                  IF (kposs=n+9) OR (kposs=n+11) THEN
                    Slut:=TRUE; -- Supported pawn
                  ELSIF n>70 THEN
                    IF (kposs=n-1) OR (kposs=n+1)
                    OR (kposs=n-12) AND (kposs<=n-8) AND (Kpos<>n+10) THEN
                      Slut:=TRUE; -- Supported pawn
                    END IF;
                  ELSE
                    IF (kposs=n-1) AND (MOD(Kpos,10)>x)
                    OR (kposs=n+1) AND (MOD(Kpos,10)<x) THEN
                      Slut:=TRUE; -- Supported pawn in one move
                      posi:=posi-SupportedPawnPenalty;
                    END IF;
                  END IF;
                ELSIF (n>50) THEN
                  -- IF Possibly supported pawn then add value:
                  IF (kposs=n+9)  OR (kposs=n+11) THEN posi:=posi+ SupportedPawn0911; END IF;
                  IF (kposs=n+10) OR (kposs=n+20) THEN posi:=posi+ SupportedPawn1020; END IF;
                  IF (kposs=n+19) OR (kposs=n+21) THEN posi:=posi+ SupportedPawn1921; END IF;
                END IF;
              END IF;
              IF Slut THEN
                posi:=posi+y*y*priW; 
                exit outer_y1;--x:=8; y:=3; -- pawn can promote, exit loop
--(*$ IF Test0 --WRITELNF(s(' fpW felt=')+l(n)+s(' posi=')+l(posi)); --(*$ ENDIF
              END IF;
            END IF;
          END IF;
        END LOOP;
      END LOOP outer_y1;
    END IF;
    -- find first (nearest promotion) free black pawn
    IF KingEndGameW THEN
      Slut:=FALSE;
      --IF NOT knEndGameW AND (posi>0) THEN -- Check if only pawns on A or H file
      --  -- IF king in the corner then equal !!!!!!! MAY NOT WORK!!!!!
      --  IF (WpLc=WpL(8)) AND ((Kpos=87) OR (Kpos=88) OR (Kpos=77) OR (Kpos=78)) 
      --  OR (WpLc=WpL(1)) AND ((Kpos=72) OR (Kpos=71) OR (Kpos=72) OR (Kpos=71)) THEN
      --    RETURN(0);
      --  END IF;
      --END IF;
      <<outer_y2>>
      FOR y IN 2..6 LOOP
        FOR x IN 1..8 LOOP
          n:=10*y+x;
          IF (stilling(stOff+ n)=bP) OR (y=6) AND (stilling(stOff+ n+10)=bP) THEN
            IF NOT Wpawns(n) THEN
              IF (FLOOR(kposs/10)-1>y) OR (ABS(MOD(kposs,10)-x)>y) THEN
                Slut:=TRUE; -- inside quadrant
              ELSIF NOT knEndGameW THEN
                IF (n<40) THEN
                  IF (Kpos=n-9) OR (Kpos=n-11) THEN
                    Slut:=TRUE; -- Supported pawn
                  ELSIF n<30 THEN
                    IF (Kpos=n-1) OR (Kpos=n+1)
                    OR (Kpos>=n+12) AND (Kpos<=n+8) AND (kposs<>n-10) THEN
                      Slut:=TRUE; -- Supported pawn
                    END IF;
                  ELSE
                    IF (Kpos=n-1) AND (MOD(kposs,10)>x)
                    OR (Kpos=n+1) AND (MOD(kposs, 10)<x) THEN
                      Slut:=TRUE; -- Supported pawn in one move
                      posi:=posi+SupportedPawnPenalty;
                    END IF;
                  END IF;
                ELSIF (n<50) THEN
                  -- IF Possibly supported pawn then add value:
                  IF (Kpos=n-9)  OR (Kpos=n-11) THEN posi:=posi- SupportedPawn0911; END IF;
                  IF (Kpos=n-10) OR (Kpos=n-20) THEN posi:=posi- SupportedPawn1020; END IF;
                  IF (Kpos=n-19) OR (Kpos=n-21) THEN posi:=posi- SupportedPawn1921; END IF;
                END IF;
              END IF;
              IF Slut THEN
                posi:=posi-(9-y)*(9-y)*priB; 
                exit outer_y2;-- x:=8; y:=6; -- pawn can promote, exit loop
--(*$ IF Test0  --WRITELNF(s(' fpB felt=')+l(n)+s(' posi=')+l(posi)); --(*$ ENDIF
              END IF;
            END IF;
          END IF;
        END LOOP;
      END LOOP outer_y2;
    END IF;
          
  END IF;

  IF TRUE THEN-- EndGame OR (matr>alpha-200) AND (matr<beta+200) THEN
    -- penalty for bad development
    m:=-1;
    IF stilling(stOff+ 12)=wN THEN m := m + 1; END IF;
    IF stilling(stOff+ 13)=wB THEN m := m + 1; END IF;
    IF stilling(stOff+ 15)=wM THEN
      m := m + 1;
      IF stilling(stOff+ 16)=wB THEN wbonus:=wbonus-BadDevelBishop; END IF;
    END IF;
    IF stilling(stOff+ 16)=wB THEN m := m + 1; END IF;
    IF stilling(stOff+ 17)=wN THEN m := m + 1; END IF;
    IF stilling(stOff+ 24)=wP THEN m := m + 1; END IF;
    IF stilling(stOff+ 25)=wP THEN m := m + 1; END IF;
    IF m>0 THEN wbonus:=wbonus-m*BadDevelPenalty; END IF;
    m:=-1;
    IF stilling(stOff+ 82)=bN THEN m := m + 1; END IF;
    IF stilling(stOff+ 83)=bB THEN m := m + 1; END IF;
    IF stilling(stOff+ 85)=bM THEN
      m := m + 1;
      IF stilling(stOff+ 86)=bB THEN bbonus:=bbonus-BadDevelBishop; END IF;
    END IF;
    IF stilling(stOff+ 86)=bB THEN m := m + 1; END IF;
    IF stilling(stOff+ 87)=bN THEN m := m + 1; END IF;
    IF stilling(stOff+ 74)=bP THEN m := m + 1; END IF;
    IF stilling(stOff+ 75)=bP THEN m := m + 1; END IF;
    IF m>0 THEN bbonus:=bbonus-m*BadDevelPenalty; END IF;

--(*$ IF Test0
--  tst2('Eval:',stilling,bK);
--(*$ ENDIF

    IF Black THEN
--(*$ IF Test --WRITELNF(s(' !!SORT!! ')); --(*$ ENDIF
  
      res:=(-matr-posi-wbonus+bbonus /* +Activity*ActivityWeight */ -inmv);
    ELSE
      res:=( matr+posi+wbonus-bbonus /* +Activity*ActivityWeight */ +inmv);
    END IF; --** posi DIV 2 !!!!!!!!!****)
    
    IF PawnOfficer THEN
      IF endgame and ABS(res) > 180 THEN -- in front, so bonus for pawns and penalty for Officers
        m := 0; 
        IF ABS(res) > 540 THEN
          n := 4;
        ELSIF ABS(res) > 360 THEN
          n := 3;
        ELSE
          n := 2;
        END IF;
        FOR y IN 1..8 LOOP
          FOR x IN 1..8 LOOP
            brik:=UPPER_n(stilling(stOff+ 10*y+x));
            CASE brik
              WHEN bQ THEN m := m - 17*n;
              WHEN bR THEN m := m - 9*n;
              WHEN bC THEN m := m - 9*n;
              WHEN bN THEN m := m - 6*n;
              WHEN bB THEN m := m - 6*n;
              WHEN bP THEN m := m + 4*n;
              WHEN bE THEN m := m + 4*n;
              ELSE NULL;
            END CASE;        
          END LOOP;
        END LOOP;
        --m:=m*5;
        IF res < 0 THEN
          res := res - m;
        ELSE
          res := res + m;
        END IF;   
        --IF TRAPon THEN WRT('In front Officers penalty+pawns bonus (keep pawns and exchange officers)='||m); END IF;
      END IF;
    END IF;

  ELSE
    IF Black THEN RETURN(-matr); ELSE RETURN(matr); END IF;
  END IF;
  --
/*
DECLARE
  english BOOLEAN := FALSE;
  ch CHAR(1);
BEGIN
    WRT('.  _______________________________');
    FOR yy IN 1..8 LOOP
      DebugS :=' '||to_char(9-yy)||' | ';
      FOR xx IN 1..8 LOOP
        DebugS := DebugS||stilling(stOff+ xx+10*(9-yy))||' | ';
      END LOOP;
      IF english THEN --english version will not show castling-rights and en-passant.
        DebugS:= TRANSLATE(DebugS,'BbEeLlSsDdTt',
                                  'PpPpBbNnQqRr');
      END IF;
      WRT(DebugS);
      WRT('.  -------------------------------');
    END LOOP;
    ch := stilling(stOff+ PL_PIG_CHESS_ENGINE.HvisTur );
    WRT('.   A   B   C   D   E   F   G   H   '''||CASE WHEN english THEN CASE when ch=bN THEN bP ELSE 'W' END ELSE ch END||'''');
END;
WRT('res='||res||' matr='||matr||' wbonus='||wbonus||' bbonus='||bbonus||' posi='||posi||'  '||stilling(stOff+ hvistur)); --matr=-1559 wbonus=-3 bbonus=24 posi=-33
*/
  -- 
  RETURN(res);
END Eval;



PROCEDURE Initialize IS
 n SIMPLE_INTEGER := 0;
BEGIN
  IF not FirstW THEN
    ToFile:=TRUE;
    FirstW:=TRUE;
    --LogVersion("SkakBrainEval.def",SkakBrainEvalDefCompilation);
    --LogVersion("SkakBrainEval.mod",SkakBrainEvalModCompilation);
    -- dbms_output.put_line('Initialize A');
     pdw.extend(pdSz);
    -- dbms_output.put_line('Initialize B');
    FOR n in 1..pdSz LOOP
      pdw(n) := 0;
    END LOOP;
    --dbms_output.put_line('Initialize C');
    pd  := pdw;
    pdb := pdw;
    --WRT('Initialize EVAL');
  ELSE
    NULL;
    --WRT('Initialize EVAL SKIP');
  END IF;
END Initialize;

END;
/
