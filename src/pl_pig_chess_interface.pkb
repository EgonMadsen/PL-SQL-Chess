DROP PACKAGE BODY PL_PIG_CHESS_INTERFACE;

CREATE OR REPLACE PACKAGE BODY PL_PIG_CHESS_INTERFACE AS
--
-- PL_PIG_CHESS_INTERFACE v 0.92
--
-- Interface for play with PL_PIG_CHESS_ENGINE using DBMS_OUTPUT
--
position          PL_PIG_CHESS_ENGINE_EVAL.STILLINGTYPE := PL_PIG_CHESS_ENGINE_EVAL.STILLINGTYPE();
InitializedStatic BOOLEAN := FALSE; -- any call will make the static initializations and set this flag.
WhiteSide         INTEGER;          -- 0=human, 2=low,  4=medium, 6=high (engine strength/timeusage, 8 and 10 is possible, but not recommended)
BlackSide         INTEGER;          -- 0=human, 2=low,  4=medium, 6=high (engine strength/timeusage, 8 and 10 is possible, but not recommended)
BlackToMove       BOOLEAN;          -- FALSE = WhiteToMove
InteractionMode   INTEGER:=1;       -- 0+1 =dbms_output, 2=table, 3=tables(asyncronous)
TheoryMode        INTEGER;          -- 0= no theory, 1=internal theory (random play 2 ply, 30 lines), 2= Mini-book  178 x (2->8)
EvTot             INTEGER:=0;       -- Total count of evals
Board             BOOLEAN := TRUE;  -- Show Board in output
trk               PL_PIG_CHESS_ENGINE.TRKDATA;   -- last move
MOVETEXT          VARCHAR2(20);     -- last move in full-text

PROCEDURE WR(s VARCHAR2) IS
BEGIN
  IF InteractionMode in (0,1) THEN
    dbms_output.put_line(s);
  ELSIF   InteractionMode=2 THEN
    NULL;
  ELSIF  InteractionMode=3 THEN
    NULL;
  END IF;
END WR;

FUNCTION SET_IN(members BINARY_INTEGER, setM BINARY_INTEGER) return BOOLEAN IS
BEGIN -- IN: member of set (M n N)
RETURN UTL_RAW.CAST_TO_BINARY_INTEGER(UTL_RAW.bit_and(UTL_RAW.CAST_FROM_BINARY_INTEGER(members),UTL_RAW.CAST_FROM_BINARY_INTEGER(setM)))=members;
END;

FUNCTION MOVETXT( trk PL_PIG_CHESS_ENGINE.TRKDATA, piece VARCHAR2 DEFAULT NULL, english BOOLEAN DEFAULT TRUE) RETURN varchar2 IS
-- converts internal format for a move to a text-format
  res VARCHAR2(20);
  x SIMPLE_INTEGER:=0;
  y SIMPLE_INTEGER:=0;
BEGIN
  IF english THEN
    CASE upper(piece)
      WHEN 'T' THEN res := 'R';
      WHEN 'S' THEN res := 'N';
      WHEN 'L' THEN res := 'B';
      WHEN 'B' THEN res := '';--'P' but don't show
      WHEN 'E' THEN res := '';--'P' but don't show
      WHEN 'D' THEN res := 'Q';
      WHEN 'M' THEN res := 'K';
      ELSE          res := upper(piece);
    END CASE;
  ELSE
    IF    upper(piece) = 'M' then res := 'K';
    ELSIF upper(piece) = 'B' then res := '';--'B' but don't show
    ELSIF upper(piece) = 'E' then res := '';--'B' but don't show
    ELSE                          res := upper(piece);
    END IF;
  END IF;
  x := MOD(Trk.Fra,10);
  y := FLOOR(Trk.Fra/10);
  res := res||chr(x+96)||y;
  IF SET_IN(PL_PIG_CHESS_ENGINE.MOVEslag,Trk.Typ) THEN
    res := res||'x';
  ELSE
    res := res||'-';
  END IF;
  x := MOD(Trk.Til,10);
  y := FLOOR(Trk.Til/10);
  res := res||chr(x+96)||y;
  IF SET_IN(PL_PIG_CHESS_ENGINE.MOVErokade,Trk.Typ) THEN  
    IF x=7 THEN res :='o-o'; else res := 'o-o-o'; END IF;
  END IF;
  IF SET_IN(PL_PIG_CHESS_ENGINE.MOVEmat,Trk.Typ) THEN
    res := res||'++';  
  ELSIF SET_IN(PL_PIG_CHESS_ENGINE.MOVEpat,Trk.Typ) THEN  
    IF english THEN
      res := res||'(pat)';  
    ELSE
      res := res||'(stalemate)';  
    END IF;
  ELSIF SET_IN(PL_PIG_CHESS_ENGINE.MOVEskak,Trk.Typ) THEN  
    res := res||'+';  
  ELSIF SET_IN(PL_PIG_CHESS_ENGINE.MOVEpromotion,Trk.Typ) THEN  
    IF english THEN
      res := res||'(d)';   
    ELSE
      res := res||'(q)';   
    END IF;
  END IF;
  return res;
END MOVETXT;  

FUNCTION SHORT(mvtxt VARCHAR2, Multiple BOOLEAN DEFAULT FALSE) RETURN VARCHAR2 IS
--Makes a short-version of a movetext so it can be compared to test-suites.
 hj_mvtxt VARCHAR2(20);
BEGIN
   hj_mvtxt := replace(replace(replace(mvtxt,'+'),'(d)'),'(q)');
   IF length(hj_mvtxt)=6 THEN 
     IF INSTR(hj_mvtxt,'x')=4 THEN
       hj_mvtxt := substr(hj_mvtxt,1,1)||substr(hj_mvtxt,4);
     ELSE
       hj_mvtxt := substr(hj_mvtxt,1,1)||substr(hj_mvtxt,5);
     END IF;
   ELSIF length(hj_mvtxt)=5 AND INSTR(hj_mvtxt,'-')=3 THEN 
     hj_mvtxt := substr(hj_mvtxt,4);
   ELSIF length(hj_mvtxt)=5 AND INSTR(hj_mvtxt,'x')=3 THEN 
     hj_mvtxt := substr(hj_mvtxt,1,1)||substr(hj_mvtxt,3);
   END IF;
   RETURN hj_mvtxt;
END SHORT;

PROCEDURE OUTPUT_POSITION(english BOOLEAN DEFAULT TRUE) IS
  s VARCHAR2(80);
  ch CHAR(1);
BEGIN
  IF BOARD THEN
    WR('.  _______________________________');
    FOR yy IN 1..8 LOOP
      s :=' '||to_char(9-yy)||' | ';
      FOR xx IN 1..8 LOOP
        s := s||chr(position(PL_PIG_CHESS_ENGINE.stOff+ xx+10*(9-yy)))||' | ';
      END LOOP;
      IF english THEN --english version will not show castling-rights and en-passant.
        s := TRANSLATE(s,'BbEeLlSsDdTt',
                         'PpPpBbNnQqRr');
      END IF;
      WR(s);
      WR('.  -------------------------------');
    END LOOP;
    ch := chr(position(PL_PIG_CHESS_ENGINE.stOff+ PL_PIG_CHESS_ENGINE_EVAL.HvisTur ));
    WR('.   A   B   C   D   E   F   G   H   '''||CASE WHEN english THEN CASE when ch='S' THEN 'B' ELSE 'W' END ELSE ch END||'''');
  END IF;
END OUTPUT_POSITION;


FUNCTION EPD_STR(operationlist  VARCHAR2 DEFAULT NULL) RETURN varchar2 IS
-- returns the current position in EPD (or FEN) format:
-- EPD_STR('co "comment";')
-- or in FEN format:
-- EPD_STR('0 1')
  hj_list VARCHAR2(200):=operationlist;
BEGIN 
  IF hj_list IS NULL THEN
    hj_list := ' bm '||SHORT(MOVETEXT)||'; id "'||Trk.vlu||'"; c0 "'||PL_PIG_CHESS_ENGINE_EVAL.Evals||'";'; 
  END IF;
  RETURN PL_PIG_CHESS_ENGINE.STILLING_TO_EPD(position, hj_list);
END; 

FUNCTION INV_EPD(epdfenstr VARCHAR2) RETURN VARCHAR2 IS
-- returns a  EPD (or FEN) position reversed
rev varchar2(256);
reo varchar2(256);
rmv varchar2(256):='';
rtx varchar2(256):='';
P integer;
G integer;
BEGIN
  P := instr(epdfenstr,' ');
  rev := TRANSLATE(substr(epdfenstr,1,p),'012345678RNBQKPrnbqkp/ ',
                                         '012345678rnbqkpRNBQKP//');
  FOR n IN 1..8 LOOP
    G := instr(rev,'/');
    IF n=1 THEN
      reo := substr(rev,1,G-1)||' ';
    ELSE
      reo := substr(rev,1,G)||reo;
    END IF;
    rev := substr(rev,G+1);
  END LOOP;
  --
  IF UPPER(substr(epdfenstr,P+1,1))='B' THEN
    reo:= reo||'w ';
  ELSE
    reo:= reo||'b ';
  END IF;
  G := instr(epdfenstr,'"');
  IF G=0 THEN
    rmv := substr(epdfenstr,P+3);
  ELSE
    rtx := substr(epdfenstr, G-1);
    rmv := substr(epdfenstr,P+3, G-(P+3)-1);
  END IF;
  rmv :=  TRANSLATE(rmv,'12345678',
                        '87654321');
  RETURN reo||rmv||rtx;
END;
--
--
FUNCTION POSITION_STR(english BOOLEAN DEFAULT TRUE) RETURN varchar2 IS 
-- makes the internal position array format to the internal string format for a position
  s VARCHAR2(80):='';--str65
  ch CHAR(1);
BEGIN
  FOR yy IN 1..8 LOOP
    FOR xx IN 1..8 LOOP
      s := s||chr(position(PL_PIG_CHESS_ENGINE.stOff+ xx+10*(9-yy)));
    END LOOP;
  END LOOP;
  ch := chr(position(PL_PIG_CHESS_ENGINE.stOff+ PL_PIG_CHESS_ENGINE.HvisTur ));
  IF english THEN
    s := TRANSLATE(SUBSTR(s,1,64),'BbLlSsDdTtRr',
                                  'PpBbNnQqRrCc')||case when ch='S' THEN 'B' else 'W' end;
    return s;
  ELSE
    return s||ch;
  END IF;
END POSITION_STR;
--
--
PROCEDURE NEW_GAME(
  White INTEGER  DEFAULT 2,                -- 0=human, 2=low,  4=medium, 6=high (engine strength/timeuse)
  Black INTEGER  DEFAULT 0,                -- 0=human, 2=low,  4=medium, 6=high (engine strength/timeuse)
  STARTPOSITION      VARCHAR2 DEFAULT NULL,  -- NULL= chess startposition. A position in FEN, EPD or the internal POSITIONSTR format (english version supported).
  p_TheoryMode       INTEGER  DEFAULT 0,     -- 0= no theory, 1=internal theory (random play 2 ply, 16 lines), 2= internal opening book (random play of about 3000 opening-lines)   
  p_InteractionMode  INTEGER  DEFAULT 1      -- 0=dbms_output (moves only) 1=dbms_output (with positions), 2=table, 3=tables(asyncronous)
  ) IS
BEGIN
  IF White > 10 then
    WhiteSide       := 10;--internal limit
  ELSE
    WhiteSide       := White;
  END IF;
  IF Black > 10 then
    BlackSide       := 10;--internal limit
  ELSE 
    BlackSide       := Black;
  END IF;
  TheoryMode      := p_TheoryMode;
  InteractionMode := p_InteractionMode;
  IF InteractionMode = 1 THEN
    Board := TRUE;
  ELSIF InteractionMode = 0 THEN
    Board := FALSE;
  END IF;
  WR('Game started '||to_char(sysdate,'dd/mm-yyyy hh24:mi:ss'));
  PL_PIG_CHESS_ENGINE.Initialize;
  IF NOT InitializedStatic THEN
    position.extend(121);--(121) OF CHAR(1); /* i PL/SQL er offset 11               -10..HvisTur: A1-H8=11-88, HvidSort='H' | 'S' */
    FOR n IN 1..121 LOOP
      position(n) := ASCII('.');
    END LOOP;
    --
    InitializedStatic := TRUE;
    --WR('Initialize S2');
  ELSE
    NULL;
    --WR('Initialize S2 SKIP');
  END IF;
  PL_PIG_CHESS_ENGINE.STILL(position,STARTPOSITION);
  OUTPUT_POSITION;
  --IF PL_PIG_CHESS_ENGINE.EPD_BESTMOVE      IS NOT NULL THEN WR('EPD best move:       '||PL_PIG_CHESS_ENGINE.EPD_BESTMOVE);      END IF;
  --IF PL_PIG_CHESS_ENGINE.EPD_SECONDARYMOVE IS NOT NULL THEN WR('EPD secondary move:  '||PL_PIG_CHESS_ENGINE.EPD_SECONDARYMOVE); END IF;
  --IF PL_PIG_CHESS_ENGINE.EPD_AVOIDMOVE     IS NOT NULL THEN WR('EPD avoid move:      '||PL_PIG_CHESS_ENGINE.EPD_AVOIDMOVE);     END IF;
  --IF PL_PIG_CHESS_ENGINE.EPD_id            IS NOT NULL THEN WR('EPD id:              '||PL_PIG_CHESS_ENGINE.EPD_ID);            END IF;
  --IF PL_PIG_CHESS_ENGINE.EPD_COMMENT0      IS NOT NULL THEN WR('EPD primary comment: '||PL_PIG_CHESS_ENGINE.EPD_COMMENT0);      END IF;
  BlackToMove := position(PL_PIG_CHESS_ENGINE.stOff+ PL_PIG_CHESS_ENGINE_EVAL.Hvistur) = ASCII('S');
  IF NOT BlackToMove AND WhiteSide > 0 OR BlackToMove AND BlackSide > 0 THEN
    DO_BOTMOVE;
  END IF;
END NEW_GAME;

PROCEDURE DO_BOTMOVE(OverruleLevel SIMPLE_INTEGER DEFAULT 0) IS
  LV INTEGER;
BEGIN
    IF NOT InitializedStatic THEN
      NEW_GAME(2,0);
    END IF;
    IF BlackToMove THEN
      LV := FLOOR(BlackSide * 3 / 2) - 2;
    ELSE
      LV := FLOOR(WhiteSide * 3 / 2) - 2;
    END IF;
    IF LV < 1 OR OverruleLevel > 0 THEN
      LV := FLOOR(OverruleLevel * 3 / 2) - 2 ;
      IF LV < 1 THEN 
        LV :=1;
      END IF;
    END IF;
    --WR('lv='||LV||' WhiteSide='||WhiteSide||' BlackSide='||BlackSide);
    PL_PIG_CHESS_ENGINE.FindTrk(position, 
          Lv,--dybde SIMPLE_INTEGER, ---- depth: 0-9 (0,1,4,7,10,13) 
          0,-- ekstra SIMPLE_INTEGER, p.t. UNUSED
          Trk-- in out TRKDATA 
          );
    EvTot := EvTot + PL_PIG_CHESS_ENGINE_EVAL.Evals;
    --WR('træk='||MOVETXT(Trk,chr(position(PL_PIG_CHESS_ENGINE.stOff +Trk.Fra)))||' vlu='||Trk.vlu||' evals='||PL_PIG_CHESS_ENGINE_EVAL.Evals);      
    MOVETEXT := MOVETXT(Trk,chr(position(PL_PIG_CHESS_ENGINE.stOff+ Trk.Fra)));
    WR('lv='||LV||' move='||MOVETEXT||'  '||Trk.Fra ||Trk.Til||' vlu='||Trk.vlu||' typ='||Trk.Typ||' evals='||PL_PIG_CHESS_ENGINE_EVAL.Evals||' tot='||EvTot);      
    IF SET_IN(PL_PIG_CHESS_ENGINE.MOVEmat,Trk.Typ) OR SET_IN(PL_PIG_CHESS_ENGINE.MOVEpat,Trk.Typ) THEN
      WR('Game ended '||to_char(sysdate,'dd/mm-yyyy hh24:mi:ss'));
    ELSE
      IF PL_PIG_CHESS_ENGINE.DoMoveOK(position, Trk.Fra, Trk.Til, Trk.Typ) THEN
        BlackToMove := position(PL_PIG_CHESS_ENGINE.stOff+ PL_PIG_CHESS_ENGINE_EVAL.Hvistur)=ASCII('S');
      ELSE
        WR('Illegal move by engine ('||Trk.Fra||'-'||Trk.Til||')!');
      END IF;  
    END IF;
    --WR(POSITION_STR);
    OUTPUT_POSITION;
END DO_BOTMOVE;

PROCEDURE DO_BOTGAME(maxmoves SIMPLE_INTEGER DEFAULT 200) IS
 -- play a game bot vs bot
BEGIN
  FOR n in 1..maxmoves LOOP
    DO_BOTMOVE;
    IF SET_IN(PL_PIG_CHESS_ENGINE.MOVEmat,Trk.Typ) OR SET_IN(PL_PIG_CHESS_ENGINE.MOVEpat,Trk.Typ) THEN
      exit;
    END IF;
  END LOOP;
END DO_BOTGAME;  


PROCEDURE DO_MOVE(fromto VARCHAR2) IS -- move in the form 'e2e4' or 'g1f3' as black 'e7e5' or 'g8f6'
-- human move
  mvtyp PL_PIG_CHESS_ENGINE.MOVETYPE := 0;
  Ffrom varchar2(5);
  Fto   varchar2(5);
BEGIN
  IF NOT InitializedStatic THEN
    NEW_GAME(0,2);
  END IF;
  Ffrom :=substr(fromto,-4,2);
  Fto   :=substr(fromto,-2);
  IF Ffrom > '9' THEN
    Ffrom := substr(Ffrom,2)||CHR(ASCII(UPPER(substr(Ffrom,1,1)))-16);
  END IF;
  IF Fto > '9' THEN
    Fto := substr(Fto,2)||CHR(ASCII(UPPER(substr(Fto,1,1)))-16);
    WR(Ffrom||Fto);
  END IF;
  IF PL_PIG_CHESS_ENGINE.DoMoveOk(position,TO_NUMBER(Ffrom),TO_NUMBER(Fto), mvtyp) THEN
    OUTPUT_POSITION;
    BlackToMove := position(PL_PIG_CHESS_ENGINE.stOff+ PL_PIG_CHESS_ENGINE_EVAL.Hvistur)=ASCII('S');
    IF NOT BlackToMove AND WhiteSide > 0 OR BlackToMove AND BlackSide > 0 THEN
      DO_BOTMOVE;
    END IF;
  ELSE
    WR('Illegal move ('||fromto||')!');
  END IF;
END DO_MOVE;


PROCEDURE SET_White(White INTEGER  DEFAULT 0) IS -- alter engine/player-selection for White
BEGIN
  WhiteSide := White;
  IF WhiteSide > 0 AND NOT BlackToMove THEN
    DO_BOTMOVE; -- get engine move
  END IF;
END SET_White; 

PROCEDURE SET_Black(Black INTEGER  DEFAULT 0) IS -- alter engine/player-selection for Black
BEGIN
  BlackSide := Black;
  IF BlackSide > 0 AND BlackToMove THEN
    DO_BOTMOVE; -- get engine move
  END IF;
END SET_Black;

PROCEDURE TAKEBACK_MOVE IS--takes back the last move (if engine, it will move again, possibly the same move)
BEGIN--(not yet implemented!)
  NULL;
END TAKEBACK_MOVE;

PROCEDURE TAKEBACK_MOVES IS--takes back the last two moves (Player can retry another move)
BEGIN--(not yet implemented!)
  NULL;
END TAKEBACK_MOVES;

  
PROCEDURE test1 IS
  --st PL_PIG_CHESS_ENGINE_EVAL.STILLINGTYPE := PL_PIG_CHESS_ENGINE_EVAL.STILLINGTYPE();
  --s VARCHAR2(80);
  --n INTEGER;m INTEGER;
  --c_n INTEGER; c CHAR(1);
  --
  pos1 VARCHAR2(89) :=
    'RSLDMLSR'
  ||'BBBBBBBB'
  ||'        '
  ||'        '
  ||'        '
  ||'        '
  ||'bbbbbbbb'
  ||'rsldmlsr'
  ||'H';
  --
  pos2 VARCHAR2(89) :=
    'R LDM SR'
  ||' BB LBB '
  ||'B SB   B'
  ||'    B   '
  ||'  l b   '
  ||'     s  '
  ||'bbbb bbb'
  ||'rsld rk '
  ||'H';
  --
  pos3 VARCHAR2(89) :=
    'RSLDMLSR'
  ||'BBBBBBBB'
  ||'        '
  ||'        '
  ||'        '
  ||'        '
  ||'bbbbbbbb'
  ||'rsldmlsr'
  ||'H';
  --
  pos4 VARCHAR2(89) :='R  DML RBBB  BBB  S BS   l B  l    b      s bd  bbb  bbbr   m  rS';
  res SIMPLE_INTEGER := 0;
  --trk PL_PIG_CHESS_ENGINE.TRKDATA;
  Lv INTEGER:=4;
BEGIN
  WR('START');--TOAD: Options -> Oracle -> General. About halfway down is a box titled "DBMS Output" Choose Courier font
  NEW_GAME(0, 0, pos4);
  OUTPUT_POSITION;
  --res := PL_PIG_CHESS_ENGINE_EVAL.Eval(position, 0, FALSE, -9999, 9999);
  --WR('Eval='||res);
  PL_PIG_CHESS_ENGINE.FindTrk(position, 
          Lv,--dybde SIMPLE_INTEGER, ---- dybde: 0-9 (0,1,4,7) 
          0,-- ekstra SIMPLE_INTEGER, p.t. UNUSED
          Trk-- in out TRKDATA 
          );
  EvTot := EvTot + PL_PIG_CHESS_ENGINE_EVAL.Evals;
  WR('træk='||MOVETXT(Trk,chr(position(PL_PIG_CHESS_ENGINE.stOff+ Trk.Fra)))||'  '||Trk.Fra ||Trk.Til||' vlu='||Trk.vlu||' typ='||Trk.Typ||' evals='||PL_PIG_CHESS_ENGINE_EVAL.Evals||' tot='||EvTot);      
  PL_PIG_CHESS_ENGINE.DoMove(position, Trk.Fra, Trk.Til, Trk.Typ);
  OUTPUT_POSITION;
  FOR n in 1..2 LOOP
    PL_PIG_CHESS_ENGINE.FindTrk(position, 
          Lv,--dybde SIMPLE_INTEGER, ---- dybde: 0-9 (0,1,4,7) 
          0,-- ekstra SIMPLE_INTEGER, p.t. UNUSED
          Trk-- in out TRKDATA 
          );
    EvTot := EvTot + PL_PIG_CHESS_ENGINE_EVAL.Evals;
    WR('træk='||MOVETXT(Trk,chr(position(PL_PIG_CHESS_ENGINE.stOff+ Trk.Fra)))||'  '||Trk.Fra ||Trk.Til||' vlu='||Trk.vlu||' typ='||Trk.Typ||' evals='||PL_PIG_CHESS_ENGINE_EVAL.Evals||' tot='||EvTot);      
    PL_PIG_CHESS_ENGINE.DoMove(position, Trk.Fra, Trk.Til, Trk.Typ);
    WR(POSITION_STR(FALSE));
  END LOOP;
  OUTPUT_POSITION;
  WR('FindCnt='||PL_PIG_CHESS_ENGINE.FindCnt||' QFindCnt='||PL_PIG_CHESS_ENGINE.QFindCnt);

END test1;

PROCEDURE test2 IS
  s VARCHAR2(80);
  --n INTEGER;m INTEGER;
  c_n INTEGER; c CHAR(1);
BEGIN
  WR('Preproc test');
  NEW_GAME(0);
  PL_PIG_CHESS_ENGINE_EVAL.PreProcessor(position);
  FOR c_n IN ASCII('B')..ASCII('t') LOOP
    c := chr(c_n);
    IF c IN ('B','b','l','L','K','k','S','s','T','t') THEN
      dbms_output.put_line('pdw for '||c);
      FOR yy in 1..8 LOOP  
        s:='. ';
        FOR xx in 1..8 LOOP 
            s := s||to_char(PL_PIG_CHESS_ENGINE_EVAL.pdw(PL_PIG_CHESS_ENGINE_EVAL.pdX(c,xx+10*(9-yy))),'099')||' ';  
        END LOOP; 
        WR(s);
      END LOOP;
    END IF;
  END LOOP;
END test2;

PROCEDURE WRMOVES IS
BEGIN
  WR('Engine='||SHORT(MOVETEXT)||' bm='||SHORT(PL_PIG_CHESS_ENGINE.EPD_BESTMOVE)
     ||CASE WHEN PL_PIG_CHESS_ENGINE.EPD_SECONDARYMOVE IS NOT NULL THEN ' sm='||SHORT(PL_PIG_CHESS_ENGINE.EPD_SECONDARYMOVE) END
     ||CASE WHEN PL_PIG_CHESS_ENGINE.EPD_AVOIDMOVE IS NOT NULL THEN ' am='||SHORT(PL_PIG_CHESS_ENGINE.EPD_AVOIDMOVE) END);
END WRMOVES;

PROCEDURE CLCPOINTS(p_points  in out INTEGER) IS      
-- calculate points by matching engine move vs test-suite best/worst move
 bm1 varchar2(80):=PL_PIG_CHESS_ENGINE.EPD_BESTMOVE;
 sm1 varchar2(80):=PL_PIG_CHESS_ENGINE.EPD_SECONDARYMOVE;
 am1 varchar2(80):=PL_PIG_CHESS_ENGINE.EPD_AVOIDMOVE;
 bm2 varchar2(80):='';
 sm2 varchar2(80):='';
 am2 varchar2(80):='';
 bm3 varchar2(80):='';
 bm4 varchar2(80):='';
 bmR varchar2(80):='';
 p INTEGER;
BEGIN
  p:=instr(bm1,' ');
  IF p > 1 THEN -- use first 4 moves only
    bm2 := substr(bm1,p+1);
    bm1 := substr(bm1,1,p-1);
    p:=instr(bm2,' ');
    IF p > 1 THEN 
      bm3 := substr(bm2,p+1);
      bm2 := substr(bm2,1,p-1);
      p:=instr(bm3,' ');
      IF p > 1 THEN 
        bm4 := substr(bm3,p+1);
        bm3 := substr(bm3,1,p-1);
        p:=instr(bm4,' ');
        IF p > 1 THEN 
          bmR := substr(bm4,p+1);
          bm4 := substr(bm4,1,p-1);
        END IF;  
      END IF;
    END IF;
  END IF;
  p:=instr(sm1,' ');
  IF p > 1 THEN -- use first 2 moves only
    sm2 := substr(sm1,p+1);
    sm1 := substr(sm1,1,p-1);
  END IF;
  p:=instr(am1,' ');
  IF p > 1 THEN -- use first 2 moves only
    am2 := substr(am1,p+1);
    am1 := substr(am1,1,p-1);
  END IF;
  IF SHORT(MOVETEXT)=SHORT(bm1) 
  OR SHORT(MOVETEXT)=SHORT(bm2) 
  OR SHORT(MOVETEXT)=SHORT(bm3) 
  OR SHORT(MOVETEXT)=SHORT(bm4) 
  OR SHORT(MOVETEXT)=SHORT(sm1) 
  OR SHORT(MOVETEXT)=SHORT(sm2) THEN 
    p_points := p_points + 1;
    WR('*** OK ***');
  ELSIF 
    SHORT(MOVETEXT) = SHORT(am1) OR SHORT(MOVETEXT) = SHORT(am2) THEN
    p_points := p_points - 1;
    WR('*** MISTAKE ***');
  END IF;
END CLCPOINTS;
    
PROCEDURE test_BKtest(lvl NUMBER DEFAULT 2, poslow INTEGER DEFAULT 1 , poshigh INTEGER DEFAULT 24) IS
  positions  INTEGER:=0;
  points INTEGER:=0;
  bkRating INTEGER;
BEGIN
  EvTot:=0;
  WR('Level: '||lvl||' The B-K test (henceforth BKT) Bratko-Kopec 24 Testpositions');
  FOR tstpos IN poslow..poshigh LOOP
    positions := positions + 1;
    NEW_GAME(lvl,lvl,PL_PIG_CHESS_DATA.bkTest(tstpos),0,0);
    WRMOVES;
    CLCPOINTS(points);        
  END LOOP;
  bkRating := 1100 + points*100  - CASE WHEN points < 1 THEN 100*(1-points) ELSE 0 END - CASE WHEN points > 9 THEN  50*(points-9) ELSE 0 END;
  WR(points||' points out of '||positions||' positions. BKT rating='||bkRating);
END;

PROCEDURE test_MSquickTest(lvl NUMBER DEFAULT 2, poslow INTEGER DEFAULT 1 , poshigh INTEGER DEFAULT 24) IS
  positions  INTEGER:=0;
  points INTEGER:=0;
BEGIN
  EvTot:=0;
  WR('Level: '||lvl||' The Quicktest by Michael Scheidl. 24 Testpositions');
  FOR tstpos IN poslow..poshigh LOOP
    positions := positions + 1;
    NEW_GAME(lvl,lvl,PL_PIG_CHESS_DATA.MSquickTest(tstpos),0,0);
    WRMOVES;
    CLCPOINTS(points);        
  END LOOP;
  WR(points||' points out of '||positions||' positions.');
END;

PROCEDURE test_THmyPosTest(lvl NUMBER DEFAULT 2, poslow INTEGER DEFAULT 1 , poshigh INTEGER DEFAULT 16) IS
  positions  INTEGER:=0;
  points INTEGER:=0;
BEGIN
  EvTot:=0;
  WR('Level: '||lvl||' MY POSITIONAL TEST SUITE by Tony Hedlund. 16 Testpositions');
  FOR tstpos IN poslow..poshigh LOOP
    positions := positions + 1;
    NEW_GAME(lvl,lvl,PL_PIG_CHESS_DATA.THmyPosTest(tstpos),0,0);
    WRMOVES;
    CLCPOINTS(points);        
  END LOOP;
  WR(points||' points out of '||positions||' positions.');
END;

PROCEDURE test_SLendgameTest(lvl NUMBER DEFAULT 2, poslow INTEGER DEFAULT 1 , poshigh INTEGER DEFAULT 20) IS
  positions  INTEGER:=0;
  points INTEGER:=0;
BEGIN
  EvTot:=0;
  WR('Level: '||lvl||' Endgame testsuite Sune Larsson 2006 / John Nunn. 20 Testpositions');
  FOR tstpos IN poslow..poshigh LOOP
    positions := positions + 1;
    NEW_GAME(lvl,lvl,PL_PIG_CHESS_DATA.SLendgameTest(tstpos),0,0);
    WRMOVES;
    CLCPOINTS(points);        
  END LOOP;
  WR(points||' points out of '||positions||' positions.');
END;

PROCEDURE test_CCRTest(lvl NUMBER DEFAULT 2, poslow INTEGER DEFAULT 1 , poshigh INTEGER DEFAULT 25) IS
  positions  INTEGER:=0;
  points INTEGER:=0;
BEGIN
  EvTot:=0;
  WR('Level: '||lvl||' One Hour Test by Larry Kaufman, published 1994 (Kaufman Test). 25 Testpositions');
  FOR tstpos IN poslow..poshigh LOOP
    positions := positions + 1;
    NEW_GAME(lvl,lvl,PL_PIG_CHESS_DATA.CCRTest(tstpos),0,0);
    WRMOVES;
    CLCPOINTS(points);        
  END LOOP;
  WR(points||' points out of '||positions||' positions.');
END;

PROCEDURE test_ColditzTest(lvl NUMBER DEFAULT 2, poslow INTEGER DEFAULT 1 , poshigh INTEGER DEFAULT 30) IS
  positions  INTEGER:=0;
  points_old INTEGER;
  points INTEGER:=0;
  Score INTEGER:=0;
  cRating INTEGER;
BEGIN
  EvTot:=0;
  --https://www.schach-computer.info/wiki/index.php/Colditz_En
  WR('Level: '||lvl||' Colditz test suite by Ferdinand Mosca, CCC, December 30, 2016. 30 Testpositions');
  FOR tstpos IN poslow..poshigh LOOP
    positions := positions + 1;
    NEW_GAME(lvl,lvl,PL_PIG_CHESS_DATA.ColditzTest(tstpos),0,0);
    WRMOVES;
    points_old :=points;
    CLCPOINTS(points); 
    IF points > points_old THEN 
      IF TRANSLATE(PL_PIG_CHESS_ENGINE.EPD_COMMENT0,'0123456789#',
                                       '########## ') IN ('#','##','###') THEN
        Score := Score + TO_NUMBER(PL_PIG_CHESS_ENGINE.EPD_COMMENT0);
      END IF;  
    END IF;
  END LOOP;
  -- Calculate Colditz-rating (approximated):
  CASE WHEN Score <=1515 THEN
           cRating := 1700-round((1515-score)*0.55);
       WHEN  Score >=1788 THEN
           cRating := 2000+round((score-1788)*2,093);
       ELSE 
           cRating := 1700+round((score-1515)*1.1);
  END CASE;
  WR(points||' points out of '||positions||' positions. Colditz-score='||Score||' Colditz ELO rating='||cRating);
END;

PROCEDURE test_BBCTest(lvl NUMBER DEFAULT 2, poslow INTEGER DEFAULT 1 , poshigh INTEGER DEFAULT 42) IS
  positions  INTEGER:=0;
  points INTEGER:=0;
BEGIN
  EvTot:=0;
  WR('Level: '||lvl||' Big Book Of Combinations. 42 Testpositions');
  FOR tstpos IN poslow..poshigh LOOP
    positions := positions + 1;
    NEW_GAME(lvl,lvl,PL_PIG_CHESS_DATA.BBCTest(tstpos),0,0);
    WRMOVES;
    CLCPOINTS(points);        
  END LOOP;
  WR(points||' points out of '||positions||' positions.');
END;
 
PROCEDURE test_ReinfeldTest(lvl NUMBER DEFAULT 2, poslow INTEGER DEFAULT 1 , poshigh INTEGER DEFAULT 300) IS
  positions  INTEGER:=0;
  points INTEGER:=0;
BEGIN
  EvTot:=0;
  WR('Level: '||lvl||' Reinfeld''s (1945) 300 (tactical) positions');
  FOR tstpos IN poslow..poshigh LOOP
    positions := positions + 1;
    NEW_GAME(lvl,lvl,PL_PIG_CHESS_DATA.ReinfeldTest(tstpos),0,0);
    WRMOVES;
    CLCPOINTS(points);        
  END LOOP;
  WR(points||' points out of '||positions||' positions.');
END;

PROCEDURE test_LCTIITest(lvl NUMBER DEFAULT 2, poslow INTEGER DEFAULT 1 , poshigh INTEGER DEFAULT 35) IS
  positions  INTEGER:=0;
  points INTEGER:=0;
BEGIN
  EvTot:=0;
  WR('Level: '||lvl||' LCT II (Louguet Chess Test II by Frdric Louguet in 1994). 35 Testpositions (1-14=positional, 15-26=tactical, 27-35=endgame');
  --points= 30 if found on level 4, 20 on level 6, 10 on level 8
  --rating= 1900 + points
  FOR tstpos IN poslow..poshigh LOOP
    positions := positions + 1;
    NEW_GAME(lvl,lvl,PL_PIG_CHESS_DATA.LCTIITest(tstpos),0,0);
    WRMOVES;
    CLCPOINTS(points);        
  END LOOP;
  WR(points||' points out of '||positions||' positions.');
END;

PROCEDURE test_SBDTest(lvl NUMBER DEFAULT 2, poslow INTEGER DEFAULT 1 , poshigh INTEGER DEFAULT 134) IS
  positions  INTEGER:=0;
  points INTEGER:=0;
BEGIN
  EvTot:=0;
  WR('Level: '||lvl||' Silent but Deadly (sbd). 134 Testpositions');
  FOR tstpos IN poslow..poshigh LOOP
    positions := positions + 1;
    NEW_GAME(lvl,lvl,PL_PIG_CHESS_DATA.SBDTest(tstpos),0,0);
    WRMOVES;
    CLCPOINTS(points);        
  END LOOP;
  WR(points||' points out of '||positions||' positions.');
END;

PROCEDURE test_STSTest(suite NUMBER, lvl NUMBER DEFAULT 2, poslow INTEGER DEFAULT 1 , poshigh INTEGER DEFAULT 100) IS
  positions  INTEGER:=0;
  points INTEGER:=0;
BEGIN
  EvTot:=0;
  WR('Level: '||lvl||' Strategic Test Suite, (STS) 15 suites x 100 positions. Suite '||suite||':');
  WR(PL_PIG_CHESS_DATA.STSsuitesTest(suite));
  FOR tstpos IN poslow..poshigh LOOP
    positions := positions + 1;
    NEW_GAME(lvl,lvl,PL_PIG_CHESS_DATA.STSTest((suite-1)*100+tstpos),0,0);
    WRMOVES;
    CLCPOINTS(points);        
  END LOOP;
  WR(points||' points out of '||positions||' positions.');
END;

PROCEDURE test_PIG(lvl NUMBER DEFAULT 2, poslow INTEGER DEFAULT 1 , poshigh INTEGER DEFAULT 4) IS
  positions  INTEGER:=0;
  points INTEGER:=0;
BEGIN
  EvTot:=0;
  WR('Level: '||lvl||' Pig-chess found errors. 4 Testpositions');
  FOR tstpos IN poslow..poshigh LOOP
    positions := positions + 1;
    NEW_GAME(lvl,lvl,PL_PIG_CHESS_DATA.PigTest(tstpos),0,1);
    WRMOVES;
    CLCPOINTS(points);        
  END LOOP;
  WR(points||' points out of '||positions||' positions.');
END;

END;
/
