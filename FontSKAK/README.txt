
                          Skak: a chess-font		3rd release (2005)
                        -------------------------
Author:
----------

Version history:
 -------------------
SKAK-K6	3rd release,  improvements to frame:
	-  no more holes in the frame.
	-  the bottom-line B on " (alt34) is now on alt0148 and @ (alt64)
              too to get easier access in word when quote conversion is ON.
	-  bottom-line D on ¤ (alt207 or alt0164) is now on $ (alt36) too
              to get continous ascii values and on ascii 160 to follow Adobe
              mapping of the ¤ (currency).
	-  bottom-line G on / (alt47) is on ´ (alt39) too to get continous
              ascii values for the bottom-line (ascii 33-40).
SKAK-K5	2nd release, first time as freeware, tested with Windows and Word.
SKAK-J	1st release, as part of the commercial version of the chess database
           PIGbase4 (for Amiga). NOTE: the freeware version of PIGbase4 can
           be found on the Aminet (a huge PD/shareware library found several
           places at the internet).

What is it:
-------------
A scalable font (Postscript type 1) with Chess-pieces. Can be used with word
processors to create and print-out high quality chess positions (diagrams) in
an easy way. Just choose the Skak font an then type the position.

Required:
------------
PC:- WARNING: Windows 7/10 is known to have problems installing this font!
   - ATM (Adobe Type Manager) installed. (included in most DTP packages).
Macintosh:
   - ATM (Adobe Type Manager) installed. (included in most DTP packages).
   - A converter program to make the Macintosh see the font as a font.
Amiga: 
   - Final Writer or other programs working with Adobe Type 1 fonts.
   - PIGbase4 useful. See Version history.

Automatic generation of positions:
-------------------------------------------
Amiga: The 'Pig-Base 4' chess-database (shareware version at Aminet) can
generate ascii text files of games with automatic generation of diagrams:
    -  Make a comment to the wanted position containing the text-string:
       <dia> to generate the diagram. Print the game to a file.
    -  Import the ascii text into your word-processor wich has to support
       the Adobe Type 1 font format (.PFB) or the TrueType format (.TTF)
       in MAC or PC version.
    -  Mark the block with the position in ascii
    -  Select the Chess font

PC/Macintosh: A small utility to convert from the FEN standard (Forsyth-Edwards
Notation) for position notation and/or PGN format is under consideration, but
until then you have to enter the positions directly in the word-processor.

Keyboard mapping of the pieces:
-----------------------------------------
Uses the Scandinavian/German piece letters (bsltdk) only (marked *). f for an
empty square.
Use capitalized for a black piece. Otherwise it's a white piece.

To get the piece on a black square, use the key above. Exeption: the rook (t)
has no letter on the key above (part of the frame) and uses the key to the
righ (y) instead!

Here at list of the piece-letters for different countries:

	Language and  Piece letters (pawn knight bishop rook queen king)
	--------------------------------------------
	Czech        P J S V D K 	   Hungarian    G H F B V K
	Danish       B S L T D K *          Icelandic    P R B H D K
	Dutch        O P L T D K  	   Italian      P C A T D R
	English      P N B R Q K 	   Norwegian    B S L T D K *
	Estonian     P R O V L K 	   Polish       P S G W H K
	Finnish      P R L T D K 	   Portuguese   P C B T D R
	French       P C F T D R 	   Romanian     P C N T D R
	German       B S L T D K *          Spanish      P C A T D R
	                                    Swedish      B S L T D K *

           Piece        White     Black     White      Black
                          (on white square)  (on black square)
           -------------------------------------------------------
           Pawn	b	B	g	G
           Knigh	s	S	w	W
           Bishop	l	L	o	O
           Rook	t	T	y	Y
           Queen	d	D	e	E
           King	k	K	i	I
           Empty	f		r

Keys to make a frame around the board:
--------------------------------------
To make Left top of a frame use:	           9
To make Top part of a frame use:	           _ (underline)
To make Right top of a frame use:	) (shift 9)
To make Left part of a frame use:	| (bar) or 8-1
To make Right part of a frame use:	\ (backslash)  or  * (asterix)
To make Left bottom of a frame use:	0
To make Bottom part of a frame use:	- (hyphen) or !"#¤%&/(  (shift 1-8)
To make Right bottom of a frame use:	= (shift 0)

Bottom B on " (alt34) is on @ (alt64) and alt0148 too to get easier access in
word when quote conversion is ON.
bottom D on ¤ (alt207 or alt0164) is on $ (alt36) too to get continous ascii
values and on ascii 160 (adobe:currency).
bottom G on / (alt47) is on ´ (alt39) too to get continous ascii values for
bottom (ascii 33-40).

Note that the left part of the frame is on 9-0, the right corners therefore
on shift9+shift0

HINTS:
----------
It's problematic to convert the font to the CG (CompuGraphic) format (Amiga)
and TrueType format (Macintosh and PC). Some of the pieces can be too complex
and may fail.

Sometimes you have to ajust the paragraph line-spacing to get a correct
printout (try 1.16). 

Examples:
-------------
Start position (mark and select skak font, resize using font size):

 TWLEKOSY
 GBGBGBGB
 frfrfrfr
 rfrfrfrf
 frfrfrfr
 rfrfrfrf
 bgbgbgbg
 ysodilwt

9________)
8TWLEKOSY\
7GBGBGBGB\
6frfrfrfr\
5rfrfrfrf\
4frfrfrfr\
3rfrfrfrf\
2bgbgbgbg*
1ysodilwt\
0!@#$%&/(=

Adresses:
------------
Aminet sites index:
http://www.iastate.edu/~rheit/mirrors.html

Two aminet ftp sites:
ftp://ftp.eunet.ch/pub/aminet/game/board/
ftp://ftp.wustl.edu/pub/aminet/game/board/

Chess Desktop Publishing Home Page:
http://www.users.dircon.co.uk/~amscott/home.htm (amscott@dircon.co.uk)

Danish Chess Union (Dansk Skak Union) homepage:
http://www.dsu.dk

Newsgroups:
rec.games.chess.computer        used for notes/discussions about this archive 
rec.games.chess.misc		            -      "      -
alt.binaries.misc               used to upload new versions of this archive

