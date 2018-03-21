C Semper 6 processing module EDITOR
C
      SUBROUTINE EDITOR
C
C Provides a text editor for macros
C
C EDITOR interfaces the editor with Semper, loading the macro
C and restoring it finally to disc (unless TRUE returned), and
C passing the text across to EDIT2 with the char count separated
C
      INTEGER IVALPN
      LOGICAL SEMROW,SEMOPN,EDIT2,SEMCON,OPTNO,TEXTU2
C
      INCLUDE 'COMMON'
C
      INTEGER CLASS,FORM,IFROM,ITO,LMACRO,NLAY,NROW
      LOGICAL LVERIF
C
C Packed names
C
      INTEGER NFROM,NTO,NLIST,NVERIF
      PARAMETER (NFROM=10335,NTO=-601,NLIST=19579,NVERIF=-3419)
C
      INTEGER MACRO(LNBUF/LNINT),IB5(LNBUF/LNINT),IB6(LNBUF/LNINT)
      EQUIVALENCE (RB4,MACRO),(RB5,IB5),(RB6,IB6)
C
C See if verification is required (default = yes)
C
      LVERIF=.NOT.OPTNO(NVERIF)
C
      IFROM=IVALPN(NFROM)
      ITO=IVALPN(NTO)
C
C Unless LIST, insist that at least one of FROM and TO are set
C
      IF (VERB.NE.NLIST) THEN
         IF (IFROM.EQ.0) THEN
            IF (ITO.EQ.0) THEN
               IDERR=NTO
               ERROR=3
               GOTO 20
            ENDIF
C
C No source: set zero text length and enter editor
C
            LMACRO=0
            GOTO 10
         ELSE
            IF (ITO.EQ.0) ITO=IFROM
         ENDIF
      ENDIF
C
C Load and read macro to line buffer
C
      IF (SEMOPN(1,IFROM,LMACRO,NROW,NLAY,CLASS,FORM,LP1)) GOTO 20
C
      IF (CLASS.NE.NCLMAC) THEN
         ERROR=6
         IDERR=IFROM
         GOTO 20
      ENDIF
C
      IF (SEMROW(1,MACRO,NFMINT,1,1,LP1)) GOTO 20
C
C If LIST, verify and quit
C
      IF (VERB.EQ.NLIST) THEN
         IF (TEXTU2(MACRO,LMACRO)) GOTO 20
         GOTO 20
      ENDIF
C
C Otherwise, call the editor itself
C
   10 IF (EDIT2(MACRO,LMACRO,IB5,IB5(81),IB5(161),IB6(81),IB6(161)))
     +   GOTO 20
C
C Editing finished - return macro to disc if not empty
C
      IF (LMACRO.LE.0) THEN
         IF (LVERIF) THEN
            IF (LMACRO.EQ.0) THEN
               IF (SEMCON('Macro is empty')) GOTO 20
            ELSE
               IF (SEMCON('Editing abandoned')) GOTO 20
            ENDIF
         ENDIF
      ELSE
         LP2=LP1
         IF (SEMOPN(2,ITO,LMACRO,1,1,2,0,LP2)) GOTO 20
C
         IF (SEMROW(2,MACRO,NFMINT,1,1,LP2)) GOTO 20
C
         IF (LVERIF) THEN
            IF (SEMCON('Editing complete')) GOTO 20
         ENDIF
      ENDIF
C
   20 RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module EDIT2
C
      LOGICAL FUNCTION EDIT2(MACRO,LMACRO,MATCH,REP,SEARCH,S1,S2)
      INTEGER MACRO(*),LMACRO,MATCH(80),REP(80),SEARCH(80),S1(80),S2(80)
C
C Editor main body
C Calls: ECDEC to decode editing directives
C        EDIT4 to move window to next/previous command
C        TEXTU1 to replace substrings of strings
C        TEXTU2 to list contents of macro
C        TEXTU3 to index substrings in strings
C
C All text is held as integer arrays in internal code.  The macro
C text is held in MACRO(1-LMACRO), with the current window extending
C from position WINDOW.
C
C Code for a given editing command is selected by an early computed GOTO
C on OP, which is set by ECDEC; any command arg is in S1/2(1-LS1/2)
C MATCH, REP and SEARCH are back-up copies of arg strings.
C
      LOGICAL ECDEC,TEXTU1,TEXTU2,TEXTU3,SEMCON,SEMINT,SEMTPS
C
      INCLUDE 'COMMON'
C
      INTEGER ABE,BAKPTR,EDIRS(40),EPTR,I,I1,I2,IP,L1,LS1,LS2
      INTEGER ML,N,OP,RL,SL,WINDOW,IWID,ILEN
      LOGICAL FAULT
C
      INTEGER NDIRS
      PARAMETER (NDIRS=20)
C
      DATA EDIRS /
     +   70,83,65,66,67,69,88,71,78,80,68,76,87,81,63,62,35,95,37,36,
     +   -1, 0,-2,-2,-1,-2, 0,-2,12*0/
C
C Initialise editor
      EDIT2=.TRUE.
C
C Fault batch or UIF input
C
      IF (.NOT.SEMINT(.TRUE.)) THEN
         ERROR=77
         IDMESS='Macro editor can only be used interactively'
         GOTO 230
      ENDIF
C
      IF (SEMCON('Editor ready')) GOTO 230
      IF (SEMCON('Type ? if you need help')) GOTO 230
C
      WINDOW=1
C
C Fetch current terminal page size
      IF (SEMTPS(IWID,ILEN)) GOTO 230
C
C Force new line of edits from terminal
   10 EPTR=0
C
C Next edit command - note current WINDOW
   20 BAKPTR=WINDOW
      IF (EPTR.NE.0) GOTO 30
C
C Verify windowed text (start 15 chars before window)
C
      IF (LMACRO.EQ.0) THEN
         IF (SEMCON('Macro is empty')) GOTO 230
C
         N=3
      ELSE
         I1=MAX(WINDOW-15,1)
         I2=MIN(I1+IWID-5,LMACRO)
C
C Prepare ellipses for missing text
C
         N=I2-I1+1
         CALL SEMCHS(RECORD(3:),MACRO(I1),N)
C
         IF (I1.EQ.1) THEN
            RECORD(1:2)='  '
         ELSE
            RECORD(1:2)='..'
         ENDIF
C
         IF (I2.EQ.LMACRO) THEN
            RECORD(N+3:N+4)='  '
         ELSE
            RECORD(N+3:N+4)='..'
         ENDIF
C
         IF (SEMCON(RECORD)) GOTO 230
C
         N=WINDOW-I1+2
      ENDIF
C
C Construct terminal prompt, suitably indented
C
      RECORD=' '
      RECORD(N:N)=':'
C
C Fetch next command (+ args)
C
   30 IF (ECDEC(RECORD(1:N),EDIRS,NDIRS,EPTR,OP,S1,LS1,S2,LS2,FAULT))
     +   GOTO 230
      IF (FAULT) GOTO 10
C
C Jump to editing code
C NB: 2nd label ??260?? is C/string: code to follow later
C
      GOTO (80,110,130,130,120,130,170,180,50,60,70,
     +      40,220,240,210,250,260,270,290,300),
     +      OP
C
C Process L
C
   40 IF (LMACRO.EQ.0) GOTO 350
      IF (SEMCON('Full text:')) GOTO 230
      IF (TEXTU2(MACRO,LMACRO)) GOTO 230
      IF (SEMCON(' ')) GOTO 230
      GOTO 20
C
C Process N
C
   50 IF (WINDOW.GT.LMACRO) GOTO 310
      CALL EDIT4(1,MACRO,LMACRO,WINDOW)
      GOTO 20
C
C Process P
C
   60 IF (WINDOW.EQ.1) GOTO 310
      CALL EDIT4(2,MACRO,LMACRO,WINDOW)
      GOTO 20
C
C Process D
C
   70 CALL EDIT4(1,MACRO,LMACRO,WINDOW)
      IF (TEXTU1(MACRO,LMACRO,LNLINB,BAKPTR,WINDOW-BAKPTR,I,0)) CONTINUE
      IF (LMACRO.EQ.0) GOTO 350
      WINDOW=BAKPTR
      GOTO 20
C
C Process F - retain string for later S commands
C
   80 SL=LS1
      IF (SL.EQ.0) GOTO 20
      DO 90 I=1,SL
         SEARCH(I)=S1(I)
   90 CONTINUE
C
C Search for string
C
  100 IF (.NOT.TEXTU3(MACRO,LMACRO,WINDOW,SEARCH,SL,IP)) GOTO 320
      WINDOW=IP
      GOTO 20
C
C Process S
C
  110 IF (SL.EQ.0) GOTO 340
      OP=1
      GOTO 100
C
C Process C
C
  120 WINDOW=LMACRO+1
      IF (TEXTU1(MACRO,LMACRO,LNLINB,WINDOW,0,S1,LS1)) GOTO 330
      GOTO 20
C
C Process A, B and E
C Retain strings for later X commands
C
  130 ABE=OP
      RL=LS2
      IF (RL.NE.0) THEN
         DO 140 I=1,RL
            REP(I)=S2(I)
  140    CONTINUE
      ENDIF
C
      ML=LS1
      IF (ML.NE.0) THEN
         DO 150 I=1,ML
            MATCH(I)=S1(I)
  150    CONTINUE
      ENDIF
C
C Search for string - within visible window only
C
  160 I1=WINDOW-15
      IF (I1.LT.1) I1=1
      I2=I1+IWID-5
      IF (I2.GT.LMACRO) I2=LMACRO
      IF (.NOT.TEXTU3(MACRO,I2,WINDOW,MATCH,ML,IP)) GOTO 320
      L1=ML
C
C Adjust pointers/lengths to accomodate A/B/E
C
      IF (ABE .LT. 4) IP = IP + L1
      IF (ABE .LE. 4) L1 = 0
C
C Make alteration
C
      IF (TEXTU1(MACRO,LMACRO,LNLINB,IP,L1,REP,RL)) GOTO 330
      GOTO 20
C
C Process X
C
  170 IF (ABE.EQ.0) GOTO 340
      GOTO 160
C
C Process G
C
  180 IF (LS1.EQ.0) GOTO 360
      WINDOW=1
  190 IF (.NOT.TEXTU3(MACRO,LMACRO,WINDOW,S1,LS1,IP)) GOTO 200
      IF (TEXTU1(MACRO,LMACRO,LNLINB,IP,LS1,S2,LS2)) GOTO 330
      WINDOW=IP+LS2
C
C As far as possible, maintain original window
C
      IF (BAKPTR.GT.IP) BAKPTR=BAKPTR+LS2-LS1
      GOTO 190
C
  200 WINDOW=BAKPTR
      IF (WINDOW.GT.LMACRO) WINDOW=LMACRO+1
      GOTO 20
C
C Process ?
C
  210 IF (SEMCON('Editing directives:')) GOTO 230
      IF (SEMCON(' ')) GOTO 230
      IF (SEMCON('N         Move viewing window to Next command'))
     +   GOTO 230
      IF (SEMCON('P         Move window to Previous command')) GOTO 230
      IF (SEMCON('D         Delete to end of command')) GOTO 230
      IF (SEMCON('L         List full macro text')) GOTO 230
      IF (SEMCON('W         Write macro to output and quit')) GOTO 230
      IF (SEMCON('Q         Quit editor (preserving original text)'))
     +   GOTO 230
      IF (SEMCON('?         Help - type this list')) GOTO 230
      IF (SEMCON(' ')) GOTO 230
      IF (SEMCON(' ')) GOTO 230
      IF (SEMCON('F/s/      Move window to string s')) GOTO 230
      IF (SEMCON('S         repeat last Search (F) command')) GOTO 230
      IF (SEMCON('A/s1/s2/  After string s1 insert string s2')) GOTO 230
      IF (SEMCON('B/s1/s2/  Before s1 insert s2')) GOTO 230
      IF (SEMCON('          (at start of window if s1 is null)'))
     +   GOTO 230
      IF (SEMCON('C/s1/     Continue macro by appending string s1'))
     +   GOTO 230
      IF (SEMCON('E/s1/s2/  Extract s1, replacing by s2')) GOTO 230
      IF (SEMCON('X         repeat last eXchange (A/B/E) command'))
     +   GOTO 230
      IF (SEMCON('G/s1/s2/  Perform E Globally (throughout macro)'))
     +   GOTO 230
      IF (SEMCON(' ')) GOTO 230
      IF (SEMCON(
     +   '   A,B,E act within the visible window, right of the prompt;'
     +   )) GOTO 230
      IF (SEMCON(
     +   '   all string matching is irrespective of case, and any char'
     +   )) GOTO 230
      IF (SEMCON('   may be used in place of / as the delimiter'))
     +   GOTO 230
      IF (SEMCON(' ')) GOTO 230
      IF (SEMCON(' ')) GOTO 230
      IF (SEMCON('>         Move window one char forward')) GOTO 230
      IF (SEMCON('#         Delete first char in window')) GOTO 230
      IF (SEMCON('_         Replace char by space')) GOTO 230
      IF (SEMCON('%         Force char to upper case')) GOTO 230
      IF (SEMCON('$         Force char to lower case')) GOTO 230
      IF (SEMCON(' ')) GOTO 230
      IF (SEMCON(
     +   '   A repeat count may precede individual directives, or'
     +   )) GOTO 230
      IF (SEMCON('   bracketed sequences, e.g. 3N 5(F/DEFOCUS/>6$)'))
     +   GOTO 230
      IF (SEMCON(' ')) GOTO 230
      GOTO 10
C
C Process W - normal return
C
  220 EDIT2=.FALSE.
C
  230 RETURN
C
C Process Q - abandon editing
C
  240 LMACRO=-1
      GOTO 220
C
C Process >
C
  250 IF (WINDOW.LE.LMACRO) GOTO 280
      GOTO 310
C
C Process #
C
  260 IF (WINDOW.GT.LMACRO) GOTO 310
      IF (TEXTU1(MACRO,LMACRO,LNLINB,WINDOW,1,I,0)) CONTINUE
      GOTO 20
C
C Process _
C
  270 IF (WINDOW.GT.LMACRO) GOTO 310
      MACRO(WINDOW)=KSPACE
  280 WINDOW=WINDOW+1
      GOTO 20
C
C Process %
C
  290 IF (WINDOW.GT.LMACRO) GOTO 310
      I=MACRO(WINDOW)
      IF (I.GE.95.AND.I.LE.122) MACRO(WINDOW)=I-32
      GOTO 280
C
C Process $
C
  300 IF (WINDOW.GT.LMACRO) GOTO 310
      I=MACRO(WINDOW)
      IF (I.GE.65.AND.I.LE.90) MACRO(WINDOW)=I+32
      GOTO 280
C
C Editing errors
C
  310 IF (SEMCON('Text exhausted')) GOTO 230
C
C Restore backed-up window
C
      GOTO 10
  320 IF (SEMCON('Not found')) GOTO 230
      GOTO 10
C
  330 CONTINUE
  340 IF (SEMCON('No operation to repeat')) GOTO 230
      GOTO 10
C
  350 IF (SEMCON('Macro is empty')) GOTO 230
      WINDOW=1
      GOTO 10
C
  360 IF (SEMCON('Bad match string for G')) GOTO 230
      GOTO 10
C
C Copyright (C) 1987-1990:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module EDIT4
C
      SUBROUTINE EDIT4(IACT,MACRO,LMACRO,WINDOW)
      INTEGER IACT,LMACRO,WINDOW
      INTEGER MACRO(LMACRO)
C
C IACT=1 Moves window forward to next line start
C      2 Moves window back to last line start
C WINDOW is maintained in the range 0 to LMACRO+1
C
C
      INCLUDE 'ICSET'
C
      IF (IACT.NE.2) THEN
C
C IACT=1 Next command
C
   10    IF (WINDOW .GT. LMACRO) GOTO 30
         IF (MACRO(WINDOW) .NE. KSEMIC) THEN
            WINDOW = WINDOW + 1
            GOTO 10
         ENDIF
      ELSE
C
C IACT=2 Previous command
C
         WINDOW = WINDOW - 2
   20    IF (WINDOW .LE. 0) THEN
            WINDOW = 0
         ELSE
            IF (MACRO(WINDOW) .NE. KSEMIC) THEN
               WINDOW = WINDOW - 1
               GOTO 20
            ENDIF
         ENDIF
      ENDIF
C
      WINDOW = WINDOW + 1
   30 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
