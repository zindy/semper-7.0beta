C Semper 6 system module SEMMAC
C
      LOGICAL FUNCTION SEMMAC(IOP,NUMBER,PTR,VD,LINE,LINELN,LINEMX)
C
C Processes verb MACRO, and expands '@' constructions in LINE
C
C Format of numbered macros: byte array of internal code chars, stored
C as class 2 1-D picture; any form in fact acceptable here
C
C IOP=1: Expands @number or @name in LINE - PTR indicates @
C IOP=2: Creates MACRO from LINE - PTR indicates semicolon
C
      INTEGER IOP,NUMBER,PTR,VD(*),LINEMX,LINE(*),LINELN
C
      INTEGER SEMPPN
      LOGICAL SEMDCR,SEMOPN,SEMROW,SEMXA1,TEXTU1
C
      INCLUDE 'COMMON'
C
      REAL X
      INTEGER*4 NC4
      INTEGER CLASS,FORM,I,K,MACRO,NCOL,NLAY,NROW,PRPTR,TEXT(256)
C
C Packed name
C
      INTEGER NMACRO
      PARAMETER (NMACRO=20843)
C
      EQUIVALENCE (RB1,TEXT)
C
C Initialise and select code
C
      SEMMAC=.FALSE.
C
C Macro expansion code
C ------------------------
C Dump any current row, to clear SEMBUF
C
      IF (SEMDCR(0)) GOTO 80
      IF (IOP.EQ.2) GOTO 30
C
C Is there a name following AT?
C
      K=PTR+1
      IF (SEMXA1(1,LINE,LINELN,K,X,MACRO)) CONTINUE
      IF (MACRO.EQ.0) GOTO 20
C
C Yes: search named macro list
C
      PRPTR=VD(4)
      IF (PRPTR.EQ.0) GOTO 110
   10 IF (VD(PRPTR).EQ.0) GOTO 110
      IF (MACRO. NE. VD(PRPTR)) THEN
         PRPTR=PRPTR+VD(PRPTR+1)+2
         GOTO 10
      ENDIF
C
C Found: effect textual replacement
C
      NCOL=VD(PRPTR+1)
      K=K-PTR
      IF (LINELN+NCOL-K.GT.LINEMX) GOTO 90
      IF (TEXTU1(LINE,LINELN,LINEMX,PTR,K,VD(PRPTR+2),NCOL)) GOTO 90
      GOTO 60
C
C Not a named macro; try a number
C
   20 IF (SEMXA1(2,LINE,LINELN,K,X,I)) CONTINUE
      MACRO=SEMPPN(NINT(X))
C
C Found: fetch macro text (as integers); forcing VERB overrides
C normal protection of macros by SEMOPN
C
      VERB=NMACRO
      IF (SEMOPN(1,MACRO,NCOL,NROW,NLAY,CLASS,FORM,LP1)) GOTO 80
      IF (CLASS.NE.2) THEN
         ERROR=6
         GOTO 70
      ENDIF
      IF (SEMROW(1,TEXT,NFMINT,1,1,LP1)) GOTO 80
C
C Fetched: effect textual replacement
C
      K=K-PTR
      IF (LINELN+NCOL-K.GT.LINEMX) GOTO 90
      IF (TEXTU1(LINE,LINELN,LINEMX,PTR,K,TEXT,NCOL)) GOTO 90
      GOTO 60
C
C Process verb MACRO
C ---------------
C
   30 DO 40 K = 0,INPLEV
         IF (INPDEV(K) .NE. 0) THEN
            ERROR = 77
            IDMESS = 'MACRO not allowed in program'
            GOTO 80
         ENDIF
   40 CONTINUE
C
C Strip leading spaces, and fault if nothing else
C
      PTR = PTR + 1
      IF (SEMXA1(0,LINE,LINELN,PTR,X,I)) GOTO 100
      NCOL = LINELN - PTR + 1
      IF (NCOL.LE.0) GOTO 100
C
C Open picture to receive it
C
      LP1 = 0
      IF (SEMOPN(2,SEMPPN(NUMBER),NCOL,1,1,2,0,LP1)) GOTO 80
C
C Copy macro to text
C
      K = PTR
      DO 50 I = 1,NCOL
         TEXT(I) = LINE(K)
         K = K + 1
   50 CONTINUE
C
C Output macro
C
      NC4 = NCOL
      CALL CFORM(TEXT,TEXT,NFMINT,NFMBYT,NC4)
      IF (SEMROW(2,TEXT,0,1,1,LP1)) GOTO 80
C
C Force fresh input command line ?
C
      LINELN = 0
   60 RETURN
C
C Errors
C
   70 IDERR = MACRO
   80 SEMMAC = .TRUE.
      GOTO 60
   90 ERROR = 21
      GOTO 80
  100 ERROR = 17
      GOTO 80
  110 ERROR = 49
      GOTO 70
C
C Copyright (C) 1987-1990:  Synoptics Ltd,  All Rights Reserved
C
      END
