C Semper 6 processing module ECDEC
C
C Decodes directives on the basis of a supplied opcode/argumentcode
C list, returning the command number in OP, numerical arguments in
C ML and RL, and string arguments in MATCH(1-ML) and REP(1-RL).
C
C Processes repeat counts and loops internally, returning opcodes as if
C the loops were written out in full; loops terminated on ABANDN
C
C Valid directives are indicated by single (internal code) chars in
C DIRS(1-NDIRS), with an operand code in the element NDIRS later as
C follows:  -1(2) => read 1(2) strings;
C               0 => none;
C            1(2) => read 1(2) numbers
C
C Calling module sets EPTR to zero initially, and each subsequent
C time a TRUE return is made by ECDEC; at this time a prompt can
C be issued if desired.  Calling module must NOT reset any other
C arguments between calls, however, as this may affect looping.
C
      LOGICAL FUNCTION ECDEC(PROMPT,DIRS,NDIRS,EPTR,OP,MATCH,ML,REP,
     +                       RL,FAULT)
C
      CHARACTER*(*) PROMPT
C
      LOGICAL ABANDN,KLINE,SEMINP,RNO,SEMCON,FAULT
      INTEGER NDIRS
      INTEGER MATCH(80),REP(80),ED(80),DIRS(NDIRS),OP,RL,DEL
      INTEGER DIGIT(10),EPTR,COUNT,NUMBER,LASTE
      CHARACTER*80 LINE
C
      INCLUDE 'COMMON'
C
      INTEGER I,IC,ICODE,LPCT,LPST,ML
      EQUIVALENCE (RB6,ED),(SMGI10,LPCT)
C
      SAVE COUNT,LASTE
      DATA COUNT,LASTE /0,0/
C
      DATA DIGIT/KZERO,KONE,KTWO,KTHREE,KFOUR,
     +           KFIVE,KSIX,KSEVEN,KEIGHT,KNINE/
C
      ECDEC=.TRUE.
C
      NUMBER=0
C
C New input forced?
C
      IF (EPTR.EQ.0) GOTO 20
C
C Abandon requested?
C
      IF (ABANDN(ERROR)) GOTO 230
C
C Count down repeat loop, returning previous args
C
      IF (COUNT.NE.0) THEN
         COUNT=COUNT-1
         GOTO 130
      ENDIF
C
C Anything left on line?
C
   10 IF (EPTR.LE.LASTE) GOTO 30
      IF (LPCT.NE.0) GOTO 200
      GOTO 210
C
C Read new line of edit directives
C
   20 LASTE=0
      IF (KLINE(PROMPT,.TRUE.,LINE,LASTE)) GOTO 230
C
C Reflect line of input and return immediately if blank line
C
      IF (LASTE.EQ.0) THEN
         IF (SEMINP(' ')) GOTO 230
         GOTO 210
      ELSE
         IF (SEMINP(LINE(1:LASTE))) GOTO 230
      ENDIF
C
      CALL SEMICS(LINE,ED,LASTE)
C
      EPTR=1
      COUNT=0
      LPCT=0
C
C Ignore spaces
C
   30 IC=ED(EPTR)
      EPTR=EPTR+1
      IF (IC.EQ.KSPACE) GOTO 10
C
C Sort on command name (forcing to upper case)
C
      IF (IC.GE.KLCA .AND. IC.LE.KLCZ) IC = IC + (KUCA - KLCA)
      DO 40 OP=1,NDIRS
         IF (IC.EQ.DIRS(OP)) GOTO 50
   40 CONTINUE
C
C Not command - try ket
C
      IF (IC.EQ.KKET) GOTO 170
C
C Finally try a count
C
      RNO=.TRUE.
      GOTO 90
C
C Found command - sort on argument type
C
   50 ICODE=OP+NDIRS
      IF (DIRS(ICODE) .EQ. 0) THEN
C
C Done
C
         GOTO 130
      ELSE IF (DIRS(ICODE) .GT. 0) THEN
C
C Read number
C
         GOTO 80
      ENDIF
C
C Read match string
C
      IF (EPTR.GT.LASTE) GOTO 180
      DEL=ED(EPTR)
      EPTR=EPTR+1
      NUMBER=0
   60 IF (EPTR.LE.LASTE) THEN
         IC=ED(EPTR)
         EPTR=EPTR+1
         IF (IC.NE.DEL) THEN
            NUMBER=NUMBER+1
            MATCH(NUMBER)=IC
            GOTO 60
         ENDIF
      ENDIF
C
      ML=NUMBER
C
C Read replacement string if needed
C
      IF (DIRS(ICODE).EQ.-1) GOTO 130
      RL=0
   70 IF (EPTR.GT.LASTE) GOTO 130
      IC=ED(EPTR)
      EPTR=EPTR+1
      IF (IC.EQ.DEL) GOTO 130
      RL=RL+1
      REP(RL)=IC
      GOTO 70
C
C Read number
C
   80 RNO=.FALSE.
      IF (EPTR.GT.LASTE) GOTO 180
      IC=ED(EPTR)
      EPTR=EPTR+1
      IF (IC.EQ.KSPACE) GOTO 80
   90 NUMBER=-1
  100 DO 110 I=1,10
         IF (IC.EQ.DIGIT(I)) GOTO 140
  110 CONTINUE
C
C TERMINATED
C
      EPTR=EPTR-1
  120 IF (RNO) GOTO 150
      IF (NUMBER.LT.0) GOTO 180
      ML=NUMBER
C
  130 FAULT = .FALSE.
      GOTO 220
C
  140 IF (NUMBER.LT.0) NUMBER=0
      NUMBER=NUMBER*10+I-1
      IF (EPTR.GT.LASTE) GOTO 120
      IC=ED(EPTR)
      EPTR=EPTR+1
      GOTO 100
C
  150 RNO=.FALSE.
      IF (NUMBER.LT.0) GOTO 190
C
C Count found - bra following?
C
  160 IF (EPTR.GT.LASTE) GOTO 200
      IC=ED(EPTR)
      EPTR=EPTR+1
      IF (IC.EQ.KSPACE) GOTO 160
      IF (IC.NE.KBRA) THEN
C
C Simple repeat count
C
         EPTR=EPTR-1
         COUNT=NUMBER-1
         GOTO 10
      ENDIF
C
C Loop found - one already in progress?
C
      IF (LPCT.NE.0) GOTO 200
C
C Set up loop (count and start pointer)
C
      LPCT=NUMBER
      IF (LPCT.EQ.0) LPCT=-1
      LPST=EPTR
      GOTO 10
C
C Ket found - loop in progress?
C
  170 IF (LPCT.EQ.0) GOTO 190
      LPCT=LPCT-1
      IF (LPCT.EQ.0) GOTO 10
C
C Reset pointer to start of loop
C
      EPTR=LPST
      GOTO 30
C
C Errors
C
  180 RECORD(1:14)='Bad arg(s) to'
      RECORD(15:15)=CHAR(DIRS(OP))
      IF (SEMCON(RECORD(1:15))) GOTO 230
      GOTO 210
C
  190 RECORD(2:9)=' unknown'
      RECORD(1:1)=CHAR(IC)
      IF (SEMCON(RECORD(1:9))) GOTO 230
      GOTO 210
C
  200 IF (SEMCON('Bad directive loop')) GOTO 230
C
  210 FAULT = .TRUE.
  220 ECDEC = .FALSE.
C
  230 RETURN
C
C Copyright (C) 1987-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
