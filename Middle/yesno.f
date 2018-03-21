C Semper 6 subsidiary module YESNO
C
      LOGICAL FUNCTION YESNO(STRING,NAME,LNO)
C
      CHARACTER*(*) STRING,NAME
      LOGICAL LNO
C
C Accepts line from terminal and returns LNO set to .FALSE. if and only
C if 'yes' is entered
C
      LOGICAL KLINE,SEMINP,SEMINT,SEMXA1
C
      REAL X
      INTEGER I,N,NR,ITEXT(80)
      CHARACTER*4 REPLY
C
      INCLUDE 'COMMON'
      EQUIVALENCE (RB2,ITEXT)
C
      INTEGER NYES
      PARAMETER (NYES=-8220)
C
      YESNO = .TRUE.
C
C Suppress confirmation if not in interactive mode
C
      IF (SEMINT(.TRUE.)) THEN
C
C Construct prompt string
C
         RECORD = STRING
         I = LEN(STRING)
         N = I + LEN(NAME) + 12
         RECORD(I+1:N) = NAME//'? (yes/no): '
C
C Output the prompt and read the response
C
         NR=0
         IF (KLINE(RECORD(1:N),.TRUE.,REPLY,NR)) GOTO 10
C
C Reflect input text and determine nature of response
C
         IF (NR.EQ.0) THEN
            IF (SEMINP(' ')) GOTO 10
C
            LNO = .TRUE.
         ELSE
            IF (SEMINP(REPLY(1:NR))) GOTO 10
C
            CALL SEMICS(REPLY,ITEXT,NR)
            I = 1
            DUMLOG = SEMXA1(1,ITEXT,NR,I,X,N)
            LNO = N .NE. NYES
         ENDIF
      ELSE
         LNO = .FALSE.
      ENDIF
C
      YESNO = .FALSE.
C
   10 RETURN
C
C Copyright (C) 1987-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
