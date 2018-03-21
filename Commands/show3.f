C Semper 6 subsidiary module SHOW3
C
      LOGICAL FUNCTION SHOW3(DEVICE,STRING,TEXT,NTEXT)
C
      INTEGER DEVICE,TEXT(*),NTEXT
      CHARACTER*(*) STRING
C
      LOGICAL SEMCON
C
      INTEGER N
C
      INCLUDE 'COMMON'
C
      WRITE(RECORD,10) DEVICE
C
      N = 4 + LEN(STRING)
      RECORD(5:N) = STRING
C
      IF (NTEXT.NE.0) THEN
         N = N + 1
         CALL SEMCHS(RECORD(N+1:N+NTEXT),TEXT,NTEXT)
         N = N + NTEXT
      ENDIF
C
      SHOW3 = SEMCON(RECORD(1:N))
C
      RETURN
C
   10 FORMAT (I3)
C
C Copyright (C) 1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
