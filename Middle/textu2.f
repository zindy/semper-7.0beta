C Semper 6 subsidiary module TEXTU2
C
      LOGICAL FUNCTION TEXTU2(TEXT,TLEN)
C
      INTEGER TEXT(*),TLEN
C
C Verifies TEXT on console output stream
C
      LOGICAL SEMCON,SEMTPS
C
      INCLUDE 'COMMON'
C
      INTEGER NC,I,J,I1,I2,IWID,ILEN
C
      TEXTU2=.TRUE.
C
      IF (TLEN .GT. 0) THEN
C
C Fetch current terminal page size
C
         IF (SEMTPS(IWID,ILEN)) GOTO 40
C
C Settle output width
C
         NC = IWID-6
C
         I1 = 1
C
   10    I2 = I1
C
         J = 0
         DO 20 I = 1,NC
            IF (TEXT(I2) .EQ. KSPACE) J = I2
            IF (I2 .EQ. TLEN) GOTO 30
            I2 = I2 + 1
   20    CONTINUE
C
C End not reached - back up to last space
C
         IF (J .EQ. 0) THEN
            I2 = I1 + NC - 1
         ELSE
            I2 = J
         ENDIF
C
C Output current part of line
C
   30    RECORD(1:6)=' '
         CALL SEMCHS(RECORD(7:),TEXT(I1),I2-I1+1)
C
         IF (SEMCON(RECORD)) GOTO 40
C
C More to come?
C
         IF (I2 .LT. TLEN) THEN
            I1 = I2 + 1
            GOTO 10
         ENDIF
      ENDIF
C
      TEXTU2=.FALSE.
C
   40 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
