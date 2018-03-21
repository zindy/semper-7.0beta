C Semper 6 system module TEXTU1
C
      LOGICAL FUNCTION TEXTU1(TEXT,LENGTH,MAXLEN,START,L1,NEW,L2)
C
C Replaces L1 chars beginning at TEXT(START) by NEW(L2), returning
C TRUE (without ERROR) iff revised buffer length would exceed MAXLEN
C
      INTEGER TEXT(*),LENGTH,MAXLEN,START,L1,NEW(*),L2
C
      INTEGER IS,IP,I,IT
      IF (LENGTH+L2-L1 .LE. MAXLEN) THEN
C
C Shift tail of line first (if any)
C
         IS = L2 - L1
         IP = START + L1
         IF (IP .LE. LENGTH) THEN
             IF (IS .LT. 0) THEN
                DO 10 I = IP,LENGTH
                  IT = I + IS
                  TEXT(IT) = TEXT(I)
   10          CONTINUE
            ELSE IF (IS .GT. 0) THEN
               I = LENGTH
   20          IT = I + IS
               TEXT(IT) = TEXT(I)
               I = I - 1
               IF (I .GE. IP) GOTO 20
            ENDIF
         ENDIF
C
C Insert replacement string
C
         LENGTH = LENGTH + IS
         IF (L2 .NE. 0) THEN
            IP = START-1
            DO 30 I = 1,L2
               IT = I + IP
               TEXT(IT) = NEW(I)
   30       CONTINUE
         ENDIF
         TEXTU1 = .FALSE.
      ELSE
C
C Length overflow
C
         TEXTU1 = .TRUE.
      ENDIF
      RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
