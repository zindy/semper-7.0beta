C Semper 6 subsidiary module SHOW0
C
      LOGICAL FUNCTION SHOW0(STRING,ILEN,NEED,TLEN)
      CHARACTER*(*) STRING
      INTEGER ILEN,NEED,TLEN
C
      LOGICAL SEMCON
C
      SHOW0 = .TRUE.
      IF (ILEN + NEED .GT. TLEN) THEN
         IF (ILEN .NE. 0) THEN
            IF (SEMCON(STRING(1:ILEN))) GOTO 10
         ENDIF
         ILEN = 0
      ENDIF
      SHOW0 = .FALSE.
C
   10 RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
