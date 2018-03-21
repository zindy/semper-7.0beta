C Semper 6 system module VARINT
C
      LOGICAL FUNCTION VARINT(NAME)
      INTEGER NAME
C
C Returns .TRUE. if variable NAME is unset or of integer value
C
      LOGICAL SEMLU
C
      REAL VALUE
C
      IF (SEMLU(-1,NAME,VALUE)) THEN
         VARINT = VALUE .EQ. ANINT(VALUE)
      ELSE
         VARINT = .TRUE.
      ENDIF
C
      RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
