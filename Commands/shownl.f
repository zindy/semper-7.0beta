C Semper 6 sub-processing module SHOWNL
C
      LOGICAL FUNCTION SHOWNL(STRING)
      CHARACTER*(*) STRING
C
      LOGICAL SEMCON,SHOWLF
C
      IF (SHOWLF( )) THEN
         SHOWNL = .TRUE.
      ELSE
         SHOWNL = SEMCON(STRING)
      ENDIF
      RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
