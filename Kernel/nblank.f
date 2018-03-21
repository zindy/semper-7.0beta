C Semper 6 system module NBLANK
C Unix version for User programs only - do not process!
C
      INTEGER FUNCTION NBLANK(STRING)
C
C Returns non-blank length of STRING (zero if all blank)
C
      CHARACTER*(*) STRING
C
      INTEGER LNBLNK
C
      NBLANK = LNBLNK(STRING)
C
      RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
