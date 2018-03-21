C Semper 6 sub-processing module SHOWNO
C
      LOGICAL FUNCTION SHOWNO(STRING)
      CHARACTER*(*) STRING
C
      LOGICAL SHOWNL
C
      INCLUDE 'COMMON'
C
      INTEGER I,LNBLNK
C
      I = LNBLNK(STRING)
      RECORD = 'No '//STRING(1:I)//'(s) found'
      I = I + 12
      SHOWNO = SHOWNL(RECORD(1:I))
C
      RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
