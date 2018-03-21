C
C Semper 6 system routine SEMIOE
C
      LOGICAL FUNCTION SEMIOE(IOS,UNIT,FILENM)
C
      INTEGER IOS,UNIT
      CHARACTER*(*) FILENM
C
      INCLUDE 'COMMON'
C
      INTEGER I
      LOGICAL SEMDIA
C
      SEMIOE = .TRUE.
      IDERR = IOS
      IDERR2 = UNIT
      IDMESS = FILENM
      ERROR = 129
C
      I = IOS
      CALL IOSMSG(I,RECORD(10:))
      RECORD(1:9) = 'Message: '
      IF (SEMDIA(RECORD,NDIERR)) GOTO 10
      SEMIOE = .FALSE.
C
   10 RETURN
C
C Copyright (C) 1989-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
