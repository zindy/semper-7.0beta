C Semper 6 system module SEMDCR
C
      LOGICAL FUNCTION SEMDCR(IDUMMY)
      INTEGER IDUMMY
C
C Dumps any current row from SEMBUF, and calls RANGE to ensure
C deletion of any recorded range
C
C     Change: LDM, July 2005
C     Ensure that gfortran does not think of range, unpack as intrinsics
      EXTERNAL RANGE, UNPACKF
      LOGICAL SEMROW,RANGE
C
      INCLUDE 'COMMON'
C
      SEMDCR = .TRUE.
      IF (BUFROW.LT.0) THEN
         IF (SEMROW(2,RB1,BUFFRM,-BUFROW,BUFLAY,BUFLPN)) GOTO 10
         IF (RANGE(3,BUFLPN)) GOTO 10
      ENDIF
C
      SEMDCR = .FALSE.
      BUFROW = 0
   10 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      IJUNK = IDUMMY
      END
