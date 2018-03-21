C Semper 6 system module SEMMED
C
      LOGICAL FUNCTION SEMMED(DEVICE,MEDIUM)
      INTEGER DEVICE,MEDIUM
C
C Checks the the device number DEVICE is legal, that the specified
C device is assigned and returns the corresponding medium number for
C that device
C
      INCLUDE 'COMMON'
C
      SEMMED=.TRUE.
C
C Fault illegal device number
C
      IF (DEVICE.LT.1.OR.DEVICE.GT.NDVS) THEN
         ERROR=76
         IDERR=DEVICE
         GOTO 10
      ENDIF
C
C Fetch medium number for the device
C
      MEDIUM=MEDN(DEVICE)
C
C Fault non-assigned device
C
      IF (MEDIUM.LE.0) THEN
         ERROR=34
         IDERR=DEVICE
         GOTO 10
      ENDIF
C
      SEMMED=.FALSE.
C
   10 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
