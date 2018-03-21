C Semper 6 system module SEMDPN
C
      LOGICAL FUNCTION SEMDPN(DEVPAR,DEVICE,PARTN)
C
C Checks a combined device/partition number and returns the
C corresponding device and partition numbers.  The device number
C defaults to that held in DISPLA.
C
      INTEGER DEVPAR,DEVICE,PARTN
      LOGICAL SEMMED
C
      INCLUDE 'COMMON'
C
      INTEGER MEDIUM
C
      SEMDPN=.TRUE.
C
C Fault zero or negative device/partition number
C
      IF (DEVPAR.LE.0) THEN
         ERROR=73
         IDERR=DEVPAR
         GOTO 10
      ENDIF
C
C Determine device number
C
      IF (DEVPAR.LT.1000) THEN
         DEVICE=INT(DISPLA)/1000
      ELSE
         DEVICE=DEVPAR/1000
      ENDIF
C
C Fetch medium number for device
C
      IF (SEMMED(DEVICE,MEDIUM)) GOTO 10
C
C Fault non-display device
C
      IF (MEDIUM.NE.MEDDS) THEN
         ERROR=71
         IDERR=DEVICE
         GOTO 10
      ENDIF
C
C Determine partition number
C
      PARTN = MOD(DEVPAR,1000)
C
C Fault illegal partition number
C
      IF (PARTN.LT.1.OR.PARTN.GT.NDPDS) THEN
         ERROR=70
         IDERR=PARTN
         GOTO 10
      ENDIF
C
      SEMDPN=.FALSE.
C
   10 RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
