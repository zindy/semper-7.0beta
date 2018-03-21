C Semper 6 system module PRUPRI
C
      LOGICAL FUNCTION PRUPRI(IOP,DEVICE)
      INTEGER IOP,DEVICE
C
C Adds (IOP = 1) or removes (IOP = 2) a device from the program
C priority queue
C
      INTEGER I,N
C
      INCLUDE 'COMMON'
C
      PRUPRI=.TRUE.
C
C Fault illegal device number
C
      IF (DEVICE.LT.1.OR.DEVICE.GT.NDVS) THEN
         ERROR=76
         IDERR=DEVICE
         GOTO 30
      ENDIF
C
C Fault non-assigned device
C
      IF (MEDN(DEVICE).EQ.0) THEN
         ERROR=34
         IDERR=DEVICE
         GOTO 30
      ENDIF
C
C See if this device is a program library
C
      IF (DVTYP(DEVICE).EQ.FLTRUN) THEN
C
C IOP = 1, add device to program priority queue
C
         IF (IOP.EQ.1) THEN
            DO 10 I=PTNUMB+1,2,-1
               PTPRIO(I)=PTPRIO(I-1)
   10       CONTINUE
C
            PTPRIO(1)=DEVICE
C
            PTNUMB=PTNUMB+1
C
C IOP = 2, remove device from program priority queue
C
         ELSE IF (IOP.EQ.2) THEN
            N=0
C
            DO 20 I=1,PTNUMB
               IF (PTPRIO(I).NE.DEVICE) THEN
                  N=N+1
                  PTPRIO(N)=PTPRIO(I)
               ENDIF
   20       CONTINUE
C
            PTNUMB=N
         ENDIF
      ENDIF
C
      PRUPRI=.FALSE.
C
   30 RETURN
C
C Copyright (C) 1993:  Synoptics Ltd,  All Rights Reserved
C
      END
