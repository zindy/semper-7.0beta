C Semper 6 system module MARSET
C
      LOGICAL FUNCTION MARSET(ANNOT,MARK)
      LOGICAL ANNOT
      INTEGER MARK
C
C Examines key MARK and returns ANNOT set to .TRUE. if the value of MARK
C is o.k. for annotation to proceed.  If MARK is in the range 1 to 999,
C its value defaults to the value of variable DISPLA.  In this case,
C ANNOT is only set to .TRUE. if DISPLA points to an existing display
C picture with an appropriate type.  Argument MARK is set to the
C corresponding picture number.
C
      INTEGER IVAL
      LOGICAL SEMDPN,SEMDPD
C
      INTEGER DEVICE,PARTN
C
      INCLUDE 'COMMON'
C
C Packed name
C
      INTEGER NMARK
      PARAMETER (NMARK=20858)
C
      MARSET=.TRUE.
C
C Fetch value of key MARK
C
      MARK=IVAL(NMARK)
C
C Fault value less than zero
C
      IF (MARK.LT.0) THEN
         ERROR=3
         IDERR=NMARK
         GOTO 10
C
C Otherwise, if value is zero (NO), return flag for no annotation
C
      ELSE IF (MARK.EQ.0) THEN
         ANNOT=.FALSE.
C
C Otherwise, if value in range 1 to 999, use value if variable DISPLA
C and check that corresponding picture exists and has valid type for
C annotation
C
      ELSE IF (MARK.GE.1.AND.MARK.LE.999) THEN
         MARK=INT(DISPLA)
C
C Check value and obtain corresponding device and partition number
C
         IF (SEMDPN(MARK,DEVICE,PARTN)) GOTO 10
C
C Fetch DPD from work disc
C
         IF (SEMDPD(1,PARTN)) GOTO 10
C
C Allow annotation for 2-D image, graph or histogram only
C
         ANNOT=DPTYP.GE.1.AND.DPTYP.LE.3
C
C Otherwise, allow annotation to proceed as usual
C
      ELSE
         ANNOT=.TRUE.
      ENDIF
C
      MARSET=.FALSE.
C
   10 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
