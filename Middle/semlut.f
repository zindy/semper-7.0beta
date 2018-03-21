C Semper 6 system module SEMLUT
C
      LOGICAL FUNCTION SEMLUT(OPC,NUMBER,MODE,LUT)
C
C Reads/writes (OPC = 1 or 2) display look-up table NUMBER from/to the
C work disc.  The lut mode is stored in an array in COMMON, in order
C to avoid having to re-arrange the lut data to accomodate the mode
C value.
C
      INTEGER OPC,NUMBER,MODE,LUT(*)
      LOGICAL DISC
      INTEGER*4 BLKN,N4
C
      INCLUDE 'COMMON'
C
      SEMLUT=.TRUE.
C
C Fault illegal look-up table number
C
      IF (NUMBER.LT.1.OR.NUMBER.GT.NLUTS) THEN
         ERROR=68
         IDERR=NUMBER
         GOTO 10
      ENDIF
C
C If write request, store mode value
C
      IF (OPC.EQ.2) THEN
         IF (MODE.LT.1.OR.MODE.GT.3) THEN
            LUTMOD(NUMBER)=0
         ELSE
            LUTMOD(NUMBER)=MODE
         ENDIF
      ENDIF
C
C If lut values defined, transfer then from/to work disc
C
      IF (LUTMOD(NUMBER).NE.0) THEN
         BLKN = WRKLUT + (NUMBER-1)*LUTSZE
         IF (LUTMOD(NUMBER).EQ.1) THEN
            N4 = LUTLEN
         ELSE
            N4 = 3*LUTLEN
         ENDIF
         IF (DISC(OPC,WRKDEV,N4,LUT,BLKN,NFMINT,NFMINT)) GOTO 10
      ENDIF
C
C If read request, return mode value
C
      IF (OPC.EQ.1) MODE=LUTMOD(NUMBER)
C
      SEMLUT=.FALSE.
C
   10 RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
