C Semper 6 system module SEMDPD
C
      LOGICAL FUNCTION SEMDPD(OPC,NUMBER)
C
C Reads/writes (OPC = 1 or 2) display partition descriptor for partition
C NUMBER to/from COMMON /SEMPDB.
C
      INTEGER OPC,NUMBER
      LOGICAL DISC
      INTEGER*4 BLKN
C
      INCLUDE 'COMMON'
C
C
C Fault illegal display partition number
C
      IF (NUMBER.LT.1 .OR. NUMBER.GT.NDPDS) THEN
         ERROR = 70
         IDERR = NUMBER
         SEMDPD = .TRUE.
      ELSE
C
C Transfer display partition descriptor to/from the work disc
C
         BLKN = WRKDPD + (NUMBER-1)*(DPDSZE+LABSZE)
         SEMDPD = DISC(OPC,WRKDEV,LNDPD4,DPMIN,BLKN,NFMBYT,NFMBYT)
      ENDIF
C
      RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
