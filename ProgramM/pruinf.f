C Semper 6 system module PRUINF
C
      LOGICAL FUNCTION PRUINF(OPCODE,DEVICE,FRADDR,FRSIZE,NDSLOT)
C
C Read/Write device info
C
      INTEGER DEVICE,OPCODE
      INTEGER*4 FRADDR,FRSIZE,NDSLOT
C
      INCLUDE 'COMMON'
C
      INTEGER*4 BUFF(3),I42,I4N
      LOGICAL DISC,PRUFLS
C
      PARAMETER (I42=2,I4N=(LNINT4*3))
C
      IF (.NOT.PRUFLS(DEVICE)) THEN
         IF (OPCODE .EQ. 2) THEN
            BUFF(1) = FRADDR
            BUFF(2) = FRSIZE
            BUFF(3) = NDSLOT
         ENDIF
         IF (DISC(OPCODE,DEVICE,I4N,BUFF,I42,NFMBYT,NFMBYT)) THEN
            PRUINF = .TRUE.
         ELSE
            IF (OPCODE .EQ. 1) THEN
               FRADDR = BUFF(1)
               FRSIZE = BUFF(2)
               NDSLOT = BUFF(3)
            ENDIF
            PRUINF=.FALSE.
         ENDIF
      ENDIF
C
      RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
