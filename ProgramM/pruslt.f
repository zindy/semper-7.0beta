C Semper 6 system module PRUSLT
C
      LOGICAL FUNCTION PRUSLT(DEV,FRSLOT,OLSLOT,FRADDR,FRSIZE,
     +                        NDSLOT,NAMLEN,NAME)
C
C Finds an empty directory slot and returns info on free space
C Also used to locate a directory entry if FRSLOT supplied non zero
C
      INCLUDE 'COMMON'
C
      LOGICAL PRUIND,PRUINF
C ****** WOS ******
C     LOGICAL LDUM
C ****** *** ******
C
      INTEGER DEV,FRSLOT,OLSLOT
      INTEGER*4 IDSLOT,FRADDR,FRSIZE,I4D,NDSLOT
      INTEGER SLTYPE,NAMLEN,DNAME(PRNACT),NAME(PRNACT)
      INTEGER I,IT,IL,ID,ISL
C
      PRUSLT=.TRUE.
C
C Get address of free space
C
C ****** WOS ******
C normal code..
      IF (PRUINF(1,DEV,FRADDR,FRSIZE,NDSLOT)) GOTO 40
C debug code..
C     LDUM=PRUINF(1,DEV,FRADDR,FRSIZE,NDSLOT)
C      write (6,1) ldum,dev,fraddr,frsize,ndslot
C 1    format (' PRUINF returns ',L1,' to PRUSLT'
C     +/'        with dev ',i2,' fraddr,frsize ',2i5,' ndslot ',i5)
C      IF (LDUM) GOTO 40
C ****** *** ******
C
C Find a free directory slot
C
      DO 20 IDSLOT=1,NDSLOT
         IF (FRSLOT.NE.0 .AND. OLSLOT.NE.0) GOTO 30
         ISL=IDSLOT
         IF (PRUIND(1,DEV,ISL,I4D,I4D,SLTYPE,IT,IL,ID,DNAME)) GOTO 40
         IF (SLTYPE.EQ.0) THEN
             IF (FRSLOT.EQ.0) FRSLOT=ISL
         ELSE IF (SLTYPE.EQ.2) THEN
             IF (ID .EQ. NAMLEN) THEN
                DO 10 I=1,ID
                   IF (DNAME(I).NE.NAME(I)) GOTO 20
   10           CONTINUE
C
C Name already in library - remember it
C
                OLSLOT = ISL
            ENDIF
         ENDIF
   20 CONTINUE
C
      IF (FRSLOT .EQ. 0) THEN
         IDERR = DEV
         ERROR = 100
         GOTO 40
      ENDIF
C
C Slot number in FRSLOT, any old one in OLSLOT
C Workspace area in FRADDR, size in FRSIZE
C
   30 PRUSLT = .FALSE.
   40 RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
