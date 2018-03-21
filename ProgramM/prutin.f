C Semper 6 system module PRUTIN
C
      LOGICAL FUNCTION PRUTIN(DEVICE)
C
C Initialises a Program Text file on DEVICE to empty
C
      INTEGER DEVICE
      LOGICAL PRUFLS,PRUIND,PRUINF
C
      INCLUDE 'COMMON'
C
      INTEGER ISL
      INTEGER*4 LASTDB,NDSLOT,PTADDR,PTSIZE,I4N,I40
      PARAMETER (I40=0)
C
C Write free space info and number of slots
C (Flush buffer for previous device at same time)
C
      IF (PRUFLS(DEVICE)) GOTO 30
      LASTDB = DRSIZ(DEVICE)
      PTSIZE = FLSIZ(DEVICE)-LASTDB
      PTADDR = LASTDB+1
      NDSLOT = (LASTDB-3)
      NDSLOT = NDSLOT / PRCSZE
      IF (PRUINF(2,DEVICE,PTADDR,PTSIZE,NDSLOT)) GOTO 30
C
C Now write an empty directory
C
      DO 10 I4N = 1,NDSLOT
         ISL = I4N
         IF (PRUIND(2,DEVICE,ISL,I40,I40,0,0,0,0,RB1)) GOTO 30
   10 CONTINUE
C
C Flush buffer
C
      PRUTIN = PRUFLS(0)
C
   20 RETURN
C
   30 PRUTIN = .TRUE.
      GOTO 20
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
