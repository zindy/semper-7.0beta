C Semper 6 system module PRUCOM
C
      LOGICAL FUNCTION PRUCOM(DEV)
C
C Compress Program Library Device
C
      INTEGER DEV
C
      INCLUDE 'COMMON'
C
      LOGICAL ABANDN,PRUFLS,PRUIND,PRUINF,PRUCOP,SEMDIA
      INTEGER NAME(PRNACT)
      INTEGER ISL,IUSE,ILL,ITL,ITY,N
      INTEGER*4 I4N,I4N1,I4N2,IBLK,NDSLOT,PTSIZE,PTADDR
C
      PRUCOM=.TRUE.
      IF (PRUINF(1,DEV,PTADDR,PTSIZE,NDSLOT)) GOTO 30
C
C IBLK will hold the insertion block position
C
      IBLK = DRSIZ(DEV) + 1
C
C IUSE indicates the next available slot
C
      IUSE = 0
      DO 10 I4N = 1,NDSLOT
C
C Refuse abandon requests politely
C
         IF (ABANDN(ERROR)) THEN
            IF (SEMDIA('Impossible to abandon Program library compress',
     +         NDIWAR)) GOTO 30
         ENDIF
         ERROR = 0
         ISL = I4N
         IF (PRUIND(1,DEV,ISL,I4N1,I4N2,ITY,ITL,ILL,N,NAME)) GOTO 30
         IF (ITY .EQ. 2) THEN
C
C Move text and label entries - earliest block first
C
             IF (I4N1 .LT. I4N2) THEN
                 IF (PRUCOP(DEV,DEV,I4N1,IBLK,ITL)) GOTO 30
                 I4N1 = IBLK
                 IBLK = IBLK + ITL
                 IF (PRUCOP(DEV,DEV,I4N2,IBLK,ILL)) GOTO 30
                 I4N2 = IBLK
                 IBLK = IBLK + ILL
             ELSE
                 IF (PRUCOP(DEV,DEV,I4N2,IBLK,ILL)) GOTO 30
                 I4N2 = IBLK
                 IBLK = IBLK + ILL
                 IF (PRUCOP(DEV,DEV,I4N1,IBLK,ITL)) GOTO 30
                 I4N1 = IBLK
                 IBLK = IBLK + ITL
             ENDIF
C
C Bump in use slot
C
             IUSE = IUSE+1
C
C Write the slot as it is active
C
             ISL = IUSE
             IF (PRUIND(2,DEV,ISL,I4N1,I4N2,ITY,ITL,ILL,N,NAME)) GOTO 30
         ENDIF
C
   10 CONTINUE
C
C Clear all unused slots
C
      I4N2 = IUSE
      I4N2 = I4N2 + 1
      IF (I4N2 .LE. NDSLOT) THEN
          I4N1 = 0
          N = 0
          DO 20 I4N = I4N2,NDSLOT
             ISL = I4N
             IF (PRUIND(2,DEV,ISL,I4N1,I4N1,N,N,N,N,NAME)) GOTO 30
   20     CONTINUE
      ENDIF
C
C Rebuild free space pointers
C
      PTADDR = IBLK
      PTSIZE = FLSIZ(DEV) - PTADDR + 1
      IF (PRUINF(2,DEV,PTADDR,PTSIZE,NDSLOT)) GOTO 30
C
C Flush buffer
C
      PRUCOM = PRUFLS(0)
   30 RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
