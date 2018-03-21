C Semper 6 system module PRUDIR
C
      LOGICAL FUNCTION PRUDIR(DEV)
C
C Report status on Program Library Device
C
      INTEGER DEV
      LOGICAL PRUIND,PRUINF,SEMCON
C
      INCLUDE 'COMMON'
C
      REAL ACTDKB,DELDKB,FREDKB,ACTFKB,DELFKB,FREFKB,V
      INTEGER NAME(PRNACT)
      INTEGER ISL,N,IDRACT,IDRDEL,IDRFRE,ITYPE,ITL,ILL
      INTEGER*4 I4N,I4N1,NDSLOT,PTADDR,PTSIZE,IFLACT,IFLDEL
C
      PRUDIR=.TRUE.
C
      IF (PRUINF(1,DEV,PTADDR,PTSIZE,NDSLOT)) GOTO 20
C
      IDRACT = 0
      IDRDEL = 0
      IDRFRE = 0
      IFLACT = 0
      IFLDEL = 0
C
C Read each slot and extract information
C
      DO 10 I4N = 1,NDSLOT
         ISL = I4N
         IF (PRUIND(1,DEV,ISL,I4N1,I4N1,ITYPE,ITL,ILL,N,NAME)) GOTO 20
         IF (ITYPE .EQ. 0) THEN
             IDRFRE = IDRFRE + 1
         ELSE IF (ITYPE .EQ. 1) THEN
             IDRDEL = IDRDEL + 1
             IFLDEL = IFLDEL + ITL + ILL
         ELSE
C
C ITYPE = 2
C
             IDRACT = IDRACT + 1
             IFLACT = IFLACT + ITL + ILL
         ENDIF
   10 CONTINUE
C
      V = REAL(PRCSZE*LNBLK)/1024.
      ACTDKB = REAL(IDRACT)*V
      DELDKB = REAL(IDRDEL)*V
      FREDKB = REAL(IDRFRE)*V
C
      V = REAL(LNBLK)/1024.
      ACTFKB = REAL(IFLACT)*V
      DELFKB = REAL(IFLDEL)*V
      FREFKB = REAL(PTSIZE)*V
C
      IF (SEMCON(' ')) GOTO 20
      IF (SEMCON('Program library       Active   Deleted      Free'))
     +   GOTO 20
      WRITE (RECORD,30) IDRACT,IDRDEL,IDRFRE
      IF (SEMCON(RECORD)) GOTO 20
      WRITE (RECORD,40) ACTDKB,DELDKB,FREDKB
      IF (SEMCON(RECORD)) GOTO 20
      WRITE (RECORD,50) ACTFKB,DELFKB,FREFKB
      IF (SEMCON(RECORD)) GOTO 20
C
      PRUDIR=.FALSE.
C
   20 RETURN
C
   30 FORMAT ('Number of entries',3I10)
   40 FORMAT ('Directory space  ',3(F8.1,'kb'))
   50 FORMAT ('File space       ',3(F8.1,'kb'))
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
