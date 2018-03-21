C Semper 6 system module PRUSOR
C
      LOGICAL FUNCTION PRUSOR(DEV,NDSLOT,SORTB,SORTI,NSORT)
C
C Prepares a list of entry pointers, sorted into alphabetical order
C by first keys
C
      INCLUDE 'COMMON'
C
      LOGICAL PRUIND,PRUINF
      INTEGER DEV
      INTEGER*4 NDSLOT
      INTEGER SORTL
      PARAMETER (SORTL=LNBUF/LNINT)
      INTEGER SORTB(SORTL),SORTI(SORTL)
C
      INTEGER NAME(PRNACT),NM2(PRNACT),IDSLOT,PTR,ITY
      INTEGER ISL,I,I2,IT,IL,J,LN1,LN2,N,N1,N2,NSORT
      INTEGER*4 I4N,I4D
C
      PRUSOR=.FALSE.
C
C Get number of slots
C
      IF (PRUINF(1,DEV,I4N,I4D,NDSLOT)) GOTO 90
      NSORT = 0
      ISL = NDSLOT
C
      DO 10 IDSLOT=1,ISL
         IF (PRUIND(1,DEV,IDSLOT,I4D,I4D,ITY,IT,IL,N,NAME)) GOTO 90
         IF (ITY.EQ.2) THEN
             IF (NSORT.EQ.SORTL) THEN
                 NSORT = -1
                 GOTO 100
             ENDIF
             NSORT = NSORT + 1
             SORTB(NSORT) = IDSLOT
             SORTI(NSORT) = (NAME(1)*256)+NAME(2)
         ENDIF
   10 CONTINUE
C
C Got all entries : Shell-sort list
C
      IF (NSORT.EQ.0) GOTO 100
      N2=0
   20 N2=N2+N2+1
      IF (N2.LT.NSORT) GOTO 20
C
   30 N2=N2/2
      IF (N2.EQ.0) GOTO 100
      N1=NSORT-N2
      DO 80 J=1,N1
         I=J
   40    I2=I+N2
C
C Compare the two short keys
C
         IF (SORTI(I2)-SORTI(I))70,50,80
   50    CONTINUE
C
C Short keys are the same (unfortunate) - pull in full keys
C
         IF (PRUIND(1,DEV,SORTB(I),I4D,I4D,ITY,IT,IL,LN1,NAME)) GOTO 90
         IF (PRUIND(1,DEV,SORTB(I2),I4D,I4D,ITY,IT,IL,LN2,NM2)) GOTO 90
C
C Compare key characters
C
         N=LN1
         IF (N.GT.LN2) N = LN2
         DO 60 PTR=1,N
            IF (NM2(PTR)-NAME(PTR))70,60,80
   60    CONTINUE
C
C Key chars present match: shorter sorts low
C
         IF (N.GE.LN1) GOTO 80
C
C Swap the key pointers
C
   70    N=SORTI(I)
         SORTI(I)=SORTI(I2)
         SORTI(I2)=N
         N=SORTB(I)
         SORTB(I)=SORTB(I2)
         SORTB(I2)=N
         I=I-N2
         IF (I.GT.0) GOTO 40
   80 CONTINUE
      GOTO 30
C
   90 PRUSOR=.TRUE.
  100 RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
