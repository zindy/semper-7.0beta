C Semper 6 subsidiary module MAGSUB
C
      LOGICAL FUNCTION MAGSUB(MAGN,ROW,ROWN,LAYER,INTERP)
C
C Supplies in RB1 row ROWN of magnified region of LP1 established via
C prior TSTSRG call; uses RB5,RB6,SMGI8; returns FP unless LP1 complex;
C must be called with ROWN incrementing serially from 1 or decrementing
C serially from NROW
C
C Might usefully handle integer data internally in the long run -
C faster for integer pictures, and less likelihood of buffer overflow
C
      LOGICAL SEMROW
C
      REAL ROW(256)
      INTEGER MAGN,ROWN,LAYER
      LOGICAL INTERP
C
      INCLUDE 'COMMON'
C
      REAL F,T,T0
      INTEGER INFORM,I,I1,I2,INC,ISTEP,J,MAGSTP,N,NCOL,NEXT,NROW
      LOGICAL IMPART
C
      EQUIVALENCE (SMGI10,INC)
C
      MAGSUB = .TRUE.
C
      NCOL=(SMGI4-SMGI1)*MAGN+1
      NROW=(SMGI5-SMGI2)*MAGN+1
C
      IF (FORMN(LP1).NE.NFMCOM) THEN
         INFORM=NFMFP
         I1=SMGI1
         I2=SMGI4
         ISTEP=1
      ELSE
         INFORM=NFMCOM
         NCOL=2*NCOL-1
         I1=2*SMGI1-1
         I2=2*SMGI4
         ISTEP=2
      ENDIF
      MAGSTP=MAGN*ISTEP
      IF (INTERP) THEN
         NEXT=MAGN+1
      ELSE
         NEXT=(MAGN+3)/2
      ENDIF
C
      IF (ROWN.EQ.1) INC=1
      IF (ROWN.EQ.NROW) INC=-1
C
C New source row(s) needed?
C
      F=1.0/REAL(MAGN)
      I=ROWN-1
      IF (.NOT.INTERP) I=I+MAGN/2
      J=I/MAGN
      IF (ROWN.EQ.1.OR.I/MAGN*MAGN.EQ.I) THEN
         IF (SEMROW(1,RB6,INFORM,SMGI2+J,LAYER,LP1)) GOTO 70
         IF (.NOT.INTERP) GOTO 30
         IF (NROW.EQ.1) GOTO 30
         IF (SEMROW(1,RB5,INFORM,SMGI2+J+INC,LAYER,LP1)) GOTO 70
         DO 10 I=I1,I2
            RB5(I)=(RB5(I)-RB6(I))*F
   10    CONTINUE
      ELSE
         IF (.NOT.INTERP) GOTO 30
         DO 20 I=I1,I2
            RB6(I)=RB6(I)+RB5(I)
   20    CONTINUE
      ENDIF
C
C Row interpolation/duplication
C
   30 N=I1
      I=1
      IMPART=.FALSE.
   40 I2=NEXT
      I2=I2*ISTEP
   50 T0=RB6(N)
      IF (INTERP) T=(RB6(N+ISTEP)-T0)*F
   60 ROW(I)=T0
      I=I+ISTEP
      IF (I.LT.I2) THEN
         IF (INTERP) T0=T0+T
         GOTO 60
      ENDIF
      N=N+ISTEP
      I2=I2+MAGSTP
      IF (I.LT.NCOL) GOTO 50
      ROW(I)=RB6(N)
C
C Im part too?
C
      IF (INFORM.EQ.NFMCOM .AND. .NOT.IMPART) THEN
         IMPART=.TRUE.
         N=I1+1
         I=2
         GOTO 40
      ENDIF
C
      MAGSUB=.FALSE.
C
   70 RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
