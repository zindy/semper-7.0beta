C Semper 6.3 processing module MAGSR
C
      SUBROUTINE MAGSR
C
C Extracts a block from LP1 to TO, with optional integer magnification
C using interpolation or (if NOINTERP) replication; multi-layer support
C
      INTEGER IVAL,IVALPN,SEMFRM
      LOGICAL TSTSRG,SEMCEN,MAGSUB,SEMOPN,SEMROW,OPT,MRKREG
C
      INCLUDE 'COMMON'
C
      INTEGER INFORM,J,MAGN,N1,N2,NL,NCOL,NLAY,NROW
      LOGICAL INTERP,SUBREG
C
      INTEGER NTIMES,NREPLI,NTO
      PARAMETER (NTIMES=-374,NREPLI=29016,NTO=-601)
C
      EQUIVALENCE (SMGL1,SUBREG)
C
C Initialise
C
      MAGN=IVAL(NTIMES)
      IF (MAGN.LE.1) THEN
         ERROR=3
         IDERR=NTIMES
         GOTO 30
      ENDIF
      INTERP=.NOT.OPT(NREPLI)
      IF (FORMN(LP1).NE.NFMCOM) THEN
         INFORM=NFMFP
      ELSE
         INFORM=NFMCOM
      ENDIF
C
C Establish subregion and mark it
C
      IF (TSTSRG(1,LP1)) GOTO 30
      IF (MRKREG(0)) GOTO 30
C
C Open output
C
      NCOL=(SMGI4-SMGI1)*MAGN+1
      NROW=(SMGI5-SMGI2)*MAGN+1
      NLAY=SMGI9
      LP2=LP1
      IF (SEMOPN(2,IVALPN(NTO),NCOL,NROW,NLAY,CLASSN(LP1),
     +   SEMFRM(FORMN(LP1)),LP2)) GOTO 30
C
C Adjust output centre
C
      N1=CCOLN(LP2)
      IF (SMGI7/2*2.EQ.SMGI7) N1=N1+MAGN/2
      N2=CROWN(LP2)
      IF (SMGI8/2*2.EQ.SMGI8) N2=N2+MAGN/2
      IF (SEMCEN(LP2,N1,N2,CLAYN(LP2))) GOTO 30
C
C Loop over layers
C
      DO 20 NL=SMGI3,SMGI6
C
C Loop over output rows
C
         DO 10 J=1,NROW
            IF (MAGSUB(MAGN,RB1,J,NL,INTERP)) GOTO 30
            IF (SEMROW(2,RB1,INFORM,J,NL-SMGI3+1,LP2)) GOTO 30
   10    CONTINUE
   20 CONTINUE
C
   30 RETURN
C
C Copyright (C) 1987-1991:  Synoptics Ltd,  All Rights Reserved
C
      END
