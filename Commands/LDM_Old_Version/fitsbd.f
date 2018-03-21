C Semper 6 processing module FITSBD
C
      SUBROUTINE FITSBD
C
C Fits linear function ax+by+c to LP1 (ax+by if NOCON), returning
C coeffs in A,B,C; if SUBTRACT and/or DIVIDE, subtracts and/or divides
C by function (in that order).
C
      INTEGER IVALPN,SEMFRM
      LOGICAL SEMOPN,SEMROW,SEMLU,OPT,OPTNO
C
      INCLUDE 'COMMON'
C
      REAL A,B,C,D,SUMCOL,SUMI,SUMII,SUMIP,SUMJ,SUMJJ,SUMJP
      REAL SUM1,SUMP,SUMROW,T
      INTEGER CCOL,CROW,I,J,NCOL,NROW
      LOGICAL DIVIDE,NOCON,SUBDIV,SUBTRA
C
C Packed names
C
      INTEGER NTO,NCON,NDIVID,NSUBTR,NA,NB,NC
      PARAMETER (NTO=-601, NCON=5414, NDIVID=6782, NSUBTR=31242)
      PARAMETER (NA=1600, NB=3200, NC=4800)
C
C Fault multi-layer pictures
C
      IF (NLAYS(LP1).NE.1) THEN
         ERROR=62
         IDERR=VERB
         GOTO 100
      ENDIF
C
      NOCON=OPTNO(NCON)
      SUBTRA=OPT(NSUBTR)
      DIVIDE=OPT(NDIVID)
      SUBDIV=SUBTRA.AND.DIVIDE
C
C Fetch source picture size and centre position
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      CCOL=CCOLN(LP1)
      CROW=CROWN(LP1)
C
C Zero column sums
C
      DO 10 I=1,NCOL
         RB2(I)=0.0
   10 CONTINUE
C
C Zero row sums
C
      DO 20 J=1,NROW
         RB3(J)=0.0
   20 CONTINUE
C
C Sum picture values along rows and columns
C
      DO 40 J=1,NROW
C
C Read row from source picture
C
         IF (SEMROW(1,RB1,NFMFP,J,1,LP1)) GOTO 100
C
C Increment column and row sums
C
         DO 30 I=1,NCOL
            RB2(I)=RB2(I)+RB1(I)
            RB3(J)=RB3(J)+RB1(I)
   30    CONTINUE
   40 CONTINUE
C
C Derive picture sums from column sums
C
      SUMCOL=0.0
      SUMIP=0.0
      DO 50 I=1,NCOL
         SUMCOL=SUMCOL+RB2(I)
         SUMIP=SUMIP+REAL(I)*RB2(I)
   50 CONTINUE
C
C Derive picture sums from row sums
C
      SUMROW=0.0
      SUMJP=0.0
      DO 60 J=1,NROW
         SUMROW=SUMROW+RB3(J)
         SUMJP=SUMJP+REAL(J)*RB3(J)
   60 CONTINUE
C
C Sum of picture values = (column sum + row sum)/2
C
      SUMP=(SUMCOL+SUMROW)/2.0
C
C Find coefficients
C
      IF (NROW.EQ.1) THEN
         IF (NCOL.EQ.1) THEN
            A=0.0
            B=0.0
            C=SUMP
         ELSE
            SUM1=REAL(NCOL)
            SUMI=SUM1*REAL(NCOL+1)/2.0
            SUMII=SUMI*REAL(2*NCOL+1)/3.0
C
            D=SUMII*SUM1-SUMI*SUMI
C
            A=(SUM1*SUMIP-SUMI*SUMP)/D
            B=0.0
            C=(SUMII*SUMP-SUMI*SUMIP)/D
         ENDIF
      ELSE
         IF (NCOL.EQ.1) THEN
            SUM1=REAL(NROW)
            SUMJ=SUM1*REAL(NROW+1)/2.0
            SUMJJ=SUMJ*REAL(2*NROW+1)/3.0
C
            D=SUMJJ*SUM1-SUMJ*SUMJ
C
            A=0.0
            B=(SUM1*SUMJP-SUMJ*SUMP)/D
            C=(SUMJJ*SUMP-SUMJ*SUMJP)/D
         ELSE
            SUM1=REAL(NCOL)*REAL(NROW)
C
            A=12.0*(SUMIP-REAL(NCOL+1)*SUMP/2.0)/
     +             (SUM1*REAL(NCOL-1)*REAL(NCOL+1))
            B=12.0*(SUMJP-REAL(NROW+1)*SUMP/2.0)/
     +             (SUM1*REAL(NROW-1)*REAL(NROW+1))
            C=((7.0*SUM1-REAL(NCOL+NROW+5))*SUMP-
     +             6.0*(REAL(NROW-1)*SUMIP+REAL(NCOL-1)*SUMJP))/
     +             (SUM1*REAL(NCOL-1)*REAL(NROW-1))
         ENDIF
      ENDIF
C
C Coefficient fit only?
C
      IF (.NOT.(SUBTRA.OR.DIVIDE)) GOTO 90
C
C Open output
C
      LP2=LP1
      IF (SEMOPN(2,IVALPN(NTO),NCOL,NROW,1,
     +   CLASSN(LP1),SEMFRM(FORMN(LP1)),LP2)) GOTO 100
C
C Process source picture to subtract/divide function
C
      DO 80 J=1,NROW
C
C Read row from source picture
C
         IF (SEMROW(1,RB1,NFMFP,J,1,LP1)) GOTO 100
C
C Initialise function for lhs
C
         IF (NOCON) THEN
            T=A*REAL(1-CCOL)+B*REAL(J-CROW)
         ELSE
            T=A+B*REAL(J)+C
         ENDIF
C
C Process picture row
C
         DO 70 I=1,NCOL
            IF (SUBDIV) THEN
               IF (T.EQ.0.) GOTO 110
               RB1(I)=(RB1(I)-T)/T
            ELSE IF (SUBTRA) THEN
               RB1(I)=RB1(I)-T
            ELSE
               IF (T.EQ.0.) GOTO 110
               RB1(I)=RB1(I)/T
            ENDIF
C
            T=T+A
   70    CONTINUE
C
C Store result in LP2
C
         IF (SEMROW(2,RB1,NFMFP,J,1,LP2)) GOTO 100
   80 CONTINUE
C
C Set variables A, B and C to plane coefficients (picture coords)
C
   90 C=A*CCOL+B*CROW+C
      B=-B
      IF (SEMLU(1,NA,A)) GOTO 100
      IF (SEMLU(1,NB,B)) GOTO 100
      IF (.NOT.NOCON) THEN
         IF (SEMLU(1,NC,C)) GOTO 100
      ENDIF
  100 RETURN
C
C Error
C
  110 ERROR=95
      GOTO 100
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
