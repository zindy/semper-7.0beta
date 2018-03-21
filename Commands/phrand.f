C Semper 6 processing module PHRAND
C
      SUBROUTINE PHRAND
C
C Multiplies all points of fourier class picture in LP1 by a
C randomly varying phase factor, placing the result in LP2.
C Conjugate symmetry is preserved for a half-plane fourier picture
C
      REAL VAL
      INTEGER IVALPN
      LOGICAL SEMROW,SEMTFC,SEMLU
C
      INCLUDE 'COMMON'
C
      REAL C,PHASE,S,U,V,XRAND,YRAND
      INTEGER CROW,I,J,NCOL,NROW
      LOGICAL HALF
C
C Packed names
C
      INTEGER NRNM,NFROM
      PARAMETER (NRNM=29373, NFROM=10335)
C
C Fault multi-layer source or output picture
C
      IF (NLAYS(LP1).NE.1.OR.NLAYS(LP2).NE.1) THEN
         ERROR=62
         IDERR=VERB
         GOTO 30
      ENDIF
C
C Fault non-fourier class source picture
C
      IF (CLASSN(LP1).NE.NCLFOU) THEN
         ERROR=6
         IDERR=IVALPN(NFROM)
         GOTO 30
      ENDIF
C
C Fault bad centre position
C
      IF (SEMTFC(LP1,HALF)) GOTO 30
C
C Fault half-plane fourier source picture with too many rows to process
C
      IF (HALF) THEN
         CROW=CROWN(LP1)
         IF (CROW-1.GT.LNBUF/LNREAL) THEN
            ERROR=5
            IDERR=IVALPN(NFROM)
            GOTO 30
         ENDIF
      ENDIF
C
C Fetch source picture size
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
C
C Fetch random number seed
C
      YRAND=VAL(NRNM)
C
C Randomize phase of every point in source picture
C
      DO 20 J=1,NROW
C
C Read source row from LP1
C
         IF (SEMROW(1,RB1,NFMCOM,J,1,LP1)) GOTO 30
C
C Generate new random number seed for each row
C
         YRAND=5.67152419*YRAND+0.134615743
         YRAND=YRAND-AINT(YRAND)
         XRAND=YRAND
C
C Randomize phase of every point in row
C
         DO 10 I=1,2*NCOL,2
            XRAND=9.378651*XRAND
            XRAND=XRAND-AINT(XRAND)
            PHASE=TWOPI*XRAND
            C=COS(PHASE)
            S=SIN(PHASE)
            U=RB1(I)
            V=RB1(I+1)
            RB1(I)=C*U-S*V
            RB1(I+1)=S*U+C*V
   10    CONTINUE
C
C If half-plane fourier picture, maintain complex-conjugate symmetry
C
         IF (HALF) THEN
C
C If above centre row, store centre column value
C
            IF (J.LT.CROW) THEN
               RB2(CROW-J)=RB1(1)
               RB3(CROW-J)=RB1(2)
C
C Otherwise, if below centre row, reset centre column value to
C complex-conjugate of corresponding value above centre row
C
            ELSE IF (J.GT.CROW) THEN
               RB1(1)=RB2(J-CROW)
               RB1(2)=-RB3(J-CROW)
C
C Otherwise, centre row, centre value must be real
C
            ELSE
               RB1(1)=SQRT(RB1(1)*RB1(1)+RB1(2)*RB1(2))
               RB1(2)=0.0
            ENDIF
         ENDIF
C
C Store result in LP2
C
         IF (SEMROW(2,RB1,NFMCOM,J,1,LP2)) GOTO 30
   20 CONTINUE
C
C Set variable RMN to current random number seed
C
      IF (SEMLU(1,NRNM,YRAND)) GOTO 30
C
   30 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
