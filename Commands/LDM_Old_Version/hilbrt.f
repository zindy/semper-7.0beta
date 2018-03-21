C Semper 6 processing module HILBRT
C
      SUBROUTINE HILBRT
C
C Copies real part from LV1 to LV2, forcing imaginary part to be the
C Hilbert transform of the real part, so providing 'analytic'
C image. LEFT/RIGHT indicate over which half the LV2 transform vanishes
C (default is LEFT); LP requests additional l.p. wedge filter in HT
C calculation tapering from 1 to 0 in both directions; NOZERO causes
C background levels of each im part to be copied from real part
C
      INTEGER IVALPN
      LOGICAL OPT,OPTNO,SEMOPN,SEMCP2,SEMROW
C
      INCLUDE 'COMMON'
C
      REAL DR,R,RNORM
      INTEGER I,I1,I2,ID,II,J,NCOL,NCOL2,NROW
      LOGICAL RIGHT,LP,NOZERO
C
C Packed names
C
      INTEGER NRIGHT,NLP,NZERO,NFROM,NTO
      PARAMETER (NRIGHT=29167, NLP=19840, NZERO=-9819)
      PARAMETER (NFROM=10335, NTO=-601)
C
C Fetch source picture size
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
C
C Fault multi-layer source
C
      IF (NLAYS(LP1).NE.1) THEN
         ERROR=62
         IDERR=VERB
         GOTO 50
      ENDIF
C
C Fault source picture with row length not a power of 2
C
      IF (SEMCP2(NCOL,1)) THEN
         IDERR=IVALPN(NFROM)
         GOTO 50
      ENDIF
C
C Open output, forcing complex
C
      LP2=LP1
      IF (SEMOPN(2,IVALPN(NTO),NCOL,NROW,1,CLASSN(LP1),NFMCOM,LP2))
     +   GOTO 50
C
C Fetch options LEFT/RIGHT, LP and NOZERO
C
      RIGHT=OPT(NRIGHT)
      LP=OPT(NLP)
      NOZERO=OPTNO(NZERO)
C
C Establish loop parameters according to half plane selected
C
      IF (RIGHT) THEN
         I1=3
         I2=NCOL
         ID=NCOL
      ELSE
         I1=NCOL+3
         I2=2*NCOL
         ID=-NCOL
      ENDIF
C
      NCOL2=2*NCOL
      RNORM=2.0/REAL(NCOL)
C
C Process source picture
C
      DO 40 J=1,NROW
C
C Read source row from LP1
C
         IF (SEMROW(1,RB1,NFMCOM,J,1,LP1)) GOTO 50
C
C Zero imaginary part
C
         DO 10 I=2,2*NCOL,2
            RB1(I)=0.0
   10    CONTINUE
C
C Transform row
C
         CALL FT1D(RB1,NCOL,-1,.FALSE.,.FALSE.,.FALSE.)
C
C Wedge + sign filter
C
         IF (LP) THEN
C
            IF (RIGHT) THEN
               R=0.0
               DR=RNORM/REAL(NCOL)
            ELSE
               R=RNORM
               DR=-RNORM/REAL(NCOL)
            ENDIF
C
            II=NCOL+3
            DO 20 I=2,NCOL
               R=R+DR
               RB1(II)=RB1(II)*R
               RB1(II+1)=RB1(II+1)*R
               II=II+2
               IF (II.GT.NCOL2) II=1
   20       CONTINUE
C
C Otherwise, sign filter only
C
         ELSE
            DO 30 I=I1,I2
               RB1(I)=RB1(I)*RNORM
               RB1(I+ID)=0.0
   30       CONTINUE
C
            RB1(1)=RB1(1)/REAL(NCOL)
         ENDIF
C
         RB1(2)=0.0
         RB1(NCOL+1)=RB1(NCOL+1)/REAL(NCOL)
C
         IF (NOZERO) THEN
            RB1(NCOL+2)=RB1(NCOL+1)
         ELSE
            RB1(NCOL+2)=0.0
         ENDIF
C
C Invert transform
C
         CALL FT1D(RB1,NCOL,1,.FALSE.,.FALSE.,.FALSE.)
C
C Store result in LP2
C
         IF (SEMROW(2,RB1,NFMCOM,J,1,LP2)) GOTO 50
   40 CONTINUE
C
   50 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
