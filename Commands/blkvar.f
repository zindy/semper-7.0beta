C Semper 6 processing module BLKVAR
C
      SUBROUTINE BLKVAR
C
C Provides verbs LSD,LVARIANCE, which calculate local sd,variance values
C over square neighbourhoods
C
C Execution time virtually independent of size of square block;
C two temporary pictures (NCOL by OVER+1) used as workspace
C
C Uses FP/COMPLEX internally
C
      INTEGER IVAL
      LOGICAL SEMOPN,SEMROW
C
      INCLUDE 'COMMON'
C
      REAL RNORM,RRR
      INTEGER I,INFORM,ISTEP,JIN,JOUT,JWKI,JWKO,LAST,LP4
      INTEGER N,NCOL,NROW,WIDTH,WIDTHP,WIDR
      LOGICAL LCOMPL,L1D,LSD
C
C Packed names
C
      INTEGER NOVER,NLSD
      PARAMETER (NOVER=24885,NLSD=19964)
C
C Initialise
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      L1D=NROW.EQ.1
      LSD=VERB.EQ.NLSD
      WIDTH=IVAL(NOVER)
      IF (WIDTH.LE.1 .OR. WIDTH.GE.NCOL) GOTO 130
      IF (.NOT.L1D .AND. WIDTH.GE.NROW) GOTO 130
      WIDTHP=WIDTH+1
      WIDR=(WIDTH-1)/2
C
      LCOMPL=FORMN(LP1).EQ.NFMCOM
      IF (LCOMPL) THEN
         INFORM=NFMCOM
         ISTEP=2
         LAST=NCOL+NCOL
      ELSE
         INFORM=NFMFP
         ISTEP=1
         LAST=NCOL
      ENDIF
C
C Open workspace
C
      IF (.NOT.L1D) THEN
         LP3=0
         IF (SEMOPN(3,0,NCOL,WIDTHP,1,1,INFORM,LP3)) GOTO 120
         LP4=0
         IF (SEMOPN(3,0,NCOL,WIDTHP,1,1,INFORM,LP4)) GOTO 120
      ENDIF
C
      JWKO=1
      JWKI=1
      JIN=WIDR-WIDTH+1
      JOUT=1
      RNORM=WIDTH
      RNORM=1./RNORM
      IF (L1D) THEN
C
C 1-D case
C
         IF (SEMROW(1,RB4,INFORM,1,1,LP1)) GOTO 120
         CALL RSUM1D(RB1,RB4,NCOL,WIDTH,ISTEP)
         IF (SEMROW(1,RB4,INFORM,1,1,LP1)) GOTO 120
         DO 10 I=1,LAST
            RB4(I)=RB4(I)*RB4(I)
   10    CONTINUE
         CALL RSUM1D(RB2,RB4,NCOL,WIDTH,ISTEP)
C
C Merge with 2-D code for OUTPUT and return
C
         GOTO 90
      ENDIF
      RNORM=RNORM*RNORM
C
C Filter is separable - code for column filter follows, using calls
C to input, output, save and recover routines below; the row
C averages are performed by RSUM1D calls embedded in the input code
C
C Zero-fill RB1 and RB2 - these accumulate the data and their square
C
      DO 20 I=1,LAST
         RB1(I)=0.
         RB2(I)=0.
   20 CONTINUE
C
C Raw data: INPUT new row to RB3 via RB4, incl 1-D running sum
C
   30 IF (JIN .LE. 0) THEN
         N = 1
      ELSE IF (JIN .GT. NROW) THEN
         N = NROW
      ELSE
         N = JIN
      ENDIF
      IF (SEMROW(1,RB4,INFORM,N,1,LP1)) GOTO 120
      CALL RSUM1D(RB3,RB4,NCOL,WIDTH,ISTEP)
C
C SAVE RB3 in workspace
C
      IF (SEMROW(2,RB3,INFORM,JWKO,1,LP3)) GOTO 120
C
C Add it to RB1
C
      DO 40 I=1,LAST
         RB1(I)=RB1(I)+RB3(I)
   40 CONTINUE
C
C Repeat INPUT, SAVE, add for data square, using RB2
C
      IF (SEMROW(1,RB4,INFORM,N,1,LP1)) GOTO 120
      DO 50 I=1,LAST
         RB4(I)=RB4(I)*RB4(I)
   50 CONTINUE
      CALL RSUM1D(RB3,RB4,NCOL,WIDTH,ISTEP)
C
C SAVE RB3 in workspace
C
      IF (SEMROW(2,RB3,INFORM,JWKO,1,LP4)) GOTO 120
C
C Add it to RB2
C
      DO 60 I=1,LAST
         RB2(I)=RB2(I)+RB3(I)
   60 CONTINUE
C
C Step input and workspace row pointers this time
C
      JIN=JIN+1
      JWKO=JWKO+1
      IF (JWKO.GT.WIDTHP) JWKO=1
      IF (JIN.LE.WIDR+1) GOTO 30
C
C Raw data again: RECOVER old row from workspace to RB3
C
      IF (SEMROW(1,RB3,INFORM,JWKI,1,LP3)) GOTO 120
C
C Subtract from RB1
C
      DO 70 I=1,LAST
         RB1(I)=RB1(I)-RB3(I)
   70 CONTINUE
C
C Repeat RECOVER, subtract for data square, using RB2
C
      IF (SEMROW(1,RB3,INFORM,JWKI,1,LP4)) GOTO 120
C
C Subtract from RB2
C
      DO 80 I=1,LAST
         RB2(I)=RB2(I)-RB3(I)
   80 CONTINUE
C
C Step workspace recovery pointer this time
C
      JWKI=JWKI+1
      IF (JWKI.GT.WIDTHP) JWKI=1
C
C OUTPUT variance from RB1, RB2 via RB4
C
   90 IF (LCOMPL) THEN
         DO 100 I=1,LAST,2
            RRR=MAX((RB2(I)+RB2(I+1)-(RB1(I)**2+RB1(I+1)**2)*RNORM) *
     +              RNORM,0.0)
            IF (LSD) THEN
               RB4(I) = SQRT(RRR)
            ELSE
               RB4(I) = RRR
            ENDIF
            RB4(I+1)=0.
  100    CONTINUE
      ELSE
         DO 110 I=1,LAST
            RRR=MAX((RB2(I)-RB1(I)**2*RNORM)*RNORM,0.0)
            IF (LSD) THEN
               RB4(I) = SQRT(RRR)
            ELSE
               RB4(I) = RRR
            ENDIF
  110    CONTINUE
      ENDIF
C
      IF (SEMROW(2,RB4,INFORM,JOUT,1,LP2)) GOTO 120
      JOUT=JOUT+1
      IF (.NOT.L1D) THEN
C
C Finished?
C
         IF (JOUT.LE.NROW) GOTO 30
      ENDIF
  120 RETURN
C
C Errors
C
  130 IDERR=NOVER
      ERROR=3
      GOTO 120
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
