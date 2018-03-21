C Semper 6 processing module EDGE
C
      SUBROUTINE EDGE
C
C Provides two standard edge detection operators - by default, a modulus
C gradient operator, and if option ROBERTS is set, the Roberts operator
C (maximum absolute difference between diagonally adjacent pixels).
C
      INTEGER IVALPN
      LOGICAL OPT,SEMROW
C
      INCLUDE 'COMMON'
C
      REAl DX,DY
      INTEGER I,J,K,NCOL,NROW,NLAY
      LOGICAL ROBERT,EVEN
C
C Packed names
C
      INTEGER NROBER,NFROM
      PARAMETER (NROBER=29402, NFROM=10335)
C
C Fetch source picture size
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      NLAY=NLAYS(LP1)
C
C Fault 1-D source picture
      IF (NROW.EQ.1) THEN
         ERROR=5
         IDERR=IVALPN(NFROM)
         GOTO 60
      ENDIF
C
C See if option ROBERTS is set
      ROBERT=OPT(NROBER)
C
C Loop over layers
      DO 50 K=1,NLAY
C
C Read first source row from LP1
      IF (SEMROW(1,RB1,NFMFP,1,K,LP1)) GOTO 60
C
C Process source picture
      EVEN=.FALSE.
      DO 50 J=1,NROW
C
C Determine order in which to use input row buffers
         IF (EVEN) THEN
C
C Read source row from LP1 into first row buffer
            IF (SEMROW(1,RB1,NFMFP,J,K,LP1)) GOTO 60
C
C Apply required edge detection operator
            IF (ROBERT) THEN
               DO 10 I=1,NCOL-1
                  RB3(I)=MAX(ABS(RB1(I+1)-RB2(I)),ABS(RB1(I)-RB2(I+1)))
   10          CONTINUE
            ELSE
               DO 20 I=1,NCOL-1
                  DX=RB1(I+1)-RB1(I)
                  DY=RB2(I)-RB1(I)
                  RB3(I)=SQRT(DX*DX+DY*DY)
   20          CONTINUE
            ENDIF
C
C Otherwise, use row buffers the other way around
         ELSE
C
C Read source row from LP1 into second row buffer
            IF (SEMROW(1,RB2,NFMFP,J,K,LP1)) GOTO 60
C
C Apply required edge detection operator
            IF (ROBERT) THEN
               DO 30 I=1,NCOL-1
                  RB3(I)=MAX(ABS(RB2(I+1)-RB1(I)),ABS(RB2(I)-RB1(I+1)))
   30          CONTINUE
            ELSE
               DO 40 I=1,NCOL-1
                  DX=RB2(I+1)-RB2(I)
                  DY=RB1(I)-RB2(I)
                  RB3(I)=SQRT(DX*DX+DY*DY)
   40          CONTINUE
            ENDIF
         ENDIF
C
C Fill in last value in output buffer
         RB3(NCOL)=ABS(RB2(NCOL)-RB1(NCOL))
C
C Store result in LP2
         IF (SEMROW(2,RB3,NFMFP,J,K,LP2)) GOTO 60
C
C Switch row input row buffers
         EVEN=.NOT.EVEN
   50 CONTINUE
C
   60 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
