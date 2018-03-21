C Semper 6 processing module RFILTR
C
      SUBROUTINE RFILTR
C
C Low or high pass nearest neighbour recursive filter:
C Y(i)=A*Y(i-1)+(1-ABS(A))*X(i) is applied forwards and backwards,
C first to the rows and then to the columns
C
C Edge region of order PI/(1-ABS(A)) in width; not quite symmetric
C
      REAL VAL
      LOGICAL SEMROW
C
      INCLUDE 'COMMON'
C
      REAL A,B,Y
      INTEGER I,J,NCOL,NROW
C
C Packed names
C
      INTEGER NA,NFROM
      PARAMETER (NA=1600, NFROM=10335)
C
C Fault multi-layer source or output picture
C
      IF (NLAYS(LP1).NE.1.OR.NLAYS(LP2).NE.1) THEN
         ERROR=62
         IDERR=VERB
         GOTO 100
      ENDIF
C
C Fetch recursion coefficient
C
      A=VAL(NA)
C
C Fault recursion coefficient outside the range -1.0 to 1.0
C
      IF (ABS(A).GT.1.0) THEN
         ERROR=3
         IDERR=NA
         GOTO 100
      ELSE
         B=1.0-ABS(A)
      ENDIF
C
C Fetch source picture size
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
C
C Carry out downward pass through source picture
C
      DO 60 J=1,NROW
C
C Read source row from LP1
C
         IF (SEMROW(1,RB1,NFMFP,J,1,LP1)) GOTO 100
C
C Set up initial filter output value
C
         IF (A.LT.0.0) THEN
            Y=0.0
         ELSE
            Y=RB1(1)
         ENDIF
C
C Apply filter from left to right
C
         DO 10 I=1,NCOL
            Y=A*Y+B*RB1(I)
            RB1(I)=Y
   10    CONTINUE
C
C Reset filter output value if high-pass filter
C
         IF (A.LT.0.0) Y=0.0
C
C Apply filter from right to left
C
         DO 20 I=NCOL,1,-1
            Y=A*Y+B*RB1(I)
            RB1(I)=Y
   20    CONTINUE
C
C Store result in LP2 and finish if 1-d source picture
C
         IF (NROW.EQ.1) THEN
            IF (SEMROW(2,RB1,NFMFP,1,1,LP2)) GOTO 100
            GOTO 100
         ENDIF
C
C Set up row of initial filter output values
C
         IF (J.EQ.1) THEN
            IF (A.LT.0.0) THEN
               DO 30 I=1,NCOL
                  RB2(I)=0.0
   30          CONTINUE
            ELSE
               DO 40 I=1,NCOL
                  RB2(I)=RB1(I)
   40          CONTINUE
            ENDIF
         ENDIF
C
C Apply filter from top to bottom (for each column in parallel)
C
         DO 50 I=1,NCOL
            RB2(I)=A*RB2(I)+B*RB1(I)
            RB1(I)=RB2(I)
   50    CONTINUE
C
C Store the intermediate results in LP2
C
         IF (SEMROW(2,RB1,NFMFP,J,1,LP2)) GOTO 100
   60 CONTINUE
C
C Reset filter output values if high-pass filter
C
      IF (A.LT.0.0) THEN
         DO 70 I=1,NCOL
            RB2(I)=0.0
   70    CONTINUE
      ENDIF
C
C Carry out final upward pass through source picture
C
      DO 90 J=NROW,1,-1
C
C Read intermediate result from LP2
C
         IF (SEMROW(1,RB1,NFMFP,J,1,LP2)) GOTO 100
C
C Apply filter from bottom to top
C
         DO 80 I=1,NCOL
            RB2(I)=A*RB2(I)+B*RB1(I)
            RB1(I)=RB2(I)
   80    CONTINUE
C
C Store final result in LP2
C
         IF (SEMROW(2,RB1,NFMFP,J,1,LP2)) GOTO 100
   90 CONTINUE
C
  100 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
