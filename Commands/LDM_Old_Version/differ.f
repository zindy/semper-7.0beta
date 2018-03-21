C Semper 6 processing module DIFFER
C
      SUBROUTINE DIFFER
C
C Differentiates LP1 to LP2, in direction indicated by angle.
C Fouriers are multiplied by 2*pi*i*k, and everything else is given
C a simple three-point local difference operator
C
      LOGICAL SEMROW,EVEN
      INTEGER NANGLE,NFROM
      INTEGER NCOL,NROW,INC,I1,I2,I3,I,J,INFORM,IVALPN
      REAL ANGLE,C,S,YFACT,FACT,VAL
C
      INCLUDE 'COMMON'
C
C Packed names
C
      PARAMETER (NANGLE=2167, NFROM=10335)
C
C Fault multi-layer source or output picture
      IF (NLAYS(LP1).NE.1.OR.NLAYS(LP2).NE.1) THEN
         ERROR=62
         IDERR=VERB
         GOTO 100
      ENDIF
C
C Set up control parameters
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      ANGLE=VAL(NANGLE)
      C=COS(ANGLE)
      S=SIN(ANGLE)
C
C For fourier (full of half plane), apply factor = 2.pi.i.k
C
      IF (CLASSN(LP1).EQ.NCLFOU) THEN
         C=TWOPI*C/NCOL
         S=TWOPI*S/NROW
         YFACT=CROWN(LP1)*S
         DO 20 J=1,NROW
            IF (SEMROW(1,RB1,NFMCOM,J,1,LP1)) GOTO 100
            YFACT=YFACT-S
            FACT=YFACT-CCOLN(LP1)*C
            DO 10 I=1,2*NCOL,2
               FACT=FACT+C
               RB2(I)=-FACT*RB1(I+1)
               RB2(I+1)=FACT*RB1(I)
   10       CONTINUE
C
C Store result in LP2
C
            IF (SEMROW(2,RB2,NFMCOM,J,1,LP2)) GOTO 100
   20    CONTINUE
C
C Otherwise, for non-fourier, apply 3 point local difference operator
C
      ELSE
C
C Fault source picture with single column
C
         IF (NCOL.EQ.1) THEN
            ERROR=5
            IDERR=IVALPN(NFROM)
            GOTO 100
         ENDIF
C
C Check for complex picture
C
         IF (FORMN(LP1).EQ.NFMCOM) THEN
            INFORM=NFMCOM
            INC=2
         ELSE
            INFORM=NFMFP
            INC=1
         ENDIF
C
C Set up loop indices
C
         I1=INC*(NCOL-1)
         I2=I1+1
         I3=INC*NCOL
C
C Fetch first picture row
C
         IF (SEMROW(1,RB1,INFORM,1,1,LP1)) GOTO 100
C
C Check for 1-D case
C
         IF (NROW.EQ.1) THEN
            DO 30 I=1,I1
               RB3(I)=RB1(I+INC)-RB1(I)
   30       CONTINUE
            DO 40 I=I2,I3
               RB3(I)=RB3(I-INC)
   40       CONTINUE
C
C Store result in LP2
C
            IF (SEMROW(2,RB3,INFORM,1,1,LP2)) GOTO 100
C
C Otherwise, process 2-D case
C
         ELSE
               EVEN=.TRUE.
               DO 90 J=2,NROW
C
C Check for even or odd picture row to process
                  IF (EVEN) THEN
                     IF (SEMROW(1,RB2,INFORM,J,1,LP1)) GOTO 100
                     DO 60 I=1,I1
                        RB3(I)=C*(RB2(I+INC)-RB2(I))+S*(RB1(I)-RB2(I))
   60                CONTINUE
                  ELSE
                     IF (SEMROW(1,RB1,INFORM,J,1,LP1)) GOTO 100
                     DO 70 I=1,I1
                        RB3(I)=C*(RB1(I+INC)-RB1(I))+S*(RB2(I)-RB1(I))
   70                CONTINUE
                  ENDIF
C
C Result for last column = result for last but one column
C
                  DO 80 I=I2,I3
                     RB3(I)=RB3(I-INC)
   80             CONTINUE
C
C Result for first row = result for second row
C
                  IF (J.EQ.2) THEN
                     IF (SEMROW(2,RB3,INFORM,1,1,LP2)) GOTO 100
                  ENDIF
C
C Store result in LP2
C
                  IF (SEMROW(2,RB3,INFORM,J,1,LP2)) GOTO 100
C
C Update flag to switch row buffers
C
                  EVEN=.NOT.EVEN
   90          CONTINUE
         ENDIF
      ENDIF
C
  100 RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
