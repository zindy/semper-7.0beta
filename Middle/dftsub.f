C Semper 6 subsidiary module FTLMSQ
C
      SUBROUTINE FTLMSQ(F,N,LN)
C
C Replaces complex array by its [ln] mod squared
C
C [in,out] complex F(1:N): array to be replaced by its [ln] mod squared
C [in]     integer N: size of F
C [in]     logical LN: flag requesting ln mod squared
C Functionally identical with former FT1D3
C
      REAL F(*),V1
      INTEGER I,N,N2
      LOGICAL LN
C
      N2=N+N
C
      IF (LN) THEN
         DO 10 I = 1,N2,2
            V1 = (F(I)*F(I)) + (F(I+1)*F(I+1))
            IF (V1.NE.0.) THEN
               F(I) = ALOG(V1)
            ELSE
               F(I) = 0.
            ENDIF
            F(I+1) = 0.
   10    CONTINUE
      ELSE
         DO 20 I = 1,N2,2
            F(I) = (F(I)*F(I)) + (F(I+1)*F(I+1))
            F(I+1) = 0.
   20    CONTINUE
      ENDIF
C
      RETURN
C
C Copyright (C) 1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C MRDGRV computes digit-reversed form of supplied index
C
      INTEGER FUNCTION MRDGRV(N,B,NB)
C
C [ret] integer mrdgrv: dig-reversed value of n
C [in]  integer n: number to be dig-reversed
C [in]  integer b(nb): base digit array
C [in]  integer nb: length of b
C
C Separates digits D1,D2..DNB of N on basis B(NB),..B(2),B(1)
C (msd at left, lsd at right), and returns value of DNB..,D2,D1
C on basis B(1),B(2),..B(NB)
C
C Reversal is effected digit by digit from the left (msd), as
C illustrated below for digit 3: the digit is extracted from the
C source by division by product b4*b3*b2*b1 (leadm), and then added to
C the output multiplied by b6*b7 (trailm)
C
C                              | leadm = rightward product of bases
C Index breaks down as d7-d6-d5-d4-d3-d2-d1
C                      d1-d2-d3-d4-d5-d6-d7
C                             ^----^ | trailm = rightward product
C
      INTEGER N,NB,B(NB)
      INTEGER LEADM,TRAILM,I,M,NS
C
C Trivial?
C
      IF (NB.EQ.1) THEN
         MRDGRV=N
         GOTO 30
      ENDIF
C
C Initialise
C
      LEADM=1
      DO 10 I=1,NB
         LEADM=LEADM*B(I)
   10 CONTINUE
      TRAILM=1
      MRDGRV=0
      NS=N
C
C Loop over digits
C
      DO 20 I=NB,1,-1
C
C Bump leadm
C
         LEADM=LEADM/B(I)
C
C Extract leading source digit
C
         M=NS/LEADM
         NS=NS-M*LEADM
C
C Add to output
C
         MRDGRV=MRDGRV+M*TRAILM
C
C Bump trailm
C
         TRAILM=TRAILM*B(I)
   20 CONTINUE
C
   30 RETURN
C
C Copyright (C) 1992:  Synoptics Ltd,  All Rights Reserved
C
      END
