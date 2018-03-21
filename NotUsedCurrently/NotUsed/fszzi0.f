C Semper 6 system module FSZZI0
C
      SUBROUTINE FSZZI0(X,N)
C
C Fills integer array X(i), i=1,N with zeros
C
      INTEGER X(*),N
C
      INTEGER I
C
      DO 10 I=1,N
         X(I)=0
   10 CONTINUE
C
      RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
