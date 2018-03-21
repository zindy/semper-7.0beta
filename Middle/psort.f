C Semper 6 subsidiary module PSORT
C
      SUBROUTINE PSORT(VALUE,LIST,N,DESCEN)
C
C Provides a list-directed insertion sort.  Array LIST contains N
C pointers to floating-point values in array VALUE.  The pointers are
C re-ordered so that they point to increasing values in array VALUE.
C The sort is done in place using a straightforward insertion sort
C which is very efficient for sorting a small number of values.
C If DESCEN is .TRUE., pointers are arranged to point to decreasing
C values.  PSORT is called by several processing routines in the
C Particle Analysis package.
C
      REAL    VALUE(*)
      INTEGER LIST(*),N
C
      LOGICAL DESCEN
C
      REAL V
      INTEGER I,J,L
C
      DO 20 I=2,N
C
         L = LIST(I)
         V = VALUE(L)
C
         DO 10 J=I-1,1,-1
C
            IF (DESCEN) THEN
C
               IF (V.GT.VALUE(LIST(J))) THEN
                  LIST(J+1)=LIST(J)
               ELSE
                  LIST(J+1)=L
                  GOTO 20
               ENDIF
C
            ELSE
C
               IF (V.LT.VALUE(LIST(J))) THEN
                  LIST(J+1)=LIST(J)
               ELSE
                  LIST(J+1)=L
                  GOTO 20
               ENDIF
C
            ENDIF
C
   10    CONTINUE
C
         LIST(1)=L
   20 CONTINUE
C
      RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
