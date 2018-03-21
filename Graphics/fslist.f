C Semper 6 system module FSLIST
C
      LOGICAL FUNCTION FSLIST(X,Y,N,MODE,SIZE)
C
C Marks N points with the symbol determined by MODE and symbol size
C determined by SIZE.  The arrays X and Y contains the positioins in
C graphics coordinates of the points to be marked.
C
      INTEGER N,MODE,SIZE
      REAL X(*),Y(*)
C
      LOGICAL FSMARK
C
      INTEGER I
C
      FSLIST=.TRUE.
C
C Mark eack point in turn
C
      DO 10 I=1,N
         IF (FSMARK(X(I),Y(I),MODE,SIZE)) GOTO 20
   10 CONTINUE
C
      FSLIST=.FALSE.
C
   20 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
