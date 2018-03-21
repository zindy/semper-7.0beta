C Semper 6 system module FSARRO
C
      LOGICAL FUNCTION FSARRO(X1,Y1,X2,Y2)
C
C Draw arrow from (X1,Y1) to (X2,Y2) on currently opened display
C partition.  Arrow head dimensions are 1/8 by 1/16 of arrow length.
C
      REAL X1,Y1,X2,Y2
C
      LOGICAL FSLINE
C
      REAL DX,DY,XA,YA,XB,YB
C
      FSARRO=.TRUE.
C
C Draw main part of arrow
C
      IF (FSLINE(X1,Y1,X2,Y2)) GOTO 10
C
C Draw arrow head
C
      DX=X2-X1
      DY=Y2-Y1
C
      XA=X2-DX/8.0
      YA=Y2-DY/8.0
C
      XB=-DY/16.0
      YB=DX/16.0
C
      IF (FSLINE(X2,Y2,XA+XB,YA+YB)) GOTO 10
      IF (FSLINE(X2,Y2,XA-XB,YA-YB)) GOTO 10
C
      FSARRO=.FALSE.
C
   10 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
