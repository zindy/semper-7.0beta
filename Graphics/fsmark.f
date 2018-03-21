C Semper 6 system module FSMARK
C
      LOGICAL FUNCTION FSMARK(X,Y,MODE,SIZE)
C
C Marks point at position (X,Y) in graphics coordinates with symbol
C determined by MODE and symbol size determined by SIZE.
C MODE may take the following values:
C    MODE = 1, horizontal/vertical cross (default)
C         = 2, diagonal cross
C         = 3, square
C         = 4, diamond
C         = 5, mark single pixel (SIZE is ignored)
C SIZE specifies the symbol radius as a multiple of display pixels.
C
      REAL X,Y
      INTEGER MODE,SIZE
C
      LOGICAL FSLINE
C
      INCLUDE 'COMMON'
C
      REAL DX,DY,S,X1,X2,Y1,Y2
      INTEGER M
C
      FSMARK=.TRUE.
C
C If MODE is outside range 1 to 5, it defaults to 1
C
      IF (MODE.LT.1.OR.MODE.GT.5) THEN
         M=1
      ELSE
         M=MODE
      ENDIF
C
C If SIZE is less than 1, it defaults to 2
C
      IF (SIZE.LT.1) THEN
         S=2.0
      ELSE
         S=REAL(SIZE)
      ENDIF
C
C Determine X and Y coordinate increments
C
      DX=S/FSXSCA
      DY=S/FSYSCA
C
C Determine extra coordinate values
C
      X1=X-DX
      X2=X+DX
      Y1=Y-DY
      Y2=Y+DY
C
C Output symbol as series of straight lines
C
      IF (M.EQ.1) THEN
         IF (FSLINE(X1,Y,X2,Y)) GOTO 10
         IF (FSLINE(X,Y1,X,Y2)) GOTO 10
      ELSE IF (M.EQ.2) THEN
         IF (FSLINE(X1,Y1,X2,Y2)) GOTO 10
         IF (FSLINE(X1,Y2,X2,Y1)) GOTO 10
      ELSE IF (M.EQ.3) THEN
         IF (FSLINE(X1,Y1,X1,Y2)) GOTO 10
         IF (FSLINE(X1,Y2,X2,Y2)) GOTO 10
         IF (FSLINE(X2,Y2,X2,Y1)) GOTO 10
         IF (FSLINE(X2,Y1,X1,Y1)) GOTO 10
      ELSE IF (M.EQ.4) THEN
         IF (FSLINE(X1,Y,X,Y1)) GOTO 10
         IF (FSLINE(X,Y1,X2,Y)) GOTO 10
         IF (FSLINE(X2,Y,X,Y2)) GOTO 10
         IF (FSLINE(X,Y2,X1,Y)) GOTO 10
      ELSE IF (M.EQ.5) THEN
         IF (FSLINE(X,Y,X,Y)) GOTO 10
      ENDIF
C
      FSMARK=.FALSE.
C
   10 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
