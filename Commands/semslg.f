C SEMSLG - Draw Synoptics logo
C
      SUBROUTINE SEMSLG
C     =================
C
      INCLUDE 'COMMON'
C
C     LOCAL VARIABLES:
C
C     Drawing mode
      INTEGER MODE
C
C     Target display
      INTEGER NDIS
C
C     Factor to scale to fit frame/partiton/drawing
      REAL XSCALE, YSCALE, SCALE
C
C     End points of the line to draw
      REAL X1, X2, Y
C
C     Loop counters
      INTEGER I, J
C
C     Number of vectors and Y values in the logo
      INTEGER NVECT, NYVAL
      PARAMETER (NVECT=141, NYVAL=15)
C
C     Logo data
      INTEGER IXVAL(2,NVECT),IYVAL(NYVAL),IXPTR(NYVAL+1)
C
C     Gets drawing related options
      LOGICAL FSOPTN
C
C     Initialises for drawing
      LOGICAL FSINIT
C
C     Draws a line
      LOGICAL FSLINE
C
C     Flushes output
      LOGICAL FSFLUS
C
      DATA ((IXVAL(I,J),I=1,2),J=1,1)     /  69, 76 /
      DATA ((IXVAL(I,J),I=1,2),J=2,4)     /  65, 80,172,176,182,186 /
      DATA ((IXVAL(I,J),I=1,2),J=5,8)     /  62, 67, 77, 82,172,176,
     +                                      182,186 /
      DATA ((IXVAL(I,J),I=1,2),J=9,12)    /   0, 40, 61, 65, 78, 83,
     +                                      172,176 /
      DATA ((IXVAL(I,J),I=1,2),J=13,25)   /   2, 42, 61, 66, 85, 89,
     +                                       98,102,105,109,112,119,
     +                                      131,141,149,153,156,164,
     +                                      170,179,182,186,194,204,
     +                                      216,224 /
      DATA ((IXVAL(I,J),I=1,2),J=26,36)   /   4, 44, 62, 68, 86, 90,
     +                                       97,101,105,121,129,143,
     +                                      149,166,170,179,182,186,
     +                                      192,206,213,227 /
      DATA ((IXVAL(I,J),I=1,2),J=37,51)   /   6, 46, 65, 81, 87, 91,
     +                                       96,100,105,109,118,123,
     +                                      127,131,141,145,149,154,
     +                                      164,168,172,176,182,186,
     +                                      190,194,203,208,212,216 /
      DATA ((IXVAL(I,J),I=1,2),J=52,65)   /   8, 48, 79, 83, 88, 92,
     +                                       95, 99,105,109,119,123,
     +                                      126,130,142,146,149,153,
     +                                      165,169,172,176,182,186,
     +                                      189,193,213,224 /
      DATA ((IXVAL(I,J),I=1,2),J=66,81)   /   9, 57, 61, 65, 80, 84,
     +                                       89, 93, 94, 98,105,109,
     +                                      119,123,126,130,142,146,
     +                                      149,153,165,169,172,176,
     +                                      182,186,189,193,217,227,
     +                                      232,511 /
      DATA ((IXVAL(I,J),I=1,2),J=82,96)   /   9, 57, 61, 65, 80, 84,
     +                                       90, 97,105,109,119,123,
     +                                      126,130,142,146,149,153,
     +                                      165,169,172,176,182,186,
     +                                      189,193,223,228,232,511 /
      DATA ((IXVAL(I,J),I=1,2),J=97,112)  /   8, 48, 62, 66, 79, 83,
     +                                       91, 96,105,109,119,123,
     +                                      127,132,140,145,149,154,
     +                                      163,168,172,177,182,186,
     +                                      190,195,203,208,211,215,
     +                                      224,228 /
      DATA ((IXVAL(I,J),I=1,2),J=113,123) /   6, 46, 64, 81, 91, 95,
     +                                      105,109,119,123,129,143,
     +                                      149,166,173,179,182,186,
     +                                      192,206,212,227 /
      DATA ((IXVAL(I,J),I=1,2),J=124,135) /   4, 44, 67, 79, 90, 94,
     +                                      105,109,119,123,132,140,
     +                                      149,153,157,163,176,179,
     +                                      182,186,195,203,216,224 /
      DATA ((IXVAL(I,J),I=1,2),J=136,138) /   2, 42, 86, 93,149,153 /
      DATA ((IXVAL(I,J),I=1,2),J=139,141) /   0, 40, 86, 91,149,153 /
C
      DATA IYVAL / 27,25,23,21,19,17,15,13,11,10,8,6,4,2,0 /
C
      DATA IXPTR / 1,2,5,9,13,26,37,52,66,82,97,113,124,136,139,142 /
C
C     Initialise for drawing in the correct mode
C
      IF ( FSOPTN ( MODE, NDIS ) ) GOTO 30
      IF ( FSINIT ( MODE, NDIS ) ) GOTO 30
C
C     Now draw the logo
C
      XSCALE = (FSBRIG - FSBLEF) / 511.0
      YSCALE = (FSBTOP - FSBBOT) / 27.0
C
      IF ( XSCALE .LT. YSCALE ) THEN
         SCALE = XSCALE
      ELSE
         SCALE = YSCALE
      ENDIF
C
      DO 20 J = 1, NYVAL
C
C        Set up the Y value for the line
C
         Y = FSBBOT + REAL ( IYVAL(J) ) * SCALE
C
         DO 10 I = IXPTR(J), IXPTR(J+1)-1
C
C           Set up the X values for the end points of the line
C
            X1 = FSBLEF + REAL ( IXVAL(1,I) ) * SCALE
            X2 = FSBLEF + REAL ( IXVAL(2,I) ) * SCALE
C
C           And draw the line
C
            IF ( FSLINE ( X1, Y, X2, Y ) ) GOTO 30
   10    CONTINUE
   20 CONTINUE
C
C     Flush the output
C
      IF ( FSFLUS (  ) ) GOTO 30
C
C     All done
C
   30 RETURN
C
C Copyright (C) Synoptics 1989, All Rights Reserved.
C
      END
