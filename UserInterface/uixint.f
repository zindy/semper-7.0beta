C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION UIXINT ( XP1,YP1, XS1,YS1, XP2,YP2, XS2,YS2 )
C       --------------------------------------------------------------
C
C       PARAMETERS:
C
C       integer xp1, yp1, xs1, ys1 : INPUT - the top left position and
C                                            size of one rectangle
C
C       integer xp2, yp2, xs2, ys2 : INPUT - the top left position and
C                                            size of the other rectangle
C
C       Checks two rectangles for intersection.
C
C       Function returns TRUE if rectangles intersect, otherwise FALSE.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION UIXINT ( XP1,YP1,XS1,YS1,XP2,YP2,XS2,YS2 )
C     -----------------------------------------------------------
C
      INTEGER XP1, YP1, XS1, YS1
      INTEGER XP2, YP2, XS2, YS2
C
C     LOCAL VARIABLES:
C
C     Opposite corners of rectangles to top left positions given.
C
      INTEGER XE1, YE1, XE2, YE2
C
C     Work out opposite corner position of rectangles
C
      XE1 = XP1 + XS1 - 1
      YE1 = YP1 + YS1 - 1
      XE2 = XP2 + XS2 - 1
      YE2 = YP2 + YS2 - 1
C
C     Check the rectangles
C
      IF ( (XP2 .LE. XE1 .AND. XE2 .GE. XP1) .AND.
     +     (YP2 .LE. YE1 .AND. YE2 .GE. YP1) ) THEN
         UIXINT = .TRUE.
      ELSE IF ( (XP1 .LE. XE2 .AND. XE1 .GE. XP2) .AND.
     +     (YP1 .LE. YE2 .AND. YE1 .GE. YP2) ) THEN
         UIXINT = .TRUE.
      ELSE
         UIXINT = .FALSE.
      ENDIF
C
      RETURN
C
C Copyright (C) 1988-1991:  Synoptics Ltd,  All Rights Reserved
C
      END
