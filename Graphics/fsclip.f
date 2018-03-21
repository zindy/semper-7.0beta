C Semper 6 system module FSCLIP
C
      LOGICAL FUNCTION FSCLIP(A,B,ACLIP)
C
C Clips line segment (A(1),B(1)) to (A(2),B(2)) in A direction, given
C pair of limits in ACLIP
C Note: ACLIP(1) must be less than or equal to ACLIP(2)
C
      REAL A(2),B(2),ACLIP(2)
      INTEGER I,J,OUT(2)
C
      FSCLIP=.TRUE.
C
C Determine out-code for each end point in turn:
C   OUT = 0, if within limits
C   OUT = 1, if beyond lower limit ACLIP(1)
C   OUT = 2, if beyond upper limit ACLIP(2)
C
      DO 10 I=1,2
         IF (A(I).LT.ACLIP(1)) THEN
            OUT(I)=1
         ELSE IF (A(I).GT.ACLIP(2)) THEN
            OUT(I)=2
         ELSE
            OUT(I)=0
         ENDIF
   10 CONTINUE
C
C See if end points lie on same side of one or other limit
C
      IF (OUT(1).EQ.OUT(2)) THEN
C
C If first end point is outside limits, entire line is outside limits
C
         IF (OUT(1).NE.0) GOTO 30
C
C Otherwise, line must cross one more limits
C
      ELSE
C
C Examine each end point and clip if necessary
C
         DO 20 I=1,2
C
C See if end point lies outside either limit
C
            J=OUT(I)
            IF (J.NE.0) THEN
C
C Determine clipped end point
C
               B(I)=B(I)+((ACLIP(J)-A(I))/(A(3-I)-A(I)))*(B(3-I)-B(I))
               A(I)=ACLIP(J)
            ENDIF
   20    CONTINUE
      ENDIF
C
      FSCLIP=.FALSE.
C
   30 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
