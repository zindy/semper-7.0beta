C Semper 6 graphics utility rotuinee FSEDGE
C
      LOGICAL FUNCTION FSEDGE(IOP,FRAME,X1,Y1,X2,Y2,XCLIP,YCLIP,JOIN)
C
      INTEGER IOP,FRAME
      REAL    X1,Y1,X2,Y2,XCLIP(2),YCLIP(2)
      LOGICAL JOIN
C
C Clips and then outputs line from (CX(1),CY(1)) to (CX(2),CY(2))
C according to clipping limits in XCLIP and YCLIP.  If IOP = 1, the line
C is drawn and if IOP = 2, the line is undrawn.  If end point of line is
C not clipped and JOIN is .TRUE., the end pixel is drawn again.
C FSEDGE is called by FSBAND and FSDRAG only.
C
      LOGICAL FSCLIP,FSRB61
C
      REAL    CX(2),CY(2)
      INTEGER IX1,IY1,IX2,IY2
      LOGICAL INSIDE
C
      INCLUDE 'COMMON'
C
      FSEDGE=.TRUE.
C
C Set up start and end point of line
C
      CX(1)=X1
      CY(1)=Y1
      CX(2)=X2
      CY(2)=Y2
C
C If option IM specified, add offset between real and imaginary parts
C
      IF (FSI1.EQ.2) THEN
         CX(1)=CX(1)+FSIOFF
         CX(2)=CX(2)+FSIOFF
      ENDIF
C
C See if end point is clipped
C
      INSIDE=.NOT.(CX(2).LT.XCLIP(1).OR.CX(2).GT.XCLIP(2).OR.
     +             CY(2).LT.YCLIP(1).OR.CY(2).GT.YCLIP(2))
C
C Clip line in X
C
      IF (FSCLIP(CX,CY,XCLIP)) GOTO 10
C
C Clip line in Y
C
      IF (FSCLIP(CY,CX,YCLIP)) GOTO 10
C
C Convert start and end points to display coordinates
C
      IX1=NINT(FSXSCA*CX(1)+FSXOFF)
      IY1=NINT(FSYSCA*CY(1)+FSYOFF)
      IX2=NINT(FSXSCA*CX(2)+FSXOFF)
      IY2=NINT(FSYSCA*CY(2)+FSYOFF)
C
C Output line
C
      IF (FSRB61(IOP,FRAME,IX1,IY1,IX2,IY2,ERROR)) GOTO 20
C
C If JOIN is set and end point not clipped, output end pixel again
C
      IF (JOIN.AND.INSIDE) THEN
         IF (FSRB61(IOP,FRAME,IX2,IY2,IX2,IY2,ERROR)) GOTO 20
      ENDIF
C
   10 FSEDGE=.FALSE.
C
   20 RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
