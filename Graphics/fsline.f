C Semper 6 system module FSLINE
C
      LOGICAL FUNCTION FSLINE(X1,Y1,X2,Y2)
C
C Outputs line on display from start point (X1,Y1) to end point (X2,Y2).
C The transformation from graphics to display coordinates and the
C clipping limits must have been previously set up in COMMON via a call
C to FSINIT. FSINIT will also have determined, in the case of picture
C coordinates being selected, whether output to the real part or the
C imaginary part instead of both parts of a complex display picture has
C been requested by means of one of the options RE or IM.
C
      REAL X1,Y1,X2,Y2
C
      LOGICAL FSCLIP,FSLN61
C
      INCLUDE 'COMMON'
C
      REAL CX(2),CY(2),XCLIP(2),YCLIP(2)
      INTEGER I,IX1,IX2,IY1,IY2,K
C
      EQUIVALENCE (XCLIP,FSXMIN),(YCLIP,FSYMIN)
C
      FSLINE=.TRUE.
C
C Output clipped line once or twice as required
C Note: FSI1,FSI2 = 1,1 = option RE
C                 = 2,2 = option IM
C                 = 1,2 = default
      DO 20 I=FSI1,FSI2
C
C Take copy of positions of end points
C
         CX(1)=X1
         CY(1)=Y1
C
         CX(2)=X2
         CY(2)=Y2
C
C Add offset for imaginary part if required
C
         IF (I.EQ.2) THEN
            CX(1)=CX(1)+FSIOFF
            CX(2)=CX(2)+FSIOFF
         ENDIF
C
C Clip line in X direction
C
         IF (FSCLIP(CX,CY,XCLIP)) GOTO 20
C
C Clip line in Y direction
C
         IF (FSCLIP(CY,CX,YCLIP)) GOTO 20
C
C Convert end points of line to display coordinates
C
         IX1=NINT(FSXSCA*CX(1)+FSXOFF)
         IY1=NINT(FSYSCA*CY(1)+FSYOFF)
         IX2=NINT(FSXSCA*CX(2)+FSXOFF)
         IY2=NINT(FSYSCA*CY(2)+FSYOFF)
C
C Output line to all relevant display frames
C
         IF (OVLIND(FSDEV)) THEN
            DO 10 K=FSFRA,FSFRA2
               IF (FSLN61(IX2,IY2,IX1,IY1,K,ERROR)) GOTO 30
   10       CONTINUE
         ELSE
            IF (FSLN61(IX2,IY2,IX1,IY1,0,ERROR)) GOTO 30
         ENDIF
C
C Set flag to flush graphics buffer at the end of the current command
C
         REQFSF=.TRUE.
   20 CONTINUE
C
      FSLINE=.FALSE.
C
   30 RETURN
C
C Copyright (C) 1987,1988,1989,1990:  Synoptics Ltd, All Rights Reserved
C
      END
