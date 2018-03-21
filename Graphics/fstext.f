C Semper 6 system module FSTEXT
C
      LOGICAL FUNCTION FSTEXT(TEXT,N,X,Y,JX,JY)
C
C Outputs N character text string in array TEXT to the display.
C Position (X,Y) of the text string specifies left edge, centre or right
C edge according to value of JX (<0, 0, >0) and lower edge, centre or
C upper edge of string according to value of JY (<0, 0, >0). The string
C is clipped so that only whole character cells, within the clipping
C limits, are output. The transformation from graphics to display
C coordinates and the clipping limits must have been previously set up
C in COMMON via a call to FSINIT. FSINIT will also have determined, in
C the case of picture coordinates being selected, whether output to the
C real part or the imaginary part instead of both parts of a complex
C display picture has been requested by means of one of the options RE
C or IM.
C
      REAL X,Y
      INTEGER N,JX,JY
      INTEGER TEXT(*)
C
      LOGICAL FSTX61
C
      INCLUDE 'COMMON'
C
      REAL DX,DY,X1,X2,XCEN,XCHSIZ,XL,XR,YCEN,YCHSIZ
      INTEGER I,I1,I2,IXCEN,IYCEN,K,NTEXT
C
      FSTEXT=.TRUE.
C
C Determine character cell size in graphics units
C
      XCHSIZ=REAL(CHSIZ(FSDEV))/ABS(FSXSCA)
      YCHSIZ=REAL(CHSI2(FSDEV))/ABS(FSYSCA)
C
C Determine Y centre position in graphics coordinates
C
      DY=YCHSIZ/2.0
      IF (JY.LT.0) THEN
         YCEN=Y+DY
      ELSE IF (JY.GT.0) THEN
         YCEN=Y-DY
      ELSE
         YCEN=Y
      ENDIF
C
C No text to output if string overlaps Y clipping limits
C Note: Y dimension very slightly reduced to allow for round-off
C
      DY=YCHSIZ/2.01
      IF (YCEN+DY.GT.FSYMAX.OR.YCEN-DY.LT.FSYMIN) GOTO 30
C
C Output text once or twice as required
C Note: FSI1,FSI2 = 1,1 = option RE
C                 = 2,2 = option IM
C                 = 1,2 = default
C
      DO 20 I=FSI1,FSI2
C
C Determine X centre position in graphics coordinates
C
         DX=REAL(N)*XCHSIZ/2.0
         IF (JX.LT.0) THEN
            XCEN=X+DX
         ELSE IF (JX.GT.0) THEN
            XCEN=X-DX
         ELSE
            XCEN=X
         ENDIF
C
C Add offset for imaginary part if required
C
         IF (I.EQ.2) XCEN=XCEN+FSIOFF
C
C No text to output if string outside X clipping limits
C
         X1=XCEN-DX
         X2=XCEN+DX
         IF (X1.GT.FSXMAX.OR.X2.LT.FSXMIN) GOTO 20
C
C Determine first and last character positions within clipping limits
C
         XL=(FSXMIN-X1)/XCHSIZ+1.0
         XR=(FSXMAX-X1)/XCHSIZ
C
C Round leftmost character position to the right
C
         IF (XL.GT.1.0) THEN
            I1=INT(XL)
            IF (REAL(I1).LT.XL) I1=I1+1
         ELSE
            I1=1
         ENDIF
C
C Round rightmost character position to the left
C
         IF (XR.LT.REAL(N)) THEN
            I2=INT(XR)
            IF (REAL(I2).GT.XR) I2=I2-1
         ELSE
            I2=N
         ENDIF
C
C Determine number of characters to output
C
         NTEXT=I2-I1+1
C
C No text to output if character count is less than 1
C
         IF (NTEXT.LT.1) GOTO 20
C
C Determine new X centre position
C
         XCEN=X1+REAL(I1+I2-1)*XCHSIZ/2.0
C
C Convert centre position to display coordinates
C
         IXCEN=NINT(FSXSCA*XCEN+FSXOFF)
         IYCEN=NINT(FSYSCA*YCEN+FSYOFF)
C
C Output text string to all relevant display frames
C
         IF (OVLIND(FSDEV)) THEN
            DO 10 K=FSFRA,FSFRA2
               IF (FSTX61(TEXT(I1),NTEXT,IXCEN,IYCEN,K,ERROR)) GOTO 40
   10       CONTINUE
         ELSE
            IF (FSTX61(TEXT(I1),NTEXT,IXCEN,IYCEN,0,ERROR)) GOTO 40
         ENDIF
C
C Set flag to flush graphics buffer at the end of the current command
C
         REQFSF=.TRUE.
   20 CONTINUE
C
   30 FSTEXT=.FALSE.
C
   40 RETURN
C
C Copyright (C) 1987,1988,1989,1990:  Synoptics Ltd, All Rights Reserved
C
      END
