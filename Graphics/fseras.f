C Semper 6 system module FSERAS
C
      LOGICAL FUNCTION FSERAS(OPC,XMIN,XMAX,YMIN,YMAX)
C
C Erases rectangular area XMIN to XMAX and YMIN to YMAX in graphics
C coordinates.  OPC specifies whether the erasing is to be done in the
C image plane, the overlay plane or both.  The rectangular area is
C clipped. The transformation from graphics to display coordinates and
C the clipping limits must have been previously set up in COMMON via a
C call to FSINIT. FSINIT will also have determined, in the case of
C picture coordinates being selected, whether output to the real part
C or the imaginary part instead of both parts of a complex display
C picture has been requested by means of one of the options RE or IM.
C All the layers of a multi-layer display picture are erased.
C
      INTEGER OPC
      REAL XMIN,XMAX,YMIN,YMAX
C
      LOGICAL FSER61
C
      INCLUDE 'COMMON'
C
      REAL X1,X2,Y1,Y2
      INTEGER I,IX,IX1,IX2,IY,IY1,IY2,K,NX,NY
C
      FSERAS=.TRUE.
C
C Erase rectangular area once or twice as required
C Note: FSI1,FSI2 = 1,1 = option RE
C                 = 2,2 = option IM
C                 = 1,2 = default
C
      DO 60 I=FSI1,FSI2
C
C Sort out X and Y rectangle limits
C
         X1=MIN(XMIN,XMAX)
         X2=MAX(XMIN,XMAX)
C
         Y1=MIN(YMIN,YMAX)
         Y2=MAX(YMIN,YMAX)
C
C Add offset for imaginary part if required
C
         IF (I.EQ.2) THEN
            X1=X1+FSIOFF
            X2=X2+FSIOFF
         ENDIF
C
C No erasing to be done if rectangle completely outside clipping limits
C
         IF (X1.GT.FSXMAX.OR.X2.LT.FSXMIN.OR.
     +       Y1.GT.FSYMAX.OR.Y2.LT.FSYMIN) GOTO 60
C
C Clip X and Y rectangle limits
C
         X1=MAX(X1,FSXMIN)
         X2=MIN(X2,FSXMAX)
C
         Y1=MAX(Y1,FSYMIN)
         Y2=MIN(Y2,FSYMAX)
C
C Convert rectangle limits to display coordinates
C
         IX1=NINT(FSXSCA*X1+FSXOFF)
         IX2=NINT(FSXSCA*X2+FSXOFF)
         IY1=NINT(FSYSCA*Y1+FSYOFF)
         IY2=NINT(FSYSCA*Y2+FSYOFF)
C
C Determine resulting size of area to erase
C
         NX=1+ABS(IX2-IX1)
         NY=1+ABS(IY2-IY1)
C
C Determine top left corner position of area
C
         IX=MIN(IX1,IX2)
         IY=MIN(IY1,IY2)
C
C Erase rectangular area (including all layers of multi-layer picture)
C
         IF (OVLIND(FSDEV)) THEN
            DO 20 K=FSFRA,FSFRA2
               IF (FSER61(OPC,NX,NY,IX,IY,K,ERROR)) GOTO 70
   20       CONTINUE
         ELSE
            IF (OPC .EQ. 1) THEN
               DO 30 K=FSFRA,FSFRA2
                  IF (FSER61(1,NX,NY,IX,IY,K,ERROR)) GOTO 70
   30          CONTINUE
            ELSE IF (OPC .EQ. 2) THEN
               IF (FSER61(2,NX,NY,IX,IY,0,ERROR)) GOTO 70
            ELSE IF (OPC .EQ. 3) THEN
               IF (FSER61(3,NX,NY,IX,IY,FSFRA,ERROR)) GOTO 70
               DO 40 K=FSFRA+1,FSFRA2
                  IF (FSER61(1,NX,NY,IX,IY,K,ERROR)) GOTO 70
   40          CONTINUE
            ENDIF
         ENDIF
C
C Set flag to flush graphics buffer at the end of the current command
C
C  50    REQFSF=.TRUE.
         REQFSF=.TRUE.
   60 CONTINUE
C
      FSERAS=.FALSE.
C
   70 RETURN
C
C Copyright (C) 1987-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
