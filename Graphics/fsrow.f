C Semper 6 system module FSROW
C
      LOGICAL FUNCTION FSROW(IOP,IROW,N,X,Y)
C
      INTEGER IOP,IROW(*),N
      REAL    X,Y
C
C Reads from (IOP = 1) or writes to (IOP = 2) the graphics image plane.
C The data is passed unscaled in array IROW.  N contiguous image pixels
C are accessed from left to right starting at graphics position (X,Y).
C Pixels that fall outside the clipping limits are discarded on output
C and zero values are returned on input.  If the target region is a
C complex display picture, the image data is written to or read from
C the real or imaginary parts of the picture according to the settings
C of the general options RE and IM (default is real part).
C
      LOGICAL FSCLIP,FSRI61,FSRO61
C
      INTEGER I,I1,I2,IX,IY,K,K1,K2
      REAL    CX(2),CY(2),XCLIP(2),DX,KX
C
      INCLUDE 'COMMON'
C
      FSROW=.TRUE.
C
C Do nothing if zero or negative image pixel count
C
      IF (N.LE.0) GOTO 50
C
C Prepare start position for clipping
C
      CX(1)=X
      CY(1)=Y
C
C Prepare end position for clipping
C
      CX(2)=X+REAL(N-1)/FSXSCA
      CY(2)=Y
C
C If imaginary part of complex picture, add necessary offset to clipped
C start position
C
      IF (FSI2.EQ.2) THEN
         CX(1)=CX(1)+FSIOFF
         CX(2)=CX(2)+FSIOFF
      ENDIF
C
C Keep copy of X start position
C
      KX=CX(1)
C
C Prepare X clipping limits
C
      XCLIP(1)=FSXMIN
      XCLIP(2)=FSXMAX
C
C See if target region entirely outside clipping limits
C
      IF (FSCLIP(CX,CY,XCLIP).OR.Y.LT.FSYMIN.OR.Y.GT.FSYMAX) THEN
C
C If reading, return all zeros
C
         IF (IOP.EQ.1) THEN
            DO 10 I=1,N
               IROW(I)=0
   10       CONTINUE
         ENDIF
C
C Otherwise, process clipped target region
C
      ELSE
C
C Calculate row index for first unclipped pixel
C
         DX=1.0+FSXSCA*(CX(1)-KX)
         I1=NINT(DX)
         IF (REAL(I1).LT.DX) I1=I1+1
C
C Calculate row index for last unclipped pixel
C
         DX=1.0+FSXSCA*(CX(2)-KX)
         I2=NINT(DX)
         IF (REAL(I2).GT.DX) I2=I2-1
C
C Transform clipped start position into display coordinates
C
         IX=NINT(FSXSCA*CX(1)+FSXOFF)
         IY=NINT(FSYSCA*CY(1)+FSYOFF)
C
C Read from image plane
C
         IF (IOP.EQ.1) THEN
C
C Pad start of return array with zeros
C
            DO 20 I=1,I1-1
               IROW(I)=0
   20       CONTINUE
C
C Read data from image plane
C
            IF (OVLIND(FSDEV)) THEN
               K=FSFRA
            ELSE
               K=0
            ENDIF
C
            IF (FSRI61(IROW(I1),I2-I1+1,NFMINT,IX,IY,K,0.0,
     +                 REAL(LUTLEN-1),0,ERROR)) GOTO 60
C
C Pad end of return array with zeros
C
            DO 30 I=I2+1,N
               IROW(I)=0
   30       CONTINUE
C
C Write to image plane
C
         ELSE
C
C Set up range of display frames to write to
C
            IF (OVLIND(FSDEV)) THEN
               K1=FSFRA
               K2=FSFRA2
            ELSE
               K1=0
               K2=0
            ENDIF
C
C Write data to image plane
C
            DO 40 K=K1,K2
               IF (FSRO61(IROW,I1,I2,1,NFMINT,IX,IY,K,0.0,
     +                    REAL(LUTLEN-1),0,ERROR)) GOTO 60
   40       CONTINUE
C
C Set flag to flush graphics buffer at the end of the current command
C
            REQFSF=.TRUE.
         ENDIF
      ENDIF
C
   50 FSROW=.FALSE.
C
   60 RETURN
C
C Copyright (C) 1990,1992:  Synoptics Ltd,  All Rights Reserved
C
      END
