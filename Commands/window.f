C Semper 6 processing module WINDOW
C
      SUBROUTINE WINDOW
C
C Zeros pixels except near lattice sites defined by lattice vectors
C (U,U2) and (V,V2).  Proximity to lattice sites determined by WIDTH
C key.  If RADIUS key is set, pixels beyond this distance from the
C picture origin are also set to zero.  The lattice origin may be
C offset from the picture centre by means of the keys POSITION and
C PO2.  If the second lattice vector (V,V2) is absent, strip windows
C perpendicular to first vector (U,U2) are applied.
C
      LOGICAL VARSET,SEMROW,OPT,SEMCON
      REAL VAL
C
      INCLUDE 'COMMON'
C
      INTEGER NRADIU,NWIDTH,NPOS,NPO2,NU,NU2,NV,NV2,NVERIF
      PARAMETER (NRADIU=28844, NWIDTH=-5165, NPOS=26219, NPO2=26232)
      PARAMETER (NU=-1601, NU2=-2881, NV=-3201, NV2=-4481)
      PARAMETER (NVERIF=-3419)
C
      LOGICAL UONLY,RADIUS
      INTEGER NCOL,NROW
      REAL XCEN,YCEN,XMIN,XMAX,X,Y,DX,D,WIDTH,R
      INTEGER IXMIN,IXMAX
      REAL U,U1,U2,V,V1,V2,UX,UY,VX,VY
      INTEGER I,I1,I2,J
C
C Fault multi-layer source or output picture
      IF (NLAYS(LP1).NE.1.OR.NLAYS(LP2).NE.1) THEN
         ERROR=62
         IDERR=VERB
         RETURN
      ENDIF
C
C Fetch source picture size
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
C
C Determine lattice origin
      XCEN=REAL(CCOLN(LP1))+VAL(NPOS)
      YCEN=REAL(CROWN(LP1))-VAL(NPO2)
C
C Fetch window size (in lattice units)
      WIDTH=VAL(NWIDTH)/2.0
C
C See if RADIUS key is set
      RADIUS=VARSET(NRADIU)
C
C If RADIUS key is set, process it
      IF (RADIUS) THEN
C
C Fault zero or negative value for RADIUS
         R=VAL(NRADIU)
         IF (R.LE.0.0) THEN
            ERROR=3
            IDERR=NRADIU
            RETURN
         ENDIF
      ENDIF
C
C Fetch lattice vectors
      U1=VAL(NU)
      U2=VAL(NU2)
      V1=VAL(NV)
      V2=VAL(NV2)
C
C See if second lattice vector is undefined, and if so,
C set it to first lattice vector rotated by 90 degrees
      UONLY=V1*V1+V2*V2.EQ.0.0
      IF (UONLY) THEN
         V1=-U2
         V2=U1
      ENDIF
C
C Fault parallel lattice vectors
      D=V1*U2-U1*V2
      IF (D.EQ.0.0) THEN
         ERROR=3
         IDERR=NU
         RETURN
      ENDIF
C
C Determine unit X and Y vectors in lattice coordinates
      UX=-V2/D
      UY=V1/D
      VX=U2/D
      VY=-U1/D
C
C Process source picture
      DO 70 J=1,NROW
C
C Determine Y picture coordinate
         Y=YCEN-REAL(J)
C
C Check for RADIUS key being set
         IF (RADIUS) THEN
C
C Check for row outside RADIUS
            IF (ABS(Y).GT.R) THEN
C
C Zero entire row
               DO 10 I=1,2*NCOL
                  RB1(I)=0.0
   10          CONTINUE
               GOTO 60
C
C Otherwise, determine extent of row within RADIUS
            ELSE
               DX=SQRT(R*R-Y*Y)
               XMIN=MAX(1.0,XCEN-DX)
               XMAX=MIN(XCEN+DX,REAL(NCOL))
C
C Round up lower limit to nearest pixel
               IXMIN=INT(XMIN)
               IF (REAL(IXMIN).LT.XMIN) IXMIN=IXMIN+1
C
C Round down upper limit to nearest pixel
               IXMAX=INT(XMAX)
               IF (REAL(IXMAX).GT.XMAX) IXMAX=IXMAX-1
            ENDIF
C
C Otherwise, window entire row
         ELSE
            IXMIN=1
            IXMAX=NCOL
         ENDIF
C
C Read source row from LP1
         IF (SEMROW(1,RB1,NFMCOM,J,1,LP1)) RETURN
C
C Determine initial lattice coordinates
         X=REAL(IXMIN-1)-XCEN
         U=UX*X+UY*Y
         V=VX*X+VY*Y
C
C Determine range of row buffer addresses to window
         I1=2*IXMIN-1
         I2=2*IXMAX
C
C Zero pixels to left of region of interest
         DO 20 I=1,I1-1
            RB1(I)=0.0
   20    CONTINUE
C
C Window pixels in region of interest
         DO 40 I=I1,I2,2
C
C See if pixel is outside window round nearest lattice point
            U=U+UX
            V=V+VX
            IF (ABS(U-ANINT(U)).GT.WIDTH) GOTO 30
            IF (UONLY) GOTO 40
            IF (ABS(V-ANINT(V)).GT.WIDTH) GOTO 30
            GOTO 40
C
C Set this pixel to zero
   30       RB1(I)=0.0
            RB1(I+1)=0.0
   40    CONTINUE
C
C Zero pixels to right of region of interest
         DO 50 I=I2+1,2*NCOL
            RB1(I)=0.0
   50    CONTINUE
C
C Store result in LP2
   60    IF (SEMROW(2,RB1,NFMCOM,J,1,LP2)) RETURN
   70 CONTINUE
C
C If VERIFY option is set, print lattice vectors
      IF (OPT(NVERIF)) THEN
         WRITE (RECORD,80) U1,U2,V1,V2
         IF (SEMCON(RECORD)) RETURN
      ENDIF
C
      RETURN
C
   80 FORMAT('Lattice vectors',2(' (',F7.2,',',F7.2,')'))
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
