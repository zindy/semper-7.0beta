C Semper subsidiary module EXTRCB
C
      LOGICAL FUNCTION EXTRCB(LPN,N,FORM,LAYER,LBLANK,VALUE)
      REAL VALUE(2)
      INTEGER LPN,N,FORM,LAYER
      LOGICAL LBLANK
C
C Performs general extraction from picture LPN, using bi-cubic
C interpolation.  Positions of pixels must be supplied in the row
C buffers RB3 (for X) and RB4 (for Y), in pixel coordinates.  The
C positions are sorted to determine which rows in LPN have to be
C accessed.  If LBLANK is set to .FALSE., any positions outside the
C source picture are reduced to fall within the source picture by
C taking the remainder after dividing by the picture size, i.e. the
C coordinates 'wrap around'.  If LBLANK is set to .TRUE., any sample
C points that lie outside the source picture return the background
C value(s) passed in array VALUE.  Row buffers RB5 and RB6 are used
C for the purposes of sorting.  The source picture rows are each input
C just once into the row buffer RB1 and the appropriate contributions
C are added to the row buffer RB2.  The number of pixel values to
C extract and the data form for the result in RB2 are specified by N
C and FORM.  The interpolation is always done in floating-point/complex
C and then the result is converted to byte/integer form if needs be.
C The layer number for extraction is given by LAYER, so that extraction
C from a single layer of a multi-layer picture can be done.
C
      LOGICAL SEMROW
C
      REAL DX,DY,DXA,DXB,X,Y,XMAX,YMAX,XFLOOR,YFLOOR,F
      INTEGER IBLANK,I,I1,I2,I3,I4,J,L,M,NCOL,NROW,INFORM
      INTEGER*4 N4
C
      INCLUDE 'COMMON'
C
      REAL RBF(0:LNBUF/LNREAL+1),RBC(-1:LNBUF/LNREAL+2)
      INTEGER IB5(-1:LNBUF/LNINT+1),IB6(LNBUF/LNINT)
C
      EQUIVALENCE (RBF(1),RBC(1),RB1),(IB5,RB5),(IB6,RB6)
C
      EXTRCB=.TRUE.
C
C Fetch source picture size
C
      NCOL=NCOLS(LPN)
      NROW=NROWS(LPN)
      XMAX=REAL(NCOL)
      YMAX=REAL(NROW)
C
C Fault source picture with too many rows to process
C
      IF (NROW.GT.LNBUF/LNINT) THEN
         ERROR=5
         IDERR=1000*DEVN(LPN)+PICN(LPN)
         GOTO 160
      ENDIF
C
C Determine data form for interpolation
C
      IF (FORM.EQ.NFMCOM) THEN
         INFORM=NFMCOM
      ELSE
         INFORM=NFMFP
      ENDIF
C
C Zero source row flags
C
      DO 10 J=1,NROW
         IB5(J)=0
   10 CONTINUE
C
C Zero pointer to blanking list
C
      IBLANK=0
C
C See if blanking is required
C
      IF (LBLANK) THEN
C
C Sort pixel positions that lie inside source picture limits, remainder
C being added to separate list for setting to specified values
C
         DO 20 I=1,N
C
C Fetch next sampling position
C
            X=RB3(I)
            Y=RB4(I)
C
C If position lies outside source picture, add point to blanking list
C
            IF (X.LT.1.0.OR.X.GT.XMAX.OR.Y.LT.1.0.OR.Y.GT.YMAX) THEN
               IB6(I)=IBLANK
               IBLANK=I
C
C Otherwise, add point to sorted row list
C
            ELSE
C
C Determine corresponding source row number
C
               J=INT(Y)
C
C Add pixel to linked list for source row and set source row flag
C
               IB6(I)=IB5(J)
               IB5(J)=I
            ENDIF
   20    CONTINUE
C
C Duplicate flag to simplify subsequent code
C
         IB5(-1)=IB5(1)
         IB5(0)=IB5(1)
         IB5(NROW+1)=IB5(NROW)
C
      ELSE
C
C Sort pixel positions in the Y direction, with wrap-around for points
C outside source picture limits
C
         DO 30 I=1,N
C
C Determine integer position below or level with Y position
C
            YFLOOR=AINT(RB4(I))
            IF (YFLOOR.GT.RB4(I)) YFLOOR=YFLOOR-1.0
C
C Determine corresponding source row number
C
            YFLOOR=YFLOOR-0.5
            IF (YFLOOR.GT.0.0) THEN
               J=1+INT(MOD(YFLOOR,YMAX))
            ELSE
               J=NROW+INT(MOD(YFLOOR,YMAX))
            ENDIF
C
C Add pixel to linked list for source row and set source row flag
C
            IB6(I)=IB5(J)
            IB5(J)=I
   30    CONTINUE
C
C Duplicate flag to simplify subsequent code
C
         IB5(-1)=IB5(NROW-1)
         IB5(0)=IB5(NROW)
         IB5(NROW+1)=IB5(1)
      ENDIF
C
C Zero output pixel values
C
      IF (INFORM.EQ.NFMFP) THEN
         DO 40 I=1,N
            RB2(I)=0.0
   40    CONTINUE
      ELSE
         DO 50 I=1,2*N
            RB2(I)=0.0
   50    CONTINUE
      ENDIF
C
C Process all contributing source rows
C
      DO 140 J=1,NROW
C
C Fetch source row flags
C
         I1=IB5(J+1)
         I2=IB5(J)
         I3=IB5(J-1)
         I4=IB5(J-2)
C
C Read source row and interpolate if either flag is set
         IF (I1.NE.0.OR.I2.NE.0.OR.I3.NE.0.OR.I4.NE.0) THEN
C
C Read source row from LPN
C
            IF (SEMROW(1,RB1,INFORM,J,LAYER,LPN)) GOTO 160
C
C Interpolate pixel values in floating-point form if required
C
            IF (INFORM.EQ.NFMFP) THEN
C
C Duplicate source pixel value to simplify subsequent code
C
               IF (LBLANK) THEN
                  RBF(0)=RBF(1)
                  RBF(NCOL+1)=RBF(NCOL)
                  RBF(NCOL+2)=RBF(NCOL)
               ELSE
                  RBF(0)=RBF(NCOL)
                  RBF(NCOL+1)=RBF(1)
                  RBF(NCOL+2)=RBF(2)
               ENDIF
C
C Add contributions to output pixels two pixels 'above' source row
C
   60          IF (I1.NE.0) THEN
                  XFLOOR=AINT(RB3(I1))
                  IF (XFLOOR.GT.RB3(I1)) XFLOOR=XFLOOR-1.0
C
                  YFLOOR=AINT(RB4(I1))
                  IF (YFLOOR.GT.RB4(I1)) YFLOOR=YFLOOR-1.0
C
                  DX=RB3(I1)-XFLOOR
                  DY=RB4(I1)-YFLOOR
C
                  DXB=DX*DX
                  DXA=DXB*(DX-1.0)
                  DXB=DXA-DXB
C
                  F=DY*(DY*(2.0-DY)-1.0)
C
                  XFLOOR=XFLOOR-0.5
                  IF (XFLOOR.GT.0.0) THEN
                     L=1+INT(MOD(XFLOOR,XMAX))
                  ELSE
                     L=NCOL+INT(MOD(XFLOOR,XMAX))
                  ENDIF
C
                  RB2(I1)=RB2(I1)+F*(DXA      *RBF(L+2)+
     +                               (DX-DXA) *RBF(L+1)+
     +                               (DXB+1.0)*RBF(L  )-
     +                               (DXB+DX) *RBF(L-1))
C
C Process next pixel in linked list
C
                  I1=IB6(I1)
                  GOTO 60
               ENDIF
C
C Add contributions to output pixels directly 'above' source row
C
   70          IF (I2.NE.0) THEN
                  XFLOOR=AINT(RB3(I2))
                  IF (XFLOOR.GT.RB3(I2)) XFLOOR=XFLOOR-1.0
C
                  YFLOOR=AINT(RB4(I2))
                  IF (YFLOOR.GT.RB4(I2)) YFLOOR=YFLOOR-1.0
C
                  DX=RB3(I2)-XFLOOR
                  DY=RB4(I2)-YFLOOR
C
                  DXB=DX*DX
                  DXA=DXB*(DX-1.0)
                  DXB=DXA-DXB
C
                  F=DY*(DY*(DY-2.0))+1.0
C
                  XFLOOR=XFLOOR-0.5
                  IF (XFLOOR.GT.0.0) THEN
                     L=1+INT(MOD(XFLOOR,XMAX))
                  ELSE
                     L=NCOL+INT(MOD(XFLOOR,XMAX))
                  ENDIF
C
                  RB2(I2)=RB2(I2)+F*(DXA      *RBF(L+2)+
     +                               (DX-DXA) *RBF(L+1)+
     +                               (DXB+1.0)*RBF(L  )-
     +                               (DXB+DX) *RBF(L-1))
C
C Process next pixel in linked list
C
                  I2=IB6(I2)
                  GOTO 70
               ENDIF
C
C Add contributions to output pixels directly 'below' source row
C
   80          IF (I3.NE.0) THEN
                  XFLOOR=AINT(RB3(I3))
                  IF (XFLOOR.GT.RB3(I3)) XFLOOR=XFLOOR-1.0
C
                  YFLOOR=AINT(RB4(I3))
                  IF (YFLOOR.GT.RB4(I3)) YFLOOR=YFLOOR-1.0
C
                  DX=RB3(I3)-XFLOOR
                  DY=RB4(I3)-YFLOOR
C
                  DXB=DX*DX
                  DXA=DXB*(DX-1.0)
                  DXB=DXA-DXB
C
                  F=DY*(1.0+DY*(1.0-DY))
C
                  XFLOOR=XFLOOR-0.5
                  IF (XFLOOR.GT.0.0) THEN
                     L=1+INT(MOD(XFLOOR,XMAX))
                  ELSE
                     L=NCOL+INT(MOD(XFLOOR,XMAX))
                  ENDIF
C
                  RB2(I3)=RB2(I3)+F*(DXA      *RBF(L+2)+
     +                               (DX-DXA) *RBF(L+1)+
     +                               (DXB+1.0)*RBF(L  )-
     +                               (DXB+DX) *RBF(L-1))
C
C Process next pixel in linked list
C
                  I3=IB6(I3)
                  GOTO 80
               ENDIF
C
C Add contributions to output pixels two pixels 'below' source row
C
   90          IF (I4.NE.0) THEN
                  XFLOOR=AINT(RB3(I4))
                  IF (XFLOOR.GT.RB3(I4)) XFLOOR=XFLOOR-1.0
C
                  YFLOOR=AINT(RB4(I4))
                  IF (YFLOOR.GT.RB4(I4)) YFLOOR=YFLOOR-1.0
C
                  DX=RB3(I4)-XFLOOR
                  DY=RB4(I4)-YFLOOR
C
                  DXB=DX*DX
                  DXA=DXB*(DX-1.0)
                  DXB=DXA-DXB
C
                  F=DY*(DY*(DY-1.0))
C
                  XFLOOR=XFLOOR-0.5
                  IF (XFLOOR.GT.0.0) THEN
                     L=1+INT(MOD(XFLOOR,XMAX))
                  ELSE
                     L=NCOL+INT(MOD(XFLOOR,XMAX))
                  ENDIF
C
                  RB2(I4)=RB2(I4)+F*(DXA      *RBF(L+2)+
     +                               (DX-DXA) *RBF(L+1)+
     +                               (DXB+1.0)*RBF(L  )-
     +                               (DXB+DX) *RBF(L-1))
C
C Process next pixel in linked list
C
                  I4=IB6(I4)
                  GOTO 90
               ENDIF
C
C Otherwise, interpolate pixel values in complex form
C
            ELSE
C
C Duplicate source pixel value to simplify subsequent code
C
               IF (LBLANK) THEN
                  RBC(-1)=RBC(1)
                  RBC(0 )=RBC(2)
                  RBC(2*NCOL+1)=RBC(2*NCOL-1)
                  RBC(2*NCOL+2)=RBC(2*NCOL)
                  RBC(2*NCOL+3)=RBC(2*NCOL-1)
                  RBC(2*NCOL+4)=RBC(2*NCOL)
               ELSE
                  RBC(-1)=RBC(2*NCOL-1)
                  RBC(0 )=RBC(2*NCOL)
                  RBC(2*NCOL+1)=RBC(1)
                  RBC(2*NCOL+2)=RBC(2)
                  RBC(2*NCOL+3)=RBC(3)
                  RBC(2*NCOL+4)=RBC(4)
               ENDIF
C
C Add contributions to output pixels two pixels 'above' source row
C
  100          IF (I1.NE.0) THEN
                  XFLOOR=AINT(RB3(I1))
                  IF (XFLOOR.GT.RB3(I1)) XFLOOR=XFLOOR-1.0
C
                  YFLOOR=AINT(RB4(I1))
                  IF (YFLOOR.GT.RB4(I1)) YFLOOR=YFLOOR-1.0
C
                  DX=RB3(I1)-XFLOOR
                  DY=RB4(I1)-YFLOOR
C
                  DXB=DX*DX
                  DXA=DXB*(DX-1.0)
                  DXB=DXA-DXB
C
                  F=DY*(DY*(2.0-DY)-1.0)
C
                  XFLOOR=XFLOOR-0.5
                  IF (XFLOOR.GT.0.0) THEN
                     L=1+INT(MOD(XFLOOR,XMAX))
                  ELSE
                     L=NCOL+INT(MOD(XFLOOR,XMAX))
                  ENDIF
C
                  L=2*L-1
                  M=2*I1-1
C
                  RB2( M )=RB2( M )+F*(DXA      *RBC(L+4)+
     +                                 (DX-DXA) *RBC(L+2)+
     +                                 (DXB+1.0)*RBC(L  )-
     +                                 (DXB+DX) *RBC(L-2))
                  RB2(M+1)=RB2(M+1)+F*(DXA      *RBC(L+5)+
     +                                 (DX-DXA) *RBC(L+3)+
     +                                 (DXB+1.0)*RBC(L+1)-
     +                                 (DXB+DX) *RBC(L-1))
C
C Process next pixel in linked list
C
                  I1=IB6(I1)
                  GOTO 100
               ENDIF
C
C Add contributions to output pixels directly 'above' source row
C
  110          IF (I2.NE.0) THEN
                  XFLOOR=AINT(RB3(I2))
                  IF (XFLOOR.GT.RB3(I2)) XFLOOR=XFLOOR-1.0
C
                  YFLOOR=AINT(RB4(I2))
                  IF (YFLOOR.GT.RB4(I2)) YFLOOR=YFLOOR-1.0
C
                  DX=RB3(I2)-XFLOOR
                  DY=RB4(I2)-YFLOOR
C
                  DXB=DX*DX
                  DXA=DXB*(DX-1.0)
                  DXB=DXA-DXB
C
                  F=DY*(DY*(DY-2.0))+1.0
C
                  XFLOOR=XFLOOR-0.5
                  IF (XFLOOR.GT.0.0) THEN
                     L=1+INT(MOD(XFLOOR,XMAX))
                  ELSE
                     L=NCOL+INT(MOD(XFLOOR,XMAX))
                  ENDIF
C
                  L=2*L-1
                  M=2*I2-1
C
                  RB2( M )=RB2( M )+F*(DXA      *RBC(L+4)+
     +                                 (DX-DXA) *RBC(L+2)+
     +                                 (DXB+1.0)*RBC(L  )-
     +                                 (DXB+DX) *RBC(L-2))
                  RB2(M+1)=RB2(M+1)+F*(DXA      *RBC(L+5)+
     +                                 (DX-DXA) *RBC(L+3)+
     +                                 (DXB+1.0)*RBC(L+1)-
     +                                 (DXB+DX) *RBC(L-1))
C
C Process next pixel in linked list
C
                  I2=IB6(I2)
                  GOTO 110
               ENDIF
C
C Add contributions to output pixels directly 'below' source row
C
  120          IF (I3.NE.0) THEN
                  XFLOOR=AINT(RB3(I3))
                  IF (XFLOOR.GT.RB3(I3)) XFLOOR=XFLOOR-1.0
C
                  YFLOOR=AINT(RB4(I3))
                  IF (YFLOOR.GT.RB4(I3)) YFLOOR=YFLOOR-1.0
C
                  DX=RB3(I3)-XFLOOR
                  DY=RB4(I3)-YFLOOR
C
                  DXB=DX*DX
                  DXA=DXB*(DX-1.0)
                  DXB=DXA-DXB
C
                  F=DY*(1.0+DY*(1.0-DY))
C
                  XFLOOR=XFLOOR-0.5
                  IF (XFLOOR.GT.0.0) THEN
                     L=1+INT(MOD(XFLOOR,XMAX))
                  ELSE
                     L=NCOL+INT(MOD(XFLOOR,XMAX))
                  ENDIF
C
                  L=2*L-1
                  M=2*I3-1
C
                  RB2( M )=RB2( M )+F*(DXA      *RBC(L+4)+
     +                                 (DX-DXA) *RBC(L+2)+
     +                                 (DXB+1.0)*RBC(L  )-
     +                                 (DXB+DX) *RBC(L-2))
                  RB2(M+1)=RB2(M+1)+F*(DXA      *RBC(L+5)+
     +                                 (DX-DXA) *RBC(L+3)+
     +                                 (DXB+1.0)*RBC(L+1)-
     +                                 (DXB+DX) *RBC(L-1))
C
C Process next pixel in linked list
C
                  I3=IB6(I3)
                  GOTO 120
               ENDIF
C
C Add contributions to output pixels two pixels 'below' source row
C
  130          IF (I4.NE.0) THEN
                  XFLOOR=AINT(RB3(I4))
                  IF (XFLOOR.GT.RB3(I4)) XFLOOR=XFLOOR-1.0
C
                  YFLOOR=AINT(RB4(I4))
                  IF (YFLOOR.GT.RB4(I4)) YFLOOR=YFLOOR-1.0
C
                  DX=RB3(I4)-XFLOOR
                  DY=RB4(I4)-YFLOOR
C
                  DXB=DX*DX
                  DXA=DXB*(DX-1.0)
                  DXB=DXA-DXB
C
                  F=DY*(DY*(DY-1.0))
C
                  XFLOOR=XFLOOR-0.5
                  IF (XFLOOR.GT.0.0) THEN
                     L=1+INT(MOD(XFLOOR,XMAX))
                  ELSE
                     L=NCOL+INT(MOD(XFLOOR,XMAX))
                  ENDIF
C
                  L=2*L-1
                  M=2*I4-1
C
                  RB2( M )=RB2( M )+F*(DXA      *RBC(L+4)+
     +                                 (DX-DXA) *RBC(L+2)+
     +                                 (DXB+1.0)*RBC(L  )-
     +                                 (DXB+DX) *RBC(L-2))
                  RB2(M+1)=RB2(M+1)+F*(DXA      *RBC(L+5)+
     +                                 (DX-DXA) *RBC(L+3)+
     +                                 (DXB+1.0)*RBC(L+1)-
     +                                 (DXB+DX) *RBC(L-1))
C
C Process next pixel in linked list
C
                  I4=IB6(I4)
                  GOTO 130
               ENDIF
            ENDIF
         ENDIF
  140 CONTINUE
C
C Process pixels in blanking list
C
  150 IF (IBLANK.NE.0) THEN
C
C Set pixel value as specified in VALUE
C
         IF (INFORM.EQ.NFMFP) THEN
            RB2(IBLANK)=VALUE(1)
         ELSE
            RB2(2*IBLANK-1)=VALUE(1)
            RB2(2*IBLANK  )=VALUE(2)
         ENDIF
C
C Process next pixel in linked list
C
         IBLANK=IB6(IBLANK)
         GOTO 150
      ENDIF
C
C Convert data to output form if necessary
C
      IF (INFORM.NE.FORM) THEN
         N4=N
         CALL CFORM(RB2,RB2,INFORM,FORM,N4)
      ENDIF
C
      EXTRCB=.FALSE.
C
  160 RETURN
C
C Copyright (C) 1987,1989,1991:  Synoptics Ltd,  All Rights Reserved
C
      END
