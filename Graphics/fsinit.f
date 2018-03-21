C Semper 6 system module FSINIT
C
      LOGICAL FUNCTION FSINIT(OPC,N)
C
C Sets up display graphics routines for output with respect to one of
C three coordinates systems, according to the value of OPC:
C    OPC = 1, frame coordinates, where N = frame number
C    OPC = 2, partition coordinates, where N = device:partition number
C    OPC = 3, picture coordinates, where N = device:picture number
C Note that some of the DPD variables in COMMON are used temporarily for
C setting up data values before they are stored elsewhere in COMMON.
C All of the data required by the graphics routines are stored in COMMON
C /SEMGDB/.  This consists of:
C
C    FSDEV,FSFRA,FSFRA2 - device number, display frame number and last
C                         frame number (for multi-layer display picture)
C
C    FSLUT - default look-up table number
C
C    FSPAR,FSPTYP - associated partition number and display picture type
C
C    FSXSCA,FSYSCA,FSXOFF,FSYOFF - transformation from graphics to
C                                  display coordinates
C
C    FSXMIN,FSXMAX,FSYMIN,FSYMAX - clipping limits in graphics
C                                  coordinates
C
C    FSI1,FSI2,FSIOFF - loop indices and offset for imaginary part to
C                       control output to real/imaginary parts of
C                       complex display picture, depending on options
C                       RE and IM
C
C    FSBLEF,FSBRIG,FSBBOT,FSBTOP - limits of frame/partition/picture
C                                  border in graphics coordinates
C
C    FSXPAN,FSYPAN - default view centre position = centre of border
C                    in graphics coordinates
C
C    FSMMOD,FSMSIZ - integer values returned for Semper variables MKMODE
C                    and MKSIZE
C
C Graphics coordinates (xg,yg) are converted to display coordinates
C (xd,yd) as follows:
C    xd = FSXSCA * xg + FSXOFF
C    yd = FSYSCA * yg + FSYOFF
C
      INTEGER OPC,N
C
      INTEGER IVAL
      LOGICAL SEMMED,SEMDPN,SEMDPD,OPT,CONOPT
C
      INTEGER DEVPAR,IXSIZE,IYSIZE,MEDIUM
      REAL X1,X2,Y1,Y2
C
      INCLUDE 'COMMON'
C
      INTEGER NRE,NIM,NMKMOD,NMKSIZ
      PARAMETER (NRE=29000,NIM=14920,NMKMOD=21253,NMKSIZ=21259)
C
      FSINIT=.TRUE.
C
C If OPC is set to 1, set up graphics for output in frame coordinates
C
      IF (OPC.EQ.1) THEN
C
C Establish display device number - from variable FS
C
         FSDEV=INT(FS)
C
C Check medium number for device
C
         IF (SEMMED(FSDEV,MEDIUM)) GOTO 10
C
C Fault non-display device
C
         IF (MEDIUM.NE.MEDDS) THEN
            ERROR=71
            IDERR=FSDEV
            GOTO 10
         ENDIF
C
C Check frame number
C
         IF (N.LT.1.OR.N.GT.NFRS(FSDEV)) THEN
            ERROR=75
            IDERR=N
            GOTO 10
         ELSE
            FSFRA=N
         ENDIF
C
C Last frame number = actual frame number
C
         FSFRA2=FSFRA
C
C Store default look-up table number = current look-up table number
C
         FSLUT=INT(CLUT)
C
C Set up associated display partition number (none)
C
         FSPAR=0
C
C Set up display picture type = 0 (not display picture)
C
         FSPTYP=0
C
C Size of graphics area is frame size for specified display device
C
         IXSIZE=FRSIZ(FSDEV)
         IYSIZE=FRSI2(FSDEV)
C
C Set up transformation from frame to display coordinates
C Note: frame coordinates = X left, Y up and origin at frame centre
C
         FSXSCA=1.0
         FSYSCA=-1.0
         FSXOFF=REAL(IXSIZE/2)
         FSYOFF=REAL(IYSIZE/2)
C
C Set up clipping limits in graphics coordinates = frame limits
C
         FSXMIN=-REAL(IXSIZE/2)
         FSXMAX=REAL((IXSIZE-1)/2)
         FSYMIN=-REAL((IYSIZE-1)/2)
         FSYMAX=REAL(IYSIZE/2)
C
C Output to imaginary part of display picture is not relevant
C
         FSI1=1
         FSI2=1
         FSIOFF=0.0
C
C Set up limits of frame border = clipping limits
C
         FSBLEF=FSXMIN
         FSBRIG=FSXMAX
         FSBBOT=FSYMIN
         FSBTOP=FSYMAX
C
C Otherwise, set up graphics for output in partition or picture
C coordinates
C
      ELSE
C
C Check device:partition/picture number
C
         IF (SEMDPN(N,FSDEV,FSPAR)) GOTO 10
C
         DEVPAR=1000*FSDEV+FSPAR
C
C Read the DPD from the work disc
C
         IF (SEMDPD(1,FSPAR)) GOTO 10
C
C Store range of frame numbers
C
         FSFRA=DPFRA
         FSFRA2=DPFRA2
C
C Store default look-up table number
C
         FSLUT=DPLUT
C
C Store display picture type (0 = not display picture)
C
         FSPTYP=DPTYP
C
C If OPC is set to 2, set up graphics for output in partition
C coordinates
C
         IF (OPC.EQ.2) THEN
C
C Fault non-existent display partition
C
            IF (DPTYP.LT.0) THEN
               ERROR=53
               IDERR=DEVPAR
               GOTO 10
            ENDIF
C
C Set up transformation from partition to display coordinates
C Note: partition coordinates = X left, Y up and origin at partition
C centre
C
            FSXSCA=1.0
            FSYSCA=-1.0
            FSXOFF=REAL(DPTLX+DPSIZ/2)
            FSYOFF=REAL(DPTLY+DPSI2/2)
C
C Set up clipping limits in graphics coordinates = partition limits
C
            FSXMIN=-REAL(DPSIZ/2)
            FSXMAX=REAL((DPSIZ-1)/2)
            FSYMIN=-REAL((DPSI2-1)/2)
            FSYMAX=REAL(DPSI2/2)
C
C Output to imaginary part of display picture is not relevant
C
            FSI1=1
            FSI2=1
            FSIOFF=0.0
C
C Set up limits of partition border = clipping limits
C
            FSBLEF=FSXMIN
            FSBRIG=FSXMAX
            FSBBOT=FSYMIN
            FSBTOP=FSYMAX
C
C Otherwise, set up graphics for output in picture coordinates
C
         ELSE
C
C Fault non-existent display picture
C
            IF (DPTYP.LT.1) THEN
               ERROR=30
               IDERR=DEVPAR
               GOTO 10
            ENDIF
C
C Fault display partition type if display picture not image, graph or
C histogram (types 1,2 or 3)
C
            IF (DPTYP.GT.3) THEN
               ERROR=48
               IDERR=DEVPAR
               GOTO 10
            ENDIF
C
C Set up transformation from picture to display coordinates
C Note: picture coordinates = X left, Y up and origin at picture origin
C
            FSXSCA=DPMA
            FSYSCA=DPMA2
            FSXOFF=DPMB
            FSYOFF=DPMB2
C
C Set up clipping limits in graphics coordinates = limits of display
C partition containing picture (beware of transformation that inverts
C either coordinate axis)
C
            X1=(REAL(DPTLX)-FSXOFF)/FSXSCA
            X2=(REAL(DPTLX+DPSIZ-1)-FSXOFF)/FSXSCA
            Y1=(REAL(DPTLY+DPSI2-1)-FSYOFF)/FSYSCA
            Y2=(REAL(DPTLY)-FSYOFF)/FSYSCA
C
            FSXMIN=MIN(X1,X2)
            FSXMAX=MAX(X1,X2)
            FSYMIN=MIN(Y1,Y2)
            FSYMAX=MAX(Y1,Y2)
C
C If display picture not complex (zero imaginary offset), set loop
C indices for output to real part only
C
            IF (DPIMO.EQ.0) THEN
               FSI1=1
               FSI2=1
C
C Otherwise, set loop indices according to option RE and IM
C
            ELSE
C
C Fault conflict between options RE and IM
C
               IF (CONOPT(NRE,NIM)) GOTO 10
C
C Determine and store loop indices
C Note: FSI1,FSI2 = 1,1 = option RE
C                 = 2,2 = option IM
C                 = 1,2 = default
C
               IF (OPT(NIM)) THEN
                  FSI1=2
               ELSE
                  FSI1=1
               ENDIF
C
               IF (OPT(NRE)) THEN
                  FSI2=1
               ELSE
                  FSI2=2
               ENDIF
            ENDIF
C
C Store offset for imaginary part of complex display picture (if any)
C
            FSIOFF=REAL(DPIMO)/FSXSCA
C
C Set up limits of picture border (truncating to clipping limits)
C
            FSBLEF=MAX(MIN(DPLEF,DPRIG),FSXMIN)
            FSBRIG=MIN(MAX(DPLEF,DPRIG),FSXMAX)
            FSBBOT=MAX(MIN(DPBOT,DPTOP),FSYMIN)
            FSBTOP=MIN(MAX(DPBOT,DPTOP),FSYMAX)
         ENDIF
      ENDIF
C
C Set up default view centre position = centre of border limits.
C Centre position for complex pictures depends on options RE and IM,
C i.e. centred on real/imaginary part or combination of both, if both
C options absent.
C
      FSXPAN=(FSBLEF+FSBRIG+REAL(FSI1+FSI2-2)*FSIOFF)/2.0
      FSYPAN=(FSBBOT+FSBTOP)/2.0
C
C Store integer values returned for Semper variables MKMODE and MKSIZE
C
      FSMMOD=IVAL(NMKMOD)
      FSMSIZ=IVAL(NMKSIZ)
C
      FSINIT=.FALSE.
C
   10 RETURN
C
C Copyright (C) 1987,1989,1990:  Synoptics Ltd,  All Rights Reserved
C
      END
