C Semper 6 processing module PARTIT
C
      SUBROUTINE PARTIT
C
C Creates and deletes display partitions.  Implements verb PARTITION.
C If option DELETE is set, the specified display partition is deleted.
C Otherwise, a new partition is created.  In either case, any picture
C associated with the partition is deleted.  The size and location of
C a new partition is specified by means of the 2-D sub-region keys and
C options working in frame coordinates and with respect to the frame
C limits.
C
      INTEGER IVAL
      LOGICAL OPT,SEMLU,SEMDPN,SEMDEL,SEMDPD,FSINIT,FSREGN
C
      INCLUDE 'COMMON'
C
      REAL XMIN,XMAX,YMIN,YMAX
      INTEGER DEVICE,DEVPAR,FRAME,FR2,LUT,PARTN
C
C Packed names
C
      INTEGER NNUMBE,NLUT,NFRAME,NFR2,NDELET
      INTEGER NENQUI,NPSIZE,NPS2,NPPOSI,NPP2,NPFRAM,NPF2
      PARAMETER (NNUMBE=23253, NLUT=20060, NFRAME=10321, NFR2=10352)
      PARAMETER (NDELET=6612, NENQUI=8577, NPSIZE=26369, NPS2=26392)
      PARAMETER (NPPOSI=26255, NPP2=26272, NPFRAM=25858, NPF2=25872)
C
C Extract and verify device and partition number specified by key NUMBER
C
      IF (SEMDPN(IVAL(NNUMBE),DEVICE,PARTN)) GOTO 10
C
      DEVPAR=1000*DEVICE+PARTN
C
C If option ENQUIRE is set, return partition size, position and frames
C in variables PSIZE, PS2, PPOSITION, PP2, PFRAME and PF2
      IF (OPT(NENQUI)) THEN
C
C Fetch DPD from work disc
C
         IF (SEMDPD(1,PARTN)) GOTO 10
C
C Set variables
C
         IF (SEMLU(1,NPSIZE,REAL(DPSIZ))) GOTO 10
         IF (SEMLU(1,NPS2,REAL(DPSI2))) GOTO 10
         IF (SEMLU(1,NPPOSI,REAL(DPTLX+DPSIZ/2-FRSIZ(DPDEV)/2))) GOTO 10
         IF (SEMLU(1,NPP2,REAL(FRSI2(DPDEV)/2-(DPTLY+DPSI2/2)))) GOTO 10
         IF (SEMLU(1,NPFRAM,REAL(DPFRA))) GOTO 10
         IF (SEMLU(1,NPF2,REAL(DPFRA2))) GOTO 10
         GOTO 10
      ENDIF
C
C Delete any associated picture
C
      IF (SEMDEL(1,DEVPAR)) GOTO 10
C
C If option DELETE is set, mark partition as undefined
C
      IF (OPT(NDELET)) THEN
         DPTYP=-1
C
C Otherwise, create new display partition
C
      ELSE
C
C Fetch value of FRAME key
C
         FRAME=IVAL(NFRAME)
C
C Fault illegal frame number
C
         IF (FRAME.LT.1.OR.FRAME.GT.NFRS(DEVICE)) THEN
            ERROR=75
            IDERR=FRAME
            GOTO 10
         ENDIF
C
C Fetch value of FR2 key
C
         FR2=IVAL(NFR2)
C
C Fault illegal frame number
C
         IF (FR2.LT.FRAME.OR.FR2.GT.NFRS(DEVICE)) THEN
            ERROR=75
            IDERR=FR2
            GOTO 10
         ENDIF
C
C Set up graphics coordinates = frame coordinates
C
         IF (FSINIT(1,FRAME)) GOTO 10
C
C Process 2-D sub-region keys and options specified with respect to
C current graphics coordinates (frame coordinates)
C
         IF (FSREGN(XMIN,XMAX,YMIN,YMAX)) GOTO 10
C
C Fetch value for key LUT
C
         LUT=IVAL(NLUT)
C
C Fault illegal look-up table number
C
         IF (LUT.LT.1.OR.LUT.GT.NLUTS) THEN
            ERROR=68
            IDERR=LUT
            GOTO 10
         ENDIF
C
C Set up new display partition parameters in DPD
C
         DPNUM=PARTN
         DPDEV=DEVICE
         DPFRA=FRAME
         DPFRA2=FR2
         DPLUT=LUT
         DPSIZ=1+NINT(ABS(FSXSCA)*(XMAX-XMIN))
         DPSI2=1+NINT(ABS(FSYSCA)*(YMAX-YMIN))
         DPTLX=NINT(FSXSCA*XMIN+FSXOFF)
         DPTLY=NINT(FSYSCA*YMAX+FSYOFF)
         DPTYP=0
C
C Update current display partition number
C
         DISPLA=REAL(DEVPAR)
      ENDIF
C
C Write DPD to work disc
C
      IF (SEMDPD(2,PARTN)) GOTO 10
C
   10 RETURN
C
C Copyright (C) 1987,1988,1989,1990:  Synoptics Ltd, All Rights Reserved
C
      END
