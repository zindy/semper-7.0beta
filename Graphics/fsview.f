C Semper 6 system module FSVIEW
C
      LOGICAL FUNCTION FSVIEW()
C
C Sets the viewing conditions to make visible the graphics area set up
C by the latest call to FSINIT.  The keys LUT, ZOOM and PAN + PA2 may be
C used on the command line to modify the view parameters.  FSINIT
C provides a default look-up table number and a default view centre. The
C default for the zoom factor is always 1.
C
      REAL VAL
      INTEGER IVAL
      LOGICAL FSVW61,VARSET,OPT
C
      REAL XPAN,YPAN
      INTEGER IXPAN,IYPAN,IZOOM,ILUT,ICLIP,IBLEF,IBRIG,IBTOP,IBBOT
C
      INCLUDE 'COMMON'
C
C Packed names
C
      INTEGER NLUT,NZOOM,NPAN,NPA2,NCLIP
      PARAMETER (NLUT=20060, NZOOM=-10216, NPAN=25654, NPA2=25672)
      PARAMETER (NCLIP=5289)
C
      FSVIEW=.TRUE.
C
C Determine look-up table number
C
      IF (VARSET(NLUT)) THEN
         ILUT=IVAL(NLUT)
      ELSE
         ILUT=FSLUT
      ENDIF
C
C Fault bad look-up table number
C
      IF (ILUT.LT.1.OR.ILUT.GT.NLUTS) THEN
         ERROR=68
         IDERR=ILUT
         GOTO 10
      ENDIF
C
C Fault non-existent look-up table
C
      IF (LUTMOD(ILUT).EQ.0) THEN
         ERROR=69
         IDERR=ILUT
         GOTO 10
      ENDIF
C
C Determine zoom factor
C
      IF (VARSET(NZOOM)) THEN
         IZOOM=IVAL(NZOOM)
      ELSE
         IZOOM=1
      ENDIF
C
C Fault zero or negative zoom factor
C
      IF (IZOOM.LE.0) THEN
         ERROR=83
         IDERR=IZOOM
         GOTO 10
      ENDIF
C
C See if option CLIP is set
C
      IF (OPT(NCLIP)) THEN
         ICLIP=1
      ELSE
         ICLIP=0
      ENDIF
C
C Convert border limits into display coordinates (for blanking limits)
C
      IBLEF=NINT(FSXSCA*FSBLEF+FSXOFF)
      IBRIG=NINT(FSXSCA*FSBRIG+FSXOFF)
      IBTOP=NINT(FSYSCA*FSBTOP+FSYOFF)
      IBBOT=NINT(FSYSCA*FSBBOT+FSYOFF)
C
C Determine view centre X position
C
      IF (VARSET(NPAN)) THEN
         XPAN=VAL(NPAN)
      ELSE
         XPAN=FSXPAN
      ENDIF
C
C Determine view centre Y position
C
      IF (VARSET(NPA2)) THEN
         YPAN=VAL(NPA2)
      ELSE
         YPAN=FSYPAN
      ENDIF
C
C Convert view centre position to display coordinates
C
      IXPAN=NINT(FSXSCA*XPAN+FSXOFF)
      IYPAN=NINT(FSYSCA*YPAN+FSYOFF)
C
C Set new viewing conditions
C
      IF (FSVW61(ILUT,IZOOM,ICLIP,IBRIG-IBLEF+1,IBBOT-IBTOP+1,
     +           IBLEF,IBTOP,IXPAN,IYPAN,FSFRA,ERROR)) GOTO 10
C
C Update variable CLUT = current look-up table number
C
      CLUT=REAL(ILUT)
C
      FSVIEW=.FALSE.
C
   10 RETURN
C
C Copyright (C) 1987,1989,1990:  Synoptics Ltd,  All Rights Reserved
C
      END
