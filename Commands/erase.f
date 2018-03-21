C Semper 6 processing module ERASE
C
      SUBROUTINE ERASE
C
C Erases sub-region defined with respect to current graphics coordinates
C Graphics coordinates are chosen according to options FRAME, PARTITION
C and PICTURE (default).  Sub-region is sepecified using 2-D sub-region
C keys in relation to border limits set up for the graphics coordinates.
C Options IMAGE and OVERLAY force erasure of just image or overlay plane
C instead of both planes.  If option VIEW is set, the view conditions
C are switched to centre the view on the graphics border (frame,
C partition or picture limits).
C
      INTEGER IPACK
      LOGICAL CONOPT,FSOPTN,FSINIT,FSREGN,FSVIEW,FSERAS,FSELEC,OPT
C
      INCLUDE 'COMMON'
C
      REAL XMIN,XMAX,YMIN,YMAX
      INTEGER MODE,N,OPC
C
C See if options IMAGE or OVERLAY are set
C
      IF (OPT(14921)) THEN
         IF (CONOPT(24885,14921)) GOTO 10
         MODE = 1
      ELSE IF (OPT(24885)) THEN
         MODE = 2
      ELSE
C
C If none given default to both
C
         MODE = 3
      ENDIF
C
C Determine nature of graphics coordinates according to options FRAME,
C PARTITION and PICTURE
C
      IF (FSOPTN(OPC,N)) GOTO 10
C
C Set up specified graphics coordinates
C
      IF (FSINIT(OPC,N)) GOTO 10
C
C If option VIEW is set, switch view to area of interest
C
      IF (OPT(-3566)) THEN
         IF (FSVIEW()) GOTO 10
      ENDIF
C
C Determine size and limits of sub-region specified with 2-D sub-region
C keys and options
C
      IF (FSREGN(XMIN,XMAX,YMIN,YMAX)) GOTO 10
C
C Erase the specified sub-region
C
      IF (FSERAS(MODE,XMIN,XMAX,YMIN,YMAX)) GOTO 10
C
C Update current frame/partition/picture number
C
      IF (FSELEC()) GOTO 10
C
   10 RETURN
C
C Copyright (C) 1987-1993:  Synoptics Ltd,  All Rights Reserved
C
      END
