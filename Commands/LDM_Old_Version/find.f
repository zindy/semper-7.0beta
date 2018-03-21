C Semper 6 processing module FIND
C
      SUBROUTINE FIND
C
C If opt CM is set, finds centre of mass of source picture.  If opt
C CM is not set, finds highest or lowest value in the source picture.
C A circular sub-region is scanned instead of the entire source picture
C if key RADIUS is set.  The centre for the sub-region is specified by
C the keys POSITION and PO2.  Further options SQUARED, NEGATIVE, LOWEST,
C and ITERATED are dealt with by FINDCM which is called by FIND to do
C the actual work.  The variables T, X and Y are set to the returned
C value (mass or peak value) and position.
C
      REAL VAL
      LOGICAL OPT,VARSET,FINDCM,SEMLU,SEMCON
      LOGICAL MARSET,FSINIT,FSCIRC,FSMARK
C
      INCLUDE 'COMMON'
C
      REAL RADIUS,VALUE,XCEN,XPOS,YCEN,YPOS
      INTEGER MARK
      LOGICAL CM,SUBREG,ANNOT
C
C Packed names
C
      INTEGER NCM,NRADIU,NPOS,NPO2,NT,NX,NY,NVERIF
      PARAMETER (NCM=5320, NRADIU=28844, NPOS=26219, NPO2=26232)
      PARAMETER (NT=-1, NX=-6401, NY=-8001, NVERIF=-3419)
C
C See if option CM is set
C
      CM=OPT(NCM)
C
C Sub-region is specified if key RADIUS is set
C
      SUBREG=VARSET(NRADIU)
C
C Fetch and check sub-region parameters
C
      IF (SUBREG) THEN
C
C Fault zero or negative sub-region radius
C
         RADIUS=VAL(NRADIU)
         IF (RADIUS.LE.0.0) THEN
            ERROR=3
            IDERR=NRADIU
            GOTO 20
         ENDIF
C
C Fetch centre position for sub-region
C
         XCEN=VAL(NPOS)
         YCEN=VAL(NPO2)
      ENDIF
C
C Find required value and position
C
      IF (FINDCM(LP1,CM,SUBREG,XCEN,YCEN,
     +           RADIUS,VALUE,XPOS,YPOS)) GOTO 20
C
C Fetch and check value of key MARK
C
      IF (MARSET(ANNOT,MARK)) GOTO 20
C
C If key MARK appropriately set, annotate specified display picture
C
      IF (ANNOT) THEN
C
C Initialise display graphics
C
         IF (FSINIT(3,MARK)) GOTO 20
C
C Annotate display picture only if 2-D image
C
         IF (FSPTYP.EQ.1) THEN
C
C If circular sub-region, draw circle round sub-region
C
            IF (SUBREG) THEN
               IF (FSCIRC(XCEN,YCEN,RADIUS)) GOTO 20
            ENDIF
C
C Mark position
C
            IF (FSMARK(XPOS,YPOS,FSMMOD,FSMSIZ)) GOTO 20
         ENDIF
      ENDIF
C
C Set variables T, X and Y to return value and position
C
      IF (SEMLU(1,NT,VALUE)) GOTO 20
      IF (SEMLU(1,NX,XPOS)) GOTO 20
      IF (SEMLU(1,NY,YPOS)) GOTO 20
C
C If VERIFY option is set, print results to console output stream
C
      IF (OPT(NVERIF)) THEN
         WRITE (RECORD,10) VALUE,XPOS,YPOS
   10    FORMAT ('Value ',G12.4,' position ',2F7.2)
         IF (SEMCON(RECORD)) GOTO 20
      ENDIF
C
   20 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
