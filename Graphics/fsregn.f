C Semper 6 system module FSREGN
C
      LOGICAL FUNCTION FSREGN(XMIN,XMAX,YMIN,YMAX)
C
C Returns sub-region limits according to 2-D sub-region keys SIZE,
C POSITION and PO2 and options LEFT, RIGHT, TOP and BOTTOM.  The
C sub-region is defined with respect to the border limits set up by the
C latest call to FSINIT.  The sub-region limits are truncated to the
C border limits.  If the sub-region is completely outside the border
C limits, an error is returned.
C
      REAL XMIN,XMAX,YMIN,YMAX
C
      REAL VAL
      LOGICAL VARSET,OPT,CONOPT
C
      INCLUDE 'COMMON'
C
      REAL XSIZE,YSIZE,XCEN,YCEN
C
C Packed names
C
      INTEGER NSIZE,NSI2,NPOSIT,NPO2,NLEFT,NRIGHT,NTOP,NBOTTO
      PARAMETER (NSIZE=30786, NSI2=30792, NPOSIT=26219, NPO2=26232)
      PARAMETER (NLEFT=19406, NRIGHT=29167, NTOP=-617, NBOTTO=3820)
C
      FSREGN=.TRUE.
C
C Determine X dimension of sub-region from SIZE key, and set up default
C for Y dimension
C
      IF (VARSET(NSIZE)) THEN
         XSIZE=ANINT(VAL(NSIZE))
         YSIZE=XSIZE
      ELSE
         XSIZE=1.0+ANINT(FSBRIG-FSBLEF)
         YSIZE=1.0+ANINT(FSBTOP-FSBBOT)
      ENDIF
C
C Determine Y dimension of sub-region from SI2 key
C
      IF (VARSET(NSI2)) YSIZE=ANINT(VAL(NSI2))
C
C Fault zero or negative sub-region size
C
      IF (XSIZE.LT.1.0.OR.YSIZE.LT.1.0) THEN
         ERROR=3
         IDERR=NSIZE
         GOTO 10
      ENDIF
C
C If any sub-region keys/options affecting X direction are set,
C determine X limits as specified
C
      IF (VARSET(NSIZE).OR.VARSET(NPOSIT).OR.
     +    OPT(NLEFT).OR.OPT(NRIGHT)) THEN
C
C Fault conflict between options LEFT and RIGHT
C
         IF (CONOPT(NLEFT,NRIGHT)) GOTO 10
C
C Determine X centre position according to options LEFT and RIGHT
C
         IF (OPT(NLEFT)) THEN
            XCEN=ANINT(FSBLEF)+AINT(XSIZE/2.0)
         ELSE IF (OPT(NRIGHT)) THEN
            XCEN=ANINT(FSBRIG)-AINT((XSIZE-1.0)/2.0)
         ELSE
            XCEN=0.0
         ENDIF
C
C Offset centre position by amount given by key POSITION
C
         XCEN=XCEN+ANINT(VAL(NPOSIT))
C
C Determine sub-region X limits
C
         XMIN=XCEN-AINT(XSIZE/2.0)
         XMAX=XCEN+AINT((XSIZE-1.0)/2.0)
C
C Otherwise, X limits default to border X limits
C
      ELSE
         XMIN=FSBLEF
         XMAX=FSBRIG
      ENDIF
C
C If any sub-region keys/options affecting Y direction are set,
C determine Y limits as specified
C
      IF (VARSET(NSIZE).OR.VARSET(NSI2).OR.VARSET(NPO2).OR.
     +    OPT(NTOP).OR.OPT(NBOTTO)) THEN
C
C Fault conflict between options TOP and BOTTOM
C
         IF (CONOPT(NTOP,NBOTTO)) GOTO 10
C
C Determine Y centre position according to options TOP and BOTTOM
C
         IF (OPT(NBOTTO)) THEN
            YCEN=ANINT(FSBBOT)+AINT((YSIZE-1.0)/2.0)
         ELSE IF (OPT(NTOP)) THEN
            YCEN=ANINT(FSBTOP)-AINT(YSIZE/2.0)
         ELSE
            YCEN=0.0
         ENDIF
C
C Offset centre position by amount given by key PO2
C
         YCEN=YCEN+ANINT(VAL(NPO2))
C
C Determine sub-region Y limits
C
         YMIN=YCEN-AINT((YSIZE-1.0)/2.0)
         YMAX=YCEN+AINT(YSIZE/2.0)
C
C Otherwise, Y limits default to border Y limits
C
      ELSE
         YMIN=FSBBOT
         YMAX=FSBTOP
      ENDIF
C
C Fault sub-region completely outside border limits
C
      IF (XMIN.GT.FSBRIG.OR.XMAX.LT.FSBLEF.OR.
     +    YMIN.GT.FSBTOP.OR.YMAX.LT.FSBBOT) THEN
         ERROR=89
         GOTO 10
      ENDIF
C
C Truncate sub-region limits to border limits
C
      XMIN=MAX(XMIN,FSBLEF)
      XMAX=MIN(XMAX,FSBRIG)
      YMIN=MAX(YMIN,FSBBOT)
      YMAX=MIN(YMAX,FSBTOP)
C
      FSREGN=.FALSE.
C
   10 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
