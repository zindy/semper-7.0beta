C Semper 6 system module TSTSRG
C
      LOGICAL FUNCTION TSTSRG(OPC,LPN)
C
      INTEGER OPC,LPN
C
C Establishes sub-region dimensions/offsets for LPN according to
C keys SIZE,POSITION and options LEFT,RIGHT,TOP,BOTTOM,NEAR,FAR
C The LAYER key may be used in preference to the keys SI3 and PO3
C and the options NEAR and FAR to set the z dimension/offset
C Non-unit sampling interval is allowed as an option
C
C The following options are provided:
C     OPC = 1, unit sampling, truncate region, fault disjoint region
C         = 2, unit sampling, flag disjoint region
C         = 3, allow integer sampling, flag disjoint region
C         = 4, allow skewed/rotated region, flag disjoint region
C
C Zero or negative dimensions or sampling interval are faulted.
C Options LEFT+RIGHT, TOP+BOTTOM and NEAR+FAR are faulted
C
C SMGL1 returns .TRUE. if region is not identical to LPN
C SMGL2 returns .TRUE. if region lies outside LPN
C SMGL3 returns .TRUE. if region contains whole pixels only
C
      LOGICAL VARSET,VARINT,OPT,CONOPT
      INTEGER IVAL
      REAL VAL
C
      LOGICAL SUBREG,DISREG,PIXREG,INSIDE,LSPEC(3)
      INTEGER FIRST(3),LAST(3),SIZE(3),AXES(3)
      INTEGER NFIRST(3),NLAST(3),NSIZE(3),NPOSN(3)
      INTEGER END(2,4),POUT(2),QOUT(2)
      INTEGER NDIMS(3),CPOSN(3)
      REAL CENTRE(2),CORNER(2),U(2),V(2)
      REAL P(2,4),LIMIT(2,2),PEND(2),QEND(2)
C
      INCLUDE 'COMMON'
C
      INTEGER ISAMPL,IP,IQ,I,J,K,L,M,N
      REAL SAMPLE,ANGLE,PMIN,PMAX,PLIM,QLIM
      EQUIVALENCE (SUBREG,SMGL1),(DISREG,SMGL2),(PIXREG,SMGL3)
      EQUIVALENCE (FIRST,SMGI1),(LAST,SMGI4),(SIZE,SMGI7)
      EQUIVALENCE (ISAMPL,SMGI10)
      EQUIVALENCE (CORNER,SMGR1),(U,SMGR3),(V,SMGR5)
C
C Packed names
C
      INTEGER NLAYER,NLA2,NSAMPL,NANGLE,NUV,NU,NU2,NV,NV2,NOK
      PARAMETER (NLAYER=19265, NLA2=19272, NSAMPL=30453, NANGLE=2167)
      PARAMETER (NUV=-2481, NU=-1601, NU2=-2881, NV=-3201, NV2=-4481)
      PARAMETER (NOK=24440)
C
C NSIZE contains SIZE,SI2,SI3
C
      DATA NSIZE /30786,30792,30793/
C
C NPOSN contains POSITION,PO2,PO3
C
      DATA NPOSN /26219,26232,26233/
C
C NFIRST contains LEFT,TOP,FAR
C
      DATA NFIRST /19406,-617,9658/
C
C NLAST contains RIGHT,BOTTOM,NEAR
C
      DATA NLAST /29167,3820,22601/
C
C Orientation of pixel axes (Y reversed)
C
      DATA AXES /1,-1,1/
C
C Start and end index for each side of sub-region
C
      DATA END /1,2,1,3,2,4,3,4/
C
      TSTSRG=.TRUE.
C
C Set up arrays containing picture size and centre position
C
      NDIMS(1)=NCOLS(LPN)
      NDIMS(2)=NROWS(LPN)
      NDIMS(3)=NLAYS(LPN)
C
      CPOSN(1)=CCOLN(LPN)
      CPOSN(2)=CROWN(LPN)
      CPOSN(3)=CLAYN(LPN)
C
C Determine X dimension and default for Y dimension
C
      IF (VARSET(NSIZE(1))) THEN
         SIZE(1)=IVAL(NSIZE(1))
         SIZE(2)=SIZE(1)
      ELSE
         SIZE(1)=NDIMS(1)
         SIZE(2)=NDIMS(2)
      ENDIF
C
C Determine Y dimension
C
      IF (VARSET(NSIZE(2))) SIZE(2)=IVAL(NSIZE(2))
C
C See if Z dimension determined by LAYER,LA2 keys
C
      IF (VARSET(NLAYER)) THEN
C
C See if any of keys/options SI3, PO3, NEAR or FAR is set
C
         IF (VARSET(NSIZE(3))) THEN
            IDERR2=NSIZE(3)
         ELSE IF (VARSET(NPOSN(3))) THEN
            IDERR2=NPOSN(3)
         ELSE IF (OPT(NFIRST(3))) THEN
            IDERR2=NFIRST(3)
         ELSE IF (OPT(NLAST(3))) THEN
            IDERR2=NLAST(3)
         ELSE
            IDERR2=NOK
         ENDIF
C
C Fault conflict between key LAYER and keys/options SI3, PO3,
C NEAR or FAR
C
        IF (IDERR2.NE.NOK) THEN
            ERROR=60
            IDERR=NLAYER
            GOTO 110
         ENDIF
C
C Determine first layer in sub-region
C
         FIRST(3)=IVAL(NLAYER)
C
C Determine last layer in sub-region
C
         IF (VARSET(NLA2)) THEN
            LAST(3)=IVAL(NLA2)
C
C Fault LA2 less than LAYER key
C
            IF (LAST(3).LT.FIRST(3)) THEN
               ERROR=3
               IDERR=NLAYER
               GOTO 110
            ENDIF
C
C Otherwise, last layer defaults to start layer (single layer
C sub-region)
C
         ELSE
            LAST(3)=FIRST(3)
         ENDIF
C
C Determine Z dimension
C
         SIZE(3)=LAST(3)-FIRST(3)+1
C
C X and Y sub-region limits still to be determined
C
         N=2
C
C Otherwise, determine Z dimension using SI3 key
C
      ELSE
         IF (VARSET(NSIZE(3))) THEN
            SIZE(3)=IVAL(NSIZE(3))
         ELSE
            SIZE(3)=NDIMS(3)
         ENDIF
C
C X, Y and Z sub-region limits still to be determined
C
         N=3
      ENDIF
C
C Fault zero or negative sub-region dimension
C
      IF (SIZE(1).LE.0.OR.SIZE(2).LE.0.OR.SIZE(3).LE.0) THEN
         ERROR=3
         IDERR=NSIZE(1)
         GOTO 110
      ENDIF
C
C See if sub-region contains whole pixels only
C
      IF (OPC.EQ.4) THEN
         PIXREG=.NOT.(OPT(NUV).OR.VARSET(NANGLE)).AND.VARINT(NSAMPL)
     +                                           .AND.VARINT(NPOSN(1))
     +                                           .AND.VARINT(NPOSN(2))
     +                                           .AND.VARINT(NPOSN(3))
      ELSE
         PIXREG=.TRUE.
      ENDIF
C
C If whole pixels only, determine integer limits of sub-region
C
      IF (PIXREG) THEN
C
C See if key SAMPLING is set
C
         IF (VARSET(NSAMPL)) THEN
C
C Fetch sampling interval
C
            ISAMPL=IVAL(NSAMPL)
C
C Fault bad sampling interval
C
            IF (ISAMPL.LE.0) GOTO 120
C
C If opcode 1 or 2, fault non-unit sampling interval
C
            IF ((OPC.EQ.1.OR.OPC.EQ.2).AND.ISAMPL.NE.1) GOTO 120
C
C Otherwise, default for sampling interval is 1
C
         ELSE
            ISAMPL=1
         ENDIF
C
C Determine whether X,Y,(Z) sub-region limits are explicitly specified
C by means of keys SIZE,SI2,(SI3) and POSITION,PO2,(PO3) and options
C LEFT,TOP,(FAR) and RIGHT,BOTTOM,(NEAR)
C
         DO 10 I=1,N
            LSPEC(I)=VARSET(NSIZE(I)).OR.
     +               VARSET(NPOSN(I)).OR.
     +               OPT(NFIRST(I)).OR.
     +               OPT(NLAST(I))
   10    CONTINUE
C
C Y limits also affected by key SIZE (default for Y dimension when
C set and key SI2 not set)
C
         IF (VARSET(NSIZE(1))) LSPEC(2)=.TRUE.
C
C X and Y limits also affected by key SAMPLING
C
         IF (VARSET(NSAMPL)) THEN
            LSPEC(1)=.TRUE.
            LSPEC(2)=.TRUE.
         ENDIF
C
C Determine remaining sub-region limits
C
         DO 20 I=1,N
C
C If any relevant sub-region keys/options are set, determine
C sub-region limits as specified
C
            IF (LSPEC(I)) THEN
C
C Fault conflict between options LEFT,TOP,(FAR) and RIGHT,BOTTOM,(NEAR)
C
               IF (CONOPT(NFIRST(I),NLAST(I))) GOTO 110
C
C Look for option LEFT,TOP,(FAR)
C
               IF (OPT(NFIRST(I))) THEN
                  FIRST(I)=1
C
C Otherwise, look for option RIGHT,BOTTOM,(NEAR)
C
               ELSE IF (OPT(NLAST(I))) THEN
                  FIRST(I)=NDIMS(I)-(ISAMPL*SIZE(I)-1)
C
C Otherwise, sub-region is centred on source region
C
               ELSE
                  FIRST(I)=CPOSN(I)-ISAMPL*(SIZE(I)/2)
               ENDIF
C
C Determine sub-region limits including any specified offset
C
               FIRST(I)=FIRST(I)+AXES(I)*IVAL(NPOSN(I))
               LAST(I)=FIRST(I)+(ISAMPL*SIZE(I)-1)
C
C Otherwise, set sub-region limits to source limits
C
            ELSE
               FIRST(I)=1
               LAST(I)=NDIMS(I)
            ENDIF
   20    CONTINUE
C
C Initialise flags for sub-region and disjoint region
C
         SUBREG=.FALSE.
         DISREG=.FALSE.
C
C Check sub-region limits
C
         DO 30 I=1,3
C
C Set flag for sub-region different to source region
C
            IF (FIRST(I).NE.1.OR.LAST(I).NE.NDIMS(I)) SUBREG=.TRUE.
C
C Set flag for disjoint sub-region (no overlap between sub-region and
C source region)
C
            IF (FIRST(I).GT.NDIMS(I).OR.LAST(I).LT.1) DISREG=.TRUE.
C
C If opcode 1, truncate sub-region to source limits
C
            IF (OPC.EQ.1) THEN
               FIRST(I)=MAX(1,FIRST(I))
               LAST(I)=MIN(LAST(I),NDIMS(I))
               SIZE(I)=LAST(I)-FIRST(I)+1
            ENDIF
   30    CONTINUE
C
C If opcode 1, fault disjoint sub-region
C
         IF (DISREG.AND.OPC.EQ.1) THEN
            ERROR=9
            GOTO 110
         ENDIF
C
C Set frame parameters
C
         CORNER(1)=REAL(FIRST(1)-CPOSN(1))
         CORNER(2)=REAL(CPOSN(2)-FIRST(2))
         U(1)=REAL(ISAMPL)
         U(2)=0.0
         V(1)=0.0
         V(2)=-REAL(ISAMPL)
C
C Otherwise, determine general frame parameters for sub-region
C
      ELSE
C
C See if key SAMPLING is set
C
         IF (VARSET(NSAMPL)) THEN
C
C Fetch sampling interval
C
            SAMPLE=VAL(NSAMPL)
C
C Fault bad sampling interval
C
            IF (SAMPLE.LE.0.0) GOTO 120
C
C Otherwise, default for sampling interval is 1.0
C
         ELSE
            SAMPLE=1.0
         ENDIF
C
C Determine sub-region centre position
C
         DO 40 I=1,2
C
C Fault conflict between options LEFT,TOP and RIGHT,BOTTOM
C
            IF (CONOPT(NFIRST(I),NLAST(I))) GOTO 110
C
C Look for option LEFT,TOP
C
            IF (OPT(NFIRST(I))) THEN
               CENTRE(I)=1.0+SAMPLE*REAL(SIZE(I)/2)
C
C Otherwise, look for option RIGHT,BOTTOM
C
            ELSE IF (OPT(NLAST(I))) THEN
               CENTRE(I)=REAL(NDIMS(I)+1)-SAMPLE*REAL((SIZE(I)+1)/2)
C
C Otherwise sub-region is centred on source region
C
            ELSE
               CENTRE(I)=REAL(CPOSN(I))
            ENDIF
C
C Convert centre position to picture coordinates including any
C specified offset
C
            CENTRE(I)=REAL(AXES(I))*(CENTRE(I)-REAL(CPOSN(I)))+
     +         VAL(NPOSN(I))
   40    CONTINUE
C
C Set up source limits in picture coordinates
C
         LIMIT(1,1)=REAL(1-CPOSN(1))
         LIMIT(2,1)=REAL(CPOSN(2)-NDIMS(2))
         LIMIT(1,2)=REAL(NDIMS(1)-CPOSN(1))
         LIMIT(2,2)=REAL(CPOSN(2)-1)
C
C If option UV is set, set up vectors for skewed axes
C
         IF (OPT(NUV)) THEN
            U(1)=VAL(NU)
            U(2)=VAL(NU2)
            V(1)=-VAL(NV)
            V(2)=-VAL(NV2)
C
C Otherwise, rotate unskewed axes by amount specified by key ANGLE
C
         ELSE
            ANGLE=VAL(NANGLE)
            U(1)=SAMPLE*COS(ANGLE)
            U(2)=SAMPLE*SIN(ANGLE)
            V(1)=U(2)
            V(2)=-U(1)
         ENDIF
C
C Set flag for sub-region
C
         SUBREG=.TRUE.
C
C Initialise flags for disjoint and totally overlapping region
C
         DISREG=.FALSE.
         INSIDE=.TRUE.
C
C Determine position of four corners of sub-region
C
         DO 50 I=1,2
C
C Determine top left corner position
C
            CORNER(I)=CENTRE(I)-REAL(SIZE(1)/2)*U(I)
     +                         -REAL(SIZE(2)/2)*V(I)
C
C Set up four corner positions in array P
C
            P(I,1)=CORNER(I)
            P(I,2)=CORNER(I)+REAL(SIZE(1)-1)*U(I)
            P(I,3)=CORNER(I)+REAL(SIZE(2)-1)*V(I)
            P(I,4)=P(I,2)+P(I,3)-P(I,1)
C
C Determine minimum and maximum coordinate values
C
            PMIN=MIN(P(I,1),P(I,2),P(I,3),P(I,4))
            PMAX=MAX(P(I,1),P(I,2),P(I,3),P(I,4))
C
C Reset flag if sub-region goes outside source region
C
            IF (PMIN.LT.LIMIT(I,1).OR.PMAX.GT.LIMIT(I,2)) INSIDE=.FALSE.
C
C Set flag if sub-region totally outside source region
C
            IF (PMIN.GT.LIMIT(I,2).OR.PMAX.LT.LIMIT(I,1)) DISREG=.TRUE.
   50    CONTINUE
C
C Skip if sub-region does not cross source limits
C
         IF (INSIDE.OR.DISREG) GOTO 100
C
C Look for any intersection points between sub-region border and each
C source limit in turn, to determine whether region is disjoint.
C Loop on X and Y limits
C
         DO 90 I=1,2
C
C Determine indices to P,Q directions = (X,Y) and (Y,X)
C
            IP=I
            IQ=3-I
C
C Loop on lower and upper limit
C
            DO 80 J=1,2
C
C Fetch limit value
C
               PLIM=LIMIT(IP,J)
C
C Initialise count of number of intersection points for current source
C limit
C
               M=0
C
C Look at each of four sides of sub-region in turn
C
               DO 70 K=1,4
C
C Determine position of each side end in relation to current source
C limit
C
                  DO 60 L=1,2
C
C Fetch end coordinates
C
                     PEND(L)=P(IP,END(L,K))
                     QEND(L)=P(IQ,END(L,K))
C
C Determine whether end lies on, below or above limit (POUT = 0,1,2)
C
                     IF (PEND(L).LT.PLIM) THEN
                        POUT(L)=1
                     ELSE IF (PEND(L).GT.PLIM) THEN
                        POUT(L)=2
                     ELSE
                        POUT(L)=0
                     ENDIF
   60             CONTINUE
C
C If side crosses limit, add intersection point to list
C
                  IF (POUT(1).NE.POUT(2)) THEN
C
C Increment count of number of intersection points
C
                     M=M+1
C
C Determine coordinate value of intersection point
C
                     QLIM=QEND(1)+(PLIM-PEND(1))*(QEND(2)-QEND(1))/
     +                                           (PEND(2)-PEND(1))
C
C Record whether intersection point lies on, below or above border of
C source region (QOUT = 0,1,2)
C
                     IF (QLIM.LT.LIMIT(IQ,1)) THEN
                        QOUT(M)=1
                     ELSE IF (QLIM.GT.LIMIT(IQ,2)) THEN
                        QOUT(M)=2
                     ELSE
                        QOUT(M)=0
                     ENDIF
                  ENDIF
   70          CONTINUE
C
C See if any intersection points were found
C
               IF (M.NE.0) THEN
C
C If so, region cannot be disjoint if intersection point lies on border
C of source region
C
                  IF (QOUT(1).EQ.0.OR.QOUT(2).EQ.0) GOTO 100
C
C Or if intersection points lie on opposite sides or source region
C
                  IF (QOUT(1).NE.QOUT(2)) GOTO 100
               ENDIF
   80       CONTINUE
   90    CONTINUE
C
C Set flag for disjoint region
C
         DISREG=.TRUE.
C
C If Z limits not specified by means of LAYER key, default is centre
C source layer
C
  100    IF (N.NE.2) THEN
            FIRST(3)=CPOSN(3)
            LAST(3)=CPOSN(3)
         ENDIF
      ENDIF
C
      TSTSRG=.FALSE.
C
  110 RETURN
C
C Fault bad sampling interval etc.
C
  120 ERROR=3
      IDERR=NSAMPL
      GOTO 110
C
C Copyright (C) 1987-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
