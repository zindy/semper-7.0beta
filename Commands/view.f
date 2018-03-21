C Semper 6 processing module VIEW
C
      SUBROUTINE VIEW
C
C Provides direct control over viewing conditions. Implements verb VIEW.
C View parameters consist of:
C    display device number
C    display frame number
C    look-up table number
C    integer zoom factor
C    view centre position
C The options FRAME, PARTITION or PICTURE (default) determine in which
C coordinate system VIEW is to work.  In setting up the corresponding
C graphics coordinate system, the device and frame number are
C established. Defaults for the look-up table number and view centre
C position are also set up as as result of the call to FSINIT.  The
C default for the zoom factor is always 1.  The keys LUT, ZOOM, PAN and
C PA2 may be used to alter any of the corresponding default values.  If
C multiple values are specified for the $1 key (keys $12, $13, ...,$19),
C the view conditions are cycled through the specified series of views.
C The number of cycles and the pause between each view switch is
C specified by the keys TIMES and WAIT. The default for the view centre
C position may be changed by using the 2-D sub-region keys and options.
C If any sub-region key/option is set, the default for the view centre
C will be set to the centre of the resulting sub-region.  Note that this
C still means that the PAN, PA2 keys still take precedence if set.
C If the ENQUIRE option is set, the current viewing conditions are
C returned in Semper variables F, Z, X1, X2, Y1 and Y2.
C
      REAL VAL
      INTEGER IVAL,IPACK
      LOGICAL OPT,VARSET,ABANDN,SEMLU,SEMWAI,SETVAR
      LOGICAL FSOPTN,FSINIT,FSREGN,FSVIEW,FSQMON,FSELEC
C
      INCLUDE 'COMMON'
C
      REAL SECS,XMIN,XMAX,YMIN,YMAX,XPAN(9),YPAN(9)
      INTEGER I,J,M,N
      INTEGER IXSIZE,IYSIZE,FRAME,ZOOM
      INTEGER OPC,IKEY(9),NKEY(9)
      LOGICAL LSIZE,SUBREG
C
C Packed names
C
      INTEGER NTIMES,NWAIT,NZOOM,NSIZE,NSI2,NPOSIT,NPO2
      INTEGER NLEFT,NRIGHT,NTOP,NBOTTO,NDLLR1,NDLR12,NDLR13
      INTEGER NDLR14,NDLR15,NDLR16,NDLR17,NDLR18,NDLR19
      PARAMETER (NTIMES=-374, NWAIT=-4850, NZOOM=-10216)
      PARAMETER (NSIZE=30786, NSI2=30792, NPOSIT=26219, NPO2=26232)
      PARAMETER (NLEFT=19406, NRIGHT=29167, NTOP=-617, NBOTTO=3820)
      PARAMETER (NDLLR1=-12441, NDLR12=-12473, NDLR13=-12474)
      PARAMETER (NDLR14=-12475, NDLR15=-12476, NDLR16=-12477)
      PARAMETER (NDLR17=-12478, NDLR18=-12479, NDLR19=-12480)
C
      DATA NKEY /NDLLR1,NDLR12,NDLR13,NDLR14,NDLR15,NDLR16,NDLR17,
     +           NDLR18,NDLR19/
C
C Determine whether frame, partition or picture coordinates are to be
C used for graphics coordinates, according to options FRAME, PARTITION
C and PICTURE
C
      IF (FSOPTN(OPC,IKEY(1))) GOTO 60
      N=1
C
C Find out how many of keys $12, $13, ... ,$19 are set
C
      DO 10 I=2,9
C
C Skip if key $1(i) not set
C
         IF (.NOT.VARSET(NKEY(I))) GOTO 20
C
C Fetch key value and increment key count
C
         IKEY(I)=IVAL(NKEY(I))
         N=N+1
   10 CONTINUE
C
C See if key SIZE or SI2 is set
C
   20 LSIZE=VARSET(NSIZE).OR.VARSET(NSI2)
C
C Determine whether any of the 2-D sub-region keys and options are set
C
      SUBREG=LSIZE.OR.VARSET(NPOSIT).OR.VARSET(NPO2).OR.
     +       OPT(NLEFT).OR.OPT(NRIGHT).OR.OPT(NTOP).OR.OPT(NBOTTO)
C
C Fetch value of ZOOM key
C
      ZOOM=IVAL(NZOOM)
C
C Determine default for view centre position for each view
C
      DO 30 I=1,N
C
C Set up graphics coordinates for view
C
         IF (FSINIT(OPC,IKEY(I))) GOTO 60
C
C If sub-region keys/options being used, determine sub-region limits
C
         IF (SUBREG) THEN
C
C If keys SIZE and SI2 not set, provide appropriate default values
C
            IF (.NOT.LSIZE) THEN
C
C Determine appropriate size = smaller of border size and monitor size
C
               IXSIZE=MIN(1+NINT(FSBRIG-FSBLEF),MONSIZ(FSDEV)/ZOOM)
               IYSIZE=MIN(1+NINT(FSBTOP-FSBBOT),MONSI2(FSDEV)/ZOOM)
C
C Temporarily set keys SIZE and SI2
C
               IF (SEMLU(2,NSIZE,REAL(IXSIZE))) GOTO 60
               IF (SEMLU(2,NSI2,REAL(IYSIZE))) GOTO 60
            ENDIF
C
C Determine sub-region limits
C
            IF (FSREGN(XMIN,XMAX,YMIN,YMAX)) GOTO 60
C
C Store default view centre position = centre of sub-region
C
            XPAN(I)=(XMIN+XMAX)/2.0
            YPAN(I)=(YMIN+YMAX)/2.0
C
C Otherwise, default for view centre position = position set up
C by FSINIT
C
         ELSE
            XPAN(I)=FSXPAN
            YPAN(I)=FSYPAN
         ENDIF
   30 CONTINUE
C
C Cycle through views if more than one view specified
C
      IF (N.GT.1) THEN
C
C Determine number of times to cycle views
C
         M=MAX(IVAL(NTIMES),1)
C
C Determine time interval between each view switch
C
         SECS=MAX(VAL(NWAIT),0.0)
C
C Cycle round series of views
C
         DO 50 I=1,M
            DO 40 J=1,N
C
C Set up graphics coordinates corresponding to next view
C
               IF (FSINIT(OPC,IKEY(J))) GOTO 60
C
C Reset default for view centre position to allow for use of 2-D
C sub-region keys and options
C
               FSXPAN=XPAN(J)
               FSYPAN=YPAN(J)
C
C Set up new viewing conditions, allowing the keys LUT, ZOOM, PAN and
C PA2 to override the default viewing parameters
C
               IF (FSVIEW()) GOTO 60
C
C Check for request to abandon this command
C
               IF (ABANDN(ERROR)) GOTO 60
C
C Wait for specified amount of time
C
               IF (SEMWAI(SECS)) GOTO 60
   40       CONTINUE
   50    CONTINUE
      ENDIF
C
C Set up graphics coordinates corresponding to final view
C
      IF (FSINIT(OPC,IKEY(1))) GOTO 60
C
C Reset default for view centre position
C
      FSXPAN=XPAN(1)
      FSYPAN=YPAN(1)
C
C Set up final viewing conditions
C
      IF (FSVIEW()) GOTO 60
C
C See if ENQUIRE option is set
C
      IF (OPT(8577)) THEN
C
C Fetch current viewing conditions
C
         IF (FSQMON(FRAME,ZOOM,XMIN,XMAX,YMIN,YMAX)) GOTO 60
C
C Return current viewing conditions in variables F,Z,X1,X2,Y1 and Y2
C
         IF (SETVAR(9600,REAL(FRAME))) GOTO 60
         IF (SETVAR(-9601,REAL(ZOOM)))  GOTO 60
         IF (SETVAR(-7641,XMIN))       GOTO 60
         IF (SETVAR(-7681,XMAX))       GOTO 60
         IF (SETVAR(-9241,YMIN))       GOTO 60
         IF (SETVAR(-9281,YMAX))       GOTO 60
      ENDIF
C
C Update current frame/partiton/picture number
C
      IF (FSELEC()) GOTO 60
C
   60 RETURN
C
C Copyright (C) 1987-1993:  Synoptics Ltd,  All Rights Reserved
C
      END
