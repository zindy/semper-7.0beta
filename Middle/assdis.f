C Semper 6 subsidiary processing module ASSDIS
C
      LOGICAL FUNCTION ASSDIS(DEVICE)
C
      INTEGER DEVICE
C
C Code for ASSIGN DISPLAY command
C
      INTEGER NFRAME,XFSIZE,YFSIZE,XCSIZE,YCSIZE,XMSIZE,YMSIZE
      INTEGER GPSIZE,WPFLAG
      LOGICAL OVFLAG
C
      INTEGER IVAL !,IPACK
      LOGICAL FSAS61,FSOQ61,SETVAR
C
      INCLUDE 'COMMON'
C
      ASSDIS=.TRUE.
C
C Set up arguments for display assign - keys FRAME, SIZE and SI2 allow
C the user to specify the number and size of frames required.  The
C actual parameters put into force are returned by FSAS61.
C
      NFRAME=IVAL(10321)
      XFSIZE=IVAL(30786)
      YFSIZE=IVAL(30792)
      XCSIZE=0
      YCSIZE=0
      XMSIZE=0
      YMSIZE=0
      GPSIZE=0
      WPFLAG=0
C
C Assign display
C
      IF (FSAS61(NFRAME,XFSIZE,YFSIZE,XMSIZE,YMSIZE,XCSIZE,YCSIZE,
     +           GPSIZE,WPFLAG,ERROR)) GOTO 10
C
C Find out what type of overlays the display has
C
      IF (FSOQ61(OVFLAG,ERROR)) GOTO 10
C
C Record display parameters in device table
C
      DVTYP(DEVICE) =FLTPIC
      NFRS(DEVICE)  =NFRAME
      FRSIZ(DEVICE) =XFSIZE
      FRSI2(DEVICE) =YFSIZE
      MONSIZ(DEVICE)=XMSIZE
      MONSI2(DEVICE)=YMSIZE
      CHSIZ(DEVICE) =XCSIZE
      CHSI2(DEVICE) =YCSIZE
      GPSIZ(DEVICE) =GPSIZE
      PROTN(DEVICE) =WPFLAG
      OVLIND(DEVICE)=OVFLAG
C
C Record display assignment
C
      MEDN(DEVICE)=MEDDS
C
C Return number of frames, frame size, and monitor siz ein variables
C NFRAME, FSIZE, FS2, MSIZE and MS2
C
      IF (SETVAR(22658,REAL(NFRAME))) GOTO 10
      IF (SETVAR(10369, REAL(XFSIZE))) GOTO 10
      IF (SETVAR(10392,   REAL(YFSIZE))) GOTO 10
      IF (SETVAR(21569, REAL(XMSIZE))) GOTO 10
      IF (SETVAR(21592,   REAL(YMSIZE))) GOTO 10
C
      ASSDIS=.FALSE.
C
   10 RETURN
C
C Copyright (C) 1993:  Synoptics Ltd,  All Rights Reserved
C
      END
