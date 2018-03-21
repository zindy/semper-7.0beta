C Semper 6 subsidiary processing module ASSSIZ
C
      LOGICAL FUNCTION ASSSIZ(FLTYPE,FLSIZE,DRSIZE)
C
      INTEGER FLTYPE,DRSIZE
      INTEGER*4 FLSIZE
C
C Determines storage device type according to the options PROGRAM and
C HELP, returning this in FLTYPE and also the file and directory size
C according to keys SIZE and SLOTS.  The SIZE key is mandatory, the
C SLOTS key optional, with appropriate defaults according to the device
C type.  The sizes are returned in FLSIZE and DRSIZE.
C
      REAL    VAL
      INTEGER IVAL !,IPACK
      LOGICAL OPT,CONOPT,VARSET
C
      REAL    RSIZE
      INTEGER SLOTS,SLTMAX
C
      INCLUDE 'COMMON'
C
      ASSSIZ=.TRUE.
C
C Fault conflicting options PROGRAM and HELP
C
      IF (CONOPT(26335,13012)) GOTO 10
C
C Determine device type according to options PROGRAM and HELP
C (if both options absent, default is picture disc)
C
      IF (OPT(26335)) THEN
         FLTYPE=FLTRUN
      ELSE IF (OPT(13012)) THEN
         FLTYPE=FLTHEL
      ELSE
         FLTYPE=FLTPIC
      ENDIF
C
C See if SIZE key is set
C
      IF (VARSET(30786)) THEN
C
C Fetch value for SIZE key
C
         RSIZE=VAL(30786)
C
C Fault bad value for SIZE key
C
         IF (RSIZE.LE.0.0) THEN
            ERROR=3
            IDERR=30786
            GOTO 10
         ENDIF
C
C Convert file size from KB to logical disc blocks, rounding up
C
         FLSIZE=1.0+(1024.0*RSIZE-1.0)/REAL(LNBLK)
C
C Fault absence of SIZE key
C
      ELSE
         ERROR=122
         IDERR=30786
         GOTO 10
      ENDIF
C
C See if SLOTS key is set
C
      IF (VARSET(30895)) THEN
C
C Fetch value for SLOTS key
C
         SLOTS=IVAL(30895)
C
C Fault bad value for SLOTS key
C
         IF (SLOTS.LT.1) THEN
            ERROR=3
            IDERR=30895
            GOTO 10
         ENDIF
C
C Otherwise, determine default value for SLOTS key according to
C device type
C
      ELSE
         IF (FLTYPE.EQ.FLTPIC) THEN
            SLOTS=2002
         ELSE IF (FLTYPE.EQ.FLTRUN) THEN
            SLOTS=64
         ELSE IF (FLTYPE.EQ.FLTHEL) THEN
            SLOTS=400
         ENDIF
      ENDIF
C
C Determine the directory size in logical disc blocks according to the
C device type
C
      IF (FLTYPE.EQ.FLTPIC) THEN
         SLTMAX=LNBUF/LNINT4/2
         DRSIZE=1+(MIN(SLOTS,2002,SLTMAX)*2*LNINT4-1)/LNBLK
      ELSE IF (FLTYPE.EQ.FLTRUN) THEN
         SLTMAX=LNBUF/LNINT
         DRSIZE=(3+MIN(SLOTS,SLTMAX))*PRCSZE
      ELSE IF (FLTYPE.EQ.FLTHEL) THEN
         DRSIZE=SLOTS
      ENDIF
C
C Include in the file size space for the file header (1 block) and the
C directory
C
      FLSIZE=1+DRSIZE+FLSIZE
C
      ASSSIZ=.FALSE.
C
   10 RETURN
C
C Copyright (C) 1993:  Synoptics Ltd,  All Rights Reserved
C
      END
