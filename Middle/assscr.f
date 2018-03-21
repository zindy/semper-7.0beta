C Semper 6 subsidiary processing module ASSSCR
C
      LOGICAL FUNCTION ASSSCR(DEVICE,FLTYPE,FLSIZE,DRSIZE)
C
      INTEGER DEVICE,FLTYPE,DRSIZE
      INTEGER*4 FLSIZE
C
C Code for ASSIGN SCRATCH command
C
      INTEGER LNBLNK
      LOGICAL ASSNAM,ASSHDR,SEMZZZ,FILEXI,SEMWAI,PRUPRI
C
      INTEGER   I
      INTEGER*4 I4
      LOGICAL   EXISTS
C
      CHARACTER*4 DEFNM
C
      INCLUDE 'COMMON'
C
      INTEGER NAME(FILMAX+1)
      CHARACTER*(FILMAX) PATHNM
C
      ASSSCR=.TRUE.
C
C Determine default file extension according to device type
C
      IF (FLTYPE.EQ.FLTPIC) THEN
         DEFNM='.dsk'
      ELSE IF (FLTYPE.EQ.FLTRUN) THEN
         DEFNM='.plb'
      ELSE IF (FLTYPE.EQ.FLTHEL) THEN
         DEFNM='.hlb'
      ENDIF
C
C Make a number of attempts to determine file name for scratch disc
C
      DO 10 I=1,20
C
C Construct name string with appropriate extension
C
         IF (SEMZZZ(PATHNM,DEFNM)) GOTO 30
C
C See if file with scratch name already exists
C
         IF (FILEXI(PATHNM,EXISTS)) GOTO 30
C
C If it does, wait for a short while and try again
C
         IF (EXISTS) THEN
            IF (SEMWAI(1.0)) GOTO 30
C
C Otherwise, go ahead and create scratch disc
C
         ELSE
            GOTO 20
         ENDIF
   10 CONTINUE
C
C Cannot establish file name for scratch disc
C
      ERROR=123
      GOTO 30
C
C Convert path name into integer array form
C
   20 NAME(1)=LNBLNK(PATHNM)
C
      CALL SEMICS(PATHNM,NAME(2),NAME(1))
C
C Store path name on work disc
C
      IF (ASSNAM(2,DEVICE,NAME)) GOTO 30
C
C Create the scratch disc
C
      CALL MCDC61(8,DEVICE,FLSIZE,0,NAME,ERROR)
C
      IF (ERROR.NE.0) THEN
         IF (ERROR.EQ.44) IDERR=DEVICE
         GOTO 30
      ENDIF
C
C Record scratch disc parameters in device table
C
      DVTYP(DEVICE)=FLTYPE
      FLSIZ(DEVICE)=FLSIZE
      DRSIZ(DEVICE)=DRSIZE
      PROTN(DEVICE)=0
      DVSCR(DEVICE)=-1
      TPSID(DEVICE)=0
C
C Record scratch disc assignment
C
      MEDN(DEVICE)=MEDDC
C
C Write file header and create empty directory
C
      IF (ASSHDR(2,DEVICE)) GOTO 40
C
C If program device, add it to head of program priority queue
C
      IF (FLTYPE.EQ.FLTRUN) THEN
         IF (PRUPRI(1,DEVICE)) GOTO 40
      ENDIF
C
      ASSSCR=.FALSE.
C
   30 RETURN
C
C Error recovery from call to ASSHDR
C
C Close and delete the scratch disc
C
   40 CALL MCDC61(7,DEVICE,I4,0,NAME,ERROR)
C
      IF (ERROR.EQ.44) IDERR=DEVICE
C
C Cancel scratch disc assignment
C
      MEDN(DEVICE)=0
C
      GOTO 30
C
C Copyright (C) 1993:  Synoptics Ltd,  All Rights Reserved
C
      END
