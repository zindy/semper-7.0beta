C Semper 6 subsidiary processing module ASSNEW
C
      LOGICAL FUNCTION ASSNEW(DEVICE,FLTYPE,FLSIZE,DRSIZE)
C
      INTEGER DEVICE,FLTYPE,DRSIZE
      INTEGER*4 FLSIZE
C
C Code for ASSIGN NEW command
C
      INTEGER LNBLNK !,IPACK
      LOGICAL ASSNAM,ASSHDR,FILMAK,FILEXI,FILSTR,YESNO,SEMCON
      LOGICAL PRUPRI
C
      INTEGER*4 I4
      LOGICAL   EXISTS,DONOT
C
      CHARACTER*4 DEFNM
C
      INCLUDE 'COMMON'
C
      INTEGER NF,NAME(FILMAX+1)
      CHARACTER*(FILMAX) FILENM,PATHNM
C
      ASSNEW=.TRUE.
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
C Fetch file name via NAME key, prompting for name if omitted
C
      CALL OUTDEF(DEFNM)
      IF (FILSTR(' ',FILENM,NF,.TRUE.)) GOTO 10
C
C Do nothing if no name given
C
      IF (NF .EQ. 0) GOTO 10
C
C Construct name string with appropriate extension
C
      IF (FILMAK(FILENM,DEFNM,PATHNM)) GOTO 10
C
C See if file with given name already exists
C
      IF (FILEXI(PATHNM,EXISTS)) GOTO 10
C
C If it does, seek confirmation for overwriting the file
C
      IF (EXISTS) THEN
         IF (YESNO('Replace existing file ',PATHNM(1:LNBLNK(PATHNM)),
     +             DONOT)) GOTO 10
C
         IF (DONOT) THEN
            IF (SEMCON('No action taken')) GOTO 10
            GOTO 10
         ENDIF
      ENDIF
C
C Convert path name into integer array form
C
      NAME(1)=LNBLNK(PATHNM)
C
      CALL SEMICS(PATHNM,NAME(2),NAME(1))
C
C Store path name on work disc
C
      IF (ASSNAM(2,DEVICE,NAME)) GOTO 10
C
C Create the new disc file
C
      CALL MCDC61(5,DEVICE,FLSIZE,0,NAME,ERROR)
C
      IF (ERROR.NE.0) THEN
         IF (ERROR.EQ.44) IDERR=DEVICE
         GOTO 10
      ENDIF
C
C Record disc parameters in device table
C
      DVTYP(DEVICE)=FLTYPE
      FLSIZ(DEVICE)=FLSIZE
      DRSIZ(DEVICE)=DRSIZE
      PROTN(DEVICE)=0
      DVSCR(DEVICE)=0
      TPSID(DEVICE)=0
C
C Record disc assignment
C
      MEDN(DEVICE)=MEDDC
C
C Write file header and create empty directory
C
      IF (ASSHDR(2,DEVICE)) GOTO 20
C
C If program device, add it to head of program priority queue
C
      IF (FLTYPE.EQ.FLTRUN) THEN
         IF (PRUPRI(1,DEVICE)) GOTO 20
      ENDIF
C
      ASSNEW=.FALSE.
C
   10 RETURN
C
C Error recovery from call to ASSHDR
C
C Close and delete the new disc file
C
   20 CALL MCDC61(7,DEVICE,I4,0,NAME,ERROR)
C
      IF (ERROR.EQ.44) IDERR=DEVICE
C
C Cancel disc assignment
C
      MEDN(DEVICE)=0
C
      GOTO 10
C
C Copyright (C) 1993-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
