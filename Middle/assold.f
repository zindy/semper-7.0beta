C Semper 6 subsidiary processing module ASSOLD
C
      LOGICAL FUNCTION ASSOLD(DEVICE)
C
      INTEGER DEVICE
C
C Code for ASSIGN [OLD] command
C
      INTEGER LNBLNK !,IPACK
      LOGICAL ASSNAM,ASSHDR,FILSTR,OPT,OPTNO,FILSEA,PRUPRI
C
C      LOGICAL FILMAK
C
      INTEGER   FLTYPE,WPFLAG,IOP,NF
      INTEGER*4 I4
      LOGICAL   EXISTS
C
      CHARACTER*4 DEFNM
C
      INCLUDE 'COMMON'
C
      INTEGER NAME(FILMAX+1)
      CHARACTER*(FILMAX) FILENM,PATHNM
C
      ASSOLD=.TRUE.
C
C Determine device type according to options PROGRAM and HELP
C and set default file extension according to device type
C
      IF (OPT(26335)) THEN
         FLTYPE=FLTRUN
         DEFNM='.plb'
      ELSE IF (OPT(13012)) THEN
         FLTYPE=FLTHEL
         DEFNM='.hlb'
      ELSE
         FLTYPE=FLTPIC
         DEFNM='.dsk'
      ENDIF
C
C Fetch file name via NAME key, prompting for name if omitted
C
      CALL INPDEF(DEFNM)
      IF (FILSTR(' ',FILENM,NF,.FALSE.)) GOTO 20
C
C Do nothing if no name given
C
      IF (NF .EQ. 0) GOTO 20
C
C See if file with given name already exists
C
      IF (FILSEA(FILENM,DEFNM,PATHNM,EXISTS)) GOTO 20
C
C Fault non-existence of file
C
      IF (.NOT.EXISTS) THEN
C
C Code should be:
C         ERROR = 130
C         IF (FILMAK(FILENM,DEFNM,IDMESS)) GOTO 10
C
C But to not break all of the run files becomes:
C
         ERROR = 44
         GOTO 20
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
      IF (ASSNAM(2,DEVICE,NAME)) GOTO 20
C
C See if WP/NOWP option is set (help libraries are write-protected
C by default)
C
      IF (OPT(-5441)) THEN
         WPFLAG=-1
      ELSE IF (OPTNO(-5441)) THEN
         WPFLAG=0
      ELSE
         IF (FLTYPE.EQ.FLTHEL) THEN
            WPFLAG=-1
         ELSE
            WPFLAG=0
         ENDIF
      ENDIF
C
C Open the disc file with read/write or readonly access
C
      IF (WPFLAG.EQ.0) THEN
         IOP=3
      ELSE
         IOP=4
      ENDIF
C
      CALL MCDC61(IOP,DEVICE,I4,0,NAME,ERROR)
C
      IF (ERROR.NE.0) THEN
         IF (ERROR.EQ.44) IDERR=DEVICE
         GOTO 20
      ENDIF
C
C Record disc parameters in device table
C
      DVTYP(DEVICE)=FLTYPE
      PROTN(DEVICE)=WPFLAG
      DVSCR(DEVICE)=0
      TPSID(DEVICE)=0
C
C Record disc assignment
C
      MEDN(DEVICE)=MEDDC
C
C Verify file header and determine file and directory sizes
C
      IF (ASSHDR(1,DEVICE)) GOTO 30
C
C If program device, add it to head of program priority queue
C
      IF (FLTYPE.EQ.FLTRUN) THEN
         IF (PRUPRI(1,DEVICE)) GOTO 30
      ENDIF
C
      ASSOLD=.FALSE.
C
   20 RETURN
C
C Error recovery from call to ASSHDR
C
C Close the disc file
C
   30 CALL MCDC61(6,DEVICE,I4,0,NAME,ERROR)
C
      IF (ERROR.EQ.44) IDERR=DEVICE
C
C Cancel disc assignment
C
      MEDN(DEVICE)=0
C
      GOTO 20
C
C Copyright (C) 1993-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
