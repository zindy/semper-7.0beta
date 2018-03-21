C Semper 6 subsidiary processing module ASSFIL
C
      LOGICAL FUNCTION ASSFIL(DEVICE)
C
      INTEGER DEVICE
C
C Code for ASSIGN FILE command
C
      INTEGER IVAL,LNBLNK !,IPACK
      LOGICAL FILMAK,FILSEA,FILSTR,ASSNAM,OPT,FILOPN,FILPOS,CONOPT
C
      INTEGER I,MODE,NF,WIDTH,HANDLE,IOP
      LOGICAL EXISTS
C
      INCLUDE 'COMMON'
C
      INTEGER NAME(FILMAX+1)
      LOGICAL LNEW,LOLD
      CHARACTER*(FILMAX) FILENM,PATHNM
      CHARACTER*4 DFEXT
C
      ASSFIL= .TRUE.
      DFEXT = '.log'
C
C Determine mode for opening log file according to options OLD and NEW
C (Assume OLD if APPEND is set)
C
      LNEW = OPT(22623)
      IF (LNEW) THEN
C
C Fault conflicting options OLD and NEW
C
         IF (CONOPT(24484,22623)) GOTO 20
         LOLD = .FALSE.
         MODE = 1
      ELSE
         LOLD = OPT(24484)
         IF (LOLD) THEN
            MODE = 3
         ELSE
            IF (OPT(2256)) THEN
               MODE = 3
            ELSE
               MODE = 2
            ENDIF
         ENDIF
      ENDIF
C
C Fetch value for WIDTH key
C
      WIDTH = IVAL(-5165)
C
C Fault bad value for WIDTH key
C
      IF (WIDTH.LT.1) THEN
         ERROR = 3
         IDERR = -5165
         GOTO 20
      ENDIF
C
C Fetch file name via NAME key, prompting for name if omitted
C
      CALL OUTDEF(DFEXT)
      IF (FILSTR(' ',FILENM,NF,.TRUE.)) GOTO 20
C
C Do nothing if no name given
C
      IF (NF .EQ. 0) GOTO 20
C
C If option NEW is set, construct file name string with appropriate
C extension
C
      IF (LNEW) THEN
         IF (FILMAK(FILENM,DFEXT,PATHNM)) GOTO 20
C
C Otherwise, see if named file exists
C
      ELSE
         IF (FILSEA(FILENM,DFEXT,PATHNM,EXISTS)) GOTO 20
C
C If it does not, construct file name string with appropriate extension
C
         IF (.NOT.EXISTS) THEN
            IF (FILMAK(FILENM,DFEXT,PATHNM)) GOTO 20
C
C If option OLD is set, fault non-existence of file
C
            IF (LOLD) THEN
               ERROR=130
               IDMESS=PATHNM(1:78)
               GOTO 20
            ENDIF
         ENDIF
      ENDIF
C
C Convert path name into integer array form
C
      NAME(1) = LNBLNK(PATHNM)
C
      CALL SEMICS(PATHNM,NAME(2),NAME(1))
C
C Store path name on work disc
C
      IF (ASSNAM(2,DEVICE,NAME)) GOTO 20
C
C Open the log file
C
      IF (FILOPN(FILENM,DFEXT,1,3,MODE,WIDTH,PATHNM,HANDLE)) GOTO 20
C
C APPEND option determines where log file is initially positioned
C
      IF (OPT(2256)) THEN
         IOP=3
      ELSE
         IOP=1
      ENDIF
C
C Position log file at start or end (APPEND option)
C
      IF (FILPOS(HANDLE,IOP,0)) GOTO 20
C
C Record log file parameters in device table
C
      DVTYP(DEVICE)=FLTTEX
      DVHAN(DEVICE)=HANDLE
      DVWID(DEVICE)=WIDTH
      PROTN(DEVICE)=0
C
C Set default echo settings = NONE
C
      DO 10 I=1,6
         FILFLG(DEVICE,I)=.FALSE.
   10 CONTINUE
C
C Record log file assignment
C
      MEDN(DEVICE)=MEDFL
C
      ASSFIL=.FALSE.
C
   20 RETURN
C
C Copyright (C) 1993-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
