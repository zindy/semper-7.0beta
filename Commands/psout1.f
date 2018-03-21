C Semper 6 subsidiary module PSOUT1
C
      LOGICAL FUNCTION PSOUT1(PATHNM,DFEXT)
      CHARACTER*(*) PATHNM,DFEXT
C
      INTEGER IPACK,LNBLNK
      LOGICAL CONOPT,FILMAK,FILSEA,FILSTR,FORTDE,FORTWR,OPT,SEMIOE
C
      INCLUDE 'COMMON'
C
      INTEGER NF,IOS
C
      LOGICAL EXISTS,LNEW,LOLD,OLDFIL
      CHARACTER*(FILMAX) FILE,FILENM
C
      PSOUT1 = .TRUE.
      IF (OPT(22623)) THEN
         IF (CONOPT(24484,22623)) GOTO 10
         LNEW = .TRUE.
      ELSE
         LOLD = OPT(24484)
         LNEW = .FALSE.
      ENDIF
C
C Fetch file name from key NAME, prompting if key is absent
C
      CALL OUTDEF(DFEXT)
      IF (FILSTR(' ',FILE,NF,.TRUE.)) GOTO 10
      IF (NF .EQ. 0) GOTO 10
C
C See if file exists
C
      IF (FILMAK(FILE,DFEXT,FILENM)) GOTO 10
      FILE = FILENM
      IF (FILSEA(FILE,DFEXT,FILENM,EXISTS)) GOTO 10
C
      IF (EXISTS) THEN
         FILE = FILENM
      ENDIF
      NF = LNBLNK(FILE)
C
C If file already exists, delete it if NEW given
C
      OLDFIL = .FALSE.
      IF (EXISTS) THEN
         IF (LNEW) THEN
            IF (FORTDE(RDWRTU,FILE,IOS)) GOTO 20
         ELSE IF (LOLD) THEN
            OLDFIL = .TRUE.
         ELSE
            IDMESS = FILE
            ERROR = 135
            GOTO 10
         ENDIF
      ENDIF
C
C Try to open the file dynamically
C
      IF (FORTWR(RDWRTU,FILE,OLDFIL,IOS)) GOTO 20
C
      PSOUT1 = .FALSE.
      PATHNM = FILE
C
   10 RETURN
C
C File I/O errors
C
   20 IF (SEMIOE(IOS,RDWRTU,FILE(1:NF))) GOTO 10
      GOTO 10
C
C Copyright (C) 1988-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
