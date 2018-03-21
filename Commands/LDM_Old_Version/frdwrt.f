C Semper 6 processing module FRDWRT
C
      SUBROUTINE FRDWRT
C
C Provides commands READ,WRITE via modules FRDWRF and FRDWRU
C The file name is supplied by means of the text key NAME.
C
      INTEGER LNBLNK !,IPACK
      LOGICAL FILMAK,FILSEA,FILSTR,FORTDE,FRDWRF,FRDWRU
      LOGICAL OPT,OUTNEW,SEMIOE,VARSET
C
      INCLUDE 'COMMON'
C
      INTEGER MYIOS
C
      INTEGER IOS,NF
C
      LOGICAL UNFORM,LWRITE,EXISTS,EOF,FAIL,LNEW
C
      CHARACTER*(FILMAX) FILE,FILENM
      CHARACTER*4 DFNAM
C
      LWRITE = VERB .EQ. -5530
      UNFORM = OPT(-2167)
      IF (UNFORM) THEN
C
C Fault option UNFORMATTED with FORMAT key
C
         IF (VARSET(10218)) THEN
            ERROR = 60
            IDERR = -2167
            IDERR2= 10218
            GOTO 30
         ENDIF
         DFNAM = '.unf'
      ELSE
         DFNAM = '.dat'
      ENDIF
C
C Fetch file name from key NAME, prompting if key absent
C
      IF (LWRITE) THEN
         CALL OUTDEF(DFNAM)
      ELSE
         CALL INPDEF(DFNAM)
      ENDIF
      IF (FILSTR(' ',FILE,NF,LWRITE)) GOTO 30
      IF (NF .EQ. 0) GOTO 30
C
C Build full name if writing
C
      IF (LWRITE) THEN
         IF (FILMAK(FILE,DFNAM,FILENM)) GOTO 30
         FILE = FILENM
      ENDIF
C
C See if file exists
C
      IF (FILSEA(FILE,DFNAM,FILENM,EXISTS)) GOTO 30
C
C If unformatted read and not found then try alternate extension
C
      IF (UNFORM .AND. .NOT.(LWRITE .OR. EXISTS)) THEN
         DFNAM = '.dat'
         IF (FILSEA(FILE,DFNAM,FILENM,EXISTS)) GOTO 30
      ENDIF
C
      IF (EXISTS) THEN
         FILE = FILENM
      ENDIF
C
      NF = LNBLNK(FILE)
C
      IF (LWRITE) THEN
         IF (EXISTS) THEN
C
C If WRITE and file already exists, delete it if option NEW
C
            LNEW = OPT(22623)
            IF (OUTNEW(LNEW,FILE,FILE(1:NF))) GOTO 30
         ENDIF
      ELSE
C
C Fault non-existing source file for READ
C
         IF (.NOT.EXISTS) THEN
            ERROR = 130
            IDMESS = FILE(1:NF)
            GOTO 30
         ENDIF
      ENDIF
C
      EOF = .FALSE.
      IF (UNFORM) THEN
         FAIL = FRDWRU(FILE(1:NF),EXISTS,LWRITE,EOF,IOS)
      ELSE
         FAIL = FRDWRF(FILE(1:NF),EXISTS,LWRITE,EOF,IOS)
      ENDIF
C
C Any errors ?
C
      IF (FAIL) THEN
C
C End of file handled specially
C
         IF (EOF) THEN
             ERROR = 131
             IDMESS = FILE(1:NF)
             EXISTS = .TRUE.
             GOTO 20
         ELSE
             GOTO 10
         ENDIF
      ENDIF
C
C Close file if opened
C
      IF (EXISTS) CLOSE (RDWRTU,ERR=40,IOSTAT=MYIOS)
      GOTO 30
C
C I/O error: report error if possible
C
   10 IF (SEMIOE(IOS,RDWRTU,FILE(1:NF))) GOTO 20
C
C If EXISTS is set then need to CLOSE (and possibly DELETE!) file
C
   20 IF (EXISTS) THEN
C
C Exists - if writing then delete output otherwise just close unit
C
         IF (LWRITE) THEN
            IF (FORTDE(RDWRTU,FILE,IOS)) GOTO 30
         ELSE
            CLOSE (RDWRTU,ERR=30)
         ENDIF
      ENDIF
C
   30 RETURN
C
   40 IOS = MYIOS
      GOTO 10
C
C Copyright (C) 1987-1996: Synoptics Ltd,  All Rights Reserved
C
      END
