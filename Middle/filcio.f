C Semper 6 primitive module FILCIO
C
      LOGICAL FUNCTION FILCIO(HANDLE,IOP,TEXT,LENGTH)
C
C Reads from/writes to the specified file (this implementation takes
C the file handle as a Fortran unit number).  LENGTH is returned set
C to -1 if an end-of-file condition is detected on read.
C
      INTEGER HANDLE,IOP,LENGTH
      CHARACTER*(*) TEXT
C
      INTEGER LNBLNK
C
      INTEGER IOS
C
      INCLUDE 'COMMON'
C
   10 FORMAT(A)
C
      FILCIO = .TRUE.
C
C Read/write a line of text
C
      IF (IOP.EQ.1) THEN
         READ (UNIT=HANDLE,FMT=10,END=50,ERR=60,IOSTAT=IOS) TEXT
         LENGTH = LNBLNK(TEXT)
      ELSE
         IF (LENGTH.GT.0) THEN
            WRITE (UNIT=HANDLE,FMT=10,ERR=60,IOSTAT=IOS) TEXT(1:LENGTH)
         ELSE
            WRITE (UNIT=HANDLE,FMT=10,ERR=60,IOSTAT=IOS)
         ENDIF
      ENDIF
C
   30 FILCIO = .FALSE.
C
   40 RETURN
C
C Flag end-of-file on read by returning blank string and LENGTH = -1
C
   50 TEXT = ' '
      LENGTH = -1
      GOTO 30
C
C Fortran I/O error detected
C
   60 ERROR = INTERR
      IDERR = HANDLE
      IDERR2 = IOS
      IDMESS = 'FILCIO'
      GOTO 40
C
C Copyright (C) 1989-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
