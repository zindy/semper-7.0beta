C Semper 6 system module FORTWR
C
C Attempts to open a FORTRAN unit to a file for writing
C If OLD is set then tries to use an existing file...
C Returns .TRUE. on error and sets IOS to the run time I/O error
C
      LOGICAL FUNCTION FORTWR(IU,FILENM,OLD,IOSPAR)
C
      INTEGER IU,IOSPAR
      LOGICAL OLD
      CHARACTER*(*) FILENM
      INTEGER IOS
C
      CHARACTER*3 STAT
C
      INCLUDE 'COMMON'
C
      FORTWR = .TRUE.
C
      IF (OLD) THEN
         STAT = 'OLD'
      ELSE
         STAT = 'NEW'
      ENDIF
C
      OPEN (IU,FILE=FILENM,STATUS=STAT,ERR=10,FORM='FORMATTED',
     +      IOSTAT=IOS)
      REWIND(IU,IOSTAT=IOS,ERR=20)
      FORTWR = .FALSE.
   10 IOSPAR = IOS
      RETURN
C
C Close file on rewind failure
C
   20 CLOSE(IU,ERR=10)
      GOTO 10
C
C Copyright (C) 1987-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
