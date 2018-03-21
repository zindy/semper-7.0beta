C Semper 6 system module FORTRD
C
C Attempts to open a FORTRAN unit to a file readonly
C Returns .TRUE. on error and sets IOS to the run time I/O error
C
      LOGICAL FUNCTION FORTRD(IU,FILENM,IOSPAR)
C
      INTEGER IU,IOSPAR
      CHARACTER*(*) FILENM
      INTEGER IOS
C
      FORTRD = .TRUE.
C
C     Removed readonly as not g77 compatible
      OPEN (IU,FILE=FILENM,STATUS='OLD',ERR=10,
     +         IOSTAT=IOS)
      REWIND(IU,IOSTAT=IOS,ERR=20)
      FORTRD = .FALSE.
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
