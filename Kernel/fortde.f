C Semper 6 system module FORTDE
C
C Attempts to delete a file via FORTRAN
C Returns .TRUE. on error and sets IOS to the run time I/O error
C
      LOGICAL FUNCTION FORTDE(IU,FILENM,IOSPAR)
C
      INTEGER IU,IOSPAR
      CHARACTER*(*) FILENM
      INTEGER IOS
C
      FORTDE = .TRUE.
C
      OPEN (IU,FILE=FILENM,STATUS='OLD',ERR=10,IOSTAT=IOS)
      CLOSE (IU,STATUS='DELETE',ERR=20,IOSTAT=IOS)
      FORTDE = .FALSE.
   10 IOSPAR = IOS
      RETURN
C
C Close unit regardless ?
C
   20 CLOSE (IU,ERR=10)
      GOTO 10
C
C Copyright (C) 1993-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
