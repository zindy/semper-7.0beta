C Semper 6 primitive module FILCLS
C
      LOGICAL FUNCTION FILCLS(HANDLE,DELETE)
C
C Closes the specified file, taking the file handle as a Fortran unit
C number
C
      INTEGER HANDLE
      LOGICAL DELETE
C
      INTEGER IOS
C
      INCLUDE 'COMMON'
C
      FILCLS = .TRUE.
C
C Close the file
C
      IF (DELETE) THEN
         CLOSE (UNIT=HANDLE,STATUS='DELETE',ERR=20,IOSTAT=IOS)
      ELSE
         CLOSE (UNIT=HANDLE,STATUS='KEEP',ERR=20,IOSTAT=IOS)
      ENDIF
C
      FILCLS = .FALSE.
C
   10 RETURN
C
C Fortran CLOSE error detected
C
   20 ERROR = INTERR
      IDERR = HANDLE
      IDERR2 = IOS
      IDMESS = 'FILCLS'
      GOTO 10
C
C Copyright (C) 1989-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
