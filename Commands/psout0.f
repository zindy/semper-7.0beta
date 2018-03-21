C Semper 6 subsidiary module PSOUTB
C
      LOGICAL FUNCTION PSOUTB( )
C
      LOGICAL PSOUT0
C
      PSOUTB = PSOUT0(' ')
C
      RETURN
C
C Copyright (C) 1988-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module PSOUT0
C
      LOGICAL FUNCTION PSOUT0(STRING)
      CHARACTER*(*) STRING
C
      INTEGER IOS
C
      INCLUDE 'COMMON'
C
      COMMON /PSOUTC/ IOS
C
   10 FORMAT(A)
      PSOUT0 = .TRUE.
      WRITE(RDWRTU,10,IOSTAT=IOS,ERR=20) STRING
      PSOUT0 = .FALSE.
   20 RETURN
C
C Copyright (C) 1988-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
