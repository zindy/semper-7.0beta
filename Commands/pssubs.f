C Subsidiary PostScript output modules for Semper 6
C
      LOGICAL FUNCTION PSDEFI(STRING,INT)
      CHARACTER*(*) STRING
      INTEGER INT
C
      INTEGER IOS
C
      INCLUDE 'COMMON'
C
      COMMON /PSOUTC/ IOS
C
   10 FORMAT (A,I8,' def')
      PSDEFI = .TRUE.
      WRITE(RDWRTU,10,IOSTAT=IOS,ERR=20) STRING,INT
      PSDEFI = .FALSE.
   20 RETURN
C
C Copyright (C) 1992-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
C
      LOGICAL FUNCTION PSDEFF(STRING,FP)
      CHARACTER*(*) STRING
      REAL FP
C
      INTEGER IOS
C
      INCLUDE 'COMMON'
C
      COMMON /PSOUTC/ IOS
C
   10 FORMAT (A,F10.5,' def')
      PSDEFF = .TRUE.
      WRITE(RDWRTU,10,IOSTAT=IOS,ERR=20) STRING,FP
      PSDEFF = .FALSE.
   20 RETURN
C
C Copyright (C) 1992-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
C
      LOGICAL FUNCTION PSDEFS(STRING,STR)
      CHARACTER*(*) STRING,STR
C
      INTEGER IOS
C
      INCLUDE 'COMMON'
C
      COMMON /PSOUTC/ IOS
C
   10 FORMAT (A,' ',A,' def')
      PSDEFS = .TRUE.
      WRITE(RDWRTU,10,IOSTAT=IOS,ERR=20) STRING,STR
      PSDEFS = .FALSE.
   20 RETURN
C
C Copyright (C) 1992-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
C
      LOGICAL FUNCTION PSDEFL(STRING,STR)
      CHARACTER*(*) STRING,STR
C
      INTEGER IOS
C
      INCLUDE 'COMMON'
C
      COMMON /PSOUTC/ IOS
C
   10 FORMAT (A,' (',A,') def')
      PSDEFL = .TRUE.
      WRITE(RDWRTU,10,IOSTAT=IOS,ERR=20) STRING,STR
      PSDEFL = .FALSE.
   20 RETURN
C
C Copyright (C) 1992-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
