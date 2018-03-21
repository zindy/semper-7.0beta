C Semper 6 support module OUTNEW
C
      LOGICAL FUNCTION OUTNEW(LNEW,FILE,FILENM)
      LOGICAL LNEW
      CHARACTER*(*) FILE,FILENM
C
      INTEGER LNBLNK
      LOGICAL FORTDE,SEMIOE,YESNO
C
      INCLUDE 'COMMON'
C
      INTEGER IOS,NF
      LOGICAL LNO,REPL
C
      NF = LNBLNK(FILE)
      REPL = LNEW
      IF (.NOT.REPL) THEN
         IF (YESNO('Replace existing file ',FILE(1:NF),LNO)) GOTO 20
         IF (.NOT.LNO) REPL = .TRUE.
      ENDIF
      IF (REPL) THEN
         IF (FORTDE(RDWRTU,FILE(1:NF),IOS)) GOTO 10
      ELSE
         ERROR  = 135
         IDMESS = FILENM
         GOTO 20
      ENDIF
      OUTNEW = .FALSE.
      RETURN
C
   10 IF (SEMIOE(IOS,RDWRTU,FILENM)) GOTO 20
   20 OUTNEW = .TRUE.
      RETURN
C
C Copyright (C) 1990-1996 Synoptics Ltd,  All Rights Reserved
C
      END
