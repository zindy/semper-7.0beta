C Semper 6 processing module REPORT
C---------------------------------------------------------------------
C
C      SUBROUTINE REPORT
C      -----------------
C
C      PARAMETERS:
C
C      None.
C
C      Handles command REPORT
C
C---------------------------------------------------------------------
C
      SUBROUTINE REPORT
C     =================
C
      INTEGER LNBLNK
      LOGICAL OPT,SEMBEE,SEMDIA
C
      INCLUDE 'COMMON'
C
      LOGICAL LTRAP,LERROR,LISERR,LISTRA
C
C Packed names
C
      INTEGER NERROR,NEXIT,NTRAP
      PARAMETER (NERROR=8738,NEXIT=8969,NTRAP=-722)
C
C Shows the last reported ERROR or TRAP, and sets ERROR=10 if option
C EXIT is given.
C
      LERROR = OPT(NERROR)
      LTRAP = OPT(NTRAP)
C
C If neither selected check both
C
      IF (.NOT.(LERROR .OR. LTRAP)) THEN
         LERROR = .TRUE.
         LTRAP  = .TRUE.
      ENDIF
C
C Check for actual error strings
C
      LISERR = LNBLNK(ERRREC) .GT. 0
      LERROR = LERROR .AND. LISERR
C
      LISTRA = LNBLNK(TRPREC) .GT. 0
      LTRAP  = LTRAP .AND. LISTRA
C
C Last error has preference over last trap
C
      IF (LERROR) THEN
         IF (SEMDIA(ERRREC,NDIERR)) GOTO 10
      ELSE IF (LTRAP) THEN
         IF (SEMDIA(TRPREC,NDIERR)) GOTO 10
      ELSE
         GOTO 10
      ENDIF
C
C If exit to be taken then set error
C
      IF (OPT(NEXIT)) THEN
         ERROR = 10
      ELSE
         IF (SEMBEE()) GOTO 10
      ENDIF
C
   10 RETURN
C
C Copyright (C) 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
