C----------------------------------------------------------------------
C
C       SUBROUTINE APPACT ( STATUS )
C       ----------------------------
C
C       PARAMETERS:
C
C       logical status : INPUT - new UIFRUN status
C
C       Sets the UIF run status, and indicates UIF is running in the
C       application (SEMPER)
C
C----------------------------------------------------------------------
C
      SUBROUTINE APPACT ( STATUS )
C     ============================
C
      LOGICAL STATUS
C
      INCLUDE 'UIFCOM'
      INCLUDE 'COMMON'
C
      UIFRUN = STATUS
      IF (UIFRUN) THEN
         UIF = 1.0
      ELSE
         UIF = 0.0
      ENDIF
      RETURN
C
C Copyright (C) 1991:  Synoptics Ltd,  All Rights Reserved
C
      END
