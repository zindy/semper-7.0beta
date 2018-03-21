C----------------------------------------------------------------------
C
C       SUBROUTINE UIFAPE
C       -----------------
C
C       PARAMETERS:
C
C       None.
C
C       Sets the application (SEMPER) error from the UIF error
C
C----------------------------------------------------------------------
C
      SUBROUTINE UIFAPE
C     =================
C
      INCLUDE 'COMMON'
      INCLUDE 'UIFCOM'
      INCLUDE 'UIFERR'
C
C     UIF error - convert to Semper error number, if error
C                 not set by Semper already
C
      IF ( UIFERR .EQ. WORKED ) THEN
         IF ( ERROR .EQ. 0 ) THEN
            ERROR = UIFEBA + WORKED
         ENDIF
      ELSE
         ERROR = UIFERR + UIFEBA
      ENDIF
      RETURN
C
C Copyright (C) 1991:  Synoptics Ltd,  All Rights Reserved
C
      END
