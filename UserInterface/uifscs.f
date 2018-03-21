C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION UIFSCS ( )
C       ---------------------------
C
C       PARAMETERS:
C
C       integer dummy : INPUT - dummy variable.
C
C       Clears the current scrolling area.
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION UIFSCS ( )
C     ===========================
C
C
      INCLUDE 'UIFCOM'
      INCLUDE 'UIXCOM'
      INCLUDE 'UIFERR'
C
C     LOCAL VARIABLES:
C
C     Return status
C
      LOGICAL STATUS
C
      STATUS = .FALSE.
      IF ( UIFISI ) THEN
C
C        Call the support routine to clear the lower (scrolling)
C        section of the window
C
         CALL SX11CT ( .FALSE., .TRUE. )
      ELSE
C
C        Error - UIF is not initialised
C
         UIFERR = NOTINI
         STATUS = .TRUE.
      ENDIF
C
      UIFSCS = STATUS
      RETURN
C
C Copyright (C) 1988-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
