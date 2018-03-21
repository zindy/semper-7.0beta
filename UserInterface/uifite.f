C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION UIFITE ( )
C       ---------------------------
C
C       PARAMETERS:
C
C       Shuts down the UIF system.
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION UIFITE ( )
C     ===========================
C
      INCLUDE 'UIFCOM'
      INCLUDE 'UIXCOM'
      INCLUDE 'UIFERR'
C************** Temporary ***********************
      INCLUDE 'COMMON'
C************** Temporary ***********************
C
C     LOCAL VARIABLES:
C
C     Return status
C
      LOGICAL STATUS
C
C     Loop counter
C
      INTEGER I
C
C     CALLED FUNCTIONS:
C
C     Shuts down access to a device
C
      LOGICAL DEVDSH
C
      STATUS = .FALSE.
      IF ( UIFISI ) THEN
C
C     Say UIF is not running
C
         CALL APPACT(.FALSE.)
C------------------------------
C        UIFISI = .FALSE.
C------------------------------
C
C        Shut down the devices
C
         DO 10 I = 1, NDEVIC
            STATUS = DEVDSH ( I )
            IF ( STATUS ) GOTO 20
   10    CONTINUE
   20    CONTINUE
C
C        Check for error
C
         IF ( STATUS ) UIFERR = BADTER
C
C************** Temporary ***********************
C
C        Tell semper the screen is full size again
C                 (not on workstations)
C
C************** Temporary ***********************
      ENDIF
C
      UIFITE = STATUS
      RETURN
C
C Copyright (C) 1988-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
