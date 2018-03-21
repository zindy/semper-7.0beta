C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION UIFSSU ( )
C       ---------------------------
C
C       PARAMETERS:
C
C       Sets things up to write out a line to the current scrolling
C       area.
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION UIFSSU ( )
C     ===========================
C
      INCLUDE 'EVENTS'
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
C     Dummy function return status
C
      LOGICAL LDUMMY
C
C     Dummy variable
C
      INTEGER DUMVAR
C
C     Hardware cursor echoing state
C
      INTEGER HARDEC
C
C     CALLED FUNCTIONS:
C
C     Gets information on a queue
C
      LOGICAL EQGETD
C
      IF ( UIFISI ) THEN
C
C        Check to see if there are any windows on the host device: if
C        not, nothing to do
C
C***************  TEMPORARY  ********************
         STATUS = .FALSE.
         IF ( DEVHAP(HOST) ) THEN
C
C           No windows.  If cursor is echoed both, this
C           implies a workstation, so nothing to do
C
            LDUMMY = EQGETD ( MPOINT, HARDEC, DUMVAR, DUMVAR )
            IF ( HARDEC .NE. ECHALL ) THEN
C
C              Move the cursor to the bottom of the scrolling area
C
               CALL DHOCMO ( DEVXMI(HOST),
     +                       DEVYMI(HOST) + DEVYSI(HOST) - 1 )
               STATUS = .FALSE.
            ENDIF
         ENDIF
C***************  TEMPORARY  ********************
      ELSE
C
C        Error - UIF is not initialised
C
         STATUS = .TRUE.
         UIFERR = NOTINI
      ENDIF
C
      UIFSSU = STATUS
      RETURN
C
C Copyright (C) 1988-1991:  Synoptics Ltd,  All Rights Reserved
C
      END
