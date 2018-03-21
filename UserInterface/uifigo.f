C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION UIFIGO ( )
C       ---------------------------
C
C       PARAMETERS:
C
C       Sets the UIF system running.  Repaint all devices, and marks
C       UIF as running for gathering input.
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION UIFIGO ( )
C     ===========================
C
      INCLUDE 'UIFCOM'
      INCLUDE 'UIXCOM'
      INCLUDE 'UIFERR'
C******************** TEMPORARY ***********************************
      INCLUDE 'COMMON'
C******************** TEMPORARY ***********************************
C
C     LOCAL VARIABLES:
C
C     Return status
C
      LOGICAL STATUS
C
C     CALLED FUNCTIONS:
C
C     Initialises the host display
C
      LOGICAL DHODST
C
C     Initialises the framestore display
C
      LOGICAL DFSDST
C
C     Refreshes a device
C
      LOGICAL UIFDRF
C
C******************** TEMPORARY ***********************************
      INTEGER LOWEST, BOT, I, YSIZE
      LOGICAL DHOSCR
C******************** TEMPORARY ***********************************
C
C     If already running, do nothing
C
      STATUS = .FALSE.
      IF ( .NOT. UIFRUN ) THEN
C
C        Check to see if there are any windows on the framestore device,
C        and if so, that it is assigned.
C
         DO 10 I = MINWIN, MAXWIN
            IF ( WINNAM(I) .GE. 0 ) THEN
               IF ( WINODE(I) .EQ. FSTORE ) THEN
C
C                 Check if display is assigned
C
                  IF ( MEDN(1) .EQ. 0 ) THEN
C
C                    Display is not assigned
C
                     UIFERR = DISNOT
                     STATUS = .TRUE.
                     GOTO 40
                  ELSE
C
C                    OK.
C
                     GOTO 20
                  ENDIF
               ENDIF
            ENDIF
   10    CONTINUE
   20    CONTINUE
C
C        Initialise the display devices
C
         STATUS = DHODST ( 1 )
         IF ( .NOT. STATUS ) THEN
            STATUS = DFSDST ( 1 )
            IF ( .NOT. STATUS ) THEN
C
C              Scan the panels on the host device, and set scrolling
C              area below them all
C
               LOWEST = -1
               DO 30 I = MINPAN, MAXPAN
                  IF ( PANNAM(I) .GE. 0 ) THEN
                     IF ( PANODE(I) .EQ. HOST ) THEN
                        IF ( PANISS(I) ) THEN
                           BOT = PANYPO(I) + PANYSI(I) - 1
                           IF ( BOT .GT. LOWEST ) LOWEST = BOT
                        ENDIF
                     ENDIF
                  ENDIF
   30          CONTINUE
C
C              Store, for the time being, the scrolling area position
C              and size in the first scrolling area slot
C
               SCRXPO(MINSCR) = DEVXMI(HOST)
               SCRYPO(MINSCR) = DEVYMI(HOST)
               SCRXSI(MINSCR) = DEVXSI(HOST)
               SCRYSI(MINSCR) = DEVYSI(HOST)
C
C              And change this, and set the scrolling as needed,
C              if we have panels on the host screen
C
               IF ( LOWEST .NE. -1 ) THEN
                  YSIZE = DEVYSI(HOST) - 1 - LOWEST
                  LOWEST = LOWEST + 1
                  STATUS = DHOSCR ( 0, LOWEST, DEVXSI(HOST), YSIZE )
                  SCRYPO(MINSCR) = LOWEST
                  SCRYSI(MINSCR) = YSIZE
               ENDIF
C
C              Refresh the devices
C
               IF ( .NOT. STATUS ) THEN
                  STATUS = UIFDRF ( HOST )
                  IF ( .NOT. STATUS ) THEN
                     STATUS = UIFDRF ( FSTORE )
                     IF ( .NOT. STATUS ) THEN
C
C                       And move the cursor to bottom left of the area
C
C------------------
C                        CALL DHOCMO ( 0, DEVYSI(HOST) - 1 )
C------------------
C
C                       and tell Semper the size of the screen (now)
C
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
C******************** TEMPORARY ***********************************
   40    CONTINUE
C
C        Make sure the event queue is empty
C
         QINTO = 0
         QREAD = 0
C
C        If all is OK, say UIF is running
C
         IF ( .NOT. STATUS ) CALL APPACT(.TRUE.)
      ENDIF
C
      UIFIGO = STATUS
      RETURN
C
C Copyright (C) 1988-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
