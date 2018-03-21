C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION UIFINI ( )
C       ---------------------------
C
C       Initialises the UIF system.  Initialises dynamic memory,
C       all the UIF common data, and sets the values of the relevent
C       application variables
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION UIFINI ( )
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
C     Number of mouse buttons
C
      INTEGER NOBUTS
C
C     Dummy variable
C
      INTEGER DUMVAR
C
C     Loop counter
C
      INTEGER I
C
C     CALLED FUNCTIONS:
C
C     Initialises dynamic memory
C
      LOGICAL INITM
C
C     Queries queue data
C
      LOGICAL EQGETD
C
C     Gets device sizes
C
      LOGICAL DEVDSI
C
C     Initialises the host display
C
      LOGICAL DHODST
C
C     Initialises the framestore display
C
      LOGICAL DFSDST
C
C     Sets the value of an application varaible
C
      LOGICAL APPACK
C
C     Check whether we are already initialised
C
      STATUS = .FALSE.
      IF ( .NOT. UIFISI ) THEN
C
C        Initialise dynamic memory
C
         UIFISI = .FALSE.
         STATUS = INITM ( )
         IF ( .NOT. STATUS ) THEN
C
C           Initialise UIF externally available variables
C
            CALL APPACT(.FALSE.)
            UIFERR = WORKED
C
C           Initialise all objects to unused
C
            DO 10 I = MINOBJ, MAXOBJ
               OBJNAM(I) = -1
   10       CONTINUE
C
C           Initialise queue pointers
C
            QINTO = 0
            QREAD = 0
C
C           Initialise internally available global variables
C
            MPANSH = .FALSE.
            MPANID = -1
            UIFAAC = .TRUE.
            BEFACT = 0
            AFTACT = 0
            VERPOS = CENTRE
            HORPOS = CENTRE
            SELTEX = -1
            ACTDEV = BOTH
            LMBACT = 0
            CMBACT = 0
            RMBACT = 0
C
C           See how many mouse buttons there are, and set up the
C           left-centre-right button numbers
C
            STATUS = EQGETD ( MBUT, NOBUTS, DUMVAR, DUMVAR )
            IF ( NOBUTS .EQ. 3 ) THEN
               LBNUMB = 1
               CBNUMB = 2
               RBNUMB = 3
            ELSE
               LBNUMB = 1
               CBNUMB = -1
               RBNUMB = 2
            ENDIF
C
C           Get device sizes
C
            DO 20 I = 1, NDEVIC
               STATUS = DEVDSI ( I, DEVXMI(I), DEVYMI(I),
     +                              DEVXSI(I), DEVYSI(I),
     +                              DEVMXS(I), DEVMYS(I),
     +                              DEVXOF(I), DEVYOF(I),
     +                              DEVNCO(I) )
               IF ( STATUS ) GOTO 30
C
C              Initialise mouse and cursor positions
C
               CURXPO(I) = DEVXMI(I)
               CURYPO(I) = DEVYMI(I)
               CURMOX(I) = 0
               CURMOY(I) = 0
               STKTOP(I) = 0
               DEVHAP(I) = .FALSE.
C
C              Initialise the 'last panel and element' data
C
               PIDL(I) = -1
               EIDL(I) = -1
   20       CONTINUE
   30       CONTINUE
C
C           Initialise the devices
C
            IF ( .NOT. STATUS ) THEN
               STATUS = DHODST ( 1 )
               IF ( .NOT. STATUS ) THEN
                  STATUS = DFSDST ( 1 )
                  IF ( .NOT. STATUS ) THEN
C
C                    Initialise the clipping rectangle
C
                     CALL UIXSCL ( 0, 0, 32765, 32765 )
C
C                    Set the application variables for current device,
C                    panel element etc.
C
                     STATUS = APPACK ( 4969, REAL (HOST) )
                     IF ( .NOT. STATUS ) THEN
                        STATUS = APPACK ( 26175, 0.0 )
                        IF ( .NOT. STATUS ) THEN
                           STATUS = APPACK ( 8575, 0.0 )
                           IF ( .NOT. STATUS ) THEN
C
C                             If everything has worked, say system is
C                             initialised
C
                              UIFISI = .TRUE.
                           ELSE
C
C                             Error - couldn't set variable
C
                              UIFERR = VARSET
                           ENDIF
                        ELSE
C
C                          Error - couldn't set variable
C
                           UIFERR = VARSET
                        ENDIF
                     ELSE
C
C                       Error - couldn't set variable
C
                        UIFERR = VARSET
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ELSE
C
C           Error - couldn't initialise dynamic memory system
C
            UIFERR = BADINI
         ENDIF
      ENDIF
C
      UIFINI = STATUS
      RETURN
C
C Copyright (C) 1988-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
