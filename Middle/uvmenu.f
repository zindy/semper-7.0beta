C       SUBROUTINE UVMENU
C       -----------------
C
C       PARAMETERS:
C
C       None.
C
C       Implements the UIF MENU verb.  Allows the creation,
C       destruction, showing etc. of UIF menus.  Will set error
C       number correctly for Semper in case of UIF error.  Note that
C       the order of reading and acting on keys and options can be
C       important, so care must be taken if it is altered.
C
C----------------------------------------------------------------------
C
      SUBROUTINE UVMENU
C     =================
C
      INCLUDE 'UIFCOM'
C
C     LOCAL VARIABLES:
C
C     Function return status
C
      LOGICAL STATUS
C
C     Id of the panel menu resides on
C
      INTEGER PID
C
C     Id of menu we are to work on
C
      INTEGER EID
C
C     Packed names for keys and options etc.
C
      INTEGER XACTIV,XCHOIC,XTOGGL,XDEACT,XFIXED,XPULLD,XPOPUP
C
      PARAMETER (XACTIV=1740,XCHOIC=5135,XTOGGL=-608)
      PARAMETER (XDEACT=6601,XFIXED=9984,XPULLD=26452,XPOPUP=26216)
C
C     CALLED FUNCTIONS:
C
C     Reads and acts on the common element keys and options
C
      LOGICAL UVELE1, UVELE2
C
C     Activates a menu
C
      LOGICAL UIFMAC
C
C     Deactivates a menu
C
      LOGICAL UIFMDA
C
C     Sets a menu's type
C
      LOGICAL UIFMTY
C
C     Sets a menu's style
C
      LOGICAL UIFMST
C
C     Checks to see if an option is set
C
      LOGICAL OPT
C
C     Get the identifier of the menu to work on: may involve
C     creating a new one
C
      STATUS = UVELE1 ( MENU, PID, EID )
      IF ( STATUS ) GOTO 20
C
C     Get the rest of the keys and options, and act on them
C
      IF ( OPT ( XACTIV ) ) THEN
C
C        Activate the menu...
C
         STATUS = UIFMAC ( EID )
         IF ( STATUS ) GOTO 10
      ENDIF
      IF ( OPT ( XDEACT ) ) THEN
C
C        De-activate the menu...
C
         STATUS = UIFMDA ( EID )
         IF ( STATUS ) GOTO 10
      ENDIF
      IF ( OPT ( XFIXED ) ) THEN
C
C        Set menu to FIXED type...
C
         STATUS = UIFMTY ( EID, FIXED )
         IF ( STATUS ) GOTO 10
      ENDIF
      IF ( OPT ( XPOPUP ) ) THEN
C
C        Set menu to POPUP type...
C
         STATUS = UIFMTY ( EID, POPUP )
         IF ( STATUS ) GOTO 10
      ENDIF
      IF ( OPT ( XPULLD ) ) THEN
C
C        Set menu to PULLDOWN type...
C
         STATUS = UIFMTY ( EID, PULLDN )
         IF ( STATUS ) GOTO 10
      ENDIF
      IF ( OPT ( XCHOIC ) ) THEN
C
C        Set menu to CHOICE style...
C
         STATUS = UIFMST ( EID, CHOICE )
         IF ( STATUS ) GOTO 10
      ENDIF
      IF ( OPT ( XTOGGL ) ) THEN
C
C        Set menu to TOGGLE style...
C
         STATUS = UIFMST ( EID, TOGGLE )
         IF ( STATUS ) GOTO 10
      ENDIF
C
C     Now do the rest of the element keys and options
C
      STATUS = UVELE2 ( MENU, PID, EID )
      IF ( STATUS ) GOTO 20
C
C     Check error status, and set the error number if needed
C
   10 CONTINUE
      IF ( STATUS ) CALL UIFAPE
C
   20 CONTINUE
C
C     All done.
C
      RETURN
C
C Copyright (C) 1988-1991:  Synoptics Ltd,  All Rights Reserved
C
      END
