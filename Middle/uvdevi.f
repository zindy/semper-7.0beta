C       SUBROUTINE UVDEVI
C       -----------------
C
C       PARAMETERS:
C
C       None.
C
C       Implements the UIF DEVICE verb.  Allows the setting of the
C       device on which panels are to appear, interaction is allowed,
C       etc.  Will set error number correctly for Semper in case of
C       UIF error.  Note that the order of reading and acting on keys
C       and options can be important, so care must be taken if it is
C       altered.
C
C----------------------------------------------------------------------
C
      SUBROUTINE UVDEVI
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
C     Id of device we are to work on
C
      INTEGER DEVICE
C
C     Real value of a key
C
      REAL VALUE
C
C     Packed names for keys and options etc.
C
      INTEGER XLOCAT, XQUERY, XSAVE, XRESTO, XACTIV, XREFRE
C
      PARAMETER (XLOCAT=19803,XQUERY=28045,XSAVE=30462,XRESTO=29019)
      PARAMETER (XACTIV=1740,XREFRE=29006)
C
C     CALLED FUNCTIONS:
C
C     Checks to see is a variable/key is set
C
      LOGICAL SEMLU
C
C     Sets the device for panels
C
C     LOGICAL UIFDLO
C
C     Sets the device interaction
C
      LOGICAL UIFDAC
C
C     Queries the size of a device
C
      LOGICAL UIFDQU
C
C     Saves the cursor position of a device
C
      LOGICAL UIFDSA
C
C     Restores the cursor position of a device
C
      LOGICAL UIFDRE
C
C     Refreshes the device
C
      LOGICAL UIFDRF
C
C     Get the keys and options, and act on them
C
C-------------------------------------
C      IF ( SEMLU ( -1, XLOCAT, VALUE ) ) THEN
CC
CC        Set device for elements....
Cc
C         DEVICE = INT ( VALUE )
C         STATUS = UIFDLO ( DEVICE )
C         IF ( STATUS ) GOTO 10
C      ENDIF
C-------------------------------------
      IF ( SEMLU ( -1, XACTIV, VALUE ) ) THEN
C
C        Set device with which interaction is allowed....
C
         DEVICE = INT ( VALUE )
         STATUS = UIFDAC ( DEVICE )
         IF ( STATUS ) GOTO 10
      ENDIF
      IF ( SEMLU ( -1, XQUERY, VALUE ) ) THEN
C
C        Query device size....
C
         DEVICE = INT ( VALUE )
         STATUS = UIFDQU ( DEVICE )
         IF ( STATUS ) GOTO 10
      ENDIF
      IF ( SEMLU ( -1, XSAVE, VALUE ) ) THEN
C
C        Save the cursor position of device....
C
         DEVICE = INT ( VALUE )
         STATUS = UIFDSA ( DEVICE )
         IF ( STATUS ) GOTO 10
      ENDIF
      IF ( SEMLU ( -1, XRESTO, VALUE ) ) THEN
C
C        Restore the cursor position of device....
C
         DEVICE = INT ( VALUE )
         STATUS = UIFDRE ( DEVICE )
         IF ( STATUS ) GOTO 10
      ENDIF
      IF ( SEMLU ( -1, XREFRE, VALUE ) ) THEN
C
C        Refresh the device....
C
         DEVICE = INT ( VALUE )
         STATUS = UIFDRF ( DEVICE )
         IF ( STATUS ) GOTO 10
      ENDIF
C
C     Check error status, and set the error number if needed
C
   10 CONTINUE
      IF ( STATUS ) CALL UIFAPE
C
C     All done.
C
      RETURN
C
C Copyright (C) 1988-1991:  Synoptics Ltd,  All Rights Reserved
C
      END
