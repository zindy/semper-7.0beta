C       SUBROUTINE UVMOUS
C       -----------------
C
C       PARAMETERS:
C
C       None.
C
C       Implements the UIF MOUSE verb.  Allows the definition of
C       mouse button actions, etc.
C
C----------------------------------------------------------------------
C
      SUBROUTINE UVMOUS
C     =================
C
      INCLUDE 'COMMON'
      INCLUDE 'UIFCOM'
C
C     LOCAL VARIABLES:
C
C     Function return status
C
      LOGICAL STATUS
C
C     Real value of a key
C
      REAL VALUE
C
C     Element id we are to position to
C
      INTEGER EID
C
C     current device (cdi)
C
      INTEGER CDI
C
C     Mouse position
C
      INTEGER XPOS, YPOS
C
C     'Set the mouse position' flag
C
      LOGICAL SETPOS
C---------------------
C
C     Button for which action is to be set
C      INTEGER BUTTON
C---------------------
C
C     Buffer length
C
      INTEGER BUFLEN
      PARAMETER (BUFLEN=LNLINB)
C
C     Buffer for reading string into as integer
C
      INTEGER IBUFF(BUFLEN)
      EQUIVALENCE (IBUFF,RB6)
C
C     Character form of the string
C
      CHARACTER*(BUFLEN) STRING
C
C     Length of the name
C
      INTEGER LENGTH
C
C     Packed names for keys and options etc.
C
      INTEGER XCENTR,XLEFTB,XRIGHT,XPOS1,XPOS2,XQUERY
      INTEGER XCDI, XID
C
      PARAMETER (XCENTR=5014,XLEFTB=19406,XRIGHT=29167)
      PARAMETER (XPOS1=26219,XPOS2=26232,XQUERY=28045)
      PARAMETER (XCDI=4969,XID=14560)
C
C     CALLED FUNCTIONS:
C
C     Checks to see if an option is set
C
      LOGICAL OPT
C
C     Checks to see is a variable/key is set
C
      LOGICAL VARSET
C
C     Checks to see is a variable/key is set and returns value
C
      LOGICAL SEMLU
C
C     Converts to character variable
C
      LOGICAL SEMTEX
C
C     Sets the mouse position
C
      LOGICAL UIFMOP
C
C     Sets the mouse position to an element
C
      LOGICAL UIFMOE
C
C     Queries the mouse position
C
      LOGICAL UIFMOQ
C
C     Sets a button action
C
      LOGICAL UIFMOB
C
C     Get the keys and options, and act on them
C
      SETPOS = .FALSE.
      XPOS = 0
      YPOS = 0
C
      STATUS = SEMLU ( -1, XCDI, VALUE )
      CDI = INT ( VALUE )
C
      IF ( SEMLU ( -1, XID, VALUE ) ) THEN
C
C        Set mouse to element/panel position
C
         EID = INT ( VALUE )
         STATUS = UIFMOE ( CDI, EID )
      ENDIF
C
      IF ( SEMLU ( -1, XPOS1, VALUE ) ) THEN
C
C        X position for the mouse
C
         XPOS = INT ( VALUE )
         SETPOS = .TRUE.
      ENDIF
      IF ( SEMLU ( -1, XPOS2, VALUE ) ) THEN
C
C        Y position for the mouse
C
         YPOS = INT ( VALUE )
         SETPOS = .TRUE.
      ENDIF
C
C     Set position if needed
C
      IF ( SETPOS ) THEN
C
C        Set mouse position
C
         STATUS = UIFMOP ( CDI, XPOS, YPOS )
      ENDIF
C
      IF ( VARSET ( XLEFTB ) ) THEN
C
C        Left button action.  Read it in.
C
         LENGTH = BUFLEN
         STATUS = SEMTEX ( XLEFTB, IBUFF, LENGTH )
         IF ( .NOT. STATUS ) THEN
C
C           Unpack the string into a character variable
C
            IF ( LENGTH .GT. 0 ) THEN
               CALL SEMCHS ( STRING, IBUFF, LENGTH )
            ENDIF
C
C           And set the action
C
            STATUS = UIFMOB ( LEFT, STRING, LENGTH )
         ELSE
            GOTO 10
         ENDIF
      ENDIF
      IF ( VARSET ( XCENTR ) ) THEN
C
C        Centre button action.  Read it in.
C
         LENGTH = BUFLEN
         STATUS = SEMTEX ( XCENTR, IBUFF, LENGTH )
         IF ( .NOT. STATUS ) THEN
C
C           Unpack the string into a character variable
C
            IF ( LENGTH .GT. 0 ) THEN
               CALL SEMCHS ( STRING, IBUFF, LENGTH )
            ENDIF
C
C           And set the action
C
            STATUS = UIFMOB ( CENTRE, STRING, LENGTH )
         ELSE
            GOTO 10
         ENDIF
      ENDIF
      IF ( VARSET ( XRIGHT ) ) THEN
C
C        Right button action.  Read it in.
C
         LENGTH = BUFLEN
         STATUS = SEMTEX ( XRIGHT, IBUFF, LENGTH )
         IF ( .NOT. STATUS ) THEN
C
C           Unpack the string into a character variable
C
            IF ( LENGTH .GT. 0 ) THEN
               CALL SEMCHS ( STRING, IBUFF, LENGTH )
            ENDIF
C
C           And set the action
C
            STATUS = UIFMOB ( RIGHT, STRING, LENGTH )
         ELSE
            GOTO 10
         ENDIF
      ENDIF
C
      IF ( OPT ( XQUERY ) ) THEN
C
C        Query the mouse position
C
         STATUS = UIFMOQ ( CDI )
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
C Copyright (C) 1988-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
