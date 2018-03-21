C----------------------------------------------------------------------
C
C       SUBROUTINE UVUIF
C       ----------------
C
C       PARAMETERS:
C
C       None.
C
C       Implements the UIF verb.  Allows the creattion, saving,
C       running, stopping etc. of a UIF user interface.  Will set
C       error number correctly for Semper in case of UIF error.
C       Note that the order of reading and acting on keys and options
C       can be important, so care must be taken if it is altered.
C
C----------------------------------------------------------------------
C
      SUBROUTINE UVUIF
C     ================
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
C     Length of the text buffer
C
      INTEGER BUFLEN
      PARAMETER (BUFLEN=LNLINB)
C
C     Buffer for reading text etc into as integer
C
      INTEGER IBUFF(BUFLEN)
      EQUIVALENCE (RB6,IBUFF)
C
C     Character form of the text
C
      CHARACTER*(BUFLEN) ctext
C
C     Length of the text
C
      INTEGER LENGTH
C
C     Packed names for keys and options
C
      INTEGER XREAD,XSAVE,ENABLE,EXIT,GO,NOERRO,XSTOP,XSTAT
C
      PARAMETER (XREAD=29001,XSAVE=30462,ENABLE=8561,EXIT=8969)
      PARAMETER (GO=11800,NOERRO=23005,XSTOP=31215,XSTAT=31201)
C
C     CALLED FUNCTIONS:
C
C     Checks for conflicting options
C
      LOGICAL CONOPT
C
C     Checks to see is a variable/key is set
C
      LOGICAL VARSET
C
C     Extracts a string from the input buffer
C
      LOGICAL SEMTEX
C
C     Initialises UIF
C
      LOGICAL UIFINI
C
C     Saves a user interface
C
      LOGICAL UIFSAV
C
C     Reads a user interface
C
      LOGICAL UIFREA
C
C     Sets UIF running
C
      LOGICAL UIFIGO
C
C     Stops UIF
C
      LOGICAL UIFITE
C
C     Give current UIF status
C
      LOGICAL UIFSTA
C
C     Checks to see if an option is set
C
      LOGICAL OPT
C
C     Check for conflicting options
C
      IF ( CONOPT ( ERROR, NOERRO ) ) THEN
         STATUS = .TRUE.
      ELSE IF ( CONOPT ( GO, XSTOP ) ) THEN
         STATUS = .TRUE.
      ELSE
         STATUS = .FALSE.
      ENDIF
      IF ( STATUS ) GOTO 20
C
C     Get the keys and options, and act on them
C
      IF ( OPT ( ENABLE ) ) THEN
C
C           Initialise UIF...
C
         STATUS = UIFINI ( )
         IF ( STATUS ) GOTO 10
      ENDIF
      IF ( OPT ( GO ) ) THEN
C
C           Set UIF running
C
         STATUS = UIFIGO ( )
         IF ( STATUS ) GOTO 10
      ENDIF
      IF ( OPT ( EXIT ) ) THEN
C
C        Shut down UIF....
C
         STATUS = UIFITE ( )
         IF ( STATUS ) GOTO 10
      ENDIF
      IF ( OPT ( XSTOP ) ) THEN
C
C        Shut down UIF and leave Semper....
C
         STATUS = UIFITE ( )
         IF ( STATUS ) GOTO 10
         CALL SEMEND
      ENDIF
      IF ( OPT ( XSTAT ) ) THEN
C
C        Give current UIF status....
C
         STATUS = UIFSTA ( )
         IF ( STATUS ) GOTO 10
      ENDIF
      IF ( VARSET ( XSAVE ) ) THEN
C
C        Save to file....
C
         LENGTH = BUFLEN
         STATUS = SEMTEX ( XSAVE, IBUFF, LENGTH )
         IF ( .NOT. STATUS ) THEN
C
C           Unpack the string into a character variable
C
            CALL SEMCHS ( CTEXT, IBUFF, LENGTH )
            STATUS = UIFSAV ( CTEXT(1:LENGTH), LENGTH )
            IF ( STATUS ) GOTO 10
         ELSE
            GOTO 20
         ENDIF
      ENDIF
      IF ( VARSET ( XREAD ) ) THEN
C
C        Read from file....
C
         LENGTH = BUFLEN
         STATUS = SEMTEX ( XREAD, IBUFF, LENGTH )
         IF ( .NOT. STATUS ) THEN
C
C           Unpack the string into a character variable
C
            CALL SEMCHS ( CTEXT, IBUFF, LENGTH )
            STATUS = UIFREA ( CTEXT(1:LENGTH), LENGTH )
            IF ( STATUS ) GOTO 10
         ELSE
            GOTO 20
         ENDIF
      ENDIF
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
C Copyright (C) 1988-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
