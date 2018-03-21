C----------------------------------------------------------------------
C
C       SUBROUTINE UVEXEC
C       -----------------
C
C       PARAMETERS:
C
C       None.
C
C       Implements the UIF EXECUTE verb.  Allows the definition
C       of actions to be performed before and after command execution
C       by Semper. Will set error number correctly for Semper in case
C       of UIF error.
C
C----------------------------------------------------------------------
C
      SUBROUTINE UVEXEC
C     =================
C
      INCLUDE 'COMMON'
      INCLUDE 'UIFCOM'
C
C     LOCAL VARIABLES:
C
C     Buffer length
C
      INTEGER BUFLEN
      PARAMETER (BUFLEN=LNLINB)
C
C     Buffer for reading text into as integer
C
      INTEGER IBUFF(BUFLEN)
      EQUIVALENCE (IBUFF,RB6)
C
C     Character form of the text
C
      CHARACTER*(BUFLEN) CTEXT
C
C     Length of the text
C
      INTEGER LENGTH
C
C     Packed names for keys and options etc.
C
      INTEGER BEFORE,AFTER,ON,OFF
      PARAMETER (BEFORE=3406,AFTER=1860,ON=24560,OFF=24246)
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
C     Extracts a string from the input buffer
C
      LOGICAL SEMTEX
C
C     Enables or disables before and after processing
C
      LOGICAL UIFIBA
C
C     Sets the 'before command' action
C
      LOGICAL UIFIBE
C
C     Sets the 'after command' action
C
      LOGICAL UIFIAF
C
C     Get the keys and options, and act on them
C
      IF ( OPT ( ON ) ) THEN
C
C        Enable before and after processing....
C
         IF ( UIFIBA ( .TRUE. ) ) GOTO 20
      ENDIF
      IF ( OPT ( OFF ) ) THEN
C
C        Disable before and after processing....
C
         IF ( UIFIBA ( .FALSE. ) ) GOTO 20
      ENDIF
      IF ( VARSET ( BEFORE ) ) THEN
C
C        Before action....
C
         LENGTH = BUFLEN
         IF ( SEMTEX ( BEFORE, IBUFF, LENGTH ) ) GOTO 10
C
C           Unpack the text into a character variable
C
         CALL SEMCHS ( CTEXT, IBUFF, LENGTH )
         IF ( UIFIBE ( CTEXT, LENGTH ) ) GOTO 20
      ENDIF
      IF ( VARSET ( AFTER ) ) THEN
C
C        After action....
C
         LENGTH = BUFLEN
         IF ( SEMTEX ( AFTER, IBUFF, LENGTH ) ) GOTO 10
C
C           Unpack the text into a character variable
C
         CALL SEMCHS ( CTEXT, IBUFF, LENGTH )
         IF ( UIFIAF ( CTEXT, LENGTH ) ) GOTO 20
      ENDIF
C
   10 CONTINUE
C
C     All done.
C
      RETURN
C
   20 CALL UIFAPE
      GOTO 10
C
C Copyright (C) 1988-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
