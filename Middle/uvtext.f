C       SUBROUTINE UVTEXT
C       -----------------
C
C       PARAMETERS:
C
C       None.
C
C       Implements the UIF TETXFIELD verb.  Allows the creation,
C       destruction, showing etc. of UIF textfields.  Will set error
C       number correctly for Semper in case of UIF error.  Note that
C       the order of reading and acting on keys and options can be
C       important, so care must be taken if it is altered.
C
C----------------------------------------------------------------------
C
      SUBROUTINE UVTEXT
C     =================
C
      INCLUDE 'COMMON'
      INCLUDE 'UIFCOM'
C
C     LOCAL VARIABLES:
C
C     Id of the panel textfield resides on
C
      INTEGER PID
C
C     Id of textfield we are to work on
C
      INTEGER EID
C
C     'Tab target' of a textfield
C
      INTEGER OTHER
C
C     Real value of a key
C
      REAL VALUE
C
C     Length of the text buffer
C
      INTEGER BUFLEN
      PARAMETER (BUFLEN=LNLINB)
C
C     Buffer for reading text etc into as integer
C
      INTEGER IBUFF(BUFLEN)
      EQUIVALENCE(IBUFF,RB6)
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
      INTEGER XAPPEN, XASSIG, XCONTE, XLENGT, XNUMER, XOUTSI, XTAB
      INTEGER XVALUE, XEXECU, XACTIV, XCLEAR, XPRIOR, XWP, XOFF
      INTEGER XCREAT, XIMMED
C
      PARAMETER (XAPPEN=2256,XASSIG=2379,XCONTE=5414,XLENGT=19414)
      PARAMETER (XNUMER=23253,XOUTSI=24860,XTAB=-43,XVALUE=-3253)
      PARAMETER (XEXECU=8965,XACTIV=1740,XCLEAR=5285,XPRIOR=26329)
      PARAMETER (XWP=-5441,XOFF=24246,XCREAT=5525,XIMMED=14933)
C
C     CALLED FUNCTIONS:
C
C     Reads and acts on the common element keys and options
C
      LOGICAL UVELE1, UVELE2
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
C     Extracts a string from the input buffer
C
      LOGICAL SEMTEX
C
C     Appends a string to the contents of a textfield
C
      LOGICAL UIFTAP
C
C     Sets the textfield cursor position
C
      LOGICAL UIFTCP
C
C     Inserts a string into the contents of a textfield
C
      LOGICAL UIFTIN
C
C     Assigns contents of string into a variable
C
      LOGICAL UIFTAS
C
C     Sets a string as the contents of a textfield
C
      LOGICAL UIFTCO
C
C     Sets the length of a textfield
C
      LOGICAL UIFTLE
C
C     Sets the 'tab target' of a textfield
C
      LOGICAL UIFTTA
C
C     Sets the contents of a textfield to the value of an expression
C
      LOGICAL UIFTVA
C
C     Executes the contents of a textfield
C
      LOGICAL UIFTEX
C
C     Selects a textfield
C
      LOGICAL UIFTSE
C
C     Sets write protection on a textfield
C
      LOGICAL UIFTWP
C
C     Get the identifier of the textfield to work on: may involve
C     creating a new one
C
      IF ( UVELE1 ( TEXFLD, PID, EID ) ) GOTO 10
      IF ( UVELE2 ( TEXFLD, PID, EID ) ) GOTO 10
C
C     Get the rest of the keys and options, and act on them
C
      IF ( SEMLU ( -1, XLENGT, VALUE ) ) THEN
C
C        Set textfield length....
C
         LENGTH = INT ( VALUE )
         IF ( UIFTLE ( EID, LENGTH ) ) GOTO 20
      ELSE
         IF ( OPT ( XCREAT ) ) THEN
C
C           Length key not given, but if we are creating the textfield,
C           then make sure it is set up with the default length.
C
            LENGTH = 5
            IF ( UIFTLE ( EID, LENGTH ) ) GOTO 20
         ENDIF
      ENDIF
      IF ( OPT ( XACTIV ) ) THEN
C
C        Select the textfield....
C
         IF ( UIFTSE ( EID ) ) GOTO 20
      ENDIF
      IF ( VARSET ( XAPPEN ) ) THEN
C
C        Append string....
C
         LENGTH = BUFLEN
         IF ( SEMTEX ( XAPPEN, IBUFF, LENGTH ) ) GOTO 10
C
C           Unpack the string into a character variable
C
         CALL SEMCHS ( CTEXT, IBUFF, LENGTH )
         IF ( UIFTAP ( EID, CTEXT, LENGTH ) ) GOTO 20
      ENDIF
      IF ( VARSET ( XPRIOR ) ) THEN
C
C        Prepend string....
C
         LENGTH = BUFLEN
         IF ( SEMTEX ( XPRIOR, IBUFF, LENGTH ) ) GOTO 10
C
C           Unpack the string into a character variable
C
         CALL SEMCHS ( CTEXT, IBUFF, LENGTH )
C
C           Set the cursor to the beginning of the field
C
         IF ( UIFTCP ( EID, ABSOL, 1 ) ) GOTO 20
C
C           And insert the string
C
         IF ( UIFTIN ( EID, CTEXT, LENGTH ) ) GOTO 20
      ENDIF
      IF ( VARSET ( XASSIG ) ) THEN
C
C        Assign textfield contents to variable....
C
         LENGTH = BUFLEN
         IF ( SEMTEX ( XASSIG, IBUFF, LENGTH ) ) GOTO 10
C
C           Unpack the string into a character variable
C
         CALL SEMCHS ( CTEXT, IBUFF, LENGTH )
         IF ( UIFTAS ( EID, CTEXT, LENGTH ) ) GOTO 20
      ENDIF
      IF ( VARSET ( XCONTE ) ) THEN
C
C        Set contents to string....
C
         LENGTH = BUFLEN
         IF ( SEMTEX ( XCONTE, IBUFF, LENGTH ) ) GOTO 10
C
C           Unpack the string into a character variable
C
         CALL SEMCHS ( CTEXT, IBUFF, LENGTH )
         IF ( UIFTCO ( EID, CTEXT, LENGTH ) ) GOTO 20
      ENDIF
      IF ( VARSET ( XNUMER ) ) THEN
C
C        Set textfield to be numeric....
C
      ENDIF
      IF ( OPT ( XWP ) ) THEN
C
C        Set write ptoection on or off....
C
         IF ( OPT ( XOFF ) ) THEN
            IF ( UIFTWP ( EID, .FALSE. ) ) GOTO 20
         ELSE
            IF ( UIFTWP ( EID, .TRUE. ) ) GOTO 20
         ENDIF
      ENDIF
      IF ( VARSET ( XOUTSI ) ) THEN
C
C        Set 'outside range' action....
C
      ENDIF
      IF ( SEMLU ( -1, XTAB, VALUE ) ) THEN
C
C        Set tab target....
C
         OTHER = INT ( VALUE )
         IF ( UIFTTA ( OTHER, EID ) ) GOTO 20
      ENDIF
      IF ( VARSET ( XVALUE ) ) THEN
C
C        Set contents to value of numeric expression....
C
         LENGTH = BUFLEN
         IF ( SEMTEX ( XVALUE, IBUFF, LENGTH ) ) GOTO 10
C
C           Unpack the string into a character variable
C
         CALL SEMCHS ( CTEXT, IBUFF, LENGTH )
         IF ( UIFTVA ( EID, CTEXT, LENGTH ) ) GOTO 20
      ENDIF
      IF ( OPT ( XCLEAR ) ) THEN
C
C        Clear the textfield....
C
         IF ( UIFTCO ( EID, ' ', 0 ) ) GOTO 20
      ENDIF
      IF ( OPT ( XEXECU ) ) THEN
C
C        Execute the contents of the textfield....
C
         IF ( UIFTEX ( EID, OPT ( XIMMED ) ) ) GOTO 20
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
