C       LOGICAL FUNCTION UVELE2 ( TYPE, PID, EID )
C       ------------------------------------------
C
C       PARAMETERS:
C
C       integer type : INPUT - the type of element.
C
C       integer pid : OUTPUT - the panel on which the element resides.
C
C       integer eid : OUTPUT - the identifier of the element.
C
C       Implements the UIF general ELEMENT keys and options apart
C       from CREATE, ID and IN.  Note that the order of reading
C       and acting on keys and options can be important, so care must be
C       taken if it is altered.
C
C       Function returns TRUE in case of error, otherwise FALSE.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION UVELE2 ( TYPE, PID, EID )
C     ==========================================
C
      INTEGER TYPE, PID, EID
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
C     Element size
C
      INTEGER XSIZE, YSIZE
C
C     Element position
C
      INTEGER XPOS, YPOS
C
C     Element colours
C
      INTEGER FGC, BGC
C
C     Buffer length
C
      INTEGER BUFLEN
      PARAMETER (BUFLEN=LNLINB)
C
C     Buffer for reading name etc. into as integer
C
      INTEGER IBUFF(BUFLEN)
      EQUIVALENCE (IBUFF,RB6)
C
C     Character form of the element name etc.
C
      CHARACTER*(BUFLEN) PNAME
C
C     Length of the name etc.
C
      INTEGER LENGTH
C
C     Packed names of keys and options
C
      INTEGER SIZE1,SIZE2,POS1,POS2,FORE,BACK,NAME
      INTEGER DESTRO,XBEGIN,XCHANG,XENDS
C
      PARAMETER (SIZE1=30786,SIZE2=30792,POS1=26219)
      PARAMETER (POS2=26232,FORE=10218,BACK=3243,NAME=22453)
      PARAMETER (DESTRO=6619,XBEGIN=3407,XCHANG=5121)
      PARAMETER (XENDS=8564)
C
C     CALLED FUNCTIONS:
C
C     Checks that an elements type and id match
C
      LOGICAL UIFECK
C
C     Checks to see if an option is set
C
      LOGICAL OPT
C
C     Reads a key as an integer
C
      INTEGER IVAL
C
C     Destroys a element
C
      LOGICAL UIFEDE
C
C     Sets the size of a element
C
      LOGICAL UIFESI
C
C     Sets element colours
C
      LOGICAL UIFECO
C
C     Sets element position
C
      LOGICAL UIFEPO
C
C     Sets element actions
C
      LOGICAL UIFEAC
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
C     Sets element name
C
      LOGICAL UIFENA
C
C     Check that the type and identifer of the element match
C
      STATUS = UIFECK ( TYPE, EID )
      IF ( STATUS ) GOTO 10
C
C     Get the rest of the keys and options, and act on them
C
      IF ( OPT ( DESTRO ) ) THEN
C
C        Destroy....
C
         STATUS = UIFEDE ( EID )
         IF ( STATUS ) GOTO 10
      ENDIF
      IF ( SEMLU ( -1, SIZE1, VALUE ) ) THEN
C
C        Size....
C
         XSIZE = INT ( VALUE )
         YSIZE = IVAL ( SIZE2 )
         STATUS = UIFESI ( EID, XSIZE, YSIZE )
         IF ( STATUS ) GOTO 10
      ENDIF
      IF ( SEMLU ( -1, POS1, VALUE ) ) THEN
C
C        Position....
C
         XPOS = INT ( VALUE )
         YPOS = IVAL ( POS2 )
         STATUS = UIFEPO ( EID, XPOS, YPOS )
         IF ( STATUS ) GOTO 10
      ENDIF
      IF ( SEMLU ( -1, FORE, VALUE ) ) THEN
C
C        Colours....
C
         FGC = INT ( VALUE )
         BGC = IVAL ( BACK )
         STATUS = UIFECO ( EID, FGC, BGC )
         IF ( STATUS ) GOTO 10
      ENDIF
      IF ( VARSET ( NAME ) ) THEN
C
C        Name....
C
         LENGTH = BUFLEN
         STATUS = SEMTEX ( NAME, IBUFF, LENGTH )
         IF ( .NOT. STATUS ) THEN
C
C           Unpack the name into a character variable
C
            CALL SEMCHS ( PNAME, IBUFF, LENGTH )
            STATUS = UIFENA ( EID, PNAME, LENGTH )
            IF ( STATUS ) GOTO 10
         ELSE
            GOTO 20
         ENDIF
      ENDIF
      IF ( VARSET ( XBEGIN ) ) THEN
C
C        Begins action....
C
         LENGTH = BUFLEN
         STATUS = SEMTEX ( XBEGIN, IBUFF, LENGTH )
         IF ( .NOT. STATUS ) THEN
C
C           Unpack the name into a character variable
C
            CALL SEMCHS ( PNAME, IBUFF, LENGTH )
            STATUS = UIFEAC ( EID, BEGACT, PNAME, LENGTH )
            IF ( STATUS ) GOTO 10
         ELSE
            GOTO 20
         ENDIF
      ENDIF
      IF ( VARSET ( XCHANG ) ) THEN
C
C        Changes action (ignore for menus)....
C
         IF ( TYPE .NE. MENU ) THEN
            LENGTH = BUFLEN
            STATUS = SEMTEX ( XCHANG, IBUFF, LENGTH )
            IF ( .NOT. STATUS ) THEN
C
C              Unpack the name into a character variable
C
               CALL SEMCHS ( PNAME, IBUFF, LENGTH )
               STATUS = UIFEAC ( EID, CHAACT, PNAME, LENGTH )
               IF ( STATUS ) GOTO 10
            ELSE
               GOTO 20
            ENDIF
         ELSE
            STATUS = .FALSE.
         ENDIF
      ENDIF
      IF ( VARSET ( XENDS ) ) THEN
C
C        Ends action....
C
         LENGTH = BUFLEN
         STATUS = SEMTEX ( XENDS, IBUFF, LENGTH )
         IF ( .NOT. STATUS ) THEN
C
C           Unpack the name into a character variable
C
            CALL SEMCHS ( PNAME, IBUFF, LENGTH )
            STATUS = UIFEAC ( EID, ENDACT, PNAME, LENGTH )
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
      UVELE2 = STATUS
C
      RETURN
C
C Copyright (C) 1988-1992 :  Synoptics Ltd,  All Rights Reserved
C
      IJUNK = PID
      END
