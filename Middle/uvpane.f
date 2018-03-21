C       SUBROUTINE UVPANE
C       -----------------
C
C       PARAMETERS:
C
C       None.
C
C       Implements the UIF PANEL verb.  Allows the creation,
C       destruction, showing etc. of UIF panels.  Will set error
C       number correctly for Semper in case of UIF error.  Note that
C       the order of reading and acting on keys and options can be
C       important, so care must be taken if it is altered.
C
C----------------------------------------------------------------------
C
      SUBROUTINE UVPANE
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
C     Id of panel we are to work on
C
      INTEGER PID
C
C     Device panel is to be created on
C
      INTEGER DEVICE
C
C     Real value of a key
C
      REAL VALUE
C
C     Panel size
C
      INTEGER XSIZE, YSIZE
C
C     Panel position
C
      INTEGER XPOS, YPOS
C
C     Panel colours
C
      INTEGER FGC, BGC
C
C     Buffer length
C
      INTEGER BUFLEN
      PARAMETER (BUFLEN=LNLINB)
C
C     Buffer for reading name into as integer
C
      INTEGER IBUFF(BUFLEN)
      EQUIVALENCE (IBUFF,RB6)
C
C     Character form of the panel name
C
      CHARACTER*(BUFLEN) PNAME
C
C     Length of the name
C
      INTEGER LENGTH
C
C     Packed names for keys and options etc.
C
      INTEGER CREATE,TRANSI,ON,SIZE1,SIZE2,POS1,POS2,FORE,BACK
      INTEGER ID,NAME,AUTO,DESTRO,HIDE,MANDAT,SHOW
C      INTEGER CDI
C
      PARAMETER (CREATE=5525,TRANSI=-722,ON=24560,SIZE1=30786)
      PARAMETER (SIZE2=30792,POS1=26219,POS2=26232,FORE=10218,BACK=3243)
      PARAMETER (ID=14560,NAME=22453,AUTO=2460,DESTRO=6619,HIDE=13164)
      PARAMETER (MANDAT=20854,SHOW=30735)
C      PARAMETER (CDI=4969)
C
C     CALLED FUNCTIONS:
C
C     Checks for conflicting options
C
C      LOGICAL CONOPT
C
C     Checks to see if an option is set
C
      LOGICAL OPT
C
C     Creates a panel
C
      LOGICAL UIFPCR
C
C     Sets the size of a panel
C
      LOGICAL UIFPSI
C
C     Sets panel colours
C
      LOGICAL UIFPCO
C
C     Sets panel position
C
      LOGICAL UIFPPO
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
C     Sets panel name
C
      LOGICAL UIFPNA
C
C     Sets panel auto-sizing
C
      LOGICAL UIFPAU
C
C     Destroys a panel
C
      LOGICAL UIFPDE
C
C     Hides a panel
C
      LOGICAL UIFPHI
C
C     Shows a panel
C
      LOGICAL UIFPSH
C
C     Makes a panel mandatory
C
      LOGICAL UIFPMA
C
C     Makes a panel transitory
C
      LOGICAL UIFPTR
C
C     Get the identifier of the panel to work on: may involve
C     creating a new one
C
      IF ( OPT ( CREATE ) ) THEN
C
C        Create a new panel.  Get the device to create it on.
C
         STATUS = SEMLU ( -1, ON, VALUE )
         DEVICE = INT ( VALUE )
         STATUS = UIFPCR ( DEVICE, PID )
      ELSE IF ( SEMLU ( -1, ID, VALUE ) ) THEN
C
C        Get the panel id.
C
         PID = INT ( VALUE )
         STATUS = .FALSE.
      ENDIF
C
      IF ( .NOT. STATUS ) THEN
C
C        Get the rest of the keys and options, and act on them
C
         IF ( OPT ( DESTRO ) ) THEN
C
C           Destroy....
C
            STATUS = UIFPDE ( PID )
            GOTO 10
         ENDIF
         IF ( OPT ( HIDE ) ) THEN
C
C           Hide....
C
            STATUS = UIFPHI ( PID )
            IF ( STATUS ) GOTO 10
         ENDIF
         IF ( SEMLU ( -1, SIZE1, VALUE ) ) THEN
C
C           Size....
C
            XSIZE = INT ( VALUE )
            STATUS = SEMLU ( -1, SIZE2, VALUE )
            YSIZE = INT ( VALUE )
            STATUS = UIFPSI ( PID, XSIZE, YSIZE )
            IF ( STATUS ) GOTO 10
         ENDIF
         IF ( SEMLU ( -1, POS1, VALUE ) ) THEN
C
C           Position....
C
            XPOS = INT ( VALUE )
            STATUS = SEMLU ( -1, POS2, VALUE )
            YPOS = INT ( VALUE )
            STATUS = UIFPPO ( PID, XPOS, YPOS )
            IF ( STATUS ) GOTO 10
         ENDIF
         IF ( SEMLU ( -1, FORE, VALUE ) ) THEN
C
C           Colours....
C
            FGC = INT ( VALUE )
            STATUS = SEMLU ( -1, BACK, VALUE )
            BGC = INT ( VALUE )
            STATUS = UIFPCO ( PID, FGC, BGC )
            IF ( STATUS ) GOTO 10
         ENDIF
         IF ( VARSET ( NAME ) ) THEN
C
C           Name....
C
            LENGTH = BUFLEN
            STATUS = SEMTEX ( NAME, IBUFF, LENGTH )
            IF ( .NOT. STATUS ) THEN
C
C              Unpack the name into a character variable
C
               CALL SEMCHS ( PNAME, IBUFF, LENGTH )
               STATUS = UIFPNA ( PID, PNAME, LENGTH )
               IF ( STATUS ) GOTO 10
            ELSE
               GOTO 20
            ENDIF
         ENDIF
         IF ( OPT ( AUTO ) ) THEN
C
C           Auto sizing....
C
            STATUS = UIFPAU ( PID )
            IF ( STATUS ) GOTO 10
         ENDIF
         IF ( OPT ( MANDAT ) ) THEN
C
C           Make mandatory....
C
            STATUS = UIFPMA ( PID )
            IF ( STATUS ) GOTO 10
         ENDIF
         IF ( OPT ( TRANSI ) ) THEN
C
C           Make transitory....
C
            STATUS = UIFPTR ( PID )
            IF ( STATUS ) GOTO 10
         ENDIF
         IF ( OPT ( SHOW ) ) THEN
C
C           Show....
C
            STATUS = UIFPSH ( PID )
            IF ( STATUS ) GOTO 10
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
