C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION FSXW61 ( IX, IY, IERROR )
C       ------------------------------------------
C
C       PARAMETERS:
C
C       integer ix, iy : INPUT/OUTPUT - The initial position for the
C                        cursor on the currently viewed frame.  Is used
C                        to return final position.
C
C       integer ierror : OUTPUT - the error code, set on error
C
C       Activates the display cursor, at an initial position IX,IY (on
C       whichever frame is currently being viewed).  The position
C       finally chosen by the user is returned in IX,IY: the position
C       is indicated by pressing the left mouse button, or the return
C       key.
C
C       The cursor may also be moved by use of the arrow keys.
C       Using the C and F keys give coarser or finer stepping when
C       using the arrow keys, and using R or . will reposition the
C       cursor to the entry position.
C
C       The V key toggles automatic view centering.
C
C       Function returns FALSE if sucessful, otherwise TRUE with error
C       code set to 40 if some display related error occurs, and set to
C       4 if control-C is pressed.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION FSXW61(IX,IY,IERROR)
C
C     =====================================
C
      INTEGER IX, IY
      INTEGER IERROR
C
      INCLUDE 'COMMON'
C
C     LOCAL VARIABLES:
C
C     Return status
C
      LOGICAL STATUS
C
C     Cross wires style
C
      INTEGER KSTYLE,KXTYPE
C
C     Current frame
C
      INTEGER IFRAME
C
C     CALLED FUNCTIONS
C
C     Opens the event queues
C
      LOGICAL EVOPEN
C
C     Closes the event queues (back to saved state)
C
      LOGICAL EVCLOS
C
C     Performs tracking of the cursor etc.
C
      LOGICAL FSXWTR
C
      COMMON /XSTYLE/ KSTYLE,KXTYPE
C
      IF (KXTYPE .LT. 1 .OR. KXTYPE .GT. 9) KXTYPE = 2
C
      STATUS = EVOPEN ( .TRUE. ,.TRUE., .TRUE. )
      IF ( .NOT. STATUS ) THEN
         IFRAME = FSFRA
         STATUS = FSXWTR ( IFRAME, IX, IY, KSTYLE, KXTYPE, IERROR )
      ENDIF
C
C     Reset the queues. Note we must always reset all queues, but
C     maintain the error status correctly.
C
      IF ( EVCLOS ( ) ) STATUS = .TRUE.
      KSTYLE = 0
C
C     All done
C
      FSXW61 = STATUS
      RETURN
C
C Copyright (C) 1990-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION FSXWTR(IFR,IX,IY,STYLE,CTYPE,IERROR)
C       -----------------------------------------------------
C
C       PARAMETERS:
C
C       integer ifr    : INPUT - the frame to use
C
C       integer ix, iy : INPUT/OUTPUT - The initial position for the
C                        cursor on the currently viewed frame.  Is used
C                        to return final position.
C
C       integer ctype : INPUT/OUTPUT - xwires style
C
C       integer style : INPUT - cursor style
C                        0 = pointer
C                        1 = line
C                        2 = box
C
C       integer ierror : OUTPUT - the error code, set on error
C
C       Performs the actual tracking of the cursor, queue interrogation
C       and key interpretation etc.
C
C       Function returns FALSE if sucessful, otherwise TRUE with error
C       code set to 40 if some display related error occurs, and set to
C       4 if control-C is pressed.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION FSXWTR(IFR,IX,IY,STYLE,CTYPE,IERROR)
C
      INTEGER IFR,IX,IY,CTYPE,STYLE,IERROR
C
      INCLUDE 'COMMON'
C
      LOGICAL FSCC61,FSCT61,FSVQ61,FSXWPH,FSXWVF,PSIGMA
      LOGICAL FSXWVW
C
C     LOCAL VARIABLES:
C
C     Initial position for cursor
C
      INTEGER IXIN, IYIN
C
C     Viewing limits
C
      INTEGER XMIN, XMAX, YMIN, YMAX
C
C     Pointer position read from event queue
C
      INTEGER XPOS, YPOS
C
C     Pointer position deltas
C
      INTEGER XDELTA, YDELTA
C
C     Switch closed number and flag
C
      INTEGER CLOBUT
      LOGICAL CLOSED
C
C     Event detected
C
      INTEGER IEVENT
C
C     Event read
C
      INTEGER ICODE
C
C     Feedback position
C
      INTEGER FBX, FBY
C
C     My feedback type
C
      INTEGER MYTYPE
C
C     Finishing flag and broken flag
C
      LOGICAL DONE,BREAK
C
C     'Cursor move needed' flag
C
      LOGICAL MOVEIT
C
C     'View move needed' flag
C
      LOGICAL REVIEW
C
C     'View tracking' flag
C
      LOGICAL VIEWIT
C
C     Feedback visible flag
C
      LOGICAL ISVIEW
C
      FSXWTR = .TRUE.
C
C     Save initial position
C
      IXIN = IX
      IYIN = IY
      XPOS = IX
      YPOS = IY
      CLOSED = .FALSE.
      ISVIEW = .FALSE.
      REVIEW = .FALSE.
      VIEWIT = .FALSE.
      DONE = .FALSE.
      BREAK = .FALSE.
      IF (STYLE .EQ. 0) THEN
         MYTYPE = CTYPE
      ELSE
         MYTYPE = 0
      ENDIF
      IF (FSCT61(MYTYPE,IERROR)) GOTO 30
      IF (FSCC61(1,IFR,IX,IY,IERROR)) GOTO 30
C
C     OK.  Pointer queue will be tracking the cursor for us.  Loop
C     reading the queues until the button or return is pressed.
C
   10 IF (CLOSED) THEN
         XPOS = IX
         YPOS = IY
      ENDIF
C
      IF (.NOT.ISVIEW) THEN
         FBX = XPOS
         FBY = YPOS
         IF (FSCT61(0,IERROR)) GOTO 30
         IF (FSXWVF(IFR,STYLE,IXIN,IYIN,FBX,FBY,.TRUE.,IERROR)) GOTO 30
         IF (FSCT61(MYTYPE,IERROR)) GOTO 30
         CALL WAITS(0.05)
         ISVIEW = .TRUE.
      ENDIF
      IF (PSIGMA(XDELTA,YDELTA,IEVENT,ICODE)) THEN
         IF (ERROR .EQ. 4) THEN
            BREAK = .TRUE.
            IX = XPOS
            IY = YPOS
            DONE = .TRUE.
         ELSE
            GOTO 30
         ENDIF
      ELSE
         IF (IEVENT .EQ. 0) THEN
            IF (XDELTA .EQ. 0 .AND. YDELTA .EQ. 0) THEN
               CALL WAITS(0.05)
               GOTO 10
            ENDIF
         ENDIF
C
C Establish view limits
C
         IF (FSVQ61(IFR,DUMINT,XMIN,XMAX,YMIN,YMAX,IERROR)) GOTO 30
C
         MOVEIT = XDELTA .NE. 0 .OR. YDELTA .NE. 0
         IF (MOVEIT) THEN
            XPOS = XPOS + XDELTA
            XPOS = MAX(MIN(XPOS,XMAX),XMIN)
            YPOS = YPOS - YDELTA
            YPOS = MAX(MIN(YPOS,YMAX),YMIN)
         ENDIF
C
         IF (IEVENT .EQ. 3) THEN
            IF (CLOSED) THEN
               IF (CLOBUT .EQ. ICODE) DONE = .TRUE.
            ENDIF
         ELSE IF (IEVENT .EQ. 2) THEN
            IF (.NOT.CLOSED) THEN
               CLOSED = .TRUE.
               CLOBUT = ICODE
               IX = XPOS
               IY = YPOS
            ENDIF
         ELSE IF (IEVENT .EQ. 1) THEN
C
C        Keyboard event.  Read the character, and act on it if
C        it is one of the ones we know about, otherwise give some
C        help.
C
            IF ( ICODE .EQ. KBRET ) THEN
C
C           All done!
C
               IX = XPOS
               IY = YPOS
               DONE = .TRUE.
            ELSE IF ( ICODE .EQ. KUCR .OR. ICODE .EQ. KLCR .OR.
     +                ICODE .EQ. KDOT ) THEN
C
C           Reposition to initial position
C
               XPOS = IXIN
               YPOS = IYIN
               MOVEIT = .TRUE.
               REVIEW = .TRUE.
            ELSE IF ( ICODE .EQ. KUCV .OR. ICODE .EQ. KLCV) THEN
C
C           Reposition the view
C
               IF ( VIEWIT ) THEN
                  VIEWIT = .FALSE.
               ELSE
                  VIEWIT = .TRUE.
                  MOVEIT = .TRUE.
               ENDIF
            ELSE IF (ICODE.GT.KZERO .AND. ICODE.LE.KNINE) THEN
               CTYPE = ICODE - KZERO
               IF (STYLE .EQ. 0) THEN
                  MYTYPE = CTYPE
                  MOVEIT = .TRUE.
               ENDIF
            ELSE
C
C           Some other character.  Give help info.
C
               IF (FSXWPH()) THEN
                  BREAK = .TRUE.
                  DONE = .TRUE.
               ENDIF
            ENDIF
         ENDIF
      ENDIF
C
C     Move the cursor if needed.  Make sure we dont fall
C     out of the window!
C
      IF ( VIEWIT ) REVIEW = .TRUE.
      IF ( MOVEIT .OR. REVIEW .OR. DONE ) THEN
         IF (FSCT61(0,IERROR)) GOTO 30
         IF ( ISVIEW ) THEN
            IF (FSXWVF(IFR,STYLE,IXIN,IYIN,FBX,FBY,
     +                 .FALSE.,IERROR)) GOTO 30
            ISVIEW = .FALSE.
         ENDIF
         XPOS = MAX(MIN(XPOS,XMAX),XMIN)
         YPOS = MAX(MIN(YPOS,YMAX),YMIN)
         IF ( REVIEW ) THEN
            IF (FSXWVW(IFR,XPOS,YPOS,
     +                     XMIN,XMAX,YMIN,YMAX,IERROR)) GOTO 30
         ENDIF
         IF (FSCC61(2,IFR,XPOS,YPOS,IERROR)) GOTO 30
      ENDIF
      REVIEW = .FALSE.
C
C     Loop again
C
      IF (.NOT.DONE) GOTO 10
C
C     Here when sucessfully tracked.  Set the cursor back to normal
C
      IF (FSCC61(3,IFR,IX,IY,IERROR)) GOTO 30
      STYLE = 0
      FSXWTR = BREAK
C
   20 RETURN
C
   30 IF (FSCC61(3,IFR,IX,IY,IERROR)) GOTO 20
      GOTO 20
C
C Copyright (C) 1990-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION FSXWVF ( IFR,STYLE,IX1,IY1,IX2,IY2,ON,IERROR )
C       ------------------------------------------------------------
C
C       PARAMETERS:
C
C       integer ifr   : INPUT - frame number
C
C       integer style : INPUT/OUTPUT - cursor style
C                        0 = pointer
C                        1 = line
C                        2 = box
C
C       integer ix1, iy1 : INPUT/OUTPUT - The initial position and
C               ix2, iy2    feedback position to use.
C
C       logical on : INPUT - draw if TRUE, undraw if FALSE
C
C       integer ierror : OUTPUT - Error number on error
C
C       Performs xwires feedback tracking in the requested style.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION FSXWVF ( IFR,STYLE,IX1,IY1,IX2,IY2,ON,IERROR )
C
C     ===============================================================
C
      INTEGER IFR, STYLE, IX1, IY1, IX2, IY2, IERROR
      LOGICAL ON
C
      LOGICAL FSRB61,FSFL61
C
      INTEGER IOP, IXM1, IXM2, IYM1, IYM2
C
      FSXWVF = .TRUE.
C
      IF ( ON ) THEN
         IOP = 1
      ELSE
         IOP = 2
      ENDIF
C
      IF ( STYLE .NE. 0 ) THEN
         IF ( STYLE .EQ. 1 .OR.
     +        STYLE .EQ. 2 .AND. (IX1 .EQ. IX2 .OR. IY1 .EQ. IY2) ) THEN
C
C           Draw a line
C
            IF (FSRB61(IOP,IFR,IX1,IY1,IX2,IY2,IERROR)) GOTO 10
         ELSE
C
C           Draw a box
C
            IXM1=MIN(IX1,IX2)
            IXM2=MAX(IX1,IX2)
            IYM1=MIN(IY1,IY2)
            IYM2=MAX(IY1,IY2)
C
            IF (FSRB61(IOP,IFR,IXM1,IYM1,IXM1,IYM2-1,IERROR)) GOTO 10
            IF (FSRB61(IOP,IFR,IXM1,IYM2,IXM2-1,IYM2,IERROR)) GOTO 10
            IF (FSRB61(IOP,IFR,IXM2,IYM2,IXM2,IYM1+1,IERROR)) GOTO 10
            IF (FSRB61(IOP,IFR,IXM2,IYM1,IXM1+1,IYM1,IERROR)) GOTO 10
         ENDIF
C
         IF (FSFL61(1,IERROR)) GOTO 10
      ENDIF
C
      FSXWVF = .FALSE.
C
   10 RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION FSXWVW (IFR,IX1,IY1,XMIN,XMAX,YMIN,YMAX,IERROR)
C       ----------------------------------------------------------------
C
C       PARAMETERS:
C
C       integer ifr   : INPUT - frame number
C
C       integer ix1,iy1 : INPUT - desired frame centre
C
C       integer xmin,xmax : OUTPUT - new monitor X limits
C       integer ymin,ymax : OUTPUT - new monitor Y limits
C
C       integer ierror : OUTPUT - Error number on error
C
C       Attempts to recentre current view about ix1,iy1 and then
C       returns the current viewing conditions.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION FSXWVW ( IFR,IX1,IY1,XMIN,XMAX,YMIN,YMAX,IERROR )
C
C     ==================================================================
C
      INTEGER IFR, IX1, IY1, XMIN, XMAX, YMIN, YMAX, IERROR
C
      LOGICAL FSVQ61,FSVW61
C
C     Returned status
C
      LOGICAL STATUS
C
C     Zoom factor
C
      INTEGER IZOOM
C
C     Border limits
C
      INTEGER IBLEF,IBRIG,IBTOP,IBBOT
C
      INCLUDE 'COMMON'
C
C     Get old view for zoom factor
C
      STATUS = FSVQ61(IFR,IZOOM,XMIN,XMAX,YMIN,YMAX,IERROR)
C
      IF (.NOT.STATUS) THEN
C
C        Convert border limits into display coordinates
C                (for blanking limits)
C
         IBLEF = NINT(FSXSCA*FSBLEF+FSXOFF)
         IBRIG = NINT(FSXSCA*FSBRIG+FSXOFF)
         IBTOP = NINT(FSYSCA*FSBTOP+FSYOFF)
         IBBOT = NINT(FSYSCA*FSBBOT+FSYOFF)
C
C        Limit IX1,IY1 by border limits later?
C
C        Set new view
C
         STATUS = FSVW61(FSLUT,IZOOM,0, IBRIG-IBLEF+1,IBBOT-IBTOP+1,
     +                   IBLEF,IBTOP, IX1,IY1,IFR,IERROR)
C
C        Re-inquire limits (XMIN etc. this time)
C
         IF (.NOT.STATUS) THEN
            STATUS = FSVQ61(IFR,IZOOM,XMIN,XMAX,YMIN,YMAX,IERROR)
         ENDIF
      ENDIF
C
      FSXWVW = STATUS
      RETURN
C
C Copyright (C) 1991:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C----------------------------------------------------------------------
C
C      LOGICAL FUNCTION FSXWPH
C      -----------------------
C
C      PARAMETERS:
C
C      None.
C
C      Function to print some help info. for FSXW61
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION FSXWPH( )
C
C     ==========================
C
      LOGICAL SEMSOP,SEMCON
C
      FSXWPH = .TRUE.
C
C Reset line count for paging
C
      IF (SEMSOP()) GOTO 10
C
C Output help information
C
      IF (SEMCON (
     + 'Use the mouse or cursor keys to move the cursor.' )) GOTO 10
      IF (SEMCON (
     + 'C and F keys can be used for coarser or finer stepping.'
     +  )) GOTO 10
      IF (SEMCON (
     + 'Any mouse button or <return> can be used to enter' )) GOTO 10
      IF (SEMCON ('the cursor position.' )) GOTO 10
      IF (SEMCON ('1-9 select cursor types.' )) GOTO 10
      IF (SEMCON (
     + 'R or . keys can be used to restore the entry position.'
     +  )) GOTO 10
      IF (SEMCON (
     + 'V key toggles automatic view centering on the current position.'
     +  )) GOTO 10
C
      FSXWPH = .FALSE.
C
   10 RETURN
C
C Copyright (C) 1990-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
