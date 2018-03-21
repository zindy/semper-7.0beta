      SUBROUTINE DRWCRV
C
C  Draw continuous curve on the display for as long as the mouse
C  button is pressed. Terminate on button release or other event
C  (except pointer move, of course!). Output curve as TO plist.
C  Cursor style can be set by the keyboard by pressing
C  '0','1','2','3' or '4'.
C
C  SYNTAX
C  Draw :DRWCRV  $1=  $2=999  to=$2  frame  partition  picture
C                position=    po2=   closed
C
      INCLUDE 'COMMON'
C
C  buffers:
C    IDX,IDY        integer incremental pointer movements
C    X,Y            real pointer positions (sum of increments)
C    LABEL          used to hold picture label
C
      INTEGER BUFLEN
      PARAMETER (BUFLEN=LNBUF/LNREAL)
C
C  storage for picture label
C
      INTEGER LABEL(LNLAB)
C
C  storage for mouse increments and positions
C
      REAL X(BUFLEN),Y(BUFLEN)
      INTEGER IDX(BUFLEN),IDY(BUFLEN)
C
C  .. these can be corrupted by SEMOPN
C
      EQUIVALENCE (IDX(1),RB1(1)),(IDY(1),RB2(1))
C
C  ..these can not
C
      EQUIVALENCE (X(1),RB4(1)),(Y(1),RB5(1)),(LABEL(1),RB6(1))
C
C  SEMPER ROUTINES:
C
      LOGICAL FSINIT,FSFLUS,FSCURV,FSLINE,FSCTYP,FSCURS,FSQTRA,FSOPTN
      LOGICAL EVOPEN,EVCLOS,EVFLUS,PDELTA,PSIGMA,SEMOPN,SEMROW,SEMLAB
      LOGICAL OPT
      INTEGER IPACK,IVALPN
      REAL VAL
C
      REAL SCALEX,SCALEY,XOFF,YOFF,X0,Y0,SDX,SDY
      INTEGER TO,FROM,MODE,DX,DY,IEVENT,ICODE,NPOINT
      INTEGER I,NMOVE,NEND,IERROR,XTYPE
      LOGICAL CLOSED,ENOUGH,DONE
C
C  pick up value of CLOSED,TO,FROM,POS,PO2
C
      CLOSED=OPT(IPACK('CLOSED'))
      TO=IVALPN(IPACK('TO'))
      X0=VAL(IPACK('POS'))
      Y0=VAL(IPACK('PO2'))
C
C  initialise graphics, source display picture
C
      IF(FSOPTN(MODE,FROM)) RETURN
      IF(FSINIT(MODE,FROM)) RETURN
C
C  Determine x,y transformation
C
      IF(FSQTRA(SCALEX,XOFF,SCALEY,YOFF)) RETURN
C
C  calculate sampling factors
C
      SDX=ABS(1.0/SCALEX)
      SDY=ABS(1.0/SCALEY)
C
C  open interactive section of code
C
      IF(EVOPEN(.TRUE.,.TRUE.,.TRUE.)) RETURN
C
C  issue small cross-wires at pos, po2
C
      XTYPE=1
      IF(FSCTYP(XTYPE)) GOTO 40
      IF(FSCURS(1,X0,Y0)) GOTO 40
C
C  flush queues
C
      IF(EVFLUS(.TRUE.,.TRUE.,.TRUE.)) GOTO 40
C
C  begin loop to track cursor position until key/button pressed
C
   10 CONTINUE
C
C  short delay
C
         CALL WAITS(0.05)
C
C  enquire total pointer displacement accumulated in queue
C
         IF(PSIGMA(DX,DY,IEVENT,ICODE)) GOTO 40
C
C  calculate new cursor position, noting picture sampling factors
C
         X0=X0+REAL(DX)*SDX
         Y0=Y0+REAL(DY)*SDY
C
C  display cursor
C
         IF(FSCURS(2,X0,Y0)) GOTO 40
C
C  flush graphics
C
         IF(FSFLUS()) GOTO 40
C
C  test events for terminating condition, change cursor if required
C
         IF(ENOUGH(IEVENT,ICODE,.FALSE.,DONE,XTYPE)) GOTO 40
C
C  if not time to move on then loop
C
         IF(.NOT. DONE) GOTO 10
C
C-----------------------------------------------------
C  arrive here when it is time to start drawing curve
C-----------------------------------------------------
C
C .. initialise number of points in curve
      NPOINT=1
C .. initialise array of points
      X(1)=X0
      Y(1)=Y0
C
C  Begin loop tracing pointer path
C
   20 CONTINUE
C
C  read increments + NMOVE = number of movements
C
         NMOVE=BUFLEN
         IF(PDELTA(IDX,IDY,NMOVE,IEVENT,ICODE)) GOTO 40
C
C  test events for terminating condition, change cursor if required
C
         IF(ENOUGH(IEVENT,ICODE,.TRUE.,DONE,XTYPE)) GOTO 40
C
C  if nothing new or not time to move on then loop with wait
C
         IF(NMOVE .EQ. 0 .AND. .NOT. DONE) THEN
            CALL WAITS(0.05)
            GOTO 20
         ENDIF
C
C  ensure end point of array does not exceed row buffer length
C
         NEND=MIN(BUFLEN,NPOINT+NMOVE)
C
C  change new increments to displacements, scaled according to sampling
C
         DO 30 I=NPOINT+1,NEND
            X(I)=X(I-1)+REAL(IDX(I-NPOINT))*SDX
            Y(I)=Y(I-1)+REAL(IDY(I-NPOINT))*SDY
   30    CONTINUE
C
C  remove cursor (make it invisible so wont interfere with graphics)
C
         IF(FSCTYP(0)) GOTO 40
C
C  draw path up to current cursor position from previous cursor position
C
         IF(FSCURV(X(NPOINT),Y(NPOINT),NEND-NPOINT+1,.FALSE.)) GOTO 40
C
C  redraw cursor at new position
C
         X0=X(NEND)
         Y0=Y(NEND)
         IF(FSCURS(2,X0,Y0)) GOTO 40
C
C  make cursor visible
C
         IF(FSCTYP(XTYPE)) GOTO 40
C
C reset current number of points in X,Y
C
         NPOINT=NEND
C
C  flush graphics
C
         IF(FSFLUS()) GOTO 40
C
C  if row buffer is filled or we are done then it is time to leave,
C  else loop
C
         IF(NPOINT .LT. BUFLEN .AND. .NOT. DONE) GOTO 20
C
C-----------------------------------------------------------------
C  arrive here when it is time to leave the curve drawing command
C-----------------------------------------------------------------
C
   40 CONTINUE
C
C  reset error status
C
      IERROR=ERROR
      ERROR=0
C
C  delete cursor
C
      IF(FSCURS(3,X0,Y0)) THEN
         IF(IERROR .EQ. 0) IERROR=ERROR
         ERROR=0
      ENDIF
C
C  end interactive section of code... it is now safe to return
C
      IF(EVCLOS()) THEN
         IF(IERROR .EQ. 0) IERROR=ERROR
      ENDIF
C
C  any errors detected so far? IERROR = first detected error.
C
      ERROR=IERROR
      IF(ERROR .NE. 0) RETURN
C
C close curve on display if required
C
      IF(CLOSED) THEN
         IF(FSLINE(X(NEND),Y(NEND),X(1),Y(1))) RETURN
      ENDIF
C
C  store values to plist
C
         LP2=0
         IF(SEMOPN(2,TO,NEND,1,2,NCLPLI,NFMFP,LP2)) RETURN
         IF(SEMROW(2,X,NFMFP,1,1,LP2)) RETURN
         IF(SEMROW(2,Y,NFMFP,1,2,LP2)) RETURN
C
C  read picture label
C
      IF(SEMLAB(1,LABEL,LP2)) RETURN
C
C  closed curve ?
C
      IF(CLOSED) THEN
C
C  set 'closed' curve flag
C
         LABEL(LBPLTY)=3
C
      ELSE
C
C  set 'open' curve flag
C
         LABEL(LBPLTY)=2
C
      ENDIF
C
C  write modified picture label
C
      IF(SEMLAB(2,LABEL,LP2)) RETURN
C
      RETURN
      END
C
C
      LOGICAL FUNCTION ENOUGH(IEVENT,ICODE,OPEN,DONE,XTYPE)
C
C  return DONE=.TRUE. iff
C     OPEN .AND. mouse button open detected .OR.
C     .NOT. OPEN .AND. mouse button close detected .OR.
C     Key press detected .AND. key is not one of '0','1','2','3','4'
C
C  If keys '0'..'4' detected then the cursor style is set appropriately
C  ENOUGH returns .TRUE. in case of error
C
C  Semper routines
C
      INTEGER MKICHA
      LOGICAL FSCTYP
C
      LOGICAL OPEN,DONE
      INTEGER IEVENT,ICODE,XTYPE
C
      ENOUGH=.TRUE.
C
C  no event detected
C
      IF(IEVENT .EQ. 0) THEN
         DONE=.FALSE.
C
C  button open
C
      ELSE IF(IEVENT .EQ. 3 .AND. OPEN) THEN
         DONE=.TRUE.
C
C  check for mouse close
C
      ELSE IF(IEVENT .EQ. 2 .AND. .NOT. OPEN) THEN
         DONE=.TRUE.
C
C  if keyboard event
C
      ELSE IF(IEVENT .EQ. 1) THEN
C
C  if not a '0','1','2','3','4' key
C
         IF(ICODE .GT. MKICHA('4') .OR. ICODE .LT. MKICHA('0')) THEN
            DONE=.TRUE.
C
C  '0','1','2','3' or '4'
C
         ELSE
            DONE=.FALSE.
            XTYPE=ICODE-MKICHA('0')
            IF(FSCTYP(XTYPE)) RETURN
         ENDIF
C
C  unknown event code
C
      ELSE
         RETURN
      ENDIF
C
      ENOUGH=.FALSE.
      RETURN
      END
