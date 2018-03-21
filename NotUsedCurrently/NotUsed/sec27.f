      SUBROUTINE RODENT
C
C  track mouse position with a polyline
C
C  SYNTAX
C  Rodent :RODENT  $1= frame  partition  picture  position=   po2=
C
      INCLUDE 'COMMON'
C
      REAL X(0:37),Y(0:37)
      EQUIVALENCE (X,RB1),(Y,RB2)
C
C  SEMPER ROUTINES:
C
      LOGICAL FSINIT,FSFLUS,FSDRAG,FSQTRA,FSOPTN,FSQMON
      LOGICAL EVOPEN,EVCLOS,EVFLUS,PSIGMA
      INTEGER IPACK
      REAL VAL
C
      REAL SCALEX,SCALEY,XOFF,YOFF,X0,Y0,SDX,SDY,XMIN,XMAX,YMIN,YMAX
      INTEGER FROM,MODE,DX,DY,IEVENT,ICODE
      INTEGER IERROR,FRAME,ZOOM
      LOGICAL DISPL
C
C  pick up value of POS,PO2
C
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
C  initialise polyline
C
      X(0)=7
      Y(0)=-22
      X(1)=-5
      Y(1)=-21
      X(2)=-4
      Y(2)=-18
      X(3)=8
      Y(3)=-15
      X(4)=24
      Y(4)=-11
      X(5)=24
      Y(5)=-3
      X(6)=20
      Y(6)=-1
      X(7)=7
      Y(7)=-5
      X(8)=5
      Y(8)=-4
      X(9)=6
      Y(9)=-2
      X(10)=-9
      Y(10)=-2
      X(11)=-15
      Y(11)=-4
      X(12)=-15
      Y(12)=-2
      X(13)=-27
      Y(13)=-3
      X(14)=-24
      Y(14)=-6
      X(15)=-27
      Y(15)=-3
      X(16)=-29
      Y(16)=-2
      X(17)=-25
      Y(17)=2
      X(18)=-27
      Y(18)=0
      X(19)=-20
      Y(19)=4
      X(20)=-18
      Y(20)=6
      X(21)=-20
      Y(21)=10
      X(22)=-16
      Y(22)=13
      X(23)=-11
      Y(23)=12
      X(24)=-12
      Y(24)=6
      X(25)=-15
      Y(25)=5
      X(26)=-16
      Y(26)=3
      X(27)=-10
      Y(27)=9
      X(28)=-5
      Y(28)=13
      X(29)=-2
      Y(29)=14
      X(30)=6
      Y(30)=18
      X(31)=12
      Y(31)=20
      X(32)=20
      Y(32)=19
      X(33)=22
      Y(33)=17
      X(34)=23
      Y(34)=13
      X(35)=23
      Y(35)=4
      X(36)=20
      Y(36)=-1
      X(37)=19
      Y(37)=-2
C
C  enquire monitor limits
C
      IF(FSQMON(FRAME,ZOOM,XMIN,XMAX,YMIN,YMAX)) RETURN
C
C  unset display flag ( polyline is NOT currently displayed )
C
      DISPL=.FALSE.
C
C  open interactive section of code
C
      IF(EVOPEN(.TRUE.,.TRUE.,.TRUE.)) RETURN
C
C  draw initial polyline at pos,po2
C
      IF(FSDRAG(1,X0,Y0,X(0),Y(0),38,.FALSE.)) GOTO 20
C
C  set display = "polyline is currently displayed"
C
      DISPL=.TRUE.
C
C  flush queues, graphics
C
      IF(EVFLUS(.TRUE.,.TRUE.,.TRUE.)) GOTO 20
      IF(FSFLUS()) GOTO 20
C
C  begin loop to track cursor position until key/button pressed
C
   10 CONTINUE
C
C  short delay: this ensures the polyline is visible
C
         CALL WAITS(0.05)
C
C  enquire total pointer displacement accumulated in queue
C
         IF(PSIGMA(DX,DY,IEVENT,ICODE)) GOTO 20
C
C  if no change loop
C
         IF(IEVENT .EQ. 0 .AND. DX .EQ. 0.0 .AND. DY .EQ. 0.0) GOTO 10
C
C  test events for terminating condition
C
         IF(IEVENT .NE. 0) GOTO 20
C
C  delete old polyline
C
         IF(FSDRAG(2,X0,Y0,X(0),Y(0),38,.FALSE.)) GOTO 20
         DISPL=.FALSE.
C
C  calculate new position, noting picture sampling factors
C
         X0=X0+REAL(DX)*SDX
         Y0=Y0+REAL(DY)*SDY
C
C  truncate to monitor limits
C
         X0=MAX(X0,XMIN)
         X0=MIN(X0,XMAX)
         Y0=MAX(Y0,YMIN)
         Y0=MIN(Y0,YMAX)
C
C  display at new location
C
         IF(FSDRAG(1,X0,Y0,X(0),Y(0),38,.FALSE.)) GOTO 20
         DISPL=.TRUE.
C
C  flush graphics
C
         IF(FSFLUS()) GOTO 20
C
C  loop
C
      GOTO 10
C
C  error handling, tidy, exit
C
   20 CONTINUE
C
C  reset error status
C
      IERROR=ERROR
      ERROR=0
C
C  delete plygon ONLY if it is displayed
C
      IF(DISPL) THEN
         IF(FSDRAG(2,X0,Y0,X(0),Y(0),38,.FALSE.)) THEN
            IF(IERROR .EQ. 0)IERROR=ERROR
         ENDIF
      ENDIF
C
C  reset error to previous value
C
      ERROR=IERROR
C
C  end intreactive section of code... it is now safe to return
C
      DUMLOG=EVCLOS()
C
      RETURN
      END
