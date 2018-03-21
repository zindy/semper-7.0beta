C Semper 6 processing module DRAG
C
      SUBROUTINE DRAG
C
C Drags lines, arcs, circles and open or closed curves.  The initial
C shape and position object is specified in the same way as for the
C MARK command.  DRAG moves the graphical object across the display
C according to any mouse movements and cursor key presses.  Dragging
C stops when a mouse button or keyboard key is pressed.  Unless the
C option NOVERIFY is given, the object is drawn in its final position.
C The final position of the object's anchor position is returned in the
C variables X and Y.
C
C Syntax:   Drag :DRAG >$fpp line circle arc region curve open closed +
C           tolerance=1 >$position >$size to= to2= radius= angle= an2= +
C           uv sampling= with= verify
C
      REAL VAL
      INTEGER IVAL,IVALPN !,IPACK
      LOGICAL FSOPTN,FSINIT,FSVIEW,FSCURV,FSELEC,FSQTRA,FSQBOR
      LOGICAL OPT,OPTNO,VARSET,CONOPT,SEMOPN,SEMLAB,SEMROW,SETVAR,DRAG2
C
      REAL R,X,Y,T1,T2,TOL,SAMPLE,THETA,DTHETA,UX,UY,VX,VY
      REAL XMIN,XMAX,YMIN,YMAX,XSCALE,YSCALE,XOFF,YOFF
      INTEGER CLASS,FORM,I,N,NCOL,NROW,NLAY,NPIC,OPC,USIZE,VSIZE,NLIST
      LOGICAL CLOSED
C
      INCLUDE 'COMMON'
C
      INTEGER LABEL(LNLAB)
      INTEGER LIST(LNBUF/LNINT)
      REAL    FDX(LNBUF/LNREAL)
      REAL    FDY(LNBUF/LNREAL)
      REAL    DX(LNBUF/LNREAL)
      REAL    DY(LNBUF/LNREAL)
C
      EQUIVALENCE (LABEL,RB1),(FDX,RB2),(FDY,RB3)
      EQUIVALENCE (LIST,RB4),(DX,RB5),(DY,RB6)
C
C Determine nature of graphics coordinates according to options FRAME,
C PARTITION and PICTURE
C
      IF (FSOPTN(OPC,N)) GOTO 80
C
C Set up specified graphics coordinates
C
      IF (FSINIT(OPC,N)) GOTO 80
C
C If option VIEW is set, switch view to area of interest
C
      IF (OPT(-3566)) THEN
         IF (FSVIEW()) GOTO 80
      ENDIF
C
C If RADIUS key is set, drag arc/circle
C
      IF (VARSET(28844)) THEN
C
C Determine centre position and radius
C
         X=VAL(26219)
         Y=VAL(26232)
         R=VAL(28844)
C
C Fault negative radius
C
         IF (R.LT.0.0) THEN
            ERROR=3
            IDERR=28844
            GOTO 80
         ENDIF
C
C Fetch graphics transformation parameters
C
         IF (FSQTRA(XSCALE,XOFF,YSCALE,YOFF)) GOTO 80
C
C If radius is less than half pixel unit, generate single vertex
C
         IF (R.LT.0.5/ABS(XSCALE)) THEN
            DX(1)=0.0
            DY(1)=0.0
            N=1
            CLOSED=.TRUE.
C
C Otherwise, generate full set of vertices for arc/circle
C
         ELSE
C
C Fetch value for TOLERANCE key
C
            TOL=VAL(-613)
C
C Fault bad value for TOLERANCE key
C
            IF (TOL.LE.0.0) THEN
               ERROR=3
               IDERR=-613
               GOTO 80
            ENDIF
C
C Limit arc/circle tolerance to equivalent of half pixel unit
C
            TOL=MAX(TOL,0.5/ABS(XSCALE))
C
C If key ANGLE is set, generate vertices for arc
C
            IF (VARSET(2167)) THEN
               T1=VAL(2167)
               T2=VAL(2192)
C
               DTHETA=T2-T1
C
   10          IF (DTHETA.LT.0.0) THEN
                  DTHETA=DTHETA+TWOPI
                  GOTO 10
               ENDIF
C
   20          IF (DTHETA.GT.TWOPI) THEN
                  DTHETA=DTHETA-TWOPI
                  GOTO 20
               ENDIF
C
               N=4*((MAX(INT(DTHETA/SQRT(8.0*TOL/R)),1)+3)/4)
C
               THETA=T1
               DTHETA=DTHETA/REAL(N)
C
               DO 30 I=1,N
                  DX(I)=R*COS(THETA)
                  DY(I)=R*SIN(THETA)
                  THETA=THETA+DTHETA
   30          CONTINUE
C
               N=N+1
               DX(N)=R*COS(THETA)
               DY(N)=R*SIN(THETA)
C
               CLOSED=.FALSE.
C
C Otherwise, generate vertices for circle
C
            ELSE
               N=4*((MAX(INT(TWOPI/SQRT(8.0*TOL/R)),1)+3)/4)
C
               THETA=0.0
               DTHETA=TWOPI/REAL(N)
C
               DO 40 I=1,N
                  DX(I)=R*COS(THETA)
                  DY(I)=R*SIN(THETA)
                  THETA=THETA+DTHETA
   40          CONTINUE
C
               CLOSED=.TRUE.
            ENDIF
         ENDIF
C
C Drag the arc/circle
C
         IF (DRAG2(X,Y,DX,DY,N,CLOSED)) GOTO 80
C
C Return dragged arc/circle centre position in Semper variables
C
         IF (SETVAR(-6401,X)) GOTO 80
         IF (SETVAR(-8001,Y)) GOTO 80
C
C If key SIZE is set, drag sub-region
C
      ELSE IF (VARSET(30786)) THEN
C
C Fetch value for SIZE key
C
         USIZE=IVAL(30786)
C
C Determine value for SI2 key
C
         IF (VARSET(30792)) THEN
            VSIZE=IVAL(30792)
         ELSE
            VSIZE=USIZE
         ENDIF
C
C Fault zero or negative size
C
         IF (USIZE.LE.0.OR.VSIZE.LE.0) THEN
            ERROR=3
            IDERR=30786
            GOTO 80
         ENDIF
C
C Determine value for SAMPLING key
C
         IF (VARSET(30453)) THEN
            SAMPLE=VAL(30453)
         ELSE
            SAMPLE=1.0
         ENDIF
C
C Fault zero or negative sampling interval
C
         IF (SAMPLE.LE.0.0) THEN
            ERROR=3
            IDERR=30453
            GOTO 80
         ENDIF
C
C Fault conflict between options LEFT and RIGHT
C
         IF (CONOPT(19406,29167)) GOTO 80
C
C Fault conflict between options TOP and BOTTOM
C
         IF (CONOPT(-617,3820)) GOTO 80
C
C Fetch graphics border limits
C
         IF (FSQBOR(XMIN,XMAX,YMIN,YMAX)) GOTO 80
C
C Determine X centre position of sub-region
C
         IF (OPT(19406)) THEN
            X=XMIN+SAMPLE*REAL(USIZE/2)
         ELSE IF (OPT(29167)) THEN
            X=XMAX-SAMPLE*REAL((USIZE-1)/2)
         ELSE
            X=0.0
         ENDIF
C
C Determine Y centre position of sub-region
C
         IF (OPT(3820))  THEN
            Y=YMIN+SAMPLE*REAL((VSIZE-1)/2)
         ELSE IF (OPT(-617)) THEN
            Y=YMAX-SAMPLE*REAL(VSIZE/2)
         ELSE
            Y=0.0
         ENDIF
C
C Add offset specified by keys POSITION and PO2
C
         X=X+VAL(26219)
         Y=Y+VAL(26232)
C
C Fault conflict between key ANGLE and option UV
C
         IF (VARSET(2167).AND.OPT(-2481)) THEN
            ERROR=60
            IDERR=2167
            IDERR2=-2481
            GOTO 80
         ENDIF
C
C Determine U and V vectors describing unit cell for sub-region
C
         IF (VARSET(2167)) THEN
            THETA=VAL(2167)
            UX=SAMPLE*COS(THETA)
            UY=SAMPLE*SIN(THETA)
            VX=-UY
            VY=UX
         ELSE IF (OPT(-2481)) THEN
            IF (VARSET(-1601)) THEN
               UX=VAL(-1601)
            ELSE
               UX=1.0
            ENDIF
C
            UY=VAL(-2881)
            VX=VAL(-3201)
C
            IF (VARSET(-4481)) THEN
               VY=VAL(-4481)
            ELSE
               VY=1.0
            ENDIF
         ELSE
            UX=SAMPLE
            UY=0.0
            VX=0.0
            VY=SAMPLE
         ENDIF
C
C Determine four vertices = corner points of sub-region
C
         DX(1)=-REAL(USIZE/2)*UX-REAL((VSIZE-1)/2)*VX
         DY(1)=-REAL(USIZE/2)*UY-REAL((VSIZE-1)/2)*VY
         DX(2)=DX(1)+REAL(USIZE-1)*UX
         DY(2)=DY(1)+REAL(USIZE-1)*UY
         DX(4)=DX(1)+REAL(VSIZE-1)*VX
         DY(4)=DY(1)+REAL(VSIZE-1)*VY
         DX(3)=DX(2)+DX(4)-DX(1)
         DY(3)=DY(2)+DY(4)-DY(1)
C
         N=4
         CLOSED=.TRUE.
C
C Drag the sub-region
C
         IF (DRAG2(X,Y,DX,DY,N,CLOSED)) GOTO 80
C
C Return dragged centre position of arc/circle in Semper variables
C
         IF (SETVAR(-6401,X)) GOTO 80
         IF (SETVAR(-8001,Y)) GOTO 80
C
C If key WITH is set, drag open/closed curve
C
      ELSE IF (VARSET(-5181)) THEN
C
C Fault conflict between options OPEN and CLOSED
C
         IF (CONOPT(24645,5295)) GOTO 80
C
C Fetch initial position for curve anchor point
C
         X=VAL(26219)
         Y=VAL(26232)
C
C Open picture containing the position list
C
         NPIC=IVALPN(-5181)
         IF (SEMOPN(1,NPIC,NCOL,NROW,NLAY,CLASS,FORM,LP1)) GOTO 80
C
C Fault picture which is not a position list
C
         IF (CLASS.NE.NCLPLI) THEN
            ERROR=6
            IDERR=NPIC
            GOTO 80
         ENDIF
C
C Fetch contents of position list
C
         IF (SEMROW(1,DX,NFMFP,1,1,LP1)) GOTO 80
         IF (SEMROW(1,DY,NFMFP,1,2,LP1)) GOTO 80
C
C See if curve is to be open or closed (option OPEN/CLOSED, default
C given by position list type)
C
         N=NCOL
         IF (OPT(24645)) THEN
            CLOSED=.FALSE.
         ELSE IF (OPT(5295)) THEN
            CLOSED=.TRUE.
         ELSE
            IF (SEMLAB(1,LABEL,LP1)) GOTO 80
            CLOSED=LABEL(LBPLTY).EQ.3
         ENDIF
C
C Fetch value for TOLERANCE key
C
         TOL=VAL(-613)
C
C Fault bad value for TOLERANCE key
C
         IF (TOL.LE.0.0) THEN
            ERROR=3
            IDERR=-613
            GOTO 80
         ENDIF
C
C Fetch graphics transformation parameters
C
         IF (FSQTRA(XSCALE,XOFF,YSCALE,YOFF)) GOTO 80
C
C Limit curve tolerance to equivalent of half pixel unit
C
         TOL=MAX(TOL,0.5/ABS(XSCALE))
C
C Filter redundant vertices down to given curve tolerance
C
         CALL PFILT2(DX,DY,1,NCOL,LIST,NLIST,TOL)
C
C Construct reduced list of curve positions
C
         DO 50 I=1,NLIST
            FDX(I)=DX(LIST(I))
            FDY(I)=DY(LIST(I))
   50    CONTINUE
C
C Drag the curve
C
         IF (DRAG2(X,Y,FDX,FDY,NLIST,CLOSED)) GOTO 80
C
C Return dragged curve anchor point in Semper variables
C
         IF (SETVAR(-6401,X)) GOTO 80
         IF (SETVAR(-8001,Y)) GOTO 80
C
C If TO key set, output vertices of dragged curve in position list
C
         IF (VARSET(-601)) THEN
C
C Open new position list picture
C
            NPIC=IVALPN(-601)
            LP2=LP1
            IF (SEMOPN(2,NPIC,NCOL,NROW,NLAY,NCLPLI,NFMFP,LP2)) GOTO 80
C
C Increment curve positions
C
            DO 60 I=1,NCOL
               FDX(I)=X+DX(I)
               FDY(I)=Y+DY(I)
   60       CONTINUE
C
C Store contents of position list
C
            IF (SEMROW(2,FDX,NFMFP,1,1,LP2)) GOTO 80
            IF (SEMROW(2,FDY,NFMFP,1,2,LP2)) GOTO 80
C
C Fetch picture label
C
            IF (SEMLAB(1,LABEL,LP2)) GOTO 80
C
C Set position list type in picture label (open or closed curve)
C
            IF (CLOSED) THEN
               LABEL(LBPLTY)=3
            ELSE
               LABEL(LBPLTY)=2
            ENDIF
C
C Store modified picture label
C
            IF (SEMLAB(2,LABEL,LP2)) GOTO 80
         ENDIF
C
C If key TO is set, drag a line
C Note:  WITH key takes precedance over TO key so that TO key can be 
C used
C        in combination with WITH key without triggering line dragging
C
      ELSE IF (VARSET(-601)) THEN
C
C Fetch start point of line
C
         X=VAL(26219)
         Y=VAL(26232)
C
C Set up vertices for line
C
         DX(1)=0.0
         DY(1)=0.0
         DX(2)=VAL(-601)-X
         DY(2)=VAL(-633)-Y
C
         N=2
         CLOSED=.FALSE.
C
C Drag the line
C
         IF (DRAG2(X,Y,DX,DY,N,CLOSED)) GOTO 80
C
C Return dragged start and end points of line in Semper variables
C
         IF (SETVAR(-6401,X)) GOTO 80
         IF (SETVAR(-8001,Y)) GOTO 80
         IF (SETVAR(-6961,X+DX(2))) GOTO 80
         IF (SETVAR(-8561,Y+DY(2))) GOTO 80
C
C Otherwise, do nothing
C
      ELSE
         N=0
      ENDIF
C
C Unless NOVERIFY option is set, draw dragged object
C
      IF (.NOT.OPTNO(-3419)) THEN
C
C Increment object positions
C
         DO 70 I=1,N
            DX(I)=X+DX(I)
            DY(I)=Y+DY(I)
   70    CONTINUE
C
C Draw object
C
         IF (FSCURV(DX,DY,N,CLOSED)) GOTO 80
      ENDIF
C
C Update current frame/partition/picture number
C
      IF (FSELEC()) GOTO 80
C
   80 RETURN
C
C Copyright (C) 1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module DRAG2
C
      LOGICAL FUNCTION DRAG2(X,Y,DX,DY,N,CLOSED)
C
C Drags open/closed curve given by array of N vertices in DX and DY.
C The initial position of the origin or anchor point is given by
C X and Y and the final position is returned in the same variables.
C If CLOSED is .TRUE., a line is drawn from the last to the first
C point of the curve.
C
C While the curve is being dragged the system cursor is turned off.
C An initial position for activating cursor control is given which
C coincides with the centre of gravity of the curve.  This ensures
C there is maximum freedom to drag the curve on systems where the
C system cursor has to be bound to the display window.
C
      REAL    X,Y,DX(*),DY(*)
      INTEGER N
      LOGICAL CLOSED
C
      LOGICAL FSQTRA,FSCTYP,FSCURS,FSDRAG,PSIGMA
      LOGICAL EVOPEN,EVFLUS,EVCLOS
C
      REAL XSCALE,YSCALE,XOFF,YOFF,XCURS,YCURS
      REAL XCG,YCG
      INTEGER IDX,IDY,IEVENT,ICODE,I
C
      DRAG2=.TRUE.
C
C Fetch graphics transformation parameters
C
      IF (FSQTRA(XSCALE,XOFF,YSCALE,YOFF)) GOTO 30
C
C Determine centre of gravity of object
C
      XCG=0.0
      YCG=0.0
C
      DO 10 I=1,N
         XCG=XCG+DX(I)
         YCG=YCG+DY(I)
   10 CONTINUE
C
      XCG=XCG/REAL(N)
      YCG=YCG/REAL(N)
C
      XCURS=X+XCG
      YCURS=Y+YCG
C
C Select hidden cursor type
C
      IF (FSCTYP(0)) GOTO 30
C
C Activate cursor handling (grab system cursor and make it disappear)
C
      IF (FSCURS(1,XCURS,YCURS)) GOTO 30
C
C Activate keyboard, pointer and button event queues
C
      IF (EVOPEN(.TRUE.,.TRUE.,.TRUE.)) GOTO 30
C
C Flush event queues
C
      IF (EVFLUS(.TRUE.,.TRUE.,.TRUE.)) GOTO 40
C
C Draw curve at initial position
C
      IF (FSDRAG(1,X,Y,DX,DY,N,CLOSED)) GOTO 40
C
C Pause to allow curve to remain visible for a while
C
      CALL WAITS(0.05)
C
C Track pointer movements until key or button pressed
C
   20 IF (PSIGMA(IDX,IDY,IEVENT,ICODE)) GOTO 40
C
C Pause a while if nothing is happening
C
      IF (IEVENT.EQ.0.AND.IDX.EQ.0.AND.IDY.EQ.0) THEN
         CALL WAITS(0.05)
         GOTO 20
      ENDIF
C
C Undraw curve at current position
C
      IF (FSDRAG(2,X,Y,DX,DY,N,CLOSED)) GOTO 40
C
C Increment curve anchor position
C
      X=X+REAL(IDX)/ABS(XSCALE)
      Y=Y+REAL(IDY)/ABS(YSCALE)
C
C Redraw curve at new position
C
      IF (FSDRAG(1,X,Y,DX,DY,N,CLOSED)) GOTO 40
C
C Pause to allow curve to remain visible for a while
C
      CALL WAITS(0.05)
C
C Keep scanning the event queues if no key or button pressed
C
      IF (.NOT.(IEVENT.EQ.1.OR.IEVENT.EQ.2)) GOTO 20
C
C Undraw curve at final position
C
      IF (FSDRAG(2,X,Y,DX,DY,N,CLOSED)) GOTO 40
C
C Restore state of event queues
C
      IF (EVCLOS()) GOTO 30
C
C De-activate cursor handling
C
      XCURS=X+XCG
      YCURS=Y+YCG
C
      IF (FSCURS(3,XCURS,YCURS)) GOTO 30
C
      DRAG2=.FALSE.
C
   30 RETURN
C
C Emergency recovery of state of event queues
C
   40 IF (EVCLOS()) GOTO 30
      GOTO 30
C
C Copyright (C) 1992:  Synoptics Ltd,  All Rights Reserved
C
      END
