C Semper 6 processing module SKETCH
C
      SUBROUTINE SKETCH
C
C Digitises a continuous curve on the display for as long as the mouse
C button is pressed (or between two key presses).  The result is output
C as a PLIST class picture (position list) in the form of a curve.  If
C the option CLOSED is set, the curve is closed, otherwise it is open.
C Sketching terminates when the mouse button is released (or when a
C second key is pressed or when the input buffer is full).  An
C approximation to the digitised curve can be obtained by setting the
C TOLERANCE key.  The resulting curve will not depart from the digitised
C curve by more than the specified value.  With a default tolerance of
C zero, any points that lie along a straight line segment will be
C filtered down to just the end points.  The POSITION keys specify where
C the cursor is first to appear.  If the VIEW option is set, the viewing
C conditions are set to centre the view on the area of interest.  The
C length of the curve is returned in variable P and the area of a closed
C curve is returned in variable A.
C
C Syntax:  Sketch :SKETCH $1= frame partition picture $2=999 to=$2 +
C                         tolerance=0 pos= po2= closed view
C
      LOGICAL FSCTYP,FSOPTN,FSINIT,FSVIEW,FSQTRA,FSCURS,FSCURV,FSLINE
      LOGICAL FSFLUS,EVOPEN,EVFLUS,EVCLOS,PSIGMA,PDELTA,SEMOPN,SEMROW
      LOGICAL SEMLAB,SEMLU,OPT
      INTEGER IVALPN
      REAL    VAL
C
      REAL    SCALEX,SCALEY,XOFF,YOFF,XCURS,YCURS,P,A,TOL
      INTEGER I,N,DX,DY,MODE,IEVE1,IEVE2,ICODE
      INTEGER NPOINT,NGOOD,NMOVE,NEND,NG
      LOGICAL CLOSED,FINISH
C
      INCLUDE 'COMMON'
C
      INTEGER LABEL(LNLAB),IB3(LNBUF/LNINT),IB4(LNBUF/LNINT)
      EQUIVALENCE (LABEL,RB1),(IB3,RB3),(IB4,RB4)
C
C Packed names
C
      INTEGER NTOLER,NCLOSE,NVIEW,NTO,NPOSIT,NPO2,NP,NA
      PARAMETER (NTOLER=-613, NCLOSE=5295, NVIEW=-3566, NTO=-601)
      PARAMETER (NPOSIT=26219, NPO2=26232, NP=25600, NA=1600)
C
C Minimum number of points accumulated before filtering pass
C
      INTEGER NMAX
      PARAMETER(NMAX=50)
C
C Fetch value for TOLERANCE key
C
      TOL=VAL(NTOLER)
C
C Fault bad value for TOLERANCE key
C
      IF (TOL.LT.0.0) THEN
         ERROR=3
         IDERR=NTOLER
         GOTO 90
      ENDIF
C
C See if option CLOSED is set
C
      CLOSED=OPT(NCLOSE)
C
C Select cursor type = small cross
C
      IF(FSCTYP(1)) GOTO 90
C
C Establish graphics mode
C
      IF(FSOPTN(MODE,N)) GOTO 90
C
C Initialise graphics
C
      IF(FSINIT(MODE,N)) GOTO 90
C
C If VIEW option is set, switch view to area of interest
C
      IF (OPT(NVIEW)) THEN
         IF (FSVIEW()) GOTO 90
      ENDIF
C
C Fetch graphics transformation parameters
C
      IF(FSQTRA(SCALEX,XOFF,SCALEY,YOFF)) GOTO 90
C
C Switch on cursor at position given by POSITION keys
C
      XCURS=VAL(NPOSIT)
      YCURS=VAL(NPO2)
C
      IF(FSCURS(1,XCURS,YCURS)) GOTO 90
C
C Activate keyboard, button and pointer event queues
C
      IF(EVOPEN(.TRUE.,.TRUE.,.TRUE.)) GOTO 90
C
C Flush event queues
C
      IF(EVFLUS(.TRUE.,.TRUE.,.TRUE.)) GOTO 100
C
C Track cursor until key or button pressed
C
   10 IF(PSIGMA(DX,DY,IEVE1,ICODE)) GOTO 100
C
C If nothing happened pause a while
C
      IF (IEVE1 .EQ. 0) THEN
         IF (DX .EQ. 0 .AND. DY .EQ. 0) THEN
            CALL WAITS(0.05)
            GOTO 10
         ENDIF
      ENDIF
C
C Increment cursor position (scale factors deal with under-sampling)
C
      XCURS=XCURS+REAL(DX)/ABS(SCALEX)
      YCURS=YCURS+REAL(DY)/ABS(SCALEY)
C
C Display cursor at new cursor position
C
      IF(FSCURS(2,XCURS,YCURS)) GOTO 100
C
C Keep scanning the event queues until a key or button is pressed
C
      IF(.NOT.(IEVE1.EQ.1.OR.IEVE1.EQ.2)) GOTO 10
C
C Key or button now pressed - start digitising the curve
C
C Set up start point of curve = current cursor position
C
      NPOINT=1
      RB1(1)=XCURS
      RB2(1)=YCURS
C
      NGOOD=1
      RB5(1)=XCURS
      RB6(1)=YCURS
C
C Initiate short wait to give events time to build up and to save
C overloading the host
C
   20 CALL WAITS(0.05)
C
C Fetch all pointer movements up to the next terminating event (if any)
C
      NMOVE=LNBUF/LNINT
      IF(PDELTA(IB3,IB4,NMOVE,IEVE2,ICODE)) GOTO 100
C
C See if required terminating event reported - another key press if
C key initially pressed or button up if key or button initially pressed
C
      FINISH=(IEVE1.EQ.1.AND.IEVE2.EQ.1).OR.
     +       (IEVE1.EQ.2.AND.(IEVE1.EQ.1.OR.IEVE2.EQ.3))
C
C If no data and no terminating event, keep scanning the event queues
C
      IF(.NOT.(NMOVE.GT.0.OR.FINISH)) GOTO 20
C
C Convert pointer increments into absolute displacements, appropriately
C scaled
C
      NEND=MIN(LNBUF/LNREAL,NPOINT+NMOVE)
      DO 30 I=NPOINT+1,NEND
         RB1(I)=RB1(I-1)+REAL(IB3(I-NPOINT))/ABS(SCALEX)
         RB2(I)=RB2(I-1)+REAL(IB4(I-NPOINT))/ABS(SCALEY)
   30 CONTINUE
C
C Hide the cursor
C
      IF(FSCTYP(0)) GOTO 100
C
C Draw curve up to current cursor position
C
      IF(FSCURV(RB1(NPOINT),RB2(NPOINT),NEND-NPOINT+1,.FALSE.)) GOTO 100
C
C Flush out any graphics output that may have been buffered
C
      IF(FSFLUS()) GOTO 100
C
C Update the number of points accumulated
C
      NPOINT=NEND
C
C New cursor position = end point of curve
C
      XCURS=RB1(NPOINT)
      YCURS=RB2(NPOINT)
C
C Move the cursor (note the curve can go outside the picture, partition
C or frame limits)
C
      IF(FSCURS(2,XCURS,YCURS)) GOTO 100
C
C Now make the cursor visible again
C
      IF(FSCTYP(1)) GOTO 100
C
C If there are enough points accumulated, go away and filter the data
C
      IF (NPOINT.GT.NMAX) THEN
C
C Establish subset of points that approximates the original curve to
C within the given tolerance
C
         CALL PFILT2(RB1,RB2,1,NPOINT,IB3,NG,TOL)
C
C If too many points left over, include the last point as a good point
C
         IF (IB3(NG)-IB3(NG-1)+1.GT.NMAX) THEN
            N=NG
         ELSE
            N=NG-1
         ENDIF
C
C Copy the good points, omitting the first point which already appears
C in the output buffer - flag buffer full as terminating condition
C
         DO 40 I=2,N
            IF (NGOOD.LT.LNBUF/LNREAL) THEN
               NGOOD=NGOOD+1
               RB5(NGOOD)=RB1(IB3(I))
               RB6(NGOOD)=RB2(IB3(I))
            ELSE
               FINISH=.TRUE.
            ENDIF
   40    CONTINUE
C
C Retain unfiltered points for next filtering pass (first point is
C always last good point)
C
         NPOINT=0
         DO 50 I=IB3(N),IB3(NG)
            NPOINT=NPOINT+1
            RB1(NPOINT)=RB1(I)
            RB2(NPOINT)=RB2(I)
   50    CONTINUE
      ENDIF
C
C If no terminating event detected, go back for more
C
      IF(.NOT.FINISH) GOTO 20
C
C Filter any remaining points
C
      IF (NPOINT.GT.1) THEN
         CALL PFILT2(RB1,RB2,1,NPOINT,IB3,NG,TOL)
C
C Add any good points to the output buffer
C
         DO 60 I=2,NG
            IF (NGOOD.LT.LNBUF/LNREAL) THEN
               NGOOD=NGOOD+1
               RB5(NGOOD)=RB1(IB3(I))
               RB6(NGOOD)=RB2(IB3(I))
            ENDIF
   60    CONTINUE
      ENDIF
C
C Restore state of event queues to that before the call to EVOPEN
C
      IF(EVCLOS()) GOTO 90
C
C Finally switch off the cursor
C
      IF(FSCURS(3,XCURS,YCURS)) GOTO 90
C
C If closed curve, draw closing line from end point to start point
C
      IF(CLOSED) THEN
         IF(FSLINE(RB5(NGOOD),RB6(NGOOD),RB5(1),RB6(1))) GOTO 90
      ENDIF
C
C Open new output picture
C
      LP2=0
      IF(SEMOPN(2,IVALPN(NTO),NGOOD,1,2,NCLPLI,NFMFP,LP2)) GOTO 90
C
C Store results in output picture
C
      IF(SEMROW(2,RB5,NFMFP,1,1,LP2)) GOTO 90
      IF(SEMROW(2,RB6,NFMFP,1,2,LP2)) GOTO 90
C
C Read picture label
C
      IF(SEMLAB(1,LABEL,LP2)) GOTO 90
C
C Set flag in picture label for open/closed curve
C
      IF(CLOSED) THEN
         LABEL(LBPLTY)=3
      ELSE
         LABEL(LBPLTY)=2
      ENDIF
C
C Write picture label
C
      IF(SEMLAB(2,LABEL,LP2)) GOTO 90
C
C If closed curve, add extra point = start point
C
      IF (CLOSED) THEN
         NGOOD=NGOOD+1
         RB5(NGOOD)=RB5(1)
         RB6(NGOOD)=RB6(1)
      ENDIF
C
C Determine curve length and return in variable P
C
      P=0.0
      DO 70 I=1,NGOOD-1
         P=P+SQRT((RB5(I+1)-RB5(I))**2+(RB6(I+1)-RB6(I))**2)
   70 CONTINUE
C
      IF (SEMLU(1,NP,P)) GOTO 90
C
C If closed curve, determine enclosed area and return value in
C variable A
C
      IF (CLOSED) THEN
         A=0.0
         DO 80 I=1,NGOOD-1
            A=A+(RB5(I)*RB6(I+1)-RB5(I+1)*RB6(I))/2.0
   80    CONTINUE
C
         IF(SEMLU(1,NA,A)) GOTO 90
      ENDIF
C
   90 RETURN
C
C Emergency recovery of state of event queues
C
  100 IF (EVCLOS()) GOTO 90
      GOTO 90
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
