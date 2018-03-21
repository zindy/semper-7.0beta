C Semper 6 processing module PXCON
C
      SUBROUTINE PXCON
C
C Inserts contour lines into picture at heights evenly spaced
C over stated RANGE, i.e. resets to VALUE the outermost pixels
C of closed regions bounded by each contour height.
C
C RB2, RB3 buffer the picture rows (with a computed switch)
C SEGS, OSEGS hold lists of row segments GE each contour
C height. LEVELS limited to 30 (arbitrarily), and number of crossings
C per level limited by row buffer (workspace) length
C
      LOGICAL SEMROW,SEMLU,OPT,SEMCON
      INTEGER IVAL
      REAL VAL
C
      INCLUDE 'COMMON'
C
      INTEGER MXLEVL
      PARAMETER (MXLEVL=30)
      INTEGER SEGS(256),OSEGS(256),SEGNUM(MXLEVL),OLSTSG(MXLEVL)
      EQUIVALENCE (RB1,SEGS),(RB4,OSEGS)
      EQUIVALENCE (RB5,SEGNUM),(RB5(31),OLSTSG)
      REAL H(MXLEVL)
      EQUIVALENCE (RB5(61),H)
C
      INTEGER SPTR,OSPTR,OLST,OBUF,NROW,NCOL,NCM1,NBUF,IHMAX,NAME,IH
      INTEGER MAXSEG,I,I0,I1,I2,I3,J,LAST,LST,IBUF,ITEST
      LOGICAL VSET,LOW,HIGH
      REAL V,RMIN,RMAX,X,H1,H2,T,PIX
C
C Packed names
C
      INTEGER NLEVEL,NVALUE,NVERIF,NLN,NL,NL1,NLIST,NRANGE,NRA2,NWITH
      PARAMETER (NLEVEL=19422,NVALUE=-3253,NVERIF=-3419,NLN=19760)
      PARAMETER (NL=19200,NL1=20440,NLIST=19579)
      PARAMETER (NRANGE=28854,NRA2=28872,NWITH=-5181)
C
      INTEGER MAXCOM,LBP2
      PARAMETER (MAXCOM=LNBUF/LNCOMP,LBP2=LNBUF/LNREAL+2*LNEDGE+2)
C
C Initialisation
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      VSET=SEMLU(-1,NVALUE,V)
      NCM1=NCOL-1
      NBUF=1
      RMIN=VAL(NRANGE)
      RMAX=VAL(NRA2)
C
C Check that FROM and WITH are same dimensions
C (different classes acceptable)
C
      IF (NCOLS(LP3).NE.NCOL .OR. NROWS(LP3).NE.NROW) THEN
         ERROR = 5
         IDERR = NWITH
         GOTO 280
      ENDIF
C
C Initialise contour heights
C
      IHMAX=IVAL(NLEVEL)
      IF (IHMAX.LE.0.OR.IHMAX.GT.MXLEVL) THEN
         IDERR=NLEVEL
         GOTO 300
      ENDIF
      IF (SEMLU(-1,NLIST,X)) THEN
C
C User-supplied height list: variables L,L2,L3..L9
C
         IHMAX=MIN(IHMAX,9)
         NAME=NL1
         DO 10 IH=1,IHMAX
            IDERR=NAME
            IF (IDERR.EQ.NL1) IDERR=NL
C
C Pick up height and check strictly increasing
C
            IF (.NOT.SEMLU(-1,IDERR,H2)) THEN
               ERROR = 25
               GOTO 280
            ENDIF
C
            IF (IH.NE.1) THEN
               IF (H2.LE.H1) GOTO 300
            ENDIF
            H(IH)=H2
            NAME=NAME+40
            H1=H2
   10    CONTINUE
      ELSE
         IF (RMIN .GT. RMAX .OR.
     +      (RMAX .EQ. RMIN .AND. IHMAX.NE.1)) GOTO 290
         IF (OPT(NLN)) THEN
C
C Geometrically (logarithmically) spaced heights
C
            IF (RMIN.LE.0.) GOTO 290
            T = EXP(ALOG(RMAX/RMIN)/(IHMAX+1))
            X = RMIN
            DO 20 IH=1,IHMAX
               X=X*T
               H(IH)=X
   20       CONTINUE
         ELSE
C
C Arithmetically spaced heights
C
            T = (RMAX-RMIN)/(IHMAX+1)
            X = RMIN
            DO 30 IH=1,IHMAX
               X=X+T
               H(IH)=X
   30       CONTINUE
         ENDIF
      ENDIF
C
      MAXSEG = MAXCOM/IHMAX
C
C Print contour heights if option VERIFY is set
C
      IF (OPT(NVERIF)) THEN
         IF (SEMCON(' ')) GOTO 280
         IF (SEMCON('Contour height(s)')) GOTO 280
C
         DO 50 I=1,IHMAX,5
   40       FORMAT (3X,5(G12.4,:))
            WRITE (RECORD,40) (H(J),J=I,MIN(I+4,IHMAX))
            IF (SEMCON(RECORD)) GOTO 280
   50    CONTINUE
      ENDIF
C
C Read rows of WITH in turn, scanning for segments GE each contour
C height, comparing with segment list for previous row so as to
C allow forcing of segments not matched in neighbouring row (above
C or below), and forcing endpoints of all segments
C
      J=1
   60 NBUF=LBP2-NBUF
      OBUF=LBP2-NBUF
      IF (SEMROW(1,RB2(NBUF),2,J,1,LP3)) GOTO 280
C
C Scan new line - each pixel is compared only with the contour
C heights next above and next below, resulting in a very fast scan
C
      LOW=.TRUE.
      HIGH=.FALSE.
      IH=0
      I0=-MAXSEG
      DO 70 I=1,IHMAX
         SEGNUM(I)=0
   70 CONTINUE
      H2=H(1)
      I=NBUF
      LAST=I+NCM1
C
C Pick up next pixel from row
C
   80 PIX=RB2(I)
C
C Test against contour above
C
      IF (HIGH .OR. PIX.LT.H2) GOTO 90
C
C Note upward transition and increment test heights
C
      IH=IH+1
      I0=I0+MAXSEG
      I1=SEGNUM(IH)+1
      SEGNUM(IH)=I1
      IF (I1.GT.MAXSEG) THEN
         ERROR=77
         IDMESS = 'Inadequate workspace - reduce levels or complexity'
         GOTO 280
      ENDIF
      SPTR=I0+I1
      SEGS(SPTR)=I+1-NBUF
      HIGH=IH.GE.IHMAX
      LOW=.FALSE.
      H1=H2
      IF (.NOT.HIGH) H2=H(IH+1)
      GOTO 80
C
C Test against contour below
C
   90 IF (LOW .OR. PIX.GE.H1) GOTO 100
C
C Note downward transition and decrement test height
C
      SPTR=I0+SEGNUM(IH)+MAXCOM
      SEGS(SPTR)=I-NBUF
      IH=IH-1
      I0=I0-MAXSEG
      HIGH=.FALSE.
      LOW=IH.LE.0
      H2=H1
      IF (.NOT.LOW) H1=H(IH)
      GOTO 90
C
C Next pixel
C
  100 I=I+1
      IF (I.LE.LAST) GOTO 80
C
C Terminate outstanding segments?
C
  110 IF (IH.NE.0) THEN
         SPTR=I0+SEGNUM(IH)+MAXCOM
         SEGS(SPTR)=NCOL
         IH=IH-1
         I0=I0-MAXSEG
         GOTO 110
      ENDIF
C
C If FROM and WITH differ, replace current row with source now
C
      IF (LP1.NE.LP3) THEN
         IF (SEMROW(1,RB2(NBUF),2,J,1,LP1)) GOTO 280
      ENDIF
C
C Segment lists are now processed for each height in turn
C
      I0=0
      DO 260 IH=1,IHMAX
C
C Compare segment lists for new and old lines - code works from
C left to right, forcing strips where one row is high and
C other is low. Four states are distinguished, designated
C LL, LH, HH and HL below - meaning high/low for old/new row
C
C Establish value to be used in marking contour
C
         IF (.NOT.VSET) THEN
            IF (RMAX-H(IH) .LT. H(IH)-RMIN) THEN
               V = RMIN
            ELSE
               V = RMAX
            ENDIF
         ENDIF
C
C Note pointer for last segment at this height
C
         LST=I0+SEGNUM(IH)
         IF (J.EQ.1) GOTO 220
         OLST=OLSTSG(IH)
         SPTR=I0+1
         OSPTR=SPTR
C
C State LL (SPTR and OSPTR point to next segment)
C
  120    IF (OSPTR.GT.OLST) THEN
            IF (SPTR.LE.LST) GOTO 130
            GOTO 220
         ENDIF
         I1 = OSEGS(OSPTR)
         IF (SPTR.GT.LST) GOTO 140
         IF (I1-SEGS(SPTR))140,200,130
  130    I1=SEGS(SPTR)+1
         IF (I1.EQ.2) I1=1
         GOTO 210
  140    I1=I1+1
         IF (I1.EQ.2) I1=1
C
C State HL (SPTR points to next segment, OSPTR to current)
C
  150    I2=OSPTR+MAXCOM
         I2=OSEGS(I2)
         IBUF=OBUF
         IF (SPTR.LE.LST) THEN
            I3=SEGS(SPTR)-1
            IF (I2.GT.I3) GOTO 170
         ENDIF
         OSPTR=OSPTR+1
  160    I2=I2-1
         IF (I2.EQ.NCM1) I2=NCOL
         I3=1
         GOTO 180
  170    I2=I3
         I3=0
C
C Force line of pixels
C
  180    I1=I1+IBUF-1
         I2=I2+IBUF-1
         DO 190 I=I1,I2
            RB2(I)=V
  190    CONTINUE
         IF (I3.NE.0) GOTO 120
C
C State HH (SPTR and OSPTR point to current segment)
C
  200    I1=SPTR+MAXCOM
         I1=SEGS(I1)
         I2=OSPTR+MAXCOM
         I2=OSEGS(I2)
         IF (I1 .LT. I2) THEN
            I1=I1+1
            SPTR=SPTR+1
            GOTO 150
         ELSE IF (I1 .EQ. I2) THEN
            SPTR=SPTR+1
            OSPTR=OSPTR+1
            GOTO 120
         ENDIF
         I1=I2+1
         OSPTR=OSPTR+1
C
C State LH (SPTR points to current segment, OSPTR to next)
C
  210    I2=SPTR+MAXCOM
         I2=SEGS(I2)
         IBUF=NBUF
         IF (OSPTR.LE.OLST) THEN
            I3=OSEGS(OSPTR)-1
            IF (I2.GT.I3) GOTO 170
         ENDIF
         SPTR=SPTR+1
         GOTO 160
C
C Fill in ends of segments (except outermost), and
C Copy segment list from SEGS to OSEGS
C
  220    OLSTSG(IH)=LST
         IF (SEGNUM(IH).LE.0) GOTO 250
         I2=I0+1
         I3=I2+SEGNUM(IH)-1
         DO 240 LST=I2,I3
            SPTR=LST
            ITEST=1
  230       I=SEGS(SPTR)
            IF (I.NE.ITEST) THEN
               I=I+NBUF-1
               RB2(I)=V
            ENDIF
            OSEGS(SPTR)=SEGS(SPTR)
            IF (SPTR.LE.MAXCOM) THEN
               SPTR=SPTR+MAXCOM
               ITEST=NCOL
               GOTO 230
            ENDIF
  240    CONTINUE
  250    I0=I0+MAXSEG
  260 CONTINUE
C
C Output old line
C
      IF (J.EQ.1) THEN
         J = 2
         GOTO 60
      ENDIF
C
  270 IF (SEMROW(2,RB2(OBUF),2,J-1,1,LP2)) GOTO 280
C
C Proceed to next line (if any)
C
      IF (J .LT. NROW) THEN
         J = J + 1
         GOTO 60
      ELSE IF (J .EQ. NROW) THEN
         J = J + 1
         OBUF = LBP2 - OBUF
         GOTO 270
      ENDIF
C
C Complete
C
  280 RETURN
C
C Errors
C
C
  290 IDERR=NRANGE
  300 ERROR=3
      GOTO 280
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
