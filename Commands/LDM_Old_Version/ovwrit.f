C Semper 6 processing module OVWRIT
C
      SUBROUTINE OVWRIT
C
C Thresholds the source picture specified by means of the WITH key and
C outputs the resulting binary image to the frame/partition/picture on
C the display.  The threshold value is specified with the THRESHOLD key
C (default is (min+max)/2, where min,max = source picture range).
C Pixels are drawn in the overlay if the corresponding source picture
C value is greater than or equal to the threshold value.  Setting the
C option NEGATED inverts this condition.  The source picture origin is
C aligned with the graphics origin.  The output is clipped at the
C graphics limits in default, and to the border limits if the CLIP
C option is set.  The layer key may be used to specify which layer of
C the source picture to use, otherwise, the first picture layer is used.
C Only the real part of complex data is used.  If the VIEW option is
C set, the viewing conditions are set to make the target region visible.
C
C Syntax:  Ovwrite :OVWRIT $1= frame partition picture $2=sel with=$2 +
C                          layer=1 threshold= negated clip view +
C                          open(lp1,old)=with
C
      LOGICAL VARSET,GETRNG,OPT,SEMROW
      LOGICAL FSOPTN,FSINIT,FSQBOR,FSQLIM,FSAMPL,FSOVRW,FSELEC,FSVIEW
      INTEGER IVAL
      REAL    VAL
C
      INTEGER I,J,I1,I2,LAYER,MODE,N,NCOL,NROW,CCOL,CROW,NX,NY
      REAL    THRESH,PMIN,PMAX,XMIN,XMAX,YMIN,YMAX,X1,X2,Y1,Y2
      REAL    XX,X,DX,Y,DY
C
      INCLUDE 'COMMON'
C
      INTEGER IB2(LNBUF/LNINT)
      EQUIVALENCE (IB2,RB2)
C
C Establish threshold value
C
      IF (VARSET(-339)) THEN
         THRESH=VAL(-339)
      ELSE
         IF (GETRNG(PMIN,PMAX,LP1)) GOTO 30
         THRESH=(PMIN+PMAX)/2.0
      ENDIF
C
C Set up threshold flags according to whether or not option NEGATED
C is set
C
      IF (OPT(22607)) THEN
         I1=0
         I2=1
      ELSE
         I1=1
         I2=0
      ENDIF
C
C LAYER key defines which sources picture layer to use
C
      LAYER=IVAL(19265)
C
C Establish graphics mode
C
      IF (FSOPTN(MODE,N)) GOTO 30
C
C  Initialise graphics
C
      IF (FSINIT(MODE,N)) GOTO 30
C
C If VIEW option is set, switch view to area of interest
C
      IF (OPT(-3566)) THEN
         IF (FSVIEW()) GOTO 30
      ENDIF
C
C Establish output limits
C
      IF (OPT(5289)) THEN
         IF (FSQBOR(XMIN,XMAX,YMIN,YMAX)) GOTO 30
      ELSE
         IF (FSQLIM(XMIN,XMAX,YMIN,YMAX))GOTO 30
      ENDIF
C
C Fetch source picture size and centre position
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      CCOL=CCOLN(LP1)
      CROW=CROWN(LP1)
C
C Establish limits of source picture
C
      X1=REAL(1-CCOL)
      X2=REAL(NCOL-CCOL)
      Y1=REAL(CROW-NROW)
      Y2=REAL(CROW-1)
C
C If picture falls completely outside output limits, do nothing
C
      IF (X1.GT.XMAX.OR.X2.LT.XMIN.OR.Y1.GT.YMAX.OR.Y2.LT.YMIN) GOTO 30
C
C Combine picture limits and output limits
C
      XMIN=MAX(X1,XMIN)
      XMAX=MIN(X2,XMAX)
      YMIN=MAX(Y1,YMIN)
      YMAX=MIN(Y2,YMAX)
C
C Establish sampling grid
C
      IF (FSAMPL(XMIN,XMAX,YMIN,YMAX,X,DX,NX,Y,DY,NY)) GOTO 30
C
C If X or Y sampling count is zero, do nothing
C
      IF (NX.EQ.0.OR.NY.EQ.0) GOTO 30
C
C For each output row
C
      DO 20 J=1,NY
C
C Read data from nearest source picture row
C
         IF (SEMROW(1,RB1,NFMFP,NINT(REAL(CROW)-Y),LAYER,LP1)) GOTO 30
C
C Threshold the data using nearest source pixel for each output pixel
C
         XX=X
         DO 10 I=1,NX
            IF (RB1(NINT(REAL(CCOL)+XX)).GE.THRESH) THEN
               IB2(I)=I1
            ELSE
               IB2(I)=I2
            ENDIF
C
C Increment column position
C
            XX=XX+DX
   10    CONTINUE
C
C Write the data to the display overlay
C
         IF (FSOVRW(2,IB2,NX,X,Y)) GOTO 30
C
C Increment row position
C
         Y=Y+DY
C
   20 CONTINUE
C
C Update current frame/partition/picture number
C
      IF (FSELEC()) GOTO 30
C
   30 RETURN
C
C Copyright (C) 1990-1993:  Synoptics Ltd,  All Rights Reserved
C
      END
