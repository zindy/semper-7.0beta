C Semper 6 processing module MARK
C
      SUBROUTINE MARK
C
      REAL VAL
      INTEGER IVAL,IVALPN
      LOGICAL FSOPTN,FSINIT,FSVIEW,FSARRO,FSLINE,FSCIRC,FSARC,FSLIST
      LOGICAL FSCURV,FSBORD,FSTEXT,FSMARK,FSELEC
      LOGICAL OPT,VARSET,CONOPT,SEMOPN,SEMLAB,SEMROW,SEMTEX,MARK2
      LOGICAL CLOSED
C
      INCLUDE 'COMMON'
C
      REAL R,SAMPLE,THETA,UX,UY,VX,VY
      REAL XCEN,X,X1,X2,X3,X4
      REAL YCEN,Y,Y1,Y2,Y3,Y4
      INTEGER CLASS,FORM,ITYPE,JX,JY,LABEL(256),N,NCOL,NLAY,NPIC,NROW
      INTEGER OPC,USIZE,VSIZE,TEXT(256)
C
      EQUIVALENCE (LABEL,TEXT,RB1)
C
C Packed names
C
      INTEGER NVIEW,NARROW,NPOSIT,NPO2,NTO,NTO2,NRADIU,NANGLE,NAN2
      INTEGER NSIZE,NSI2,NSAMPL,NUV,NLEFT,NRIGHT,NTOP,NBOTTO
      INTEGER NU,NU2,NV,NV2,NWITH,NLIST,NCURVE,NBORDE,NTEXT,NABOVE
      INTEGER NBELOW,NLJ,NRJ,NTJ,NBJ,NOPEN,NCLOSE
C
      PARAMETER (NVIEW=-3566, NARROW=2338)
      PARAMETER (NPOSIT=26219, NPO2=26232, NTO=-601, NTO2=-633)
      PARAMETER (NRADIU=28844, NANGLE=2167, NAN2=2192)
      PARAMETER (NSIZE=30786, NSI2=30792, NSAMPL=30453, NUV=-2481)
      PARAMETER (NLEFT=19406, NRIGHT=29167, NTOP=-617, NBOTTO=3820)
      PARAMETER (NU=-1601, NU2=-2881, NV=-3201, NV2=-4481)
      PARAMETER (NWITH=-5181, NLIST=19579, NCURVE=5658, NBORDE=3818)
      PARAMETER (NTEXT=-225, NABOVE=1695, NBELOW=3412)
      PARAMETER (NLJ=19600, NRJ=29200, NTJ=-401, NBJ=3600)
      PARAMETER (NOPEN=24645, NCLOSE=5295)
C
C Determine nature of graphics coordinates according to options FRAME,
C PARTITION and PICTURE
C
      IF (FSOPTN(OPC,N)) GOTO 30
C
C Set up specified graphics coordinates
C
      IF (FSINIT(OPC,N)) GOTO 30
C
C If option VIEW is set, switch view to area of interest
C
      IF (OPT(NVIEW)) THEN
         IF (FSVIEW()) GOTO 30
      ENDIF
C
C If key TO is set, draw a line or an arrow
C
      IF (VARSET(NTO)) THEN
C
C Determine start and end positions
C
         X1=VAL(NPOSIT)
         Y1=VAL(NPO2)
         X2=VAL(NTO)
         Y2=VAL(NTO2)
C
C If option ARROW is set, draw an arrow
C
         IF (OPT(NARROW)) THEN
            IF (FSARRO(X1,Y1,X2,Y2)) GOTO 30
C
C Otherwise, draw a line
C
         ELSE
            IF (FSLINE(X1,Y1,X2,Y2)) GOTO 30
         ENDIF
C
C If key RADIUS is set, draw a circle or an arc
C
      ELSE IF (VARSET(NRADIU)) THEN
C
C Determine centre position and radius
C
         X=VAL(NPOSIT)
         Y=VAL(NPO2)
         R=VAL(NRADIU)
C
C Fault negative radius
C
         IF (R.LT.0.0) THEN
            ERROR=3
            IDERR=NRADIU
            GOTO 30
         ENDIF
C
C If key ANGLE is set, draw an arc
C
         IF (VARSET(NANGLE)) THEN
            IF (FSARC(X,Y,R,VAL(NANGLE),VAL(NAN2))) GOTO 30
C
C Otherwise, draw a circle
C
         ELSE
            IF (FSCIRC(X,Y,R)) GOTO 30
         ENDIF
C
C If key SIZE is set, draw frame round specified sub-region
C
      ELSE IF (VARSET(NSIZE)) THEN
C
C Fetch value for SIZE key
C
         USIZE=IVAL(NSIZE)
C
C Determine value for SI2 key
C
         IF (VARSET(NSI2)) THEN
            VSIZE=IVAL(NSI2)
         ELSE
            VSIZE=USIZE
         ENDIF
C
C Fault zero or negative size
C
         IF (USIZE.LE.0.OR.VSIZE.LE.0) THEN
            ERROR=3
            IDERR=NSIZE
            GOTO 30
         ENDIF
C
C Determine value for SAMPLING key
C
         IF (VARSET(NSAMPL)) THEN
            SAMPLE=VAL(NSAMPL)
         ELSE
            SAMPLE=1.0
         ENDIF
C
C Fault zero or negative sampling interval
C
         IF (SAMPLE.LE.0.0) THEN
            ERROR=3
            IDERR=NSAMPL
            GOTO 30
         ENDIF
C
C Fault conflict between options LEFT and RIGHT
C
         IF (CONOPT(NLEFT,NRIGHT)) GOTO 30
C
C Fault cobflict between options TOP and BOTTOM
C
         IF (CONOPT(NTOP,NBOTTO)) GOTO 30
C
C Determine X centre position of sub-region
C
         IF (OPT(NLEFT)) THEN
            XCEN=FSBLEF+SAMPLE*REAL(USIZE/2)
         ELSE IF (OPT(NRIGHT)) THEN
            XCEN=FSBRIG-SAMPLE*REAL((USIZE-1)/2)
         ELSE
            XCEN=0.0
         ENDIF
C
C Determine Y centre position of sub-region
C
         IF (OPT(NBOTTO))  THEN
            YCEN=FSBBOT+SAMPLE*REAL((VSIZE-1)/2)
         ELSE IF (OPT(NTOP)) THEN
            YCEN=FSBTOP-SAMPLE*REAL(VSIZE/2)
         ELSE
            YCEN=0.0
         ENDIF
C
C Add offset specified by keys POSITION and PO2
C
         XCEN=XCEN+VAL(NPOSIT)
         YCEN=YCEN+VAL(NPO2)
C
C Fault conflict between key ANGLE and option UV
C
         IF (VARSET(NANGLE).AND.OPT(NUV)) THEN
            ERROR=60
            IDERR=NANGLE
            IDERR2=NUV
            GOTO 30
         ENDIF
C
C Determine U and V vectors describing unit cell for sub-region
C
         IF (VARSET(NANGLE)) THEN
            THETA=VAL(NANGLE)
            UX=SAMPLE*COS(THETA)
            UY=SAMPLE*SIN(THETA)
            VX=-UY
            VY=UX
         ELSE IF (OPT(NUV)) THEN
            IF (VARSET(NU)) THEN
               UX=VAL(NU)
            ELSE
               UX=1.0
            ENDIF
C
            UY=VAL(NU2)
            VX=VAL(NV)
C
            IF (VARSET(NV2)) THEN
               VY=VAL(NV2)
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
C Determine four corner points of sub-region
C
         X1=XCEN-REAL(USIZE/2)*UX-REAL((VSIZE-1)/2)*VX
         Y1=YCEN-REAL(USIZE/2)*UY-REAL((VSIZE-1)/2)*VY
         X2=X1+REAL(USIZE-1)*UX
         Y2=Y1+REAL(USIZE-1)*UY
         X3=X1+REAL(VSIZE-1)*VX
         Y3=Y1+REAL(VSIZE-1)*VY
         X4=X2+X3-X1
         Y4=Y2+Y3-Y1
C
C Draw frame round sub-region
C
         IF (FSLINE(X1,Y1,X2,Y2)) GOTO 30
         IF (FSLINE(X2,Y2,X4,Y4)) GOTO 30
         IF (FSLINE(X4,Y4,X3,Y3)) GOTO 30
         IF (FSLINE(X3,Y3,X1,Y1)) GOTO 30
C
C If key WITH is set, mark the specified position list
C
      ELSE IF (VARSET(NWITH)) THEN
C
C Fault conflict between options LIST and CURVE
C
         IF (CONOPT(NLIST,NCURVE)) GOTO 30
C
C Fault conflict between options OPEN and CLOSED
C
         IF (CONOPT(NOPEN,NCLOSE)) GOTO 30
C
C Open picture containing the position list
C
         NPIC=IVALPN(NWITH)
         IF (SEMOPN(1,NPIC,NCOL,NROW,NLAY,CLASS,FORM,LP1)) GOTO 30
C
C Fault picture which is not a position list
C
         IF (CLASS.NE.NCLPLI) THEN
            ERROR=6
            IDERR=NPIC
            GOTO 30
         ENDIF
C
C Fetch contents of picture label
C
         IF (SEMLAB(1,LABEL,LP1)) GOTO 30
C
C Get position list type from picture label
C
         ITYPE=LABEL(LBPLTY)
C
C Fault position list type which conflicts with options LIST, CURVE,
C OPEN and CLOSED
C
         IF ((OPT(NLIST).AND.ITYPE.NE.1).OR.
     +       (OPT(NCURVE).AND.ITYPE.EQ.1).OR.
     +       (OPT(NOPEN).AND.ITYPE.EQ.3).OR.
     +       (OPT(NCLOSE).AND.ITYPE.EQ.2)) THEN
            ERROR=72
            IDERR=NPIC
            GOTO 30
         ENDIF
C
C Loop over Plist rows
C
         DO 10 N=1,NROW
C
C Fetch contents of position list
C
            IF (SEMROW(1,RB1,NFMFP,N,1,LP1)) GOTO 30
            IF (SEMROW(1,RB2,NFMFP,N,2,LP1)) GOTO 30
C
C If straight position list, mark each point with symbol
C
            IF (ITYPE.EQ.1) THEN
               IF (FSLIST(RB1,RB2,NCOL,FSMMOD,FSMSIZ)) GOTO 30
C
C Otherwise, mark position list with series of straight lines
C
            ELSE
               CLOSED=ITYPE.EQ.3
               IF (FSCURV(RB1,RB2,NCOL,CLOSED)) GOTO 30
            ENDIF
   10    CONTINUE
C
C If option BORDER is set, draw border round border limits
C
      ELSE IF (OPT(NBORDE)) THEN
C
C Draw border
C
         IF (FSBORD()) GOTO 30
C
C If key TEXT is set, draw text string
C
      ELSE IF (VARSET(NTEXT)) THEN
C
C Fetch text string from the command line
C
         N=256
         IF (SEMTEX(NTEXT,TEXT,N)) GOTO 30
C
C Do nothing if null string
C
         IF (N.EQ.0) GOTO 20
C
C If option ABOVE or BELOW is set, annotate display picture as a whole.
C Special code is required for this in order to bypass the duplication
C of text when annotating a complex display picture
C
         IF (OPT(NABOVE).OR.OPT(NBELOW)) THEN
C
C Annotate display picture only, faulting output to frame or partition
C
            IF (MARK2(TEXT,N)) GOTO 30
C
            GOTO 20
         ENDIF
C
C Determine X origin for text
C
         IF (OPT(NLEFT)) THEN
C
C Fault conflict between options LEFT and RIGHT
C
            IF (CONOPT(NRIGHT,NLEFT)) GOTO 30
            X=FSBLEF
            JX=-2
         ELSE IF (OPT(NRIGHT)) THEN
            X=FSBRIG
            JX=2
         ELSE
            X=0.0
            JX=0
         ENDIF
C
C Add 2 pixel offset away from X border limit
C
         X=X-REAL(JX)/ABS(FSXSCA)
C
C Determine Y origin for text
C
         IF (OPT(NBOTTO)) THEN
C
C Fault conflict between options TOP and BOTTOM
C
            IF (CONOPT(NBOTTO,NTOP)) GOTO 30
            Y=FSBBOT
            JY=-2
         ELSE IF (OPT(NTOP)) THEN
            Y=FSBTOP
            JY=2
         ELSE
            Y=0.0
            JY=0
         ENDIF
C
C Add 2 pixel offset away from Y border limit
C
         Y=Y-REAL(JY)/ABS(FSYSCA)
C
C Add offset specified by keys POSITION and PO2
C
         X=X+VAL(NPOSIT)
         Y=Y+VAL(NPO2)
C
C Determine X justification direction
C
         IF (OPT(NLJ)) THEN
C
C Fault conflict between options LJ and RJ
C
            IF (CONOPT(NRJ,NLJ)) GOTO 30
            JX=-1
         ELSE IF (OPT(NRJ)) THEN
            JX=1
         ENDIF
C
C Determine Y justification direction
C
         IF (OPT(NBJ)) THEN
C
C Fault conflict between options TJ and BJ
C
            IF (CONOPT(NBJ,NTJ)) GOTO 30
            JY=-1
         ELSE IF (OPT(NTJ)) THEN
            JY=1
         ENDIF
C
C Output text string
C
         IF (FSTEXT(TEXT,N,X,Y,JX,JY)) GOTO 30
C
C Default action is to mark point specified by keys POSITION and PO2
C
      ELSE
         IF (FSMARK(VAL(NPOSIT),VAL(NPO2),FSMMOD,FSMSIZ)) GOTO 30
      ENDIF
C
C Update current frame/partition/picture number
C
   20 IF (FSELEC()) GOTO 30
C
   30 RETURN
C
C Copyright (C) 1987-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module MARK2
C
      LOGICAL FUNCTION MARK2(TEXT,N)
      INTEGER N,TEXT(N)
C
C Deals with special case of adding text above or below display
C pictures, whether real or complex, and forcing text to be output
C just once.
C
      LOGICAL OPT,CONOPT,FSTEXT
C
      INCLUDE 'COMMON'
C
      REAL X,Y
      INTEGER JX,JY
      LOGICAL LINSID
C
C Packed names
C
      INTEGER NABOVE,NBELOW,NFRAME,NPARTI,NINSID,NOUTSI
      PARAMETER (NABOVE=1695, NBELOW=3412, NFRAME=10321, NPARTI=25658)
      PARAMETER (NINSID=14979, NOUTSI=24860)
C
      MARK2=.TRUE.
      IF (OPT(NINSID)) THEN
C
C Fault conflict between options INSIDE and OUTSIDE
C
         IF (CONOPT(NOUTSI,NINSID)) GOTO 10
         LINSID = .TRUE.
      ELSE
         LINSID = .FALSE.
      ENDIF
C
C Text is centred on default view centre position in X direction
C
      X=FSXPAN
      JX=0
C
C Text is positioned on top or bottom border limit, according to
C option ABOVE or BELOW
C
      IF (OPT(NABOVE)) THEN
C
C Fault conflict between options ABOVE and BELOW
C
         IF (CONOPT(NBELOW,NABOVE)) GOTO 10
C
C Fault conflict with option FRAME or PARTITION
C
         IF (CONOPT(NFRAME,NABOVE)) GOTO 10
         IF (CONOPT(NPARTI,NABOVE)) GOTO 10
         Y=FSBTOP
C
C Determine vertical justification direction according to option
C INSIDE or OUTSIDE
C
         IF (LINSID) THEN
            JY=2
         ELSE
            JY=-2
         ENDIF
      ELSE
C
C Fault conflict with option FRAME or PARTITION
C
         IF (CONOPT(NFRAME,NBELOW)) GOTO 10
         IF (CONOPT(NPARTI,NBELOW)) GOTO 10
         Y=FSBBOT
C
C Determine vertical justification direction
C
         IF (LINSID) THEN
            JY=-2
         ELSE
            JY=2
         ENDIF
      ENDIF
C
C Add 2 pixel offset away from border limit
C
      Y=Y-REAL(JY)/ABS(FSYSCA)
C
C Disable duplication of text onto imaginary part (if any) of picture
C
      FSI1=1
      FSI2=1
C
C Output text string
C
      MARK2 = FSTEXT(TEXT,N,X,Y,JX,JY)
C
   10 RETURN
C
C Copyright (C) 1987,1989,1990:  Synoptics Ltd,  All Rights Reserved
C
      END
