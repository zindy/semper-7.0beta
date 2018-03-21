C Semper 6 subsidiary module MASK1
C
      SUBROUTINE MASK1(INSIDE,LVALUE)
C
C Masks the source picture with a circular mask.  The mask radius
C is specified by the key RADIUS and may be offset from the source
C picture origin by the keys POSITION and PO2.  If the key WIDTH
C is set, the mask tails off with a Gaussian profile within
C the distance specified by key WIDTH.  All pixels outside the
C circular mask are reset, unless key INSIDE is set, in which case
C all pixels inside the mask are reset.  The reset value is specified
C by the key VALUE, if set, and otherwise is determined by calculating
C the mean value round the edge of the circular mask.  If the key
C VALUE is unset, the circular mask must intersect the source picture,
C so that a reset value can be arrived at.
C
      LOGICAL INSIDE,LVALUE
C
      REAL VAL
      LOGICAL VARSET,SEMROW,EXTRCT
      LOGICAL MARSET,FSINIT,FSCIRC,FSFLUS
C
      INCLUDE 'COMMON'
C
      REAL DUMMY(2)
      REAL DR,DX,DY,DYY,F,RM,RRM,RRG,RR1,RR2,RR3,RR4,S,T,X,Y
      REAL X0,X1,X2,Y0,DTHETA,THETA,WIDTH,WW,XMIN,YMIN,XMAX,YMAX
      INTEGER I,I1,I2,II,J,K,L,M,N,LG1,LG2,LM1,LM2,LM3
      INTEGER EXTFRM,EXTLEN,CCOL,CROW,INFORM,MARK,NCOL,NROW,NLAY
      LOGICAL GAUSSN,NOROW,ANNOT
C
      INTEGER IB1(LNBUF/LNINT)
      EQUIVALENCE (IB1,RB1)
C
C Packed names
C
      INTEGER NRADIU,NPOS,NPO2,NWIDTH,NVALUE,NVA2
      PARAMETER (NRADIU=28844, NPOS=26219, NPO2=26232, NWIDTH=-5165)
      PARAMETER (NVALUE=-3253, NVA2=-3273)
C
      DATA DUMMY / 2*0.0 /
C
C Fetch source picture size and centre position
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      NLAY=NLAYS(LP1)
      CCOL=CCOLN(LP1)
      CROW=CROWN(LP1)
C
C Fetch mask offset defined by keys POSITION and PO2
C
      DX=VAL(NPOS)
      DY=VAL(NPO2)
C
C Determine centre position of mask in pixel coordinates
C
      X0=REAL(CCOL)+DX
      Y0=REAL(CROW)-DY
C
C See if mask radius defined by key RADIUS
C
      IF (VARSET(NRADIU)) THEN
C
C Fetch value for mask radius
C
         RM=VAL(NRADIU)
C
C Fault zero or negative mask radius
C
         IF (RM.LE.0.0) THEN
            ERROR=3
            IDERR=NRADIU
            GOTO 140
         ENDIF
C
         RRM=RM*RM
C
C Otherwise, set default radius to half maximum radius
C
      ELSE
         XMIN=REAL(1-CCOL)-DX
         XMAX=REAL(NCOL-CCOL)-DX
         YMIN=REAL(CROW-NCOL)-DY
         YMAX=REAL(CROW-1)-DY
C
         RR1=XMIN*XMIN+YMIN*YMIN
         RR2=XMIN*XMIN+YMAX*YMAX
         RR3=XMAX*XMAX+YMIN*YMIN
         RR4=XMAX*XMAX+YMAX*YMAX
C
         RRM=MAX(RR1,RR2,RR3,RR4)/4.0
         RM=SQRT(RRM)
      ENDIF
C
C See if key WIDTH is set for Gaussian edge
C
      GAUSSN=VARSET(NWIDTH)
      IF (GAUSSN) THEN
C
C Fetch value for key WIDTH
C
         WIDTH=VAL(NWIDTH)
C
C Fault zero or negative value for WIDTH
C
         IF (WIDTH.LE.0.0) THEN
            ERROR=3
            IDERR=NWIDTH
            GOTO 140
         ENDIF
C
         IF (INSIDE) WIDTH=-WIDTH
C
         WW=WIDTH*WIDTH
C
C Set up Gaussian mask radius
C
         RRG=MAX(RM+4.0*WIDTH,0.0)**2
      ENDIF
C
C Fetch and check value of key MARK
C
      IF (MARSET(ANNOT,MARK)) GOTO 140
C
C If key MARK appropriately set, annotate specified display picture
C
      IF (ANNOT) THEN
C
C Initialise display graphics
C
         IF (FSINIT(3,MARK)) GOTO 140
C
C Annotate display picture only if 2-D image
C
         IF (FSPTYP.EQ.1) THEN
C
C Draw circle round mask
C
            IF (FSCIRC(DX,DY,RM)) GOTO 140
C
C If Gaussian edge specified, draw second circle
C
            IF (GAUSSN) THEN
               IF (FSCIRC(DX,DY,RM+2.0*WIDTH)) GOTO 140
            ENDIF
C
C Flush graphics buffer
C
            IF (FSFLUS()) GOTO 140
         ENDIF
      ENDIF
C
C Determine form for processing source picture
C
      INFORM=FORMN(LP1)
      IF (INFORM.EQ.NFMBYT) INFORM=NFMINT
C
C See if reset value is defined by key VALUE
C
      IF (LVALUE) THEN
C
C Fetch reset values from keys VALUE and VA2
C
         S=VAL(NVALUE)
         T=VAL(NVA2)
C
C Otherwise, set up data for determining perimeter mean to establish
C a reset value
C
      ELSE
C
C Determine form for determining perimeter mean (using EXTRCT)
C
         IF (INFORM.EQ.NFMCOM) THEN
            EXTFRM=NFMCOM
            EXTLEN=LNCOMP
         ELSE
            EXTFRM=NFMFP
            EXTLEN=LNREAL
         ENDIF
      ENDIF
C
C Process each source picture layer in turn
C
      DO 130 K=1,NLAY
C
C If reset value not specified, establish it by determining the
C perimeter mean
C
         IF (.NOT.LVALUE) THEN
C
C Determine number of perimeter sampling points
C
            M=MAX(4,INT(MIN(TWOPI*RM,REAL(LNBUF/EXTLEN))))
C
C Determine positions of all sampling points
C
            DTHETA=TWOPI/REAL(M)
            THETA=0.0
C
            N=0
            DO 10 I=1,M
C
C Determine position of next sampling point in pixel coordinates
C
               X=X0+RM*COS(THETA)
               Y=Y0+RM*SIN(THETA)
C
C Add sampling point to list if within source picture limits
C
               IF (X.GE.1.0.AND.X.LE.REAL(NCOL).AND.
     +             Y.GE.1.0.AND.Y.LE.REAL(NROW)) THEN
                  N=N+1
                  RB3(N)=X
                  RB4(N)=Y
               ENDIF
C
C Move on to next sampling point
C
               THETA=THETA+DTHETA
   10       CONTINUE
C
C Fault disjoint perimeter (no sampling points)
C
            IF (N.EQ.0) THEN
               ERROR = 77
               IDMESS = 'Boundary entirely outside picture'
               GOTO 140
            ENDIF
C
C Extract source picture values at each sampling point using bi-linear
C interpolation
C
            IF (EXTRCT(LP1,N,EXTFRM,K,.FALSE.,DUMMY)) GOTO 140
C
C Determine perimeter mean
C
            IF (EXTFRM.EQ.NFMFP) THEN
               S=0.0
               DO 20 I=1,N
                  S=S+RB2(I)
   20          CONTINUE
               S=S/REAL(N)
            ELSE
               S=0.0
               T=0.0
               DO 30 I=1,2*N
                  S=S+RB2(I)
                  T=T+RB2(I+1)
   30          CONTINUE
               S=S/REAL(N)
               T=T/REAL(N)
            ENDIF
         ENDIF
C
C Process source picture
C
         DO 120 J=1,NROW
C
C Count picture limits as intersection points if option INSIDE not set
C
            IF (INSIDE) THEN
               N=0
            ELSE
               RB2(1)=1.0
               RB2(2)=REAL(NCOL)
               N=2
            ENDIF
C
C Determine distance between current row and mask centre
C
            DY=REAL(J)-Y0
            DYY=DY*DY
C
C Add intersections (if any) with edge of mask
C
            IF (DYY.LT.RRM) THEN
               DX=SQRT(RRM-DYY)
               RB2(N+1)=X0-DX
               RB2(N+2)=X0+DX
               N=N+2
            ENDIF
C
C If Gaussian edge, add intersections (if any) with limit of profile
C
            IF (GAUSSN) THEN
               IF (DYY.LT.RRG) THEN
                  DX=SQRT(RRG-DYY)
                  RB2(N+1)=X0-DX
                  RB2(N+2)=X0+DX
                  N=N+2
               ENDIF
            ENDIF
C
C Skip this row if no intersection points and in situ processing
C
            IF (N.EQ.0.AND.LP1.EQ.LP2) GOTO 120
C
C Set flag for outstanding input of current source row
C
            NOROW=.TRUE.
C
C Sort intersection points
C
            CALL MASK3(RB2,N)
C
C If Gaussian edge, adjust pixel values using Gaussian weighting factor
C
            IF (GAUSSN) THEN
C
C Determine loop indices for Gaussian mask
C
               IF (INSIDE) THEN
                  LG1=1
                  LG2=N
C
                  LM1=2
                  LM2=N-1
                  LM3=2
               ELSE
                  LG1=2
                  LG2=N-1
C
                  LM1=1
                  LM2=N
                  LM3=ABS(N-3)+1
               ENDIF
C
C Loop over sections of source row with Gaussian mask
C
               DO 70 L=LG1,LG2,2
C
C Fetch section limits and ignore section if outside picture limits
C
                  X1=RB2(L)
                  X2=RB2(L+1)
                  IF (X1.GE.REAL(NCOL).OR.X2.LE.1.0) GOTO 70
C
C Clip section to picture limits and round inwards to next pixel
C
                  X1=MAX(X1,1.0)
                  X2=MIN(X2,REAL(NCOL))
C
                  I1=INT(X1)
                  IF (REAL(I1).LT.X1) I1=I1+1
                  I2=INT(X2)
                  IF (REAL(I2).GT.X2) I2=I2-1
C
C Read row from source picture if still not input
C
                  IF (NOROW) THEN
                     IF (SEMROW(1,RB1,INFORM,J,K,LP1)) GOTO 140
C
                     NOROW=.FALSE.
                  ENDIF
C
C Adjust section according to form for processing the source picture
C
                  IF (INFORM.EQ.NFMINT) THEN
                     DO 40 I=I1,I2
                        DX=REAL(I)-X0
                        DR=SQRT(DX*DX+DYY)-RM
                        F=EXP(-(DR*DR/WW))
                        IB1(I)=NINT(F*REAL(IB1(I))+(1.0-F)*S)
   40                CONTINUE
                  ELSE IF (INFORM.EQ.NFMFP) THEN
                     DO 50 I=I1,I2
                        DX=REAL(I)-X0
                        DR=SQRT(DX*DX+DYY)-RM
                        F=EXP(-(DR*DR/WW))
                        RB1(I)=F*RB1(I)+(1.0-F)*S
   50                CONTINUE
                  ELSE
                     II=2*I1-1
                     DO 60 I=I1,I2
                        DX=REAL(I)-X0
                        DR=SQRT(DX*DX+DYY)-RM
                        F=EXP(-(DR*DR/WW))
                        RB1(II)=F*RB1(II)+(1.0-F)*S
                        RB1(II+1)=F*RB1(II+1)+(1.0-F)*T
                        II=II+2
   60                CONTINUE
                  ENDIF
   70          CONTINUE
C
C Otherwise, set loop indices for plain mask
C
            ELSE
               LM1=1
               LM2=N
               LM3=2
            ENDIF
C
C Loop over sections of source row with plain mask
C
            DO 110 L=LM1,LM2,LM3
C
C Fetch section limits and ignore section if outside picture limits
C
               X1=RB2(L)
               X2=RB2(L+1)
               IF (X1.GE.REAL(NCOL).OR.X2.LE.1.0) GOTO 110
C
C Clip section to picture limits and round inwards to next pixel
C
               X1=MAX(X1,1.0)
               X2=MIN(X2,REAL(NCOL))
C
               I1=INT(X1)
               IF (REAL(I1).LT.X1) I1=I1+1
               I2=INT(X2)
               IF (REAL(I2).GT.X2) I2=I2-1
C
C Read row from source picture if still not input
C
               IF (NOROW) THEN
                  IF (SEMROW(1,RB1,INFORM,J,K,LP1)) GOTO 140
C
                  NOROW=.FALSE.
               ENDIF
C
C Reset section according to form for processing the source picture
C
               IF (INFORM.EQ.NFMINT) THEN
                  DO 80 I=I1,I2
                     IB1(I)=NINT(S)
   80             CONTINUE
               ELSE IF (INFORM.EQ.NFMFP) THEN
                  DO 90 I=I1,I2
                     RB1(I)=S
   90             CONTINUE
               ELSE
                  DO 100 I=2*I1-1,2*I2,2
                     RB1(I)=S
                     RB1(I+1)=T
  100             CONTINUE
               ENDIF
  110       CONTINUE
C
C Check to see if source row still not input
C
            IF (NOROW) THEN
C
C Skip this row if in situ processing
C
               IF (LP1.EQ.LP2) GOTO 120
C
C Otherwise, read row from source picture
C
               IF (SEMROW(1,RB1,INFORM,J,K,LP1)) GOTO 140
            ENDIF
C
C Store result in LP2
C
            IF (SEMROW(2,RB1,INFORM,J,K,LP2)) GOTO 140
  120    CONTINUE
  130 CONTINUE
C
  140 RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
