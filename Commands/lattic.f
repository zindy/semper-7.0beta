C Semper 6 processing module LATTIC
C
      SUBROUTINE LATTIC
C
      REAL VAL
      INTEGER IVAL,IVALPN
      LOGICAL ABANDN,CONOPT,FSOPTN,FSINIT,FSREGN,FSLIST,FSLINE
      LOGICAL MARSET,OPT,SEMDIA,SEMLAB,SEMOPN,SEMROW,TSTSRG,VARSET
C
      INCLUDE 'COMMON'
C
      REAL D,HC(4),HMIN,HMAX,KC(4),KMIN,KMAX,RADIUS,RR,RXMAX,RXMIN
      REAL RYMAX,RYMIN,S,T1,T2,U(2),V(2),W(2),X,X1,X2,XC(4)
      REAL XMAX,XMIN,Y,Y1,Y2,YC(4),YMAX,YMIN
C
      INTEGER CLASS,FIRST(3),FORM,H,H1,H2,I,IH,IK,K,K1,K2,LABEL(256)
      INTEGER LAST(3),MARK,N,NCOL,NH,NK,NLAY,NROW,OPC
      LOGICAL ANNOT,LIST,SUBREG,LSITES,FULL
C
      EQUIVALENCE (LABEL,RB1),(FIRST,SMGI1),(LAST,SMGI4)
C
C Packed names
C
      INTEGER NFROM,NFRAME,NPARTI,NPICTU,NSPACI,NRADIU,NSITES
      INTEGER NTO,NDLLR1,NMARK
      PARAMETER (NFROM=10335, NFRAME=10321, NPARTI=25658, NPICTU=25963)
      PARAMETER (NSPACI=31041, NRADIU=28844, NSITES=30780, NTO=-601)
      PARAMETER (NDLLR1=-12441, NMARK=20858)
C
      INTEGER NU(2),NV(2),NW(2)
      DATA NU,NV,NW /-1601,-2881,-3201,-4481,-4801,-6081/
C
C If option FROM is set, lattic sub-region is specified with respect to
C source picture specified by key $1
C
      IF (OPT(NFROM)) THEN
C
C Fault conflict between option FROM and options FRAME, PARTITION and
C PICTURE
C
         IF (CONOPT(NFRAME,NFROM)) GOTO 100
         IF (CONOPT(NPARTI,NFROM)) GOTO 100
         IF (CONOPT(NPICTU,NFROM)) GOTO 100
C
C Open source picture
C
         IF (SEMOPN(1,IVALPN(NDLLR1),NCOL,NROW,NLAY,CLASS,FORM,LP1))
     +      GOTO 100
C
C Establish sub-region
C
         IF (TSTSRG(1,LP1)) GOTO 100
C
C Determine sub-region limits
C
         XMIN=REAL(FIRST(1)-CCOLN(LP1))
         XMAX=REAL(LAST(1)-CCOLN(LP1))
         YMIN=REAL(CROWN(LP1)-LAST(2))
         YMAX=REAL(CROWN(LP1)-FIRST(2))
C
C See if key MARK is set, requesting display annotation
C
         IF (MARSET(ANNOT,MARK)) GOTO 100
C
C If so, initialise display graphics and see if display picture is o.k.
C
         IF (ANNOT) THEN
            IF (FSINIT(3,MARK)) GOTO 100
C
            ANNOT=FSPTYP.EQ.1
         ENDIF
C
C Otherwise, lattice sub-region specified in graphics coordinates
C
      ELSE
C
C Determine nature of graphics coordinates according to whether option
C FRAME, PARTITION or PICTURE (default) is set
C
         IF (FSOPTN(OPC,N)) GOTO 100
C
C Initialise display graphics
C
         IF (FSINIT(OPC,N)) GOTO 100
C
C Determine size and limits of lattice sub-region
C
         IF (FSREGN(XMIN,XMAX,YMIN,YMAX)) GOTO 100
C
C Annotation is required if key MARK is set to YES
C
         ANNOT=OPT(NMARK)
      ENDIF
C
C See if key TO is set
C
      LIST=IVAL(NTO).NE.0
C
C Do nothing if no annotation or position list required
C
      IF (.NOT.(ANNOT.OR.LIST)) GOTO 100
C
C If key SPACING is set, set up square lattice vectors
C
      IF (VARSET(NSPACI)) THEN
C
C Fetch value of key SPACING
C
         S=VAL(NSPACI)
C
C Fault zero or negative value
C
         IF (S.LE.0.0) THEN
            ERROR=3
            IDERR=NSPACI
            GOTO 100
         ENDIF
C
C Set up lattice vectors and lattice origin
C
         U(1)=S
         U(2)=0.0
         V(1)=0.0
         V(2)=S
C
         W(1)=0.0
         W(2)=0.0
C
         D=S*S
C
C Otherwise, use keys U,U2 and V,V2 and W,W2
C
      ELSE
C
C Set up lattice vectors and lattice origin
C
         DO 10 I=1,2
            U(I)=VAL(NU(I))
            V(I)=VAL(NV(I))
            W(I)=VAL(NW(I))
   10    CONTINUE
C
C Calculate determinant for lattice transformation
C
         D=U(1)*V(2)-U(2)*V(1)
C
C Fault parallel U and V vectors
C
         IF (ABS(D)/
     +          SQRT( (U(1)*U(1)+U(2)*U(2)) * (V(1)*V(1)+V(2)*V(2)) )
     +               .LT. 0.001) THEN
            ERROR=79
            GOTO 100
         ENDIF
      ENDIF
C
C See if key RADIUS is set
C
      SUBREG=VARSET(NRADIU)
C
C If so, lattice is limited to circular sub-region centred on the
C lattice origin
C
      IF (SUBREG) THEN
C
C Fetch value of key RADIUS
C
         RADIUS=VAL(NRADIU)
C
C Fault zero or negative radius
C
         IF (RADIUS.LE.0.0) THEN
            ERROR=3
            IDERR=NRADIU
            GOTO 100
         ENDIF
C
C Determine X and Y limits of circle
C
         RXMIN=W(1)-RADIUS
         RXMAX=W(1)+RADIUS
         RYMIN=W(2)-RADIUS
         RYMAX=W(2)+RADIUS
C
C Fault circle outside lattice sub-region
C
         IF (RXMIN.GT.XMAX.OR.RXMAX.LT.XMIN.OR.
     +       RYMIN.GT.YMAX.OR.RYMAX.LT.YMIN) THEN
            ERROR=9
            GOTO 100
         ENDIF
C
C Clip sub-region limits to circle limits
C
         XMIN=MAX(XMIN,RXMIN)
         XMAX=MIN(XMAX,RXMAX)
         YMIN=MAX(YMIN,RYMIN)
         YMAX=MIN(YMAX,RYMAX)
C
C Set up square of radius for later use
C
         RR=RADIUS*RADIUS
      ENDIF
C
C Set up four corner positions of lattice sub-region
C
      XC(1)=XMIN
      YC(1)=YMIN
      XC(2)=XMIN
      YC(2)=YMAX
      XC(3)=XMAX
      YC(3)=YMIN
      XC(4)=XMAX
      YC(4)=YMAX
C
C Transform corner positions into lattice coordinates
C
      DO 20 I=1,4
         HC(I)=(V(2)*(XC(I)-W(1))-V(1)*(YC(I)-W(2)))/D
         KC(I)=(U(1)*(YC(I)-W(2))-U(2)*(XC(I)-W(1)))/D
   20 CONTINUE
C
C Determine maximum and minimum lattice coordinate values
C
      HMIN=MIN(HC(1),HC(2),HC(3),HC(4))
      HMAX=MAX(HC(1),HC(2),HC(3),HC(4))
      KMIN=MIN(KC(1),KC(2),KC(3),KC(4))
      KMAX=MAX(KC(1),KC(2),KC(3),KC(4))
C
C Convert to integer values (rounding inwards)
C
      H1=INT(HMIN)
      IF (REAL(H1).LT.HMIN) H1=H1+1
C
      H2=INT(HMAX)
      IF (REAL(H2).GT.HMAX) H2=H2-1
C
      K1=INT(KMIN)
      IF (REAL(K1).LT.KMIN) K1=K1+1
C
      K2=INT(KMAX)
      IF (REAL(K2).GT.KMAX) K2=K2-1
C
C See if option SITES is set
C
      LSITES=OPT(NSITES)
C
C Initialise count and flag for position list
C
      N=0
C
      FULL=.FALSE.
C
C Generate lattice site positions
C
      DO 60 H=H1,H2
C
C Store lattice positions in K direction in a temporary buffer
C
         NK=0
         DO 30 K=K1,K2
C
C Determine lattice position with respect to lattice origin
C
            X=REAL(H)*U(1)+REAL(K)*V(1)
            Y=REAL(H)*U(2)+REAL(K)*V(2)
C
C If circular sub-region specified, see if lattice point lies outside
C
            IF (SUBREG) THEN
               IF (X*X+Y*Y.GT.RR) GOTO 30
            ENDIF
C
C Add offset for lattice origin
C
            X=X+W(1)
            Y=Y+W(2)
C
C See if lattice point lies outside lattice sub-region
C
            IF (X.LT.XMIN.OR.X.GT.XMAX.OR.
     +          Y.LT.YMIN.OR.Y.GT.YMAX) GOTO 30
C
C Add point to temporary buffer
C
            IF (NK.LT.LNBUF/LNREAL) THEN
               NK=NK+1
               RB1(NK)=X
               RB2(NK)=Y
            ENDIF
   30    CONTINUE
C
         IF (ABANDN(ERROR)) GOTO 100
C
C If position list is required and it is not full, add lattice positions
C
         IF (LIST.AND.(.NOT.FULL)) THEN
C
C Transfer each point in turn
C
            DO 40 IK=1,NK
C
C Check that row buffers not yet full
C
               IF (N.LT.LNBUF/LNREAL) THEN
                  N=N+1
                  RB4(N)=RB1(IK)
                  RB5(N)=RB2(IK)
C
C Otherwise, set flag to indicate position list overflow
C
               ELSE
                  FULL=.TRUE.
               ENDIF
   40       CONTINUE
         ENDIF
C
C See if display annotation is required
C
         IF (ANNOT) THEN
C
C See if lattice sites are to be marked
C
            IF (LSITES) THEN
C
C Mark last batch of lattice sites
C
               IF (FSLIST(RB1,RB2,NK,FSMMOD,FSMSIZ)) GOTO 100
C
C Otherwise, mark lattice lines
C
            ELSE
C
C Step through last batch of lattice sites
C
               DO 50 IK=1,NK-1
C
C Determine start and end point of next line segment
C
                  T1 = RB1(IK)
                  T2 = RB1(IK+1)
                  X1 = 0.8*T1+0.2*T2
                  X2 = 0.2*T1+0.8*T2
                  T1 = RB2(IK)
                  T2 = RB2(IK+1)
                  Y1 = 0.8*T1+0.2*T2
                  Y2 = 0.2*T1+0.8*T2
C
C Draw line segment
C
                  IF (FSLINE(X1,Y1,X2,Y2)) GOTO 100
   50          CONTINUE
            ENDIF
         ENDIF
   60 CONTINUE
C
C If lattice lines are to be marked, re-generate lattice points in
C H direction
C
      IF (ANNOT.AND.(.NOT.LSITES)) THEN
C
C Generate lattice site positions
C
         DO 90 K=K1,K2
C
C Store lattice positions in H direction in a temporary buffer
C
            NH=0
            DO 70 H=H1,H2
C
C Determine lattice position with respect to lattice origin
C
               X=REAL(H)*U(1)+REAL(K)*V(1)
               Y=REAL(H)*U(2)+REAL(K)*V(2)
C
C If circular sub-region specified, see if lattice point lies outside
C
               IF (SUBREG) THEN
                  IF (X*X+Y*Y.GT.RR) GOTO 70
               ENDIF
C
C Add offset for lattice origin
C
               X=X+W(1)
               Y=Y+W(2)
C
C See if lattice point lies outside lattice sub-region
C
               IF (X.LT.XMIN.OR.X.GT.XMAX.OR.
     +             Y.LT.YMIN.OR.Y.GT.YMAX) GOTO 70
C
C Add point to temporary buffer
C
               IF (NH.LT.LNBUF/LNREAL) THEN
                  NH=NH+1
                  RB1(NH)=X
                  RB2(NH)=Y
               ENDIF
   70       CONTINUE
C
C Step through last batch of lattice sites
C
            DO 80 IH=1,NH-1
C
C Determine start and end point of next line segment
C
               T1 = RB1(IH)
               T2 = RB1(IH+1)
               X1 = 0.8*T1+0.2*T2
               X2 = 0.2*T1+0.8*T2
               T1 = RB2(IH)
               T2 = RB2(IH+1)
               Y1 = 0.8*T1+0.2*T2
               Y2 = 0.2*T1+0.8*T2
C
C Draw line segment
C
               IF (FSLINE(X1,Y1,X2,Y2)) GOTO 100
   80       CONTINUE
   90    CONTINUE
      ENDIF
C
C Store contents of position list
C
      IF (LIST) THEN
C
C If no points, output warning message
C
         IF (N.EQ.0) THEN
            IF (SEMDIA('No lattice points generated',NDIWAR)) GOTO 100
C
C Otherwise, store contents of position list
C
         ELSE
C
C If position list overflow, output warning message
C
            IF (FULL) THEN
               IF (SEMDIA(
     +         'Position list overflow - some lattice postions omitted',
     +            NDIWAR)) GOTO 100
            ENDIF
C
C Set up LP entry number
C
            IF (OPT(NFROM)) THEN
               LP2=LP1
            ELSE
               LP2=0
            ENDIF
C
C Open output picture
C
            IF (SEMOPN(2,IVALPN(NTO),N,1,2,NCLPLI,NFMFP,LP2)) GOTO 100
C
C Add type code to picture label for straight position list = 1
C
            IF (SEMLAB(1,LABEL,LP2)) GOTO 100
C
            LABEL(LBPLTY)=1
C
            IF (SEMLAB(2,LABEL,LP2)) GOTO 100
C
C Store contents of position list
C
            IF (SEMROW(2,RB4,NFMFP,1,1,LP2)) GOTO 100
            IF (SEMROW(2,RB5,NFMFP,1,2,LP2)) GOTO 100
         ENDIF
      ENDIF
C
  100 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
