C Semper 6 processing module XWIRES
C
      SUBROUTINE XWIRES
C
      INCLUDE 'COMMON'
C
      INTEGER NOPTIO
      PARAMETER ( NOPTIO = 15 )
C
      REAL VAL
      INTEGER IVAL,IVALPN
      LOGICAL OPT,OPTNO,VARSET,CONOPT,SEMLU,SEMOPN,SEMROW,SEMLAB,SEMCEN
      LOGICAL FSOPTN,FSINIT,FSVIEW,FSXWIR,FSLINE,FSCIRC,FSARC,FSMARK
      LOGICAL FSFLUS,FSELEC
C
      REAL A,D,D1,D2,D3,DP,DU,DV,DX,DY,DX13,DY13,DX21,DY21,DX32,DY32,P
      REAL R,S,S1,S2,S3,SAMPLE,THETA,THETA1,THETA2,THETA3,USCALE,VSCALE
      REAL U,V,UX,UY,VX,VY,XMIN,XMAX,X,Y,X0,Y0,X1,Y1,X2,Y2,X3,Y3,X4,Y4
C
      INTEGER IB1(256),NOPT(NOPTIO),OPC,SIZE,USIZE,VSIZE,LABEL(256),CCOL
      INTEGER I,J,N,NP,IP,IX,IX1,IX2,NCOL,NSEG
C
      LOGICAL LOPT(NOPTIO),LVERIF,LLINE,LCIRCL,LARC,LREGIO,LLIST,LCURVE
      LOGICAL LGRAPH,LONED,LANGLE,LUV,LOPEN,LCLOSE,LSYMME,LANTIS
      LOGICAL LSIZE,LSAMPL,CLOSED,LSECTI
C
      EQUIVALENCE (LABEL,IB1,RB1)
      EQUIVALENCE (LLINE,LOPT(1)),(LCIRCL,LOPT(2)),(LARC,LOPT(3))
      EQUIVALENCE (LREGIO,LOPT(4)),(LLIST,LOPT(5)),(LCURVE,LOPT(6))
      EQUIVALENCE (LGRAPH,LOPT(7)),(LSECTI,LOPT(8)),(LONED,LOPT(9))
      EQUIVALENCE (LANGLE,LOPT(10)),(LUV,LOPT(11)),(LOPEN,LOPT(12))
      EQUIVALENCE (LCLOSE,LOPT(13)),(LSYMME,LOPT(14)),(LANTIS,LOPT(15))
C
      REAL TOLSQD
      PARAMETER (TOLSQD = 2.5)
C
C Packed names
C
      INTEGER NVERIF,NFRAME,NPARTI,NVIEW,NLINE,NCIRCL,NARC,NREGIO
      INTEGER NLIST,NCURVE,NONED,NANGLE,NUV,NSIZE,NSI2,NSAMPL
      INTEGER NX,NY,NR,NR2,NTHETA,NTH2,NU,NU2,NV,NV2,NXN,NYN,NXWIRE
      INTEGER NTO,NPOSIT,NPO2,NGRAPH,NAMEP,NA,NSYMME,NANTIS
      INTEGER NOPEN,NCLOSE,NSECTI
C
      PARAMETER (NVERIF=-3419,NFRAME=10321,NPARTI=25658,NVIEW=-3566)
      PARAMETER (NLINE=19574,NCIRCL=5178,NARC=2323,NREGIO=29007)
      PARAMETER (NLIST=19579,NCURVE=5658,NONED=24565,NANGLE=2167)
      PARAMETER (NUV=-2481,NSIZE=30786,NSI2=30792,NSAMPL=30453)
      PARAMETER (NX=-6401,NY=-8001,NR=28800,NR2=30080,NTHETA=-326)
      PARAMETER (NTH2=-353,NU=-1601,NU2=-2881,NV=-3201,NV2=-4481)
      PARAMETER (NXN=-6961,NYN=-8561,NXWIRE=-7330)
      PARAMETER (NTO=-601,NPOSIT=26219,NPO2=26232,NGRAPH=11921)
      PARAMETER (NAMEP=25600,NA=1600,NSYMME=31413,NANTIS=2180)
      PARAMETER (NOPEN=24645,NCLOSE=5295,NSECTI=30603)
C
      DATA NOPT /NLINE,NCIRCL,NARC,NREGIO,NLIST,NCURVE,NGRAPH,NSECTI,
     +           NONED,NANGLE,NUV,NOPEN,NCLOSE,NSYMME,NANTIS/
C
C See if options LINE, CIRCLE, ARC, REGION, LIST, CURVE, GRAPH, SECTION,
C ONED, ANGLED, UV, OPEN, CLOSED, SYMMETRIC and ANTISYMMETRIC are set
C
      DO 10 I = 1,NOPTIO
         LOPT(I) = OPT(NOPT(I))
   10 CONTINUE
C
C Fault conflict between options LINE, CIRCLE, ARC, REGION, LIST, CURVE
C GRAPH and SECTION
C
      DO 30 I = 1,7
         DO 20 J = I+1,8
            IF (LOPT(I).AND.LOPT(J)) THEN
               ERROR = 60
               IDERR = NOPT(I)
               IDERR2 = NOPT(J)
               GOTO 230
            ENDIF
   20    CONTINUE
   30 CONTINUE
C
C Determine whether frame, partition or picture coordinates are to be
C used for graphics coordinates according to options FRAME, PARTITION
C and PICTURE
C
      IF (FSOPTN(OPC,N)) GOTO 230
C
C Set up required graphics coordinates
C
      IF (FSINIT(OPC,N)) GOTO 230
C
C If option VIEW is set, switch view to area of interest
C
      IF (OPT(NVIEW)) THEN
         IF (FSVIEW()) GOTO 230
      ENDIF
C
C Determine cursor start position from keys POSITION and PO2
C
      X0 = VAL(NPOSIT)
      Y0 = VAL(NPO2)
C
C See if option NOVERIFY is not set
C
      LVERIF = .NOT.OPTNO(NVERIF)
C
C Set point style initially
C
      CALL FSXS61(0)
C
C Option LINE
C
      IF (LLINE) THEN
C
C Read two points, marking first point if required
C
         IF (FSXWIR(X0,Y0,X1,Y1)) GOTO 230
C
C Set line style
C
         CALL FSXS61(1)
         IF (FSXWIR(X1,Y1,X2,Y2)) GOTO 230
C
C Determine length and slope of line
C
         DX = X2-X1
         DY = Y2-Y1
         R = SQRT(DX*DX+DY*DY)
C
         IF (R.EQ.0.0) THEN
            THETA = 0.0
         ELSE
            THETA = ATAN2(DY,DX)
         ENDIF
C
C Set variables - (X,Y) = start point of line
C               - (XN,YN) = end point of line
C               - R = length of line
C               - THETA = angle of slope of line
C
         IF (SEMLU(1,NX,X1)) GOTO 230
         IF (SEMLU(1,NY,Y1)) GOTO 230
         IF (SEMLU(1,NXN,X2)) GOTO 230
         IF (SEMLU(1,NYN,Y2)) GOTO 230
         IF (SEMLU(1,NR,R)) GOTO 230
         IF (SEMLU(1,NTHETA,THETA)) GOTO 230
C
C Draw line if required
C
         IF (LVERIF) THEN
            IF (FSLINE(X1,Y1,X2,Y2)) GOTO 230
         ENDIF
C
C Options CIRCLE and ARC
C
      ELSE IF (LCIRCL.OR.LARC) THEN
C
C Read three points, marking first two points if required
C
         IF (FSXWIR(X0,Y0,X1,Y1)) GOTO 230
C
         IF (LVERIF) THEN
            IF (FSMARK(X1,Y1,5,0)) GOTO 230
            IF (FSFLUS()) GOTO 230
         ENDIF
C
         IF (FSXWIR(X1,Y1,X2,Y2)) GOTO 230
C
         IF (LVERIF) THEN
            IF (FSMARK(X2,Y2,5,0)) GOTO 230
            IF (FSFLUS()) GOTO 230
         ENDIF
C
         IF (FSXWIR(X2,Y2,X3,Y3)) GOTO 230
C
C Circle/arc defined by three points lying on the circle/arc.  These
C three points describe a triangle and the circle/arc lies on the
C circumscribing circle for the triangle.
C
C Determine lengths of three sides of triangle
C
         DX32 = X3-X2
         DY32 = Y3-Y2
C
         DX13 = X1-X3
         DY13 = Y1-Y3
C
         DX21 = X2-X1
         DY21 = Y2-Y1
C
         S1 = SQRT(DX32*DX32+DY32*DY32)
         S2 = SQRT(DX13*DX13+DY13*DY13)
         S3 = SQRT(DX21*DX21+DY21*DY21)
C
C Determine radius of inscribed circle for triangle
         S = (S1+S2+S3)/2.0
C
         IF (S.EQ.0.0) THEN
            R = 0.0
         ELSE
            R = SQRT((S-S1)*(S-S2)*(S-S3)/S)
         ENDIF
C
C Fault circle diameter less than half pixel width, i.e. three points
C describing the triangle are collinear
C
         IF (2.0*ABS(FSXSCA)*R.LT.0.5) THEN
            ERROR = 78
            GOTO 230
         ENDIF
C
C Determine coefficients for calculating circle/arc centre
C
         D1 = DX32*(X3+X2)+DY32*(Y3+Y2)
         D2 = DX13*(X1+X3)+DY13*(Y1+Y3)
         D3 = DX21*(X2+X1)+DY21*(Y2+Y1)
C
         D = 2.0*(DX32*Y1+DX13*Y2+DX21*Y3)
C
C Determine circle/arc centre point
C
         X = (D1*Y1+D2*Y2+D3*Y3)/D
         Y = (D1*X1+D2*X2+D3*X3)/(-D)
C
C Determine circle/arc radius = radius of circumscribed circle
C
         R = S1*S2*S3/(4.0*R*S)
C
C If arc, determine start and finish angles
C
         IF (LARC) THEN
            THETA1 = ATAN2(Y1-Y,X1-X)
            THETA2 = ATAN2(Y2-Y,X2-X)
            THETA3 = ATAN2(Y3-Y,X3-X)
C
            IF (THETA1.LT.THETA2) THETA1 = THETA1+TWOPI
            IF (THETA3.LT.THETA2) THETA3 = THETA3+TWOPI
C
            THETA = MAX(THETA1,THETA3)-TWOPI
            THETA2 = MIN(THETA1,THETA3)
C
C Set variables - THETA = arc start angle
C               - TH2 = arc finish angle
C
            IF (SEMLU(1,NTHETA,THETA)) GOTO 230
            IF (SEMLU(1,NTH2,THETA2)) GOTO 230
         ENDIF
C
C Set variables - (X,Y) = circle/arc centre position
C               - R = circle/arc radius
C
         IF (SEMLU(1,NX,X)) GOTO 230
         IF (SEMLU(1,NY,Y)) GOTO 230
         IF (SEMLU(1,NR,R)) GOTO 230
C
C Draw circle/arc if required
C
         IF (LVERIF) THEN
            IF (LCIRCL) THEN
               IF (FSCIRC(X,Y,R)) GOTO 230
            ELSE
               IF (FSARC(X,Y,R,THETA,THETA2)) GOTO 230
            ENDIF
         ENDIF
C
C Option REGION or SECTION (synonymous with REGION ONED)
C
      ELSE IF (LREGIO.OR.LSECTI) THEN
C
C Fault conflict between options SECTION, ONED, ANGLED and UV
C
         DO 50 I = 8,10
            DO 40 J = I+1,11
               IF (LOPT(I).AND.LOPT(J)) THEN
                  ERROR = 60
                  IDERR = NOPT(I)
                  IDERR2 = NOPT(J)
                  GOTO 230
               ENDIF
   40       CONTINUE
   50    CONTINUE
C
C If option UV is set, determine size of region from keys SIZE,SI2
C
         IF (LUV) THEN
C
C Determine value for first dimension (default = 2)
C
            IF (VARSET(NSIZE)) THEN
               USIZE = IVAL(NSIZE)
            ELSE
               USIZE = 2
            ENDIF
C
C Determine value for second dimension
C
            IF (VARSET(NSI2)) THEN
               VSIZE = IVAL(NSI2)
            ELSE
               VSIZE = USIZE
            ENDIF
C
C Fault dimension less than 2
C
            IF (USIZE.LT.2.OR.VSIZE.LT.2) GOTO 260
C
C Otherwise, determine sampling interval from key SAMPLING
C
         ELSE
C
C Determine sampling interval (default = 1.0)
C
            IF (VARSET(NSAMPL)) THEN
               SAMPLE = VAL(NSAMPL)
            ELSE
               SAMPLE = 1.0
            ENDIF
C
C Fault zero or negative sampling interval
C
            IF (SAMPLE.LE.0.0) GOTO 250
         ENDIF
C
C Read two points, marking both points if required
C
         IF (FSXWIR(X0,Y0,X1,Y1)) GOTO 230
C
         IF (LVERIF.AND.(LUV.OR.LANGLE)) THEN
            IF (FSMARK(X1,Y1,5,0)) GOTO 230
            IF (FSFLUS()) GOTO 230
         ENDIF
C
C Set cursor style
C
         IF (LSECTI.OR.LONED) THEN
            CALL FSXS61(1)
         ELSE IF (.NOT.(LUV.OR.LANGLE)) THEN
            CALL FSXS61(2)
         ENDIF
C
         IF (FSXWIR(X1,Y1,X2,Y2)) GOTO 230
C
         IF (LVERIF.AND.(LUV.OR.LANGLE)) THEN
            IF (FSMARK(X2,Y2,5,0)) GOTO 230
            IF (FSFLUS()) GOTO 230
         ENDIF
C
C If option ONED is set, region may be rotated - line from first to
C second point defines 1-D region.  If option ANGLED is set, region
C is rotated, line from first to third point defines angle of rotation
C
         IF (LSECTI.OR.LONED.OR.LANGLE) THEN
C
C If option ONED is set, make third point = second point
C
            IF (LSECTI.OR.LONED) THEN
               X3 = X2
               Y3 = Y2
            ELSE
C
C Otherwise, read third point, cursor start position = first point
C
               CALL FSXS61(1)
C
               IF (FSXWIR(X1,Y1,X3,Y3)) GOTO 230
            ENDIF
C
C Determine components and length of rotation vector
C
            DX = X3-X1
            DY = Y3-Y1
C
            D = SQRT(DX*DX+DY*DY)
C
C Set up rotation angle, UV transformation matrix and determinant
C
            IF (D.EQ.0.0) THEN
               THETA = 0.0
               UX = 1.0
               UY = 0.0
            ELSE
               THETA = ATAN2(DY,DX)
               UX = DX/D
               UY = DY/D
            ENDIF
C
            VX = -UY
            VY = UX
C
            D = 1.0
C
C Set variable THETA = angle of rotation for region
C
            IF (SEMLU(1,NTHETA,THETA)) GOTO 230
C
C Otherwise, if option UV is set, third and fourth points determine
C U and V directions
C
         ELSE IF (LUV) THEN
C
C Read third and fourth points, cursor start position = first point
C
            CALL FSXS61(1)
            IF (FSXWIR(X1,Y1,X3,Y3)) GOTO 230
C
            CALL FSXS61(1)
            IF (FSXWIR(X1,Y1,X4,Y4)) GOTO 230
C
C Determine unit vector in U direction
C
            DX = X3-X1
            DY = Y3-Y1
C
            D = SQRT(DX*DX+DY*DY)
C
            IF (D.EQ.0.0) THEN
               UX = 1.0
               UY = 0.0
            ELSE
               UX = DX/D
               UY = DY/D
            ENDIF
C
C Determine unit vector in V direction
C
            DX = X4-X1
            DY = Y4-Y1
C
            D = SQRT(DX*DX+DY*DY)
C
            IF (D.EQ.0.0) THEN
               VX = 0.0
               VY = 1.0
            ELSE
               VX = DX/D
               VY = DY/D
            ENDIF
C
C Calculate determinant for UV transformation matrix
C
            D = UX*VY-UY*VX
C
C Fault determinant too close to zero, i.e. U and V directions are
C parallel
C
            IF (ABS(D).LT.0.001) THEN
               ERROR = 79
               GOTO 230
            ENDIF
C
C Otherwise, region is un-rotated and un-skewed
C
         ELSE
C
C Set up UV transformation matrix and its determinant
C
            UX = 1.0
            UY = 0.0
            VX = 0.0
            VY = 1.0
C
            D = 1.0
         ENDIF
C
C Determine position of second point with respect to first point
C in UV coordinates = U and V dimensions of region
C
         DX = X2-X1
         DY = Y2-Y1
C
         U = (VY*DX-VX*DY)/D
         V = (UX*DY-UY*DX)/D
C
C If option UV is set, region size in UV coordinates is specified by
C means of keys SIZE and SI2
C
         IF (LUV) THEN
C
C Adjust U and V vectors so that each corresponds to one unit of size
C in U and V directions
C
            USCALE = ABS(U)/REAL(USIZE-1)
            VSCALE = ABS(V)/REAL(VSIZE-1)
C
            UX = USCALE*UX
            UY = USCALE*UY
C
            VX = VSCALE*VX
            VY = VSCALE*VY
C
C Set variables - (U,U2) = U-axis vector
C               - (V,V2) = V-axis vector
C
            IF (SEMLU(1,NU,UX)) GOTO 230
            IF (SEMLU(1,NU2,UY)) GOTO 230
            IF (SEMLU(1,NV,VX)) GOTO 230
            IF (SEMLU(1,NV2,VY)) GOTO 230
C
C Otherwise, key SAMPLING determines unit of size in U and V directions
C
         ELSE
C
C Determine region size in UV coordinates
C
            USIZE = 1+NINT(ABS(U)/SAMPLE)
            VSIZE = 1+NINT(ABS(V)/SAMPLE)
C
C Adjust U and V vectors so that each correspond to one unit of size
C
            UX = SAMPLE*UX
            UY = SAMPLE*UY
            VX = SAMPLE*VX
            VY = SAMPLE*VY
         ENDIF
C
C Determine offset from first point to centre of region in U and V
C directions
C
         IF (U.LT.0.0) THEN
            DU = -REAL((USIZE-1)/2)
         ELSE
            DU = REAL(USIZE/2)
         ENDIF
C
         IF (V.LT.0.0) THEN
            DV = -REAL(VSIZE/2)
         ELSE
            DV = REAL((VSIZE-1)/2)
         ENDIF
C
C Set variables - (X,Y) = centre of region
C               - R = first (U) dimension of region
C               - R2 = second (V) dimension of region
C
         IF (SEMLU(1,NX,X1+DU*UX+DV*VX)) GOTO 230
         IF (SEMLU(1,NY,Y1+DU*UY+DV*VY)) GOTO 230
         IF (SEMLU(1,NR,REAL(USIZE))) GOTO 230
         IF (SEMLU(1,NR2,REAL(VSIZE))) GOTO 230
C
C Draw region border if required
C
         IF (LVERIF) THEN
C
C Determine offsets along border sides in U and V directions
C
            DU = SIGN(REAL(USIZE-1),U)
            DV = SIGN(REAL(VSIZE-1),V)
C
C Determine position of corner points in graphics coordinates
C
            X3 = X1+DU*UX
            Y3 = Y1+DU*UY
C
            X4 = X1+DV*VX
            Y4 = Y1+DV*VY
C
            X2 = X3+X4-X1
            Y2 = Y3+Y4-Y1
C
C Draw border
C
            IF (FSLINE(X1,Y1,X3,Y3)) GOTO 230
            IF (FSLINE(X3,Y3,X2,Y2)) GOTO 230
            IF (FSLINE(X2,Y2,X4,Y4)) GOTO 230
            IF (FSLINE(X4,Y4,X1,Y1)) GOTO 230
         ENDIF
C
C Option LIST
C
      ELSE IF (LLIST) THEN
C
C Read first point
C
         IF (FSXWIR(X0,Y0,RB4(1),RB5(1))) GOTO 230
C
C Read subsequent points
C
         DO 60 IP = 1,LNBUF/LNREAL
C
C Mark current point if required
C
            IF (LVERIF) THEN
C
C Mark point
C
               IF (FSMARK(RB4(IP),RB5(IP),FSMMOD,FSMSIZ)) GOTO 230
C
C Flush contents of graphics buffer
C
               IF (FSFLUS()) GOTO 230
            ENDIF
C
C Read next point
C
            IF (FSXWIR(RB4(IP),RB5(IP),RB4(IP+1),RB5(IP+1))) GOTO 230
C
C Terminate cursor input if last point close enough to previous point
C
            DX = FSXSCA*(RB4(IP+1)-RB4(IP))
            DY = FSYSCA*(RB5(IP+1)-RB5(IP))
            IF (DX*DX+DY*DY.LT.TOLSQD) GOTO 70
   60    CONTINUE
C
C Maximum for number of input points has been exceeded
C
         ERROR = 80
         GOTO 230
C
C Open new picture for the position list, suppressing selection of
C output picture
C
   70    SELOPN = .FALSE.
C
         LP1 = 0
         IF (SEMOPN(2,IVALPN(NTO),IP,1,2,NCLPLI,NFMFP,LP1)) GOTO 230
C
C Add identifier code for position list to picture label (1 = straight
C position list)
C
         IF (SEMLAB(1,LABEL,LP1)) GOTO 230
C
         LABEL(LBPLTY) = 1
C
         IF (SEMLAB(2,LABEL,LP1)) GOTO 230
C
C Store contents of new position list
C
         IF (SEMROW(2,RB4,NFMFP,1,1,LP1)) GOTO 230
         IF (SEMROW(2,RB5,NFMFP,1,2,LP1)) GOTO 230
C
C Option CURVE
C
      ELSE IF (LCURVE) THEN
C
C Fault conflict between options OPEN and CLOSED
C
         IF (CONOPT(NOPEN,NCLOSE)) GOTO 230
C
C Fault conflict between keys SIZE and SAMPLIMG
C
         LSIZE = VARSET(NSIZE)
         LSAMPL = VARSET(NSAMPL)
         IF (CONOPT(NSIZE,NSAMPL)) GOTO 230
C
C If key SIZE is set, fetch its value
C
         IF (LSIZE) THEN
            SIZE = IVAL(NSIZE)
C
C Fault size less than 2
C
            IF (SIZE.LT.2) GOTO 260
         ENDIF
C
C If key SAMPLING is set, fetch its value
C
         IF (LSAMPL) THEN
            SAMPLE = VAL(NSAMPL)
C
C Fault zero or negative sampling interval
C
            IF (SAMPLE.LE.0.0) GOTO 250
         ENDIF
C
C Read first point
C
         IF (FSXWIR(X0,Y0,RB1(1),RB2(1))) GOTO 230
C
C Read subsequent points
C
         DO 90 IP = 1,LNBUF/LNREAL
C
C Read next point
C
C Set line style
C
            CALL FSXS61(1)
            IF (FSXWIR(RB1(IP),RB2(IP),RB1(IP+1),RB2(IP+1))) GOTO 230
C
C Terminate cursor input if last point close enough to previous point
C
            DX = FSXSCA*(RB1(IP+1)-RB1(IP))
            DY = FSYSCA*(RB2(IP+1)-RB2(IP))
            IF (DX*DX+DY*DY.LT.TOLSQD) THEN
C
C If option CLOSED is set, terminate input by forcing closure of curve
C
               IF (LCLOSE) THEN
                  CLOSED = .TRUE.
                  GOTO 80
C
C Otherwise, termination is for open curve
C
               ELSE
                  NP = IP
                  GOTO 100
               ENDIF
            ENDIF
C
C If option OPEN is set, bypass closure test
C
            IF (LOPEN) THEN
               CLOSED = .FALSE.
C
C Otherwise, determine whether curve closes back on itself
C
            ELSE
               DX = FSXSCA*(RB1(IP+1)-RB1(1))
               DY = FSYSCA*(RB2(IP+1)-RB2(1))
               CLOSED = DX*DX+DY*DY.LT.TOLSQD
            ENDIF
C
C If curve is closed, make last point same as first point
C
   80       IF (CLOSED) THEN
               RB1(IP+1) = RB1(1)
               RB2(IP+1) = RB2(1)
            ENDIF
C
C Draw line joining previous point to last point if required
C
            IF (LVERIF) THEN
C
C Draw line
C
               IF (FSLINE(RB1(IP),RB2(IP),RB1(IP+1),RB2(IP+1))) GOTO 230
C
C Flush contents of graphics buffer
C
               IF (FSFLUS()) GOTO 230
            ENDIF
C
C Terminate cursor input if curve is closed
C
            IF (CLOSED) THEN
               NP = IP+1
               GOTO 100
            ENDIF
   90    CONTINUE
C
C Maximum for number of input points has been exceeded
C
         ERROR = 80
         GOTO 230
C
C Do nothing more if just single point obtained
C
  100    IF (NP.EQ.1) GOTO 230
C
C If key SIZE or SAMPLING is set, interpolated positions are required
C
         IF (LSIZE.OR.LSAMPL) THEN
C
C Determine and sum distances between points
C
            P = 0.0
            DO 110 IP = 1,NP-1
               RB3(IP) = SQRT((RB1(IP+1)-RB1(IP))**2+
     +                      (RB2(IP+1)-RB2(IP))**2)
C
               P = P+RB3(IP)
  110       CONTINUE
C
C If key SIZE is set, size of position list is taken from key SIZE
C
            IF (LSIZE) THEN
               NCOL = SIZE
C
C Determine number of straight line segments
C
               IF (CLOSED) THEN
                  NSEG = NCOL
               ELSE
                  NSEG = NCOL-1
               ENDIF
C
C Otherwise, if key SAMPLING is set, size determined so that distance
C between each point is sampling interval
C
            ELSE IF (LSAMPL) THEN
C
C Fault sampling interval that is larger than the curve length
C
               IF (SAMPLE.GT.P) GOTO 250
C
C Determine number of straight line segments
C
               NSEG = NINT(P/SAMPLE)
C
C Determine number points along curve
C
               IF (CLOSED) THEN
                  NCOL = NSEG
               ELSE
                  NCOL = NSEG+1
               ENDIF
            ENDIF
C
C Fault size for position list which is too large
C
            IF (NCOL.GT.LNBUF/LNREAL) THEN
               ERROR = 5
               IDERR = IVALPN(NTO)
               GOTO 230
            ENDIF
C
C Determine actual distance between each point
C
            DP = P/REAL(NSEG)
C
C Determine intermediate output points by interpolation
C
            P = 0.0
            IP = 1
            DO 130 I = 2,NSEG
C
C Step forward to next point
C
               P = P+DP
C
C Make sure point lies on current chord
C
  120          IF (P.GT.RB3(IP)) THEN
                  P = P-RB3(IP)
                  IP = IP+1
                  GOTO 120
               ENDIF
C
C Determine position for next point by interpolating along chord
C
               RB4(I) = RB1(IP)+P*(RB1(IP+1)-RB1(IP))/RB3(IP)
               RB5(I) = RB2(IP)+P*(RB2(IP+1)-RB2(IP))/RB3(IP)
  130       CONTINUE
C
C Otherwise, no interpolation required
C
         ELSE
C
C Number of output points = number of input points
C
            NCOL = IP
C
C Determine number of straight line segments
C
            IF (CLOSED) THEN
               NSEG = NCOL
            ELSE
               NSEG = NCOL-1
            ENDIF
C
C Copy intermediate input points to output row buffers
C
            DO 140 I = 2,NSEG
               RB4(I) = RB1(I)
               RB5(I) = RB2(I)
  140       CONTINUE
         ENDIF
C
C Copy first and last input points to output row buffers
C
         RB4(1) = RB1(1)
         RB5(1) = RB2(1)
C
         RB4(NSEG+1) = RB1(NP)
         RB5(NSEG+1) = RB2(NP)
C
C Open new picture for position list, suppressing selection of output
C picture
C
         SELOPN = .FALSE.
C
         LP1 = 0
         IF (SEMOPN(2,IVALPN(NTO),NCOL,1,2,NCLPLI,NFMFP,LP1)) GOTO 230
C
C Add identifier code for position list
C                       to picture label = 2, open curve
C                                        = 3, closed curve
C
         IF (SEMLAB(1,LABEL,LP1)) GOTO 230
C
         IF (CLOSED) THEN
            LABEL(LBPLTY) = 3
         ELSE
            LABEL(LBPLTY) = 2
         ENDIF
C
         IF (SEMLAB(2,LABEL,LP1)) GOTO 230
C
C Store contents of position list
C
         IF (SEMROW(2,RB4,NFMFP,1,1,LP1)) GOTO 230
         IF (SEMROW(2,RB5,NFMFP,1,2,LP1)) GOTO 230
C
C If not display picture which is 1-D graph or histogram, determine
C length of curve and, for closed curve, the enclosed area
C
         IF (.NOT.(FSPTYP.EQ.2.OR.FSPTYP.EQ.3)) THEN
C
C Determine curve length
C
            P = 0.0
            DO 150 I = 1,NSEG
               P = P+SQRT((RB4(I+1)-RB4(I))**2+(RB5(I+1)-RB5(I))**2)
  150       CONTINUE
C
C Set variable - P = length of curve
C
            IF (SEMLU(1,NAMEP,P)) GOTO 230
C
C If closed curve, determine enclosed area
C
            IF (CLOSED) THEN
C
C Determine enclosed area
C
               A = 0.0
               DO 160 I = 1,NSEG
                  A = A+(RB4(I)*RB5(I+1)-RB4(I+1)*RB5(I))/2.0
  160          CONTINUE
C
C Set variable - A = area enclosed by closed curve
C
               IF (SEMLU(1,NA,A)) GOTO 230
            ENDIF
         ENDIF
C
C Option GRAPH
C
      ELSE IF (LGRAPH) THEN
C
C Fault conflict with option FRAME or PARTITION
C
         IF (CONOPT(NFRAME,NGRAPH)) GOTO 230
         IF (CONOPT(NPARTI,NGRAPH)) GOTO 230
C
C Fault graphics coordinates not set up for use with a 1-D graph picture
C
         IF (FSPTYP.NE.2) THEN
            ERROR = 81
            GOTO 230
         ENDIF
C
C Fault conflict between options SYMMETRIC and ANTISYMMETRIC
C
         IF (CONOPT(NSYMME,NANTIS)) GOTO 230
C
C Fault conflict between keys SIZE and SAMPLING
C
         LSIZE = VARSET(NSIZE)
         LSAMPL = VARSET(NSAMPL)
         IF (CONOPT(NSIZE,NSAMPL)) GOTO 230
C
C If key SIZE is set, fetch its value
C
         IF (LSIZE) THEN
            SIZE = IVAL(NSIZE)
C
C Fault size less than 2
C
            IF (SIZE.LT.2) GOTO 260
         ENDIF
C
C If key SAMPLING is set, fetch its value
C
         IF (LSAMPL) THEN
            SAMPLE = VAL(NSAMPL)
C
C Fault zero or negative sampling interval
C
            IF (SAMPLE.LE.0.0) GOTO 250
         ENDIF
C
C Read first point
C
         IF (FSXWIR(X0,Y0,RB4(1),RB5(1))) GOTO 230
C
C Fault point beyond right-hand edge of graph
C
         IF (RB4(1).GT.FSBRIG) THEN
            ERROR = 82
            GOTO 230
         ENDIF
C
C Read subsequent points
C
         DO 180 IP = 1,LNBUF/LNREAL
C
C Cursor start position = previous point
C
            X0 = RB4(IP)
            Y0 = RB5(IP)
C
C Read next point
C
  170       CONTINUE
C
C Set line style
C
            CALL FSXS61(1)
            IF (FSXWIR(X0,Y0,RB4(IP+1),RB5(IP+1))) GOTO 230
C
C If input has started to left of graph, special processing is required
C
            IF (RB4(1).LT.FSBLEF) THEN
C
C Reject second point to left of graph
C
               IF (RB4(2).LT.FSBLEF) THEN
                  RB4(1) = RB4(2)
                  RB5(1) = RB5(2)
C
                  X0 = RB4(2)
                  Y0 = RB5(2)
                  GOTO 170
C
C Otherwise chord crosses left-hand edge of graph and must be clipped
C
               ELSE
                  RB5(1) = RB5(1)+(RB5(2)-RB5(1))*(FSBLEF-RB4(1))/
     +                          (RB4(2)-RB4(1))
                  RB4(1) = FSBLEF
               ENDIF
            ENDIF
C
C If point lies outside right-hand edge of graph, clip the chord
C
            IF (RB4(IP+1).GT.FSBRIG) THEN
               RB5(IP+1) = RB5(IP)+(RB5(IP+1)-RB5(IP))*(FSBRIG-RB4(IP))/
     +                         (RB4(IP+1)-RB4(IP))
               RB4(IP+1) = FSBRIG
            ENDIF
C
C Terminate cursor input if last point close enough to previous point
C
            DX = FSXSCA*(RB4(IP+1)-RB4(IP))
            DY = FSYSCA*(RB5(IP+1)-RB5(IP))
            IF (DX*DX+DY*DY.LT.TOLSQD) THEN
               NP = IP
               GOTO 190
            ENDIF
C
C Reject point if it is not to right of previous point
C
            IF (RB4(IP+1).LE.RB4(IP)) GOTO 170
C
C Draw chord if required
C
            IF (LVERIF) THEN
C
C Draw line
C
               IF (FSLINE(RB4(IP),RB5(IP),RB4(IP+1),RB5(IP+1))) GOTO 230
C
C Flush contents of graphics buffer
C
               IF (FSFLUS()) GOTO 230
            ENDIF
C
C Terminate cursor input if right-hand edge of graph has been reached
C
            IF (RB4(IP+1).EQ.FSBRIG) THEN
               NP = IP+1
               GOTO 190
            ENDIF
  180    CONTINUE
C
C Maximum for number of input points has been exceeded
C
         ERROR = 80
         GOTO 230
C
C Determine sampling interval for the output graph
C
  190    IF (LSIZE) THEN
            SAMPLE = (FSBRIG-FSBLEF)/REAL(SIZE-1)
         ELSE IF (LSAMPL) THEN
C
C Fault sampling interval greater than size of input graph
C
            IF (SAMPLE.GT.FSBRIG-FSBLEF) GOTO 250
         ELSE
            SAMPLE = 1.0
         ENDIF
C
C Determine size and centre position for the output graph
C
         NCOL = 1+NINT((FSBRIG-FSBLEF)/SAMPLE)
         CCOL = 1-NINT(FSBLEF/SAMPLE)
C
C Fault size for output graph which is too large
C
         IF (NCOL.GT.LNBUF/LNREAL) THEN
            ERROR = 5
            IDERR = IVALPN(NTO)
            GOTO 230
         ENDIF
C
C Initialise range of output positions spanned by input graph
C
         XMIN = FSBRIG
         XMAX = FSBLEF
C
C Sort contributions of input points to output graph
C
         DO 210 IP = 1,NP-1
C
C Fetch start and end point of chord
C
            X1 = RB4(IP)
            X2 = RB4(IP+1)
C
C Update limits of input graph
C
            XMIN = MIN(XMIN,X1)
            XMAX = MAX(XMAX,X2)
C
C Determine start of chord in output picture coordinates (rounded up)
C
            X1 = X1/SAMPLE
            IX1 = INT(X1)
            IF (REAL(IX1).LT.X1) IX1 = IX1+1
C
C Determine end of chord in output picture coordinates (rounded down)
C
            X2 = X2/SAMPLE
            IX2 = INT(X2)
            IF (REAL(IX2).GT.X2) IX2 = IX2-1
C
C Add chord's contribution to list corresponding to each point in
C the output graph
C
            DO 200 I = IX1+CCOL,IX2+CCOL
               IB1(I) = IP
  200       CONTINUE
  210    CONTINUE
C
C Generate interpolated output values
C
         DO 220 I = 1,NCOL
C
C Determine output position in picture coordinates
C
            IX = I-CCOL
C
C If option SYMMETRIC or ANTISYMMETRIC set, generate value from
C right-hand side of input graph
C
            IF (LSYMME.OR.LANTIS) IX = ABS(IX)
C
C Determine corresponding input position
C
            X = SAMPLE*REAL(IX)
C
C If output point to left of input graph, use left-most input value
C
            IF (X.LT.XMIN) THEN
               Y = RB5(1)
C
C Otherwise, if point to right of graph, use right-most value
C
            ELSE IF (X.GT.XMAX) THEN
               Y = RB5(NP)
C
C Otherwise, interpolate along corresponding input chord
C
            ELSE
               IP = IB1(IX+CCOL)
               Y = RB5(IP)+(RB5(IP+1)-RB5(IP))*(X-RB4(IP))/
     +                   (RB4(IP+1)-RB4(IP))
            ENDIF
C
C If option ANTISYMMETRIC is set, make sure output value satisfies
C requirement for antisymmetry
C
            IF (LANTIS) THEN
               IF (IX.LT.0) THEN
                  RB6(I) = -Y
               ELSE IF (IX.GT.0) THEN
                  RB6(I) = Y
               ELSE
                  RB6(I) = 0.0
               ENDIF
C
C Otherwise, use output value as is
C
            ELSE
               RB6(I) = Y
            ENDIF
  220    CONTINUE
C
C Open new picture for output graph
C
         LP1 = 0
         IF (SEMOPN(2,IVALPN(NTO),NCOL,1,1,NCLIMA,NFMFP,LP1)) GOTO 230
C
C Set centre position to corresspond to centre position of input graph
C
         IF (SEMCEN(LP1,CCOL,1,1)) GOTO 230
C
C Store output results
C
         IF (SEMROW(2,RB6,NFMFP,1,1,LP1)) GOTO 230
C
C No options - just return first point
C
      ELSE
C
C Read one point
C
         IF (FSXWIR(X0,Y0,X,Y)) GOTO 230
C
C Set variables - (X,Y) = cursor position
C
         IF (SEMLU(1,NX,X)) GOTO 230
         IF (SEMLU(1,NY,Y)) GOTO 230
C
C Mark point if required
C
         IF (LVERIF) THEN
            IF (FSMARK(X,Y,FSMMOD,FSMSIZ)) GOTO 230
         ENDIF
      ENDIF
C
C Update current frame/partition/picture number
C
      IF (FSELEC()) GOTO 230
C
  230 CONTINUE
      RETURN
C
C Common errors:
C
  250 ERROR = 3
      IDERR = NSAMPL
      GOTO 230
C
  260 ERROR = 3
      IDERR = NSIZE
      GOTO 230
C
C Copyright (C) 1987,1988,1989,1990:  Synoptics Ltd, All Rights Reserved
C
      END
