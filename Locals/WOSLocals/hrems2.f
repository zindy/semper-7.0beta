      LOGICAL FUNCTION SEMLAY(OPC,A,MP,FORM,LAYER,LPN)
C
C Reads/writes (OPC=1/2) layer LAYER of picture LPN to/from 2-D array A
C
C - MP is first physical dimension of A
C - calling module responsible for ensuring picture fits array
C - also at present that A contains enough free space following last
C   row to accommodate rounding up of transfer to next block boundary
C   (it is sufficient to ensure that MP represents a whole number of
C   blocks)
C - FORM will normally indicate Integer, Fp or Complex only, as Byte
C   arrays are not supported by Fortran; however, Byte arrays are in
C   fact loaded successfully by SEMLAY provided that MP represents a
C   whole number of Integers.
C
C Global declarations
      INCLUDE 'COMMON'
C
C Local declarations
      LOGICAL SEMROW,SEMLNF
      INTEGER A(*),OPC,MP,FORM,LAYER,LPN,NIPR,J,LNFORM
      INTEGER*4 N1
C
      SEMLAY=.TRUE.
C
C Establish number of integers per row
      IF (SEMLNF(FORM,LNFORM)) RETURN
      NIPR=(MP*LNFORM+LNINT-1)/LNINT
      N1=1
C
C Loop over layer rows
      N1=1
      DO 10 J=1,NROWS(LPN)
C Read/write next row to/from A with appropriate offset
         IF (SEMROW(OPC,A(N1),FORM,J,LAYER,LPN)) RETURN
         N1=N1+NIPR
10    CONTINUE
      SEMLAY=.FALSE.
      RETURN
C
      END
C Semper 6.2 local module MANDEL - exactly as on Vax
C
C Generates Mandelbrot patterns in TO
C
      SUBROUTINE MANDEL
C
C For further information see Computer Recreations
C Scientific American August 1985.
C
C Verb descriptor:
C  Mandelbrot :MANDEL size=256 si2= position= po2= width=3 power=2 +
C    levels=10 $1=sel to=$1 open(lp1,new)=to
C
C GLobal declarations
      INCLUDE 'COMMON'
      INTEGER MAXIPB
      PARAMETER (MAXIPB=LNBUF/LNINT)
C
C Local declarations
      LOGICAL SEMROW
      INTEGER IVAL,COUNT,POWER,GREY,NCOL,NROW,CCOL,CROW,I,J
      COMPLEX CENTRE,C,Z
      REAL VAL,WIDTH,STEP
      INTEGER IB1(MAXIPB)
      EQUIVALENCE (RB1,IB1)
C
C Packed names
      INTEGER NPOSIT,NPO2,NWIDTH,NLEVEL,NPOWER
      PARAMETER (NPOSIT=26219,NPO2=26232,NWIDTH=-5165)
      PARAMETER (NLEVEL=19422,NPOWER=26223)
C
C Centre, WIDTH of area of interest
      CENTRE=CMPLX(VAL(NPOSIT),VAL(NPO2))
      WIDTH=VAL(NWIDTH)
C Exponent in z^n+c)
      POWER=VAL(NPOWER)
C Number of grey levels
      GREY=IVAL(NLEVEL)

C Establish dimensions
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      CCOL=CCOLN(LP1)
      CROW=CROWN(LP1)
      STEP=WIDTH/NCOL
C
C Loop over rows
      DO 30 J=1,NROW
         C=CENTRE+CMPLX(REAL(1-CCOL),REAL(CROW-J))*STEP
C Loop over pixels
         DO 20 I=1,NCOL
            Z=0.
C Loop over grey level count
            COUNT=0
   10       IF (REAL(Z)**2+AIMAG(Z)**2.LT.2. .AND. COUNT.LT.GREY) THEN
               IF (POWER.EQ.2) THEN
                  Z=Z*Z+C
               ELSE
                  Z=Z**POWER+C
               ENDIF
               COUNT=COUNT+1
               GOTO 10
C End count loop
            ENDIF
            IB1(I)=COUNT
            C=C+STEP
   20    CONTINUE
C End pixel loop
C
C Output result
         IF (SEMROW(2,IB1,NFMINT,J,1,LP1)) RETURN
   30 CONTINUE
C End row loop
C
      RETURN
      END
C Semper 6 local processing module APODIS
C
      SUBROUTINE APODIS
C
C Applies 2-D separable window function to image, with various profiles:
C   [default] half-cosine window
C   TRIANGLE triangular ramp
C   HANN   von Hann (hanning) window
C   EXPON  exp{-x^8/r^8} with r = .4 of dimn
C Internal form Fp only
C Single layer only
C
C Verb descriptor:
C  Apodise :APODIS triangle hann gauss >$sel
C
C Global declarations
      INCLUDE 'COMMON'
C
C Local declarations
      REAL VAL,V,S,X,Y,YF,F,W
      LOGICAL SEMOPN,SEMROW,OPT,HCOSIN,TRIANG,VHANN,EXPON
      INTEGER SEMFRM,NCOL,NROW,CCOL,CROW,N,IMAX,I,I1,FORM
      PARAMETER (PIB2=PI/2)
C
C Packed names
      INTEGER NTO,NTRIAN,NHANNI,NEXPON
      PARAMETER (NTO=-601,NTRIAN=-730,NHANNI=12854,NEXPON=8976)
C
C Initialise
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      CCOL=NCOL/2+1
      CROW=NROW/2+1
C
      TRIANG=OPT(NTRIAN)
      VHANN=OPT(NHANNI)
      EXPON=OPT(NEXPON)
      HCOSIN=.NOT.(TRIANG.OR.VHANN.OR.EXPON)
C
C Open output, forcing Byte to Integer for windows with centre > 1
      FORM=SEMFRM(FORMN(LP1))
      IF (FORMN(LP1).EQ.NFMBYT.AND.(HCOSIN.OR.VHANN)) FORM=NFMINT
      LP2=LP1
      IF (SEMOPN(2,IVALPN(NTO),NCOL,NROW,1,CLASSN(LP1),FORM,LP2)) RETURN
C
C Set up 1-D xwindow function in RB2
      R=NCOL*.4
C
C Loop over pixels
      DO 1 I=1,NCOL
         X=ABS(I-CCOL)
         IF (EXPON) THEN
C 8th-order neg exp window
            RB2(I)=EXP(-(X/R)**8)
C
         ELSE IF (VHANN) THEN
C von Hann window
            RB2(I)=1+COS(TWOPI*X/NCOL)
C
         ELSE IF (TRIANG) THEN
C Triangle window
            RB2(I)=1-2*X/REAL(NCOL)
         ELSE
C
C Default: Half-cosine window
             RB2(I)=PIB2*COS(PI*X/NCOL)
         ENDIF
 1       CONTINUE
C
C Loop over rows
      R=NROW*.4
      DO 22 N=1,NROW
         IF (SEMROW(1,RB1,NFMFP,N,1,LP1)) RETURN
         Y=ABS(N-CROW)
C
C Calculate 1-D y-window function W
         IF (EXPON) THEN
            W=EXP(-(Y/R)**8)
         ELSE IF (VHANN) THEN
C von Hann window
            W=1+COS(TWOPI*Y/NROW)
         ELSE IF (TRIANG) THEN
C Triangle window
            W=1-2*Y/REAL(NROW)
         ELSE
C Default: Half-cosine window
            W=PIB2*COS(PI*Y/NROW)
         ENDIF
C
C Apply window to pixels
         DO 8 I=1,NCOL
            RB1(I)=RB1(I)*W*RB2(I)
8        CONTINUE
          IF (SEMROW(2,RB1,NFMFP,N,1,LP2)) RETURN
22    CONTINUE
      RETURN
C
      END
C Semper 6 local processing module MERGES - exactly as elsewhere
C Changes to MERGES by LDM to avoid intrinsic conflicts
C
      SUBROUTINE MERGES
C
C Combines two sources LP1,LP3 to LP2 on geometrical basis
C - only mode offered currently is LP1 if azim(X,Y) > ANGLE (up to ANG+pi)
C   allowing top/bot split via ANG 0 (default)
C           left/right     via ANG PI/2
C   etc
C Intended for display purposes, so using Fp only; faster than CALC
C
C Verb descriptor:
C  Merge :MERGES with= angle= >$w3
C
C Global declarations
      INCLUDE 'COMMON'
C
C Local declarations
      REAL VAL,V,KX,KY,X,Y,XI,X1,X2
      LOGICAL SEMROW
      INTEGER IVALPN,NCOL,NROW,CCOL,CROW,N,I1,I2,I
C Packed names
      INTEGER NANGLE,NFROM
      PARAMETER (NANGLE=2167,NFROM=10335)
C
C Initialise
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      CCOL=CCOLN(LP1)
      CROW=CROWN(LP1)
      X1=1-CCOL
      X2=NCOL-CCOL
C
C Check source sizes match
      IF (NCOLS(LP3).NE.NCOL.OR.NROWS(LP3).NE.NROW) THEN
         IDERR=IVALPN(NFROM)
         ERROR=5
         RETURN
      ENDIF
C
C Establish split line direction
      V=VAL(NANGLE)+PI/2
      KX=COS(V)
      KY=SIN(V)
C
C Pass through data
      DO 2 N=1,NROW
         IF (SEMROW(1,RB1,NFMFP,N,1,LP1)) RETURN
         IF (SEMROW(1,RB3,NFMFP,N,1,LP3)) RETURN
C
C If split line horiz..
         Y=CROW-N
         IF (KX.EQ.0.) THEN
C ..output LP1 or LP3 unchanged
            IF (KY*Y.GE.0.) THEN
               GOTO 3
            ELSE
               GOTO 4
            ENDIF
         ENDIF
C
C If line at an angle, find intersection
         X=1-CCOL
         XI=-KY*Y/KX
C
C Line intersects row in range: establish which pic appears left
         IF (KX*X+KY*Y.GE.0.) THEN
C LP1 left of XI, LP3 right
C If intersection out of range, output LP1 unchanged
            IF (XI.LE.X1.OR.XI.GE.X2) GOTO 3
            X=XI+REAL(CCOL)
            I1=X
            IF (X.NE.REAL(I1)) I1=I1+1
            IF (I1.GT.NCOL/2) THEN
               I2=NCOL
               GOTO 6
            ELSE
               I2=I1-1
               I1=1
               GOTO 7
            ENDIF
         ELSE
C LP3 left of XI, LP1 right:
C If intersection out of range, output LP1 unchanged
            IF (XI.LE.X1.OR.XI.GE.X2) GOTO 4
            I2=INT(XI+REAL(CCOL))
            IF (I2.GT.NCOL/2) THEN
               I1=I2+1
               I2=NCOL
               GOTO 7
            ELSE
               I1=1
               GOTO 6
            ENDIF
         ENDIF
C
C Xfer LP3 to LP1 and output
6        DO 8 I=I1,I2
            RB1(I)=RB3(I)
8        CONTINUE
3        IF (SEMROW(2,RB1,NFMFP,N,1,LP2)) RETURN
         GOTO 2
C Xfer LP1 to LP3 and output
7        DO 9 I=I1,I2
            RB3(I)=RB1(I)
9        CONTINUE
4        IF (SEMROW(2,RB3,NFMFP,N,1,LP2)) RETURN
C
C End of data pass
2     CONTINUE
      RETURN
C
      END
C Semper 6 local processing module DPZERO
C
C Marks DP zeros - sin^2(gamma) without tilt - on current display;
C for use in matching experimental diffractograms
C
      SUBROUTINE DPZERO
C
C The imaging conditions are defined by a key DEFOCUS and variables
C STEP(2), ASTIGMATISM,APHI, in PHYSICAL units (from KV,CS) as for CTF;
C marks all zeros curves up to max sp.freq. KMAX(nm), defaulting to min
C along X/Y axes
C
C Verb descriptor needed:
C   DPZeros :DPZERO $1=dis kmax= angle= an2= erase zeros
C
C Global declarations
      INCLUDE 'COMMON'
C
C Local declarations
      LOGICAL FSINIT,FSVIEW,FSERAS,FSLINE,VARSET,OPT,OPTNO,ABANDN
      INTEGER IVALPN
C
      REAL VAL,GAMMA,DEF,AST,APHI,SCH,GL,V,CS,G,D,D2,PHI1
      REAL X,K,K2,KP,KO,K2O,DKX,DKY,PHI,DPHI,C,S,CO,SO
      LOGICAL HALF
      INTEGER N,N1,N2,NZ,NZO,NCOL,NROW,ID,NDIR
C
C Packed names
      INTEGER NDOLL1,NERASE,NVIEW,NZEROS,NANGLE,NAN2
      INTEGER NKV,NCS,NSTEP,NST2,NAPHI,NDEFOC,NASTIG,NKMAX
      PARAMETER (NDOLL1=-12441, NERASE=8721)
      PARAMETER (NVIEW=-3566, NZEROS=-9819, NANGLE=2167, NAN2=2192)
      PARAMETER (NKV=18480, NCS=5560, NSTEP=31205, NST2=31232)
      PARAMETER (NAPHI=2248, NDEFOC=6606, NASTIG=2380, NKMAX=18121)
C
C Statement function defining gamma
      GAMMA(K,D) = PI * (.5*K**2 - D) * K**2
C
C Initialise
C ----------
C
C Find display number
      N=IVALPN(NDOLL1)
C Set up graphics coordinates mode: picture
      IF (FSINIT(3,N)) GOTO 3000
C If option VIEW is set, switch view to area of interest
      IF (FSVIEW(OPT(NVIEW))) GOTO 3000
C Erase unless NOERASE
      IF (.NOT.OPTNO(NERASE)) THEN
         IF (FSERAS(2,FSBLEF,FSBRIG,FSBBOT,FSBTOP)) GOTO 3000
      ENDIF
C Reset clipping limits to picture border
      FSXMIN=FSBLEF
      FSXMAX=FSBRIG
      FSYMIN=FSBBOT
      FSYMAX=FSBTOP
C Default mark mode to 5
      IF (FSMMOD.EQ.0) FSMMOD=5
C
C Note dimension (assuming square or 1-D) and whether half-plane
      NCOL=FSBRIG-FSBLEF+1
      NROW=FSBTOP-FSBBOT+1
      HALF=FSBLEF.EQ.0
      IF (HALF) NCOL=2*(NCOL-1)
C
C Initialise
C ----------
C Number of section dirns
      IF (VARSET(NANGLE)) THEN
         PHI1=VAL(NANGLE)
      ELSE
         PHI1=-PI/2
      ENDIF
      IF (HALF) PHI1=MAX(PHI1,-PI/2)
      IF (VARSET(NAN2)) THEN
         D2=VAL(NAN2)
      ELSE
         D2=PHI1+2*PI
      ENDIF
      IF (HALF) D2=MIN(D2,PI/2)
C Angular sampling 0.1rad gives about 15 sections per quadrant
      DPHI=.1
      NDIR=MAX(NINT((D2-PHI1)/DPHI),2)
      DPHI=(D2-PHI1)/(NDIR-1)
C
C Imaging parameters
      V=VAL(NKV)*1E3
      IF (V.EQ.0.) THEN
         ERROR=3
         IDERR=NKV
         RETURN
      ENDIF
      CS=VAL(NCS)*1E6
      IF (CS.EQ.0.) THEN
         ERROR=3
         IDERR=NCS
         RETURN
      ENDIF
C Wavelength/nm
      X=V*1.60206E-4/9.1083/2.99763**2
      X=6.62517/SQRT(2.0*1.60206*9.1083*V*(1.0+X/2.0))
C Scherzer and Glaser units/nm and Glaser/Scherzer/mrad
      SCH=SQRT(CS*X)
      GL=SQRT(SQRT(CS*X*X*X))
      DEF=VAL(NDEFOC)/SCH
      AST=VAL(NASTIG)/SCH
      IF (AST.LT.0.) THEN
         ERROR=3
         IDERR=NASTIG
         GOTO 3000
      ENDIF
      APHI=VAL(NAPHI)
      DKX=VAL(NSTEP)
      IF (DKX.LE.0.) THEN
         ERROR=3
         IDERR=NSTEP
         GOTO 3000
      ENDIF
      DKY=VAL(NST2)
      IF (DKY.LE.0.) DKY=DKX
      DKX=GL/NCOL/DKX
      DKY=GL/NROW/DKY
C
C Max K, defaulting to peripheral value
      KP=VAL(NKMAX)*GL
      IF (KP.LE.0.) KP=MIN(NCOL/2*DKX,NROW/2*DKY)
C
C Gamma range, and hence range of pi/2 multiples covered
      D=DEF+.5*AST
      D2=DEF-.5*AST
      G=MIN(0.,GAMMA(KP,D))
      IF (D.GT.0.) THEN
         K=SQRT(D)
         IF (K.LT.KP) G=MIN(G,GAMMA(K,D))
      ENDIF
      N1=G/PI*2
      G=MAX(0.,GAMMA(KP,D2))
      N2=G/PI*2
C
C Zeros or maxima?
      IF (OPT(NZEROS)) THEN
C If zeros, force range limits inwards to even pi/2 multiple
         IF (N1.NE.N1/2*2) N1=N1-1
         IF (N2.NE.N2/2*2) N2=N2+1
      ELSE
C If maxima, force range limits inwards to odd PI/2 multiple
         IF (N1/2*2.EQ.N1) N1=N1-1
         IF (N2/2*2.EQ.N2) N2=N2+1
      ENDIF
C
C Begin loop over zero orders
      DO 124 N=N1,N2,2
C
C Begin loop over directions
         PHI=PHI1
         ID=1
123      C=COS(PHI)/DKX
         S=SIN(PHI)/DKY
         D=DEF+.5*AST*COS(2*(PHI-APHI))
C
C For effective defocus D, gamma is pi.k^2(.5k^2-D); zero order n
C occurs where gamma = n.pi, giving k^2 = D +/- root(D^2+n)
C
            NZ=0
            X=D*D+N
            IF (X.GE.0.) THEN
               X=SQRT(X)
               IF (D+X.GE.0.) THEN
                  NZ=1
                  K=SQRT(D+X)
                  IF (D-X.GE.0) THEN
                     NZ=2
                     K2=SQRT(D-X)
                  ENDIF
               ENDIF
            ENDIF
C
C Unless first time, join to zeros in prev dirn
         IF (ID.NE.1) THEN
            IF (NZ.EQ.0) THEN
C No zeros in this dirn: close any for prev dirn
               IF (NZO.NE.0) THEN
                  IF (KO.LE.KP.OR.K2O.LE.KP) THEN
                     IF (FSLINE(KO*CO,KO*SO,K2O*CO,K2O*SO)) RETURN
                  ENDIF
               ENDIF
            ELSE IF (NZ.EQ.1) THEN
C One zero in this dirn: join to prev dirn
                  IF (KO.LE.KP.OR.K.LE.KP) THEN
                     IF (FSLINE(KO*CO,KO*SO,K*C,K*S)) RETURN
                  ENDIF
            ELSE
C Two zeros in this dirn: join to any zeros in prev dirn..
               IF (NZO.NE.0) THEN
                  IF (KO.LE.KP.OR.K.LE.KP) THEN
                     IF (FSLINE(KO*CO,KO*SO,K*C,K*S)) RETURN
                  ENDIF
                  IF (K2O.LE.KP.OR.K2.LE.KP) THEN
                     IF (FSLINE(K2O*CO,K2O*SO,K2*C,K2*S)) RETURN
                  ENDIF
               ELSE
C .. and connect the two otherwise
                  IF (K.LE.KP.OR.K2.LE.KP) THEN
                      IF (FSLINE(K*C,K*S,K2*C,K2*S)) RETURN
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
C Stack data on this dirn
         KO=K
         K2O=K2
         NZO=NZ
         CO=C
         SO=S
C
C Incr dirn
         PHI=PHI+DPHI
         ID=ID+1
         IF (ID.LE.NDIR) GOTO 123
C
C Abandon requested?
         IF (ABANDN(ERROR)) RETURN
C
  124    CONTINUE
C
C Exit
3000  RETURN
C
      END
C Semper VI local module  POLYF
C
      SUBROUTINE POLYF
C
C As verb PF2, fits biquadratic  to subregion of LP1
C (standard keys) or to arbitrary region defined by non-zero pixels of
C LP3.  If TO set, generates output matching source size as follows:
C - in default, fitted function
C - if SUBTRACT, source minus fitted
C - if DIVIDE, source / fitted
C - if both, (source minus fitted) / fitted
C and in all cases, if NOCONSTANT the constant term of the fitted
C function is omitted.
C
C Verb descriptor:
C Pf2 :POLYF within= >$size >$position >Select to=
C subtract divide constantC Notes on fitting:
C
C Lots of trouble found fitting 1+2x+3y+4^2+5xy+6y^2 over 101x101 pic
C - in SP with no X,Y scaling (via XSC,YSC here), got .0042 for 1, with
C     others within .0005 at least; matrix had top left far smaller
C     than bot rig
C - with X,Y scaling as here, got 1.46 for 1 with others within .0007;
C     matrix top left about 100 times bot rig
C - with X,Y scaled so that top left and bot rig equal, still got
C     1.64 for 1 with others within .0003
C - with matrix and RHS calculated in DP, got coeffs correct to 6figs
C On a different problem, with coeffs 1,.02,.02,.0004,.0004,.0004 so
C that each term has same max (viz 1), the first was still poor, getting
C 3e-7 for 1 (others OK), but the other three methods were all fine.
C
C Global declarations
      INCLUDE 'COMMON'
C Local declarations
      LOGICAL SEMOPN,SEMROW,SEMLU,TSTSRG,OPTNO,VARSET
      LOGICAL ARBREG,NOCON
      REAL A(6,6),B(6),CV(6)
C      real mvprod
C      logical semcon
      INTEGER SEMFRM
      EQUIVALENCE (SMGI1,NC1),(SMGI2,NR1),(SMGI4,NC2),(SMGI5,NR2)
C
C Packed names
C C, CX, CY, CXX, CXY, CYY
      INTEGER CNAME(6)
      DATA CNAME /4800,5760,5800,5784,5785,5825/
      DATA NTO/-601/,NWITH/-5181/,NSUBTR/31242/,NDIVID/6782/
      DATA NCONST/5414/
C
C debug!
C      mvprod(i)=a(i,1)*cv(1)+a(i,2)*cv(2)+a(i,3)*cv(3)
C     +         +a(i,4)*cv(4)+a(i,5)*cv(5)+a(i,6)*cv(6)
C
C      if (semcon('Entry to POLYF')) return
C
C Initialise
      IF (NLAYS(LP1).NE.1) THEN
         ERROR=62
         IDMESS='PF2'
         RETURN
      ENDIF
      NC0=CCOLN(LP1)
C
C Find fitted region
      ARBREG=VARSET(NWITH)
      IF (ARBREG) THEN
         NC1=1
         NR1=1
         IF (SEMOPN(1,IVALPN(NWITH),NC2,NR2,NL2,M,N,LP3)) RETURN
         IF (NC2.NE.NCOLS(LP1).OR.NR2.NE.NROWS(LP1)
     +      .OR.NLAYS(LP3).NE.NLAYS(LP1)) THEN
            ERROR=5
            IDERR=IVALPN(NWITH)
            RETURN
         ENDIF
      ELSE
         IF (TSTSRG(2,LP1)) RETURN
      ENDIF
C
C Initialise accumulators
      DO 10 N=1,6
         B(N)=0
         DO 10 I=1,6
            A(I,N)=0
 10   CONTINUE
C
C Loop over fitted rows accumulating pixel and coord sums;
C all coords scaled into -1,1 to improve conditioning
      YSC=SQRT(SQRT(80./NROWS(LP1)**4))
      XSC=SQRT(SQRT(80./NCOLS(LP1)**4))
      DO 30 N=NR1,NR2
         IF (SEMROW(1,RB1,NFMFP,N,1,LP1)) RETURN
         IF (ARBREG) THEN
            IF (SEMROW(1,RB2,NFMFP,N,1,LP3)) RETURN
         ENDIF
         Y=(CROWN(LP1)-N)*YSC
         DO 20 I=NC1,NC2
            IF (ARBREG.AND.RB2(I).EQ.0) GOTO 20
            X=(I-NC0)*XSC
            P=RB1(I)
            B(1)=B(1)+P
            B(2)=B(2)+P*X
            B(3)=B(3)+P*Y
            B(4)=B(4)+P*X*X
            B(5)=B(5)+P*X*Y
            B(6)=B(6)+P*Y*Y
            A(1,1)=A(1,1)+1
            A(1,2)=A(1,2)+X
            A(1,3)=A(1,3)+Y
            A(1,4)=A(1,4)+X*X
            A(1,5)=A(1,5)+X*Y
            A(1,6)=A(1,6)+Y*Y
            A(2,4)=A(2,4)+X*X*X
            A(2,5)=A(2,5)+X*X*Y
            A(2,6)=A(2,6)+X*Y*Y
            A(3,6)=A(3,6)+Y*Y*Y
            A(4,4)=A(4,4)+X*X*X*X
            A(4,5)=A(4,5)+X*X*X*Y
            A(4,6)=A(4,6)+X*X*Y*Y
            A(5,6)=A(5,6)+X*Y*Y*Y
            A(6,6)=A(6,6)+Y*Y*Y*Y
 20      CONTINUE
 30   CONTINUE
C
C Complete coeff matrix and RHS
      A(2,1)=A(1,2)
      A(2,2)=A(1,4)
      A(2,3)=A(1,5)
      A(3,1)=A(1,3)
      A(3,2)=A(2,3)
      A(3,3)=A(1,6)
      A(3,4)=A(2,5)
      A(3,5)=A(2,6)
      A(4,1)=A(1,4)
      A(4,2)=A(2,4)
      A(4,3)=A(3,4)
      A(5,1)=A(1,5)
      A(5,2)=A(2,5)
      A(5,3)=A(3,5)
      A(5,4)=A(4,5)
      A(5,5)=A(4,6)
      A(6,1)=A(1,6)
      A(6,2)=A(2,6)
      A(6,3)=A(3,6)
      A(6,4)=A(4,6)
      A(6,5)=A(5,6)
C
C      if (semcon('LS fit coefficient matrix:')) return
C      do 5 j=1,6
C         write (record,4) (a(j,i),i=1,6)
C 4       format (1x,6f13.5)
C         if (semcon(record)) return
C 5     continue
C      write (record,6) (b(i),i=1,6)
C 6    format (' RHS:  ',6f12.0)
C      if (semcon(record)) return
C
C Solve for coefficients
      CALL LQSET(A,6,6,B,CV,CONDN)
C
C debug...
C verify solution
C      write (record,11) (mvprod(i),i=1,6)
C 11   format (' A.CV is ',6f12.0)
C      if (semcon(record)) return
C
C      write (record,183) condn
C 183  format (' Condition number ',g12.4)
C      if (semcon(record)) return
C      if (semcon(' After LQSET, before inv.scaling: soln vector:'))
C     +   return
C      write (record,182) (cv(i),i=1,6)
C 182  format (1x,6g12.4)
C      if (semcon(record)) return
C
C Restore X,Y scaling
      CV(2)=CV(2)*XSC
      CV(3)=CV(3)*YSC
      CV(4)=CV(4)*XSC*XSC
      CV(5)=CV(5)*XSC*YSC
      CV(6)=CV(6)*YSC*YSC
C
C Return coefficients C,CX,CY,CXX,CXY,CYY
      DO 100 I=1,6
         IF (SEMLU(1,CNAME(I),CV(I))) RETURN
 100  CONTINUE
C
C Is output needed?
      IF (.NOT.VARSET(NTO)) RETURN
C
C Yes: establish options
      MODE=1
      IF (VARSET(NSUBTR)) MODE=2
      IF (VARSET(NDIVID)) MODE=3
      IF (MODE.EQ.3.AND.VARSET(NSUBTR)) MODE=4
      NOCON=OPTNO(NCONST)
C
C Open output
      IF (SEMOPN(2,IVALPN(NTO),NCOLS(LP1),NROWS(LP1),1,
     +   CLASSN(LP1),SEMFRM(FORMN(LP1)),LP2)) RETURN
C
C Generate output rows
      DO 210 N=1,NROWS(LP1)
         IF (MODE.NE.1) THEN
            IF (SEMROW(1,RB1,NFMFP,N,1,LP1)) RETURN
         ENDIF
         Y=CROWN(LP1)-N
         DO 200 I=1,NCOLS(LP1)
            X=I-NC0
            F=CV(2)*X+CV(3)*Y+CV(4)*X*X+CV(5)*X*Y+CV(6)*Y*Y
            IF (.NOT.NOCON) F=F+CV(1)
C Switch code acc to mode
            IF (MODE.EQ.1) THEN
               RB1(I)=F
            ELSE IF (MODE.EQ.2) THEN
               RB1(I)=RB1(I)-F
            ELSE IF (MODE.EQ.3) THEN
               RB1(I)=RB1(I)/F
            ELSE
               RB1(I)=(RB1(I)-F)/F
            ENDIF
 200     CONTINUE
         IF (SEMROW(2,RB1,NFMFP,N,1,LP2)) RETURN
 210  CONTINUE
C
C Normal exit
      RETURN
      END
C
C Semper VI local module POLYF - DP version for exploration
C
      SUBROUTINE POLYFD
C
C As verb PF2, fits biquadratic  to subregion of LP1
C (standard keys) or to arbitrary region defined by non-zero pixels of
C LP3.  If TO set, generates output matching source size as follows:
C - in default, fitted function
C - if SUBTRACT, source minus fitted
C - if DIVIDE, source / fitted
C - if both, (source minus fitted) / fitted
C and in all cases, if NOCONSTANT the constant term of the fitted
C function is omitted.
C
C Global declarations
      INCLUDE 'COMMON'
C Local declarations
      LOGICAL SEMOPN,SEMROW,SEMLU,TSTSRG,OPTNO,VARSET
      LOGICAL ARBREG,NOCON
      REAL*8 A(6,6),B(6),CV(6)
      REAL*8 X,Y,XSC,YSC,P,CONDN
C      real*8 mvprod
C      logical semcon
      INTEGER SEMFRM
      EQUIVALENCE (SMGI1,NC1),(SMGI2,NR1),(SMGI4,NC2),(SMGI5,NR2)
C
C Packed names
C C, CX, CY, CXX, CXY, CYY
      INTEGER CNAME(6)
      DATA CNAME /4800,5760,5800,5784,5785,5825/
      DATA NTO/-601/,NWITH/-5181/,NSUBTR/31242/,NDIVID/6782/
      DATA NCONST/5414/
C
C debug!
C      mvprod(i)=a(i,1)*cv(1)+a(i,2)*cv(2)+a(i,3)*cv(3)
C     +         +a(i,4)*cv(4)+a(i,5)*cv(5)+a(i,6)*cv(6)
C
C      if (semcon('Entry to POLYF')) return
C
C Initialise
      IF (NLAYS(LP1).NE.1) THEN
         ERROR=62
         IDMESS='PF2'
         RETURN
      ENDIF
      NC0=CCOLN(LP1)
C
C Find fitted region
      ARBREG=VARSET(NWITH)
      IF (ARBREG) THEN
         NC1=1
         NR1=1
         IF (SEMOPN(1,IVALPN(NWITH),NC2,NR2,NL2,M,N,LP3)) RETURN
         IF (NC2.NE.NCOLS(LP1).OR.NR2.NE.NROWS(LP1)
     +      .OR.NLAYS(LP3).NE.NLAYS(LP1)) THEN
            ERROR=5
            IDERR=IVALPN(NWITH)
            RETURN
         ENDIF
      ELSE
         IF (TSTSRG(2,LP1)) RETURN
      ENDIF
C
C debug
C      if (semcon('Subregion col,row limits')) return
C      write (record,21) nc1,nc2,nr1,nr2
C 21   format (4i5)
C      if (semcon(record)) return
C
C Initialise accumulators
      DO 10 N=1,6
         B(N)=0
         DO 10 I=1,6
            A(I,N)=0
 10   CONTINUE
C
C Loop over fitted rows accumulating pixel and coord sums;
C all coords scaled into -1,1 to improve conditioning
      YSC=1./DBLE(NROWS(LP1))
      XSC=1./DBLE(NCOLS(LP1))
      DO 30 N=NR1,NR2
         IF (SEMROW(1,RB1,NFMFP,N,1,LP1)) RETURN
         IF (ARBREG) THEN
            IF (SEMROW(1,RB2,NFMFP,N,1,LP3)) RETURN
         ENDIF
         Y=DBLE(CROWN(LP1)-N)*YSC
         DO 20 I=NC1,NC2
            IF (ARBREG.AND.RB2(I).EQ.0) GOTO 20
            X=DBLE(I-NC0)*XSC
            P=RB1(I)
            B(1)=B(1)+P
            B(2)=B(2)+P*X
            B(3)=B(3)+P*Y
            B(4)=B(4)+P*X*X
            B(5)=B(5)+P*X*Y
            B(6)=B(6)+P*Y*Y
            A(1,1)=A(1,1)+1
            A(1,2)=A(1,2)+X
            A(1,3)=A(1,3)+Y
            A(1,4)=A(1,4)+X*X
            A(1,5)=A(1,5)+X*Y
            A(1,6)=A(1,6)+Y*Y
            A(2,4)=A(2,4)+X*X*X
            A(2,5)=A(2,5)+X*X*Y
            A(2,6)=A(2,6)+X*Y*Y
            A(3,6)=A(3,6)+Y*Y*Y
            A(4,4)=A(4,4)+X*X*X*X
            A(4,5)=A(4,5)+X*X*X*Y
            A(4,6)=A(4,6)+X*X*Y*Y
            A(5,6)=A(5,6)+X*Y*Y*Y
            A(6,6)=A(6,6)+Y*Y*Y*Y
 20      CONTINUE
 30   CONTINUE
C
C Complete coeff matrix and RHS
      A(2,1)=A(1,2)
      A(2,2)=A(1,4)
      A(2,3)=A(1,5)
      A(3,1)=A(1,3)
      A(3,2)=A(2,3)
      A(3,3)=A(1,6)
      A(3,4)=A(2,5)
      A(3,5)=A(2,6)
      A(4,1)=A(1,4)
      A(4,2)=A(2,4)
      A(4,3)=A(3,4)
      A(5,1)=A(1,5)
      A(5,2)=A(2,5)
      A(5,3)=A(3,5)
      A(5,4)=A(4,5)
      A(5,5)=A(4,6)
      A(6,1)=A(1,6)
      A(6,2)=A(2,6)
      A(6,3)=A(3,6)
      A(6,4)=A(4,6)
      A(6,5)=A(5,6)
C
C      if (semcon('LS fit coefficient matrix:')) return
C      do 5 j=1,6
C         write (record,4) (a(j,i),i=1,6)
C 4       format (1x,6f13.5)
C         if (semcon(record)) return
C 5     continue
C      write (record,6) (b(i),i=1,6)
C 6    format (' RHS:  ',6f12.0)
C      if (semcon(record)) return
C
C Solve for coefficients
      CALL LQSETD(A,6,6,B,CV,CONDN)
C
C debug...
C verify solution
C      write (record,11) (mvprod(i),i=1,6)
C 11   format (' A.CV is ',6f12.0)
C      if (semcon(record)) return
C
C      write (record,183) condn
C 183  format (' Condition number ',g12.4)
C      if (semcon(record)) return
C      if (semcon(' After LQSET, before inv.scaling: soln vector:'))
C     +   return
C      write (record,182) (cv(i),i=1,6)
C 182  format (1x,6g12.4)
C      if (semcon(record)) return
C
C Restore X,Y scaling
      CV(2)=CV(2)*XSC
      CV(3)=CV(3)*YSC
      CV(4)=CV(4)*XSC*XSC
      CV(5)=CV(5)*XSC*YSC
      CV(6)=CV(6)*YSC*YSC
C
C Return coefficients C,CX,CY,CXX,CXY,CYY
      DO 100 I=1,6
         IF (SEMLU(1,CNAME(I),REAL(CV(I)))) RETURN
 100  CONTINUE
C
C Is output needed?
      IF (.NOT.VARSET(NTO)) RETURN
C
C Yes: establish options
      MODE=1
      IF (VARSET(NSUBTR)) MODE=2
      IF (VARSET(NDIVID)) MODE=3
      IF (MODE.EQ.3.AND.VARSET(NSUBTR)) MODE=4
      NOCON=OPTNO(NCONST)
C
C Open output
      IF (SEMOPN(2,IVALPN(NTO),NCOLS(LP1),NROWS(LP1),1,
     +   CLASSN(LP1),SEMFRM(FORMN(LP1)),LP2)) RETURN
C
C Generate output rows
      DO 210 N=1,NROWS(LP1)
         IF (MODE.NE.1) THEN
            IF (SEMROW(1,RB1,NFMFP,N,1,LP1)) RETURN
         ENDIF
         Y=CROWN(LP1)-N
         DO 200 I=1,NCOLS(LP1)
            X=I-NC0
            F=CV(2)*X+CV(3)*Y+CV(4)*X*X+CV(5)*X*Y+CV(6)*Y*Y
            IF (.NOT.NOCON) F=F+CV(1)
C Switch code acc to mode
            IF (MODE.EQ.1) THEN
               RB1(I)=F
            ELSE IF (MODE.EQ.2) THEN
               RB1(I)=RB1(I)-F
            ELSE IF (MODE.EQ.3) THEN
               RB1(I)=RB1(I)/F
            ELSE
               RB1(I)=(RB1(I)-F)/F
            ENDIF
 200     CONTINUE
         IF (SEMROW(2,RB1,NFMFP,N,1,LP2)) RETURN
 210  CONTINUE
C
C Normal exit
      RETURN
      END
C
C Module LQSET solves linear equations, checking conditioning
C
      SUBROUTINE LQSETD(A,MA,N,RHS,SOLN,CONDN)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER MA,N,MAXN
      PARAMETER (MAXN=10)
      REAL*8 A(MA,N),RHS(N),SOLN(N),W(MAXN),V(MAXN,MAXN),T,WMAX,WMIN,CONDN
      INTEGER I
C Debug
C      character*80 record
C      logical semcon,ldum
C
C Make singular-value decomposition
      CALL SVDCMD(A,N,N,MA,MA,W,V)
C
C Debug
C      ldum=semcon('Coeff matrix eigenvalues:')
C      write (record,1) (w(i),i=1,n)
C 1    format (6g12.4)
C      ldum=semcon(record)
C
C Appraise conditioning
      WMAX=W(1)
      WMIN=W(1)
      DO 10 I=1,N
         T=ABS(W(I))
         WMAX=MAX(WMAX,T)
         WMIN=MIN(WMIN,T)
   10 CONTINUE
      IF (WMAX.EQ.0.OR.WMIN.LT.1E-5*WMAX) THEN
         CONDN=1E5
      ELSE
         CONDN=WMAX/WMIN
      ENDIF
 
C Zero small elements of W
      DO 20 I=1,N
         IF (W(I).LT.1E-6*WMAX) W(I)=0.
   20 CONTINUE
C
C Back-substitute
      CALL SVBKSD(A,W,V,N,N,MA,MA,RHS,SOLN)
C
      RETURN
      END
C
C Pro tem, from Numerical Recipes with no more than minor adjs
C
      SUBROUTINE SVDCMD(A,M,N,MP,NP,W,V)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER NMAX,M,N,MP,NP
      PARAMETER (NMAX=10)
      REAL*8 A(MP,*),W(*),V(NP,*),RV1(NMAX)
      INTEGER I,J,K,L,ITS,NM
      G=0.0
      SCALE=0.0
      ANORM=0.0
      DO 140 I=1,N
         L=I+1
         RV1(I)=SCALE*G
         G=0.0
         S=0.0
         SCALE=0.0
         IF (I.LE.M) THEN
            DO 10 K=I,M
               SCALE=SCALE+ABS(A(K,I))
   10       CONTINUE
            IF (SCALE.NE.0.0) THEN
               DO 20 K=I,M
                  A(K,I)=A(K,I)/SCALE
                  S=S+A(K,I)*A(K,I)
   20          CONTINUE
               F=A(I,I)
               G=-SIGN(SQRT(S),F)
               H=F*G-S
               A(I,I)=F-G
               IF (I.NE.N) THEN
                  DO 50 J=L,N
                     S=0.0
                     DO 30 K=I,M
                        S=S+A(K,I)*A(K,J)
   30                CONTINUE
                     F=S/H
                     DO 40 K=I,M
                        A(K,J)=A(K,J)+F*A(K,I)
   40                CONTINUE
   50             CONTINUE
               ENDIF
               DO 60 K= I,M
                  A(K,I)=SCALE*A(K,I)
   60          CONTINUE
            ENDIF
         ENDIF
         W(I)=SCALE*G
         G=0.0
         S=0.0
         SCALE=0.0
         IF ((I.LE.M).AND.(I.NE.N)) THEN
            DO 70 K=L,N
               SCALE=SCALE+ABS(A(I,K))
   70       CONTINUE
            IF (SCALE.NE.0.0) THEN
               DO 80 K=L,N
                  A(I,K)=A(I,K)/SCALE
                  S=S+A(I,K)*A(I,K)
   80          CONTINUE
               F=A(I,L)
               G=-SIGN(SQRT(S),F)
               H=F*G-S
               A(I,L)=F-G
               DO 90 K=L,N
                  RV1(K)=A(I,K)/H
   90          CONTINUE
               IF (I.NE.M) THEN
                  DO 120 J=L,M
                     S=0.0
                     DO 100 K=L,N
                        S=S+A(J,K)*A(I,K)
  100                CONTINUE
                     DO 110 K=L,N
                        A(J,K)=A(J,K)+S*RV1(K)
  110                CONTINUE
  120             CONTINUE
               ENDIF
               DO 130 K=L,N
                  A(I,K)=SCALE*A(I,K)
  130          CONTINUE
            ENDIF
         ENDIF
         ANORM=MAX(ANORM,(ABS(W(I))+ABS(RV1(I))))
  140 CONTINUE
C
      DO 200 I=N,1,-1
         IF (I.LT.N) THEN
            IF (G.NE.0.0) THEN
               DO 150 J=L,N
                  V(J,I)=(A(I,J)/A(I,L))/G
  150          CONTINUE
               DO 180 J=L,N
                  S=0.0
                  DO 160 K=L,N
                     S=S+A(I,K)*V(K,J)
  160             CONTINUE
                  DO 170 K=L,N
                     V(K,J)=V(K,J)+S*V(K,I)
  170             CONTINUE
  180          CONTINUE
            ENDIF
            DO 190 J=L,N
               V(I,J)=0.0
               V(J,I)=0.0
  190       CONTINUE
         ENDIF
         V(I,I)=1.0
         G=RV1(I)
         L=I
  200 CONTINUE
C
      DO 270 I=N,1,-1
         L=I+1
         G=W(I)
         IF (I.LT.N) THEN
            DO 210 J=L,N
               A(I,J)=0.0
  210       CONTINUE
         ENDIF
         IF (G.NE.0.0) THEN
            G=1.0/G
            IF (I.NE.N) THEN
            DO 240 J=L,N
               S=0.0
               DO 220 K=L,M
                  S=S+A(K,I)*A(K,J)
  220          CONTINUE
               F=(S/A(I,I))*G
               DO 230 K=I,M
                  A(K,J)=A(K,J)+F*A(K,I)
  230          CONTINUE
  240       CONTINUE
         ENDIF
         DO 250 J=I,M
            A(J,I)=A(J,I)*G
  250    CONTINUE
      ELSE
         DO 260 J= I,M
            A(J,I)=0.0
  260    CONTINUE
      ENDIF
      A(I,I)=A(I,I)+1.0
  270 CONTINUE
C
      DO 390 K=N,1,-1
         DO 370 ITS=1,30
            DO 280 L=K,1,-1
               NM=L-1
               IF ((ABS(RV1(L))+ANORM).EQ.ANORM)  GOTO 320
               IF ((ABS(W(NM))+ANORM).EQ.ANORM)  GOTO 290
  280       CONTINUE
  290       C=0.0
            S=1.0
            DO 310 I=L,K
               F=S*RV1(I)
               IF ((ABS(F)+ANORM).NE.ANORM) THEN
                  G=W(I)
                  H=SQRT(F*F+G*G)
                  W(I)=H
                  H=1.0/H
                  C= (G*H)
                  S=-(F*H)
                  DO 300 J=1,M
                     Y=A(J,NM)
                     Z=A(J,I)
                     A(J,NM)=(Y*C)+(Z*S)
                     A(J,I)=-(Y*S)+(Z*C)
  300             CONTINUE
               ENDIF
  310       CONTINUE
  320       Z=W(K)
            IF (L.EQ.K) THEN
               IF (Z.LT.0.0) THEN
                  W(K)=-Z
                  DO 330 J=1,N
                     V(J,K)=-V(J,K)
  330             CONTINUE
               ENDIF
               GOTO 380
            ENDIF
            X=W(L)
            NM=K-1
            Y=W(NM)
            G=RV1(NM)
            H=RV1(K)
            F=((Y-Z)*(Y+Z)+(G-H)*(G+H))/(2.0*H*Y)
            G=SQRT(F*F+1.0)
            F=((X-Z)*(X+Z)+H*((Y/(F+SIGN(G,F)))-H))/X
            C=1.0
            S=1.0
            DO 360 J=L,NM
               I=J+1
               G=RV1(I)
               Y=W(I)
               H=S*G
               G=C*G
               Z=SQRT(F*F+H*H)
               RV1(J)=Z
               C=F/Z
               S=H/Z
               F= (X*C)+(G*S)
               G=-(X*S)+(G*C)
               H=Y*S
               Y=Y*C
               DO 340 NM=1,N
                  X=V(NM,J)
                  Z=V(NM,I)
                  V(NM,J)= (X*C)+(Z*S)
                  V(NM,I)=-(X*S)+(Z*C)
  340          CONTINUE
               Z=SQRT(F*F+H*H)
               W(J)=Z
               IF (Z.NE.0.0) THEN
                  Z=1.0/Z
                  C=F*Z
                  S=H*Z
               ENDIF
               F= (C*G)+(S*Y)
               X=-(S*G)+(C*Y)
               DO 350 NM=1,M
                  Y=A(NM,J)
                  Z=A(NM,I)
                  A(NM,J)= (Y*C)+(Z*S)
                  A(NM,I)=-(Y*S)+(Z*C)
  350          CONTINUE
  360       CONTINUE
            RV1(L)=0.0
            RV1(K)=F
            W(K)=X
  370    CONTINUE
  380    CONTINUE
  390 CONTINUE
      RETURN
      END
C
      SUBROUTINE SVBKSD(U,W,V,M,N,MP,NP,B,X)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER NMAX,M,N,MP,NP,I,J,JJ
      PARAMETER (NMAX=10)
      REAL*8 U(MP,*),W(*),V(NP,*),B(*),X(*),TMP(10)
      DO 20 J=1,N
         S=0.
         IF(W(J).NE.0.) THEN
            DO 10 I=1,M
               S=S+U(I,J)*B(I)
   10       CONTINUE
            S=S/W(J)
         ENDIF
         TMP(J)=S
   20 CONTINUE
      DO 40 J=1,N
         S=0.
         DO 30 JJ=1,N
            S=S+V(J,JJ)*TMP(JJ)
   30    CONTINUE
         X(J)=S
   40 CONTINUE
      RETURN
      END
C Semper VI processing module SSTRIP
C
      SUBROUTINE SSTRIP
C
C Extracts from LP1 a picture TO containing a strip of LP1 around the
C curve defined by a position list in LP3; sections SIZE points in
C length, normal to the curve, are extracted in turn and placed in
C the rows of TO (dimensions SIZE by <curve length>).
C
C - code derived from EMBL (mainly by Tony Pitt, SSTR2 by A.N.Other),
C   and rewritten for VI by WOS: keys revised for consistency and
C   functionality altered (formerly embedded to match source array
C   size without option); key MARK added, mirror reflection of output
C   removed; bugs causing SSTR2 to loop indefinitely on some data
C   corrected (bad backward stepping at knots); better gradient
C   estimation introduced (via polynomial differentiation in SSTR2),
C   and incorrect interpolation formula in main corrected!
C
C Calls SSTR2 to interpolate between the supplied points on a
C fixed step basis; this requires at least four positions
C
C VD required:
C $w4 >$r3 open(lp3,old)=wit
C ...
C Sstrip :SSTRIP size=64 with=999 >$w4
C
      LOGICAL EXTRCT,SEMOPN,SEMROW
      LOGICAL MARSET,FSINIT,FSCURV,FSLINE,SEMLNF
      LOGICAL ANNOT,REFORM
      REAL XFIT(256),YFIT(256),XNORM(256),YNORM(256)
      REAL XF2(256),YF2(256),XN2(256),YN2(256)
      INTEGER SEMFRM,PASS,STRIPW,STRIPL,OUTF
      INTEGER*4 I4
C Packed names
      PARAMETER (NSIZE=30786,NWITH=-5181,NFROM=10335,NTO=-601)
      PARAMETER (NPOSIT=26219,NPO2=26232)
C
      INCLUDE 'COMMON'
      PARAMETER (MCP1=LNBUF/LNCOMP+1)
C
      EQUIVALENCE (RB1,XF2),(RB1(MCP1),YF2)
      EQUIVALENCE (RB2,XN2),(RB2(MCP1),YN2)
      EQUIVALENCE (RB4,XFIT),(RB4(MCP1),YFIT)
      EQUIVALENCE (RB5,XNORM),(RB5(MCP1),YNORM)
C
      STRIPL=LNBUF/LNREAL
      STRIPW=IVAL(NSIZE)
      XC=CCOLN(LP1)
      YC=CROWN(LP1)
C
C Check WITH
      IDERR=IVALPN(NWITH)
      IF (CLASSN(LP3).NE.NCLPLI) THEN
         ERROR=6
         RETURN
      ENDIF
      IF (NCOLS(LP3).LT.4.OR.NROWS(LP3).NE.1) THEN
         ERROR=5
         RETURN
      ENDIF
C
C Load position list
      IF (SEMROW(1,RB1,NFMFP,1,1,LP3)) RETURN
      IF (SEMROW(1,RB2,NFMFP,1,2,LP3)) RETURN
C
C Call SSTR2 to generate spline-fitted curve with unit normals
      CALL SSTR2(RB1,RB2,NCOLS(LP3),XFIT,YFIT,STRIPL,
     +   XNORM,YNORM,1.)
C Save the result in a workspace Plist
      LP4=LP3
      IF (SEMOPN(3,0,STRIPL,1,4,NCLPLI,NFMFP,LP4)) RETURN
      IF (SEMROW(2,XFIT,NFMFP,1,1,LP4)) RETURN
      IF (SEMROW(2,YFIT,NFMFP,1,2,LP4)) RETURN
      IF (SEMROW(2,XNORM,NFMFP,1,3,LP4)) RETURN
      IF (SEMROW(2,YNORM,NFMFP,1,4,LP4)) RETURN
C
C Open output
      OUTF=SEMFRM(FORMN(LP1))
      LP2=LP1
      IF (SEMOPN(2,IVALPN(NTO),STRIPW,STRIPL,1,
     +   CLASSN(LP1),OUTF,LP2)) RETURN
C
C If output form longer than fp, buffered output rows will need
C reforming locally before output
      IF (SEMLNF(OUTF,I)) RETURN
      IF (SEMLNF(NFMFP,N)) RETURN
      REFORM=I.GT.N
      R1=STRIPW/2
      R2=-(STRIPW-1)/2
C Mark strip on display?
      IF (MARSET(ANNOT,MARK)) RETURN
      IF (ANNOT) THEN
         IF (FSINIT(3,MARK)) RETURN
C
C Take end, midpoint and other end of strips in turn
         R=R1
         DO 20 PASS=1,3
C
C Scan along strip constructing curve for strip left, centre or right
            DO 10 N=1,STRIPL
               X2=XFIT(N)
               Y2=YFIT(N)
               DX=XNORM(N)
               DY=YNORM(N)
               RB1(N)=X2+R*DX
               RB2(N)=Y2+R*DY
   10       CONTINUE
C Mark curve
            IF (FSCURV(RB1,RB2,STRIPL,.FALSE.)) RETURN
C
C Select position on strip for next pass
            R=0.
            IF (PASS.EQ.2) R=R2
   20    CONTINUE
C
C Draw across ends of loop
         IF (FSLINE(XFIT(1)+R1*XNORM(1),YFIT(1)+R1*YNORM(1),
     +      XFIT(1)+R2*XNORM(1),YFIT(1)+R2*YNORM(1))) RETURN
         IF (FSLINE(XFIT(STRIPL)+R1*XNORM(STRIPL),
     +      YFIT(STRIPL)+R1*YNORM(STRIPL),
     +      XFIT(STRIPL)+R2*XNORM(STRIPL),
     +      YFIT(STRIPL)+R2*YNORM(STRIPL))) RETURN
      ENDIF
C
C Scan along strip again, generating output rows this time
C
C Loop over blocks of output rows fitting buffer
      NBUF=(LNBUF/LNREAL)/STRIPW
      DO 60 J=1,STRIPL,NBUF
         J1=J
         J2=MIN(J1+NBUF-1,STRIPL)
C
C Recover fitted plist
         IF (SEMROW(1,XF2,NFMFP,1,1,LP4)) RETURN
         IF (SEMROW(1,YF2,NFMFP,1,2,LP4)) RETURN
         IF (SEMROW(1,XN2,NFMFP,1,3,LP4)) RETURN
         IF (SEMROW(1,YN2,NFMFP,1,4,LP4)) RETURN
C
C Loop over rows within block
         IP=1
         DO 40 N=J1,J2
C
C Store positions required for one output row (one position
C along strip)
            DX=XN2(N)
            DY=YN2(N)
            X=XF2(N)+R1*DX
            Y=YF2(N)+R1*DY
            DO 30 I=1,STRIPW
               RB3(IP)=XC+X
               RB4(IP)=YC-Y
               X=X-DX
               Y=Y-DY
               IP=IP+1
   30       CONTINUE
   40    CONTINUE
C
C Call EXTRCT to interpolate values at all positions within block
         IF (EXTRCT(LP1,(J2-J1+1)*STRIPW,NFMFP,1,.FALSE.,0.)) RETURN
C
C Break up block and output rows
         IP=1
         DO 50 N=J1,J2
            IF (REFORM) THEN
               I4=STRIPW
               CALL CFORM(RB2(IP),RB1,NFMFP,OUTF,I4)
               IF (SEMROW(2,RB1,OUTF,N,1,LP2)) RETURN
            ELSE
               IF (SEMROW(2,RB2(IP),NFMFP,N,1,LP2)) RETURN
            ENDIF
            IP=IP+STRIPW
   50    CONTINUE
C
C End of loop over output blocks
   60 CONTINUE
      RETURN
C
      END
C Semper VI subsidiary module SSTR2
C
      SUBROUTINE SSTR2(XIN,YIN,NIN,XOUT,YOUT,NOUT,XNORM,YNORM,STEP)
C
C Fits a smooth curve through the points given in XIN and YIN,
C returning the coordinates in XOUT and YOUT.  The curve may be
C reentrant (i.e. multiply-valued in X or Y).  The output points
C are equally spaced along the curve, in steps of length STEP.
C
C NIN must be greater than 3.  If the end of the input points is
C reached before NOUT output points have been calculated, then
C NOUT will simply be reset to the number that have been produced,
C and the routine returns.
C
C On input:
C    XIN   x-coords of points to be fitted
C    YIN   y-coords
C    NIN   length of XIN,YIN
C    NOUT  maximum number of output points required
C    STEP  required output step size along the curve
C
C On output:
C    XOUT  x-coords of fitted points
C    YOUT  y-coords
C    NOUT  number of points fitted
C    XNORM x-coords of steps along local curve normal
C    YNORM y-coords
C
      REAL XIN(NIN),YIN(NIN),XT(10),YT(10),MSTEP
      REAL XOUT(NOUT),YOUT(NOUT),XNORM(NOUT),YNORM(NOUT)
      INTEGER ENDINP
C
C Variables (x3,y3), (x4,y4), (x5,y5) are three successive data points
C and vectors (a1,b1), (a1,b2), (a3,b3), (a4,b4) and (a5,b5) are vectors
C pointing to them from the previous data point; points (x1,y1) and
C (x2,y2) do not seem to be used explicitly
C
C Coefficients c0,c1,c2,c3 define a fitted cubic c0+c1.z+c2.z^2+c3.z^3
C for x coords, with similar set d0,d1.. for y coords
C
C Set up initial values ready for input data pass
      C0=XIN(1)
      XT(10)=C0
      XOUT(1)=C0
      D0=YIN(1)
      YT(10)=D0
      YOUT(1)=D0
      X3=XIN(2)
      Y3=YIN(2)
      A2=X3-C0
      B2=Y3-D0
      X4=XIN(3)
      Y4=YIN(3)
      A3=X4-X3
      B3=Y4-Y3
C Obtain a1 by linear extrapolation of a2,a3 backwards
      A1=2.*A2-A3
      B1=2.*B2-B3
      KIN=4
C Progress along curve is actually made in microsteps,
C each one tenth of the requested step size
      MSTEP=STEP*.1
      KOUT=1
      KT=0
      DR=0
      ENDINP=-1
C
C Duplicate differentiation section from main loop
      WT2=ABS(A2*B3-A3*B2)
      WT3=ABS(A1*B2-A2*B1)
      IF (WT2+WT3.NE.0.) GOTO 10
      WT2=SQRT(A2*A2+B2*B2)
      WT3=SQRT(A1*A1+B1*B1)
   10 COST3=WT2*A1+WT3*A2
      SINT3=WT2*B1+WT3*B2
      R=COST3*COST3+SINT3*SINT3
      IF (R.EQ.0.) GOTO 30
      R=SQRT(R)
      COST3=COST3/R
      SINT3=SINT3/R
      GOTO 30
C
C Next cycle at outermost level: take new input datum
C and fit new poly segments
   20 C0=X3
      D0=Y3
      X3=X4
      Y3=Y4
      X4=X5
      Y4=Y5
      A1=A2
      B1=B2
      A2=A3
      B2=B3
      A3=A4
      B3=B4
      KIN=KIN+1
      IF (KIN.LE.NIN) GOTO 30
      ENDINP=ENDINP+1
      A4=2.*A3-A2
      B4=2.*B3-B2
      GOTO 40
   30 X5=XIN(KIN)
      Y5=YIN(KIN)
      A4=X5-X4
      B4=Y5-Y4
   40 DXOUT=C0-X3
      DYOUT=D0-Y3
C
C Numerical differentiation (details not understood by WOS!)
      COST2=COST3
      SINT2=SINT3
      WT2=ABS(A3*B4-A4*B3)
      WT3=ABS(A1*B2-A2*B1)
      IF (WT2+WT3.NE.0.) GOTO 50
      WT2=SQRT(A3*A3+B3*B3)
      WT3=SQRT(A2*A2+B2*B2)
   50 COST3=WT2*A2+WT3*A3
      SINT3=WT2*B2+WT3*B3
      R=COST3*COST3+SINT3*SINT3
      IF (R.EQ.0.) GOTO 60
      R=SQRT(R)
      COST3=COST3/R
      SINT3=SINT3/R
C
C Evaluate polynomial coefficients
C - X is fitted by c0+c1.x+c2.x^2+c3.x^3 and Y by d0+d1.y...
   60 R=SQRT(A2*A2+B2*B2)
      C1=R*COST2
      C2=3.*A2-R*(2.*COST2+COST3)
      C3=A2-C1-C2
      D1=R*SINT2
      D2=3.*B2-R*(2.*SINT2+SINT3)
      D3=B2-D1-D2
C Record normal at first output point
      IF (KOUT.NE.1) GOTO 70
      R1=D1
      R2=-C1
      R=STEP/SQRT(R1*R1+R2*R2)
      XNORM(1)=R1*R
      YNORM(1)=R2*R
C Evaluate coefficients of polynomial for ds/dz
   70 SZ0=C1*C1+D1*D1
      SZ1=4.*(C1*C2+D1*D2)
      SZ2=4.*(C2*C2+D2*D2)+6.*(C1*C3+D1*D3)
      SZ3=12.*(C2*C3+D2*D3)
      SZ4=9.*(C3*C3+D3*D3)
C Set ds/dz itself, approximating z by 0
      SZ=SQRT(SZ0)
C
C Set initial Z to correspond to a distance MSTEP-DR beyond first datum
C (DR is left over in general from previous point)
      Z=(MSTEP-DR)/SZ
C
C Compute new X,Y from polynomial fits
   80 RNXT=C0+Z*(C1+Z*(C2+Z*C3))
      RNYT=D0+Z*(D1+Z*(D2+Z*D3))
C Is distance from previous point close to MSTEP?
      I=10
      IF (KT.GT.0) I=KT
      R=SQRT((RNXT-XT(I))**2+(RNYT-YT(I))**2)
      R=R/MSTEP-1.
      IF (ABS(R).LT..05) GOTO 90
C No: adjust a little and try again (should happen rarely)
      R=1.+R*.9
      Z=Z-DZ
      DZ=DZ/R
      Z=Z+DZ
      GOTO 80
C
C Record new microstep
   90 KT=KT+1
      XT(KT)=RNXT
      YT(KT)=RNYT
      IF (KT.LT.10) GOTO 100
C
C Every ten microsteps, record new step,
C together with the components of a step along the local normal
      KOUT=KOUT+1
      XOUT(KOUT)=RNXT
      YOUT(KOUT)=RNYT
      R1=D1+Z*(2.*D2+Z*3.*D3)
      R2=-C1-Z*(2.*C2+Z*3.*C3)
      R=STEP/SQRT(R1*R1+R2*R2)
      XNORM(KOUT)=R1*R
      YNORM(KOUT)=R2*R
      KT=0
C
C Output arrays exhausted?
      IF (KOUT.GE.NOUT) RETURN
C
C No: revise z microstep size with new ds/dz estimate
  100 SZ=SQRT(SZ0+Z*(SZ1+Z*(SZ2+Z*(SZ3+Z*SZ4))))
      DZ=MSTEP/SZ
C Microstep Z
      Z=Z+DZ
C Examine distance (r1,r2) from next target datum
      R1=XT(10)
      R2=YT(10)
      IF (KT.EQ.0) GOTO 110
      R1=XT(KT)
      R2=YT(KT)
  110 R1=R1-X3
      R2=R2-Y3
C Have we overshot the target in x or y,
C while within a microstep of it?
      IF ((R1*DXOUT.LT.0..OR.R2*DYOUT.LT.0.)
     +    .AND.SQRT(R1*R1+R2*R2).LT.MSTEP) GOTO 120
C No: save residue and make next microstep
      DXOUT=R1
      DYOUT=R2
      DR=SQRT(DXOUT*DXOUT+DYOUT*DYOUT)
      GOTO 80
C
C Yes: back up a microstep..
  120 KT=KT-1
      IF (KT.GE.0) GOTO 130
      KT=KT+10
      KOUT=KOUT-1
  130 Z=Z-DZ
C ..and fetch next input datum
      IF (ENDINP.LE.0) GOTO 20
C
C Input data exhausted
      NOUT=KOUT
      RETURN
C
      END
C
C Semper local module LQSET
C -  solves linear equations, checking conditioning
C
      SUBROUTINE LQSET(A,MA,N,RHS,SOLN,CONDN)
      INTEGER MA,N,MAXN
      PARAMETER (MAXN=10)
      REAL A(MA,N),RHS(N),SOLN(N),W(MAXN),V(MAXN,MAXN),T,WMAX,WMIN,CONDN
      INTEGER I
C Debug
C      character*80 record
C      logical semcon,ldum
C
C Make singular-value decomposition
      CALL SVDCMP(A,N,N,MA,MA,W,V)
C
C Debug
C      ldum=semcon('Coeff matrix eigenvalues:')
C      write (record,1) (w(i),i=1,n)
C 1    format (6g12.4)
C      ldum=semcon(record)
C
C Appraise conditioning
      WMAX=W(1)
      WMIN=W(1)
      DO 10 I=1,N
         T=ABS(W(I))
         WMAX=MAX(WMAX,T)
         WMIN=MIN(WMIN,T)
   10 CONTINUE
      IF (WMAX.EQ.0.OR.WMIN.LT.1E-5*WMAX) THEN
         CONDN=1E5
      ELSE
         CONDN=WMAX/WMIN
      ENDIF
 
C Zero small elements of W
      DO 20 I=1,N
         IF (W(I).LT.1E-6*WMAX) W(I)=0.
   20 CONTINUE
C
C Back-substitute
      CALL SVBKSB(A,W,V,N,N,MA,MA,RHS,SOLN)
C
      RETURN
      END
C
C Pro tem, from Numerical Recipes with no more than minor adjs
C
      SUBROUTINE SVDCMP(A,M,N,MP,NP,W,V)
      INTEGER NMAX,M,N,MP,NP
      PARAMETER (NMAX=10)
      REAL A(MP,*),W(*),V(NP,*),RV1(NMAX)
      REAL C,F,G,H,SCALE,ANORM,S,X,Y,Z
      INTEGER I,J,K,L,ITS,NM
      G=0.0
      SCALE=0.0
      ANORM=0.0
      DO 140 I=1,N
         L=I+1
         RV1(I)=SCALE*G
         G=0.0
         S=0.0
         SCALE=0.0
         IF (I.LE.M) THEN
            DO 10 K=I,M
               SCALE=SCALE+ABS(A(K,I))
   10       CONTINUE
            IF (SCALE.NE.0.0) THEN
               DO 20 K=I,M
                  A(K,I)=A(K,I)/SCALE
                  S=S+A(K,I)*A(K,I)
   20          CONTINUE
               F=A(I,I)
               G=-SIGN(SQRT(S),F)
               H=F*G-S
               A(I,I)=F-G
               IF (I.NE.N) THEN
                  DO 50 J=L,N
                     S=0.0
                     DO 30 K=I,M
                        S=S+A(K,I)*A(K,J)
   30                CONTINUE
                     F=S/H
                     DO 40 K=I,M
                        A(K,J)=A(K,J)+F*A(K,I)
   40                CONTINUE
   50             CONTINUE
               ENDIF
               DO 60 K= I,M
                  A(K,I)=SCALE*A(K,I)
   60          CONTINUE
            ENDIF
         ENDIF
         W(I)=SCALE *G
         G=0.0
         S=0.0
         SCALE=0.0
         IF ((I.LE.M).AND.(I.NE.N)) THEN
            DO 70 K=L,N
               SCALE=SCALE+ABS(A(I,K))
   70       CONTINUE
            IF (SCALE.NE.0.0) THEN
               DO 80 K=L,N
                  A(I,K)=A(I,K)/SCALE
                  S=S+A(I,K)*A(I,K)
   80          CONTINUE
               F=A(I,L)
               G=-SIGN(SQRT(S),F)
               H=F*G-S
               A(I,L)=F-G
               DO 90 K=L,N
                  RV1(K)=A(I,K)/H
   90          CONTINUE
               IF (I.NE.M) THEN
                  DO 120 J=L,M
                     S=0.0
                     DO 100 K=L,N
                        S=S+A(J,K)*A(I,K)
  100                CONTINUE
                     DO 110 K=L,N
                        A(J,K)=A(J,K)+S*RV1(K)
  110                CONTINUE
  120             CONTINUE
               ENDIF
               DO 130 K=L,N
                  A(I,K)=SCALE*A(I,K)
  130          CONTINUE
            ENDIF
         ENDIF
         ANORM=MAX(ANORM,(ABS(W(I))+ABS(RV1(I))))
  140 CONTINUE
C
      DO 200 I=N,1,-1
         IF (I.LT.N) THEN
            IF (G.NE.0.0) THEN
               DO 150 J=L,N
                  V(J,I)=(A(I,J)/A(I,L))/G
  150          CONTINUE
               DO 180 J=L,N
                  S=0.0
                  DO 160 K=L,N
                     S=S+A(I,K)*V(K,J)
  160             CONTINUE
                  DO 170 K=L,N
                     V(K,J)=V(K,J)+S*V(K,I)
  170             CONTINUE
  180          CONTINUE
            ENDIF
            DO 190 J=L,N
               V(I,J)=0.0
               V(J,I)=0.0
  190       CONTINUE
         ENDIF
         V(I,I)=1.0
         G=RV1(I)
         L=I
  200 CONTINUE
C
      DO 270 I=N,1,-1
         L=I+1
         G=W(I)
         IF (I.LT.N) THEN
            DO 210 J=L,N
               A(I,J)=0.0
  210       CONTINUE
         ENDIF
         IF (G.NE.0.0) THEN
            G=1.0/G
            IF (I.NE.N) THEN
            DO 240 J=L,N
               S=0.0
               DO 220 K=L,M
                  S=S+A(K,I)*A(K,J)
  220          CONTINUE
               F=(S/A(I,I))*G
               DO 230 K=I,M
                  A(K,J)=A(K,J)+F*A(K,I)
  230          CONTINUE
  240       CONTINUE
         ENDIF
         DO 250 J=I,M
            A(J,I)=A(J,I)*G
  250    CONTINUE
      ELSE
         DO 260 J= I,M
            A(J,I)=0.0
  260    CONTINUE
      ENDIF
      A(I,I)=A(I,I)+1.0
  270 CONTINUE
C
      DO 390 K=N,1,-1
         DO 370 ITS=1,30
            DO 280 L=K,1,-1
               NM=L-1
               IF ((ABS(RV1(L))+ANORM).EQ.ANORM)  GOTO 320
               IF ((ABS(W(NM))+ANORM).EQ.ANORM)  GOTO 290
  280       CONTINUE
  290       C=0.0
            S=1.0
            DO 310 I=L,K
               F=S*RV1(I)
               IF ((ABS(F)+ANORM).NE.ANORM) THEN
                  G=W(I)
                  H=SQRT(F*F+G*G)
                  W(I)=H
                  H=1.0/H
                  C= (G*H)
                  S=-(F*H)
                  DO 300 J=1,M
                     Y=A(J,NM)
                     Z=A(J,I)
                     A(J,NM)=(Y*C)+(Z*S)
                     A(J,I)=-(Y*S)+(Z*C)
  300             CONTINUE
               ENDIF
  310       CONTINUE
  320       Z=W(K)
            IF (L.EQ.K) THEN
               IF (Z.LT.0.0) THEN
                  W(K)=-Z
                  DO 330 J=1,N
                     V(J,K)=-V(J,K)
  330             CONTINUE
               ENDIF
               GOTO 380
            ENDIF
            X=W(L)
            NM=K-1
            Y=W(NM)
            G=RV1(NM)
            H=RV1(K)
            F=((Y-Z)*(Y+Z)+(G-H)*(G+H))/(2.0*H*Y)
            G=SQRT(F*F+1.0)
            F=((X-Z)*(X+Z)+H*((Y/(F+SIGN(G,F)))-H))/X
            C=1.0
            S=1.0
            DO 360 J=L,NM
               I=J+1
               G=RV1(I)
               Y=W(I)
               H=S*G
               G=C*G
               Z=SQRT(F*F+H*H)
               RV1(J)=Z
               C=F/Z
               S=H/Z
               F= (X*C)+(G*S)
               G=-(X*S)+(G*C)
               H=Y*S
               Y=Y*C
               DO 340 NM=1,N
                  X=V(NM,J)
                  Z=V(NM,I)
                  V(NM,J)= (X*C)+(Z*S)
                  V(NM,I)=-(X*S)+(Z*C)
  340          CONTINUE
               Z=SQRT(F*F+H*H)
               W(J)=Z
               IF (Z.NE.0.0) THEN
                  Z=1.0/Z
                  C=F*Z
                  S=H*Z
               ENDIF
               F= (C*G)+(S*Y)
               X=-(S*G)+(C*Y)
               DO 350 NM=1,M
                  Y=A(NM,J)
                  Z=A(NM,I)
                  A(NM,J)= (Y*C)+(Z*S)
                  A(NM,I)=-(Y*S)+(Z*C)
  350          CONTINUE
  360       CONTINUE
            RV1(L)=0.0
            RV1(K)=F
            W(K)=X
  370    CONTINUE
  380    CONTINUE
  390 CONTINUE
      RETURN
      END
C
      SUBROUTINE SVBKSB(U,W,V,M,N,MP,NP,B,X)
      INTEGER NMAX,M,N,MP,NP,I,J,JJ
      PARAMETER (NMAX=10)
      REAL U(MP,*),W(*),V(NP,*),B(*),X(*),TMP(10)
      REAL S
      DO 20 J=1,N
         S=0.
         IF(W(J).NE.0.) THEN
            DO 10 I=1,M
               S=S+U(I,J)*B(I)
   10       CONTINUE
            S=S/W(J)
         ENDIF
         TMP(J)=S
   20 CONTINUE
      DO 40 J=1,N
         S=0.
         DO 30 JJ=1,N
            S=S+V(J,JJ)*TMP(JJ)
   30    CONTINUE
         X(J)=S
   40 CONTINUE
      RETURN
      END
