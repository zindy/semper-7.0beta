C Semper 6 local PL package for Position List manipulation
C
C Semper 6 processing module PLANAL
C
      SUBROUTINE PLANAL
C
C Provides verb PLANALYSE.  Expects in LP1 an indexed plist with at
C least 4 layers as below, containing distorted position x,y coords,
C h,k indices, and optional position weights; generates in output TO a
C plist with additional layers measuring the distortion from an ideal
C lattice defined by U,V,W:
C
C            /  1,2   x,y   posn coords                        \
C    source |   3      m    posn weight [opt'l in source]       |
C            \  4,5   h,k   posn indices wrt u,v                |
C               6,7  x0,y0  original lattice site coords        |
C               8,9  dx,dy  displacement cmpts [1,2 minus 6,7]  | output
C               10   theta  local rotation [anticlockwise/rad]  |
C               11    mag   local magnification                 |
C               12   elong  local elongation factor             |
C               13   edirn  local elongation direction          |
C                           [anticlockwise from +X; /rad]       |
C               14     n    serial number 0,1,2.. left->rig    /
C
C The displacement field is deduced by recorded positions (x,y) with
C those generated from the recorded (h,k) and the supplied lattice;
C its derivatives are estimated for all posns with at least two
C independent neighbours, and the resulting displacement tensor
C analysed in terms of the standard parameters noted above.
C
C Positions with insufficient neighbours are omitted from the final
C list, which may therefore be shorter than the original.  The last
C layer may be found useful for keeping track of individual posns
C following subsequent PLSORTing.
C
C Note on coordinate systems:
C 'cart' means cartesian - the original picture coord system;
C 'lr' means lattice-relative - wrt the fitted U,V,W system.

C VD required:
C Planalyse :PLANAL verify >$r3
C
C Global declarations
      INCLUDE 'COMMON'
      INTEGER MAXIPB,MAXRPB
      PARAMETER (MAXIPB=LNBUF/LNINT,MAXRPB=LNBUF/LNREAL)
C
C Local declarations
      LOGICAL SEMOPN,SEMLAB,SEMROW,SEMLU,OPT,SEMCON,FITUIJ
      INTEGER HLAY
C
C NB: Two additional row buffers needed
      REAL RB7(MAXRPB),RB8(MAXRPB)
      REAL UVW(6),DU(8),DV(8),DX(8),DY(8),MAG,K
      INTEGER NUVW(6),LABEL(256),IND(MAXIPB)
      LOGICAL LVERIF
C
      EQUIVALENCE (LABEL,RB1),(RB2,IND)
      EQUIVALENCE (UVW(1),U1),(UVW(2),V1),(UVW(3),W1)
      EQUIVALENCE (UVW(4),U2),(UVW(5),V2),(UVW(6),W2)
C
C Packed names
      PARAMETER (NFROM=10335,NTO=-601,NVERIF=-3419,NN=22400,NR=28800)
C NUVW contains U,V,W,U2,V2,W2
      DATA NUVW/-1601,-3201,-4801,-2881,-4481,-6081/
C
C Initialise
      LVERIF=OPT(NVERIF)
      NCOL=NCOLS(LP1)
      NLAY=MIN(NLAYS(LP1),5)
C
C Check source class
      IF (CLASSN(LP1).NE.NCLPLI) THEN
         ERROR=6
         IDERR=IVALPN(NFROM)
         GOTO 130
      ENDIF
C
C Check source size
      IF (NLAY.LT.4) THEN
         WRITE (RECORD,1) N
1        FORMAT ('No indices found in ',I5)
         IF (SEMCON(RECORD)) RETURN
         ERROR=10
         RETURN
C
C Find layer numbers for indices, allowing for possible absence of weights
      ELSE IF (NLAY.EQ.4) THEN
         HLAY=3
         KLAY=4
      ELSE
         HLAY=4
         KLAY=5
      ENDIF
C
C Open intermediate output as workspace
      LP3=LP1
      IF (SEMOPN(3,0,NCOL,1,13,NCLPLI,NFMFP,LP3)) GOTO 130
C
C Fetch lattice parameters U,V,W
      DO 10 I=1,6
         UVW(I)=VAL(NUVW(I))
   10 CONTINUE
C
C Check U,V non-zero
      R=U1*U1+U2*U2
      IF (R.EQ.0.) THEN
         ERROR=3
         IDERR=NUVW(1)
         GOTO 130
      ENDIF
      R2=V1*V1+V2*V2
      IF (R2.EQ.0.) THEN
         ERROR=3
         IDERR=NUVW(4)
         GOTO 130
      ENDIF
C Check U,V independent
      D=U1*V2-U2*V1
      IF (D*D/R/R2.LT.1E-6) THEN
         ERROR=79
         GOTO 130
      ENDIF
C
C Fetch source data
      IF (SEMROW(1,RB1,NFMFP,1,1,LP1)) GOTO 130
      IF (SEMROW(1,RB2,NFMFP,1,2,LP1)) GOTO 130
      IF (NLAY.GT.4) THEN
         IF (SEMROW(1,RB5,NFMFP,1,3,LP1)) GOTO 130
      ELSE
C 5th layer (weights) absent: generate zeros
         DO 20 I=1,NCOL
            RB5(I)=0.
   20    CONTINUE
      ENDIF
      IF (SEMROW(1,RB3,NFMFP,1,HLAY,LP1)) GOTO 130
      IF (SEMROW(1,RB4,NFMFP,1,KLAY,LP1)) GOTO 130
C
C Copy to workspace
      IF (SEMROW(2,RB1,NFMFP,1,1,LP3)) GOTO 130
      IF (SEMROW(2,RB2,NFMFP,1,2,LP3)) GOTO 130
      IF (SEMROW(2,RB5,NFMFP,1,3,LP3)) GOTO 130
      IF (SEMROW(2,RB3,NFMFP,1,4,LP3)) GOTO 130
      IF (SEMROW(2,RB4,NFMFP,1,5,LP3)) GOTO 130
C
C Generate original posns and save in workspace
      DO 30 I=1,NCOL
         RB5(I)=W1+U1*RB3(I)+V1*RB4(I)
         RB6(I)=W2+U2*RB3(I)+V2*RB4(I)
   30 CONTINUE
      IF (SEMROW(2,RB5,NFMFP,1,6,LP3)) GOTO 130
      IF (SEMROW(2,RB6,NFMFP,1,7,LP3)) GOTO 130
C
C Generate displacements and save in workspace
      DO 40 I=1,NCOL
         RB1(I)=RB1(I)-RB5(I)
         RB2(I)=RB2(I)-RB6(I)
   40 CONTINUE
      IF (SEMROW(2,RB1,NFMFP,1,8,LP3)) GOTO 130
      IF (SEMROW(2,RB2,NFMFP,1,9,LP3)) GOTO 130
C
C RB1,2 now contains displacements, and RB3,4 indices; RB5,6,7,8
C hold the parameters rotn,mag,elong,edirn as they are generated.
C RB5 also holds rejection flags for insufficient neighbours.
C
      NPOSNS=0
C
C Begin loop over posns
      DO 70 I=1,NCOL
C
C Force rejection flag on: it is subsequently overwritten where
C sufficient data exist for strain parameters to be estimated
         RB5(I)=1E6
C Initialise neighbour count
         NNB=0
C
C Fetch posn indices
         H=RB3(I)
         K=RB4(I)
         DX0=RB1(I)
         DY0=RB2(I)
C
C Begin level 2 loop over posns, seeking 8-neighbours in lr coords
C
C    NB: reordering posns by K would make the searching process
C    here much more efficient.  Alternatively, a 2-D indexed list
C    could be maintained.
C    Also: we could usefully include 1,1 type neighbours for
C    hexagonal lattices.
C    Reconsider details later.
C
         DO 50 J=1,NCOL
C
C Neighbour if not self..
            IF (J.EQ.I) GOTO 50
C ..but indices differ by no more than one
            DH=RB3(J)-H
            DK=RB4(J)-K
            IF (ABS(DH).LE.1. .AND. ABS(DK).LE.1.) THEN
C
C Neighbour found: count it
               NNB=NNB+1
C Record its (undistorted) cart posn difference
               DX(NNB)=DH*U1+DK*V1
               DY(NNB)=DH*U2+DK*V2
C Record its cart displacement difference
               DU(NNB)=RB1(J)-DX0
               DV(NNB)=RB2(J)-DY0
C Don't take more than 8 neighbours
               IF (NNB.EQ.8) GOTO 60
            ENDIF
C
C End of level 2 loop
   50    CONTINUE
C
C If too few neighbours, abandon the posn
   60    IF (NNB.LE.1) GOTO 70
C Estimate displacement derivatives with LSQ criterion,
C abandoning if ill-conditioned
         IF (FITUIJ(DX,DY,DU,DV,NNB,UX,UY,VX,VY)) GOTO 70
C Analyse in terms of local rot'n, magn'n, elong and elong dir'n
         CALL PSC2D(UX,UY,VX,VY,THETA,MAG,ELONG,ALPHA,E1,E2)
C Count the posn and save the four parameters
         NPOSNS=NPOSNS+1
         RB5(I)=THETA
         RB6(I)=MAG
         RB7(I)=ELONG
         RB8(I)=ALPHA
C
   70 CONTINUE
C
C Save parameter lists (uncondensed, unsorted)
      IF (SEMROW(2,RB5,NFMFP,1,10,LP3)) GOTO 130
      IF (SEMROW(2,RB6,NFMFP,1,11,LP3)) GOTO 130
      IF (SEMROW(2,RB7,NFMFP,1,12,LP3)) GOTO 130
      IF (SEMROW(2,RB8,NFMFP,1,13,LP3)) GOTO 130
C
C Output phase
C ------------
C Open final output with NPOSNS cols
      LP2=LP1
      IF (SEMOPN(2,IVALPN(NTO),NPOSNS,1,14,NCLPLI,NFMFP,LP2))
     +   GOTO 130
C Record Plist type List
      IF (SEMLAB(1,LABEL,LP2)) GOTO 130
      LABEL(LBPLTY)=1
      IF (SEMLAB(2,LABEL,LP2)) GOTO 130
C
C Initialise plist condensation routine
      CALL PLCOND(1,RB5,NCOL,IND,NCC)
C
C Prepare rms displacement accumulator
      R=0.
C
C Move data from workspace to final output
      DO 100 N=1,14
C For layer 14, generate serials
         IF (N.EQ.14) THEN
            DO 80 I=1,NPOSNS
               RB1(I)=I
   80       CONTINUE
         ELSE
C Otherwise, fetch layer from workspace..
            IF (SEMROW(1,RB1,NFMFP,1,N,LP3)) GOTO 130
C .. and condense
            CALL PLCOND(2,RB1,NCOL,IND,NCC)
C For layers 8,9 accumulate squared displacement cmpts
            IF (N.EQ.8.OR.N.EQ.9) THEN
               DO 90 I=1,NPOSNS
                  R=R+RB1(I)**2
   90          CONTINUE
            ENDIF
         ENDIF
         IF (SEMROW(2,RB1,NFMFP,1,N,LP2)) GOTO 130
  100 CONTINUE
      R=SQRT(R/REAL(NPOSNS))
C
C Return number of positions as variable N, and rms displacement as R
      IF (SEMLU(1,NN,REAL(NPOSNS))) GOTO 130
      IF (SEMLU(1,NR,R)) GOTO 130
C
C Report number of positions where strain estimated
      IF (LVERIF) THEN
         WRITE (RECORD,110) NPOSNS
  110    FORMAT ('Strain parameters estimated at',I5,' positions')
         IF (SEMCON(RECORD)) GOTO 130
C Report rms displacement
         WRITE (RECORD,120) R
  120    FORMAT ('Rms displacement:',F6.2,' pixels')
         IF (SEMCON(RECORD)) GOTO 130
      ENDIF
C
  130 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 local subsidiary module PLCOND
C
      SUBROUTINE PLCOND(OPC,X,N,IND,NI)
C
C Condenses arrays omitting values > 1e5; the two opcodes make it
C convenient to condense several arrays in turn like the first.
C   OPC=1: Fills IND(N) with list of elements of X to be retained,
C          without changing X; returns number retained in NI
C   OPC=2: Applies condensation defined in IND(1-NI) (by initial
C          OPC=1 call) to array X(N)
C Example:
C   CALL PLCOND(1,X,50,IND,NI) to initialise IND from X
C   CALL PLCOND(2,X,50,IND,NI) to condense X
C   CALL PLCOND(2,Y,50,IND,NI) to condense Y
C   ...
C
      INTEGER IND(N),OPC,NI,I
      REAL X(N)
C
C OPC=1: List retained elements
C
      IF (OPC.EQ.1) THEN
         NI=0
         DO 10 I=1,N
            IF (X(I).LE.1E5) THEN
               NI=NI+1
               IND(NI)=I
            ENDIF
   10    CONTINUE
C
C OPC 2: condense list from IND
      ELSE
         DO 20 I=1,NI
            X(I)=X(IND(I))
   20    CONTINUE
      ENDIF
C
      RETURN
C
      END
C Semper 6 local subsidiary module FITUIJ
C
C Fits displacement field derivatives to set of displacement values
C at arbitrary positions around central position
C
      LOGICAL FUNCTION FITUIJ(DX,DY,DU,DV,N,UX,UY,VX,VY)
C
      REAL DX(N),DY(N),DU(N),DV(N)
C
C Routine expects data in the form of a set of du,dv values for incrs
C dx,dy from an initial position at which the displacement derivatives
C are estimated by a least squares fit:
C   |dui|  =  | du/dx  du/dy | |dxi|  =  | ux uy | |dxi|
C   |dvi|     | dv/dx  dv/dy | |dyi|     | vx vy | |dyi|
C Defining a sum square residuals function R = Sumi{dui-ux.dxi-uy.dyi}
C for the du equation, independently of the dv equation, minimisation
C leads to
C   | Sxx  Sxy | |ux| = |Sux|
C   | Sxy  Syy | |uy|   |Suy|  with Sxx denoting Sumi{dxi.dxi} etc
C A similar solution applies to the v derivatives.
C
      FITUIJ=.TRUE.
C
C Initialise accumulators
      SUX=0.
      SUY=0.
      SVX=0.
      SVY=0.
      SXX=0.
      SXY=0.
      SYY=0.
C
C Accumulate sums
      DO 10 I=1,N
         SUX=SUX+DU(I)*DX(I)
         SUY=SUY+DU(I)*DY(I)
         SVX=SVX+DV(I)*DX(I)
         SVY=SVY+DV(I)*DY(I)
         SXX=SXX+DX(I)*DX(I)
         SXY=SXY+DX(I)*DY(I)
         SYY=SYY+DY(I)*DY(I)
   10 CONTINUE
C
C Calculate (unsymmetrised) displacement derivative matrix uij
      D=SXX*SYY-SXY*SXY
      IF (D.EQ.0.) RETURN
      UX=(SUX*SYY-SUY*SXY)/D
      UY=(SXX*SUY-SXY*SUX)/D
      VX=(SVX*SYY-SVY*SXY)/D
      VY=(SXX*SVY-SXY*SVX)/D
C
C Normal return
      FITUIJ=.FALSE.
      RETURN
C
      END
C Semper 6 local subsidiary module PSC2D
C
C Extracts principal strain components for 2-D fields
C 
      SUBROUTINE PSC2D(UX,UY,VX,VY,THETA,MAG,ELONG,ALPHA,E1,E2)
C
C Given derivatives of local displacement (u,v) wrt original posn (x,y),
C returns
C    - theta: rotation angle; anticlockwise
C    -   mag: isotropic magnification factor = root{ (1+e1).(1+e2) }
C    - elong: elongation factor = root{ (1+e1)/(1+e2) }
C    - alpha: dirn of e1, anticl from +X
C    - e1,e2: princ tensile strain components; e1 is the larger (the
C             more positive)
C Original posns (x,y) are converted into final posns (x+u,y+v) by first
C rotating by theta anticlockwise, then magnifying in the alpha dir'n by
C factor mag*elong = 1+e1, and at 90 degrees to it by mag/elong = 1+e2.
C [Interchanging the order of rotation and deformation involves no more
C than subtracting theta from alpha.]
C
C General notes on 2-D strain fields
C ----------------------------------
C Given a revised position function r'(r), ie x'(x,y) and y'(x,y), the
C displacement field is (u,v) with u(x,y)=x'-x and v(x,y)=y'-y
C The transformation of small vectors dr is given by
C   |dx'| =  | dx'/dx  dx'/dy | |dx|   = | 1+du/dx  du/dy | |dx|
C   |dy'|    | dy'/dx  dy'/dy | |dy|     | dv/dx  1+dv/dy | |dy|
C ie
C   dr'i = Dij drj = (1 + uij) drj
C with the normal matrix convention for suffices:
C   | du/dx  du/dy | = | u11  u12 |
C   | dv/dx  dv/dy |   | u21  u22 |
C
C The use of infinitesimals linearises the transformation locally in
C the usual way.  We now decompose Dij into a product RE, with R a
C rotation and E symmetric.  (The decomposition is not unique without
C some such condition on E... pure tensile strain along two principal
C axes is symmetric, and rotating axes leaves a symm matrix symm -
C sufficient?)  To decompose, D = RE => E = R^-1D = |c -s|D, after
C which symmetry of E gives                         |s  c|
C   E21=E12  =>  c.D12 - s.D22 = s.D11 + c.D21
C   => s/c = tan(theta) = (D12-D21)/(D11+D22) = (u12-u21) (2+u11+u22)
C for the local (clockwise) rotation.  [Rotation by angles more than
C pi/2 can be detected from the fact that E11 and E22 must be positive,
C though the present code ignores this possibility.]
C
C E is then obtained as R^-1D, ie
C   E = | c(1+ucc)-su21  cu12-s(1+u22) |
C       | s(1+u11)+cu21  su12+c(1+u11) |
C after which we extract the strain by subtracting a unit matrix:
C   e = E - 1
C We diagonalise this to find the tensile strains and principal axes
C as usual, obtaining
C   e1 = (1/2) [ e11 + e22 + root{ (e11-e22)^2 + 4e12^2}
C   e2 =                   -
C the direction of e1 being alpha anticlockwise from +X, with
C   tan(alpha)=(e1-e11)/e12
C [Alternatively, tan(2.alpha) = 2e12/(e11-e22) can be used, but it
C does not distinguish the two axes.]  The routine interchanges e1,2
C if necessary so that e1 is the more positive.
C
C The maximum shear strain is found by considering e'21 after a
C rotation, and is root{(e11-e22)^2/4+e12^2}  - wrt any axes - and thus
C simply (e1-e2)/2 (by choosing those to be the principal axes).  There
C is not in general a pair of axes wrt which the strain is pure shear:
C since the trace is invariant on rotating, the diagonals could not be
C zero unless the tensile components happened to sum to 0.  The area
C strain is (1+e1)(1+e2)-1 = e1+e2+e1.e2
C
C This routine does not return max shear or area strain however, but
C offers a different factorisation of the distortion: into an isotropic
C magn'n by factor mag and an area-preserving elongation by factor elong
C (magnifying by factor elong along the first princ axis and shrinking
C by same factor along second).  The values of these are obtained from
C e1,e2 by solving
C   1 + e1  =  mag.elong
C   1 + e2  =  mag/elong
C to give mag = root{(1+e1)(1+e2)}
C       elong = root{(1+e1)/(1+e2)}
C
C Note that if D is decomposed as E'R instead, the fact that RE = E'R
C means that E' is simply RER^-1, i.e. E wrt rotated axes, giving the
C same principal axes etc as before.  The routine in fact returns alpha
C with theta subtracted, as if the decomposition HAD been done this way,
C and thus gives the direction of the principal axes directly.
C
C Small displacement approximation: if u<<1, the rotation angle theta
C above becomes simply (u12-u21)/2; E becomes
C    | 1+u11  (u12+u21)/2 |
C    | (u12+u21)/2  1+u22 |
C A simpler derivation is possible in this limit however: D = 1 + U
C is decomposed into (1+A)(1+S) = 1 + A + S to first order, making
C A the antisymm part of U, ie (uij-uji)/2, and S the symmetric part
C (uij+uji)/2.
C
      REAL MAG
      PARAMETER (PIB2=3.1415926536/2.)
C
C Rename displacement components for readability
      U11=UX
      U12=UY
      U21=VX
      U22=VY
C
C Obtain local rotation angle
      THETA = ATAN( (U12-U21) / (2+U11+U22) )
      C = COS(THETA)
      S = SIN(THETA)
C
C Obtain strain tensor E, as R^-1 ( 1 + U ) - 1
      E11 = C*(1+U11)-S*U21 - 1
      E12 = C*U12-S*(1+U22)
      E22 = S*U12+C*(1+U22) - 1
C
C Obtain princ comps
      R = SQRT( (E11-E22)**2 + 4*E12*E12 )
      E1 = ( E11+E22 + R )/2.
      E2 = ( E11+E22 - R )/2.
      IF (E1.LT.E2) THEN
         R=E1
         E1=E2
         E2=R
      ENDIF
      IF (E12.EQ.0.) THEN
         IF (E11.GT.E12) THEN
            ALPHA=0.
         ELSE
            ALPHA=PIB2
         ENDIF
      ELSE
         ALPHA = ATAN ( (E1-E11)/E12 )
      ENDIF
C
C Obtain magnfication and elongation factor
      MAG  =SQRT( (1+E1)*(1+E2) )
      ELONG=SQRT( (1+E1)/(1+E2) )
C
C Offset alpha by theta, and finally reverse sense of theta
      ALPHA=ALPHA-THETA
      THETA=-THETA
      RETURN
C
      END
C Semper 6 processing module PLUVXY
C
      SUBROUTINE PLUVXY
C
C Provides verb PLUVXY.  Expects as source a 14-layer plist as generated
C by PLANALYSE; recovers local rotation/deformation parameters for a single
C posn indicated either via NUMBER (0,1,2.. from beginning of list) or by
C approximate POSITION, and sets U,U2, V,V2 and X,Y so that EXTRACT UV @XY
C recovers a locally unbent region centred at the posn.
C
C VD required:
C Pluvxy :PLUVXY position= po2= number= >select
C
C Global declarations
      INCLUDE 'COMMON'
C
C Local declarations
      LOGICAL SEMROW,SEMCON,SEMLU,VARSET
C
      REAL DFAP(6),UVXY(6),MAGN,L11,L12,L21,L22
      INTEGER PLAY(6),NUVXY(6),ROW,PIX
C
C Packed names
      PARAMETER (NFROM=10335)
      PARAMETER (NPOSIT=26219,NPO2=26232,NNUMBE=23253)
      PARAMETER (NH=12800,NK=17600,NX=-6401,NY=-8001)
      PARAMETER (NU=-1601,NU2=-2881,NV=-3201,NV2=-4481)
C
C Displacement/rotation/deformation pars in order (for pick-up loop)
      EQUIVALENCE (DFAP(1),XC),(DFAP(2),YC),(DFAP(3),THETA)
      EQUIVALENCE (DFAP(4),MAGN),(DFAP(5),ELONG),(DFAP(6),ALPHA)
C Extraction parameters U,U2,V,V2,X,Y in order (for deposit loop)
      EQUIVALENCE (UVXY(1),L11),(UVXY(2),L21),(UVXY(3),L12)
      EQUIVALENCE (UVXY(4),L22),(UVXY(5),XC),(UVXY(6),YC)
C
C Layer numbers containing the pars
      DATA PLAY/1,2,10,11,12,13/
C Names U,U2,V,V2,X,Y
      DATA NUVXY/NU,NU2,NV,NV2,NX,NY/
C
C Initialise
      NPOSNS=NCOLS(LP1)
      NROW1=NROWS(LP1)
      N=IVALPN(NFROM)
C
C Check source class
      IF (CLASSN(LP1).NE.NCLPLI) THEN
         ERROR=6
         IDERR=N
         RETURN
      ENDIF
C
C Check number of layers
      IF (NLAYS(LP1).LT.13) THEN
         WRITE (RECORD,1) N
1        FORMAT ('No rotation/deformation information found in ',I5)
         IF (SEMCON(RECORD)) RETURN
         ERROR=10
         RETURN
      ENDIF
C
C Find posn number, directly or via PLFIND if POSN mode
      IF (VARSET(NNUMBE)) THEN
         H=IVAL(NNUMBE)
         K=0
      ELSE
         CALL PLFIND
         H=IVAL(NH)
         K=IVAL(NK)
      ENDIF
C
C Fetch posn coords, rotation and deformation pars
      ROW=CROWN(LP1)-K
      PIX=CCOLN(LP1)+H
      DO 2 N=1,6
         IF (SEMROW(1,RB1,NFMFP,ROW,PLAY(N),LP1)) RETURN
         DFAP(N)=RB1(PIX)
    2 CONTINUE
C
C Recombine deformation parameters as principal axis magnifications
C Local deformation = magnifying the principal axes by factors E1,E2
C i.e. matrix E = (e1,0)
C                 (0,e2) wrt axes in ALPHA dirn and its normal; this is
C R.E.R^-1 with R a clockwise rotation by ALPHA wrt x,y axes; this is
C preceded by a clockwise rotation of axes by THETA, which combines with
C R^-1 to give a anticlockwise rotation by ALPHA-THETA:
C    L = (cs -sa) (e1 0) ( cat sat)
C        (sa  ca) (0 e2) (-sat cat)
C      = (e1.ca.cat+e2.sa.sat  e1.ca.sat-e2.sa.cat)
C        (e1.sa.cat-e2.ca.sat  e1.sa.sat+e2.ca.cat)
C
      E1=MAGN*ELONG
      E2=MAGN/ELONG
      CA=COS(ALPHA)
      SA=SIN(ALPHA)
      CAT=COS(ALPHA-THETA)
      SAT=SIN(ALPHA-THETA)
      L11 = (E1*CA*CAT+E2*SA*SAT                     )
      L12 = (                     E1*CA*SAT-E2*SA*CAT)
      L21 = (E1*SA*CAT-E2*CA*SAT                     )
      L22 = (                     E1*SA*SAT+E2*CA*CAT)
C
C Set extraction sampling vectors U,V and position X,Y for EXT UV @XY
      DO 9 N=1,6
         IF (SEMLU(1,NUVXY(N),UVXY(N))) GOTO 10
    9 CONTINUE
   10 RETURN
C
      END
C Semper 6 local processing module PLMARK
C
      SUBROUTINE PLMARK
C
C Marks information from a multi-layer Plist on the display in various
C ways:
C - in default, simply marks posns (like MARK, but with NUMBERS key)
C - ROTATION marks lines (half-size MKSIZE) rotated from X axis acc
C   to local rotn; exaggerated by TIMES
C - MAGNIFICATION marks squares (half-size MKSIZE) distorted acc to
C     local magn, elong, elong dirn; magn,elong exaggerated by TIMES
C - SERIAL marks serial numbers bot/left justified to posns
C - WEIGHT marks in style MKMODE, with half-size acc to weights;
C     actual range (or MIN,MAX if PRESET) is scaled to 1-MKSIZE
C - DVECTOR marks displacements as arrows from original posn towards
C     actual; exaggerated by TIMES
C - ORIGINAL marks in style MKMODE,MKSIZE the original posns
C Except for the last two cases, marks are placed at the actual rather
C then original posn; default 5 applied to MKSIZE if unset.
C
C VD required:
C Plmark :PLMARK >$fpp rotation magnification serial weight dvector +
C   original preset with=sel times=5 numbers= nu2=
C
C Global declarations
      INCLUDE 'COMMON'
C
C Local declarations
      LOGICAL SEMOPN,SEMCON,OPT,VARSET,SEMROW,SEMXA1
      LOGICAL FSOPTN,FSINIT,FSVIEW,FSARRO,FSLINE,FSTEXT,FSMARK,FSLIST
      REAL MAG
      INTEGER TEXT(5),OPC,CLASS,FORM
C
C Packed names
C
      PARAMETER (NVIEW=-3566,NNUMBE=23253,NNU2=23272,NDVECT=7285)
      PARAMETER (NROTAT=29420,NMAGNI=20847,NSERIA=30618,NWEIGH=-5010)
      PARAMETER (NPRESE=26325,NTIMES=-374,NWITH=-5181,NORIGI=24729)
C
C Open plist
      IDERR=IVALPN(NWITH)
      IF (SEMOPN(1,IDERR,NCOL,NROW,NLAY,CLASS,FORM,LP1)) RETURN
      IF (CLASS.NE.NCLPLI) THEN
         ERROR=6
         RETURN
      ENDIF
      TIMES=VAL(NTIMES)
C
C Load actual x,y values
      IF (SEMROW(1,RB1,NFMFP,1,1,LP1)) RETURN
      IF (SEMROW(1,RB2,NFMFP,1,2,LP1)) RETURN
C
C Fetch posn number range
      IF (VARSET(NNUMBE)) THEN
         I1=MAX(IVAL(NNUMBE),0)
      ELSE
         I1=1
      ENDIF
      IF (VARSET(NNU2)) THEN
         I2=MIN(IVAL(NNU2),NCOL)
      ELSE
         I2=NCOL
      ENDIF
      IF (I2.LT.I1) THEN
         ERROR=3
         IDERR=NNUMBE
         RETURN
      ENDIF
      NPOSNS=I2-I1+1
C
C Initialise graphics coordinate mode
      IF (FSOPTN(OPC,N)) RETURN
      IF (FSINIT(OPC,N)) RETURN
C Provide default MKSIZE 5
      IF (FSMSIZ.EQ.0) FSMSIZ=5
C Note mark size in graphics coordinates
      SIZE=FSMSIZ/FSXSCA
C Update current partition number
      IF (OPC.NE.1) DISPLA=REAL(1000*FSDEV+FSPAR)
C
C If option VIEW is set, switch view to area of interest
      IF (FSVIEW(OPT(NVIEW))) RETURN
C
C Code for ROTATION case
C -----------------
      IF (.NOT.OPT(NROTAT)) GOTO 20
      IF (SEMROW(1,RB3,NFMFP,1,10,LP1)) RETURN
      DO 10 I=I1,I2
         DX=SIZE*COS(RB3(I)*TIMES)
         DY=SIZE*SIN(RB3(I)*TIMES)
         X=RB1(I)
         Y=RB2(I)
         IF (FSLINE(X-DX,Y-DY,X+DX,Y+DY)) RETURN
   10 CONTINUE
      RETURN
C
C Code for MAGNIFICATIONS case
C -----------------------
   20 IF (.NOT.OPT(NMAGNI)) GOTO 40
      IF (SEMROW(1,RB3,NFMFP,1,11,LP1)) RETURN
      IF (SEMROW(1,RB4,NFMFP,1,12,LP1)) RETURN
      IF (SEMROW(1,RB5,NFMFP,1,13,LP1)) RETURN
      DO 30 I=I1,I2
         X=RB1(I)
         Y=RB2(I)
         DX=SIZE*COS(RB5(I))
         DY=SIZE*SIN(RB5(I))
         MAG  =1.+(RB3(I)-1.)*TIMES
         ELONG=1.+(RB4(I)-1.)*TIMES
         DX2=-DY*MAG/ELONG
         DY2=DX*MAG/ELONG
         DX=DX*MAG*ELONG
         DY=DY*MAG*ELONG         
         IF (FSLINE(X+DX+DX2,Y+DY+DY2,X+DX-DX2,Y+DY-DY2)) RETURN
         IF (FSLINE(X+DX-DX2,Y+DY-DY2,X-DX-DX2,Y-DY-DY2)) RETURN
         IF (FSLINE(X-DX-DX2,Y-DY-DY2,X-DX+DX2,Y-DY+DY2)) RETURN
         IF (FSLINE(X-DX+DX2,Y-DY+DY2,X+DX+DX2,Y+DY+DY2)) RETURN
   30 CONTINUE
      RETURN
C
C Code for SERIALS case
C ----------------
   40 IF (.NOT.OPT(NSERIA)) GOTO 60
      IF (SEMROW(1,RB3,NFMFP,1,14,LP1)) RETURN
      DO 50 I=I1,I2
         N=1
         IF (SEMXA1(4,TEXT,5,N,RB3(I),IV)) RETURN
         IF (FSTEXT(TEXT,N-1,RB1(I),RB2(I),-1,-1)) RETURN
   50 CONTINUE
      RETURN
C
C Code for WEIGHTS case
C ----------------
   60 IF (.NOT.OPT(NWEIGH)) GOTO 90
      IF (SEMROW(1,RB3,NFMFP,1,3,LP1)) RETURN
      IF (OPT(NPRESE)) THEN
         B=VMIN
         T=VMAX
      ELSE
C Find weight range
         B=RB3(I1)
         T=B
         DO 70 I=I1,I2
            IF (RB3(I).GT.T) THEN
               T=RB3(I)
            ELSE IF (RB3(I).LT.B) THEN
               B=RB3(I)
            ENDIF
   70    CONTINUE
      ENDIF
C Scale into range 1-MKSIZE and mark
      IF (T.NE.B) SCF=(FSMSIZ-1)/(T-B)
      DO 80 I=I1,I2
         S=0.
         IF (T.NE.B) S=(MAX(MIN(RB3(I),T),B)-B)*SCF+1.
         IF (FSMARK(RB1(I),RB2(I),FSMMOD,NINT(S))) RETURN
   80 CONTINUE
      RETURN
C
C Code for ORIGINALS case
C ------------------
   90 IF (.NOT.OPT(NORIGI)) GOTO 100
C Fetch original posns
      IF (SEMROW(1,RB3,NFMFP,1,6,LP1)) RETURN
      IF (SEMROW(1,RB4,NFMFP,1,7,LP1)) RETURN
      IF (FSLIST(RB3(I1),RB4(I1),NPOSNS,FSMMOD,FSMSIZ)) RETURN
      RETURN
C
C Code for DVECTORS case
C -----------------
  100 IF (.NOT.OPT(NDVECT)) GOTO 120
C Fetch original posns
      IF (SEMROW(1,RB3,NFMFP,1,6,LP1)) RETURN
      IF (SEMROW(1,RB4,NFMFP,1,7,LP1)) RETURN
      DO 110 I=I1,I2
         IF (FSARRO(RB3(I),RB4(I),
     +      RB3(I)+TIMES*(RB1(I)-RB3(I)),RB4(I)+TIMES*(RB2(I)-RB4(I))))
     +      RETURN
  110 CONTINUE
      RETURN
C
C Code for default case
  120 IF (FSLIST(RB1(I1),RB2(I1),NPOSNS,FSMMOD,FSMSIZ)) RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 local processing module PLSORT
C
      SUBROUTINE PLSORT
C
C Sorts position lists into ASCENDing or DESCENDing order by any one
C indicated layer or by subsidiary picture.  If WITH is set, its first
C layer is used as the sorting basis.  Otherwise a given LAYER of the
C source is used, indicated either directly or indirectly via @ROTN etc 
C
C VD required:
C Plsort :PLSORT ascending descending layer=2 >$ft
C
C Global declarations
      INCLUDE 'COMMON'
      PARAMETER (MAXRPB=LNBUF/LNREAL)
C
C Local declarations
      LOGICAL SEMOPN,SEMROW,CONOPT,OPT,VARSET,ASCEND
      INTEGER P(MAXRPB),CLASS,FORM
C
C Packed names
      PARAMETER (NFROM=10335,NWITH=-5181,NLAYER=19265)
      PARAMETER (NASCEN=2363,NDESCE=6619)
C
      EQUIVALENCE (RB2,P)
C
C Initialise and check class
      NCOL=NCOLS(LP1)
      IF (CLASSN(LP1).NE.NCLPLI) THEN
         ERROR=6
         IDERR=IVAL(NFROM)
      ENDIF
C
C Establish layer to be sorted
      LAYER=IVAL(NLAYER)
C
C Establish sense of sorting
      IF (CONOPT(NASCEN,NDESCE)) RETURN
      ASCEND=.NOT.OPT(NDESCE)
C
C Subsidiary picture needed?
      LP3=LP1
      IDERR=IVALPN(NWITH)
      IF (VARSET(NWITH)) THEN
         IF (SEMOPN(1,IDERR,N,NROW,LAYER,CLASS,FORM,LP3)) RETURN
         LAYER=1
         IF (N.NE.NCOL) THEN
            ERROR=5
            RETURN
         ENDIF
      ENDIF
C
C Load layer to be sorted and initialise pointer array
      IF (SEMROW(1,RB1,NFMFP,1,LAYER,LP3)) RETURN
      CALL SHSORT(1,RB1,RB6,P,NCOL,ASCEND)
C
C Pass through all layers, reordering acc to P
      DO 10 LAYER=1,NLAYS(LP1)
         IF (SEMROW(1,RB1,NFMFP,1,LAYER,LP1)) RETURN
         CALL SHSORT(2,RB1,RB6,P,NCOL,ASCEND)
         IF (SEMROW(2,RB6,NFMFP,1,LAYER,LP2)) RETURN
   10 CONTINUE
      RETURN
C
      END
C Semper 6 local subsidiary module SHSORT
C
      SUBROUTINE SHSORT(OPC,V,W,P,N,ASCEND)
C
C 1-D Shell sorting routine, in ascending or descending order:
C   opc=1 Initialise pointer array P(N) so as to order V(P(i))
C       2 Reorder V(N) to W(N) as indicated by P
C [Sorting with a pointer array as here makes workspace necessary,
C but makes it convenient to sort several arrays together by calling
C opc 1 once and opc 2 several times on different arrays in turn.]
C NB: W must not be equivalent to V
C
      REAL V(N),W(N),V1,V2
      INTEGER P(N),OPC
      LOGICAL ASCEND
C
C Establish mode
      IF (OPC.EQ.2) GOTO 60
C
C Determine number of passes required
C
      NPASS=0
      M=1
   10 IF (M.LT.N) THEN
         NPASS=NPASS+1
         M=M+M+1
         GOTO 10
      ENDIF
C
C Opcode 1: set P so as to point to ordered V
C --------
C Initialise pointers
      DO 20 I=1,N
         P(I)=I
   20 CONTINUE
C
C Make required number of passes through data
C
      DO 50 IPASS=1,NPASS
C
C Determine next increment
         M=M/2
C
C Sort data with next increment
         DO 40 I=1,N-M
            DO 30 J=I,1,-M
               K=J+M
               IF (ASCEND) THEN
                  IF (V(P(J)).LE.V(P(K))) GOTO 40
               ELSE
                  IF (V(P(K)).LE.V(P(J))) GOTO 40
               ENDIF
C Swap pointers
               JK=P(J)
               P(J)=P(K)
               P(K)=JK
   30       CONTINUE
   40    CONTINUE
   50 CONTINUE
      RETURN
C
C Opcode 2: Reorder V to W acc to P
C --------
   60 X=V(1)
      DO 70 I=1,N
         W(I)=V(P(I))
   70 CONTINUE
      RETURN
C
      END

C Semper 6 local processing module PLDEL
C
      SUBROUTINE PLDEL
C
C Provides verb PLDELETE which deletes one or more posns from a Plist,
C with corresponding adjustments to all layers.
C In default, deletes posn nearest POSN,PO2 (eg XWI; PLDEL @XY)
C If RADIUS is set, posns inside / OUTSIDE circle RADIUS from POSN,PO2
C    (eg XWI CIRCLE; PLDEL @CIRCLE)
C If SIZE is set, posns inside / OUTSIDE region SIZE,SI2 at POSN,PO2
C    (eg XWI REGION; PLDEL @REGION)
C If WITH is set to a closed plist number, deletes posns inside /OUTSIDE
C    the polygon (eg XWI CLOSED CURVE TO 2; PLDEL 1 WITH 2)
C If IF or UNLESS is set to the text of a condition expression, deletes
C    posns meeting the condition - with several variables set to posn
C    parameters at time of evaluation, as described in PLDEL2
C
C VD required:
C Pldelete :PLDEL inside outside verify >$rp >$size with= +
C   if=' unless=' >$r3
C
C Global declarations
      INCLUDE 'COMMON'
      PARAMETER (MAXRPB=LNBUF/LNREAL)
C
C Local declarations
      LOGICAL SEMOPN,SEMROW,SEMLAB,SEMCON,SEMLU,VARSET,OPT,OPTNO
      LOGICAL INPLYG,INSIDE
      INTEGER LABEL(256),IND(MAXRPB),PTR,CLASS,FORM
      EQUIVALENCE (RB1,LABEL),(RB6,IND)
C
C Packed names
      PARAMETER (NFROM=10335,NTO=-601,NWITH=-5181,NOUTSI=24860)
      PARAMETER (NRADIU=28844,NPOSIT=26219,NPO2=26232)
      PARAMETER (NSIZE=30786,NSI2=30792,NN=22400,NVERIF=-3419)
      PARAMETER (NIF=14640, NUNLES=-2173)
C
C Initialise
      NCOL=NCOLS(LP1)
      NLAY=NLAYS(LP1)
C
C Check source class
      IF (CLASSN(LP1).NE.NCLPLI) THEN
         ERROR=6
         IDERR=IVALPN(NFROM)
         RETURN
      ENDIF
C
C Switch to PLDEL2 instead if IF or UNLESS is set
      IF (OPT(NIF).OR.OPT(NUNLES)) THEN
         CALL PLDEL2
         GOTO 60
      ENDIF
C
C Fetch position, inside/outside mode
      X=VAL(NPOSIT)
      Y=VAL(NPO2)
      INSIDE=.NOT.OPT(NOUTSI)
C
C Fetch X,Y coords
      IF (SEMROW(1,RB5,NFMFP,1,1,LP1)) RETURN
      IF (SEMROW(1,RB6,NFMFP,1,2,LP1)) RETURN
C
C Code for circular region
C ------------------------
      IF (.NOT.VARSET(NRADIU)) GOTO 20
      RR=VAL(NRADIU)**2
      DO 10 I=1,NCOL
         T=(RB5(I)-X)**2+(RB6(I)-Y)**2
C Mark for deletion if within R of X,Y and INSIDE - or vice versa
         IF (T.LE.RR.EQV.INSIDE) THEN
            RB5(I)=1E6
         ENDIF
   10 CONTINUE
C Output the revised list
      GOTO 60
C
C Code for rectangular region
C ---------------------------
   20 IF (.NOT.VARSET(NSIZE)) GOTO 40
C Find size
      NC=IVAL(NSIZE)
      NR=IVAL(NSI2)
      IF (NR.EQ.0) NR=NC
C Find x,y ranges inside region (with usual rounding)
      X1=X-NC/2
      X2=X+(NC-1)/2
      Y1=Y-(NR-1)/2
      Y2=Y+NR/2
      DO 30 I=1,NCOL
C Mark for deletion if within range and INSIDE - or vice versa
         IF (RB5(I).GE.X1.AND.RB5(I).LE.X2 .AND.
     +       RB6(I).GE.Y1.AND.RB6(I).LE.Y2 .EQV. INSIDE) THEN
            RB5(I)=1E6
         ENDIF
   30 CONTINUE
C Output the revised list
      GOTO 60
C
C Code for polygon region
C -----------------------
   40 IF (.NOT.VARSET(NWITH)) GOTO 45
C Fetch and check plist
      N=IVALPN(NWITH)
      IF (SEMOPN(1,N,NC,NR,NL,CLASS,FORM,LP3)) RETURN
      IF (CLASS.NE.NCLPLI) THEN
         ERROR=6
         IDERR=N
         RETURN
      ENDIF
C Fetch polygon vertices
      IF (SEMROW(1,RB1,NFMFP,1,1,LP3)) RETURN
      IF (SEMROW(1,RB2,NFMFP,1,2,LP3)) RETURN
C Ensure last point repeats first
      RB1(NC+1)=RB1(1)
      RB2(NC+1)=RB2(1)
C Pass through posns listed, testing each against polygon
      DO 48 I=1,NCOL
         IF (INPLYG(RB5(I),RB6(I),RB1,RB2,NC).EQV.INSIDE) THEN
            RB5(I)=1E6
         ENDIF
   48 CONTINUE
C Output the revised list
      GOTO 60

C Code for default case: posn nearest X,Y only
C ---------------------
C Find posn nearest X,Y
   45 R=1E12
      PTR=0
      DO 50 I=1,NCOL
         T=(RB5(I)-X)**2+(RB6(I)-Y)**2
         IF (T.LT.R) THEN
            R=T
            PTR=I
         ENDIF
   50 CONTINUE
C Mark it for deletion
      RB5(PTR)=1E6
C
C Output phase (for PLDEL2 too)
C ------------
C Initialise plist condensation routine
   60 CALL PLCOND(1,RB5,NCOL,IND,NCC)
C
C Open final output with NCC cols
      LP2=LP1
      IF (SEMOPN(2,IVALPN(NTO),NCC,1,NLAY,NCLPLI,NFMFP,LP2)) RETURN
C Record Plist type List
      IF (SEMLAB(1,LABEL,LP2)) RETURN
      LABEL(LBPLTY)=1
      IF (SEMLAB(2,LABEL,LP2)) RETURN
C
C Copy data from source to output, condensing en passant
      DO 70 N=1,NLAY
         IF (SEMROW(1,RB2,NFMFP,1,N,LP1)) RETURN
         CALL PLCOND(2,RB2,NCOL,IND,NCC)
         IF (SEMROW(2,RB2,NFMFP,1,N,LP2)) RETURN
   70 CONTINUE
C
C Return final number of posns as N
      IF (SEMLU(1,NN,REAL(NCC))) RETURN
C
C Report number on console
      IF (.NOT.OPTNO(NVERIF)) THEN
         WRITE (RECORD,90) NCC
   90    FORMAT ('Number of positions retained: ',I5)
         IF (SEMCON(RECORD)) RETURN
      ENDIF
C
      RETURN
C
      END
C Semper 6 local subsidiary module INPLYG
C
      LOGICAL FUNCTION INPLYG(X,Y,BX,BY,N)
C
C Returns .TRUE. iff supplied posn X,Y lies inside supplied polygon
C
C BX(0:N),BY(0:N) supply vertex coords for N line segments making up
C polygon boundary; first vertex must be duplicated at end.
C
C Method used is to scan all polygon boundary segments, counting
C intersections with horizontal line from left to x,y: target point is
C inside if this number is odd.  Care is needed if any boundary vertices
C are at target y:
C - if both ends are on line, ignore
C - if only start is on line, ignore
C - if only end is on line, count if [next] segment has same dirn of y
C   variation; [next] means next taken circularly roun poly, ignoring
C   horizontal segments
C In all cases, x,y lying on boundary/intersection is trapped and
C treated as IN.
C
      REAL BX(0:N),BY(0:N)
C
C Initialise intersection count
      NX=0
      XIMAX=-1E6
C
C Loop over boundary curve segments
      DO 30 NS=0,N-1
         X1=BX(NS)
         Y1=BY(NS)
         NS2=MOD(NS+1,N)
         X2=BX(NS2)
         Y2=BY(NS2)
C
C Ignore at once if seg completely above or below line
         T=(Y-Y1)*(Y-Y2)
C Both ends together above or below line: ignore
         IF (T.GT.0.) GOTO 30
C One end above, one end below: simple intersection
         IF (T.LT.0.) GOTO 20
C
C Both ends are on line:
         IF (Y.EQ.Y1.AND.Y.EQ.Y2) THEN
C - trap x on line
            XP1=MIN(X1,X2)
            XP2=MAX(X1,X2)
            IF (X.GE.XP1.AND.X.LE.XP2) THEN
               GOTO 40
            ELSE
C - else, ignore
               GOTO 30
            ENDIF
         ENDIF
C
C Only start is on line: ignore
         IF (Y2.NE.Y) GOTO 30
C
C Only end is on line:
C - trap intersection at target X
         IF (X2.EQ.X) GOTO 40
C - else ignore intersection if [next] seg reverses y dirn
         NXS=NS
   10    NXS=MOD(NXS+1,N)
         NXS2=MOD(NXS+1,N)
         DY=BY(NXS2)-BY(NXS)
C - skip horizontal sections
         IF (DY.EQ.0.) THEN
            GOTO 10
         ELSE IF (DY*(Y2-Y1).LT.0.) THEN
            GOTO 30
         ENDIF
C
C Find and count intersection
   20    XI=X1+(Y-Y1)/(Y2-Y1)*(X2-X1)
         IF (XI.LT.X) THEN
            NX=NX+1
         ELSE IF (XI.EQ.X) THEN
            GOTO 40
         ENDIF
C
   30 CONTINUE
C
C Return result based on count
      INPLYG=NX/2*2.NE.NX
      RETURN
C
C Return .TRUE.
   40 INPLYG=.TRUE.
      RETURN
C
      END
C Semper 6 local processing module PLDEL2
C
      SUBROUTINE PLDEL2
C
C Deletes posns from a multi-layer plist that meet a condition specified
C in an IF or UNLESS expression.  In the expression, the following
C variable names are treated specially:
C   WEIGHT posn weight
C   ROT  local rotation factor for a posn
C   MAG  local magnification factor..
C   ELONG  local elong factor..
C   EDIRN  local elong direction..
C   DX displacement x comp't..
C   DY displacement y comp't..
C
C Number of parameters with special names as above; to add more, increase
C declaration here, and extend DATA statements for PNAME and PLAY
      PARAMETER (NPARS=7)
C
C Global declarations
      INCLUDE 'COMMON'
      PARAMETER (MAXIPB=LNBUF/LNINT)
      PARAMETER (MAXRPB=LNBUF/LNREAL)
C
C Local declarations
C
C Number of processing blocks, ie.g. number of parameter sets
C fitting 4 row buffers
      PARAMETER (NBLKS=4*(LNBUF/(NPARS*LNREAL)))
C
      LOGICAL SEMROW,VARSET,OPT,SEMLU,SEMEXP,SEMCON
      LOGICAL LIF
      REAL PVAL(NPARS),PBUF(NPARS,NBLKS)
      INTEGER PNAME(NPARS),PLAY(NPARS),CPTR,PTR
C
C Packed names
      PARAMETER (NFROM=10335,NIF=14640, NUNLES=-2173)
      PARAMETER (NROTN=29420,NMAGN=20847,NELONG=8495,NEDIRN=8169)
      PARAMETER (NWEIGH=-5010,NDVX=7304,NDVY=7305)
C
      EQUIVALENCE (RB1,PBUF)
C
C Posn parameter names in order
      DATA PNAME / NWEIGH,NDVX,NDVY,NROTN,NMAGN,NELONG,NEDIRN /
C Parameter storage layer numbers in order
      DATA PLAY / 3,8,9,10,11,12,13 /
C
C Initialise
      NCOL=NCOLS(LP1)
C
C Check number of layers
      IF (NLAYS(LP1).LT.13) THEN
         WRITE (RECORD,10) IVALPN(NFROM)
   10    FORMAT ('No rotation/deformation information found in ',I5)
         IF (SEMCON(RECORD)) RETURN
         ERROR=10
         RETURN
      ENDIF
C
C Back up current states of parameter variables
      DO 20 K=1,NPARS
         IF (SEMLU(3,PNAME(K),VALUE)) RETURN
   20 CONTINUE
C
C Fetch copy of first layer to RB5, for use as rejection flags
      IF (SEMROW(1,RB5,NFMFP,1,1,LP1)) RETURN
C
C Find whether IF or UNLESS, and establish pointer to expression
      LIF=VARSET(NIF)
      IF (LIF) THEN
         CPTR=IVAL(NIF)
      ELSE
         CPTR=IVAL(NUNLES)
      ENDIF
      IF (CPTR.EQ.0) THEN
         WRITE (RECORD,3)
3        FORMAT ('Neither IF nor UNLESS specified')
         ERROR=10
         RETURN
      ENDIF
C
C Processing phase: evaluate conditions in blocks
C ----------------  to reduce disc access
C
C Begin outermost loop, over posn blocks
      DO 70 J=1,NCOL,NBLKS
C
C Find range of posns in this block
         N1=J
         N2=MIN(J+NBLKS-1,NCOL)
C
C Loop over pars, fetching those for this block to local buffer
         DO 40 K=1,NPARS
            IF (SEMROW(1,RB6,NFMFP,1,PLAY(K),LP1)) RETURN
            N=0
            DO 30 I=N1,N2
               N=N+1
               PBUF(K,N)=RB6(I)
   30       CONTINUE
   40    CONTINUE
C
C Begin loop over posns in block, testing each for acceptance
         N=0
         DO 60 I=N1,N2
            N=N+1
C
C Set Semper variables to pars for this posn
            DO 50 K=1,NPARS
               IF (SEMLU(1,PNAME(K),PBUF(K,N))) RETURN
   50       CONTINUE
C Use SEMEXP to evaluate condition
            PTR=CPTR
            IF (SEMEXP(LINBUF,LINLEN,PTR,VALUE,.FALSE.)) RETURN
C If condition not met, mark particle for rejection
            IF (LIF.EQV.VALUE.NE.0.) RB5(I)=1E6
C
   60    CONTINUE
C End loop over posns within block
   70 CONTINUE
C End outermost loop over blocks
C
C Return to PLDEL, which condenses and outputs
      RETURN
C
      END
C Semper 6 local processing module PLFIND
C
      SUBROUTINE PLFIND
C
C Provides verb PLFIND: locates posn nearest to POS,PO2
C and returns via Semper variables
C   X,Y the actual recorded posn
C   H,K the picture coordinates for posn
C All rows of plist are examined.
C
C Global declarations
      INCLUDE 'COMMON'
C
C Local declarations
      LOGICAL SEMROW,SEMLU
C
C Packed names
      PARAMETER (NFROM=10335,NPOSIT=26219,NPO2=26232)
      PARAMETER (NX=-6401,NY=-8001,NH=12800,NK=17600)
C
C VD required:
C Plfind :PLFIND position= po2= >select
C
C Initialisation
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      X=VAL(NPOSIT)
      Y=VAL(NPO2)
      RR=1E12
C
C Begin loop over plist rows
      DO 1 J=1,NROW
      IF (SEMROW(1,RB1,NFMFP,J,1,LP1)) RETURN
      IF (SEMROW(1,RB2,NFMFP,J,2,LP1)) RETURN
C
C Search posns within row
         DO 2 I=1,NCOL
            XI=RB1(I)
            YI=RB2(I)
            T=(XI-X)**2 + (YI-Y)**2
            IF (T.LT.RR) THEN
               RR=T
               XN=XI
               YN=YI
               IN=I
               JN=J
            ENDIF
    2   CONTINUE
C
C End loop over rows
    1 CONTINUE
C
C Return actual posn..
      IF (SEMLU(1,NX,XN)) RETURN
      IF (SEMLU(1,NY,YN)) RETURN
C ..and picture coords of posn
      IF (SEMLU(1,NH,REAL(IN-CCOLN(LP1)))) RETURN
      IF (SEMLU(1,NK,REAL(CROWN(LP1)-JN))) RETURN
      RETURN
C
      END
