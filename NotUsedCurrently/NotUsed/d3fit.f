C Semper 6 local module D3FIT
C
      SUBROUTINE D3FIT
C
C Tabulates XCC between model CTF^2 and supplied DP, as function of
C defocus values in (internally located) mirror axis dirns; uses
C free-standing submodules operating on memory only for speed
C
C NB: Contains relatively large local array declarations
C
C Verb descriptor:
C  DPfit :D3FIT krange=.5 kr2=4 within=300 of= of2= of3= +
C    under over >Select $2=0 to=$2 size=21 si2= verify mark=
C
C Global declarations
      INCLUDE 'COMMON'
C
C Local declarations
      LOGICAL D2FIT2
      LOGICAL SEMOPN,SEMLAY,SEMLU,VARSET,SEMTFC,SEMCON,OPT,CONOPT
      LOGICAL MARSET,FSINIT,FSCIRC,ANNOT
      LOGICAL HALF
      REAL VAL
      REAL R1,R2,AMIRR,KMIN,KMAX,D1B,D1T,D2B,D2T,DD,DK,CS,V,LAM,S
      REAL C1,A1,A12,A,APHI,AM,CAM,COS45,EFL,X
      PARAMETER (COS45=.707107)
      INTEGER IVAL,IVALPN
      INTEGER NCOL,NROW,NLAY,CLASS,NCOLO,NROWO,MARK
      INTEGER*4 N4
C Max # defocus samples
      INTEGER NXM
      PARAMETER (NXM=101)
      REAL XC(NXM,NXM)
C Buffer DP is used as 2-D array, holding components of specimen wave
      INTEGER*4 MAXF
      PARAMETER (MAXF=80000)
      REAL DP(MAXF)
C Workspace for mirror axis location
      INTEGER NW,NWP2
      PARAMETER (NW=1024,NWP2=NW+2)
      REAL SEC(NWP2),OCF(NW)
C
C Packed names
      INTEGER NFROM,NTO,NSIZE,NSI2,NKRANG,NKR2
      INTEGER NOF,NOF2,NOF3,NUNDER,NOVER,NWITHI
      INTEGER NSTEP,NCS,NKV,NVERIF,NC1,NA1,NA12,NT,NEFLAG
      INTEGER NPHYSI,NIPS
      PARAMETER (NFROM=10335,NTO=-601,NSIZE=30786,NSI2=30792)
      PARAMETER (NKRANG=18321,NKR2=18352)
      PARAMETER (NOF=24240,NOF2=24272,NOF3=24273)
      PARAMETER (NUNDER=-2165,NOVER=24885,NWITHI=-5181)
      PARAMETER (NSTEP=31205,NCS=5560,NKV=18480)
      PARAMETER (NVERIF=-3419,NC1=6040,NA1=2840,NA12=2872)
      PARAMETER (NT=-1,NEFLAG=8252)
      PARAMETER (NPHYSI=25945,NIPS=15059)
C
C Fetch most parameters
      IDERR=NKRANG
      KMIN=MAX(VAL(NKRANG),0.)
      KMAX=MIN(VAL(NKR2),20.)
      IF (KMAX.LE.KMIN) GOTO 20
      IDERR=NSTEP
      S=VAL(NSTEP)
      IF (S.LE.0) GOTO 20
      IDERR=NCS
      CS=VAL(NCS)*1E6
      IF (CS.LE.0) GOTO 20
      IDERR=NKV
      V=VAL(NKV)*1E3
      IF (V.LE.0) GOTO 20
C
C Wavelength/nm
      X=V*1.60206E-4/9.1083/2.99763**2
      LAM=6.62517/SQRT(2*1.60206*9.1083*V*(1+.5*X))
C
C Source picture initialisation and loading
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      NLAY=NLAYS(LP1)
      CLASS=CLASSN(LP1)
C
C Fault over-sized or wrong-shaped source
C (latter only 'cos single DK used)
      IDERR=IVALPN(NFROM)
      N4=NCOL
      IF (N4*NROW.GT.MAXF.OR.NCOL.NE.NROW/2+1) THEN
         ERROR=5
         RETURN
      ENDIF
C
C Fault multi-layer source
      IF (NLAY.NE.1) THEN
         ERROR=62
         IDERR=VERB
         RETURN
      ENDIF
C
C Insist source is Spectrum or Fourier, and half-plane
      IF (CLASS.NE.NCLSPE.AND.CLASS.NE.NCLFOU) THEN
         ERROR=6
         RETURN
      ENDIF
      IF (SEMTFC(LP1,HALF)) RETURN
      IF (.NOT.HALF) THEN
         ERROR=63
         RETURN
      ENDIF
C
C Establish table dimensions
      NCOLO=IVAL(NSIZE)
      NROWO=NCOLO
      IF (VARSET(NSI2)) NROWO=IVAL(NSI2)
      IF (NCOLO.LT.3.OR.NCOLO.GT.NXM
     +   .OR.NROWO.LT.3.OR.NROWO.GT.NXM) THEN
         ERROR=5
      RETURN
      ENDIF
C
C Output picture initialisation - if TO set
      IDERR=IVALPN(NTO)
      IF (IDERR.NE.0) THEN
         LP2=LP1
         IF (SEMOPN(2,IDERR,NCOLO,NROWO,1,NCLCOR,NFMFP,LP2))
     +   RETURN
      ENDIF
C
C Load source pic to array DP
C [Does anything trap overflow..?]
      IF (SEMLAY(1,DP,NCOL,NFMFP,1,LP1)) RETURN
C
C Truncate KMAX at picture boundary
      DK=1./NROW/S
      KMAX=MIN(KMAX,(NCOL-1.1)*DK)
C
C Annotate display?
      IF (MARSET(ANNOT,MARK)) RETURN
      IF (ANNOT) THEN
         IF (FSINIT(3,MARK)) RETURN
         IF (FSPTYP.NE.1) RETURN
C Reset clipping limits to picture border
         FSXMIN=FSBLEF
         FSXMAX=FSBRIG
         FSYMIN=FSBBOT
         FSYMAX=FSBTOP
C Mark circles at KMIN,MKAX
         IF (FSCIRC(0.,0.,KMIN/DK)) RETURN
         IF (FSCIRC(0.,0.,KMAX/DK)) RETURN
      ENDIF
C
C Find mirror axis (the one nearer +X)
      R1=KMIN/DK
      R2=KMAX/DK
      IF (R1.GT.R2) THEN
         IDERR=NKRANG
         GOTO 20
      ENDIF
      CALL FMIRRD(DP,NCOL,NCOL,NROW,AMIRR,R1,R2,SEC,OCF,NW)
C
C Establish central focus for search in each direction
      D1B=VAL(NOF)
      D2B=VAL(NOF2)
C If three values provided for OF, interpret as C1,A1[2]
      IF (VARSET(NOF3)) THEN
         C1=D1B
         A1=D2B
         A12=VAL(NOF3)
         A=SQRT(A1*A1+A12*A12)
C Mirror may indicate A1 dirn or its normal; D1 is C1+|A| if the
C former, but C1-|A| if the latter
         IF (A.NE.0.) THEN
            APHI=.5*ATAN2(A12,A1)
            CAM=COS(AMIRR)*COS(APHI)+SIN(AMIRR)*SIN(APHI)
            IF (ABS(CAM).LT.COS45) A=-A
         ENDIF
         D1B=C1+A
         D2B=C1-A
      ENDIF
C
C Set search range to WITHIN around D1B,D2B
      DD=VAL(NWITHI)
      IDERR=NWITHI
      IF (DD.LE.0) GOTO 20
      D1T=D1B+DD
      D1B=D1B-DD
      D2T=D2B+DD
      D2B=D2B-DD
C Suppress part ranges to honour UNDER,OVER
      IF (CONOPT(NUNDER,NOVER)) RETURN
      IF (OPT(NUNDER)) THEN
         D1B=MIN(D1B,0)
         D1T=MIN(D1T,0)
         D2B=MIN(D2B,0)
         D2T=MIN(D2T,0)
      ENDIF
      IF (OPT(NOVER)) THEN
         D1B=MAX(D1B,0)
         D1T=MAX(D1T,0)
         D2B=MAX(D2B,0)
         D2T=MAX(D2T,0)
      ENDIF
C
C Tabulate XCC array
C - ugly ERROR setting is to allow abandon w/o including
C   COMMON in DPFIT2
      IF (D2FIT2(DP,NCOL,NCOL,NROW,XC,NXM,NCOLO,NROWO,
     +    AMIRR,KMIN,KMAX,D1B,D1T,D2B,D2T,DK,CS,LAM,
     +    C1,A1,A12,A,EFL)) THEN
        ERROR=4
        RETURN
      ENDIF
C
C Report results
      IF (OPT(NVERIF)) THEN
         WRITE (RECORD,10) C1,A1,A12
   10 FORMAT (' Best fitting C1,A1[2]: ',3F8.2)
         IF (SEMCON(RECORD)) RETURN
      ENDIF
C
C Return fitted parameters as C1,A1[2] with xcc in T
      IF (SEMLU(1,NC1,C1)) RETURN
      IF (SEMLU(1,NA1,A1)) RETURN
      IF (SEMLU(1,NA12,A12)) RETURN
C Force PHYS and IPS=2
      IF (SEMLU(1,NPHYSI,1.)) RETURN
      IF (SEMLU(1,NIPS,2.)) RETURN
C Return xcc value
      IF (SEMLU(1,NT,A)) RETURN
C Flag xcc value at edge of search region
      IF (SEMLU(1,NEFLAG,EFL)) RETURN
C
C Write XCC array to output
      IF (IVALPN(NTO).NE.0) THEN
         IF (SEMLAY(2,XC,NXM,NFMFP,1,LP2)) RETURN
      ENDIF
C
C Mark ring maxima along principal axes on dislay
C
      IF (ANNOT) THEN
C Ensure A is dirn of max Overfocus
         AM=SQRT(A1**2+A12**2)
         IF (AM.NE.0.) THEN
            A=.5*ATAN2(A12,A1)
         ELSE
            A=0
         ENDIF
C Mark maxima along principal directions
         CALL D3FMKM(C1+AM,A,DK,KMAX,LAM,CS)
         CALL D3FMKM(C1-AM,A+PI/2,DK,KMAX,LAM,CS)
      ENDIF
C
      RETURN
C
C Error return(s)
C Bad value
   20 ERROR=3
      RETURN
      END
C
C Submodule D3FMKM marks CTF maxima for D3FIT
C
      LOGICAL FUNCTION D3FMKM(D,A,DK,KMAX,LAM,CS)
      REAL D,A,DK,KMAX,LAM,CS
      LOGICAL D3FMKP
      REAL G,GM,R1,R2,S
      REAL PI
      PARAMETER (PI=3.1415927)
C
C Prepare error return
      D3FMKM=.TRUE.
      S=KMAX/DK*.07
C
C Overfocus: the easier case
      IF (D.GT.0) THEN
         GM = .5*PI*LAM**3*CS*KMAX**4 + PI*LAM*D*KMAX**2
         G=PI/2
 200     IF (G.LE.GM) THEN
            R1=SQRT((-D+SQRT(D**2+2*LAM*CS*G/PI))/LAM**2/CS)
            IF (D3FMKP(R1/DK,A,S)) RETURN
            G=G+PI
            GOTO 200
         ENDIF
      ELSE
C Underforcus: the harder case
         GM=-PI/2*D**2/LAM/CS
         G=-PI/2
 210     IF (G.GE.GM) THEN
            R2=D**2+2*LAM*CS*G/PI
            IF (R1.LT.0) RETURN
            R1=SQRT((-D-SQRT(R2))/LAM**2/CS)
C mark it
            IF (R1.LE.KMAX) THEN
               IF (D3FMKP(R1/DK,A,S)) RETURN
            ENDIF
            R1=SQRT((-D+SQRT(R2))/LAM**2/CS)
            IF (R1.LE.KMAX) THEN
               IF (D3FMKP(R1/DK,A,S)) RETURN
            ENDIF
            G=G-PI
            GOTO 210
         ENDIF
      ENDIF
C
C Normal return
      D3FMKM=.FALSE.
      RETURN
C
      END
C
C Submodule D3FMKP - for D3FMKM only
      LOGICAL FUNCTION D3FMKP(R,A,S)
      LOGICAL FSLINE
      REAL R,A,S,X,Y,DX,DY
C
C Prepare error return
      D3FMKP=.TRUE.
C
      X=R*COS(A)
      Y=R*SIN(A)
      DX=-S*SIN(A)
      DY=S*COS(A)
      IF (FSLINE(X-DX,Y-DY,X+DX,Y+DY)) RETURN
      IF (FSLINE(-X-DX,-Y-DY,-X+DX,-Y+DY)) RETURN
      D3FMKP=.FALSE.
      RETURN
      END
C
C Submodule FMIRRD finds mirror axis orientation
C
      SUBROUTINE FMIRRD(A,MA,MAP,NA,AMIRR,R1,R2,SEC,OCF,NW)
C
C Cross-correlates circular sections with their own reflection;
C rings are evenly spread over radii R1,R2 at one-pixel increments,
C and individual ring XCFs are weighted in proportion to radius
C before accumulation
C
C SEC(NW+2) and OCF(NW) provide internal workspace
C
      REAL PI
      PARAMETER (PI=3.1415917)
C
      REAL A(MAP,NA),SEC(NW),OCF(NW),AMIRR,R1,R2,R,DR,C1,C2,C3,T,X
      INTEGER MAP,MA,NA,NR,NW,NP,N,I,J
C
C Number of rings
      NR=NINT(R2-R1)
C Number of sample points around rings
      NP=32
   10 IF (NP.LT.NINT(PI*R2)) THEN
         IF (NP*2.LE.NW) THEN
            NP=NP*2
            GOTO 10
         ENDIF
      ENDIF
C Initialise OCF accumulator
      DO 30 I=1,NP
   30 OCF(I)=0
C
C Begin loop over rings
      DR=(R2-R1)/(NR-1)
      R=R1
      DO 60 N=1,NR
         CALL CSECXT(A,MAP,NA,SEC,NP,1,NA/2+1,R,.FALSE.)
         CALL FT1D(SEC,NP/2,-1,.TRUE.,.FALSE.,.FALSE.)
C Suppress DC values
         SEC(1)=0
C Product of conjugate and reflected xform = squared conjugate
         DO 40 I=1,NP/2+1
            T=SEC(2*I-1)**2-SEC(2*I)**2
            SEC(2*I)=-2*SEC(2*I-1)*SEC(2*I)
            SEC(2*I-1)=T
   40 CONTINUE
         CALL FT1D(SEC,NP/2,1,.TRUE.,.FALSE.,.FALSE.)
         DO 50 I=1,NP
            OCF(I)=OCF(I)+SEC(I)*R
   50 CONTINUE
C
C End loop over rings
      R=R+DR
   60 CONTINUE
C
C Find maximum
      X=0
      DO 80 I=1,NP
         IF (OCF(I).GT.X) THEN
            X=OCF(I)
            J=I
         ENDIF
   80 CONTINUE
C
C Fit quadratic to 5 points near peak
      C1=0
      C2=0
      C3=0
      DO 100 I=1,5
         T=OCF(1+MOD(J+I-4+NP,NP))
         C3=C3+T
         C2=C2+(I-3)*T
         C1=C1+(I-3)**2*T
  100 CONTINUE
      C2=C2/10
      C1=(C1-2*C3)/14
      X=-C2/2/C1
      AMIRR=(NP/2-J-X)*PI/NP/2
      RETURN
      END
C
C CSECXT submodule extracting interpolated circular section from 2-D
C        array from A(M,N) to SEC(L); ring centre M0,N0 radius R,
C        with samples evenly spread anticlockwise from the bottom;
C        semicircle unless FULL
C        NB: no checks on arguments
C
      SUBROUTINE CSECXT(A,M,N,SEC,L,M0,N0,R,FULL)
      REAL A(M,N),SEC(1),R,DTH,COD,SID,CO,SI,X,Y,CX,CY
      INTEGER M,N,L,M0,N0,I,J,K
      LOGICAL FULL
C
      DTH=3.1415927/L
      IF (FULL) DTH=DTH+DTH
      COD=COS(DTH)
      SID=SIN(DTH)
      CO=0
      SI=-R
C
C Loop over pixels around ring
      DO 10 K=1,L
         X=M0+CO
         Y=N0-SI
         I=X
         J=Y
         X=X-I
         Y=Y-J
         CX=1-X
         CY=1-Y
C Bilinear interpolation
         SEC(K)=CX*(A(I,J)*CY+A(I,J+1)*Y)+X*(A(I+1,J)*CY+A(I+1,J+1)*Y)
         X=CO
         CO=CO*COD-SI*SID
         SI=SI*COD+X*SID
 10   CONTINUE
C
      RETURN
      END
C
