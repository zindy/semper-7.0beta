C Semper 6 processing module BASFIT
C
C Recent bug fixes
C - direction of displacement marks corrected
C - divn overflow when U,V both zero trapped safely
C - value for R now returned correctly
C harmless enhancement:
C - circ subregion marked on display if appropriate
C
      SUBROUTINE BASFIT
C
C Provides verbs BASE and STRAIN
C - BASE fits a lattice to the peak list in LP1, using a least squares
C   criterion to assign base vectors U,V and offset W
C - STRAIN deduces local strain values on the basis of the deviations
C   from perfect lattice sites, and outputs a fresh list sorted in
C   order of increasing strain level
C NB: WOS believes STRAIN should be expunged from the next release, as
C of insufficient real value; people who want to do this kind of thing
C need the PL package instead.
C
C Peaks further than RADIUS from POSITION, and those further
C than TOLERANCE from the initially assumed lattice sites, are excluded
C from the fitting process; those outside the circle or off the
C fitted lattice are excluded from an optional condensed list output
C to TO if this is set
C
      REAL VAL
      INTEGER IVAL,IVALPN
      LOGICAL SEMOPN,SEMLAB,SEMROW,SEMLU,OPT,VARSET,BAS2,SEMDIA,SEMCON
      LOGICAL MARSET,FSINIT,FSLINE,FSCIRC,FSFLUS
C
      INCLUDE 'COMMON'
C
C Packed names
C
      INTEGER NRADIU,NR,NTIMES,NFROM,NTO,NVERIF,NTOLER,NSTRAI
      INTEGER NUVONL,NNUMBE,NNU2,NPOSIT,NPO2,NN
      PARAMETER (NRADIU=28844,NR=28800,NTIMES=-374,NFROM=10335,NTO=-601)
      PARAMETER (NVERIF=-3419,NTOLER=-613,NSTRAI=31218,NUVONL=-2496)
      PARAMETER (NNUMBE=23253,NNU2=23272,NPOSIT=26219,NPO2=26232)
      PARAMETER (NN=22400)
C
      REAL C(9),CSAVE(9),VCMPS(3,2),TIMES
      REAL D,DR,DX,DY,ONEMW,ONEPW,R,R2,RM,RN,RRMAX,W,X,X0,XI,Y,Y0,YI
      REAL UVW(3,2),U1,U2,V1,V2,W1,W2
      INTEGER NUVW(3,2),NBS(8),RANK,LABEL(256)
      INTEGER I,I1,I2,J,M,MARK,N,NCOL,NDEL,NLAY,NNB,NPEAKS,NTOTAL
      LOGICAL FITTED,VTOO,LIST,ANNOT,LVERIF
C
      EQUIVALENCE (LABEL,RB1)
      EQUIVALENCE (UVW(1,1),U1),(UVW(2,1),V1),(UVW(3,1),W1)
      EQUIVALENCE (UVW(1,2),U2),(UVW(2,2),V2),(UVW(3,2),W2)
C
C NUVW contains U,V,W,U2,V2,W2
C
      DATA NUVW/-1601,-3201,-4801,-2881,-4481,-6081/
C
C Initialise
C
      TIMES=VAL(NTIMES)
      LVERIF=OPT(NVERIF)
      W=VAL(NTOLER)
      RRMAX=VAL(NRADIU)**2
      IF (RRMAX.EQ.0.) RRMAX=1E10
      X0=VAL(NPOSIT)
      Y0=VAL(NPO2)
      RANK=3
      IF (OPT(NUVONL).AND.VERB.NE.NSTRAI) RANK=2
      NCOL=NCOLS(LP1)
      NLAY=MIN(NLAYS(LP1),3)
      VTOO=NLAY.EQ.3.OR.VERB.EQ.NSTRAI
      LIST=IVAL(NTO).NE.0
C
C Initialise display annotation
C
      IF (MARSET(ANNOT,MARK)) GOTO 160
C
      IF (ANNOT) THEN
         IF (FSINIT(3,MARK)) GOTO 160
         ANNOT=FSPTYP.EQ.1
C WOS addition: mark circular region if specified
C (this is useful, and it does not hurt that the docn does
C not mention it)
         IF (RRMAX.NE.1E10) THEN
            IF (FSCIRC(X0,Y0,VAL(NRADIU))) GOTO 160
         ENDIF
      ENDIF
C
C Fetch peak list and check class
C
      IF (CLASSN(LP1).NE.NCLPLI) THEN
         ERROR=6
         IDERR=IVALPN(NFROM)
         GOTO 160
      ENDIF
C
C Set initial estimates for U,V,W
C
      W1=0.
      W2=0.
      DO 20 I=1,RANK
         DO 10 J=1,2
            UVW(I,J)=VAL(NUVW(I,J))
   10    CONTINUE
   20 CONTINUE
C
C Fetch X and Y values contained in position list
C
      IF (SEMROW(1,RB4,NFMFP,1,1,LP1)) GOTO 160
      IF (SEMROW(1,RB5,NFMFP,1,2,LP1)) GOTO 160
C
C Establish peak numbers to be fitted
C
      IF (VARSET(NNUMBE)) THEN
         I1=MAX(IVAL(NNUMBE),0)
      ELSE
         I1=1
      ENDIF
C
      IF (VARSET(NNU2)) THEN
         I2=MIN(IVAL(NNU2),NCOL)
      ELSE
         I2=NCOL
      ENDIF
C
      IF (I2.LT.I1) THEN
         ERROR=3
         IDERR=NNUMBE
         GOTO 160
      ENDIF
C
      FITTED=.FALSE.
C
C Check U,V non-zero
   30 R=U1*U1+U2*U2
      IF (R.EQ.0.) THEN
         ERROR=3
         IDERR=NUVW(1,1)
         GOTO 160
      ENDIF
      R2=V1*V1+V2*V2
      IF (R2.EQ.0.) THEN
         ERROR=3
         IDERR=NUVW(2,1)
         GOTO 160
      ENDIF
C Check U,V independent
      D=U1*V2-U2*V1
      IF (D*D/R/R2.LT.1E-6) THEN
         ERROR=79
         GOTO 160
      ENDIF
C
C Zero accumulators
C
      DO 50 I=1,3
         DO 40 J=1,2
            VCMPS(I,J)=0.
   40    CONTINUE
   50 CONTINUE
C
      DO 60 I=1,9
         C(I)=0.
   60 CONTINUE
      NTOTAL=0
      NPEAKS=0
      R=0.
C
C Passes 1 and 2 through peaks
C ============================
C - Pass 1 accumulates coefficients for fitting process
C - Pass 2 appraises the quality of fit
      DO 80 I=I1,I2
      XI=RB4(I)
      X=XI-X0
      YI=RB5(I)
      Y=YI-Y0
      R2=X*X+Y*Y
      X=XI-W1
      Y=YI-W2
C
C Map into lattice coordinates
      RM=(X*V2-Y*V1)/D
      RN=(Y*U1-X*U2)/D
C Note (exact) mapped coords in RB1,RB2
      RB1(I)=RM
      RB2(I)=RN
C Note mapped displacements in DX,DY; raw in X,Y and RB3,RB5
      M=NINT(RM)
      N=NINT(RN)
      DX=ABS(REAL(M)-RM)
      DY=ABS(REAL(N)-RN)
      RM=REAL(M)
      RN=REAL(N)
      X=XI-(RM*U1+RN*V1+W1)
      Y=YI-(RM*U2+RN*V2+W2)
      RB3(I)=X
      RB5(I)=Y
C
C Ignore if outside specified region or if too far from lattice
C (these tests deferred till here so that info needed by STRAIN
C evaluation code is available for all peaks)
      IF (R2.LE.RRMAX) THEN
C Count number of peaks within region fitted
         NTOTAL=NTOTAL+1
         IF (DX.LE.W .AND. DY.LE.W) GOTO 70
      ENDIF
C
C On pass 2, delete peaks beyond fitting tolerance
C
      IF (FITTED) RB4(I)=1E6
      GOTO 80
C
C Count peak, and either accumulate equation coefficients..
   70 NPEAKS=NPEAKS+1
      IF (.NOT.FITTED) THEN
         C(1)=C(1)+RM*RM
         C(2)=C(2)+RM*RN
         C(3)=C(3)+RM
         VCMPS(1,1)=VCMPS(1,1)+XI*RM
         VCMPS(1,2)=VCMPS(1,2)+YI*RM
         C(5)=C(5)+RN*RN
         C(6)=C(6)+RN
         VCMPS(2,1)=VCMPS(2,1)+XI*RN
         VCMPS(2,2)=VCMPS(2,2)+YI*RN
         VCMPS(3,1)=VCMPS(3,1)+XI
         VCMPS(3,2)=VCMPS(3,2)+YI
C
C ..or compare data with fitted lattice
      ELSE
         DR=X*X+Y*Y
         R=R+DR
C
C Mark deviation (magnified) on display?
         IF (ANNOT) THEN
C WOS change: - for + to reverse direction of line
            IF (FSLINE(XI,YI,XI-X*TIMES,YI-Y*TIMES)) GOTO 160
         ENDIF
      ENDIF
C
   80 CONTINUE
C
C End of pass - any peaks found?
C
      IF (NPEAKS.EQ.0) THEN
         ERROR=77
         IDMESS = 'No positions found'
         GOTO 160
      ENDIF
C
      IF (.NOT.FITTED) THEN
C
C Fill in remaining elements of coefficient array (by symmetry)
C
         C(4)=C(2)
         C(7)=C(3)
         C(8)=C(6)
         C(9)=REAL(NPEAKS)
C
C Solve for X and Y components in turn
C - Keep copy of coefficients for 2nd time
C
         DO 90 I=1,9
            CSAVE(I)=C(I)
   90    CONTINUE
         IF (BAS2(C,VCMPS,3,RANK)) GOTO 170
         IF (BAS2(CSAVE,VCMPS(1,2),3,RANK)) GOTO 170
C
C Reset lattice vectors and return them
C
         DO 110 I=1,RANK
            DO 100 J=1,2
               X=VCMPS(I,J)
               IF (SEMLU(1,NUVW(I,J),X)) GOTO 160
               UVW(I,J)=X
  100       CONTINUE
  110    CONTINUE
C
C Restore raw Y positions
         IF (SEMROW(1,RB5,NFMFP,1,2,LP1)) GOTO 160
C Go through peak list again, appraising fit obtained
         FITTED=.TRUE.
         GOTO 30
      ENDIF
C
C Flush display?
C
      IF (ANNOT) THEN
         IF (FSFLUS()) GOTO 160
      ENDIF
C
C Return number of positions, and rms deviation
C
      IF (SEMLU(1,NN,REAL(NPEAKS))) GOTO 160
      R=R/REAL(NPEAKS)
      R=SQRT(R)
      IF (SEMLU(1,NR,R)) GOTO 160
C
C If VERIFY option is set, print vectors and quality of fit
C
      NDEL=NTOTAL-NPEAKS
      X=100.0*REAL(NDEL)/REAL(NTOTAL)
C
      IF (LVERIF) THEN
         WRITE (RECORD,180) U1,U2,V1,V2
         IF (SEMCON(RECORD)) GOTO 160
C
         IF (RANK.EQ.3) THEN
            WRITE (RECORD,190) W1,W2
            IF (SEMCON(RECORD)) GOTO 160
         ENDIF
C
         WRITE (RECORD,200) R
         IF (SEMCON(RECORD)) GOTO 160
         WRITE (RECORD,210) NDEL,X
         IF (SEMCON(RECORD)) GOTO 160
      ENDIF
C
C If high proportion off lattice, print warning on terminal
C
      IF (X.GT.10.0) THEN
         WRITE (RECORD,220) X
         IF (SEMDIA(RECORD,NDIWAR)) GOTO 160
      ENDIF
C
C Lattice fitting complete
C
C If STRAIN, make third pass, evaluating local strain
C - i.e. rms differential displacements
      IF (VERB.EQ.NSTRAI) THEN
         ONEPW=1.+W
         ONEMW=1.-W
C Note longest base vector, allowing 1+TOLER tolerance
         RRMAX=U1*U1+U2*U2
         X=V1*V1+V2*V2
         IF (X.GT.RRMAX) RRMAX=X
         RRMAX=RRMAX*ONEPW*ONEPW
         DO 150 I=1,NCOL
C Position of this peak
            X0=RB1(I)
            Y0=RB2(I)
C
C Find and list neighbours, defined as peaks whose lattice coords
C are nearly 0/1/-1 wrt self and whose real distance is not much
C greater than the longer base vector
            NNB=0
            DO 120 J=1,NCOL
               X=RB1(J)-X0
               IF (X.LT.0.) X=-X
               Y=RB2(J)-Y0
               IF (Y.LT.0.) Y=-Y
               IF (X.GT.ONEPW .OR. Y.GT.ONEPW) GOTO 120
               IF (X.GT.W .AND. X.LT.ONEMW) GOTO 120
               IF (Y.GT.W .AND. Y.LT.ONEMW) GOTO 120
               IF (X.LT.W .AND. Y.LT.W) GOTO 120
               XI=X*U1+Y*V1
               YI=X*V1+Y*V2
               IF (XI*XI+YI*YI.GT.RRMAX) GOTO 120
C
C Neighbour found - note it
C
               NNB=NNB+1
               NBS(NNB)=J
C Don't take more than 8 neighbours (excessive, surely)
               IF (NNB.EQ.8) GOTO 130
  120       CONTINUE
C
C If too few neighbours, flag peak for deletion later
C
            IF (NNB.LE.1) THEN
               RB4(I)=1E6
               GOTO 150
            ENDIF
C
C Find rms differential displacement in neighbourhood
  130       R=0.
            DO 140 J=1,NNB
               M=NBS(J)
               X=RB3(M)-RB3(I)
               Y=RB5(M)-RB5(I)
               R=R+X*X+Y*Y
  140       CONTINUE
            RB6(I)=SQRT(R/REAL(NNB))
  150    CONTINUE
      ENDIF
C
C Revised list to be output?
      IF (LIST) THEN
C
C Recover raw Y values
         IF (SEMROW(1,RB5,NFMFP,1,2,LP1)) GOTO 160
C Load any supplementary information for corresponding sifting
         IF (VTOO) THEN
            IF (SEMROW(1,RB6,NFMFP,1,3,LP1)) GOTO 160
         ENDIF
C
C Condense list omitting any deleted positions
         CALL PEAKSD(RB4,RB5,RB6,NCOL,VTOO)
C
C IF STRAIN, re-sort list into ascending strain order
         IF (VERB.EQ.NSTRAI) THEN
            NDEL=(NCOLS(LP1)-NCOL)-NDEL
            IF (LVERIF) THEN
               WRITE (RECORD,230) NDEL
               IF (SEMCON(RECORD)) GOTO 160
            ENDIF
            CALL PEAKSR(RB4,RB5,RB6,NCOL,.TRUE.)
         ENDIF
C
C Open output and record Plist type
         LP2=LP1
         IF (SEMOPN(2,IVALPN(NTO),NCOL,1,NLAY,NCLPLI,NFMFP,LP2))
     +      GOTO 160
         IF (SEMLAB(1,LABEL,LP2)) GOTO 160
         LABEL(LBPLTY)=1
         IF (SEMLAB(2,LABEL,LP2)) GOTO 160
C Output data
         IF (SEMROW(2,RB4,NFMFP,1,1,LP2)) GOTO 160
         IF (SEMROW(2,RB5,NFMFP,1,2,LP2)) GOTO 160
         IF (VTOO) THEN
            IF (SEMROW(2,RB6,NFMFP,1,3,LP2)) GOTO 160
         ENDIF
      ENDIF
C
  160 RETURN
C
C Errors
C
  170 ERROR=77
      IDMESS = 'Badly conditioned data'
      GOTO 160
C
  180 FORMAT ('Fitted base vectors: U=',F7.2,',',F7.2,
     +                           '  V=',F7.2,',',F7.2)
  190 FORMAT ('             Offset: W=',F7.2,',',F7.2)
  200 FORMAT ('Rms deviation of fitted positions:',F6.2,' pixels')
  210 FORMAT ('Positions excluded:',I5,', i.e.',F6.2,'%')
  220 FORMAT ('Warning:',F6.2,'% of peaks outside fitting tolerance')
  230 FORMAT ('Positions excluded by strain threshold:',I5)
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module BAS2
C
      LOGICAL FUNCTION BAS2(A,B,MA,M)
C
C Solves linear equations A.X=B using Gaussian elimination
C and back-substitution, but without any pivoting
C - solution replaces B, and A is used as workspace
C - FALSE returned unless singular or pivot vanishes
C - NB Coefficient ordering is transposed wrt normal matrix notation
C
      INTEGER M,MA
      REAL A(MA,*),B(*)
C
      REAL F,X
      INTEGER I,J,JP,K
C
C Elimination
C
      BAS2 = .FALSE.
      DO 30 J=2,M
         JP=J-1
         X=A(JP,JP)
         IF (X.EQ.0.) GOTO 70
         DO 20 K=J,M
            F=A(JP,K)/X
            DO 10 I=J,M
               A(I,K)=A(I,K)-F*A(I,JP)
   10       CONTINUE
            B(K)=B(K)-F*B(JP)
   20    CONTINUE
   30 CONTINUE
C
C Back substitution
C
      X=0.
      J=M
      GOTO 60
C
   40 JP=J+1
      X=0.
      DO 50 I=JP,M
         X=X+A(I,J)*B(I)
   50 CONTINUE
   60 F=A(J,J)
      IF (F.NE.0.) THEN
         B(J)=(B(J)-X)/F
         IF (J.EQ.1) GOTO 80
         J=J-1
         GOTO 40
      ENDIF
C
C Pivot or determinant vanishes
C
   70 BAS2=.TRUE.
   80 RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
