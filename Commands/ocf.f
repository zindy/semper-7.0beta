C Semper 6 processing module OCF
C
      SUBROUTINE OCF
C
C Calculates angular correlation function between LP1 and LP2.
C Extacts a number RINGS of rings from each source picture, covering
C a range of radii from RADIUS to RA2 (default = 0.75 * max. radius).
C The rings are centred on the corresponding source picture centre
C positions and they occupy an angular range from -90 to +270 degrees
C if option FULL is set, or -90 to +90 degrees otherwise.  The cross
C correlation function for each pair of rings is calculated and added
C to the final result with weighting proportional to the rings' radius.
C The resulting correlation function is normalised and then output to
C a new picture if key TO is set.  Finally, the peak of the correlation
C function is located and its angular position stored in the variable
C THETA.  The rings are marked on the specified display picture if key
C MARK is set.
C
      REAL VAL
      INTEGER IVAL,IVALPN,OCF2
      LOGICAL OPT,VARSET,SEMOPN,SEMROW,SEMLU,EXTRCT,SEMCON
      LOGICAL MARSET,FSINIT,FSARC,FSFLUS
C
      INCLUDE 'COMMON'
C
      REAL C1,C2,C3,DR,DTH,MU,MU1,MU2,PEAK,R,R1,R2,RMAX
      REAL S1,S2,S3,SIGMA,SIGMA1,SIGMA2,SUMR,SUMXX,SUMXXR(2)
      REAL TH,TH1,TH2,THMAX,THETA,X0,Y0
      INTEGER I,IR,IRMAX(2),IPIC,ITO,ITOCF2,J,LPN,LPTEMP
      INTEGER MARK,NR,NPEAK,NTH
      LOGICAL ANNOT,FULL
C
      INTEGER LP(2)
C
      EQUIVALENCE (LP,LP1),(SIGMA1,SUMXXR(1)),(SIGMA2,SUMXXR(2))
C
C Packed names
C
      INTEGER NRINGS,NFULL,NRADIU,NRA2,NFROM,NWITH,NTO,NTHETA,NVERIF
      PARAMETER (NRINGS=29174, NFULL=10452, NRADIU=28844, NRA2=28872)
      PARAMETER (NFROM=10335, NWITH=-5181, NTO=-601, NTHETA=-326)
      PARAMETER (NVERIF=-3419)
C
      REAL DUMMY(2)
      DATA DUMMY / 2*0.0 /
C
C Fault multi-layer source picture
C
      IF (NLAYS(LP1).NE.1.OR.NLAYS(LP2).NE.1) THEN
         ERROR=62
         IDERR=VERB
         GOTO 140
      ENDIF
C
C Fault source picture FROM if 1-D
C
      IF (NCOLS(LP1).EQ.1.OR.NROWS(LP1).EQ.1) THEN
         ERROR=5
         IDERR=IVALPN(NFROM)
         GOTO 140
      ENDIF
C
C Fault source picture WITH if 1-D
C
      IF (NCOLS(LP2).EQ.1.OR.NROWS(LP2).EQ.1) THEN
         ERROR=5
         IDERR=IVALPN(NWITH)
         GOTO 140
      ENDIF
C
C Fault zero or negative value for key RINGS
C
      NR=IVAL(NRINGS)
      IF (NR.LE.0) THEN
         ERROR=3
         IDERR=NRINGS
         GOTO 140
      ENDIF
C
C See if option FULL is set
C
      FULL=OPT(NFULL)
C
C Determine maximum ring radius, allowing for 180 or 360 degree range
C
      DO 10 IPIC=1,2
         LPN=LP(IPIC)
         IRMAX(IPIC)=MIN(NCOLS(LPN)-CCOLN(LPN),NROWS(LPN)-CROWN(LPN),
     +                   CROWN(LPN)-1)
         IF (FULL) IRMAX(IPIC)=MIN(IRMAX(IPIC),CCOLN(LPN)-1)
   10 CONTINUE
C
      RMAX=REAL(MIN(IRMAX(1),IRMAX(2)))
C
C Determine value for outer ring radius
C
      IF (VARSET(NRA2)) THEN
         R2=VAL(NRA2)
      ELSE
         R2=0.75*RMAX
      ENDIF
C
C Determine value for inner radius
C
      IF (VARSET(NRADIU)) THEN
         R1=VAL(NRADIU)
      ELSE
         R1=R2/REAL(NR)
      ENDIF
C
C Fault bad value for radii
C
      IF (R1.LE.0.0.OR.R2.LT.R1.OR.R2.GT.RMAX) THEN
         ERROR=3
         IDERR=NRADIU
         GOTO 140
      ENDIF
C
C Determine radius increment
C
      IF (NR.EQ.1) THEN
         DR=0.0
      ELSE
         DR=(R2-R1)/REAL(NR-1)
       ENDIF
C
C Determine number of sample points in each ring (reduced to power of 2
C and to fit row buffer size)
C
      IR = INT(RMAX)
      NTH=4*OCF2(IR)
      IF (FULL) NTH=2*NTH
      ITOCF2 = LNBUF/LNCOMP 
      NTH=MIN(NTH,OCF2(ITOCF2))
C
C Determine angular increment
C
      IF (FULL) THEN
         THMAX=TWOPI
      ELSE
         THMAX=PI
      ENDIF
      DTH=THMAX/REAL(NTH)
      TH1=-(PI/2.0)
      TH2=TH1+THMAX
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
C Draw rings
C
            DO 20 IR=1,NR
               IF (FSARC(0.0,0.0,R1+REAL(IR-1)*DR,TH1,TH2)) GOTO 140
   20       CONTINUE
C
C Flush graphics buffer
C
            IF (FSFLUS()) GOTO 140
         ENDIF
      ENDIF
C
C Open temporary picture for intermediate storage of data
C
      LPTEMP=0
      IF (SEMOPN(3,0,NTH,3,1,NCLIMA,NFMFP,LPTEMP)) GOTO 140
C
C Initialise temporary cross correlation buffer
C
      DO 30 I=1,NTH
         RB4(I)=0.0
   30 CONTINUE
C
      IF (SEMROW(2,RB4,NFMFP,3,1,LPTEMP)) GOTO 140
C
C Initialise sums
C
      MU1=0.0
      MU2=0.0
      SIGMA1=0.0
      SIGMA2=0.0
C
C Process each ring in turn
C
      DO 90 IR=1,NR
C
C Determine ring radius
C
         R=R1+REAL(IR-1)*DR
C
C Process each source picture in turn
C
         DO 60 IPIC=1,2
C
C Determine LP number and centre position
C
            LPN=LP(IPIC)
            X0=REAL(CCOLN(LPN))
            Y0=REAL(CROWN(LPN))
C
C Calculate sample point positions (in pixel coordinates) for ring
C
            TH=TH1
            DO 40 I=1,NTH
               RB3(I)=X0+R*COS(TH)
               RB4(I)=Y0-R*SIN(TH)
               TH=TH+DTH
   40       CONTINUE
C
C Extract value from source picture for each sample point using
C bi-linear interpolation (result in row buffer RB2)
C
            IF (EXTRCT(LPN,NTH,NFMFP,1,.FALSE.,DUMMY)) GOTO 140
C
C Calculate sums for radially weighted mean square
C
            SUMXX=0.0
            DO 50 I=1,NTH
               SUMXX=SUMXX+RB2(I)*RB2(I)
   50       CONTINUE
C
            SUMXXR(IPIC)=SUMXXR(IPIC)+R*SUMXX
C
C Store ring values in temporary picture row (because EXTRCT uses row
C buffers RB1 to RB6 for extraction process)
C
            IF (SEMROW(2,RB2,NFMFP,IPIC,1,LPTEMP)) GOTO 140
   60    CONTINUE
C
C Fetch extracted values for the two rings
C
         IF (SEMROW(1,RB1,NFMCOM,1,1,LPTEMP)) GOTO 140
         IF (SEMROW(1,RB2,NFMCOM,2,1,LPTEMP)) GOTO 140
C
C Fourier transform both rings
C
         CALL FT1D(RB1,NTH,-1,.FALSE.,.FALSE.,.FALSE.)
         CALL FT1D(RB2,NTH,-1,.FALSE.,.FALSE.,.FALSE.)
C
C Calculate sums for radially weighted mean
C
         MU1=MU1+R*RB1(NTH+1)
         MU2=MU2+R*RB2(NTH+1)
C
C Multiply transforms together
C
         DO 70 I=1,2*NTH,2
            RB3(I)=RB1(I)*RB2(I)+RB1(I+1)*RB2(I+1)
            RB3(I+1)=RB1(I)*RB2(I+1)-RB1(I+1)*RB2(I)
   70    CONTINUE
C
C Inverse fourier transform the product
C
         CALL FT1D(RB3,NTH,1,.FALSE.,.FALSE.,.FALSE.)
C
C Add radially weighted cross correlation function for rings to temp
C cross correlation buffer
C
         IF (SEMROW(1,RB4,NFMFP,3,1,LPTEMP)) GOTO 140
C
         DO 80 I=1,NTH
            RB4(I)=RB4(I)+R*RB3(2*I-1)
   80    CONTINUE
C
         IF (SEMROW(2,RB4,NFMFP,3,1,LPTEMP)) GOTO 140
   90 CONTINUE
C
C Determine sum of weights
C
      SUMR=REAL(NTH)*REAL(NR)*(R1+R2)/2.0
C
C Fault nearly constant source picture from
C
      SIGMA1=SIGMA1-MU1*MU1/SUMR
      IF (SIGMA1.LE.0) THEN
         ERROR=12
         IDERR=IVALPN(NFROM)
         GOTO 140
      ENDIF
C
C Fault nearly constant source picture WITH
C
      SIGMA2=SIGMA2-MU2*MU2/SUMR
      IF (SIGMA2.LE.0) THEN
         ERROR=12
         IDERR=IVALPN(NWITH)
         GOTO 140
      ENDIF
C
C Determine mean and standard deviation for cross correlation function
C
      MU=REAL(NTH)*MU1*MU2/SUMR
      SIGMA=REAL(NTH)*SQRT(SIGMA1*SIGMA2)
C
C Fetch temporary cross correlation buffer
C
      IF (SEMROW(1,RB4,NFMFP,3,1,LPTEMP)) GOTO 140
C
C Normalise cross correlation function
C
      DO 100 I=1,NTH
         RB4(I)=(RB4(I)-MU)/SIGMA
  100 CONTINUE
C
C Store result in output picture if key TO set
C
      ITO=IVALPN(NTO)
      IF (ITO.GT.0) THEN
C
C Open output
C
         LP3=LP1
         IF (SEMOPN(2,ITO,NTH,1,1,NCLCOR,NFMFP,LP3)) GOTO 140
C
C Store result in LP3
C
         IF (SEMROW(2,RB4,NFMFP,1,1,LP3)) GOTO 140
      ENDIF
C
C Find peak in correlation function
C
      NPEAK=1
      DO 110 I=2,NTH
         IF (ABS(RB4(I)).GT.ABS(RB4(NPEAK))) NPEAK=I
  110 CONTINUE
C
C Fit parabola onto region around peak
C
      S1=0.0
      S2=0.0
      S3=0.0
      DO 120 I=-2,2
         J=MOD(NPEAK+I+NTH-1,NTH)+1
         S3=S3+RB4(J)
         S2=S2+REAL(I)*RB4(J)
         S1=S1+REAL(I*I)*RB4(J)
  120 CONTINUE
C
C Determine peak value, angular position and width of peak
C
      C1=(S1-2.0*S3)/14.0
      C2=S2/10.0
      C3=(5.0*S1-17.0*S3)/35.0
C
      PEAK=ABS(RB4(NPEAK))
      THETA=THMAX*((REAL(NPEAK-1)-C2/(2.0*C1))/REAL(NTH)-0.5)
      SIGMA=THMAX*SQRT(C3/(20.0*C1))/REAL(NTH)
C
C Set variable THETA to angular position of peak
C
      IF (SEMLU(1,NTHETA,THETA)) GOTO 140
C
C If VERIFY option is set, print results
C
      IF (OPT(NVERIF)) THEN
         WRITE (RECORD,130) PEAK,THETA,SIGMA
  130    FORMAT ('Peak ',F7.3,' at angle ',F7.3,' +/- ',F5.3)
         IF (SEMCON(RECORD)) GOTO 140
      ENDIF
C
  140 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module OCF2
C
      INTEGER FUNCTION OCF2(N)
      INTEGER N
C
C Returns power of 2 less than or equal to N
C
      INTEGER RESULT
C
C Initialise return value
C
      RESULT = 2
C
C Double return value while it is less than or equal to N
C
   10 IF (RESULT .LE. N) THEN
         RESULT = RESULT + RESULT
         GOTO 10
      ENDIF
C
C Adjust return value
C
      OCF2 = RESULT/2
C
      RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
