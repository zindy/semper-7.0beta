C Semper 6 processing module NOISE
C
      SUBROUTINE NOISE
C
C Gaussian, negative exponential, uniform or poisson noise simulation.
C
C Generates (white) noisy version of LP1 in LP2
C - in default, adds noise with a zero-mean gaussian distribution with
C   with s.d. WIDTH (typical of image noise)
C - if EXPONENTIAL, adds noise with a negative exponential distribution
C   with s.d. WIDTH (typical of diffraction patterns)
C - if UNIFORM, adds noise with a uniform distribution in 0-WIDTH
C   (including 0 but not WIDTH)
C - if key DOSE set, generates instead Poisson quantum counts for each
C   pixel, scaled so as roughly to preserve the picture mean; all pixel
C   values must be positive in this case.
C
C VD required:
C Noise :NOISE width=.1 dose= exponential uniform preset >$ft
C
      INCLUDE 'COMMON'
C
      REAL NOISE2,VAL
      LOGICAL VARSET,MEANSD,SEMROW,SEMLU,OPT,DOSET,NEGEXP,UNIF
C
      REAL MBD,DOSE,WIDTH,DBM,EMU,RMU,G
      INTEGER I,J,L,N,NCOL,NROW,NLAY,INFORM,ISTEP,NP
C
C Random value generator (RVG) parameters
      INTEGER*4 RVGM,RVGA,RVGC,RV
      PARAMETER (RVGM=714025,RVGA=1366,RVGC=150889)
C ****** CHANGE ******
C If no INTEGER*4 available, the values
C     PARAMETER (RVGM=11979,RVGA=430,RVGC=2531)
C should be used instead
C ****** ****** ******
      REAL RVGS
      PARAMETER (RVGS=1./RVGM)
C
C Packed names
      INTEGER NDOSE,NWIDTH,NRNM,NFROM,NEXPON,NUNIFO,NPRESE
      PARAMETER (NDOSE=7019,NWIDTH=-5165,NRNM=29373,NFROM=10335)
      PARAMETER (NEXPON=8976,NUNIFO=-2170,NPRESE=26325)
C
C Fetch source picture size
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      NLAY=NLAYS(LP1)
C
C Establish form to be used internally
      INFORM=NFMFP
      ISTEP=1
      IF (FORMN(LP1).EQ.NFMCOM) THEN
         INFORM=NFMCOM
         NCOL=2*NCOL
         ISTEP=2
      ENDIF
C
C Establish mode
      NEGEXP=OPT(NEXPON)
      UNIF=OPT(NUNIFO)
      DOSET=VARSET(NDOSE)
C
C If DOSE, check value..
      IF (DOSET) THEN
         DOSE=VAL(NDOSE)
         IF (DOSE.LE.0.0) THEN
            ERROR=3
            IDERR=NDOSE
            GOTO 110
         ENDIF
C ..and determine picture mean(s)
         IF (.NOT.OPT(NPRESE)) THEN
            SMGL1=.FALSE.
            IF (MEANSD(LP1)) GOTO 110
         ENDIF
         IF (VMEAN.LE.0.
     +      .OR.VME2.LE.0..AND.INFORM.EQ.NFMCOM) THEN
            ERROR=22
            GOTO 110
         ENDIF
C
C Otherwise, fetch and check WIDTH
      ELSE
         WIDTH=VAL(NWIDTH)
         IF (WIDTH.LE.0.0) THEN
            ERROR=3
            IDERR=NWIDTH
            GOTO 110
         ENDIF
      ENDIF
C
C Fetch RVG seed
      RV=MOD(VAL(NRNM),1.)*RVGM+.5
C Initialise gaussian RVG in case
      SMGL1=.FALSE.
C
C Process source picture
      DO 100 L=1,NLAY
         DO 90 J=1,NROW
C
C Read source row from LP1
            IF (SEMROW(1,RB1,INFORM,J,L,LP1)) GOTO 110
C
C If DOSE set, generate quantum count-limited pixel values
            IF (DOSET) THEN
C
C Process each pixel in row
C
C Poisson generator needs separate passes for real, imag parts
               DO 40 NP=1,ISTEP
                  MBD=VMEAN/DOSE
                  IF (NP.EQ.2) MBD=VME2/DOSE
                  DBM=1./MBD
                  DO 30 I=NP,NCOL,ISTEP
                     RMU=DBM*RB1(I)
C
C For counts > 10, approximate Poisson by Gaussian
                     IF (RMU.GT.10.0) THEN
                        RB1(I)=RB1(I)+MBD*SQRT(RMU)*NOISE2(RV)
C
C Otherwise, for positive values, generate Poisson random number
                     ELSE IF (RMU.GE.0.0) THEN
                        EMU=EXP(-RMU)
                        G=1.0
C
                        DO 10 N=0,100
                           RV=MOD(RV*RVGA+RVGC,RVGM)
                           G=G*RV*RVGS
                           IF (G.LT.EMU) GO TO 20
   10                   CONTINUE
   20                   RB1(I)=REAL(N)*MBD
C
C Otherwise, fault negative source pixel value
                     ELSE
                        ERROR=22
                        GOTO 110
                     ENDIF
   30             CONTINUE
   40          CONTINUE
C
C Otherwise, add suitably distributed random number value to each pixel
            ELSE IF (NEGEXP) THEN
C Negative exponential, obtained as -log(uniform)
               DO 60 I=1,NCOL
   50             RV=MOD(RV*RVGA+RVGC,RVGM)
                  IF (RV.EQ.0.) GOTO 50
                  RB1(I)=RB1(I)-WIDTH*LOG(RV*RVGS)
   60          CONTINUE
            ELSE IF (UNIF) THEN
C Uniform
               DO 70 I=1,NCOL
                  RV=MOD(RV*RVGA+RVGC,RVGM)
                  RB1(I)=RB1(I)+WIDTH*RV*RVGS
   70          CONTINUE
            ELSE
C Gaussian
               DO 80 I=1,NCOL
                  RB1(I)=RB1(I)+WIDTH*NOISE2(RV)
   80          CONTINUE
            ENDIF
C
C Output result
            IF (SEMROW(2,RB1,INFORM,J,L,LP2)) GOTO 110
   90    CONTINUE
  100 CONTINUE
C
C Set variable RNM to current random number seed
      DUMLOG=SEMLU(1,NRNM,RV*RVGS)
C
  110 RETURN
C
C Copyright (C) 1987:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module NOISE2
C
      REAL FUNCTION NOISE2(RV)
C
C Gaussian (normal) random value generator; zero mean, unit s.d.
C SMGL1 should be initalised to .FALSE. before first call; the
C module uses SMGL1,SMGR1 thereafter
C
C Random value generator (RVG) parameters
      INTEGER*4 RVGM,RVGA,RVGC,RV
      PARAMETER (RVGM=714025,RVGA=1366,RVGC=150889)
      REAL RVGS
      PARAMETER (RVGS=1./RVGM)
C
      REAL V1,V2,R,FAC
C
      INCLUDE 'COMMON'
C
C Generate two random numbers in the unit circle
      IF (.NOT.SMGL1) THEN
   10    CONTINUE
         RV=MOD(RV*RVGA+RVGC,RVGM)
         V1=2.*RV*RVGS-1.
         RV=MOD(RV*RVGA+RVGC,RVGM)
         V2=2.*RV*RVGS-1.
         R=V1*V1+V2*V2
         IF (R.GE.1.) GOTO 10
C Box-Muller transformation provides two normal deviates
         FAC=SQRT(-2.*LOG(R)/R)
C Save one for next time..
         SMGR1=V1*FAC
C ..and return the other
         NOISE2=V2*FAC
         SMGL1=.TRUE.
      ELSE
C Use second deviate produced on last call
         NOISE2=SMGR1
         SMGL1=.FALSE.
      ENDIF
      RETURN
C
      END
