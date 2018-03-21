C Semper 6 processing module LPDET
C
      SUBROUTINE LPDET
C
C Linear Peak Detection program.
C
C Takes 1-D spectrum LP1, assumed to be positive peaks over a constant
C baseline and writes
C (1) a list of peak parameters as a 25-layer picture
C          (c.f. Particle analysis).
C (2) optionally a line spectrum consisting of isolated lines at the
C     peak maxima with heights equal to the integrated peak areas with
C     bounds of integration flagged by negative excursions.
C (3) Returns the semper variable N = number of peaks found
C   NOTE:  Complex input spectra are not supported.
C          All six buffers RB1-6 are used.
C
C-----------------------------------------------------------------------
C
C  Semper syntax:
C
C  LPDet :LPDET >$r5 baseline= height= saturate= area= line +
C               $3=999 plist=$3
C
C-----------------------------------------------------------------------
C
C  KEY     TYPE     Description
C
C  HEI      FP      Noise level.
C                   Default=max(bas-min/3,0)
C                   Peaks have one point above TH1 in order to be
C                   accepted.
C  SAT      FP      Threshold above which minima are ignored. For two
C                   peaks to be resolved the separating minimum must
C                   fall below this threshold.
C                       (USEFUL FOR SATURATED PEAKS)
C                   Default=max.
C  ARE      FP      Area threshold for peaks of interest. Default=0.
C  BAS      FP      Baseline level. Spectrum values below this value are
C                   ignored. BAS thereforedefines the edges of peaks.
C                   BAS should be set high enough to eliminate the bulk
C                   of baseline wobble w/o eliminating portions of
C                   desired peaks.
C                   Default=0.
C  LIN       L      IF (lin) then output line spectrum
C                   Default=TRUE
C  PLI       I      Selected stream for list of peak parameters.
C                   Default=picture 998
C------------------------------------------------------------------
C
C  On exit PLI is unchanged
C          SEL is set to the output spectrum if one is produced
C                else to the source spectrum.
C
C--------------------------------------------------------------------
C
C  VAR     TYPE     Description
C
C  SUM      FP      Integrated peak area.
C  IPOS      I      Location of peak maximum
C  YMAX     FP      Hight of peak maximum
C  YMIN     FO      Value of peak minimum
C  LEFT      I      Left-most boundary of peak
C  IRIGHT    I      Right-most boundary
C  INPK      L      .TRUE. if current pixel lies within bounds of a peak
C  VALID     L      .TRUE. if peak is regarded as acceptable
C  GMAX     FP      Global maximum for spectrum
C  GMIN     FP        ...  minimum .....
C  NPEAK     I      Total no. of peaks detected.
C
C-----------------------------------------------------------------------
C  BLAME:  Konrad
C-----------------------------------------------------------------------
C
C  Method:
C   (1) Locate peaks as regions of signal above the baseline BAS.
C   (2) Locate the set of local maxima and minima.
C       Maxima are constrained to lie above the baseline BAS.
C       Minima must lie below the saturation level TH2.
C   (3) A peak is acceptable if:
C           its maximum lies sufficiently above TH1
C           its area exceeds TH3
C
C       If no peaks are found or too many are found then a warning is
C       printed. The semper variable N is set to the number of peaks
C       found.
C
C-----------------------------------------------------------------------
C
      INCLUDE 'COMMON'
C
      INTEGER IPACK
      LOGICAL SEMROW,SEMOPN,OPTNO,SEMRNG,VARSET,SEMLU
C
      INTEGER LP4
      INTEGER NCOL,NROW,NLAY,INFORM,IVALPN
      INTEGER PLI,NPEAK,NLAYP,INCLAS
      INTEGER NCCOL,MXP,IPFROM
      INTEGER I,II,J,LEFT,IRIGHT,IPOS
      LOGICAL INPK,VALID,SPE
      REAL TH1,TH2,TH3,BAS,VAL,TEMP,TEMP2,SUM,YMAX,YMIN,GMAX,GMIN
C
      PARAMETER (NLAYP=25)
C
C  open 'from' picture
C
      IPFROM = IVALPN(10335)
      IF (SEMOPN(1,IPFROM,NCOL,NROW,NLAY,INCLAS,INFORM,LP1))
     +                                                         GOTO 130
      SPE = .NOT. OPTNO(19574)
C
C  optionally open 'to' picture LP2, floating point, same params
C     as 'from'
C
      IF (SPE) THEN
         LP2=LP1
         IF (SEMOPN(2,IVALPN(-601),NCOL,NROW,NLAY,
     +                INCLAS,NFMFP,LP2)) GOTO 130
      ENDIF
C
C---read 'from' picture max,min
C
      IF (SEMRNG(1,GMIN,GMAX,LP1)) GOTO 130
C
C  Fetch parameters/keys
C
      BAS = VAL(3259)
C
      IF (VARSET(13009)) THEN
         TH1=VAL(13009)
      ELSE
         TH1=(BAS-GMIN)/3.0
      ENDIF
C
      IF (VARSET(30460)) THEN
         TH2=VAL(30460)
      ELSE
         TH2=GMAX
      ENDIF
C
C Default for TH3 is 0 (when not quoted)
C
      TH3 = VAL(2325)
      PLI = IVALPN(26089)
C
C  Fault silly parameter values
C
      IF (TH1 .LT. 0.0) THEN
         IDERR = 13009
         ERROR = 3
         GOTO 130
      ENDIF
C
      IF (BAS .LT. 0.0) THEN
         IDERR = 3259
         ERROR = 3
         GOTO 130
      ENDIF
C
      IF (TH2 .LT. BAS .OR. TH2 .LT. TH1) THEN
         IDERR = 30460
         ERROR = 3
         GOTO 130
      ENDIF
C
      IF (GMAX .LE. BAS) THEN
         IDMESS = 'All points below baseline'
         ERROR = 77
         GOTO 130
      ENDIF
C
C Fault Complex picture type
C
      IF (INFORM .EQ. NFMCOM) THEN
         IDERR = IPFROM
         ERROR = 43
         GOTO 130
      ENDIF
C
C Fault multi-layer or multi-row source picture
C
      IF (NLAY .NE. 1) THEN
         IDERR = 19844
         ERROR = 62
         GOTO 130
      ENDIF
C
      IF (NROW .NE.1) THEN
         IDMESS = 'Only 1-d source pictures allowed'
         ERROR = 77
         GOTO 130
      ENDIF
C
C Fault source picture with three or fewer columns
C
      IF (NCOL .LE. 3) THEN
         ERROR = 5
         IDERR = IPFROM
         GOTO 130
      ENDIF
C
C Maximum number is NCOL/2 -1 because end points are always baseline
C
      MXP = (NCOL/2) - 1
C
C  open temporary plist for output
C
      LP3 = 0
      IF (SEMOPN(3,000,MXP,1,NLAYP,NCLPLI,NFMFP,LP3)) GOTO 130
C
C initialise to zero
C
      CALL LPDCLR(RB1,NCOL)
C
      DO 10 I=1,25
         IF (SEMROW(2,RB1,NFMFP,1,I,LP3)) GOTO 130
   10 CONTINUE
C
C Set buffer type to floating Point and fetch first picture
C
      IF (SEMROW(1,RB4,NFMFP,1,1,LP1)) GOTO 130
C
C  Set end points of spectrum to cutoff value
C
      RB4(1)=BAS
      RB4(NCOL)=BAS
C
C  First pass:
C             Set output buffer RB1 values
C
C  0  : Image value at or below baseline (bas)
C  -1 : Local minimum (above baseline and below th2)
C  1  : Above baseline not local min or max
C  2  : Local maximum
C
C
      RB1(1)=0.
      RB1(NCOL)=0
C
      DO 20 I=2,NCOL-1
         RB1(I)=0.0
C
C  Determine whether local maximum or minimum and store result in buffer
C  Plateaux are triggered at the leading edge.
C
         IF (RB4(I) .GT. BAS) THEN
            RB1(I)=1
            IF (RB4(I) .GE. RB4(I+1)) THEN
C
C  ..local maximum
C
               IF (RB4(I) .GT. RB4(I-1)) RB1(I)=2
            ENDIF
            IF (RB4(I) .LT. RB4(I-1)) THEN
C
C  ..local minimum (but not for points above th2)
C
               IF (RB4(I) .LE. RB4(I+1) .AND. RB4(I) .LT. TH2) RB1(I)=-1
            ENDIF
         ENDIF
   20 CONTINUE
C
C Pass 2:
C         move minima/maxima to center of any plateau regions
C
      J=1
      DO 30 I=2,NCOL-1
         IF (J .LT. I) THEN
            IF (RB1(I) .EQ. -1 .OR. RB1(I) .EQ. 2) THEN
               II=I
               CALL LPDCEN(RB4,J,II,NCOL-1)
C
C  returns J=right hand edge of plateau, II=middle
C
               IF (ERROR .NE. 0) GOTO 130
               IF (II .NE. I) THEN
                  RB1(II)=RB1(I)
                  RB1(I)=1
               ENDIF
            ENDIF
         ENDIF
   30 CONTINUE
C
C Pass 3:
C
C  set up following buffers:
C
C  Buffer      contents             destination layer
C                                       in plist
C   RB2      Position of maximum           1
C   RB3      Integrated area              19
C   RB5      Left hand edge                8
C   RB6      Right hand edge               9
C
C  initialise:
C
C  RB1 contains max/min/baseline flags
C
      CALL LPDCLR(RB2,MXP)
      CALL LPDCLR(RB3,MXP)
C
C  Leave RB4 which contains the original spectrum
C
      CALL LPDCLR(RB5,MXP)
      CALL LPDCLR(RB6,MXP)
C
      INPK=.FALSE.
      LEFT=0
      IRIGHT=0
      IPOS=0
      NPEAK=0
C
      DO 60 I=1,NCOL
C
C  are we still in region of the same peak as previously?
C
         IF (.NOT. INPK) THEN
C
C  NO:
C
            YMAX=BAS
            SUM=0.0
            IF (RB1(I) .NE. 0) THEN
C
C    entering new peak
C
               INPK=.TRUE.
C
C    mark left hand edge of peak
C
               LEFT=I
C
C    update area
C
               SUM=SUM+RB4(I)
C
C    mark peak maximum if applicable
C
               IF (RB1(I).EQ.2) THEN
                  IPOS=I
                  YMAX=RB4(IPOS)
               ENDIF
            ENDIF
         ELSE
C
C  YES:
C
           SUM=SUM+RB4(I)
           IF (RB1(I).GE. 2) THEN
              IF (RB4(I) .GT. YMAX) THEN
                 IPOS=I
                 YMAX=RB4(IPOS)
              ENDIF
           ENDIF
C
C    if end of peak: (i.e. baseline or minimum encountered)
C
           IF (RB1(I) .LE. 0) THEN
C
C    mark right hand edge of peak
C
              IF (RB1(I) .EQ. 0) THEN
                 IRIGHT=I-1
              ELSE
                 IRIGHT=I
              ENDIF
C
              INPK=.FALSE.
C
C  reject single pixel peaks
C             VALID=IRIGHT .GT. LEFT
C
              VALID=.TRUE.
C
C  Height and area threshold
C
              IF (VALID) VALID=(YMAX .GT. TH1 .AND. SUM .GT. TH3)
C
C  If valid peak, store parameters in plist:
C
              IF (VALID) THEN
                 NPEAK = NPEAK+1
C
C   Correct position of maximum for saturated peaks by
C           moving to center of saturation area
C
                 IF (YMAX .GT. TH2) THEN
                    TEMP=LEFT
                    DO 40 II=LEFT,IPOS
                       IF (RB4(II) .LT. TH2) TEMP=II
   40               CONTINUE
                    TEMP2=IRIGHT
                    DO 50 II=IRIGHT,IPOS,-1
                       IF (RB4(II) .LT. TH2) TEMP2=II
   50               CONTINUE
                    IPOS=(TEMP+TEMP2)/2
                 ENDIF
C
C  Position of maximum
C
                 RB2(NPEAK)=IPOS
C
C  Integrated area
C
                 RB3(NPEAK)=SUM
C
C  Left hand edge
C
                 RB5(NPEAK)=LEFT
C
C  Right hand edge
C
                 RB6(NPEAK)=IRIGHT
              ENDIF
           ENDIF
        ENDIF
   60 CONTINUE
C
C  Save results to plist
C
      IF (SEMROW(2,RB2,NFMFP,1,1,LP3)) GOTO 130
      IF (SEMROW(2,RB3,NFMFP,1,19,LP3)) GOTO 130
      IF (SEMROW(2,RB5,NFMFP,1,8,LP3)) GOTO 130
      IF (SEMROW(2,RB6,NFMFP,1,9,LP3)) GOTO 130
C
C  Construct optional spectrum LP2 in RB1
C
      IF (SPE) THEN
         CALL LPDCLR(RB1,NCOL)
         IF (NPEAK .LE. 0) GOTO 120
         DO 70 I=1,NPEAK
C
C diagnostics:
C
            IF (RB2(I) .GT. NCOL .OR. RB2(I) .LT. 1 .OR.
     +          RB5(I) .GT. NCOL .OR. RB5(I) .LT. 1 .OR.
     +          RB6(I) .GT. NCOL .OR. RB6(I) .LT. 1) THEN
               IDMESS = 'Internal Logic Error'
               ERROR = 77
               GOTO 130
            ENDIF
C
            RB1( NINT(RB5(I)) )=-GMAX
            RB1( NINT(RB6(I)) )=-GMAX
            RB1( NINT(RB2(I)) )= RB3(I)
   70    CONTINUE
C
C  save
C
         IF (SEMROW(2,RB1,NFMFP,1,1,LP2)) GOTO 130
      ENDIF
C
C Pass 4:
C
C  calculate remaining parameters of interest:
C  ceneters of mass etc... if required
C  (currently only additional params are SATURATION YMAX and YMIN)
C  Ymax is (unfortunately unnecessarily) re-calculated
C
C  set up following buffers:
C
C  Buffer      contents             destination layer
C                                       in plist
C   RB1      Minimum value attained       10
C   RB2      Saturated? (1/0)              7
C   RB3      Peak value attained          11
C   RB4      Original spectrum
C   RB5      Left hand edge
C   RB6      Right hand edge
C
C  clear output buffers:
C
      CALL LPDCLR(RB1,MXP)
      CALL LPDCLR(RB2,MXP)
      CALL LPDCLR(RB3,MXP)
C
C  calculate params:
C
      DO 90 I=1,NPEAK
         LEFT=RB5(I)
         IRIGHT=RB6(I)
         YMAX=BAS
         TEMP=GMAX
         DO 80 II=LEFT,IRIGHT
            YMAX=AMAX1(YMAX,RB4(II))
            YMIN=AMIN1(YMIN,RB4(II))
   80    CONTINUE
         RB1(I)=YMIN
         RB3(I)=YMAX
         IF (YMAX .GT. TH2) THEN
            RB2(I)=1.
         ELSE
           RB2(I)=0.
         ENDIF
   90 CONTINUE
C
C  Save
C
      IF (SEMROW(2,RB1,NFMFP,1,10,LP3)) GOTO 130
      IF (SEMROW(2,RB2,NFMFP,1,7,LP3)) GOTO 130
      IF (SEMROW(2,RB3,NFMFP,1,11,LP3)) GOTO 130
C
C  read picture center coordinates for original spectrum
C
      NCCOL=CCOLN(LP1)
C
C  shift values appropriately so that
C  positions now relative to old coordinate system
C
C-- Clarity rather than efficiency!
C
C
C   position of maximum
C
      IF (SEMROW(1,RB1,NFMFP,1,1,LP3)) GOTO 130
      CALL LPDSH1(RB1,REAL(-NCCOL),NPEAK)
      IF (SEMROW(2,RB1,NFMFP,1,1,LP3)) GOTO 130
C
C   left hand edge
C
      IF (SEMROW(1,RB1,NFMFP,1,8,LP3)) GOTO 130
      CALL LPDSH1(RB1,REAL(-NCCOL),NPEAK)
      IF (SEMROW(2,RB1,NFMFP,1,8,LP3)) GOTO 130
C
C   right hand edge
C
      IF (SEMROW(1,RB1,NFMFP,1,9,LP3)) GOTO 130
      CALL LPDSH1(RB1,REAL(-NCCOL),NPEAK)
      IF (SEMROW(2,RB1,NFMFP,1,9,LP3)) GOTO 130
C
C  edit plist
C
C  open plist for output
C
      IF (NPEAK .LE. 0) THEN
         GOTO 120
      ELSE
         LP4 = 0
         IF (SEMOPN(2,PLI,NPEAK,1,NLAYP,NCLPLI,NFMFP,LP4))GOTO 130
C
C initialise to zero
C
         CALL LPDCLR(RB1,NCOL)
C
C  Copy
C
C Set buffer type to floating Point and fetch first picture
C
         DO 100 I=1,25
            IF (SEMROW(1,RB1,NFMFP,1,I,LP3)) GOTO 130
            IF (SEMROW(2,RB1,NFMFP,1,I,LP4)) GOTO 130
  100    CONTINUE
C
C  set up peak identifiers (layer 3 of plist)
C
         DO 110 I=1,NPEAK
            RB1(I)=I
  110    CONTINUE
         IF (SEMROW(2,RB1,NFMFP,1,3,LP4)) GOTO 130
      ENDIF
C
C  tidy: reset selected picture (sel) and (pli)
C
C   sel:
C
      IF (SPE) THEN
         OPLPN=LP2
      ELSE
         OPLPN=LP1
      ENDIF
C
C  set N to number of peaks found
C
  120 IF (SEMLU(1,22400,REAL(NPEAK))) GOTO 130
  130 RETURN
C
C Copyright (C) 1987-1993:  Synoptics Ltd,  All Rights Reserved
C
      END
C
      SUBROUTINE LPDCEN(X,I,J,N)
C
C   For J left-hand edge of plateau move J to center
C   Return coordinates of end of plateau in I
C
      INTEGER J,I,K,N
      REAL X(N),TEMP
C
      INCLUDE 'COMMON'
C
      IF (J .GT. N .OR. J .LT. 1) THEN
         IDMESS = 'Internal error 1 in LPDCEN'
         ERROR = 77
         GOTO 20
      ENDIF
C
      I=0
      TEMP=X(J)
C
C  set I = length of plateau
C
   10 CONTINUE
         I=I+1
         IF (X(J+I) .EQ. TEMP .AND. (J+I) .LT. N) GOTO 10
C
C  and shift position of maximum/minimum
C
      K=J
      J=J+ (I-1)/2
      I=K+I-1
C
C--diagnostic
C
      IF (J .GT. N .OR. J .LT. 1) THEN
         IDMESS = 'Internal error 2 in LPDCEN'
         ERROR = 77
      ENDIF
C
C--end diagnostic
C
   20 RETURN
C
C Copyright (C) 1987-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C
      SUBROUTINE LPDCLR(X,N)
C
C  Set vector X = 0
C
      INTEGER N,I
      REAL X(N)
C
      DO 10 I=1,N
         X(I)=0.0
   10 CONTINUE
C
      RETURN
C
C Copyright (C) 1987-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C
      SUBROUTINE LPDSH1(X,C,N)
C
C  Set vector X <- X + Constant
C
      INTEGER I,N
      REAL C,X(N)
      DO 10 I=1,N
         X(I)=X(I)+C
   10 CONTINUE
      RETURN
C
C Copyright (C) 1987-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
