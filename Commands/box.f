C Semper 6 module: BOX
C---------------------------------------------------------------------
C
C BOX:
C
C Perform a box classification of a picture based upon the class-
C ification data given in a separate picture.
C
C Arguments:
C   None.
C
      SUBROUTINE BOX
C     ==============
C
      INTEGER*4 LNGONE,I44
      PARAMETER (LNGONE = 1, I44 = 4)
      INTEGER NSTRID, NTO, NWITH
      PARAMETER (NWITH= -5181, NSTRID = 31218, NTO = -601)
C
      INCLUDE 'COMMON'
C
      INTEGER*4 LNREA4
      PARAMETER (LNREA4 = LNREAL)
C
      LOGICAL CLSOVP, SEMOPN, SEMROW, TSTSRG, SEMDIA
      INTEGER IVAL, IVALPN, SEMFRM
      EXTERNAL CLSOVP, IVAL, IVALPN, SEMFRM, SEMOPN, SEMROW, TSTSRG
C
      INTEGER*4 BASE4, INCR, IX0, IX1, IXA, IXB, MAXLYR, STEPS4
      INTEGER BASE, HITS, I, IB5(LNBUF/LNINT), J, K, L, LP, NCLAS,
     +        NL, NX, NY, OFORM, SIZCLA, STEP
      EQUIVALENCE (RB5(1), IB5(1))
C
C Phase 1: Check options and keys, class limits. Define sub region
C limits and sampling interval across windowed picture.
C
      IF (TSTSRG(1, LP1)) GOTO 120
      IF (.NOT.SMGL1) THEN
        SMGI1 = 1
        SMGI2 = 1
        SMGI3 = 1
        SMGI4 = NCOLS(LP1)
        SMGI5 = NROWS(LP1)
        SMGI6 = NLAYS(LP1)
        SMGI7 = SMGI4
        SMGI8 = SMGI5
        SMGI9 = SMGI6
      ENDIF
      STEP = IVAL(NSTRID)
      IF (STEP .LT. 1) THEN
        ERROR = 3
        IDERR = NSTRID
        GOTO 120
      ENDIF
      NX = MAX(1, (SMGI7 / STEP))
      NY = MAX(1, (SMGI8 / STEP))
      NL = NCOLS(LP2)
      NCLAS = NLAYS(LP2)
C
C Check number of layers in input picture matches the width of the
C covariance information
C
      IF (NL .NE. SMGI9) THEN
        ERROR = EBASRS + 0
        GOTO 120
      ENDIF
C
C The 4 in the following expression is for the mean, standard deviation
C and range of each layer.
C
      SIZCLA = (NL + 4) * NL
      IF ((NCLAS * SIZCLA) .GT. (LNBUF / LNREA4)) THEN
        ERROR = EBASRS + 1
        GOTO 120
      ENDIF
      MAXLYR = (I44 * LNBUF / LNREA4) / NCOLS(LP1) 
      IF (SMGI9 .GT. MAXLYR) THEN 
        ERROR = EBASRS + 2
        GOTO 120
      ELSE IF (CLASSN(LP2) .NE. NCLUND) THEN
        ERROR = 6
        IDERR = IVALPN(NWITH)
        GOTO 120
      ENDIF
C
C Phase 2: Open output picture, passing picture title across. Note that
C is opened as a byte class picture which means that a maximum of 255
C classes are possible (0 being reserved for unclassified).
C
      LP = LP1
      OFORM = SEMFRM(NFMBYT)
      IF (SEMOPN(2, IVALPN(NTO), NX, NY, 1, NCLIMA, OFORM, LP)) GOTO 120
C
C Phase 3: Load class information (covariance, mean, sd, minima,
C maxima)
C
      IX0 = LNGONE
      DO 20 I = 1, NCLAS
        DO 10 J = 1, (NL + 4)
          IF (SEMROW(1, RB6(IX0), NFMFP, J, I, LP2)) GOTO 120
          IX0 = IX0 + (NL)
   10   CONTINUE
   20 CONTINUE
C
C Phase 4: Check for class overlap
C
      HITS = 0
      DO 40 I = 1, (NCLAS - 1)
        IX0 = ((I - 1) * SIZCLA + 1)
        DO 30 J = (I + 1), NCLAS
          IX1 = ((J - 1) * SIZCLA + 1)
          IF (CLSOVP(RB6(IX0), RB6(IX1), NL, NL + 4)) HITS = HITS + 1
   30   CONTINUE
   40 CONTINUE
      IF (HITS .NE. 0) THEN
        IF (SEMDIA('Classes overlap: multiple classification possible',
     +     NDIWAR)) GOTO 120
      ENDIF
C
C Phase 5: Perform classification, first read in layer data then find
C the first class which matches the data
C
      BASE = SMGI2
      BASE4 = (NL + 2) * (NL) + LNGONE
      INCR = (NL)
      STEPS4 = (NCOLS(LP1))
      DO 110 I = 1, NY
        IX0 = LNGONE
        DO 50 J = SMGI3, SMGI6
          IF (SEMROW(1, RB1(IX0), NFMFP, BASE, J, LP1)) GOTO 120
          IX0 = IX0 + STEPS4
   50   CONTINUE
        IX0 = (SMGI1)
        DO 100 J = 1, NX
C
C Initially assume that the data are unclassified
C
          IB5(J) = 0
          IX1 = BASE4
C
C Scan each classification region for a match ...
C
          DO 80 K = 1, NCLAS
            IXA = IX1
            IXB = IX0
C
C ... which consists of spectral bands
C
            DO 60 L = 1, NL
              IF ((RB1(IXB) .LT. RB6(IXA)) .OR.
     +            (RB1(IXB) .GT. RB6(IXA + INCR))) GOTO 70
              IXA = IXA + LNGONE
              IXB = IXB + STEPS4
   60       CONTINUE
C
C If we get here then the pixel was matched to a class
C
            IB5(J) = K
            GOTO 90
C
C and here if we fail: move to next class
C
   70       IX1 = IX1 + (SIZCLA)
   80     CONTINUE
   90     IX0 = IX0 + (STEP)
  100   CONTINUE
        IF (SEMROW(2, IB5, NFMINT, I, 1, LP)) GOTO 120
        BASE = BASE + STEP
  110 CONTINUE
  120 RETURN
C
C Copyright (C) 1988,1989: Synoptics Ltd, All Rights Reserved
C
      END
C Semper 6 subsidiary module: CLSOVP
C---------------------------------------------------------------------
C
C CLSOVP:
C
C Check for overlap between the ranges of classes. This is determined
C by checking to see if overlap occurs between any range.
C
C Arguments:
C   PAR1         Details of first class (covariance, mean, sd, range)
C   PAR2         Details of second class
C   NL           Number of rows
C   NM           Number of columns
C
C Returns:
C   .TRUE.       Overlap occurs between class ranges
C   .FALSE.      Classes do not overlap
C
      LOGICAL FUNCTION CLSOVP(PAR1, PAR2, NL, NM)
C     ===========================================
C
      INTEGER CMAX, CMIN, I, NL, NM
      REAL    PAR1(NL, NM), PAR2(NL, NM)
C
      CMIN = NL + 3
      CMAX = CMIN + 1
C
C Assume prior result
C
      CLSOVP = .FALSE.
      DO 10 I = 1, NL
        IF (PAR1(I, CMIN) .GT. PAR2(I, CMAX)) GOTO 20
        IF (PAR2(I, CMIN) .GT. PAR1(I, CMAX)) GOTO 20
   10 CONTINUE
C
C If it gets here then an overlap occurred between all ranges and
C hence two classes intersect.
C
      CLSOVP = .TRUE.
   20 RETURN
C
C Copyright (C) 1988,1989: Synoptics Ltd, All Rights Reserved
C
      END
