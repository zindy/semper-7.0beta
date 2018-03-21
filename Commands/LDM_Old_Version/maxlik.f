C Semper 6 module: MAXLIK
C---------------------------------------------------------------------
C
C MAXLIK:
C
C Perform a maximum likelihood classification with, optionally a
C probability and threshold applied to each class.
C
C Arguments:
C   None.
C
      SUBROUTINE MAXLIK
C     =================
C
      INTEGER*4 LNGONE
      INTEGER NSTRID, NTO, NWITH
      PARAMETER (LNGONE = 1, NWITH= -5181, NSTRID = 31218, NTO = -601)
      INCLUDE 'COMMON'
C
      INTEGER*4 LNREA4,I44,I432
      PARAMETER (LNREA4=LNREAL,I44=4,I432=32)
C
      LOGICAL HASPRB, HASTHR, HODPRP, SEMOPN, SEMROW, TSTSRG, VARSET
      INTEGER*4 IX0, LST, MAXLYR
      INTEGER BASE, I, IB5(LNBUF / LNINT), IVAL, IVALPN, J, LP, NCLAS,
     +        NL, NPROB(9), NTHRS(9), NX, NY, OFORM, SEMFRM, SIZCLA,
     +        STEP
      REAL TMP, SUMPRO, VAL
      EXTERNAL HODPRP, IVAL, IVALPN, SEMFRM, SEMOPN, SEMROW, TSTSRG,
     +         VAL, VARSET
      EQUIVALENCE (RB5(1), IB5(1))
      DATA    NTHRS/-339, -353, -354, -355, -356,
     +              -357, -358, -359, -360/,
     +        NPROB/26335, 26352, 26353, 26354, 26355,
     +              26356, 26357, 26358, 26359/
C
C Phase 1: Check options and keys, class limits. Define sub region
C limits and sampling interval across windowed picture.
C
      IF (TSTSRG(1, LP1)) GOTO 110
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
        GOTO 110
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
        GOTO 110
      ENDIF
C
C The 4 in the following expression is for the mean, standard
C deviation and range of each layer.
C
      SIZCLA = (NL + 4) * NL
      IF (LONGF(NCLAS * SIZCLA) .GT. (LNBUF / LNREA4)) THEN
        ERROR = EBASRS + 1
        GOTO 110
      ENDIF
C
C *****                  *****                              *****
C The maximum number of layers being 32 is the current size of a
C scratch array in MAXHOD. If you alter this then do so in MAXHOD!
C *****                  *****                              *****
C
      MAXLYR = MIN((I44 * LNBUF / LNREA4) /
     +             LONGF(NCOLS(LP1)), I432)
      IF (NLAYS(LP1) .GT. SHORTF(MAXLYR)) THEN
        ERROR = EBASRS + 2
        GOTO 110
      ELSE IF (CLASSN(LP2) .NE. NCLUND) THEN
        ERROR = 6
        IDERR = IVALPN(NWITH)
        GOTO 110
      ENDIF
C
C Check to see if thresholds are to be applied and that they are
C in the range 0.0 .. 100.0 (what use is a 0.0% threshold?)
C
      HASTHR = VARSET(NTHRS(1))
      IF (HASTHR) THEN
        I = 1
   10   IF (.NOT.VARSET(NTHRS(I))) GOTO 20
          TMP = VAL(NTHRS(I))
          IF ((TMP .LT. 0.0) .OR. (TMP .GT. 100.0)) THEN
            ERROR = EBASRS + 3
            GOTO 110
          ENDIF
          RB5(I) = TMP / 100.0
          I = I + 1
        IF (I .LT. 10) GOTO 10
C
C If only one threshold then apply to all classification regions
C
   20   I = I - 1
        IF (I .EQ. 1) THEN
          DO 30 J = 2, NCLAS
            RB5(J) = RB5(1)
   30     CONTINUE
        ELSE IF (I .NE. NCLAS) THEN
          ERROR = EBASRS + 4
          GOTO 110
        ENDIF
      ENDIF
C
C Check to see if probabilities are to be used, and that they are in
C range and that the sum is less than 1.0.
C
      HASPRB = VARSET(NPROB(1))
      IF (HASPRB) THEN
        SUMPRO = 0.0
        I = 1
   40   IF (.NOT.VARSET(NPROB(I))) GOTO 50
          TMP = VAL(NPROB(I))
          IF ((TMP .LE. 0.0) .OR. (TMP .GT. 1.0)) THEN
            ERROR = EBASRS + 5
            GOTO 110
          ENDIF
          RB5(NCLAS + I) = TMP
          SUMPRO = SUMPRO + TMP
          I = I + 1
        IF (I .LT. 10) GOTO 40
   50   I = I - 1
        IF (SUMPRO .GT. 1.0) THEN
          ERROR = EBASRS + 6
          GOTO 110
        ENDIF
        IF (I .NE. NCLAS) THEN
          ERROR = EBASRS + 15
          GOTO 110
        ENDIF
      ENDIF
C
C Phase 2: Open output picture, passing picture title across. Note that
C is opened as a byte class picture which means that a maximum of 255
C classes are possible (0 being reserved for unclassified).
C
      LP = LP1
      OFORM = SEMFRM(NFMBYT)
      IF (SEMOPN(2, IVALPN(NTO), NX, NY, 1, NCLIMA, OFORM, LP)) GOTO 110
C
C Phase 3: Load class information (covariance, mean, sd, minima,
C maxima).
C
      IX0 = LNGONE
      DO 70 I = 1, NCLAS
        DO 60 J = 1, (NL + 4)
          IF (SEMROW(1, RB6(IX0), NFMFP, J, I, LP2)) GOTO 110
          IX0 = IX0 + LONGF(NL)
   60   CONTINUE
   70 CONTINUE
C
C Phase 4: Calculate matrix inverse, determinant and threshold levels
C if used for each class.
C
      IX0 = LNGONE
      DO 80 I = 1, NCLAS
        IF (HODPRP(RB6(IX0), NL, NL + 4, RB5(I), HASTHR,
     +             RB5(I + NCLAS), HASPRB, RB1(1))) GOTO 110
        IX0 = IX0 + LONGF(SIZCLA)
   80 CONTINUE
C
C Phase 5: Perform classification, first read in layer data
C
      BASE = SMGI2
      LST = LONGF(NCOLS(LP1))
      DO 100 I = 1, NY
        IX0 = LNGONE
        DO 90 J = 1, NL
          IF (SEMROW(1, RB1(IX0), NFMFP, BASE, J, LP1)) GOTO 110
          IX0 = IX0 + LST
   90   CONTINUE
C
C Now classify the elements present in the row
C
        IX0 = LONGF(SMGI1)
        CALL MAXHOD(RB6(1), NL, NL + 4, NCLAS, HASTHR, LST, RB1(IX0),
     +              STEP, NX, NL * (NL + 4) * NCLAS)
        IF (SEMROW(2, IB5, NFMINT, I, 1, LP)) GOTO 110
        BASE = BASE + STEP
  100 CONTINUE
  110 RETURN
C
C Copyright (C) 1988,1989: Synoptics Ltd, All Rights Reserved
C
      END
C Semper 6 sub module: HODPRP
C---------------------------------------------------------------------
C
C HODPRP:
C
C Prepare the data for maximum likelihood classification. This involves
C finding the determininant of the covariance matrix, inverting it, and
C if thresholds or probabilities are to be applied calculating the
C gamma function. Note the matrix row/column ordering.
C
C Arguments:
C   CLAS      Covariance matrix with mean and standard deviation too
C   NL        Dimensions of CLAS()
C   NM
C   THRESH    Threshold value (if used)
C   USETHR    Flag to indicate if thresholds applied
C   PROB      Probability value (if used)
C   USEPRO    Flag to indicate if probabilities applied
C   SCRACH    Scratch workspace
C
C Returns:
C   .TRUE.    Failed (zero determinant)
C   .FALSE.   All done on this class
C
      LOGICAL FUNCTION HODPRP(CLAS, NL, NM, THRESH, USETHR, PROB,
     +                        USEPRO, SCRACH)
C     ===========================================================
C
      INTEGER I, J, NL, NM
      REAL CLAS(NL, NM), D, PROB, SCRACH(NL, NM), THRESH
      LOGICAL USETHR, USEPRO
      REAL    CHISQ, DETER
      EXTERNAL CHISQ, DETER
      INCLUDE 'COMMON'
C
C Calculate the determinant of the covariance matrix: this destroys
C the input matrix. Note that absolute value of determinant taken ...
C is this correct? (Magnitude of determinant decides influence).
C
      CALL CROUT(CLAS, NL)
      D = ABS(DETER(CLAS, NL))
C
C If determinant is zero then no inverse exists. Really the following
C piece of code should check if the determinant is close to zero not
C just for being zero (which, given floating point numbers, may be
C unlikely).
C
      IF (D .EQ. 0.0) THEN
        ERROR = EBASRS + 16
        HODPRP = .TRUE.
        RETURN
      ENDIF
C
C Now invert matrix and copy back the result into the same space.
C
      CALL INVERT(CLAS, SCRACH, NL)
      DO 20 I = 1, NL
        DO 10 J = 1, NL
          CLAS(I, J) = SCRACH(I, J) * 0.5
   10   CONTINUE
   20 CONTINUE
      CLAS(1, NL + 2) = -LOG(D) * 0.5
      IF (USEPRO) CLAS(1, NL + 2) = CLAS(1, NL + 2) + LOG(PROB)
      IF (USETHR) THEN
        IF (USEPRO) THEN
          CLAS(1, NL + 3) = -0.5 * (CHISQ(NL, THRESH) + LOG(D)) +
     +                      LOG(PROB)
        ELSE
          CLAS(1, NL + 3) = -0.5 * (CHISQ(NL, THRESH) + LOG(D))
        ENDIF
      ENDIF
      HODPRP = .FALSE.
      RETURN
      END
C Semper 6 sub module: MAXHOD
C---------------------------------------------------------------------
C
C MAXHOD:
C
C Classify the a row of pixels given according to the class information
C If necessary apply a threshold value to the class. This will be slow.
C
C *** Note warnings in MAXLIK about the size of SCR() ***
C *** Also note the order of data is very important: this code heavily
C     optimised for speed (or so we think) ***
C
C Arguments:
C   CLAS     Class information
C   NL       Theoretical dimensions of CLAS()
C   NM
C   NN
C   FLAGTH   TRUE if thresholds are to be applied
C   STRIDE   Displacement between layers of a data point
C   RB()     Data array containing point to be classified
C   STEP     Step between raw data points
C   N        Number of elements to classify
C   SIZ      Total number of elements in CLAS()
C
      SUBROUTINE MAXHOD(CLAS, NL, NM, NN, FLAGTH, STRIDE, RB, STEP, N,
     +                  SIZE)
C     ================================================================
C
      INTEGER*4 LNGONE
      PARAMETER (LNGONE = 1)
C
      INTEGER H, I, J, K, NL, NM, NN, N, PX, SIZE, STEP
      INTEGER*4 CBIX, CIX, IX, IX0, O1, O2, O3, O4, STRIDE
      LOGICAL FLAGTH
      REAL    CLAS(SIZE), LAST, RB(*), SCR(32), TMP, TMP2
      INCLUDE 'COMMON'
      INTEGER IB5(LNBUF / LNINT)
      EQUIVALENCE (RB5(1), IB5(1))
C
      IX0 = LNGONE
C      O1 = LONGF(1 * NL)
C      O2 = LONGF(2 * NL)
      O1 = LONGF(NL)
      O2 = O1 + O1
      O3 = LONGF(NL * NL)
      O4 = LONGF(NL * NM)
      DO 60 H = 1, N
C
C Assume prior result, that the pixel is unclassified
C
        PX = 0
        CBIX = LNGONE
C
C For each possible class calculate the likelihood of this point
C
        DO 50 I = 1, NN
C
C Calculate point - mean for all layers, (NL + 1) has layer means
C
          IX = IX0
          CIX = CBIX + O3
          DO 10 J = 1, NL
            SCR(J) = RB(IX) - CLAS(CIX)
            CIX = CIX + LNGONE
            IX = IX + STRIDE
   10     CONTINUE
C
C                      t  -1                                 -1
C Now calculate (x - m)  E   (x - m), a single value, where E   is
C the inverse of the covariance matrix.
C
          TMP2 = 0.0
          CIX = CBIX
          DO 30 J = 1, NL
            TMP = 0.0
            DO 20 K = 1, NL
              TMP = TMP + SCR(K) * CLAS(CIX)
              CIX = CIX + LNGONE
   20       CONTINUE
            TMP2 = TMP2 + TMP * SCR(J)
   30     CONTINUE
C
C Finally add in (probability) and determinant to give likelihood.
C This has been previously calculated and stored
C
          TMP = CLAS(CIX + O1) - TMP2
C
C Threshold checks
C
          IF ((FLAGTH) .AND. (TMP .LT. CLAS(CIX + O2))) GOTO 40
C
C Now find if this is the most likely class
C
          IF ((PX .EQ. 0) .OR. (TMP .GT. LAST)) THEN
            PX = I
            LAST = TMP
          ENDIF
   40     CBIX = CBIX + O4
   50   CONTINUE
        IB5(H) = PX
        IX0 = IX0 + LONGF(STEP)
   60 CONTINUE
      RETURN
      END
