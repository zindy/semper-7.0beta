C Semper 6 module: MINDIS
C----------------------------------------------------------------------
C
C MINDIS:
C
C Perform a minimum distance to means classification with, optionally a
C threshold applied to each class. (Should this be to each spectral
C band which is present in the picture?). If only one threshold is
C specified (THR) it is applied to all classes.
C
C Arguments:
C   None.
C
      SUBROUTINE MINDIS
C     =================
C
      INTEGER*4 LNGONE
      INTEGER NSTRID, NTO, NWITH
      PARAMETER (LNGONE = 1, NWITH = -5181, NSTRID = 31218, NTO = -601)
C
      INCLUDE 'COMMON'
C
      INTEGER*4 LNREA4,I44
      PARAMETER (LNREA4=LNREAL,I44=4)
C
      LOGICAL HASTHR, SEMOPN, SEMROW, TSTSRG, VARSET
      INTEGER*4 IX0, LST, MAXLYR
      REAL    TMP, VAL
      INTEGER BASE, I, IB5(LNBUF / LNINT), IVAL, IVALPN, J, LP, NCLAS,
     +        NL, NTHRS(9), NX, NY, OFORM, SEMFRM, SIZCLA, STEP
      EXTERNAL IVAL, IVALPN, SEMFRM, SEMOPN, SEMROW, TSTSRG, VARSET
      EQUIVALENCE (RB5(1), IB5(1))
      DATA    NTHRS/-339, -353, -354, -355, -356,
     +              -357, -358, -359, -360/
C
C Phase 1: Check options and keys, class limits. Define sub region
C limits and sampling interval across windowed picture.
C
      IF (TSTSRG(1, LP1)) GOTO 90
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
        GOTO 90
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
        GOTO 90
      ENDIF
C
C The 4 in the following expression is for the mean, standard
C deviation and range of each layer.
C
      SIZCLA = (NL + 4) * NL
      IF (LONGF(NCLAS * SIZCLA) .GT. (LNBUF / LNREA4)) THEN
        ERROR = EBASRS + 1
        GOTO 90
      ENDIF
      MAXLYR = (I44 * LNBUF / LNREA4) / LONGF(NCOLS(LP1))
      IF (NLAYS(LP1) .GT. MAXLYR) THEN
        ERROR = EBASRS + 2
        GOTO 90
      ELSE IF (CLASSN(LP2) .NE. NCLUND) THEN
        ERROR = 6
        IDERR = IVALPN(NWITH)
        GOTO 90
      ENDIF
C
C Check to see if thresholds are to be applied, and that they are
C positive.
C
      HASTHR = VARSET(NTHRS(1))
      IF (HASTHR) THEN
        I = 1
   10   IF (.NOT.VARSET(NTHRS(I))) GOTO 20
          TMP = VAL(NTHRS(I))
          IF (TMP .LT. 0.0) THEN
            ERROR = EBASRS + 8
            GOTO 90
          ENDIF
          RB5(I) = TMP
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
          GOTO 90
        ENDIF
      ENDIF
C
C Phase 2: Open output picture, passing picture title across. Note
C that is opened as a byte class picture which means that a maximum
C of 255 classes are possible (0 being reserved for unclassified).
C
      LP = LP1
      OFORM = SEMFRM(NFMBYT)
      IF (SEMOPN(2, IVALPN(NTO), NX, NY, 1, NCLIMA, OFORM, LP)) GOTO 90
C
C Phase 3: Load class information (covariance, mean, sd, minima,
C maxima)
C
      IX0 = LNGONE
      DO 50 I = 1, NCLAS
        DO 40 J = 1, (NL + 4)
          IF (SEMROW(1, RB6(IX0), NFMFP, J, I, LP2)) GOTO 90
          IX0 = IX0 + LONGF(NL)
   40   CONTINUE
   50 CONTINUE
C
C Phase 4: Precalculate m ** 2, 2 * m and threshold limits
C
      IX0 = LNGONE
      DO 60 I = 1, NCLAS
        IF (HASTHR) THEN
          CALL MINPRP(RB6(IX0), NL, NL + 4, RB5(I))
        ELSE
          CALL MINPRP(RB6(IX0), NL, NL + 4, 0.0)
        ENDIF
        IX0 = IX0 + LONGF(SIZCLA)
   60 CONTINUE
C
C Phase 5: Perform classification, first read in layer data
C
      BASE = SMGI2
      LST = LONGF(NCOLS(LP1))
      DO 80 I = 1, NY
        IX0 = LNGONE
        DO 70 J = 1, NL
          IF (SEMROW(1, RB1(IX0), NFMFP, BASE, J, LP1)) GOTO 90
          IX0 = IX0 + LST
   70   CONTINUE
C
C Now classify the elements present in the row
C
        IX0 = LONGF(SMGI1)
        CALL MINCLA(RB6(1), NL, NL + 4, NCLAS, HASTHR, LST, RB1(IX0),
     +              STEP, NX, (NL * (NL + 4) * NCLAS))
        IF (SEMROW(2, IB5, NFMINT, I, 1, LP)) GOTO 90
        BASE = BASE + STEP
   80 CONTINUE
   90 RETURN
C
C Copyright (C) 1988,1989: Synoptics Ltd, All Rights Reserved
C
      END
C Semper 6 subsidiary module: MINPRP
C---------------------------------------------------------------------
C
C MINPRP:
C
C Prepare the class statistics (we could do this round the inner loop
C of classification but why waste valuable time).
C
C Arguments:
C   ARR       Array containing class statistics
C   NL        Number of rows in array
C   NM        Number of columns in array
C   THRS      Threshold to apply to class
C
      SUBROUTINE MINPRP(ARR, NL, NM, THRS)
C     ====================================
C
      INTEGER CMEAN, CMIN, CMAX, CSD, I, NL, NM
      REAL    ARR(NL, NM), THRS
C
      CMEAN = NL + 1
      CSD = CMEAN + 1
      CMIN = CSD + 1
      CMAX = CMIN + 1
      DO 10 I = 1, NL
        ARR(I, CMIN) = ARR(I, CMEAN) - ARR(I, CSD) * THRS
        ARR(I, CMAX) = ARR(I, CMEAN) + ARR(I, CSD) * THRS
        ARR(I, CSD) = ARR(I, CMEAN) * 2.0
        ARR(I, CMEAN) = ARR(I, CMEAN) ** 2.0
   10 CONTINUE
      RETURN
      END
C Semper 6 subsidiary module: MINCLA
C--------------------------------------------------------------------
C
C MINCLA:
C
C Find classification of a row of pixels
C
C Arguments:
C   CLASES     Classification statistics
C   NL         Array sizes
C   NM
C   NN
C   TH         Whether thresholds are to be applied
C   ST         Step along data array between layers
C   RB         Data array
C   STEP       Step between raw data points
C   N          Number of points to classify
C   SZ         Size of clas array
C
C Returns:
C   Class of pixel
C
      SUBROUTINE MINCLA(CLASES, NL, NM, NN, TH, ST, RB, STEP, N, SZ)
C     ==============================================================
C
      INTEGER*4 LNGONE
      PARAMETER (LNGONE = 1)
C
      INTEGER*4 CBIX, CIX, IX, IX0, O1, O2, O3, O4, O5, ST
      INTEGER   H, I, J, NL, NM, NN, N, PX, STEP, SZ
      LOGICAL   TH
      REAL CLASES(SZ), D0, D1, RB(*), TMP
      INCLUDE   'COMMON'
      INTEGER   IB5(LNBUF / LNINT)
      EQUIVALENCE (RB5(1), IB5(1))
C
      O1 = LONGF(NL * NM)
      O2 = LONGF(NL * NL)
C      O3 = LONGF(1 * NL)
C      O4 = LONGF(2 * NL)
C      O5 = LONGF(3 * NL)
      O3 = LONGF(NL)
      O4 = O3 + O3
      O5 = O4 + O3
      IX0 = 1
      DO 40 H = 1, N
C
C Assume unclassified to start with, scan each available class
C
        PX = 0
        CBIX = O2 + LNGONE
        DO 30 I = 1, NN
          IX = IX0
          D0 = 0.0
          CIX = CBIX
C
C Find distance to mean, abandoning the comparison if the data point
C lies outside the threshold limits.
C
          DO 10 J = 1, NL
            TMP = RB(IX)
            IF (TH) THEN
               IF ((TMP .LT. CLASES(CIX + O4)) .OR.
     +             (TMP .GT. CLASES(CIX + O5))) GOTO 20
            ENDIF
            D0 = D0 - CLASES(CIX + O3) * TMP + CLASES(CIX)
            CIX = CIX + LNGONE
            IX = IX + ST
   10     CONTINUE
C
C If first class or less than previous set new
C
          IF ((PX .EQ. 0) .OR. (D0 .LT. D1)) THEN
            D1 = D0
            PX = I
          ENDIF
   20     CBIX = CBIX + O1
   30   CONTINUE
        IB5(H) = PX
        IX0 = IX0 + LONGF(STEP)
   40 CONTINUE
      RETURN
      END
