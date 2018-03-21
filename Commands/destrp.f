C Semper 6 module: DESTRP
C-----------------------------------------------------------------
C
C DESTRP:
C
C Destripe an image, using either the line means or the line means
C and standard deviations, to account for variations in sensor
C response. This is typically used in LANDSAT images where six sensors
C are used at once and the image has to be corrected to account for
C their variations. Uses all row buffers.
C
C Arguments:
C   None.
C
      SUBROUTINE DESTRP
C     =================
C
      INTEGER*4 LNGZER, LNGONE
      INTEGER NLAY, NLINE, NMEA, NMODE, NSD, NTO
      PARAMETER (NLAY = 19265, NLINE = 19574, NMEA = 29564,
     +           NMODE = 21404, NSD = 29325, NTO = -601,
     +           LNGONE = 1, LNGZER = 0)
C
      INCLUDE 'COMMON'
      INTEGER*4 NPX(LNBUF / LNINT4)
      INTEGER I, J, L, LINE, LP, OFORM, REFLIN
      LOGICAL USESD
      REAL A, B, REFMN, REFSD, SUMX(LNBUF / LNREAL),
     +     SUMX2(LNBUF / LNREAL), SX, SX2, TMP
C
      LOGICAL OPT, SEMOPN, SEMROW, TSTSRG, VARSET
      INTEGER IVAL, IVALPN, SEMFRM
      REAL    VAL
      EXTERNAL IVAL, IVALPN, OPT, SEMFRM, SEMOPN, SEMROW, TSTSRG,
     +         VARSET, VAL
      EQUIVALENCE (RB4(1), NPX(1)), (RB5(1), SUMX(1)),
     +            (RB6(1), SUMX2(1))
C
C First see how layers are to be processed, default is all
C
      IF (TSTSRG(1, LP1)) RETURN
      IF (VARSET(NLAY)) THEN
        SMGI3 = IVAL(NLAY)
        SMGI6 = SMGI3
        IF ((SMGI3 .LT. 1) .OR. (SMGI3 .GT. NLAYS(LP1))) THEN
          ERROR = 3
          IDERR = NLAY
          RETURN
        ENDIF
      ELSE
        SMGI3 = 1
        SMGI6 = NLAYS(LP1)
      ENDIF
      SMGI9 = SMGI6 - SMGI3 + 1
C
C Now see how many sensors (lines) there are.
C
      LINE = IVAL(NLINE)
      IF ((LINE .LT. 2) .OR. (LINE .GT. NROWS(LP1))) THEN
        ERROR = 3
        IDERR = NLINE
        RETURN
      ENDIF
C
C Open output picture, copying source title across and using the same
C form and class as the original.
C
      LP = LP1
      OFORM = SEMFRM(FORMN(LP1))
      IF (SEMOPN(2, IVALPN(NTO), SMGI7, SMGI8, SMGI9, CLASSN(LP1),
     +           OFORM, LP)) RETURN
C
C Now determine mode to use when correcting and process each layer
C
      USESD = OPT(NMODE)
C
C Start processing each layer
C
      DO 100 L = SMGI3, SMGI6
C
C 1. Calculate line mean and standard deviations
C
        DO 10 I = 1, LINE
          SUMX(I) = 0.0
          SUMX2(I) = 0.0
          NPX(I) = LNGZER
   10   CONTINUE
        REFLIN = 1
        DO 30 I = SMGI2, SMGI5
          IF (SEMROW(1, RB1, NFMFP, I, L, LP1)) RETURN
          SX = 0.0
          SX2 = 0.0
          DO 20 J = SMGI1, SMGI4
            TMP = RB1(J)
            SX = SX + TMP
            SX2 = SX2 + TMP * TMP
   20     CONTINUE
          NPX(REFLIN) = NPX(REFLIN) + (SMGI7)
          SUMX(REFLIN) = SUMX(REFLIN) + SX
          SUMX2(REFLIN) = SUMX2(REFLIN) + SX2
          REFLIN = REFLIN + 1
          IF (REFLIN .GT. LINE) REFLIN = 1
   30   CONTINUE
C
C Calculate mean and standard deviation. Notice that we are using the
C same storage.
C
        DO 40 I = 1, LINE
          TMP = SUMX(I)
          SUMX(I) = TMP / REAL(NPX(I))
          TMP = (SUMX2(I) - SUMX(I) * TMP) / REAL(NPX(I) - LNGONE)
          SUMX2(I) = SQRT(TMP)
   40   CONTINUE
C
C 2. Calculate correction coefficients. Here we are (effectively)
C forming a line equation y = ax + b where x is the original,
C uncorrected value.
C
        IF (USESD) THEN
          IF (VARSET(NSD)) THEN
            REFMN = VAL(NMEA)
            REFSD = VAL(NSD)
          ELSE
            REFMN = SUMX(1)
            REFSD = SUMX2(1)
            DO 50 I = 2, LINE
              IF (SUMX2(I) .GT. REFSD) THEN
                REFMN = SUMX(I)
                REFSD = SUMX2(I)
              ENDIF
   50       CONTINUE
          ENDIF
        ELSE
          REFSD = 1.0
          IF (VARSET(NMEA)) THEN
            REFMN = VAL(NMEA)
          ELSE
            REFMN = SUMX(1)
            DO 60 I = 2, LINE
              REFMN = MIN(REFMN, SUMX(I))
   60       CONTINUE
          ENDIF
        ENDIF
        DO 70 I = 1, LINE
          IF (SUMX2(I) .EQ. 0.0) SUMX2(I) = REFSD
          IF (USESD) THEN
            A = REFSD / SUMX2(I)
          ELSE
            A = 1.0
          ENDIF
          B = REFMN - A * SUMX(I)
          SUMX2(I) = A
          SUMX(I) = B
   70   CONTINUE
C
C 3. Correct output picture. Slightly inefficient code when correcting
C by the mean alone.
C
        REFLIN = 1
        DO 90 I = 1, SMGI8
          IF (SEMROW(1, RB1, NFMFP, SMGI2 + I - 1, L, LP1)) RETURN
          A = SUMX2(REFLIN)
          B = SUMX(REFLIN)
          DO 80 J = 1, SMGI7
            RB2(J) = RB1(SMGI1 + J - 1) * A + B
   80     CONTINUE
          IF (SEMROW(2, RB2, NFMFP, I, L - SMGI3 + 1, LP)) RETURN
          REFLIN = REFLIN + 1
          IF (REFLIN .GT. LINE) REFLIN = 1
   90   CONTINUE
  100 CONTINUE
      RETURN
      END
C
C Copyright (C) 1988,1989: Synoptics Ltd, All Rights Reserved
C
