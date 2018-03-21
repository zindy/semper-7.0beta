C Semper 6 sub module: CHISQ
C---------------------------------------------------------------------
C
C CHISQ:
C
C Evaluate chi-squared for a given probability and degrees of freedom
C
C Arguments:
C   IDOF      Number of degrees of freedom
C   CHI       Probability to evaluate
C
C Returns:
C   Chi-squared
C
      REAL FUNCTION CHISQ(IDOF, CHI)
C     ==============================
C
      INTEGER IDOF
      REAL    CHI, DEL, DOF, GAM, S, X0
C
C Machine accuracy
C
      REAL EPS
      PARAMETER (EPS = 1.0E-5)
C
      DOF = REAL(IDOF)
      S = 0.0
      IF (DOF .EQ. 0.0 .AND. CHI .EQ. 0.0) GOTO 20
      S = 1.0
      DEL = 1.0
C
C Estimate value
C
   10 X0 = GAM(DOF / 2.0, S)
C
C Too low: increase
C
      IF (X0 .LT. CHI) THEN
        DEL = DEL * 2.0
        S = S + DEL
C
C Too high: decrease
C
      ELSE
        DEL = DEL / 2.0
        S = S - DEL
      ENDIF
C
C Accurate enough?
C
      IF (ABS(X0 - CHI) .GT. EPS * CHI) GOTO 10
      S = S * 2.0
   20 CHISQ = S
      RETURN
C
C Copyright (C) 1988,1989: Synoptics Ltd, All Rights Reserved
C
      END
C Semper 6 sub module: GAM
C---------------------------------------------------------------------
C
C GAM:
C
C Incomplete Gamma function
C
C Arguments:
C  A        (Order)
C  X        (Power)
C
C Returns:
C  Evaluated value of incomplete gamma function
C
      REAL FUNCTION GAM(A, X)
C     =======================
C
      REAL A, X
C
      REAL GCFRA, GSERIS
      EXTERNAL GCFRA, GSERIS
C
      GAM = 0.0
      IF ((X .LT. 0.0) .OR. (A .LE. 0.0)) RETURN
      IF (X .LT. (A + 1.0)) THEN
        GAM = GSERIS(A, X)
      ELSE
        GAM = 1.0 - GCFRA(A, X)
      ENDIF
      RETURN
C
C Copyright (C) 1988,1989: Synoptics Ltd, All Rights Reserved
C
      END
C Semper 6 sub module: GSERIS
C---------------------------------------------------------------------
C
C GSERIS:
C
C Evaluate the incomplete gamma function for the given arguments,
C X should be .lt. (a + 1.0) otherwise the evaluation is likely to
C blow up numerically. (See Knuth Vol 1 pp 113-119 for algorithm).
C
C Arguments:
C   A        (Order)
C   X        (Power)
C
C Returns:
C   Evaluated incomplete gamma function
C
      REAL FUNCTION GSERIS(A, X)
C     ==========================
C
      INTEGER I
      REAL    A, CF, TERM, TOTAL, X
C
      REAL GAMMLN
      EXTERNAL GAMMLN
C
      INTEGER GOES
      REAL    TINY
      PARAMETER (GOES = 100, TINY = 1.0E-5)
C
      IF (X .LE. 0.0) THEN
        GSERIS = 0.0
        RETURN
      ENDIF
      CF = A
      TERM = 1.0 / A
      TOTAL = 0.0
      DO 10 I = 1, GOES
        TOTAL = TOTAL + TERM
        CF = CF + 1.0
        TERM = TERM * X / CF
        IF (ABS(TERM) .LT. ABS(TOTAL) * TINY) GOTO 20
   10 CONTINUE
   20 GSERIS = TOTAL * EXP(-X + A * LOG(X) - GAMMLN(A))
      RETURN
C
C Copyright (C) 1988,1989: Synoptics Ltd, All Rights Reserved
C
      END
C Semper 6 sub module: GCFRA
C---------------------------------------------------------------------
C
C GCFRA:
C
C Calculate incomplete gamma function by using continued fractions,
C rather than a series (GSERIS). More stable for X .gt (A + 1.0)
C
C Arguments:
C   A   (Order)
C   X   (Power)
C
C Returns:
C   Evaluated incomplete gamma function
C
      REAL FUNCTION GCFRA(A, X)
C     =========================
C
      INTEGER I
      REAL A, A0, A1, AN, ANA, ANF, B0, B1, CF, GLAST, GTHIS, X
C
      INTEGER GOES
      REAL TINY
      PARAMETER (GOES = 100, TINY = 1.E-5)
C
      REAL GAMMLN
      EXTERNAL GAMMLN
C
      GLAST = 0.0
      A0 = 1.0
      A1 = X
      B0 = 0.0
      B1 = 1.0
      CF = 1.0
      DO 10 I = 1, GOES
        AN = REAL(I)
        ANA = AN - A
        A0 = (A1 + A0 * ANA) * CF
        B0 = (B1 + B0 * ANA) * CF
        ANF = AN * CF
        A1 = X * A0 + ANF * A1
        B1 = X * B0 + ANF * B1
        IF (A1 .NE. 0.) THEN
          CF = 1.0 / A1
          GTHIS = B1 * CF
          IF (ABS((GTHIS - GLAST) / GTHIS) .LT. TINY) GOTO 20
          GLAST = GTHIS
        ENDIF
   10 CONTINUE
   20 GCFRA = GTHIS * EXP(-X + A * ALOG(X) - GAMMLN(A))
      RETURN
C
C Copyright (C) 1988,1989: Synoptics Ltd, All Rights Reserved
C
      END
C Semper 6 sub module: GAMMLN
C---------------------------------------------------------------------
C
C GAMMLN:
C
C Calculate the log of the gamma function
C
C Arguments:
C   X         Source (must be greater than 0.0)
C
C Returns:
C   log(gamma(x))
C
      REAL FUNCTION GAMMLN(X)
C     =======================
C
      REAL A0, A1, A2, A3, A4, A5
      PARAMETER (A0 = 76.1809173, A1 = -86.50532033,
     +           A2 = 24.0140982, A3 = -1.231739516,
     +           A4 = 0.12085800E-2, A5 = -.536382E-5)
      REAL SQR2PI
      PARAMETER (SQR2PI = 2.50662827)
      REAL S, SUM, X, Z
C
      Z = X - 1.0
      S = Z + 5.5
      S = (Z + 0.5) * LOG(S) - S
      SUM = 1.0 + A0 / (Z + 1) + A1 / (Z + 2) + A2 / (Z + 3) +
     +            A3 / (Z + 4) + A4 / (Z + 5)
      GAMMLN = S + LOG(SUM * SQR2PI)
      RETURN
      END
