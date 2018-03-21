C Semper 6 sub module: FNDLSQ
C----------------------------------------------------------------------
C
C FNDLSQ:
C
C Finds the least squares solution to the supplied map and image co-
C ordinate systems. If necessary, calculated co-ordinates are compared
C with originals and deleted from the list (which should cause this
C routine to be called again).
C
C Arguments:
C   VALID     .TRUE. if the data point is valid
C   POWX      Powers of X for the polynomial
C   PWOY      Powers of Y for the polynomial
C   CIMA      Image co-ordinates (distorted)
C   CMAP      Map co-ordinates (true)
C   COEF      Evaluated cofficients
C   N         Number of coefficients and powers
C   NDAT      Number of data points (always greater than N)
C   SUM       Scratch array for matrix of summed values
C   INV       Scratch array for matrix inverse
C   RES       Calculated coordinate values
C   ERRMAX    Maximum squared error in image co-ordinate terms,
C             less than 0.0 if no check required.
C   SF        Scale factor of coefficients (prevent numerical o/f)
C
C Returns:
C   0         Everything calculated correctly, coefficients, etc.
C   1         Data items deleted from list (outside max error)
C   2         Insufficient points to solve or zero determinant
C
      INTEGER FUNCTION FNDLSQ(VALID, POWX, POWY, CIMA, CMAP, COEF,
     +                        N, NDAT, SUM, INV, RES, ERRMAX, SF)
C     ============================================================
C
      INTEGER I, J, K, N, NDAT, NEL, POWX(N), POWY(N), PX, PY
      REAL    CIMA(NDAT, 2), CMAP(NDAT, 2), COEF(N, 2), D, ERRMAX,
     +        INV(N, N), RES(NDAT, 2), SF, SUM(N, N), SX, SY, TMP
      LOGICAL CHKERR, VALID(N)
C
      REAL    DETER, EPOL
C
C Presuppose error result
C
      FNDLSQ = 2
      CHKERR = (ERRMAX .GT. 0.0)
C
C Check that there are sufficient data points in the set and determine
C scaling factor
C
      NEL = 0
      SF = 0.0
      DO 10 I = 1, NDAT
         IF (VALID(I)) THEN
            NEL = NEL + 1
            SF = MAX(ABS(CMAP(I, 1)), SF)
            SF = MAX(ABS(CMAP(I, 2)), SF)
         ENDIF
   10 CONTINUE
      IF (NEL .LT. (N + 1)) GOTO 110
C
C Determine coefficient scaling factor
C
      SF = 1.0 / SF
C
C Sum each row of data, raising to the power as required, the general
C form is:
C                  Nij    Mij
C     Sij = Sij + X    * Y
C
      DO 60 I = 1, N
         DO 30 J = 1, N
            PX = (POWX(I) + POWX(J))
            PY = (POWY(I) + POWY(J))
            TMP = 0.0
            DO 20 K = 1, NDAT
               IF (VALID(K)) THEN
                  SX = CMAP(K, 1) * SF
                  SY = CMAP(K, 2) * SF
                  TMP = TMP + EPOL(SX, PX, SY, PY)
               ENDIF
   20       CONTINUE
            SUM(I, J) = TMP
   30    CONTINUE
         DO 50 J = 1, 2
            PX = POWX(I)
            PY = POWY(I)
            TMP = 0.0
            DO 40 K = 1, NDAT
               IF (VALID(K)) THEN
                  SX = CMAP(K, 1) * SF
                  SY = CMAP(K, 2) * SF
                  TMP = TMP + (EPOL(SX, PX, SY, PY) * CIMA(K, J))
               ENDIF
   40       CONTINUE
            RES(I, J) = TMP
   50    CONTINUE
   60 CONTINUE
C
C Decompose matrix into LU form and then find determinant (just to
C check that the matrix is not singular).
C
      CALL CROUT(SUM, N)
      D = DETER(SUM, N)
      IF (D .EQ. 0.0) GOTO 110
      CALL INVERT(SUM, INV, N)
C
C Having inverted the matrix now evaluate the polynomial coefficients
C
      DO 90 I = 1, 2
         DO 80 J = 1, N
            TMP = 0.0
            DO 70 K = 1, N
               TMP = TMP + INV(J, K) * RES(K, I)
   70       CONTINUE
            COEF(J, I) = TMP
   80    CONTINUE
   90 CONTINUE
C
C Now evaluate reconstructed point co-ordinates and check that they are
C within the maximum error range (if required).
C
C Assume result
C
      FNDLSQ = 0
      DO 100 I = 1, NDAT
         SX = CMAP(I, 1) * SF
         SY = CMAP(I, 2) * SF
         CALL EVAL(COEF, POWX, POWY, SX, SY, N, RES(I, 1), RES(I, 2))
         IF (CHKERR .AND. VALID(I)) THEN
            SX = (RES(I, 1) - CIMA(I, 1)) ** 2
            SY = (RES(I, 2) - CIMA(I, 2)) ** 2
            IF ((SX + SY) .GT. ERRMAX) THEN
               VALID(I) = .FALSE.
               FNDLSQ = 1
            ENDIF
         ENDIF
  100 CONTINUE
  110 RETURN
C
C Copyright (C) 1988,1989,1991: Synoptics Ltd, All Rights Reserved.
C
      END
