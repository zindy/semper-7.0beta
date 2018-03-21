C Semper 6 module: PCA
C--------------------------------------------------------------------
C
C PCA:
C
C Perform a principal component analysis of a given picture. If nec-
C essary the covariance of the picture is calculated. The eigenmatrix
C and eigenvalues of the covariance matrix may be output if required.
C This routine uses ALL (yes all) of the available SEMPER row buffers.
C
C Arguments:
C   None.
C
      SUBROUTINE PCA
C
C     ==============
C
      INTEGER*4 IX, IX0, IX1, IX2, IX3, IX4, MAXLYR, MAXDAT
      INTEGER   I, ICL, IFRM, IVALPN, LP, NC, NL, NPIC, NR, OFORM
      LOGICAL   STR
      INTEGER*4 LNGONE
      INTEGER   NCOVAR, NEIGEN, NESTIM, NFROM, NHOTEL, NINVER,
     +          NSTREC, NTO
      PARAMETER (NCOVAR = 5422, NEIGEN = 8367, NESTIM = 8780,
     +           NFROM = 10335, NHOTEL = 13420, NINVER = 14982,
     +           NSTREC = 31218, NTO = -601, LNGONE = 1)
      LOGICAL   COVAR2, INV, JAC, OPT, PCACAL, SEMCLS, SEMOPN, SEMROW,
     +          TSTSRG, VARSET
      EXTERNAL  OPT
      INTEGER   SEMFRM
C
      INCLUDE   'COMMON'
C
      INTEGER*4 LNREA4,I42,I44,N4BUF
      PARAMETER (LNREA4=LNREAL,I42=2,I44=4,N4BUF=NNBUF-5)
C
      REAL RL(N4BUF*LNBUF/LNREAL)
C
      EQUIVALENCE (RB6(1), RL(1))
C
C Collect a few options and check that they are consistent
C
      INV = OPT(NINVER)
      IF (INV .AND. .NOT.VARSET(NCOVAR)) THEN
C
C Inverse specified but no covariance matrix given: not possible.
C
         ERROR = 60
         IDERR = NINVER
         IDERR2 = NCOVAR
         GOTO 50
      ENDIF
C
C Get subregion limits
C
      IF (TSTSRG(1, LP1)) GOTO 50
C
C Check size of problem against system limits (dependent upon the
C length of rows, etc.).
C
      MAXLYR = (I44 * LNBUF) / (LNREA4 * LONGF(NCOLS(LP1)))
      MAXDAT = SMGI9
      MAXDAT = MAXDAT*(MAXDAT+I44+MAXDAT+I42)
      IF ((SMGI9 .LT. 2) .OR. (LONGF(SMGI9) .GT. MAXLYR) .OR.
     +    (MAXDAT .GT. (N4BUF * LNBUF) / LNREA4)) THEN
        ERROR = 5
        IDERR = IVALPN(NFROM)
        GOTO 50
      ENDIF
C
C Open output picture, with the same details as the input picture,
C excepting the size.
C
      LP2 = LP1
      OFORM = SEMFRM(FORMN(LP1))
      IF (SEMOPN(2, IVALPN(NTO), SMGI7, SMGI8, SMGI9, CLASSN(LP1),
     +           OFORM, LP2)) GOTO 50
C
C Indexes into row buffer for covariance, layer mean and eigenmatrix
C respectively, which will be stored in RB6().
C
      IX0 = LNGONE
      IX1 = IX0 + LONGF(SMGI9 * SMGI9)
      IX2 = IX1 + LONGF(SMGI9 * 4)
      IX3 = IX1 + LONGF(SMGI9 * 2)
      IX4 = IX2 + LONGF(SMGI9 * SMGI9)
C
C See if covariance matrix has been specified. If it has and it does
C not exist then calculate it.
C
      IF (VARSET(NCOVAR)) THEN
        NPIC = IVALPN(NCOVAR)
        IF (SEMOPN(1, NPIC, NC, NR, NL, ICL, IFRM, LP)) GOTO 50
C
C Check that the picture dimensions are appropriate: the number of
C rows and columns must be equal to the number of layers in the
C source picture. The covariance picture has extra rows which
C give the statistics per layer.
C
        IF ((SMGI9 .NE. NC) .OR. ((SMGI9 + 4) .NE. NR) .OR.
     +      (1 .NE. NL)) THEN
          ERROR = EBASRS + 0
          GOTO 50
        ELSE IF (ICL .NE. NCLUND) THEN
          ERROR = 6
          IDERR = NPIC
          GOTO 50
        ENDIF
C
C Otherwise, generate covariance data into a temporary picture
C
      ELSE
C
C Open temporary picture to store covariance data
C
        LP = 0
        IF (SEMOPN(3,0,SMGI9,SMGI9+4,1,NCLUND,NFMFP,LP)) GOTO 50
C
C Calculate covariance data
C
        IF (COVAR2(LP1,LP)) GOTO 50
      ENDIF
C
C Read the covariance data into array
C
      DO 10 I = 1, SMGI9 + 4
        IF (SEMROW(1, RL((LONGF(I) - IX0) * LONGF(SMGI9)
     +      + LNGONE), NFMFP, I, 1, LP)) GOTO 50
   10 CONTINUE
C
      IF (SEMCLS(LP)) GOTO 50
C
C If the mean is not to be subtracted (added) then set all means
C to zero; an enhancement would be to pass a flag down to the PCACAL
C routine which has a separate piece of code to deal with this
C special case.
C
      IF (.NOT. OPT(NHOTEL)) THEN
        DO 20 I = 1, SMGI9
          RL(IX1 + LONGF(I - 1)) = 0.0
   20   CONTINUE
      ENDIF
C
C Find the eigenvalues and eigenvectors of the covariance matrix
C and then sort them.
C
      IF (JAC(RL(IX0), RL(IX2), SMGI9)) THEN
        ERROR = EBASRS + 9
        GOTO 50
      ENDIF
      CALL EIGSRT(RL(IX0), RL(IX2), SMGI9)
      IF (VARSET(NEIGEN)) THEN
C
C Eigenvalues are to be output to a separate picture, transfers
C source picture label across to output eigenvalue picture.
C
        NC = SMGI9
        NR = SMGI9 + 1
        NL = 1
        NPIC = IVALPN(NEIGEN)
        LP = LP1
        IF (SEMOPN(2, NPIC, NC, NR, NL, NCLUND, NFMFP, LP)) GOTO 50
C
C Form eigenvalues into one contiguous row, and output it
C
        DO 30 I = 1, SMGI9
          RB1(I) = RL(IX0 + LONGF((I - 1) * SMGI9 + I - 1))
   30   CONTINUE
        IF (SEMROW(2, RB1, NFMFP, 1, 1, LP)) GOTO 50
C
C Now output eigenmatrix
C
        DO 40 I = 1, SMGI9
           IX = IX2 + LONGF((I - 1) * SMGI9)
           IF (SEMROW(2, RL(IX), NFMFP, I + 1, 1, LP)) GOTO 50
   40   CONTINUE
        IF (SEMCLS(LP)) GOTO 50
      ENDIF
C
C If BYTE form picture then check to see if it is necessary to scale
C the output data using a linear stretch
C
      IF (OFORM .EQ. NFMBYT) THEN
        STR = OPT(NSTREC)
        IF (OPT(NESTIM) .AND. (.NOT. INV)) THEN
          CALL GETEST(SMGI9, RL(IX4), RL(IX3), RL(IX2))
        ELSE
          IF (PCACAL(LP1, LP2, RL(IX2), RL(IX1), SMGI9, NCOLS(LP1),
     +               INV, .FALSE., .FALSE., RL(IX4))) GOTO 50
        ENDIF
        CALL GETSCA(255, RL(IX4), SMGI9, STR)
      ELSE
        STR = .FALSE.
      ENDIF
C
C Now do the transform
C
      IF (PCACAL(LP1, LP2, RL(IX2), RL(IX1), SMGI9, NCOLS(LP1), INV,
     +          .TRUE., STR, RL(IX4))) GOTO 50
C
   50 RETURN
C
C Copyright Synoptics 1988-1995
C
      END
C
C Semper 6 sub module: PCACAL
C--------------------------------------------------------------------
C
C PCACAL:
C
C Calculate Principal Component transform of input picture given the
C eigenmatrix and mean vector.
C
C Arguments:
C   L1        Input picture identifier
C   L2        Output picture identifier
C   EIGMAT    Matrix of eigenvectors
C   MEAN      Mean of picture layers
C   NL        Number of layers
C   NCOL      Number of columns in unwindowed source picture
C   INVERS    .TRUE. if the inverse transform is to be performed
C   NOUTPT    .TRUE. if output to picture
C   NSTRECH   .TRUE. scale output
C   SCALEF    contains max or min if NOUTPT else offset and gain
C
C Results:
C  .TRUE.     Failed (in SEMROW somehow)
C  .FALSE.    Successful
C
      LOGICAL FUNCTION PCACAL(L1, L2, EIGMAT, MEAN, NL, NCOL, INVERS,
     +                        OUTPUT, STRECH, SCALEF)
C
C     ===============================================================
C
      INTEGER*4 LNGONE
      PARAMETER (LNGONE = 1)
      INTEGER*4 BASE, I, J, K, L, SRC, SRCINC
      INTEGER L1, L2, NCOL, NL
      REAL    EIGMAT(NL, NL), MEAN(NL), MN, MX, OF, SCALEF(NL, 2), SF,
     +        TMP
      LOGICAL FIRST, INVERS, OUTPUT, SEMROW, STRECH
      INCLUDE 'COMMON'
C
C Assume prior result
C
      PCACAL = .TRUE.
C
C Start Principal Component Transform proper.
C
      FIRST = .TRUE.
      BASE = LONGF(SMGI2)
      DO 90 I = 1, LONGF(SMGI8)
C
C Read in original row from each layer. Note that all the row will
C be read in, not just the windowed columns.
C
        SRC = LNGONE
        DO 10 J = LONGF(SMGI3), LONGF(SMGI6)
          IF (SEMROW(1, RB1(SRC), NFMFP,
     +           SHORTF(BASE + I - LNGONE), SHORTF(J),
     +           L1)) RETURN
          SRC = SRC + LONGF(NCOL)
   10   CONTINUE
C
C Work down each layer in the picture
C
        DO 80 J = 1, LONGF(SMGI9)
          SRC = LONGF(SMGI1)
          SRCINC = LONGF(1 - NCOL * SMGI9)
C
C Separate code for inverse transform, notice that EIGMAT is not
C transposed directly rather the indices ordering is reversed. The
C mean vector must be the means of the ORIGINAL picture (a forward
C Hotelling transform gives a picture(?, using the term loosely).
C
          IF (INVERS) THEN
            DO 30 K = 1, LONGF(SMGI7)
               TMP = 0.0
               DO 20 L = 1, LONGF(SMGI9)
                  TMP = TMP + RB1(SRC) * EIGMAT(J, L)
                  SRC = SRC + LONGF(NCOL)
   20          CONTINUE
               RB5(K) = TMP + MEAN(J)
               SRC = SRC + SRCINC
   30       CONTINUE
          ELSE
            DO 50 K = 1, LONGF(SMGI7)
               TMP = 0.0
               DO 40 L = 1, LONGF(SMGI9)
                  TMP = TMP + (RB1(SRC) - MEAN(L)) * EIGMAT(L, J)
                  SRC = SRC + LONGF(NCOL)
   40          CONTINUE
               RB5(K) = TMP
               SRC = SRC + SRCINC
   50       CONTINUE
          ENDIF
C
C A complete transformed row has been calculated so output it if
C required after scaling it or finding range
C
          IF (OUTPUT) THEN
            IF (STRECH) THEN
              OF = SCALEF(J, 1)
              SF = SCALEF(J, 2)
              DO 60 K = 1, LONGF(SMGI7)
                RB5(K) = (RB5(K) - OF) * SF
   60         CONTINUE
            ENDIF
            IF (SEMROW(2, RB5, NFMFP, SHORTF(I),
     +                 SHORTF(J), L2)) RETURN
          ELSE
            MN = RB5(1)
            MX = MN
            DO 70 K = 2, LONGF(SMGI7)
              MN = MIN(MN, RB5(K))
              MX = MAX(MX, RB5(K))
   70       CONTINUE
            IF (FIRST) THEN
              SCALEF(J, 1) = MN
              SCALEF(J, 2) = MX
            ELSE
              SCALEF(J, 1) = MIN(SCALEF(J, 1), MN)
              SCALEF(J, 2) = MAX(SCALEF(J, 2), MX)
            ENDIF
          ENDIF
   80   CONTINUE
        FIRST = .FALSE.
   90 CONTINUE
      PCACAL = .FALSE.
      RETURN
C
C Copyright Synoptics 1988
C
      END
C
C Semper 6 sub module: JAC
C----------------------------------------------------------------------
C
C JAC:
C
C Using Jacobi transformations determine the eigenmatrix and eigen-
C values of the matrix SRC. The routine, as written, assumes that
C underflows are set to zero if this is NOT the case then code for
C Arp and Arq must be changed. The algorithm is taken from Numerical
C Recipes, pp 342 .. 346. Notice that column 1 of the eigenmatrix
C contains the eigenvector of eigenvalue 1, column 2 of eigenvalue 2
C etc.
C
C Arguments:
C   SRC    Source matrix, eigenvalues returned on leading diagonal
C   DEST   Destination matrix: to contain eigenmatrix
C   N      Size of matrices
C
C Results:
C   .TRUE.  Too many iterations
C   .FALSE. Success
C
      LOGICAL FUNCTION JAC(SRC, DEST, N)
C
C     ==================================
C
      INTEGER COL, I, J, N, ROW, SWEEPS
      REAL    ANG, COSG, DEL1, DEL2, DEST(N, N), LIMIT, SING,
     +        SRC(N, N), SUM, TANG, TAU
C
C Assume prior result
C
      JAC = .FALSE.
      SWEEPS = 0
C
C Initially the eigenmatrix is the identity matrix, and the eigen-
C values are the leading diagonal of the source matrix.
C
      DO 20 I = 1, N
        DO 10 J = 1, N
          DEST(I, J) = 0.0
   10   CONTINUE
        DEST(I, I) = 1.0
   20 CONTINUE
C
C Now start the iterative loop in which the superdiagonal elements are
C summed completion occurs when the sum is zero.
C
   30 SUM = 0.0
      DO 50 I = 1, (N - 1)
        DO 40 J = (I + 1), N
          SUM = SUM + ABS(SRC(I, J))
   40   CONTINUE
   50 CONTINUE
      IF (SUM .EQ. 0.0) RETURN
C
C Determine limit of whether to perform rotation. This allows
C slightly faster convergence as the sweep steps do not have to
C be so large.
C
      IF (SWEEPS .LT. 5) THEN
        LIMIT = SUM / (5.0 * N * N)
      ELSE
        LIMIT = 0.0
      ENDIF
C
C Next start the process of performing Jacobi rotations, reducing
C the source matrix to superdiagonal form. First calculate required
C angle of rotation, provided that the current element is greater
C than the previously calculated limit.
C
      DO 90 ROW = 1, (N - 1)
        DO 80 COL = (ROW + 1), N
          DEL1 = 100.0 * SRC(ROW, COL)
C
C If the element is out of the machine accuracy range then set it to
C zero, but make sure that this is not during initial iterations.
C
          IF (((ABS(SRC(ROW, ROW)) + DEL1) .EQ. ABS(SRC(ROW, ROW)))
     +    .AND. ((ABS(SRC(COL, COL)) + DEL1) .EQ. ABS(SRC(COL, COL)))
     +    .AND. (SWEEPS .GT. 4)) THEN
            SRC(ROW, COL) = 0.0
            SRC(COL, ROW) = 0.0
          ELSE IF (ABS(SRC(ROW, COL)) .GT. LIMIT) THEN
            DEL2 = SRC(COL, COL) - SRC(ROW, ROW)
C
C Check that squaring the rotation angle will not cause machine
C overflow
C
            IF (DEL2 .EQ. (DEL1 + DEL2)) THEN
              TANG = SRC(ROW, COL) / DEL2
            ELSE
              ANG = 0.5 * DEL2 / SRC(ROW, COL)
              TANG = SIGN(1.0 / (ABS(ANG) + SQRT(1.0 + ANG ** 2)), ANG)
            ENDIF
            COSG = 1.0 / SQRT(1.0 + TANG ** 2)
            SING = TANG * COSG
            TAU = SING / (1.0 + COSG)
            DEL1 = TANG * SRC(ROW, COL)
C
C Reduce the diagonal elements (iterate towards eigenvalues on the
C leading diagonal).
C
            SRC(ROW, ROW) = SRC(ROW, ROW) - DEL1
            SRC(COL, COL) = SRC(COL, COL) + DEL1
            SRC(ROW, COL) = 0.0
            SRC(COL, ROW) = 0.0
C
C Now deal with the off diagonal elements: (Underflow possible in
C this code section).
C
C Arp = Arp - sin(ang).(Arq + Tau.Arp)
C Arq = Arq + sin(ang).(Arp - Tau.Arq)
C
            DO 60 I = 1, N
              IF ((ROW .EQ. I) .OR. (COL .EQ. I)) GOTO 60
              DEL1 = SRC(I, ROW)
              DEL2 = SRC(I, COL)
              SRC(I, ROW) = DEL1 - SING * (DEL2 + TAU * DEL1)
              SRC(I, COL) = DEL2 + SING * (DEL1 - TAU * DEL2)
              DEL1 = SRC(ROW, I)
              DEL2 = SRC(COL, I)
              SRC(ROW, I) = DEL1 - SING * (DEL2 + TAU * DEL1)
              SRC(COL, I) = DEL2 + SING * (DEL1 - TAU * DEL2)
   60       CONTINUE
C
C Finally transform the emerging eigenmatrix
C Vrp = Vrp - sin(ang).(Vrq + Tau.Vrp)
C Vrq = Vrq + sin(ang).(Vrp - Tau.Vrq)
C
            DO 70 I = 1, N
              DEL1 = DEST(I, ROW)
              DEL2 = DEST(I, COL)
              DEST(I, ROW) = DEL1 - SING * (DEL2 + TAU * DEL1)
              DEST(I, COL) = DEL2 + SING * (DEL1 - TAU * DEL2)
   70       CONTINUE
          ENDIF
   80   CONTINUE
   90 CONTINUE
      SWEEPS = SWEEPS + 1
      IF (SWEEPS .LT. 100) GOTO 30
C
C Could not iterate to a solution successfully: is the matrix
C symmetric?
C
      JAC = .TRUE.
      RETURN
C
C Copyright Synoptics 1988
C
      END
C
C Semper 6 sub module: EIGSRT
C--------------------------------------------------------------------
C
C EIGSRT:
C
C Sort the eigenvalues given into descending absolute numerical order.
C Uses a straight insertion sort since sizes are small.
C
C Arguments:
C   EIGVEC      Matrix with eigenvalues on leading diagonal
C   EIGMAT      Matrix containing eigenvectors
C   N           Size of matrices
C
      SUBROUTINE EIGSRT(EIGVEC, EIGMAT, N)
C
C     ====================================
C
      INTEGER I, J, K, N
      REAL    EIGMAT(N, N), EIGVEC(N, N), TMP
C
      DO 30 I = 1, (N - 1)
        TMP = EIGVEC(I, I)
        K = I
        DO 10 J = (I + 1), N
          IF (ABS(EIGVEC(J, J)) .GT. ABS(TMP)) THEN
            K = J
            TMP = EIGVEC(J, J)
          ENDIF
   10   CONTINUE
        IF (K .NE. I) THEN
          EIGVEC(K, K) = EIGVEC(I, I)
          EIGVEC(I, I) = TMP
          DO 20 J = 1, N
            TMP = EIGMAT(J, I)
            EIGMAT(J, I) = EIGMAT(J, K)
            EIGMAT(J, K) = TMP
   20     CONTINUE
        ENDIF
   30 CONTINUE
      RETURN
C
C Copyright Synoptics 1988
C
      END
C
C Semper 6 sub module: GETEST
C---------------------------------------------------------------------
C
C GETEST:
C
C Estimate the likely range of the output picture given the range of
C data in the source picture and the eigenmatrix.
C
C Arguments:
C   NL        Number of layers
C   SCAL      Array to hold maximum and minimum output values
C   RG        Range of values in source picture
C   EIGMAT    Eigenmatrix of input picture covariance
C
      SUBROUTINE GETEST(NL, SCAL, RG, EIGMAT)
C
C     =======================================
C
      INTEGER I, J, K, KEY, NCASES, NL
      REAL    EIGMAT(NL, NL), RG(NL, 2), SCAL(NL, 2), SM
      LOGICAL FIRST
C
C Find number of permutations possible given the number of layers
C
      NCASES = 2 ** NL
      FIRST = .TRUE.
      DO 30 I = 0, (NCASES - 1)
C
C For each layer in picture find the sum, calculated dependent
C upon the bit pattern in I
C
        DO 20 J = 1, NL
          SM = 0.0
          KEY = 1
          DO 10 K = 1, NL
            IF (IAND(I, KEY) .NE. 0) THEN
              SM = SM + EIGMAT(J, K) * RG(K, 2)
            ELSE
              SM = SM + EIGMAT(J, K) * RG(K, 1)
            ENDIF
            KEY = KEY * 2
   10     CONTINUE
          IF (FIRST) THEN
            SCAL(J, 1) = SM
            SCAL(J, 2) = SM
          ELSE
            SCAL(J, 1) = MIN(SCAL(J, 1), SM)
            SCAL(J, 2) = MAX(SCAL(J, 2), SM)
          ENDIF
   20   CONTINUE
        FIRST = .FALSE.
   30 CONTINUE
      RETURN
C
C Copyright Synoptics 1988
C
      END
C
C Semper 6 sub module: GETSCA
C----------------------------------------------------------------------
C
C GETSCA:
C
C Calculate the offset and scale factors required for the output
C picture
C
C Arguments:
C   IMAX       Maximum value of output picture element
C   SCAL       Initially contains minima and maxima, then offset and
C              scale
C   NL         Dimension of SCAL
C   FLAG       .TRUE. if true scale factors
C
      SUBROUTINE GETSCA(IMAX, SCAL, NL, FLAG)
C
C     =======================================
C
      INTEGER I, IMAX, NL
      REAL    SCAL(NL, 2)
      LOGICAL FLAG
C
      DO 10 I = 1, NL
        IF (FLAG) THEN
          IF (SCAL(I, 1) .EQ. SCAL(I, 2)) THEN
             SCAL(I, 1) = 0.0
             SCAL(I, 2) = 1.0
          ELSE
             SCAL(I, 2) = REAL(IMAX) / (SCAL(I, 2) - SCAL(I, 1))
          ENDIF
        ELSE
          SCAL(I, 2) = 1.0
      ENDIF
   10 CONTINUE
      RETURN
C
C Copyright Synoptics 1988
C
      END
