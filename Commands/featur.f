C Semper 6 module: FEATUR
C-----------------------------------------------------------------
C
C FEATUR:
C
C Form a feature space plot (or 2-D histogram if you prefer) of a
C multilayer picture. Because of the space which may be required the
C output histogram (picture) is restricted to a maximum of the square
C root of a SEMPER row buffer length. A region of the picture may be
C sampled in the usual way (TSTSRG options).
C
C Arguments:
C   None.
C
      SUBROUTINE FEATUR
C     =================
C
C Parameter keys
C
      INTEGER NBAN, NBA2, NCHA, NCH2, NMA2, NMAX, NMI2, NMIN, NPRE, NTO
      INTEGER NVERIF
      PARAMETER (NBAN = 3254, NBA2 = 3272, NCHA = 5121, NCH2 = 5152,
     +           NMA2 = 20872, NMAX = 20864, NMI2 = 21192,
     +           NMIN = 21174, NPRE = 26325, NTO = -601, NVERIF = -3419)
C
      INTEGER BINSX, BINSY, DEVC, HIS, I, IOP, LAYR1, LAYR2, LP,
     +        MEDM, R
      REAL    RG(4), VXMAX, VXMIN, VYMAX, VYMIN
      LOGICAL SUBREG, TODIS
      EQUIVALENCE (VXMAX, RG(1)), (VXMIN, RG(2)),
     +            (VYMAX, RG(3)), (VYMIN, RG(4))
C
C Useful functions
C
      INTEGER IVAL, IVALPN
      REAL    VAL
C     CHANGED: LDM
C     Make sure that RANGE is not considered as an external
      EXTERNAL RANGE
      LOGICAL GENHS2, RANGE, SEMLU, SEMMED, SEMOPN, SEMROW, TSTSRG,
     +        VARSET, SEMCON, OPT
C
      INCLUDE 'COMMON'
C
      INTEGER*4 LNREA4
      PARAMETER (LNREA4=LNREAL)
C
      EQUIVALENCE (SUBREG, SMGL1)
C
C Check is output to display
C
      HIS = IVALPN(NTO)
      DEVC = HIS / 1000
      IF (SEMMED(DEVC, MEDM)) GOTO 30
      TODIS = MEDM .EQ. MEDDS
C
C Determine limits of histogram and check that we can cope with this
C given the size of SEMPER row buffers.
C
      BINSX = IVAL(NCHA)
      IF (VARSET(NCH2)) THEN
         BINSY = IVAL(NCH2)
      ELSE
         BINSY = BINSX
      ENDIF
      IF ((BINSX .LT. 1) .OR. (BINSY .LT. 1)) THEN
         BINSX = INT(SQRT(REAL(LNBUF / LNREA4)))
         BINSY = BINSX
      ENDIF
      IF (((BINSX) * (BINSY)) .GT.
     +    (LNBUF / LNREA4)) THEN
         ERROR = EBASRS + 7
         IDERR = INT(SQRT(REAL(LNBUF / LNREA4)))
         GOTO 30
      ENDIF
C
C Now check that the layers to be histogrammed are valid
C
      LAYR1 = IVAL(NBAN)
      LAYR2 = IVAL(NBA2)
      IF ((LAYR1 .LT. 1) .OR. (LAYR1 .GT. NLAYS(LP1)) .OR.
     +    (LAYR2 .LT. 1) .OR. (LAYR2 .GT. NLAYS(LP1))) THEN
         ERROR = 3
         IDERR = NBAN
         GOTO 30
      ENDIF
C
C Define sampling limits on source
C
      IF (TSTSRG(1, LP1)) GOTO 30
      IF (.NOT. SUBREG) THEN
         SMGI1 = 1
         SMGI2 = 1
         SMGI4 = NCOLS(LP1)
         SMGI5 = NROWS(LP1)
C
C This assignment, below, is to prevent RANGE from using all the
C picture and from rewriting range in the picture label
C
         SUBREG = .TRUE.
      ENDIF
C
C Open output histogram, copying old picture title if available.
C Open a temporary file if the output is to go to the display.
C
      LP = LP1
      IF (TODIS) THEN
         IOP = 3
      ELSE
         IOP = 2
      ENDIF
      IF (SEMOPN(IOP, HIS, BINSX, BINSY, 1, NCLUND, NFMFP, LP)) GOTO 30
C
C Now find ranges of picture layers.
C
      SMGI3 = LAYR1
      SMGI6 = LAYR1
      IF (RANGE(1, LP1)) GOTO 30
      VXMIN = VMIN
      VXMAX = VMAX
C
C To accomodate settings of variables MIN, MAX, MI2 and MA2 now
C juggle the values of VMIN and VMAX. First we have to check that
C if the preset key is set. Really RANGE should do this but it cannot
C handle '2D' ranges.
C
      IF (OPT(NPRE)) THEN
         VYMIN = VAL(NMI2)
         VYMAX = VAL(NMA2)
      ELSE
         SMGI3 = LAYR2
         SMGI6 = LAYR2
         IF (RANGE(1, LP1)) GOTO 30
         VYMIN = VMIN
         VYMAX = VMAX
      ENDIF
C
C After all that, calculate the histogram itself
C
      IF (GENHS2(LP1, BINSX, BINSY, RG, LAYR1, LAYR2)) GOTO 30
C
C Now output the final histogram
C
      R = 1
      DO 10 I = 1, BINSY
         IF (SEMROW(2, RB1(R), NFMFP, BINSY - I + 1, 1, LP)) GOTO 30
         R = R + BINSX
   10 CONTINUE
C
C Now check if we are outputting to the display
C
      IF (TODIS) THEN
         LP1 = LP
         LBLINC = .FALSE.
         CALL DISP
      ELSE
C
C Just output ranges pro tem, decide what to do with these later on,
C and set all the relevant variables
C
         IF (OPT(NVERIF)) THEN
            WRITE (RECORD,20) 'X', VXMIN, VXMAX, 'Y', VYMIN, VYMAX
   20       FORMAT(2('Layer: ', A, E10.3, ',', E10.3, 1X))
            IF (SEMCON(RECORD)) GOTO 30
         ENDIF
C
         DUMLOG = SEMLU(1, NMIN, VXMIN)
         DUMLOG = SEMLU(1, NMAX, VXMAX)
         DUMLOG = SEMLU(1, NMI2, VYMIN)
         DUMLOG = SEMLU(1, NMA2, VYMAX)
      ENDIF
C
   30 RETURN
C
C Copyright (C) 1988,1989,1990: Synoptics Ltd, All Rights Reserved
C
      END
C Semper 6 sub module: GENHS2
C----------------------------------------------------------------------
C
C GENHS2:
C
C The two-dimensional equivalent of GENHST. Form a two dimensional
C histogram in RB1. Uses RB2/RB3/RB4. Product of BINSX and BINSY must
C be less than the maximum value of a default integer. A possible
C improvement to this is to pass down the COUNT array and declare it
C to be a 2-D array. This then saves some mucking about in the inner-
C most loop (perhaps).
C
C This bears a striking resemblance to GENHST!
C
C Arguments:
C  LP       File (?) handle
C  BINSX    Number of X (column) bins
C  BINSY    Number of Y (row) bins
C  RG       Array containing the layer ranges (Xmax, Xmin, Ymax, Ymin)
C  LAYR1    First layer (X layer)
C  LAYR2    Second layer (may be same as first, though not useful)
C
C Returns:
C  .FALSE.  Histogram is arrayed row by row in RB1
C  .TRUE.   I/O or abandon error
C
      LOGICAL FUNCTION GENHS2(LP, BINSX, BINSY, RG, LAYR1, LAYR2)
C     ===========================================================
C
      INTEGER BINSX, BINSY, COLS, COLE, I, IA, IB, INFORM, LAYR1,
     +        LAYR2, LP, N, NELS, NSAFE, R
      REAL    A, B, RG(4), XSCALE, YSCALE, VXMAX, VXMIN, VYMAX, VYMIN
      LOGICAL SEMROW
C
      INCLUDE 'COMMON'
C
C Partial count buffer
C
      INTEGER COUNT(LNBUF / LNINT)
      EQUIVALENCE (COUNT(1), RB2(1))
C
C Assume prior result
C
      GENHS2 = .TRUE.
C
      VXMAX = RG(1)
      VXMIN = RG(2)
      VYMAX = RG(3)
      VYMIN = RG(4)
      NELS = BINSX * BINSY
C
C Set loop parameters
C
      IF (FORMN(LP) .EQ. NFMCOM) THEN
         COLS = 2 * SMGI1 - 1
         COLE = 2 * SMGI4
         INFORM = NFMCOM
      ELSE
         COLS = SMGI1
         COLE = SMGI4
         INFORM = NFMFP
      ENDIF
C
C Establish scaling
C
      XSCALE = REAL(BINSX - 0.01) / (VXMAX - VXMIN)
      YSCALE = REAL(BINSY - 0.01) / (VYMAX - VYMIN)
C
C Limit overflow count. 32767 is max size of a signed 16 bit integer
C
      NSAFE = INT(32767. / REAL(SMGI4 - SMGI1 + 1))
C
C Clear result buffers
C
      DO 10 I = 1, NELS
         RB1(I) = 0.0
   10 CONTINUE
C
C Start main loop
C
      N = 0
      DO 50 R = SMGI2, SMGI5
         IF (N .EQ. 0) THEN
            DO 20 I = 1, NELS
               COUNT(I) = 0
   20       CONTINUE
         ENDIF
C
C Fetch source rows from layers
C
         IF (SEMROW(1, RB3, INFORM, R, LAYR1, LP)) GOTO 60
         IF (SEMROW(1, RB4, INFORM, R, LAYR2, LP)) GOTO 60
         DO 30 I = COLS, COLE
            A = RB3(I)
            IF ((A .GE. VXMIN) .AND. (A .LE. VXMAX)) THEN
               B = RB4(I)
               IF ((B .GE. VYMIN) .AND. (B .LE. VYMAX)) THEN
                  IA = INT((A - VXMIN) * XSCALE) + 1
                  IB = INT((B - VYMIN) * YSCALE) * BINSX + IA
                  COUNT(IB) = COUNT(IB) + 1
               ENDIF
            ENDIF
   30    CONTINUE
         N = N + 1
         IF ((N .GE. NSAFE) .OR. (R .EQ. SMGI5)) THEN
            DO 40 I = 1, NELS
               RB1(I) = RB1(I) + REAL(COUNT(I))
   40       CONTINUE
            N = 0
         ENDIF
   50 CONTINUE
      GENHS2 = .FALSE.
   60 RETURN
C
C Copyright (C) 1988,1989: Synoptics Ltd, All Rights Reserved
C
      END
