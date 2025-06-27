C Semper 6 module: WARP
C----------------------------------------------------------------------
C
C WARP:
C
C Correct a picture for geometrical distortion using a specified order
C of polynomial. The corrected picture is resampled in one of three ways
C according to the setting of various options.
C
C Arguments:
C   None
C
      SUBROUTINE WARP
C
C     ===============
C
      REAL VAL
      INTEGER FNDLSQ,IVAL,IVALPN,PWRGEN,SEMFRM
      LOGICAL EXTRCB,EXTRCT,EXTRNN,GENTM1,GENXY,SEMCLS
      LOGICAL SEMOPN,SEMROW,SEMROWI,VARSET,OPT,SEMCON
C
      INTEGER NBILIN,NBICUB,NIMAGE,NLAY,NMAP,NMAX,NORDER
      INTEGER NPOS,NPOS2,NSIZE,NSI2,NSTRDE,NTO,NVERIF
      PARAMETER (NBILIN=3572,NBICUB=3563,NIMAGE=14921,NLAY=19265,
     +           NMAP=20856,NMAX=8738,NORDER=24724,NPOS=26219,
     +           NPOS2=26232,NSIZE=30786,NSI2=30792,NSTRDE=31218,
     +           NTO=-601,NVERIF=-3419)
C
      INTEGER*4 LNGONE, LNGZER
      PARAMETER (LNGONE=1, LNGZER=0)
C
      REAL EMAX,DX,DY,DX2,DY2,SCAL,STRIDE,VALS(2)
      INTEGER*4 ICFA,ICFB,ICLX,ICLY,IIMX,IIMY,IMPX,IMPY,IPWX,IPWY,IX
      INTEGER*4 LNNELS,N4
      INTEGER I,IFORM,J,K,LP,LPA,LPB,MODE,NC1,NC2,NCI,NCL1,NCL2,NCM
      INTEGER NELS,NFM,NL,NR,OFORM,ORDER
      LOGICAL LBILIN,LBICUB,LVERIF
C
      INCLUDE 'COMMON'
C
      INTEGER LNBUFJ
      PARAMETER (LNBUFJ=LNBUF/(2*LNREAL)+1)
C
      INTEGER*4 LNREA4,LNRBY4,LNRBY6
      PARAMETER (LNREA4=LNREAL,LNRBY4=LNREAL*4,LNRBY6=LNREAL*6)
C
      INTEGER IBPWX(LNBUF/(2 * LNINT))
      INTEGER IBPWY(LNBUF/(2 * LNINT)), IB6(LNBUF/LNINT)
      LOGICAL LFLAG(LNBUF/LNINT)
C
      EQUIVALENCE (IBPWX(1),RB4(1)), (IBPWY(1),RB4(LNBUFJ)),
     +            (LFLAG(1),RB4(1)), (IB6(1),RB6(1))
C
C Check that we can cope with a polynomial of the specified order,
C first calculate maximum order possible on system.
C
      J = INT((SQRT(SQRT(REAL(LNBUF/LNREA4)) * 8.0 + 1.0) - 3.0) / 2.0)
      ORDER = IVAL(NORDER)
      IF ((ORDER .LT. 1) .OR. (ORDER .GT. J)) THEN
         NELS = 0
      ELSE
         I = (LNBUF/LNRBY4)
         NELS = PWRGEN(ORDER, IBPWX, IBPWY, I)
      ENDIF
      LNNELS = (NELS)
      IF ((NELS .EQ. 0) .OR.
     +   ((LNNELS * LNNELS) .GT. (LNBUF/LNREA4))) THEN
         IDERR = J
         ERROR = EBASRS + 10
         GOTO 130
      ENDIF
C
      LVERIF = OPT(NVERIF)
C
C Determine interpolation mode
C
      LBILIN = OPT(NBILIN)
      LBICUB = OPT(NBICUB)
      IF (LBILIN) THEN
         IF (LBICUB) THEN
            IDERR = NBILIN
            IDERR2 = NBICUB
            ERROR = 60
            GOTO 130
         ENDIF
         MODE = 1
      ELSE IF (LBICUB) THEN
         MODE = 2
      ELSE
         MODE = 0
      ENDIF
C
C Check to see if the solution is to be error bounded
C
      IF (VARSET(NMAX)) THEN
         EMAX = VAL(NMAX)
         EMAX = EMAX * EMAX
      ELSE
         EMAX = -1.0
      ENDIF
C
C Now find mode to use for interpolation
C
      OFORM = SEMFRM(FORMN(LP1))
      IFORM = OFORM
C
C Bilinear and cubic require at least floating form
C
      IF (MODE .NE. 0) THEN
         IF ((IFORM .EQ. NFMBYT) .OR. (IFORM .EQ. NFMINT)) IFORM = NFMFP
      ELSE IF (IFORM .EQ. NFMBYT) THEN
C
C Nearest neighbour takes int, fp or complex
C
         IFORM = NFMINT
      ENDIF
C
C Check details of control points: type must be plist and they must
C both be the same length for obvious reasons
C
      IF (SEMOPN(1,IVALPN(NMAP), NCM, NR, NL, NCL1, NFM, LPA)) GOTO 130
      IF (SEMOPN(1,IVALPN(NIMAGE),NCI,NR, NL, NCL2, NFM, LPB)) GOTO 130
      IF ((NCL1 .NE. NCLPLI) .OR. (NCL2 .NE. NCLPLI)) THEN
         ERROR = EBASRS + 11
         GOTO 130
      ENDIF
      IF (NCM .NE. NCI) THEN
         ERROR = EBASRS + 12
         GOTO 130
      ENDIF
      I = (LNBUF/LNRBY6)
      IF (NCM .GT. I) THEN
         IDERR = I
         ERROR = EBASRS + 13
         GOTO 130
      ENDIF
C
      IF (VARSET(NSIZE)) THEN
         NC1 = IVAL(NSIZE)
         IF (VARSET(NSI2)) THEN
            NC2 = IVAL(NSI2)
         ELSE
            NC2 = NC1
         ENDIF
      ELSE
         NC1 = NCOLS(LP1)
         NC2 = NROWS(LP1)
      ENDIF
C
      IF ((NC1) .GT. (LNBUF / LNREA4)) THEN
         ERROR = 5
         GOTO 130
      ENDIF
C
C Read control points
C
      IX = LNGONE
      LP = LPA
      J = 1
      DO 10 I = 1, 4
         IF (SEMROW(1, RB5(IX), NFMFP, 1, J, LP)) GOTO 130
         IX = IX + (NCM)
         J = J + 1
         IF (J .EQ. 3) THEN
            IF (SEMCLS(LP)) GOTO 130
            J = 1
            LP = LPB
         ENDIF
   10 CONTINUE
C
C Now transfer calculated powers of X to their new resting place (RB6),
C note that in setting up the indexes integers are assumed to be less
C than or equal to the size of reals (LNINT .LE. LNREAL)
C
      IPWX = LNGONE
      LNNELS = (NELS)
      IPWY = IPWX + LNNELS
      ICFA = IPWY + LNNELS
      ICFB = ICFA + LNNELS
      N4 = 0
      DO 20 I = 1, NELS
         IB6(IPWX + N4) = IBPWX(I)
         IB6(IPWY + N4) = IBPWY(I)
         N4 = N4 + 1
   20 CONTINUE
C
C Initially all data points are valid so say so and set indexes to
C data
C
      DO 30 I = 1, NCM
         LFLAG(I) = .TRUE.
   30 CONTINUE
      N4 = (NCM)
      IMPX = LNGONE
      IMPY = IMPX + N4
      IIMX = IMPY + N4
      IIMY = IIMX + N4
      ICLX = IIMY + N4
      ICLY = ICLX + N4
C
C Start of REPEAT .. UNTIL loop
C
   40 CONTINUE
         I = FNDLSQ(LFLAG, IB6(IPWX), IB6(IPWY), RB5(IIMX), RB5(IMPX),
     +              RB6(ICFA), NELS, NCM, RB1(1), RB2(1), RB5(ICLX),
     +              EMAX, SCAL)
         IF (I .EQ. 2) THEN
            ERROR = EBASRS + 14
            GOTO 130
         ENDIF
C
C If specified write out details of solution set
C
         IF (LVERIF) THEN
            DX2 = 0.0
            DY2 = 0.0
            NCI = 0
            IX = LNGZER
            IF (SEMCON(' ')) GOTO 130
            WRITE (RECORD,50) ORDER
            IF (SEMCON(RECORD)) GOTO 130
            RECORD(1:43)='   Map                        Actual Image '
            RECORD(44:)='Estimated Image    Differences'
            IF (SEMCON(RECORD)) GOTO 130
C
   50       FORMAT ('Control Point Co-ordinates (X, Y) for a ',
     +              'polynomial of order: ', I2)
C
            DO 70 J = 1, NCM
C
C Differences between calculated and actual positions
C
               DX = RB5(IIMX + IX) - RB5(ICLX + IX)
               DY = RB5(IIMY + IX) - RB5(ICLY + IX)
C
C Only valid points contribute towards rms errors
C
               WRITE (RECORD,60) RB5(IMPX + IX), RB5(IMPY + IX),
     +                           RB5(IIMX + IX), RB5(IIMY + IX),
     +                           RB5(ICLX + IX), RB5(ICLY + IX),
     +                           DX, DY
               IF (LFLAG(J)) THEN
                  DX2 = DX2 + DX * DX
                  DY2 = DY2 + DY * DY
                  NCI = NCI + 1
               ELSE
                  RECORD(75:75) = '*'
               ENDIF
               IF (SEMCON(RECORD)) GOTO 130
C
   60          FORMAT (2E13.5, 4F8.1, 2F8.2)
               IX = IX + LNGONE
   70       CONTINUE
            IF (NCI .NE. 0) THEN
               DX2 = SQRT(DX2 / REAL(NCI))
               DY2 = SQRT(DY2 / REAL(NCI))
               IF (SEMCON(' ')) GOTO 130
               WRITE (RECORD,80) DX2, DY2
               IF (SEMCON(RECORD)) GOTO 130
C
   80          FORMAT ('Rms error (X, Y): ', 2F8.2)
            ENDIF
         ENDIF
         IF ((I .EQ. 1) .AND. (EMAX .GT. 0.0)) GOTO 40
C
C Now have the coefficients required for the polynomial. The next stage
C is to calculate all the powers of X required and store them in a
C temporary picture (saves lots of flops).
C
C
C Position is centred on output image
C
      STRIDE = VAL(NSTRDE) * SCAL
      DX2 = STRIDE
      DX = VAL(NPOS) * SCAL - REAL(NC1 / 2) * DX2
      IF (GENTM1(ORDER, LPA, NC1, DX, DX2, NCI)) GOTO 130
C
C Now store coefficients and powers in their own picture too (extract
C grabs ALL row buffers).
C
      LPB=0
      IF (SEMOPN(3, I, NELS, 4, 1, NCLUND, NFMFP, LPB)) GOTO 130
      IX = LNGONE
      DO 90 I = 1, 2
         IF (SEMROWI(2, IB6(IX), NFMINT, I, 1, LPB)) GOTO 130
         IF (SEMROW(2, RB6(ICFA + IX - LNGONE), NFMFP, I + 2, 1, LPB))
     +     GOTO 130
         IX = IX + (NELS)
   90 CONTINUE
C
C Open output picture, copying old title across
C
      LP = LP1
      IF (VARSET(NLAY)) THEN
         NL = 1
         NFM = IVAL(NLAY)
         IF ((NFM .LT. 1) .OR. (NFM .GT. NLAYS(LP1))) THEN
            ERROR = 9
            GOTO 130
         ENDIF
      ELSE
         NL = NLAYS(LP1)
         NFM = 1
      ENDIF
      SMGI3 = NFM
      SMGI6 = SMGI3 + NL - 1
      IF (SEMOPN(2, IVALPN(NTO), NC1, NC2, NL, CLASSN(LP1),
     +           OFORM, LP)) GOTO 130
      IF (LP1 .EQ. LP) THEN
         ERROR = 59
         GOTO 130
      ENDIF
C
C Finally ready to start correction of the source image. This is fairly
C tricky as all row buffers are used by the extraction process. It
C proceeds as follows:
C     Reload powers, coefficients
C     Load first block of calculated X values
C     Evaluate powers of Y for this row
C     Determine source co-ordinates
C     Extract pixels (for all required layers)
C
      DY2 = STRIDE
      DY = VAL(NPOS2) * SCAL + REAL(NC2 / 2) * DY2
      VALS(1) = 0.0
      VALS(2) = 0.0
      DO 120 J = 1, NC2
C
C Reload (destroyed) powers and coefficients, then evaluate picture
C co-ordinates.
C
         IX = LNGONE
         LNNELS = (NELS)
         DO 100 I = 1, 2
            IF (SEMROWI(1,IB6(IX),NFMINT,I,1,LPB)) GOTO 130
            IF (SEMROW(1,RB6(ICFA + IX - 1),NFMFP,I+2,1,LPB)) GOTO 130
            IX = IX + LNNELS
  100    CONTINUE
         IF (GENXY(LPA,NCI,RB6(ICFA),RB6(ICFB),IB6(IPWX),IB6(IPWY),
     +             NELS,DY,NC1,ORDER)) GOTO 130
C
C Now extract those pixels
C
         DO 110 K = SMGI3, SMGI6
            IF (MODE .EQ. 0) THEN
               IF (EXTRNN(LP1, NC1, IFORM, K, .TRUE., VALS)) GOTO 130
            ELSE IF (MODE .EQ. 1) THEN
               IF (EXTRCT(LP1, NC1, IFORM, K, .TRUE., VALS)) GOTO 130
            ELSE IF (MODE .EQ. 2) THEN
               IF (EXTRCB(LP1, NC1, IFORM, K, .TRUE., VALS)) GOTO 130
            ENDIF
            IF (SEMROW(2, RB2, IFORM, J, K - SMGI3 + 1, LP)) GOTO 130
  110    CONTINUE
         DY = DY - DY2
  120 CONTINUE
  130 RETURN
C
C Copyright (C) 1988,1989: Synoptics Ltd, All Rights Reserved.
C
      END
C
C Semper 6 sub module: EPOL
C---------------------------------------------------------------------
C
C EPOL:
C
C Evaluate a single polynomial expression; this should not be used for
C 'heavy' calculations as it is rather inefficient. The code is ordered
C such that 0 ** n where n is not zero gives 0.0, but when 0 gives 1.0.
C
C Arguments:
C   X        Value to be raised
C   IPOWX    Exponentiation value
C   Y        Value to be raised
C   IPOWY    Exponentiation value
C
C Returns:
C             IPOWX    IPOWY
C     EPOL = X      * Y
C
      REAL FUNCTION EPOL(X, IPOWX, Y, IPOWY)
C
C     ======================================
C
      REAL X,Y
      INTEGER IPOWX,IPOWY
C
      REAL SX,SY
      INTEGER POWER
C
      POWER = IPOWX
      SX = 1.0
   10 IF (POWER .EQ. 0) GOTO 20
         SX = SX * X
         POWER = POWER - 1
         GOTO 10
   20 POWER = IPOWY
      SY = 1.0
   30 IF (POWER .EQ. 0) GOTO 40
         SY = SY * Y
         POWER = POWER - 1
         GOTO 30
   40 EPOL = SX * SY
      RETURN
C
C Copyright (C) 1988,1989,1991 : Synoptics Ltd, All Rights Reserved.
C
      END
C
C Semper 6 sub module: EVAL
C---------------------------------------------------------------------
C
C EVAL:
C
C Evaluate polynomial expression, returning calculated values.
C
C Arguments:
C   CF       Array of coefficients
C   POWX     Array of powers of X for each coefficient
C   POWY     Array of powers of Y for each coefficient
C   X        Value of X to evaluate for
C   Y        Value of Y to evaluate for
C   LV       Number of cofficients to use
C   XR       Calculated value of X
C   YR       Calculated value of Y
C
      SUBROUTINE EVAL(CF, POWX, POWY, X, Y, LV, XR, YR)
C
C     =================================================
C
      INTEGER LV
      REAL CF(LV,2),X,XR,Y,YR
      INTEGER POWX(LV),POWY(LV)
C
      INTEGER I, PX, PY
      REAL POL1
C
      REAL EPOL
C
      XR = 0.0
      YR = 0.0
      DO 10 I = 1, LV
         PX = POWX(I)
         PY = POWY(I)
         POL1 = EPOL(X, PX, Y, PY)
         XR = XR + (POL1 * CF(I, 1))
         YR = YR + (POL1 * CF(I, 2))
   10 CONTINUE
      RETURN
C
C Copyright (C) 1988,1989,1991: Synoptics Ltd, All Rights Reserved.
C
      END
C
C Semper 6 sub module: PWRGEN
C---------------------------------------------------------------------
C
C PWRGEN:
C
C Generate a list of the powers of x and y required for least squares
C solution.
C
C Arguments:
C   ORDER       Order of polynomial required
C   PWRX        Array to contain powers of X
C   PWRY        Array to contain powers of Y
C   MAX         Maximum size of arrays
C
C Returns:
C   Number of elements in arrays or 0 if not enough space
C
      INTEGER FUNCTION PWRGEN(ORDER, PWRX, PWRY, MAX)
C
C     ===============================================
C
      INTEGER IX, MAX, NELS, ORDER, PCUR, PWR, PWRX(MAX), PWRY(MAX)
C
      NELS = (2 + (ORDER * (ORDER + 3))) / 2
      IF (NELS .GT. MAX) THEN
         PWRGEN = 0
      ELSE
         IX = 1
         PWR = 0
   10    CONTINUE
            PCUR = 0
   20       CONTINUE
               PWRX(IX) = PWR - PCUR
               PWRY(IX) = PCUR
               PCUR = PCUR + 1
               IX = IX + 1
            IF (PCUR .LE. PWR) GOTO 20
            PWR = PWR + 1
         IF (PWR .LE. ORDER) GOTO 10
         PWRGEN = NELS
      ENDIF
      RETURN
C
C Copyright (C) 1988,1989: Synoptics Ltd, All Rights Reserved.
C
      END
