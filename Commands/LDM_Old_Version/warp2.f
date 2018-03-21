C Semper 6 sub module: GENTM1
C---------------------------------------------------------------------
C
C GENTM1:
C
C Generate a scratch (temporary) picture containing powers of X (in
C terms of the map image). Returning the temporary picture number and
C the number of rows in the temporary picture. Uses row buffers 1, 2
C and 3 (implicitly by calling SEMOPN).
C
C Arguments:
C   ORDER     Number of polynomial terms required
C   LP        (Returned) picture handle
C   SIZE      Number of elements required
C   XBASE     Base value of X
C   XSTEP     Step value of X
C   LROW      (Output) length of picture row
C
C Returns:
C   .FALSE.   Powers stored, picture handle stored in LP
C   .TRUE.    Could not open picture or I/O error, etc.
C
      LOGICAL FUNCTION GENTM1(ORDER, LP, SIZE, XBASE, XSTEP, LROW)
C
C     ============================================================
C
      REAL    X, XBASE, XP, XSTEP
      INTEGER CORDER, CURCOL, CURROW, LP, LROW, NREM, NROW, ORDER, SIZE
C
      LOGICAL SEMOPN, SEMROW
C
      INCLUDE 'COMMON'
C
      REAL VALS(LNBUF / LNREAL)
      EQUIVALENCE (VALS(1), RB1(1))
C
C Assume prior result
C
      GENTM1 = .TRUE.
C
C Calculate blocking factors
C
      LROW = (SHORTF(LNBUF/LNREAL) / (ORDER + 1)) * (ORDER + 1)
      NROW = ((ORDER + 1) * SIZE / LROW) + 1
C
C If one row then only as long as required
C
      IF (NROW .EQ. 1) LROW = (ORDER + 1) * SIZE
      LP = 0
      IF (SEMOPN(3, NREM,  LROW, NROW, 1, NCLUND, NFMFP, LP)) RETURN
      NREM = SIZE
      CURCOL = 1
      CURROW = 1
      X = XBASE
   10 IF (NREM .EQ. 0) GOTO 30
         XP = 1.0
         CORDER = ORDER
C
C Evaluate powers of X to required order, note the way that this is
C achieved. Ok since we will not be handling large powers of X (up to
C about 6).
C
   20    CONTINUE
            VALS(CURCOL) = XP
            XP = XP * X
            CURCOL = CURCOL + 1
            CORDER = CORDER - 1
         IF (CORDER .GE. 0) GOTO 20
         NREM = NREM - 1
         X = X + XSTEP
         IF (CURCOL .LT. LROW) GOTO 10
         IF (SEMROW(2, VALS, NFMFP, CURROW, 1, LP)) RETURN
         CURROW = CURROW + 1
         CURCOL = 1
      GOTO 10
C
C Output any dangling powers (flush buffer).
C
   30 IF (CURCOL .NE. 1) THEN
         GENTM1 = SEMROW(2, VALS, NFMFP, CURROW, 1, LP)
      ELSE
         GENTM1 = .FALSE.
      ENDIF
      RETURN
C
C Copyright (C) 1988,1989,1991: Synoptics Ltd, All Rights Reserved.
C
      END
C
C Semper 6 sub module: GENXY
C----------------------------------------------------------------------
C
C GENXY:
C
C Generate the X and Y co-ordinates of points in the (distorted) input
C image into row buffers RB3 (X) and RB4 (Y). Uses the following row
C buffers:
C     RB2  Powers of Y
C     RB3  X co-ordinates
C     RB4  Y co-ordinates
C     RB5  Powers of X
C     RB6  Powers and coefficients (passed in as arguments)
C
C Arguments:
C   LP        Picture containing powers of X
C   LENX      Row length of the above
C   CFA       Coefficients for X polynomial term
C   CFB       Coefficients for Y polynomial term
C   PWRX      Powers of X in polynomial
C   PWRY      Powers of Y in polynomial
C   NCOF      Number of coefficient terms
C   YVAL      Value for Y
C   NTERMS    Number of points to evaluate for
C   ORDER     Order of polynomial
C
C Returns:
C   .FALSE.    Co-ordinates found successfully
C   .TRUE.     I/O error or abandon
C
      LOGICAL FUNCTION GENXY(LP, LENX, CFA, CFB, PWRX, PWRY, NCOF,
     +                       YVAL, NTERMS, ORDER)
C
C     ============================================================
C
      INTEGER  CROW, I, IX, J, LENX, LP, NCOF, NTERMS, ORDER,
     +         PWRX(NCOF), PWRY(NCOF)
      REAL     CFA(NCOF), CFB(NCOF), CX, CY, TMP, TMPX, TMPY, YVAL
C
      LOGICAL  SEMROW
C
      INCLUDE  'COMMON'
C
      REAL XS(0: LNBUF / LNREAL - 1), YS(0: LNBUF / LNREAL - 1)
      EQUIVALENCE (XS(0), RB5(1)), (YS(0), RB2(1))
C
C Assume prior result
C
      GENXY = .TRUE.
      CX = REAL(CCOLN(LP1))
      CY = REAL(CROWN(LP1))
C
C Generate powers of Y for this row
C
      TMPY = 1.0
      DO 10 I = 1, (ORDER + 1)
         RB2(I) = TMPY
         TMPY = TMPY * YVAL
   10 CONTINUE
C
C Force first row of powers of X to be loaded in. Notice that the
C temporary picture is arranged so that complete powers of X are
C stored (it is some multiple of ORDER long) in the picture.
C
      CROW = 0
      IX = LENX
      DO 30 I = 1, NTERMS
C
C Time to load in next set of powers?
C
         IF (IX .GE. LENX) THEN
            IX = 0
            CROW = CROW + 1
            IF (SEMROW(1, RB5(1), NFMFP, CROW, 1, LP)) RETURN
         ENDIF
         TMPX = 0.0
         TMPY = 0.0
C
C Main polynomial loop. If you can optimise this do so!
C
         DO 20 J = 1, NCOF
            TMP = XS(IX + PWRX(J)) * YS(PWRY(J))
            TMPX = TMPX + TMP * CFA(J)
            TMPY = TMPY + TMP * CFB(J)
   20    CONTINUE
         RB3(I) = TMPX + CX
         RB4(I) = CY - TMPY
         IX = IX + ORDER + 1
   30 CONTINUE
C
C Accomplished successfully
C
      GENXY = .FALSE.
      RETURN
C
C Copyright (C) 1988,1989: Synoptics Ltd, All Rights Reserved.
C
      END
