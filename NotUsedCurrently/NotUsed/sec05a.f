      SUBROUTINE GRADX
C----------------------------------------------------------------------
C
C   Output FP picture = d/dx (input picture). The differential is
C                       exact for linear and quadratic functions.
C
C   NOTE:  Complex input is not supported
C          Picture must have at least 3 columns
C
C----------------------------------------------------------------------
C
C  Semper syntax:
C
C  Ddx :GRADX $1=sel from=$1 $2=from to=$2 open(lp1,old)=from
C
C----------------------------------------------------------------------
C
C  Semper functions:
C
      LOGICAL SEMROW,SEMOPN
      INTEGER IPACK,IVALPN
C
      INTEGER NCOL,NROW,NLAY,INFORM,INCLAS
      INTEGER NFROM,FROM,TO,NTO,I,J,K
C
      INCLUDE 'COMMON'
C
C  Create FP row buffer BUF1 starting at column 0 and reaching at least
C  one pixel beyond the end of the picture row.
C
C
      REAL BUF1(0:LNBUF/LNREAL+1)
      EQUIVALENCE (BUF1(1),RB1(1))
C
C Set SEMPER internal codes for TO / FROM
C
      NFROM=IPACK('FROM')
      NTO=IPACK('TO')
C
C Set up dimensions, form, class, centre of source picture
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      NLAY=NLAYS(LP1)
      INFORM=FORMN(LP1)
      INCLAS=CLASSN(LP1)
C
C Set (INTEGER) TO = destination picture, FROM = source picture
C
      TO=IVALPN(NTO)
      FROM=IVALPN(NFROM)
C
C Fault complex input
C
      IF(INFORM .EQ. NFMCOM) THEN
         ERROR=43
         IDERR=IVALPN(NFROM)
         RETURN
      ENDIF
C
C Fault too few columns
C
      IF(NCOL .LT. 3) THEN
         ERROR=77
         WRITE(IDMESS,10) FROM
   10    FORMAT('Too few columns in picture ',I6)
         RETURN
      ENDIF
C
C Open 'to' picture as LP2, same class, origin and title as LP1
C
      LP2=LP1
      IF(SEMOPN(2,TO,NCOL,NROW,NLAY,INCLAS,NFMFP,LP2)) RETURN
C
C  Start main body of routine..
C
C .. for each layer and row..
C
      DO 40 K=1,NLAY
         DO 30 J=1,NROW
C
C ..read in row data to row buffer BUF1 in floating point form, from LP1
C
            IF (SEMROW(1,BUF1(1),NFMFP,J,K,LP1)) RETURN
C
C Extrapolate edge values (quadratic extrapolation)
C
            BUF1(0)=BUF1(3)+3.0*(BUF1(1)-BUF1(2))
            BUF1(NCOL+1)=3.0*(BUF1(NCOL)-BUF1(NCOL-1))+BUF1(NCOL-2)
C
C Calculate differential to buffer RB2
C
             DO 20 I=1,NCOL
                RB2(I)=(BUF1(I+1)-BUF1(I-1))*0.5
   20        CONTINUE
C
C  Output processed row to LP2
C
            IF (SEMROW(2,RB2,NFMFP,J,K,LP2)) RETURN
C
   30    CONTINUE
   40 CONTINUE
C
      RETURN
C
      END
