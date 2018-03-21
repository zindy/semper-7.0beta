      SUBROUTINE GRADY
C----------------------------------------------------------------------
C
C   Output FP picture = d/dy (input picture). The differential is
C                       exact for linear and quadratic functions.
C
C   NOTE:  Complex input is not supported
C          Picture must have at least 3 rows
C
C----------------------------------------------------------------------
C
C  Semper syntax:
C
C  Ddy :GRADY $1=sel from=$1 $2=from to=$2 open(lp1,old)=from
C
C----------------------------------------------------------------------
C
C  Semper functions:
C
      LOGICAL SEMROW,SEMOPN
      INTEGER IPACK,IVALPN
C
      INTEGER NCOL,NROW,NLAY,INFORM,INCLAS
      INTEGER NFROM,FROM,TO,NTO,J,K
      INTEGER*4 I1,I2,I3,I4,IBUFLN,IOFSET
C
      INCLUDE 'COMMON'
C
C  Create one long buffer spanning RB2,RB3,RB4
C      (i.e. from RB2(1) to RB4(LNBUF/LNREAL))
C
      PARAMETER(IBUFLN=3*(LNBUF/LNREAL)+4*LNEDGE)
      PARAMETER(IOFSET=LNBUF/LNREAL+2*LNEDGE)
C
      REAL RB(IBUFLN)
C
C Set up I2,I3,I4 so that RB(I2)=RB2(1),RB(I3)=RB3(1),RB(I4)=RB4(1)
C
      EQUIVALENCE (RB(1),RB2(1))
C
      I2=1
      I3=I2+IOFSET
      I4=I3+IOFSET
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
C Set TO, FROM
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
C Fault too few rows
C
      IF(NROW .LT. 3) THEN
         ERROR=77
         WRITE(IDMESS,10) FROM
   10    FORMAT('Too few rows in picture ',I6)
         RETURN
      ENDIF
C
C Open 'to' picture as LP2, same origin and title as LP1
C
      LP2=LP1
      IF(SEMOPN(2,TO,NCOL,NROW,NLAY,INCLAS,NFMFP,LP2)) RETURN
C
C  Start main body of routine..
C
C .. for each layer
C
      DO 30 K=1,NLAY
C
C ..read in row data to row buffers RB2, RB3, RB4 from LP1
C   RB2=row 1, RB3=row2, RB4=row3
C
         IF(SEMROW(1,RB2,NFMFP,1,K,LP1)) RETURN
         IF(SEMROW(1,RB3,NFMFP,2,K,LP1)) RETURN
         IF(SEMROW(1,RB4,NFMFP,3,K,LP1)) RETURN
C
C  Correct for top edge:
C
C
C Set RB1 to extrapolated values for row(0)
C
         CALL EXTRAP(RB4,RB3,RB2,RB1,NCOL)
C
C Set RB1 = d/dy (row 1) = -1/2 (row 2 - row 0)
C NOTE sign change: FORTRAN Y-direction is MINUS pixel Y-direction.
C
         CALL GRADY1(RB1,RB3,RB1,NCOL)
C
C Set RB6 = d/dy (row 2) = -1/2 (row 3 - row 1)
C
         CALL GRADY1(RB6,RB4,RB2,NCOL)
C
C Output the first two rows to the destination picture
C
         IF(SEMROW(2,RB1,NFMFP,1,K,LP2)) RETURN
         IF(SEMROW(2,RB6,NFMFP,2,K,LP2)) RETURN
C
C Now process areas away from the edges
C
         DO 20 J=3,NROW-1
C
C  The pointers are, on entry:  I2 --> row J-2
C                               I3 --> row J-1
C                               I4 --> row J
C
C                             i--------------------i
C Cycle pointers to buffers   1---> (I2,I3,I4) >---1
C
            I1=I2
            I2=I3
            I3=I4
            I4=I1
C
C  The pointers are now:  I2 --> row J-1
C                         I3 --> row J
C                         I4 --> row J-2
C
C Read in next row to oldest buffer (I4) which can be discarded
C
            IF(SEMROW(1,RB(I4),NFMFP,J+1,K,LP1)) RETURN
C
C  We now have:  I2 --> row J-1
C                I3 --> row J
C                I4 --> row J+1
C
C Calculate d/dy (row j) and store in buffer 6
C     RB6 = -1/2 (RB(I4)-RB(I2))
C
            CALL GRADY1(RB6,RB(I4),RB(I2),NCOL)
C
C  Output processed row to LP2, row J
C
            IF (SEMROW(2,RB6,NFMFP,J,K,LP2)) RETURN
C
   20    CONTINUE
C
C Take care of bottom edge:
C
C Currently  I2 --> row (NROW-2)
C            I3 --> row (NROW-1)
C            I4 --> row (NROW)
C
C
C
C Extrapolate 'bottom' edge values: set RB6 to values for row(NROW+1)
C
         CALL EXTRAP(RB(I2),RB(I3),RB(I4),RB6,NCOL)
C
C Set RB6 = d/dy (row (nrow)) = -1/2 (RB6 - RB(I3))
C
         CALL GRADY1(RB6,RB6,RB(I3),NCOL)
C
C  Output final processed row to LP2
C
         IF (SEMROW(2,RB6,NFMFP,J,K,LP2)) RETURN
C
   30 CONTINUE
C
      RETURN
C
      END
C
C
C
C
      SUBROUTINE GRADY1(X,Y,Z,N)
C
C  X = -1/2 (Y - Z)
C
      INTEGER I,N
      REAL X(N),Y(N),Z(N)
C
      DO 10 I=1,N
         X(I)=(Z(I)-Y(I))*0.5
   10 CONTINUE
C
      RETURN
      END
C
      SUBROUTINE EXTRAP(Y1,Y2,Y3,YFOUR,N)
C
C  extrapolate three equally spaced values Y1,Y2,Y3 to YFOUR
C  extrapolation is exact for quadratics Y=ax*x+bx+c
C
C  NOTE: to extrapolate from Y1,Y2,Y3 to the previous value YZERO
C        you should call EXTRAP(Y3,Y2,Y1,YZERO,N)
C
C
      INTEGER I,N
      REAL Y1(N),Y2(N),Y3(N),YFOUR(N)
C
      DO 10 I=1,N
         YFOUR(I)=3.0*(Y3(I)-Y2(I))+Y1(I)
   10 CONTINUE
C
      RETURN
      END
