      SUBROUTINE CONVRT
C----------------------------------------------------------------------
C
C   Convert pictures between Complex and (R,Theta) representations.
C
C   The source picture may be either single-layer complex or two-layer
C   real with layer 1 = R, layer 2 = Theta (in radians). CONVRT produces
C   the equivalent picture in the alternative format and sets the SEMPER
C   variable
C
C   RMS :
C            [  ---                                     ]  1/2
C            [  \               2   /                   ]
C    RMS  =  [  /          | R |   /  Number of pixels  ]
C            [  ---           i   /                     ]
C            [  pixels i                                ]
C
C----------------------------------------------------------------------
C
C  Semper syntax:
C
C   Rpc  :CONVRT $1=sel from=$1 $2=from to=$2 open(lp1,old)=from
C
C----------------------------------------------------------------------
C
C  Semper functions:
C
      LOGICAL SEMROW,SEMOPN,SEMTIT,SEMCEN,SETVAR
      INTEGER IPACK,IVALPN,NBLANK
C
      REAL SUMSQR
C
      INTEGER NCOL,NROW,NLAY,INFORM,INCLAS,CCOL,CROW
      INTEGER NFROM,FROM,TO,NTO,J,NRMS
      LOGICAL TOPOLA,TORECT
      REAL RMS,TEMP
      CHARACTER*156 TITLE
C
      INCLUDE 'COMMON'
C
C  Create FP and complex row buffers
C
      COMPLEX CBUF(LNBUF/LNCOMP)
      REAL XBUF(LNBUF/LNREAL), YBUF(LNBUF/LNREAL)
      REAL RBUF(LNBUF/LNREAL), TBUF(LNBUF/LNREAL)
C
      EQUIVALENCE (CBUF(1),RB1(1))
      EQUIVALENCE (XBUF(1),RB2(1)), (YBUF(1),RB3(1))
      EQUIVALENCE (RBUF(1),RB4(1)), (TBUF(1),RB5(1))
C
C  Set SEMPER internal codes for TO,FROM, RMS
C
      NFROM=IPACK('FROM')
      NTO=IPACK('TO')
      NRMS=IPACK('RMS')
C
C  Set up dimensions, form, class, centre of source picture
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      NLAY=NLAYS(LP1)
      INFORM=FORMN(LP1)
      INCLAS=CLASSN(LP1)
      CCOL=CCOLN(LP1)
      CROW=CROWN(LP1)
C
C  Set (INTEGER) TO = destination picture, FROM = source picture
C
      TO=IVALPN(NTO)
      FROM=IVALPN(NFROM)
C
C  Fault bad input and set mode = to polar or to rectangular
C
      TOPOLA=.FALSE.
      TORECT=.FALSE.
C
      IF(INFORM .EQ. NFMCOM .AND. NLAY .EQ. 1) THEN
         TOPOLA=.TRUE.
      ENDIF
C
      IF(INFORM .NE. NFMCOM .AND. NLAY .EQ. 2) THEN
         TORECT=.TRUE.
      ENDIF
C
      IF(.NOT. (TOPOLA .OR. TORECT)) THEN
         ERROR=77
         WRITE(IDMESS,10) FROM
   10    FORMAT('Cannot convert, bad picture type for picture ',I6)
         RETURN
      ENDIF
C
C  Fault bad picture size
C
      IF(TORECT .AND. NCOL .GT. LNBUF/LNCOMP) THEN
         ERROR=77
         WRITE(IDMESS,20) FROM
   20    FORMAT
     +   ('Cannot convert to complex, too many columns in picture ',I6)
         RETURN
      ENDIF
C
C  Open 'to' picture as LP2
C
      IF(TOPOLA) THEN
C
C  Two layer real output picture
C
         LP2=0
         IF(SEMOPN(2,TO,NCOL,NROW,2,INCLAS,NFMFP,LP2)) RETURN
      ELSE
C
C  to rectangular: single layer complex picture
C
         LP2=0
         IF(SEMOPN(2,TO,NCOL,NROW,1,INCLAS,NFMCOM,LP2)) RETURN
      ENDIF
C
C  Set picture origin
C
      IF(SEMCEN(LP2,CCOL,CROW,1)) RETURN
C
C  Read source picture title
C
      IF(SEMTIT(1,TITLE,LP1)) RETURN
C
C  J = location of last nonblank character in title
C
      J=NBLANK(TITLE)
C
      IF(TOPOLA) THEN
C
C  Set '-POLAR' flag in picture title of destination picture
C
         TITLE(J+1:J+7)=' -POLAR'
      ELSE
C
C  from polar:  remove '-POLAR' flag from title if present
C
         IF(TITLE(J-6:J) .EQ. ' -POLAR') TITLE(J-6:J)='       '
      ENDIF
C
C  Write title to destination picture
C
      IF(SEMTIT(2,TITLE,LP2)) RETURN
C
C
C  Start main body of routine..
C
      RMS=0.0
C
      IF(TORECT) THEN
C
C  For each row
C
         DO 30 J=1,NROW
C
C  Read layer one (R) to RBUF, layer two (THETA) to TBUF
C
            IF (SEMROW(1,RBUF,NFMFP,J,1,LP1)) RETURN
            IF (SEMROW(1,TBUF,NFMFP,J,2,LP1)) RETURN
C
C  Calculate sum R*R
C
            RMS=RMS+SUMSQR(RBUF,NCOL)
C
C  Convert to rectangular coordinates
C
            CALL RT2XY(RBUF,TBUF,XBUF,YBUF,NCOL)
C
C  Convert to complex form
C
            CALL XY2C(XBUF,YBUF,CBUF,NCOL)
C
C  Save result to single layer complex picture
C
            IF (SEMROW(2,CBUF,NFMCOM,J,1,LP2)) RETURN
C
   30    CONTINUE
C
      ENDIF
C
C
      IF(TOPOLA) THEN
C
C  For each row
C
         DO 40 J=1,NROW
C
C  Read row to CBUF
C
            IF (SEMROW(1,CBUF,NFMCOM,J,1,LP1)) RETURN
C
C  Convert to X, Y form
C
            CALL C2XY(CBUF,XBUF,YBUF,NCOL)
C
C  Convert to R-squared, THETA coordinates
C
            CALL XY2R2T(XBUF,YBUF,RBUF,TBUF,NCOL)
C
C  Calculate sum R*R and convert to polar coordinates (R <-- SQRT(R) )
C
            CALL SUMRT(RBUF,TEMP,NCOL)
            RMS=RMS+TEMP
C
C  Save result to two layer real picture: R --> layer 1, THETA --> 
C layer 2
C
            IF (SEMROW(2,RBUF,NFMFP,J,1,LP2)) RETURN
            IF (SEMROW(2,TBUF,NFMFP,J,2,LP2)) RETURN
C
   40    CONTINUE
      ENDIF
C
C  Calculate RMS value
C
      RMS=SQRT(RMS/FLOAT(NCOL)/FLOAT(NROW))
C
C  Set SEMPER variable RMS
C
      IF(SETVAR(NRMS,RMS)) RETURN
C
      RETURN
C
      END
C
C
C
      SUBROUTINE XY2C(X,Y,C,N)
C
C  C = X + i Y
C
      INTEGER K,N
      REAL X(N), Y(N)
      COMPLEX C(N)
C
      DO 10 K=1,N
         C(K) = CMPLX( X(K), Y(K) )
   10 CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE C2XY(C,X,Y,N)
C
C  X = Re( C ),  Y = Im( C )
C
      INTEGER K,N
      REAL X(N), Y(N)
      COMPLEX C(N)
C
      DO 10 K=1,N
         X(K) = REAL( C(K) )
         Y(K) = AIMAG( C(K) )
   10 CONTINUE
C
      RETURN
      END
C
C
      SUBROUTINE XY2R2T(X,Y,R2,THETA,N)
C
C                                   -1
C  R2 = (X*X + Y*Y)       THETA = TAN  ( Y/X )
C
      INTEGER K,N
      REAL X(N), Y(N), R2(N), THETA(N)
C
      DO 10 K=1,N
         IF(X(K) .EQ. 0.0 .AND. Y(K) .EQ. 0.0) THEN
            THETA(K)=0.0
            R2(K)=0.0
         ELSE
            THETA(K) = ATAN2( Y(K), X(K) )
            R2(K) = X(K)*X(K) + Y(K)*Y(K)
         ENDIF
   10 CONTINUE
C
      RETURN
      END
C
C
      SUBROUTINE RT2XY(R,THETA,X,Y,N)
C
C  X = R COS(THETA) ,  Y = R SIN(THETA)
C
      INTEGER K,N
      REAL X(N), Y(N), R(N), THETA(N)
C
      DO 10 K=1,N
         X(K) = R(K) * COS(THETA(K))
         Y(K) = R(K) * SIN(THETA(K))
   10 CONTINUE
C
      RETURN
      END
C
C
C
      REAL FUNCTION SUMSQR(X,N)
C
C
C  Calculate SUMSQ  =  SUM   X * X
C                     i=1,N   i   i
C
      INTEGER N,I
      REAL X(N)
C
      SUMSQR=0.0
C
      DO 10 I=1,N
         SUMSQR=SUMSQR+X(I)*X(I)
   10 CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE SUMRT(X,SUM,N)
C
C
C  Calculate SUM    =  SUM   X
C                     i=1,N   i
C
C                  1/2
C  Replace X  <-- X
C           i      i
C
      INTEGER N,I
      REAL X(N), SUM
C
      SUM=0.0
C
      DO 10 I=1,N
         SUM=SUM+X(I)
         X(I)=SQRT(X(I))
   10 CONTINUE
C
      RETURN
      END
