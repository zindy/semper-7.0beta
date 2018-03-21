      SUBROUTINE GRADXY
C----------------------------------------------------------------------
C
C   Output FP picture layer 1 = d/dx (input picture)
C                     layer 2 = d/dy (input picture)
C
C   The differentials are exact for quadratic functions,
C   P=(ax+by+c)(dx+ey+1), except at the edges where values are repeated
C
C   NOTE:  Complex input is not supported
C          Picture must have at least 3 columns and 3 rows.
C          Multilayer input is not supported
C
C----------------------------------------------------------------------
C
C  Semper syntax:
C   Dxy   :GRADXY $1=sel  from=$1  $2=from  to=$2  open(lp1,old)=from
C
C----------------------------------------------------------------------
C
C  Semper functions:
C
      LOGICAL SEMROW,SEMOPN,SEMCEN
      INTEGER IPACK,IVALPN
C
      INTEGER NCOL,NROW,NLAY,INFORM,INCLAS,CCOL,CROW
      INTEGER NFROM,FROM,TO,NTO,J
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
      CCOL=CCOLN(LP1)
      CROW=CROWN(LP1)
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
C Fault too few rows/columns
C
      IF(NROW .LT. 3 .OR. NCOL .LT. 3) THEN
         ERROR=77
         WRITE(IDMESS,10) FROM
   10    FORMAT('Too few rows/columns in picture ',I6)
         RETURN
      ENDIF
C
C Fault more than one layer
C
      IF(NLAY .NE. 1) THEN
         ERROR=62
         IDERR=IPACK('DXY')
         RETURN
      ENDIF
C
C Open 'to' picture as LP2, with two layers. Title is copied from LP1
C
      LP2=LP1
      IF(SEMOPN(2,TO,NCOL,NROW,2,INCLAS,NFMFP,LP2)) RETURN
C
C Set picture origin for LP2
C
      IF(SEMCEN(LP2,CCOL,CROW,1)) RETURN
C
C
C  Start main body of routine..
C
C ..read in row data to row buffers RB3, RB4
C   row 1 -> RB3 , row 2 -> RB4
C
      IF(SEMROW(1,RB(I3),NFMFP,1,1,LP1)) RETURN
      IF(SEMROW(1,RB(I4),NFMFP,2,1,LP1)) RETURN
C
C  we now have:  I2 --> RB2 contains -nothing-
C                I3 --> RB3 contains row 1
C                I4 --> RB4 contains row 2
C
C
      DO 20 J=2,NROW-1
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
         IF(SEMROW(1,RB(I4),NFMFP,J+1,1,LP1)) RETURN
C
C  We now have:  I2 --> row J-1
C                I3 --> row J
C                I4 --> row J+1
C
C Calculate d/dy (row j) and store in buffer 6
C           d/dx (row j)   ------->   buffer 5
C
         CALL GRAD1(RB5,RB6,RB(I2),RB(I4),NCOL)
C
C  Output processed rows to LP2, row J
C  d/dx = rb5 to layer 1
C  d/dy = rb6 to layer 2
C
         IF (SEMROW(2,RB5,NFMFP,J,1,LP2)) RETURN
         IF (SEMROW(2,RB6,NFMFP,J,2,LP2)) RETURN
C
C top/bottom edge effects:
C
         IF(J .EQ. 2) THEN
            IF (SEMROW(2,RB5,NFMFP,1,1,LP2)) RETURN
            IF (SEMROW(2,RB6,NFMFP,1,2,LP2)) RETURN
         ENDIF
C
         IF(J .EQ. NROW-1) THEN
            IF (SEMROW(2,RB5,NFMFP,NROW,1,LP2)) RETURN
            IF (SEMROW(2,RB6,NFMFP,NROW,2,LP2)) RETURN
         ENDIF
C
   20 CONTINUE
C
      RETURN
C
      END
C
C
C
      SUBROUTINE GRAD1(DX,DY,ZMINUS,ZPLUS,N)
C
C  calculate DX= value of d/dx of a picture, evaluated the current row
C       and  DY= value of d/dy at the current row where:
C
C  ZMINUS contains row data for the previous row
C  ZPLUS  contains data for the next row
C
C
C                            _____         _____
C     ZMINUS            .... |I-1|    I    |I+1| ....
C                            -----         -----
C     CURRENT ROW       ....  I-1     I     I+1  ....
C                            _____         _____
C     ZPLUS             .... |I-1|    I    |I+1| ....
C                            -----         -----
C
C  The derivatives at CURRENT ROW J make use of the four boxed values
C
C
      INTEGER I,N
      REAL DX(N),DY(N),ZMINUS(N),ZPLUS(N)
C
      DO 10 I=2,N-1
         DX(I)=(ZMINUS(I+1)-ZMINUS(I-1)+ZPLUS(I+1)-ZPLUS(I-1))*0.25
C
C  FORTRAN y direction is MINUS pixel y direction
C
         DY(I)=-(ZPLUS(I-1)-ZMINUS(I-1)+ZPLUS(I+1)-ZMINUS(I+1))*0.25
   10 CONTINUE
C
C .. edge effects
C
      DX(1)=DX(2)
      DX(N)=DX(N-1)
      DY(1)=DY(2)
      DY(N)=DY(N-1)
C
      RETURN
      END
