      SUBROUTINE ANOT3
C---------------------------------------------------------------------
C
C   Performs XWIres, then marks the direction of grad(picture) as an
C   arrow in the overlay plane.
C
C   The arrows point 'UPHILL' i.e. towards bright (high value) pixels.
C
C---------------------------------------------------------------------
C
C  Semper syntax:
C
C  An3   :ANOT3  $1=dis  to=$1  radius=10  position=0  po2=0
C                open(lp1,old)=to
C
C---------------------------------------------------------------------
C
      INCLUDE 'COMMON'
C
C  Semper functions
C
      LOGICAL FSINIT,FSXWIR,FSARRO,SEMROW
      INTEGER IPACK,IVALPN
      REAL VAL
C
      INTEGER NTO,TO
      INTEGER NROW,NCOL,CCOL,CROW,INFORM,I,J,NPOS,NPO2,NRAD
      REAL XPOS,YPOS,DX,DY,R,RAD,X0,Y0
C
C  Source picture parameters
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      INFORM=FORMN(LP1)
      CCOL=CCOLN(LP1)
      CROW=CROWN(LP1)
C
C  Set SEMPER internal codes for TO,POS,PO2,RAD
C
      NTO=IPACK('TO')
      NPOS=IPACK('POSITION')
      NPO2=IPACK('PO2')
      NRAD=IPACK('RADIUS')
C
C  TO = displayed picture
C
      TO=IVALPN(NTO)
C
C  Fault complex pictures
C
      IF(INFORM .EQ. NFMCOM) THEN
         ERROR=43
         IDERR=TO
         RETURN
      ENDIF
C
C  Initialise graphics  (picture coordinates)
C
      IF(FSINIT(3,TO)) RETURN
C
C  Default position for XWIRES
C
      X0=VAL(NPOS)
      Y0=VAL(NPO2)
C
C   Xwires at PICTURE (origin = default)
C
      IF(FSXWIR(X0,Y0,XPOS,YPOS)) RETURN
C
C  Calculate picture column and row corresponding to XPOS,YPOS
C
C  ..column
      I=CCOL+NINT(XPOS)
C  ..row
      J=CROW-NINT(YPOS)
C
C  edge processing/outside picture
C
      IF(I.LE.1 .OR. I.GE.NCOL .OR. J.LE.1 .OR. J.GE.NROW) THEN
         ERROR=9
         RETURN
      ENDIF
C
C  read rows to buffers RB1,RB2
C
      IF(SEMROW(1,RB1,NFMFP,J-1,1,LP1)) RETURN
      IF(SEMROW(1,RB2,NFMFP,J+1,1,LP1)) RETURN
C
C  Calculate derivatives
C
      DX=(RB1(I+1)-RB1(I-1)+RB2(I+1)-RB2(I-1))*0.25
      DY=-(RB2(I-1)-RB1(I-1)+RB2(I+1)-RB1(I+1))*0.25
C
C  Default value for radius = length of arrow
C
      RAD=VAL(NRAD)
C
C  Normalise to desired size/ do nothing if DX=DY=0
C
      R=SQRT(DX*DX+DY*DY)
      IF(R .LT. 1.0E-20) RETURN
      R=RAD/R
      DX=DX*R
      DY=DY*R
C
C  Draw arrow
C
      IF(FSARRO(XPOS,YPOS,XPOS+DX,YPOS+DY)) RETURN
C
C  Done !
C
      RETURN
      END
