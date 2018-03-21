      SUBROUTINE RDOVER
C----------------------------------------------------------------------
C
C  reads overlay from a display partition, the frame or (default) a
C  displayed picture and outputs the result as a byte picture with 0
C  where overlay off, 1 where overlay on. Two dimensional rectangular
C  subregions are supported.
C
C----------------------------------------------------------------------
C
C  Semper syntax:
C
C  Orb  :RDOVER  $1=  $2=sel  to=$2  frame  partition  picture
C                >$position   >$size
C
C  where
C    $position -->  position=  po2=  left  right  top  bottom
C  and
C    $size     -->  size= si2=
C
C----------------------------------------------------------------------
C
C  Semper routines
C
      LOGICAL SEMOPN,SEMROW,FSINIT,FSOVRW,FSOPTN,FSREGN,FSAMPL
      INTEGER IPACK,IVALPN
C
      INCLUDE 'COMMON'
C
C  Integer row buffer
C
      INTEGER IBUFL
      PARAMETER(IBUFL=LNBUF/LNINT)
C
      INTEGER IB1(IBUFL)
      EQUIVALENCE (IB1(1),RB1(1))
C
C
      INTEGER TO,NTO,FROM,MODE,NCOL,NROW,I,J
      REAL X,DX,Y,DY,XMIN,XMAX,YMIN,YMAX
C
C  Internal codes
C
      NTO=IPACK('TO')
C
C  values
C
      IF(FSOPTN(MODE,FROM)) RETURN
      TO=IVALPN(NTO)
C
C  Initialise graphics mode
C
      IF(FSINIT(MODE,FROM)) RETURN
C
C  establish subregion borders in graphics coordinates
C
      IF(FSREGN(XMIN,XMAX,YMIN,YMAX)) RETURN
C
C  establish sampling grid
C
      IF(FSAMPL(XMIN,XMAX,YMIN,YMAX,X,DX,NCOL,Y,DY,NROW)) RETURN
C
C  Open destination picture (bad region size will return NROW=NCOL=0,
C  which will be picked up by SEMOPN)
C
      LP2=0
      IF(SEMOPN(2,TO,NCOL,NROW,1,NCLIMA,NFMBYT,LP2)) RETURN
C
C  Initialise graphics mode again: SEMOPN interacts with FSINIT
C
      IF(FSINIT(MODE,FROM)) RETURN
C
C  for each row in destination picture
C
      DO 20 J=1,NROW
C
C  read overlay row to IB1
C
         IF(FSOVRW(1,IB1,NCOL,X,Y)) RETURN
C
C  Convert to 1/0
C
         DO 10 I=1,NCOL
            IF(IB1(I) .NE. 0) IB1(I)=1
   10    CONTINUE
C
C   Write row to destination picture
C
         IF(SEMROW(2,IB1,NFMINT,J,1,LP2)) RETURN
C
C   and increment row position
C
         Y=Y+DY
C
   20 CONTINUE
C
      RETURN
      END
