      SUBROUTINE ANOTAT
C----------------------------------------------------------------------
C
C  Default usage: ANOTATE to dis:n with yyy threshold x
C
C  draws an overlay on dis:n which is 'on' wherever the pixels of
C  picture yyy equal or exceed the value x. The picture origins are
C  co-located.
C
C  If NEGATIVE is set then values below threshold are turned 'on'.
C  If ERASE is set then the overlay is first erased.
C  If CLIP is set then the annotation and erasure are limited to the
C  boundaries of the displayed picture (in dis:n), otherwise they apply
C  to the whole display partition.
C
C  Only the real part of complex data is used
C  LAYER determines the layer of yyy to be used
C
C----------------------------------------------------------------------
C
C  Semper syntax:
C
C  Anotate :ANOTAT  $1=dis to=$1 $2=sel with=$2 threshold= negative clip
C                   layer=1             open(lp2,old)=with
C
C----------------------------------------------------------------------
C
      INCLUDE 'COMMON'
C
C  integer buffer
C
      INTEGER IBUFL
      PARAMETER(IBUFL=LNBUF/LNINT)
C
      INTEGER IB1(IBUFL)
      EQUIVALENCE (IB1(1),RB1(1))
C
C  logical buffer
C
      LOGICAL LB2(IBUFL)
      EQUIVALENCE (LB2(1),RB2(1))
C
C  Semper functions
C
      LOGICAL FSINIT,FSLINE,FSQBOR,FSQLIM,FSQTRA,FSERAS
      LOGICAL GETRNG,OPT,VARSET,SEMROW,SEMSEL,FSELEC
      INTEGER IPACK,IVALPN,IVAL
      REAL VAL
C
      INTEGER NTO,TO,NTHRES,ITHRES,NNEGAT,NERASE
      INTEGER NLAYER,LAYER,NCLIP
      REAL THRESH,PMIN,PMAX
      LOGICAL NEGATI,ERASE,CLIP
C
      INTEGER  WINROW,WINCOL,WICROW,WICCOL,WINLAY,WIFORM,I,J
      INTEGER  IFIRST,ILAST,JFIRST,JLAST
C
      REAL LEFT,RIGHT,TOP,BOTTOM,X1,X2,Y,SCALEX,XOFF,SCALEY,YOFF
      INTEGER IDX,IDY
      LOGICAL INKING
C
C   SEMPER codes
C
      NTO=IPACK('TO')
      NNEGAT=IPACK('NEGATIVE')
      NERASE=IPACK('ERASE')
      NLAYER=IPACK('LAYER')
      NTHRES=IPACK('THRESHOLD')
      NCLIP=IPACK('CLIP')
C
C  to picture
C
      TO=IVALPN(NTO)
C
C  with picture parameters
C
      WINROW=NROWS(LP2)
      WINCOL=NCOLS(LP2)
      WINLAY=NLAYS(LP2)
      WICROW=CROWN(LP2)
      WICCOL=CCOLN(LP2)
      WIFORM=FORMN(LP2)
C
C  keys and options
C
      IF(VARSET(NTHRES)) THEN
         THRESH=VAL(NTHRES)
      ELSE
         IF(GETRNG(PMIN,PMAX,LP2)) RETURN
         THRESH=(PMIN+PMAX)*0.5
      ENDIF
C
      NEGATI=OPT(NNEGAT)
      ERASE=OPT(NERASE)
      LAYER=IVAL(NLAYER)
      CLIP=OPT(NCLIP)
C
C  fault bad layers
C
      IF(LAYER .GT. WINLAY .OR. LAYER .LT. 1) THEN
         ERROR=9
         RETURN
      ENDIF
C
C  Initialise graphics in picture coordinates, set to = display picture
C
      IF(FSINIT(3,TO)) RETURN
C
C  establish  draw/erase limits
C
      IF(CLIP) THEN
         IF(FSQBOR(LEFT,RIGHT,BOTTOM,TOP)) RETURN
      ELSE
         IF(FSQLIM(LEFT,RIGHT,BOTTOM,TOP))RETURN
      ENDIF
C
C  erase overlay if required
C
      IF(ERASE) THEN
         IF(FSERAS(2,LEFT,RIGHT,BOTTOM,TOP)) RETURN
      ENDIF
C
C  Processing:
C
C
C  Determine x,y sampling factors
C
      IF(FSQTRA(SCALEX,XOFF,SCALEY,YOFF)) RETURN
C
C  sampling factors will be assumed integer for the display image
C
      IDX=NINT(ABS(1.0/SCALEX))
      IDY=NINT(ABS(1.0/SCALEY))
C
C----------------------------------------------------------------------
C  The transformation for picture coordinates:
C
C  A point (x,y) in graphics coordinates corresponds to the point on the
C  displayed ('to') picture x columns to the right of the picture origin
C  and y rows above it
C
C  The corresponding rows and columns in the 'with' picture are
C
C  column:  y = CROW     - J    ==>    J = CROW     -   y
C                   with                       with
C
C  row:     x = I - CCOL        ==>    I = CCOL     +   x
C                       with                   with
C
C----------------------------------------------------------------------
C
C
C  Row in 'with' picture corresponding to top of picture/partition
C
      JFIRST=WICROW-NINT(TOP)
C
C  Row corresponding to bottom of displayed picture/partition
C
      JLAST= WICROW-NINT(BOTTOM)
C
C  Column corresponding to left boundary of picture/partition
C
      IFIRST=WICCOL+NINT(LEFT)
C
C  Column in 'with' picture corresponding to right edge
C
      ILAST=WICCOL+NINT(RIGHT)
C
C  clip rows,columns at 'with' picture edges
C
C  rows
      IF(JFIRST .LT. 1) JFIRST=1
      IF(JLAST .GT. WINROW) JLAST=WINROW
C  columns
      IF(IFIRST .LT. 1) IFIRST=1
      IF(ILAST .GT. WINCOL) ILAST=WINCOL
C
C  scan 'with' image line by line noting any under-sampling
C
      DO 50 J=JFIRST,JLAST,IDY
C
C  Y = graphics coordinate corresponding to current value of J
C
         Y=WICROW-J
C
C  read source row and set logical buffer to .TRUE. where there is 'ink'
C  .FALSE. where there is 'paper', noting any under-sampling
C
         IF(WIFORM .EQ. NFMINT .OR. WIFORM .EQ. NFMBYT) THEN
C
C  read
C
            IF(SEMROW(1,IB1,NFMINT,J,LAYER,LP2)) RETURN
C
C  convert threshold to suitable integer
C
            ITHRES=INT(THRESH)
            IF(FLOAT(ITHRES) .LT. THRESH) ITHRES=ITHRES+1
C
C  convert integer buffer to logical buffer
C
            DO 10 I=IFIRST,ILAST,IDX
               LB2(I)=IB1(I) .GE. ITHRES
   10       CONTINUE
C
         ELSE
C
C  use real buffers
C
C  read
            IF(SEMROW(1,RB1,NFMFP,J,LAYER,LP2)) RETURN
C  convert
            DO 20 I=IFIRST,ILAST,IDX
               LB2(I)=RB1(I) .GE. THRESH
   20       CONTINUE
C
         ENDIF
C
C  invert paper and ink if required
C
         IF(NEGATI) THEN
            DO 30 I=IFIRST,ILAST,IDX
               LB2(I)=.NOT. LB2(I)
   30       CONTINUE
         ENDIF
C
C  Convert LB2 to series of line segments
C
         INKING=.FALSE.
C
         DO 40 I=IFIRST,ILAST,IDX
C
            IF((.NOT. INKING) .AND. LB2(I)) THEN
C
C  start of line
C
               X1=FLOAT(I-WICCOL)
               INKING=.TRUE.
            ENDIF
C
            IF(INKING .AND. (.NOT. LB2(I))) THEN
C
C  end of line: draw from X1 to X2=point PRIOR to current value of I
C
C  end of line
               X2=FLOAT(I-WICCOL-IDX)
               INKING=.FALSE.
C  draw
               IF(FSLINE(X1,Y,X2,Y)) RETURN
C
            ENDIF
C
   40    CONTINUE
C
C  tidy up edge (if still drawing then draw to end)
C
         IF(INKING) THEN
            X2=FLOAT(ILAST-WICCOL)
            IF(FSLINE(X1,Y,X2,Y)) RETURN
         ENDIF
C
   50 CONTINUE
C
C  reset select and display
C
      IF(FSELEC()) RETURN
      IF(SEMSEL(LP2)) RETURN
C
      RETURN
      END
