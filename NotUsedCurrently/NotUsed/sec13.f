      SUBROUTINE SLICE
C----------------------------------------------------------------------
C
C  Forms a multilayer picture with each layer an extracted
C  (2-dimensional) region of the pictures in the range from,fr2
C  The size of this region is truncated to that of the region 
C overlapping
C  the first picture in the series.
C	
C----------------------------------------------------------------------
C
C  Semper syntax:
C
C  Slice  :SLICE  $1=sel  from=$1  $12=from  fr2=$12  $2=from  to=$2
C                 verify  >$position  >$size
C
C ..or.. size= si2=  position= po2= left right top bottom ...
C
C----------------------------------------------------------------------
C
C  Semper routines
C
      LOGICAL SEMROW,SEMCON,SEMOPN,SEMTIT,GETRG1,SEMCLS,GETFRM,OPT
      INTEGER IPACK,IVALPN
C
      INCLUDE 'COMMON'
C
      INTEGER NCOL,NROW,NLAY,ICLASS,IFORM,OFORM,OROW,OCOL,TO
      INTEGER COL1,ROW1,LAY1,COL2,ROW2,LAY2,FIRST,LAST,IP,IDUMMY,J,K,KK
      LOGICAL VERIFY
      CHARACTER*156 TITLE
C
C  Verbosity
C
      VERIFY=OPT(IPACK('VERIFY'))
C
C  first,last picture numbers
C
      FIRST=IVALPN(IPACK('FROM'))
      LAST=IVALPN(IPACK('FR2'))
C  check..
      IF(FIRST.GT.LAST) THEN
          ERROR=77
          WRITE(IDMESS,10)FIRST,LAST
   10     FORMAT( 'bad range of pictures, ',I6,' to ',I6)
          RETURN
      ENDIF
C
C  destination picture
C
      TO=IVALPN(IPACK('TO'))
C
C  open first picture as LP1
C
      IF(SEMOPN(1,FIRST,NCOL,NROW,NLAY,ICLASS,IFORM,LP1)) RETURN
C
C  define subregion (the layer numbers LAY1, LAY2 will be ignored)
C
      IF(GETRG1(COL1,ROW1,LAY1,COL2,ROW2,LAY2,LP1)) RETURN
C
C  determine form of output pictures (fp by default)
C
      IF(GETFRM(OFORM,NFMFP)) RETURN
C
C  open temporary FP output picture LP3
C
      OROW=ROW2-ROW1+1
      OCOL=COL2-COL1+1
      LP3=0
      IF(SEMOPN(3,IDUMMY,OCOL,OROW,LAST-FIRST+1,NCLIMA,NFMFP,LP3))RETURN
C
C  Process this picture to layer 1
C
      K=1
      CALL SLICE2(COL1,COL2,ROW1,ROW2,K,LP1,LP3)
      IF(ERROR .NE. 0) RETURN
C
C  Output information to console
C
      IF(VERIFY) THEN
         IF(SEMCON
     +   ('first picture successfully placed in layer 1. Title:'))
     +                                                      RETURN
         IF(SEMTIT(1,TITLE,LP1)) RETURN
         IF(SEMCON(TITLE)) RETURN
      ENDIF
C
C  Process remaining pictures
C
      DO 40 IP=FIRST+1,LAST
C
C  Open next picture as LP1
C
         IF(SEMOPN(1,IP,NCOL,NROW,NLAY,ICLASS,IFORM,LP1)) THEN
C
C  Trap error 30 = picture does not exist
C
            IF(ERROR .NE. 30) RETURN
            ERROR=0
C
         ELSE
C
C  Picture exists: define 2-D subregion
C
            IF(GETRG1(COL1,ROW1,LAY1,COL2,ROW2,LAY2,LP1)) THEN
C
C  trap ERROR=9, region outside picture
C
               IF(ERROR .NE. 9) RETURN
               ERROR=0
C
C  subregion not applicable to this picture
C
               IF(VERIFY) THEN
                  WRITE(TITLE,20) IP
   20             FORMAT('Picture ',I6,' is unsuitable. Title:')
                  IF(SEMCON(TITLE)) RETURN
                  IF(SEMTIT(1,TITLE,LP1)) RETURN
                  IF(SEMCON(TITLE)) RETURN
               ENDIF
C
            ELSE
C
C  process picture to next layer incrementing layer number
C
               K=K+1
               CALL SLICE2(COL1,COL2,ROW1,ROW2,K,LP1,LP3)
               IF(ERROR .NE. 0) RETURN
C
C
C  Output information to console
C
               IF(VERIFY) THEN
                  WRITE(TITLE,30) IP,K
   30             FORMAT('Placing subregion of picture ',I6,
     +                                 ' in layer ',I5,'. Title:')
                  IF(SEMCON(TITLE)) RETURN
                  IF(SEMTIT(1,TITLE,LP1)) RETURN
                  IF(SEMCON(TITLE)) RETURN
               ENDIF
C
            ENDIF
C
C  Release picture number
C
            IF(SEMCLS(LP1)) RETURN
C
         ENDIF
C
   40 CONTINUE
C
C  finally output picture of sensible size
C
C  open..
      LP2=0
      IF(SEMOPN(2,TO,OCOL,OROW,K,NCLIMA,OFORM,LP2))RETURN
C  copy
      DO 60 KK=1,K
         DO 50 J=1,OROW
            IF(SEMROW(1,RB1,NFMFP,J,KK,LP3)) RETURN
            IF(SEMROW(2,RB1,NFMFP,J,KK,LP2)) RETURN
   50    CONTINUE
   60 CONTINUE
C
C
      RETURN
      END
C
C
C
      SUBROUTINE SLICE2(C1,C2,R1,R2,LAYOUT,LPIN,LPOUT)
C
C  copy source region to destination picture
C
      LOGICAL SEMROW
      INCLUDE 'COMMON'
      INTEGER C1,C2,R1,R2,LAYOUT,LPIN,LPOUT,I,J,JJ
C
C  double length real buffer
C
      REAL XRB1(2*LNBUF/LNREAL)
      EQUIVALENCE (XRB1(1),RB1(1))
C
C  truncate subregion to output picture bounds
C
      IF(R2-R1+1 .GT. NROWS(LPOUT)) R2=R1+NROWS(LPOUT)-1
C
C  for each row in the subregion
C
      JJ=1
      DO 20 J=R1,R2
C
C  read row
C
         IF(SEMROW(1,RB1,NFMFP,J,1,LPIN)) RETURN
C
C  zero extend if necessary
C
         DO 10 I=C2+1,NCOLS(LPOUT)+C1-1
            XRB1(I)=0.0
   10    CONTINUE
C
C  write row and increment row counter
C
         IF(SEMROW(2,RB1(C1),NFMFP,JJ,LAYOUT,LPOUT)) RETURN
         JJ=JJ+1
C
   20 CONTINUE
C
C  finally if rows lie below the subregion
C
      IF(NROWS(LPOUT) .GE. JJ) THEN
C
C  fill RB1 with zeros
C
         DO 30 I=1,NCOLS(LPOUT)
            RB1(I)=0.0
   30    CONTINUE
C
C  fill remainder of LPOUT with zeros
C
         DO 40 J=JJ,NROWS(LPOUT)
            IF(SEMROW(2,RB1,NFMFP,J,LAYOUT,LPOUT)) RETURN
   40    CONTINUE
C
      ENDIF
C
      RETURN
      END
