C Semper 6 processing module DSKIZ
C
      SUBROUTINE DSKIZ
C
C Outputs the skiz of the regions defined in the source picture.  Each
C region is labelled with a different positive integer value and may be
C disjoint.  The result is a binary picture where non-zero pixels mark
C the pixels furthest removed from each labelled region, that is, it
C marks the boundary between the zones of influence of each region.
C The result is obtained in three passes.  The first two passes produce
C the zones of influence, and the final pass detects the boundary
C pixels.  A boundary pixel is any pixel which is negative (equidistant
C pixel), or a positive valued pixel with a non-equal, non-zero
C 4-neighbour.  Pixels outside the limits of the picture are treated as
C being set to zero.  If the LABEL option is set, the source picture is
C taken to be a labelled image.  Otherwise, it is treated as a binary
C image and a labelled image is obtained from it prior to determining
C the zones of influence.
C
      INTEGER IVALPN !,IPACK
      LOGICAL CONOPT,OPT,SEMOPN,SEMROW,DLABL1,DLABL3,DLABL5,DSKIZ1
      LOGICAL SEMRNG
C
      INTEGER IOP,NPIC,NCOL,NROW,NLAY,J,K1,K2,K3
      LOGICAL LZERO,LNZERO
      REAL    RMIN,RMAX
C
      INCLUDE 'COMMON'
C
      INTEGER LABEL(0:LNBUF/LNINT+1,0:2)
      INTEGER DIST(0:LNBUF/LNINT+1,0:1)
      INTEGER DX(0:LNBUF/LNINT+1,0:1)
      INTEGER DY(0:LNBUF/LNINT+1,0:1)
      INTEGER SKIZ(LNBUF/LNINT)
C
      EQUIVALENCE (LABEL,RB1),(DIST,DX,RB3),(DY,RB5),(SKIZ,RB4)
C
C Fault conflicting options CIRCLE, DIAMOND, SQUARE and OCTAGON
C
      IF (CONOPT(5178,6761))  GOTO 60
      IF (CONOPT(5178,31101))   GOTO 60
      IF (CONOPT(5178,24140))  GOTO 60
      IF (CONOPT(6761,31101))  GOTO 60
      IF (CONOPT(6761,24140)) GOTO 60
      IF (CONOPT(31101,24140))  GOTO 60
C
C See which of these options is set (CIRCLE is default)
C
      IF (OPT(5178)) THEN
         IOP=0
      ELSE IF (OPT(6761)) THEN
         IOP=1
      ELSE IF (OPT(31101)) THEN
         IOP=2
      ELSE IF (OPT(24140)) THEN
         IOP=3
      ELSE
         IOP=0
      ENDIF
C
C Determine size of temporary picture to store intermediate results
C
      IF (IOP.EQ.0) THEN
         NLAY=3
      ELSE
         NLAY=2
      ENDIF
C
C Fetch source picture size
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
C
C Open output picture
C
      NPIC=IVALPN(-601)
      LP2=LP1
      IF (SEMOPN(2,NPIC,NCOL,NROW,1,NCLIMA,NFMBYT,LP2)) GOTO 60
C
C Open temporary picture to store intermediate results
C
      LP3=0
      IF (SEMOPN(3,0,NCOL,NROW,NLAY,NCLIMA,NFMINT,LP3)) GOTO 60
C
C LABEL option specifies a labelled source image
C
      IF (.NOT.OPT(19242)) THEN
C
C Source image not labelled, so treat it as a binary image and generate
C labelled image from it
C
         IF (DLABL1()) GOTO 60
C
C Labelled image in tempoary picture becomes source image
C
         LP1=LP3
      ENDIF
C
C Initialise source range flags
C
      LZERO=.FALSE.
      LNZERO=.FALSE.
C
C See if Euclidean distance transform required (option CIRCLE)
C
      IF (IOP.EQ.0) THEN
C
C Fetch initial row of data
C
         IF (DLABL3(1,LABEL(0,1),DX(0,1),DY(0,1),0,NCOL,NROW,
     +              LZERO,LNZERO)) GOTO 60
C
C Run forward pass through source picture
C
         DO 10 J=1,NROW
C
C Determine data buffer pointers
C
            K1=MOD(J,2)
            K2=MOD(J+1,2)
C
C Fetch data for current row
C
            IF (DLABL3(1,LABEL(0,K2),DX(0,K2),DY(0,K2),J,NCOL,NROW,
     +                 LZERO,LNZERO)) GOTO 60
C
C Process row data
C
            CALL DLABL4(1,LABEL(0,K1),DX(0,K1),DY(0,K1),
     +                    LABEL(0,K2),DX(0,K2),DY(0,K2),NCOL)
C
C Store intermediate results
C
            IF (SEMROW(2,LABEL(1,K2),NFMINT,J,1,LP3)) GOTO 60
            IF (SEMROW(2,   DX(1,K2),NFMINT,J,2,LP3)) GOTO 60
            IF (SEMROW(2,   DY(1,K2),NFMINT,J,3,LP3)) GOTO 60
   10    CONTINUE
C
C Fault absence of structure in source picture
C
         IF (.NOT.(LZERO.AND.LNZERO)) GOTO 70
C
C Fetch initial row of data
C
         IF (DLABL3(2,LABEL(0,0),DX(0,0),DY(0,0),NROW+1,NCOL,NROW,
     +              LZERO,LNZERO)) GOTO 60
C
C Run backward pass through source picture
C
         DO 20 J=NROW,1,-1
C
C Determine data buffer pointers
C
            K1=MOD(NROW-J,2)
            K2=MOD(NROW-J+1,2)
C
C Fetch data for current row
C
            IF (DLABL3(2,LABEL(0,K2),DX(0,K2),DY(0,K2),J,NCOL,NROW,
     +                 LZERO,LNZERO)) GOTO 60
C
C Process row data
C
            CALL DLABL4(2,LABEL(0,K1),DX(0,K1),DY(0,K1),
     +                    LABEL(0,K2),DX(0,K2),DY(0,K2),NCOL)
C
C Store labelled result
C
            IF (SEMROW(2,LABEL(1,K2),NFMINT,J,1,LP3)) GOTO 60
   20    CONTINUE
C
C Otherwise, calculate distance transform based on square grid
C
      ELSE
C
C Fetch initial row of data
C
         IF (DLABL5(1,LABEL(0,1),DIST(0,1),0,NCOL,NROW,
     +              LZERO,LNZERO)) GOTO 60
C
C Run forward pass through source picture
C
         DO 30 J=1,NROW
C
C Determine data buffer pointers
C
            K1=MOD(J,2)
            K2=MOD(J+1,2)
C
C Fetch data for current row
C
            IF (DLABL5(1,LABEL(0,K2),DIST(0,K2),J,NCOL,NROW,
     +                 LZERO,LNZERO)) GOTO 60
C
C Process row data
C
            CALL DLABL6(1,IOP,LABEL(0,K1),DIST(0,K1),
     +                        LABEL(0,K2),DIST(0,K2),NCOL)
C
C Store intermediate results
C
            IF (SEMROW(2,LABEL(1,K2),NFMINT,J,1,LP3)) GOTO 60
            IF (SEMROW(2, DIST(1,K2),NFMINT,J,2,LP3)) GOTO 60
   30    CONTINUE
C
C Fault absence of structure in source picture
C
         IF (.NOT.(LZERO.AND.LNZERO)) GOTO 70
C
C Fetch initial row of data
C
         IF (DLABL5(2,LABEL(0,0),DIST(0,0),NROW+1,NCOL,NROW,
     +              LZERO,LNZERO)) GOTO 60
C
C Run backward pass through source picture
C
         DO 40 J=NROW,1,-1
C
C Determine data buffer pointers
C
            K1=MOD(NROW-J,2)
            K2=MOD(NROW-J+1,2)
C
C Fetch data for current row
C
            IF (DLABL5(2,LABEL(0,K2),DIST(0,K2),J,NCOL,NROW,
     +                 LZERO,LNZERO)) GOTO 60
C
C Process row data
C
            CALL DLABL6(2,IOP,LABEL(0,K1),DIST(0,K1),
     +                        LABEL(0,K2),DIST(0,K2),NCOL)
C
C Store labelled result
C
            IF (SEMROW(2,LABEL(1,K2),NFMINT,J,1,LP3)) GOTO 60
   40    CONTINUE
      ENDIF
C
C Initialise output range flags
C
      LZERO=.FALSE.
      LNZERO=.FALSE.
C
C Fetch first two rows of data
C
      IF (DSKIZ1(LABEL(0,1),0,NCOL,NROW)) GOTO 60
      IF (DSKIZ1(LABEL(0,2),1,NCOL,NROW)) GOTO 60
C
C Final pass to detect boundaries between zones of influence
C
      DO 50 J=1,NROW
C
C Determine data buffer pointers
C
            K1=MOD(J,3)
            K2=MOD(J+1,3)
            K3=MOD(J+2,3)
C
C Fetch next row of data
C
         IF (DSKIZ1(LABEL(0,K3),J+1,NCOL,NROW)) GOTO 60
C
C Detect boundary pixels between zones of influence
C
         CALL DSKIZ2(LABEL(0,K1),LABEL(0,K2),LABEL(0,K3),SKIZ,NCOL,
     +               LZERO,LNZERO)
C
C Store final result in output picture
C
         IF (SEMROW(2,SKIZ,NFMINT,J,1,LP2)) GOTO 60
   50 CONTINUE
C
C Determine output range values from range flags
C
      IF (LZERO) THEN
         RMIN=0.0
      ELSE
         RMIN=1.0
      ENDIF
C
      IF (LNZERO) THEN
         RMAX=1.0
      ELSE
         RMAX=0.0
      ENDIF
C
C Store output range values in picture label
C
      IF (SEMRNG(2,RMIN,RMAX,LP2)) GOTO 60
C
   60 RETURN
C
C Fault source picture with no structure in it
C
   70 ERROR=12
      IDERR=IVALPN(10335)
      GOTO 60
C
C Copyright (C) 1991-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module DSKIZ1
C
      LOGICAL FUNCTION DSKIZ1(LABEL,ROW,NCOL,NROW)
C
      INTEGER LABEL(0:*),ROW,NCOL,NROW
C
C Reads data from row ROW of picture LP3 and sets edge pixels to zero.
C
      LOGICAL SEMROW
C
      INTEGER I
C
      INCLUDE 'COMMON'
C
      DSKIZ1=.TRUE.
C
C If edge row, set all label values to zero
C
      IF (ROW.LT.1.OR.ROW.GT.NROW) THEN
         DO 10 I=1,NCOL
            LABEL(I)=0
   10    CONTINUE
C
C Otherwise, read data from disc
C
      ELSE
         IF (SEMROW(1,LABEL(1),NFMINT,ROW,1,LP3)) GOTO 20
      ENDIF
C
C Set left and right edge values to zero
C
      LABEL(0)=0
      LABEL(NCOL+1)=0
C
      DSKIZ1=.FALSE.
C
   20 RETURN
C
C Copyright (C) 1991-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module DSKIZ2
C
      SUBROUTINE DSKIZ2(LABEL1,LABEL2,LABEL3,SKIZ,NCOL,LZERO,LNZERO)
C
      INTEGER LABEL1(0:*),LABEL2(0:*),LABEL3(0:*),SKIZ(*),NCOL
      LOGICAL LZERO,LNZERO
C
C Detects boundary pixels between zones of influence.  Data for three
C adjacent rows are passed via arrays LABEL1, LABEL2 and LABEL3 and the
C result is returned in array SKIZ.  NCOL is the row length.  Boundary
C pixels are returned set to 1 and all other other pixels are set to
C zero.  The range flags LZERO and LNZERO are set as appropriate.
C Boundary pixels are pixels in the zones of influence that are
C negative, or which have a positive, non-equal 4-neighbour.
C
      INTEGER   I,LABEL
C
C Process row of data
C
      DO 10 I=1,NCOL
C
C Fetch current label value
C
         LABEL=LABEL2(I)
C
C Non-zero result if zero label or if label is not equal to non-zero
C 4-neighbour label value
C
         IF (LABEL.LT.0.OR.
     +       (LABEL1(I  ).GT.0.AND.LABEL1(I  ).NE.LABEL).OR.
     +       (LABEL2(I-1).GT.0.AND.LABEL2(I-1).NE.LABEL).OR.
     +       (LABEL2(I+1).GT.0.AND.LABEL2(I+1).NE.LABEL).OR.
     +       (LABEL3(I  ).GT.0.AND.LABEL3(I  ).NE.LABEL)) THEN
            SKIZ(I)=1
            LNZERO=.TRUE.
         ELSE
            SKIZ(I)=0
            LZERO=.TRUE.
         ENDIF
   10 CONTINUE
C
      RETURN
C
C Copyright (C) 1991-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
