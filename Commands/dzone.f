C Semper 6 processing module DZONE
C
      SUBROUTINE DZONE
C
C Outputs the zones of influence of the regions defined in the source
C picture.  Each region is labelled with a different positive integer
C value and may be disjoint.  Every pixel in the output picture is
C assigned the label of the nearest labelled pixel, or zero, if more
C than one labelled pixel is equidistant from the output pixel in
C question.  If the LABEL option is set, the source picture is taken
C to be a labelled image.  Otherwise, it is treated as a binary image
C and a labelled image is obtained from it prior to determining the
C zones of influence.
C
      INTEGER IPACK,IVALPN
      LOGICAL CONOPT,OPT,SEMOPN,SEMROW,DLABL1,DLABL3,DLABL5,SEMRNG
C
      INTEGER IOP,NPIC,NCOL,NROW,NLAY,I,J,K1,K2,IMIN,IMAX
      LOGICAL LZERO,LNZERO
C
      INCLUDE 'COMMON'
C
      INTEGER ZONE(LNBUF/LNINT)
      INTEGER LABEL(0:LNBUF/LNINT+1,0:1)
      INTEGER DIST(0:LNBUF/LNINT+1,0:1)
      INTEGER DX(0:LNBUF/LNINT+1,0:1)
      INTEGER DY(0:LNBUF/LNINT+1,0:1)
C
      EQUIVALENCE (ZONE,LABEL,RB1),(DIST,DX,RB3),(DY,RB5)
C
C Fault conflicting options CIRCLE, DIAMOND, SQUARE and OCTAGON
C
      IF (CONOPT(5178,6761))  GOTO 70
      IF (CONOPT(5178,31101))   GOTO 70
      IF (CONOPT(5178,24140))  GOTO 70
      IF (CONOPT(6761,31101))  GOTO 70
      IF (CONOPT(6761,24140)) GOTO 70
      IF (CONOPT(31101,24140))  GOTO 70
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
C Determine size of temporary picture to store intermediate label and
C distance information
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
      IF (SEMOPN(2,NPIC,NCOL,NROW,1,NCLIMA,NFMINT,LP2)) GOTO 70
C
C Open temporary picture to store intermediate results
C
      LP3=0
      IF (SEMOPN(3,0,NCOL,NROW,NLAY,NCLIMA,NFMINT,LP3)) GOTO 70
C
C LABEL option specifies a labelled source image
C
      IF (.NOT.OPT(19242)) THEN
C
C Source image not labelled, so treat it as binary image and generate
C labeled image from it
C
         IF (DLABL1()) GOTO 70
C
C Labeled image in temporary picture becomes source image
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
     +              LZERO,LNZERO)) GOTO 70
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
     +                 LZERO,LNZERO)) GOTO 70
C
C Process row data
C
            CALL DLABL4(1,LABEL(0,K1),DX(0,K1),DY(0,K1),
     +                    LABEL(0,K2),DX(0,K2),DY(0,K2),NCOL)
C
C Store intermediate results
C
            IF (SEMROW(2,LABEL(1,K2),NFMINT,J,1,LP3)) GOTO 70
            IF (SEMROW(2,   DX(1,K2),NFMINT,J,2,LP3)) GOTO 70
            IF (SEMROW(2,   DY(1,K2),NFMINT,J,3,LP3)) GOTO 70
   10    CONTINUE
C
C Fault absence of structure in source picture
C
         IF (.NOT.(LZERO.AND.LNZERO)) GOTO 80
C
C Fetch initial row of data
C
         IF (DLABL3(2,LABEL(0,0),DX(0,0),DY(0,0),NROW+1,NCOL,NROW,
     +              LZERO,LNZERO)) GOTO 70
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
     +                 LZERO,LNZERO)) GOTO 70
C
C Process row data
C
            CALL DLABL4(2,LABEL(0,K1),DX(0,K1),DY(0,K1),
     +                    LABEL(0,K2),DX(0,K2),DY(0,K2),NCOL)
C
C Store labelled result
C
            IF (SEMROW(2,LABEL(1,K2),NFMINT,J,1,LP3)) GOTO 70
   20    CONTINUE
C
C Otherwise, calculate distance transform based on square grid
C
      ELSE
C
C Fetch initial row of data
C
         IF (DLABL5(1,LABEL(0,1),DIST(0,1),0,NCOL,NROW,
     +              LZERO,LNZERO)) GOTO 70
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
     +                 LZERO,LNZERO)) GOTO 70
C
C Process row data
C
            CALL DLABL6(1,IOP,LABEL(0,K1),DIST(0,K1),
     +                        LABEL(0,K2),DIST(0,K2),NCOL)
C
C Store intermediate results
C
            IF (SEMROW(2,LABEL(1,K2),NFMINT,J,1,LP3)) GOTO 70
            IF (SEMROW(2, DIST(1,K2),NFMINT,J,2,LP3)) GOTO 70
   30    CONTINUE
C
C Fault absence of structure in source picture
C
         IF (.NOT.(LZERO.AND.LNZERO)) GOTO 80
C
C Fetch initial row of data
C
         IF (DLABL5(2,LABEL(0,0),DIST(0,0),NROW+1,NCOL,NROW,
     +              LZERO,LNZERO)) GOTO 70
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
     +                 LZERO,LNZERO)) GOTO 70
C
C Process row data
C
            CALL DLABL6(2,IOP,LABEL(0,K1),DIST(0,K1),
     +                        LABEL(0,K2),DIST(0,K2),NCOL)
C
C Store labelled result
C
            IF (SEMROW(2,LABEL(1,K2),NFMINT,J,1,LP3)) GOTO 70
   40    CONTINUE
      ENDIF
C
C Initialise output range variables
C
      IMIN=32767
      IMAX=0
C
C Final pass to set equidistant pixels to zero
C
      DO 60 J=1,NROW
C
C Fetch next row of data
C
         IF (SEMROW(1,ZONE,NFMINT,J,1,LP3)) GOTO 70
C
C Set equidistant pixels to zero and update output range variables
C
         DO 50 I=1,NCOL
            IF (ZONE(I).LT.0) THEN
               ZONE(I)=0
               IMIN=0
            ELSE
               IMIN=MIN(ZONE(I),IMIN)
               IMAX=MAX(ZONE(I),IMAX)
            ENDIF
   50    CONTINUE
C
C Store final result in output picture
C
         IF (SEMROW(2,ZONE,NFMINT,J,1,LP2)) GOTO 70
   60 CONTINUE
C
C Store output range values in picture label
C
      IF (SEMRNG(2,REAL(IMIN),REAL(IMAX),LP2)) GOTO 70
C
   70 RETURN
C
C Fault source picture with no structure in it
C
   80 ERROR=12
      IDERR=IVALPN(10335)
      GOTO 70
C
C Copyright (C) 1991-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
