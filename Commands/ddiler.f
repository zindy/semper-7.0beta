C Semper 6 processing module DDILER
C
      SUBROUTINE DDILER
C
C Implements commands DERODE and DDILATE to carry out erosion and
C dilation with diamond/square/octagon/circle shaped disc of given
C radius by calculating distance transform for foreground or background
C regions defined by the source picture.
C
      REAL    VAL
      INTEGER IPACK,IVALPN
      LOGICAL CONOPT,OPT,SEMOPN,SEMROW,DTRAN1,DTRAN3,SEMRNG
C
      INTEGER*4 NC4
      INTEGER IOP,NPIC,NCOL,NROW,NLAY,I,J,K1,K2,IMIN,IMAX,DMAX
      LOGICAL LBG,LZERO,LNZERO
      REAL    R
C
      INCLUDE 'COMMON'
C
      INTEGER PIXEL(LNBUF/LNINT)
      INTEGER IDIST(0:LNBUF/LNINT+1,0:1)
      INTEGER DX(0:LNBUF/LNINT+1,0:1)
      INTEGER DY(0:LNBUF/LNINT+1,0:1)
C
      EQUIVALENCE (PIXEL,RB1),(IDIST,DX,RB3),(DY,RB5)
C
C See if command DDILATE was invoked (means distance transform for
C background rather than foreground regions is required)
C
      LBG=VERB.EQ.6569
C
C Fetch value for RADIUS key
C
      R=VAL(28844)
C
C Fault negative radius value
C
      IF (R.LT.0.0) THEN
         ERROR=3
         IDERR=28844
         GOTO 60
      ENDIF
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
C Determine size of intermediate picture
C
      IF (IOP.EQ.0) THEN
         NLAY=2
      ELSE
         NLAY=1
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
C Open temporary picture for storing intermediate results
C
      LP3=0
      IF (SEMOPN(3,0,NCOL,NROW,NLAY,NCLIMA,NFMINT,LP3)) GOTO 60
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
         IF (DTRAN1(1,LBG,.FALSE.,DX(0,1),DY(0,1),0,NCOL,NROW,
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
            IF (DTRAN1(1,LBG,.FALSE.,DX(0,K2),DY(0,K2),J,NCOL,NROW,
     +                 LZERO,LNZERO)) GOTO 60
C
C Process row data
C
            CALL DTRAN2(1,DX(0,K1),DY(0,K1),DX(0,K2),DY(0,K2),NCOL)
C
C Store intermediate results
C
            IF (SEMROW(2,DX(1,K2),NFMINT,J,1,LP3)) GOTO 60
            IF (SEMROW(2,DY(1,K2),NFMINT,J,2,LP3)) GOTO 60
   10    CONTINUE
C
C Check for absence of structure in source picture
C
         IF (.NOT.(LZERO.AND.LNZERO)) GOTO 70
C
C Initialise output range values
C
         IMIN=1
         IMAX=0
C
C Fetch initial row of data
C
         IF (DTRAN1(2,LBG,.FALSE.,DX(0,0),DY(0,0),NROW+1,NCOL,NROW,
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
            IF (DTRAN1(2,LBG,.FALSE.,DX(0,K2),DY(0,K2),J,NCOL,NROW,
     +                 LZERO,LNZERO)) GOTO 60
C
C Process row data
C
            CALL DTRAN2(2,DX(0,K1),DY(0,K1),DX(0,K2),DY(0,K2),NCOL)
C
C Calculate Euclidean distance and threshold the distance values,
C result = D > R (DERODE) or D < R (DDILATE)
C
            CALL DTHR1(LBG,DX(1,K2),DY(1,K2),PIXEL,NCOL,R,IMIN,IMAX)
C
C Store final result in output picture
C
            IF (SEMROW(2,PIXEL,NFMINT,J,1,LP2)) GOTO 60
   20    CONTINUE
C
C Otherwise, calculate distance transform based on square grid
C
      ELSE
C
C Fetch initial row of data
C
         IF (DTRAN3(1,LBG,.FALSE.,IDIST(0,1),0,NCOL,NROW,
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
            IF (DTRAN3(1,LBG,.FALSE.,IDIST(0,K2),J,NCOL,NROW,
     +                 LZERO,LNZERO)) GOTO 60
C
C Process row data
C
            CALL DTRAN4(1,IOP,IDIST(0,K1),IDIST(0,K2),NCOL,DMAX)
C
C Store intermediate results
C
            IF (SEMROW(2,IDIST(1,K2),NFMINT,J,1,LP3)) GOTO 60
   30    CONTINUE
C
C Check for absence of structure in source picture
C
         IF (.NOT.(LZERO.AND.LNZERO)) GOTO 70
C
C Initialise output range values
C
         IMIN=1
         IMAX=0
C
C Fetch initial row of data
C
         IF (DTRAN3(2,LBG,.FALSE.,IDIST(0,0),NROW+1,NCOL,NROW,
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
            IF (DTRAN3(2,LBG,.FALSE.,IDIST(0,K2),J,NCOL,NROW,
     +                 LZERO,LNZERO)) GOTO 60
C
C Process row data
C
            CALL DTRAN4(2,IOP,IDIST(0,K1),IDIST(0,K2),NCOL,DMAX)
C
C Threshold the distance values, result = D > R (DERODE)
C                                       = D < R (DDILATE)
C
            CALL DTHR2(LBG,IDIST(1,K2),PIXEL,NCOL,R,IMIN,IMAX)
C
C Store final result in output picture
C
            IF (SEMROW(2,PIXEL,NFMINT,J,1,LP2)) GOTO 60
   40    CONTINUE
      ENDIF
C
C Store output range values in picture label
C
   50 IF (SEMRNG(2,REAL(IMIN),REAL(IMAX),LP2)) GOTO 60
C
   60 RETURN
C
C Source picture has no structure in it - set output picture to
C constant value (same as source picture value)
C
C Determine output pixel value
C
   70 IF (LZERO)  DMAX=0
      IF (LNZERO) DMAX=1
C
C Fill row buffer with constant output value
C
      DO 80 I=1,NCOL
         PIXEL(I)=DMAX
   80 CONTINUE
C
C Convert row buffer to byte form to avoid conversion on output
C
      NC4=NCOL
      CALL CFORM(PIXEL,PIXEL,NFMINT,NFMBYT,NC4)
C
C Fill output picture
C
      DO 90 J=1,NROW
         IF (SEMROW(2,PIXEL,NFMBYT,J,1,LP2)) GOTO 60
   90 CONTINUE
C
C Set up output range values
C
      IMIN=DMAX
      IMAX=DMAX
C
      GOTO 50
C
C Copyright (C) 1991,1993:  Synoptics Ltd,  All Rights Reserved
C
      END
