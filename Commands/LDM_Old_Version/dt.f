C Semper 6 processing module DT
C
      SUBROUTINE DT
C
C Calculates distance transform from specified foreground/background
C region(s) defined by the source picture.
C
      INTEGER IVALPN !,IPACK
      LOGICAL CONOPT,OPT,SEMOPN,SEMROW,DTRAN1,DTRAN3,SEMRNG,SEMMED
C
      INTEGER IOP,NPIC,NCOL,NROW,NLAY,J,K1,K2,FORM,IMAX,MEDIUM
      LOGICAL LBG,LCLOSE,LZERO,LNZERO
      REAL    RMAX
C
      INCLUDE 'COMMON'
C
      REAL    DIST(LNBUF/LNINT)
      INTEGER IDIST(0:LNBUF/LNINT+1,0:1)
      INTEGER DX(0:LNBUF/LNINT+1,0:1)
      INTEGER DY(0:LNBUF/LNINT+1,0:1)
C
      EQUIVALENCE (DIST,IDIST,RB1),(DX,RB3),(DY,RB5)
C
C Fault conflicting options FG and BG
C
      IF (CONOPT(9880,3480)) GOTO 50
C
C See if option BG is set
C
      LBG=OPT(3480)
C
C Fault conflicting options OPEN and CLOSED
C
      IF (CONOPT(24645,5295)) GOTO 50
C
C See if option CLOSED is set
C
      LCLOSE=OPT(5295)
C
C Fault conflicting options CIRCLE, DIAMOND, SQUARE and OCTAGON
C
      IF (CONOPT(5178,6761))  GOTO 50
      IF (CONOPT(5178,31101))   GOTO 50
      IF (CONOPT(5178,24140))  GOTO 50
      IF (CONOPT(6761,31101))  GOTO 50
      IF (CONOPT(6761,24140)) GOTO 50
      IF (CONOPT(31101,24140))  GOTO 50
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
C Determine data form for output picture (FP for Euclidean distance
C transform and INTEGER otherwise) and size of intermediate picture
C
      IF (IOP.EQ.0) THEN
         FORM=NFMFP
         NLAY=2
      ELSE
         FORM=NFMINT
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
      IF (SEMOPN(2,NPIC,NCOL,NROW,1,NCLIMA,FORM,LP2)) GOTO 50
C
C Fetch medium associated with output picture
C
      IF (SEMMED(DEVN(LP2),MEDIUM)) GOTO 50
C
C If output picture is not suitable for intermediate data storage
C (wrong size/form or display picture)
C
      IF (IOP.EQ.0.OR.MEDIUM.EQ.MEDDS) THEN
         LP3=0
         IF (SEMOPN(3,0,NCOL,NROW,NLAY,NCLIMA,NFMINT,LP3)) GOTO 50
      ELSE
         LP3=LP2
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
         IF (DTRAN1(1,LBG,LCLOSE,DX(0,1),DY(0,1),0,NCOL,NROW,
     +              LZERO,LNZERO)) GOTO 50
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
            IF (DTRAN1(1,LBG,LCLOSE,DX(0,K2),DY(0,K2),J,NCOL,NROW,
     +                 LZERO,LNZERO)) GOTO 50
C
C Process row data
C
            CALL DTRAN2(1,DX(0,K1),DY(0,K1),DX(0,K2),DY(0,K2),NCOL)
C
C Store intermediate results in temporary picture
C
            IF (SEMROW(2,DX(1,K2),NFMINT,J,1,LP3)) GOTO 50
            IF (SEMROW(2,DY(1,K2),NFMINT,J,2,LP3)) GOTO 50
   10    CONTINUE
C
C Fault absence of structure in source picture
C
         IF (.NOT.(LZERO.AND.LNZERO)) GOTO 60
C
C Initialise range variable
C
         RMAX=0.0
C
C Fetch initial row of data
C
         IF (DTRAN1(2,LBG,LCLOSE,DX(0,0),DY(0,0),NROW+1,NCOL,NROW,
     +              LZERO,LNZERO)) GOTO 50
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
            IF (DTRAN1(2,LBG,LCLOSE,DX(0,K2),DY(0,K2),J,NCOL,NROW,
     +                 LZERO,LNZERO)) GOTO 50
C
C Process row data
C
            CALL DTRAN2(2,DX(0,K1),DY(0,K1),DX(0,K2),DY(0,K2),NCOL)
C
C Determine Euclidean distance from final integer offsets
C
            CALL DT2(DX(1,K2),DY(1,K2),DIST,NCOL,RMAX)
C
C Store final result in output picture
C
            IF (SEMROW(2,DIST,NFMFP,J,1,LP2)) GOTO 50
   20    CONTINUE
C
C Otherwise, calculate distance transform based on square grid
C
      ELSE
C
C Fetch initial row of data
C
         IF (DTRAN3(1,LBG,LCLOSE,IDIST(0,1),0,NCOL,NROW,
     +              LZERO,LNZERO)) GOTO 50
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
            IF (DTRAN3(1,LBG,LCLOSE,IDIST(0,K2),J,NCOL,NROW,
     +                 LZERO,LNZERO)) GOTO 50
C
C Process row data
C
            CALL DTRAN4(1,IOP,IDIST(0,K1),IDIST(0,K2),NCOL,IMAX)
C
C Store intermediate results
C
            IF (SEMROW(2,IDIST(1,K2),NFMINT,J,1,LP3)) GOTO 50
   30    CONTINUE
C
C Fault absence of structure in source picture
C
         IF (.NOT.(LZERO.AND.LNZERO)) GOTO 60
C
C Initialise range variable
C
         IMAX=0
C
C Fetch initial row of data
C
         IF (DTRAN3(2,LBG,LCLOSE,IDIST(0,0),NROW+1,NCOL,NROW,
     +              LZERO,LNZERO)) GOTO 50
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
            IF (DTRAN3(2,LBG,LCLOSE,IDIST(0,K2),J,NCOL,NROW,
     +                 LZERO,LNZERO)) GOTO 50
C
C Process row data
C
            CALL DTRAN4(2,IOP,IDIST(0,K1),IDIST(0,K2),NCOL,IMAX)
C
C Store final result in output picture
C
            IF (SEMROW(2,IDIST(1,K2),NFMINT,J,1,LP2)) GOTO 50
   40    CONTINUE
C
C Set up final range value
C
         RMAX=REAL(IMAX)
      ENDIF
C
C Store output range values in picture label
C
      IF (SEMRNG(2,0.0,RMAX,LP2)) GOTO 50
C
   50 RETURN
C
C Fault source picture with no structure in it
C
   60 ERROR=12
      IDERR=IVALPN(10335)
      GOTO 50
C
C Copyright (C) 1991:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module DT2
C
      SUBROUTINE DT2(DX,DY,DIST,NCOL,RMAX)
C
      INTEGER DX(*),DY(*),NCOL
      REAL    DIST(*),RMAX
C
C Evaluate final Euclidean distances from offsets to nearest
C background pixel and update range variable RMAX if less than
C largest distance calculated
C
      INTEGER   I
      INTEGER*4 IDX,IDY
C
      DO 10 I=1,NCOL
         IDX=DX(I)
         IDY=DY(I)
         DIST(I)=SQRT(REAL(IDX*IDX+IDY*IDY))
         RMAX=MAX(RMAX,DIST(I))
   10 CONTINUE
C
      RETURN
C
C Copyright (C) 1991:  Synoptics Ltd,  All Rights Reserved
C
      END
