C Semper 6 processing module FLOOD
C
      SUBROUTINE FLOOD
C
C Floods the regions defined by the mask picture starting from the
C seed points defined in the seed picture.  The seed picture is either
C a binary image (where non-zero pixels are treated as ones) or a
C position list where each position specifies a seed point.  In
C addition, the POSITION,POS keys can be used to specify a single
C seed point.  The options FG and BG specify which combination of
C foreground and/or background regions are to br processed.
C
      LOGICAL OPT,VARSET,SEMOPN,SEMROW,FLOOD2,LABEL4,SEMRNG
      INTEGER IVALPN,IPACK
      REAL    VAL
C
      INTEGER LPS,LPT,CCOL,CROW,IYSEED,FG,FG1,FG2,J1,J2
      INTEGER SCOL,SROW,NSCOL,NSROW,NSLAY,CLASS,FORM,ISEED
      INTEGER I,J,K,N,KN,KO,KOF,KOP,KNF,KNP,KOB,OLDID,NEWID,NEXID
      INTEGER NCOL,NROW,NPIC,NCHD,NSTART,NEND,NOVER(0:1),IMIN,IMAX
      INTEGER LASCHD,MAXCHD,KOLD,NOLD,KNEW,NNEW,CURID,OLDLNK,NEWLNK
      REAL    X,Y,XMIN,XMAX,YMIN,YMAX
      LOGICAL LFG,LBG
C
      INCLUDE 'COMMON'
C
C Indices for chord parameters
C
      INTEGER NIDENT,NFORWP,NBACKP,NCOL1,NCOL2,NFGRND
      PARAMETER (NIDENT=1)
      PARAMETER (NFORWP=NIDENT+1)
      PARAMETER (NBACKP=NFORWP+1)
      PARAMETER (NCOL1=NBACKP+1)
      PARAMETER (NCOL2=NCOL1+1)
      PARAMETER (NFGRND=NCOL2+1)
C
C Number of parameters for each chord
C
      INTEGER NIPAR
      PARAMETER (NIPAR=NFGRND)
C
C Maximum number of chords in chord buffer
C
      INTEGER NCHORD
      PARAMETER (NCHORD=3*(LNBUF/(NIPAR*LNINT)))
C
      INTEGER SEED(LNBUF/LNINT)
      INTEGER ICOVER(NCHORD,4)
      INTEGER IB1(LNBUF/LNINT)
      INTEGER IB2(LNBUF/LNINT)
      INTEGER IXSEED(LNBUF/LNINT)
      INTEGER IB3(LNBUF/LNINT)
      INTEGER IB4(LNBUF/LNINT)
      INTEGER ICBUFF(NCHORD,NIPAR)
      REAL    XSEED(LNBUF/LNREAL)
      REAL    YSEED(LNBUF/LNREAL)
C
      EQUIVALENCE (SEED,ICOVER,IB1,RB1)
      EQUIVALENCE (IXSEED,IB2,RB2)
      EQUIVALENCE (IB3,RB3)
      EQUIVALENCE (IB4,ICBUFF,RB4)
      EQUIVALENCE (XSEED,RB5)
      EQUIVALENCE (YSEED,RB6)
C
C See if options FG and BG are set (default option is FG)
C
      LFG=OPT(9880)
      LBG=OPT(3480)
C
      IF (.NOT.(LFG.OR.LBG)) LFG=.TRUE.
C
C Set up loop indices for processing background and/or foreground chords
C
      IF (LBG) THEN
         FG1=0
      ELSE
         FG1=1
      ENDIF
C
      IF (LFG) THEN
         FG2=1
      ELSE
         FG2=0
      ENDIF
C
C Fetch mask picture size
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
C
C Fetch source picture centre position
C
      CCOL=CCOLN(LP1)
      CROW=CROWN(LP1)
C
C Establish coordinate limits for source picture
C
      XMIN=REAL(1-CCOL)
      XMAX=REAL(NCOL-CCOL)
      YMIN=REAL(CROW-NROW)
      YMAX=REAL(CROW-1)
C
C Initialise single seed point values to undefined state
C
      SCOL=0
      SROW=0
C
C See if POSITION key is set to define single seed point
C
      IF (VARSET(26219)) THEN
C
C Fetch specified X and Y position
C
         X=VAL(26219)
         Y=VAL(26232)
C
C If position within range of source picture, set up values for
C single seed point
C
         IF (X.GE.XMIN.AND.X.LE.XMAX.AND.Y.GE.YMIN.AND.Y.LE.YMAX) THEN
            SCOL=CCOL+NINT(X)
            SROW=CROW-NINT(Y)
         ENDIF
      ENDIF
C
C Initialise LP number for seed picture to undefined state
C
      LPS=0
C
C See if seed picture specified by means of WITH key
C
      IF (VARSET(-5181)) THEN
C
C Open input seed picture
C
         NPIC=IVALPN(-5181)
         IF (SEMOPN(1,NPIC,NSCOL,NSROW,NSLAY,CLASS,FORM,LP3)) GOTO 240
C
C See if seed picture is a position list
C
         IF (CLASS.EQ.NCLPLI) THEN
C
C Fault mask picture with too many rows to process
C
            IF (NROW.GT.LNBUF/LNINT) THEN
               ERROR=5
               IDERR=IVALPN(10335)
               GOTO 240
            ENDIF
C
C Open temporary seed picture
C
            IF (SEMOPN(3,0,NCOL,NROW,1,NCLIMA,NFMBYT,LPS)) GOTO 240
C
C Fetch seed point X and Y positions
C
            IF (SEMROW(1,XSEED,NFMFP,1,1,LP3)) GOTO 240
            IF (SEMROW(1,YSEED,NFMFP,1,2,LP3)) GOTO 240
C
C Initialise Y sort array
C
            DO 10 J=1,NROW
               IB3(J)=0
   10       CONTINUE
C
C Sort seed points into row-by-row order
C
            DO 20 I=1,NSCOL
C
C Reject seed point if outside source picture limits
C
               IF (XSEED(I).LT.XMIN.OR.XSEED(I).GT.XMAX.OR.
     +             YSEED(I).LT.YMIN.OR.YSEED(I).GT.YMAX) GOTO 20
C
C Round seed point position to nearest pixel position
C
               IXSEED(I)=CCOL+NINT(XSEED(I))
               IYSEED   =CROW-NINT(YSEED(I))
C
C Update linked-list values
C
               IB4(I)=IB3(IYSEED)
               IB3(IYSEED)=I
   20       CONTINUE
C
C Initialise seed point row buffer
C
            DO 30 I=1,NCOL
               SEED(I)=0
   30       CONTINUE
C
C Convert seed point positions into binary seed image
C
            DO 60 J=1,NROW
C
C Set seed points for this row
C
               I=IB3(J)
C
   40          IF (I.NE.0) THEN
                  SEED(IXSEED(I))=1
                  I=IB4(I)
                  GOTO 40
               ENDIF
C
C Store result for this row in temporary picture
C
               IF (SEMROW(2,SEED,NFMINT,J,1,LPS)) GOTO 240
C
C Reset seed points for this row
C
               I=IB3(J)
C
   50          IF (I.NE.0) THEN
                  SEED(IXSEED(I))=0
                  I=IB4(I)
                  GOTO 50
               ENDIF
   60       CONTINUE
C
C Otherwise, fault seed picture size if different from mask picture size
C
         ELSE
            IF (NSCOL.NE.NCOL.OR.NSROW.NE.NROW) THEN
               ERROR=5
               IDERR=NPIC
               GOTO 240
            ENDIF
C
C Seed image is obtained directly from picture specified by means of
C the WITH key
C
            LPS=LP3
         ENDIf
      ENDIF
C
C Open final output picture
C
      NPIC=IVALPN(-601)
      LP2=LP1
      IF (SEMOPN(2,NPIC,NCOL,NROW,1,NCLIMA,NFMBYT,LP2)) GOTO 240
C
C Open temporary picture for storing intermediate results
C
      LPT=0
      IF (SEMOPN(3,0,NCOL,NROW,1,NCLIMA,NFMINT,LPT)) GOTO 240
C
C First output row that would have to be labeled
C
      J1=1
C
C Set up limit for chord id's
C
      MAXCHD=MIN(LNBUF/LNINT,32766)
C
C Initialise counts and pointers
C
      NCHD=0
      LASCHD=0
C
C No old chords to start with
C
      KOLD=1
      NOLD=0
C
C Process source data
C
      DO 200 J=1,NROW+1
C
C Set up pointer for new chords
C
         KNEW=KOLD+NOLD
         IF (KNEW.GT.NCHORD) KNEW=KNEW-NCHORD
C
C Generate new chord data (includes check for overlap with seed data)
C
         IF (FLOOD2(LP1,LPS,SCOL,SROW,J,FG1,FG2,NCHORD-NOLD,KNEW,NNEW))
     +      GOTO 240
C
C Look for start and end chords and any chord overlaps
C
         CALL LABEL3(KOLD,NOLD,KNEW,NNEW,FG1,FG2,NSTART,NEND,NOVER)
C
C If we are going to run out of chord id's, we must relabel the output
C data in the hope that this will free up sufficient chord id's
C
         IF (NCHD+NSTART.GT.MAXCHD) THEN
C
C Initialise relabeling array
C
            DO 70 I=1,LASCHD
               IB2(I)=0
   70       CONTINUE
C
C Zero chord count
C
            NCHD=0
C
C Generate relabeling array from information in link table
C
            DO 90 I=1,LASCHD
C
C Entries greater than MAXCHD denote flooded regions
C
               IF (IB3(I).GT.MAXCHD) THEN
                  IB2(I)=-1
C
C Otherwise, assign new chord id for unfinished region
C
               ELSE IF (IB2(I).EQ.0.AND.IB3(I).NE.0) THEN
C
C Establish new chord id
C
                  NCHD=NCHD+1
C
C Initialise seed flag
C
                  ISEED=1
C
C Set all linked entries in relabeling array to new chord id, looking
C for any flagged entries along the way
C
                  CURID=I
C
   80             IB2(CURID)=NCHD
                  IF (IB3(CURID).LT.0) ISEED=-1
                  CURID=ABS(IB3(CURID))
C
                  IF (CURID.NE.I) GOTO 80
C
C Initialise new link table entry (negative if seeded region)
C
                  IB3(NCHD)=SIGN(NCHD,ISEED)
               ENDIF
   90       CONTINUE
C
C Fault more chords than available chord id's
C
            IF (NCHD+NSTART.GT.MAXCHD) THEN
               ERROR=77
               IDMESS='Too many chords'
               GOTO 240
            ENDIF
C
C Relabel the output results so far
C
            J2=J-1
C
            IF (LABEL4(LPT,J1,J2)) GOTO 240
C
C Relabel old chords
C
            KO=KOLD
C
            DO 100 K=1,NOLD
               IF (ICBUFF(KO,NIDENT).GT.0) THEN
                  ICBUFF(KO,NIDENT)=IB2(ICBUFF(KO,NIDENT))
               ENDIF
C
               KO=KO+1
               IF (KO.GT.NCHORD) KO=1
  100       CONTINUE
C
C Re-establish data for start and end chords and any chord overlaps
C (use of RB1 and RB2 to relabel overwrote data produced by LABEL3)
C
            CALL LABEL3(KOLD,NOLD,KNEW,NNEW,FG1,FG2,NSTART,NEND,NOVER)
         ENDIF
C
C Process start chords
C
         DO 110 K=1,NSTART
C
C Fetch start chord index
C
            KN=ICOVER(K,1)
C
C Fetch seed flag
C
            ISEED=ICBUFF(KN,NIDENT)
C
C Establish new chord id
C
            NCHD=NCHD+1
C
C Store new chord id
C
            ICBUFF(KN,NIDENT)=NCHD
C
C Initialise link table entry (negative if seed chord)
C
            IB3(NCHD)=SIGN(NCHD,ISEED)
C
C Initialise link pointers (start chord points to itself)
C
            ICBUFF(KN,NFORWP)=KN
            ICBUFF(KN,NBACKP)=KN
  110    CONTINUE
C
C Process end chords
C
         DO 140 K=1,NEND
C
C Fetch end chord index
C
            KO=ICOVER(K,2)
C
C If chord points to itself, end chord is last chord in region
C
            IF (ICBUFF(KO,NFORWP).EQ.KO) THEN
C
C Fetch old chord id
C
               OLDID=ICBUFF(KO,NIDENT)
C
C Initialise seed flag
C
               ISEED=0
C
C See if any associated entries in link table have been flagged
C
               CURID=OLDID
C
  120          IF (IB3(CURID).LT.0) ISEED=MAXCHD+1
               CURID=ABS(IB3(CURID))
C
               IF (CURID.NE.OLDID) GOTO 120
C
C Update link table (0 = unflooded region, MAXCHD + 1 = flooded region)
C
  130          NEXID=ABS(IB3(CURID))
               IB3(CURID)=ISEED
               CURID=NEXID
C
               IF (CURID.NE.OLDID) GOTO 130
C
C Otherwise, remove end chord from linked list
C
            ELSE
               KOF=ICBUFF(KO,NFORWP)
               KOB=ICBUFF(KO,NBACKP)
C
               ICBUFF(KOB,NFORWP)=KOF
               ICBUFF(KOF,NBACKP)=KOB
            ENDIF
  140    CONTINUE
C
C Process chord overlaps
C
         K=0
C
         DO 170 FG=FG1,FG2
C
C Set up null indices to previous overlapping chords
C
            KOP=0
            KNP=0
C
C Process overlaps for background or foreground chords
C
            DO 160 N=1,NOVER(FG)
C
C Fetch chord indices
C
               K=K+1
C
               KO=ICOVER(K,3)
               KN=ICOVER(K,4)
C
C Fetch old and new chord id
C
               OLDID=ICBUFF(KO,NIDENT)
               NEWID=ICBUFF(KN,NIDENT)
C
C If old chords are the same, we have a fork
C
               IF (KO.EQ.KOP) THEN
C
C Seed flag = new chord id
C
                  ISEED=NEWID
C
C Update link table entry to record presence of seed chord
C
                  IF (ISEED.LT.0) IB3(OLDID)=-ABS(IB3(OLDID))
C
C Set new chord id to old chord id
C
                  ICBUFF(KN,NIDENT)=OLDID
C
C Insert new chord in linked list after previous new chord
C
                  KNF=ICBUFF(KNP,NFORWP)
C
                  ICBUFF(KNP,NFORWP)=KN
                  ICBUFF(KN,NBACKP)=KNP
C
                  ICBUFF(KN,NFORWP)=KNF
                  ICBUFF(KNF,NBACKP)=KN
C
C If new chords are the same, we have a join
C
               ELSE IF (KN.EQ.KNP) THEN
C
C If old and new chord id's not the same, we must relabel to make
C them the same
C
                  IF (OLDID.NE.NEWID) THEN
C
C Set id's for all chords linked to old chord to new chord id
C
                     KOF=KO
C
  150                ICBUFF(KOF,NIDENT)=NEWID
                     KOF=ICBUFF(KOF,NFORWP)
C
                     IF (KOF.NE.KO) GOTO 150
C
C Merge together linked lists linking old and new chords
C
                     KOF=ICBUFF(KO,NFORWP)
                     KNF=ICBUFF(KN,NFORWP)
C
                     ICBUFF(KO,NFORWP)=KNF
                     ICBUFF(KNF,NBACKP)=KO
C
                     ICBUFF(KN,NFORWP)=KOF
                     ICBUFF(KOF,NBACKP)=KN
C
C Merge linked lists associated with old and new chords in link table
C
                     OLDLNK=IB3(OLDID)
                     NEWLNK=IB3(NEWID)
C
                     IB3(OLDID)=SIGN(NEWLNK,IB3(OLDID))
                     IB3(NEWID)=SIGN(OLDLNK,IB3(NEWID))
                  ENDIF
C
C Otherwise, we have a straight overlap between old and new chord
C
               ELSE
C
C Seed flag = new chord id
C
                  ISEED=NEWID
C
C Update link table entry to record presence of seed chord
C
                  IF (ISEED.LT.0) IB3(OLDID)=-ABS(IB3(OLDID))
C
C Set new chord id to old chord id
C
                  ICBUFF(KN,NIDENT)=OLDID
C
C Insert new chord in linked list after old chord
C
                  KOF=ICBUFF(KO,NFORWP)
C
                  ICBUFF(KO,NFORWP)=KN
                  ICBUFF(KN,NBACKP)=KO
C
                  ICBUFF(KN,NFORWP)=KOF
                  ICBUFF(KOF,NBACKP)=KN
               ENDIF
C
C Remove old chord from linked list (if not done previously)
C
               IF (KO.NE.KOP) THEN
                  KOF=ICBUFF(KO,NFORWP)
                  KOB=ICBUFF(KO,NBACKP)
C
                  ICBUFF(KOB,NFORWP)=KOF
                  ICBUFF(KOF,NBACKP)=KOB
               ENDIF
C
C Current chords become previous chords for next loop
C
               KOP=KO
               KNP=KN
  160       CONTINUE
  170    CONTINUE
C
C If current row not outside picture limits, write resulting row
C to output picture
C
         IF (.NOT.(J.LT.1.OR.J.GT.NROW)) THEN
C
C Convert new chords back into pixel representation
C
            KN=KNEW
C
            DO 190 K=1,NNEW
C
C Fetch chord id
C
               NEWID=ICBUFF(KN,NIDENT)
C
C Set corresponding pixels to chord id
C
               DO 180 I=ICBUFF(KN,NCOL1),ICBUFF(KN,NCOL2)
                  IB1(I)=NEWID
  180          CONTINUE
C
C Increment chord index
C
               KN=KN+1
               IF (KN.GT.NCHORD) KN=1
  190       CONTINUE
C
C Write data to output picture
C
            IF (SEMROW(2,IB1,NFMINT,J,1,LPT)) GOTO 240
         ENDIF
C
C Record number of chords at the end of this pass
C
         LASCHD=NCHD
C
C New chords become old chords on next pass
C
         KOLD=KNEW
         NOLD=NNEW
  200 CONTINUE
C
C Generate relabeling array to achieve final result.  By this stage the
C link table should contain only zero values for unflooded regions and
C non-zero values (MAXCHD+1) for flooded regions.
C
      DO 210 I=1,LASCHD
         IF (IB3(I).EQ.0) THEN
            IB2(I)=0
         ELSE
            IB2(I)=1
         ENDIF
  210 CONTINUE
C
C Initialise range values for output picture
C
      IMIN=1
      IMAX=0
C
C Convert labeled image into final form
C
      DO 230 J=1,NROW
C
C Fetch row of partially labeled data
C
         IF (SEMROW(1,IB1,NFMINT,J,1,LPT)) GOTO 240
C
C Relabel data into final form
C
         DO 220 I=1,NCOL
            IF (IB1(I).GT.0) THEN
               IB1(I)=IB2(IB1(I))
            ELSE IF (IB1(I).LT.0) THEN
               IB1(I)=1
            ENDIF
C
C Update range values
C
            IF (IB1(I).EQ.0) THEN
               IMIN=0
            ELSE
               IMAX=1
            ENDIF
  220    CONTINUE
C
C Write final result to output picture
C
         IF (SEMROW(2,IB1,NFMINT,J,1,LP2)) GOTO 240
  230 CONTINUE
C
C Store output range in picture label
C
      IF (SEMRNG(2,REAL(IMIN),REAL(IMAX),LP2)) GOTO 240
C
  240 RETURN
C
C Copyright (C) 1991:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module FLOOD2
C
      LOGICAL FUNCTION FLOOD2(LPM,LPS,SCOL,SROW,ROW,FG1,FG2,NMAX,KNEW,
     +                        NNEW)
C
      INTEGER LPM,LPS,SCOL,SROW,ROW,FG1,FG2,NMAX,KNEW,NNEW
C
C Reads the row ROW from mask picture LPM and locates all foreground
C and background chords along that row.  A pixel is a background pixel
C if its value is zero.  The chord id is set to -1 for any chords which
C overlap non-zero pixels in the seed picture LPS or if the chord
C overlaps the additional seed point at position (SCOL,SROW), and to 1
C otherwise.  If LPS is zero, the seed image does not exist.  The chord
C buffer will accept up to NMAX chords starting at KNEW.  NNEW returns
C the number of chords found.  FG1 and FG2 define which combination of
C background and/or foreground regions are ultimately to be processed.
C Chords that do not take part in the flooding process are returned with
C a zero chord id.
C
      LOGICAL SEMROW
C
      INTEGER NCOL,NROW,I,KN
C
      INCLUDE 'COMMON'
C
C Indices for chord parameters
C
      INTEGER NIDENT,NFORWP,NBACKP,NCOL1,NCOL2,NFGRND
      PARAMETER (NIDENT=1)
      PARAMETER (NFORWP=NIDENT+1)
      PARAMETER (NBACKP=NFORWP+1)
      PARAMETER (NCOL1=NBACKP+1)
      PARAMETER (NCOL2=NCOL1+1)
      PARAMETER (NFGRND=NCOL2+1)
C
C Number of parameters for each chord
C
      INTEGER NIPAR
      PARAMETER (NIPAR=NFGRND)
C
C Maximum number of chords in chord buffer
C
      INTEGER NCHORD
      PARAMETER (NCHORD=3*(LNBUF/(NIPAR*LNINT)))
C
      INTEGER MASK(LNBUF/LNINT)
      INTEGER SEED(LNBUF/LNINT)
      INTEGER ICBUFF(NCHORD,NIPAR)
C
      EQUIVALENCE (MASK,RB1),(SEED,RB2),(ICBUFF,RB4)
C
      FLOOD2=.TRUE.
C
C Initialise chord count
C
      NNEW=0
C
C Fetch mask picture size
C
      NCOL=NCOLS(LPM)
      NROW=NROWS(LPM)
C
C If row outside mask picture, return no new chords
C
      IF (ROW.LT.1.OR.ROW.GT.NROW) GOTO 60
C
C Read in mask picture row
C
      IF (SEMROW(1,MASK,NFMINT,ROW,1,LPM)) GOTO 70
C
C Read in seed picture row (if seed image exists)
C
      IF (LPS.NE.0) THEN
          IF (SEMROW(1,SEED,NFMINT,ROW,1,LPS)) GOTO 70
      ENDIF
C
C Initialise chord pointer
C
      KN=KNEW
C
C Initialise pixel pointer
C
      I=1
C
C See if leftmost pixel begins foreground chord
C
      IF (MASK(1).NE.0) GOTO 30
C
C Start of background chord - fault full chord buffer
C
   10 IF (NNEW.EQ.NMAX) GOTO 80
C
C Increment chord count
C
      NNEW=NNEW+1
C
C Initialise chord id, record chord start position and set flag for
C background chord
C
      ICBUFF(KN,NIDENT)=1
      ICBUFF(KN,NCOL1)=I
      ICBUFF(KN,NFGRND)=0
C
C If seed data defined and seed pixel non-zero, set chord id to flag
C this (1 = no seed, -1 = seed)
C
   20 IF (LPS.NE.0) THEN
         IF (SEED(I).NE.0) ICBUFF(KN,NIDENT)=-1
      ENDIF
C
C Move on to next pixel
C
      I=I+1
C
C Return if end of row
C
      IF (I.GT.NCOL) GOTO 50
C
C If background pixel, keep scanning for more
C
      IF (MASK(I).EQ.0) GOTO 20
C
C Record chord end position
C
      ICBUFF(KN,NCOL2)=I-1
C
C If chord overlaps single seed point, set chord id to flag this
C
      IF (ROW.EQ.SROW) THEN
         IF (ICBUFF(KN,NCOL1).LE.SCOL.AND.ICBUFF(KN,NCOL2).GE.SCOL) THEN
            ICBUFF(KN,NIDENT)=-1
         ENDIF
      ENDIF
C
C If chord is not involved in flooding process, reset chord id
C
      IF (FG1.GT.0) ICBUFF(KN,NIDENT)=0
C
C Increment chord pointer
C
      KN=KN+1
      IF (KN.GT.NCHORD) KN=1
C
C Start of foreground chord - fault full chord buffer
C
   30 IF (NNEW.EQ.NMAX) GOTO 80
C
C Increment chord count
C
      NNEW=NNEW+1
C
C Initialise chord id, record chord start position and set flag for
C foreground chord
C
      ICBUFF(KN,NIDENT)=1
      ICBUFF(KN,NCOL1)=I
      ICBUFF(KN,NFGRND)=1
C
C If seed data defined and seed pixel non-zero, set chord id to flag
C this
C
   40 IF (LPS.NE.0) THEN
         IF (SEED(I).NE.0) ICBUFF(KN,NIDENT)=-1
      ENDIF
C
C Move on to next pixel
C
      I=I+1
C
C Return if end of row
C
      IF (I.GT.NCOL) GOTO 50
C
C If foreground pixel, keep scanning for more
C
      IF (MASK(I).NE.0) GOTO 40
C
C Record chord end position
C
      ICBUFF(KN,NCOL2)=I-1
C
C If chord overlaps single seed point, set chord id to flag this
C
      IF (ROW.EQ.SROW) THEN
         IF (ICBUFF(KN,NCOL1).LE.SCOL.AND.ICBUFF(KN,NCOL2).GE.SCOL) THEN
            ICBUFF(KN,NIDENT)=-1
         ENDIF
      ENDIF
C
C If chord is not involved in flooding process, reset chord id
C
      IF (FG2.LT.1) ICBUFF(KN,NIDENT)=0
C
C Increment chord pointer
C
      KN=KN+1
      IF (KN.GT.NCHORD) KN=1
C
C Go back and scan past next background chord
C
      GOTO 10
C
C Record last chord end position
C
   50 ICBUFF(KN,NCOL2)=NCOL
C
C If last chord overlaps single seed point, set chord id to flag this
C
      IF (ROW.EQ.SROW) THEN
         IF (ICBUFF(KN,NCOL1).LE.SCOL.AND.ICBUFF(KN,NCOL2).GE.SCOL) THEN
            ICBUFF(KN,NIDENT)=-1
         ENDIF
      ENDIF
C
C If last chord is not involved in flooding process, reset chord id
C
      IF (ICBUFF(KN,NFGRND).LT.FG1.OR.ICBUFF(KN,NFGRND).GT.FG2) THEN
         ICBUFF(KN,NIDENT)=0
      ENDIF
C
   60 FLOOD2=.FALSE.
C
   70 RETURN
C
C Fault full chord buffer
C
   80 ERROR=150
      GOTO 70
C
C Copyright (C) 1991:  Synoptics Ltd,  All Rights Reserved
C
      END
