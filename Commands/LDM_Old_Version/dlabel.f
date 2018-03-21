C Semper 6 subsidiary module DLABL1
C
      LOGICAL FUNCTION DLABL1()
C
C Generates a labeled version of the source picture where each
C connected region is uniquely numbered and put the result in the
C temporary picture pointed to by LP3
C
      LOGICAL OPT,SEMROW,DLABL2,LABEL4
C     INTEGER IPACK
C
      INTEGER I,J,K,N,KN,KO,KOF,KOP,KNF,KNP,KOB,FG,FG1,FG2
      INTEGER NEWID,OLDID,CURID,NEXID,OLDLNK,NEWLNK,J1,J2
      INTEGER NCOL,NROW,NREG,NCHD,NSTART,NEND,NOVER(0:1)
      INTEGER LASCHD,MAXCHD,MAXREG,KOLD,NOLD,KNEW,NNEW
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
      INTEGER ICOVER(NCHORD,4)
      INTEGER IB1(LNBUF/LNINT)
      INTEGER IB2(LNBUF/LNINT)
      INTEGER IB3(LNBUF/LNINT)
      INTEGER ICBUFF(NCHORD,NIPAR)
C
      EQUIVALENCE (ICOVER,IB1,RB1)
      EQUIVALENCE (IB2,RB2)
      EQUIVALENCE (IB3,RB3)
      EQUIVALENCE (ICBUFF,RB4)
C
      DLABL1=.TRUE.
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
C Fetch source picture size
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
C
C First output row that would have to be relabeled
C
      J1=1
C
C Set up limits for chord and region id's
C
      MAXCHD=MIN(LNBUF/LNINT,32766)
      MAXREG=32767
C
C Initialise counts and pointers
C
      NCHD=0
      NREG=0
      LASCHD=0
C
C No old chords to start with
C
      KOLD=1
      NOLD=0
C
C Process source picture
C
      DO 130 J=1,NROW+1
C
C Set up pointer for new chords
C
         KNEW=KOLD+NOLD
         IF (KNEW.GT.NCHORD) KNEW=KNEW-NCHORD
C
C Generate new chord data
C
         IF (DLABL2(J,NCHORD-NOLD,KNEW,NNEW)) GOTO 170
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
            DO 10 I=1,LASCHD
               IB2(I)=0
   10       CONTINUE
C
C Zero chord count
C
            NCHD=0
C
C Generate relabeling array from information in link table
C
            DO 30 I=1,LASCHD
C
C Negative entries denote region id's
C
               IF (IB3(I).LT.0) THEN
                  IB2(I)=IB3(I)
C
C Otherwise, assign new chord id for linked entries
C
               ELSE IF (IB2(I).EQ.0) THEN
C
C Establish new chord id
C
                  NCHD=NCHD+1
C
C Set all linked entries in relabeling array to new chord id
C
                  CURID=I
C
   20             IB2(CURID)=NCHD
                  CURID=IB3(CURID)
C
                  IF (CURID.NE.I) GOTO 20
C
C Initialise new link table entry
C
                  IB3(NCHD)=NCHD
               ENDIF
   30       CONTINUE
C
C Fault more chords than available chord id's
C
            IF (NCHD+NSTART.GT.MAXCHD) THEN
               ERROR=77
               IDMESS='Too many chords'
               GOTO 170
            ENDIF
C
C Relabel the output results so far
C
            J2=J-1
C
            IF (LABEL4(LP3,J1,J2)) GOTO 170
C
C Relabel old chords
C
            KO=KOLD
C
            DO 40 K=1,NOLD
               IF (ICBUFF(KO,NIDENT).GT.0) THEN
                  ICBUFF(KO,NIDENT)=IB2(ICBUFF(KO,NIDENT))
               ENDIF
C
               KO=KO+1
               IF (KO.GT.NCHORD) KO=1
   40       CONTINUE
C
C Re-establish data for start and end chords and any chord overlaps
C (use of RB1 and RB2 to relabel overwrote data produced by LABEL3)
C
            CALL LABEL3(KOLD,NOLD,KNEW,NNEW,FG1,FG2,NSTART,NEND,NOVER)
         ENDIF
C
C Process start chords
C
         DO 50 K=1,NSTART
C
C Fetch start chord index
C
            KN=ICOVER(K,1)
C
C Establish new chord id
C
            NCHD=NCHD+1
C
C Store new chord id
C
            ICBUFF(KN,NIDENT)=NCHD
C
C Initialise link table entry
C
            IB3(NCHD)=NCHD
C
C Initialise link pointers (start chord points to itself)
C
            ICBUFF(KN,NFORWP)=KN
            ICBUFF(KN,NBACKP)=KN
   50    CONTINUE
C
C Process end chords
C
         DO 70 K=1,NEND
C
C Fetch end chord index
C
            KO=ICOVER(K,2)
C
C If chord points to itself, end chord is last chord in region
C
            IF (ICBUFF(KO,NFORWP).EQ.KO) THEN
C
C If all region id's used up, generate error
C
               IF (NREG.EQ.MAXREG) THEN
                  ERROR=77
                  IDMESS='Too many separate regions to label'
                  GOTO 170
               ENDIF
C
C Obtain new region id
C
               NREG=NREG+1
C
C Fetch old chord id
C
               OLDID=ICBUFF(KO,NIDENT)
C
C Update link table (negative entries denote region id's)
C
               CURID=OLDID
C
   60          NEXID=IB3(CURID)
               IB3(CURID)=-NREG
               CURID=NEXID
C
               IF (CURID.NE.OLDID) GOTO 60
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
   70    CONTINUE
C
C Process chord overlaps
C
         K=0
C
         DO 100 FG=FG1,FG2
C
C Set up null indices to previous overlapping chords
C
            KOP=0
            KNP=0
C
C Process overlaps for background or foreground chords
C
            DO 90 N=1,NOVER(FG)
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
   80                ICBUFF(KOF,NIDENT)=NEWID
                     KOF=ICBUFF(KOF,NFORWP)
C
                     IF (KOF.NE.KO) GOTO 80
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
C Merge link lists associated with old and new chords in link table
C
                     OLDLNK=IB3(OLDID)
                     NEWLNK=IB3(NEWID)
C
                     IB3(OLDID)=NEWLNK
                     IB3(NEWID)=OLDLNK
                  ENDIF
C
C Otherwise, we have a straight overlap between old and new chord
C
               ELSE
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
   90       CONTINUE
  100    CONTINUE
C
C If current row not outside source picture limits, write resulting row
C to output picture
C
         IF (.NOT.(J.LT.1.OR.J.GT.NROW)) THEN
C
C Convert new chords back into pixel representation
C
            KN=KNEW
C
            DO 120 K=1,NNEW
C
C Fetch chord id
C
               NEWID=ICBUFF(KN,NIDENT)
C
C Set corresponding pixels to chord id
C
               DO 110 I=ICBUFF(KN,NCOL1),ICBUFF(KN,NCOL2)
                  IB1(I)=NEWID
  110          CONTINUE
C
C Increment chord index
C
               KN=KN+1
               IF (KN.GT.NCHORD) KN=1
  120       CONTINUE
C
C Write data to temporary picture
C
            IF (SEMROW(2,IB1,NFMINT,J,1,LP3)) GOTO 170
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
  130 CONTINUE
C
C Generate relabeling array to achieve final result (by now the link
C table should only contain negative region id's)
C
      DO 140 I=1,LASCHD
         IB2(I)=-IB3(I)
  140 CONTINUE
C
C Convert labeled image into final form
C
      DO 160 J=1,NROW
C
C Fetch row of partially labeled data
C
         IF (SEMROW(1,IB1,NFMINT,J,1,LP3)) GOTO 170
C
C Relabel data into final form.  Negative picture values represent
C already determined region id's.  Positive values represent chord
C id's still to be relabeled.  Zero values represent unlabeled pixels.
C
         DO 150 I=1,NCOL
            IF (IB1(I).LT.0) THEN
               IB1(I)=-IB1(I)
            ELSE IF (IB1(I).GT.0) THEN
               IB1(I)=IB2(IB1(I))
            ENDIF
  150    CONTINUE
C
C Write final result to temporary picture
C
         IF (SEMROW(2,IB1,NFMINT,J,1,LP3)) GOTO 170
  160 CONTINUE
C
      DLABL1=.FALSE.
C
  170 RETURN
C
C Copyright (C) 1991-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module DLABL2
C
      LOGICAL FUNCTION DLABL2(ROW,NMAX,KNEW,NNEW)
C
      INTEGER ROW,NMAX,KNEW,NNEW
C
C Reads the row ROW from source picture LP1 and locates all foreground
C and background chords along that row.  A pixel is a background pixel
C if its value is zero.  The chord buffer will accept up to NMAX chords
C starting at KNEW.  NNEW returns the number of chords found.  Any row
C outside the source picture is treated as all background.
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
      INTEGER PIXEL(LNBUF/LNINT)
      INTEGER ICBUFF(NCHORD,NIPAR)
C
      EQUIVALENCE (PIXEL,RB1),(ICBUFF,RB4)
C
      DLABL2=.TRUE.
C
C Initialise chord count
C
      NNEW=0
C
C Fetch source picture size
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
C
C If row outside source picture, return no new chords
C
      IF (ROW.LT.1.OR.ROW.GT.NROW) GOTO 50
C
C Read in source picture row
C
      IF (SEMROW(1,PIXEL,NFMINT,ROW,1,LP1)) GOTO 60
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
      IF (PIXEL(1).NE.0) GOTO 30
C
C Start of background chord - fault full chord buffer
C
   10 IF (NNEW.EQ.NMAX) GOTO 60
C
C Increment chord count
C
      NNEW=NNEW+1
C
C Initialise chord id, record background chord start position and set
C flag for background
C
      ICBUFF(KN,NIDENT)=0
      ICBUFF(KN,NCOL1)=I
      ICBUFF(KN,NFGRND)=0
C
C If not end of source row, move on to next pixel
C
   20 IF (I.LT.NCOL) THEN
         I=I+1
C
C Otherwise, record background chord end position and return
C
      ELSE
         ICBUFF(KN,NCOL2)=NCOL
         GOTO 50
      ENDIF
C
C If background pixel, keep scanning for more
C
      IF (PIXEL(I).EQ.0) GOTO 20
C
C Record background chord end position
C
      ICBUFF(KN,NCOL2)=I-1
C
C Increment chord pointer
C
      KN=KN+1
      IF (KN.GT.NCHORD) KN=1
C
C Start of foreground chord - fault full chord buffer
C
   30 IF (NNEW.EQ.NMAX) GOTO 70
C
C Increment chord count
C
      NNEW=NNEW+1
C
C Initialise chord id, record foreground chord start position and set
C flag for foreground
C
      ICBUFF(KN,NIDENT)=0
      ICBUFF(KN,NCOL1)=I
      ICBUFF(KN,NFGRND)=1
C
C If not end of source row, move on to next pixel
C
   40 IF (I.LT.NCOL) THEN
         I=I+1
      ELSE
C
C Otherwise, record background chord end position and return
C
         ICBUFF(KN,NCOL2)=NCOL
         GOTO 50
      ENDIF
C
C If foreground pixel, keep scanning for more
C
      IF (PIXEL(I).NE.0) GOTO 40
C
C Record foreground chord end position
C
      ICBUFF(KN,NCOL2)=I-1
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
   50 DLABL2=.FALSE.
C
   60 RETURN
C
C Fault full chord buffer
C
   70 ERROR=150
      GOTO 60
C
C Copyright (C) 1991-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
