C Semper 6 subsidiary module LABEL3
C
      SUBROUTINE LABEL3(KOLD,NOLD,KNEW,NNEW,FG1,FG2,NSTART,NEND,NOVER)
C
      INTEGER KOLD,NOLD,KNEW,NNEW,FG1,FG2,NSTART,NEND,NOVER(0:1)
C
C Given two series of chords on successive source picture rows (old
C and new chords), locates overlaps between chords.  Any chords which
C are non-overlapping are recorded in separate lists (start and end
C chords).  All of the results are stored in row buffers RB1 and RB2
C via the equivalenced array ICOVER.  Each overlap between old and new
C chords is separately recorded.  Succeeding overlaps with a common old
C chord are 'forks' and overlaps with a common new chord are 'joins'.
C
      INTEGER KO,NO,KN,NN,LO1,LO2,LN1,LN2,I,FG
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
      INTEGER ICBUFF(NCHORD,NIPAR)
C
      EQUIVALENCE (ICOVER,RB1)
      EQUIVALENCE (ICBUFF,RB4)
C
C Initialise return counts
C
      NSTART=0
      NEND=0
      NOVER(0)=0
      NOVER(1)=0
C
C Process background/foreground chords as required
C
      DO 60 FG=FG1,FG2
C
C Initialise pointer and count for old and new chord
C
         IF (ICBUFF(KOLD,NFGRND).EQ.FG) THEN
            KO=KOLD
            NO=1
         ELSE
            KO=KOLD+1
            IF (KO.GT.NCHORD) KO=KO-NCHORD
            NO=2
         ENDIF
C
         IF (ICBUFF(KNEW,NFGRND).EQ.FG) THEN
            KN=KNEW
            NN=1
         ELSE
            KN=KNEW+1
            IF (KN.GT.NCHORD) KN=KN-NCHORD
            NN=2
         ENDIF
C
C If no old chords, any new chords must be start chords
C
         IF (NO.GT.NOLD) GOTO 20
C
C If no new chords, any old chords must be end chords
C
         IF (NN.GT.NNEW) GOTO 40
C
C Set up chord limits for first old and new chord (1 or 0 is added
C according to whether foreground/background chord to allow for 8/4
C connected overlaps)
C
         LO1=ICBUFF(KO,NCOL1)
         LO2=ICBUFF(KO,NCOL2)+ICBUFF(KO,NFGRND)
C
         LN1=ICBUFF(KN,NCOL1)
         LN2=ICBUFF(KN,NCOL2)+ICBUFF(KN,NFGRND)
C
C If old chord lies to left of new chord, old chord is an end chord
C
   10    IF (LO2.LT.LN1) THEN
C
C Record end chord
C
            NEND=NEND+1
            ICOVER(NEND,2)=KO
C
C Advance to next old chord
C
            NO=NO+2
C
C If old chords finished, remaining new chords must be start chords
C
            IF (NO.GT.NOLD) GOTO 20
C
C Increment chord pointer
C
            KO=KO+2
            IF (KO.GT.NCHORD) KO=KO-NCHORD
C
C Fetch chord limits
C
            LO1=ICBUFF(KO,NCOL1)
            LO2=ICBUFF(KO,NCOL2)+ICBUFF(KO,NFGRND)
C
C Or if new chord lies to left of old chord, new chord is start chord
C
         ELSE IF (LN2.LT.LO1) THEN
C
C Record start chord
C
            NSTART=NSTART+1
            ICOVER(NSTART,1)=KN
C
C Advance to next new chord
C
            NN=NN+2
C
C If new chords finished, remaining old chords must be end chords
C
            IF (NN.GT.NNEW) GOTO 40
C
C Increment chord pointer
C
            KN=KN+2
            IF (KN.GT.NCHORD) KN=KN-NCHORD
C
C Fetch chord limits
C
            LN1=ICBUFF(KN,NCOL1)
            LN2=ICBUFF(KN,NCOL2)+ICBUFF(KN,NFGRND)
C
C Otherwise, chords overlap
C
         ELSE
C
C Record chord pointers
C
            NOVER(FG)=NOVER(FG)+1
            ICOVER(NOVER(0)+NOVER(1),3)=KO
            ICOVER(NOVER(0)+NOVER(1),4)=KN
C
C Check to see if old or new chord stretches further to the right
C
            IF (LO2.LT.LN2) THEN
C
C Advance to next old chord
C
               NO=NO+2
C
C See if old chords finished
C
               IF (NO.GT.NOLD) THEN
C
C Advance to next new chord
C
                  NN=NN+2
C
C Return if new chords finished
C
                  IF (NN.GT.NNEW) GOTO 60
C
C Increment chord pointer
C
                  KN=KN+2
                  IF (KN.GT.NCHORD) KN=KN-NCHORD
C
C Remaining new chords must be start chords
C
                  GOTO 20
               ENDIF
C
C Increment chord pointer
C
               KO=KO+2
               IF (KO.GT.NCHORD) KO=KO-NCHORD
C
C Fetch chord limits
C
               LO1=ICBUFF(KO,NCOL1)
               LO2=ICBUFF(KO,NCOL2)+ICBUFF(KO,NFGRND)
C
C Check to see if new chord to lies to left of old chord
C
               IF (LN2.LT.LO1) THEN
C
C Advance to next new chord
C
                  NN=NN+2
C
C If new chords finished, remaining old chords must be end chords
C
                  IF (NN.GT.NNEW) GOTO 40
C
C Increment chord pointer
C
                  KN=KN+2
                  IF (KN.GT.NCHORD) KN=KN-NCHORD
C
C Fetch chord limits
C
                  LN1=ICBUFF(KN,NCOL1)
                  LN2=ICBUFF(KN,NCOL2)+ICBUFF(KN,NFGRND)
               ENDIF
            ELSE
C
C Advance to next new chord
C
               NN=NN+2
C
C See if new chords finished
C
               IF (NN.GT.NNEW) THEN
C
C Advance to next old chord
C
                  NO=NO+2
C
C Return if old chords finished
C
                  IF (NO.GT.NOLD) GOTO 60
C
C Increment chord pointer
C
                  KO=KO+2
                  IF (KO.GT.NCHORD) KO=KO-NCHORD
C
C Remaining old chords must be end chords
C
                  GOTO 40
               ENDIF
C
C Increment chord pointer
C
               KN=KN+2
               IF (KN.GT.NCHORD) KN=KN-NCHORD
C
C Fetch chord limits
C
               LN1=ICBUFF(KN,NCOL1)
               LN2=ICBUFF(KN,NCOL2)+ICBUFF(KN,NFGRND)
C
C Check to see if old chord to lies to left of new chord
C
               IF (LO2.LT.LN1) THEN
C
C Advance to next old chord
C
                  NO=NO+2
C
C If old chords finished, remaining new chords must be start chords
C
                  IF (NO.GT.NOLD) GOTO 20
C
C Increment chord pointer
C
                  KO=KO+2
                  IF (KO.GT.NCHORD) KO=KO-NCHORD
C
C Fetch chord limits
C
                  LO1=ICBUFF(KO,NCOL1)
                  LO2=ICBUFF(KO,NCOL2)+ICBUFF(KO,NFGRND)
               ENDIF
            ENDIF
         ENDIF
C
C Carry on with chord overlap processing
C
         GOTO 10
C
C No more old chords, remaining new chords are start chords
C
   20    DO 30 I=NN,NNEW,2
C
C Add new chord to list of start chords
C
            NSTART=NSTART+1
            ICOVER(NSTART,1)=KN
C
C Advance to next new chord
C
            KN=KN+2
            IF (KN.GT.NCHORD) KN=KN-NCHORD
   30    CONTINUE
C
         GOTO 60
C
C No more new chords, remaining old chords are end chords
C
   40    DO 50 I=NO,NOLD,2
C
C Add old chord to list of end chords
C
            NEND=NEND+1
            ICOVER(NEND,2)=KO
C
C Advance to next old chord
C
            KO=KO+2
            IF (KO.GT.NCHORD) KO=KO-NCHORD
   50    CONTINUE
C
   60 CONTINUE
C
      RETURN
C
C Copyright (C) 1991:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module LABEL4
C
      LOGICAL FUNCTION LABEL4(LPN,J1,J2)
C
      INTEGER LPN,J1,J2
C
C Relabels the segmented output picture LPN to take into account the
C results so far of the region scan.  Relabeling is carried out over
C rows J1 to J2 of the output picture.  J1 is updated to reflect the
C absence of unfinished regions in the earlier parts of the output
C picture.  Chord id's are propagated down through subsequent
C overlapping chords.  When two series of chords merge into one, the
C id for the second series of chords must be made the same as for the
C first series.  The array IB2, which is equivalenced to row buffer
C array RB2, records any change of id.  Changes of id caused by the
C completion of scanning of a region are similarly recorded.  In this
C case the new id is the region id, or zero.  A relabeling pass is
C required when all valid chord id's have been used.
C
      LOGICAL SEMROW
C
      INTEGER I,J,NCOL
      LOGICAL DIFFER
C
      INCLUDE 'COMMON'
C
      INTEGER IB1(LNBUF/LNINT),IB2(LNBUF/LNINT)
C
      EQUIVALENCE (IB1,RB1),(IB2,RB2)
C
      LABEL4=.TRUE.
C
C Fetch number of picture columns
C
      NCOL=NCOLS(LPN)
C
C Relabel output picture
C
      DO 20 J=J1,J2
C
C Read row from output picture
C
         IF (SEMROW(1,IB1,NFMINT,J,1,LPN)) GOTO 30
C
C Initialise change flag
C
         DIFFER=.FALSE.
C
C Relabel output picture row
C
         DO 10 I=1,NCOL
C
C Only values greater than zero need be changed
C
            IF (IB1(I).GT.0) THEN
C
C Relabel pixel
C
               IB1(I)=IB2(IB1(I))
C
C Set change flag
C
               DIFFER=.TRUE.
            ENDIF
   10    CONTINUE
C
C If output picture row changed, store it
C
         IF (DIFFER) THEN
            IF (SEMROW(2,IB1,NFMINT,J,1,LPN)) GOTO 30
C
C Otherwise, update start row value if no changes detected on
C previous rows
C
         ELSE
            IF (J1.EQ.J) J1=J1+1
         ENDIF
   20 CONTINUE
C
      LABEL4=.FALSE.
C
   30 RETURN
C
C Copyright (C) 1991:  Synoptics Ltd,  All Rights Reserved
C
      END
