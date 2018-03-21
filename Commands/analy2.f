C Semper 6 subsidiary module ANALY2
C
      LOGICAL FUNCTION ANALY2(IROW,PMIN,PMAX,MODE,NMAX,KNEW,NNEW)
      INTEGER IROW,MODE,NMAX,KNEW,NNEW
      REAL PMIN,PMAX
C
C Reads specified source picture row and locates all particle chords
C along that row.  A pixel is a background pixel if its value does not
C satisfy the threshold requirements specified by MODE, PMIN and PMAX
C i.e., if    MODE = 1, pixel < PMIN
C                  = 2, pixel > PMAX
C                  = 3, pixel < PMIN or pixel > PMAX
C                  = 4, PMAX < pixel < PMIN
C The chord buffer will accept up to NMAX chords starting at KNEW
C NNEW returns the number of chords found
C
      LOGICAL SEMROW
C
      INCLUDE 'COMMON'
C
C Indices for integer chord parameters
C
      INTEGER NIDENT,NFORWP,NBACKP,NCOL1,NCOL2,NCMIN,NCMAX,NRMIN
      INTEGER NX1MIN,NX1MAX,NX2MIN,NX2MAX,NVERT1,NVERT2
      PARAMETER (NIDENT=1)
      PARAMETER (NFORWP=NIDENT+1)
      PARAMETER (NBACKP=NFORWP+1)
      PARAMETER (NCOL1=NBACKP+1)
      PARAMETER (NCOL2=NCOL1+1)
      PARAMETER (NCMIN=NCOL2+1)
      PARAMETER (NCMAX=NCMIN+1)
      PARAMETER (NRMIN=NCMAX+1)
      PARAMETER (NX1MIN=NRMIN+1)
      PARAMETER (NX1MAX=NX1MIN+1)
      PARAMETER (NX2MIN=NX1MAX+1)
      PARAMETER (NX2MAX=NX2MIN+1)
      PARAMETER (NVERT1=NX2MAX+1)
      PARAMETER (NVERT2=NVERT1+1)
C
C Indices for floating point chord parameters
C
      INTEGER NHOLES,NHPROJ,NVPROJ,NPERIM,NSUM,NSUMC,NSUMR
      INTEGER NSUMCC,NSUMRR,NSUMCR
      PARAMETER (NHOLES=1)
      PARAMETER (NHPROJ=NHOLES+1)
      PARAMETER (NVPROJ=NHPROJ+1)
      PARAMETER (NPERIM=NVPROJ+1)
      PARAMETER (NSUM=NPERIM+1)
      PARAMETER (NSUMC=NSUM+1)
      PARAMETER (NSUMR=NSUMC+1)
      PARAMETER (NSUMCC=NSUMR+1)
      PARAMETER (NSUMRR=NSUMCC+1)
      PARAMETER (NSUMCR=NSUMRR+1)
C
C Number of integer and real parameters for each chord
C
      INTEGER NIPAR,NRPAR
      PARAMETER (NIPAR=NVERT2)
      PARAMETER (NRPAR=NSUMCR)
C
C Maximum number of chords in chord buffer
C
      INTEGER NCHORD
      PARAMETER (NCHORD=4*(LNBUF/(NIPAR*LNINT+NRPAR*LNREAL)))
C
C Local variables
C
      REAL PIXL
      INTEGER ICOL,ICOL1,ICOL2,KN,NCOL
C
      ANALY2=.TRUE.
C
C Fetch required source row
C
      IF (SEMROW(1,RB1,NFMFP,IROW,1,LP1)) GOTO 40
C
C Initialise chord pointer
C
      KN=KNEW
C
C Initialise chord count
C
      NNEW=0
C
C Initialise pixel pointer
C
      ICOL=0
C
C Fetch source row size
C
      NCOL=NCOLS(LP1)
C
C Scan past background pixels
C
   10 ICOL=ICOL+1
C
C Return if end of source row
C
      IF (ICOL.GT.NCOL) GOTO 30
C
C Keep scanning if background pixel
C
      PIXL = RB1(ICOL)
      IF (MODE .EQ. 1) THEN
         IF (PIXL.LT.PMIN) GOTO 10
      ELSE IF (MODE .EQ. 2) THEN
         IF (PIXL.GT.PMAX) GOTO 10
      ELSE IF (MODE .EQ. 3) THEN
         IF (PIXL.LT.PMIN .OR. PIXL.GT.PMAX) GOTO 10
      ELSE IF (MODE .EQ. 4) THEN
C
C pmin greater than pmax
C
         IF (PIXL.GT.PMAX .AND. PIXL.LT.PMIN) GOTO 10
      ENDIF
C
C Increment chord count
C
      NNEW=NNEW+1
C
C Fault overflow of chord buffer
C
      IF (NNEW.GT.NMAX) THEN
         ERROR=150
         GOTO 40
      ENDIF
C
C Keep chord start position
C
      ICOL1=ICOL
C
C Scan past end of chord
C
   20 ICOL=ICOL+1
C
C If end of source row reached, initialise chord data and return
C
      IF (ICOL.GT.NCOL) THEN
C
C Determine chord end position
C
         ICOL2=ICOL-1
C
C Initialise chord data
C
         CALL ANALY4(KN,ICOL1,ICOL2,IROW)
C
         GOTO 30
      ENDIF
C
C If background pixel, end of chord has been reached
C
      PIXL = RB1(ICOL)
      IF (MODE .EQ. 1) THEN
         IF (PIXL.GE.PMIN) GOTO 20
      ELSE IF (MODE .EQ. 2) THEN
         IF (PIXL.LE.PMAX) GOTO 20
      ELSE IF (MODE .EQ. 3) THEN
         IF (PIXL.GE.PMIN .AND. PIXL.LE.PMAX) GOTO 20
      ELSE IF (MODE .EQ. 4) THEN
C
C pmin greater than pmax
C
         IF (PIXL.LE.PMAX .OR. PIXL.GE.PMIN) GOTO 20
      ENDIF
C
C Determine chord end position
C
      ICOL2=ICOL-1
C
C Initialise chord data
C
      CALL ANALY4(KN,ICOL1,ICOL2,IROW)
C
C Increment chord pointer
C
      KN=KN+1
      IF (KN.GT.NCHORD) KN=1
C
C Go back and scan for start of next chord
C
      GOTO 10
C
   30 ANALY2=.FALSE.
C
   40 RETURN
C
C Copyright (C) 1987-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module ANALY3
C
      SUBROUTINE ANALY3(KOLD,NOLD,KNEW,NNEW,NSTART,NEND,NOVER)
      INTEGER KOLD,NOLD,KNEW,NNEW,NSTART,NEND,NOVER
C
C Given two series of chords on successive source picture rows (old
C and new chords), locates overlaps between chords.  Any chords which
C are non-overlapping are recorded in separate lists (start and end
C chords).  All of the results are stored in row buffer RB1 via the
C equivalenced array ICOVER.  The vertical projection value for new
C overlapping chords is re-calculated.  A chord's vertical projection
C is its length minus any overlaps with old chords.  Each overlap
C between old and new chords is separately recorded.  Succeeding
C overlaps with a common old chord are 'forks' and overlaps with a
C common new chord are 'joins'.
C
      INCLUDE 'COMMON'
C
C Indices for integer chord parameters
C
      INTEGER NIDENT,NFORWP,NBACKP,NCOL1,NCOL2,NCMIN,NCMAX,NRMIN
      INTEGER NX1MIN,NX1MAX,NX2MIN,NX2MAX,NVERT1,NVERT2
      PARAMETER (NIDENT=1)
      PARAMETER (NFORWP=NIDENT+1)
      PARAMETER (NBACKP=NFORWP+1)
      PARAMETER (NCOL1=NBACKP+1)
      PARAMETER (NCOL2=NCOL1+1)
      PARAMETER (NCMIN=NCOL2+1)
      PARAMETER (NCMAX=NCMIN+1)
      PARAMETER (NRMIN=NCMAX+1)
      PARAMETER (NX1MIN=NRMIN+1)
      PARAMETER (NX1MAX=NX1MIN+1)
      PARAMETER (NX2MIN=NX1MAX+1)
      PARAMETER (NX2MAX=NX2MIN+1)
      PARAMETER (NVERT1=NX2MAX+1)
      PARAMETER (NVERT2=NVERT1+1)
C
C Indices for floating point chord parameters
C
      INTEGER NHOLES,NHPROJ,NVPROJ,NPERIM,NSUM,NSUMC,NSUMR
      INTEGER NSUMCC,NSUMRR,NSUMCR
      PARAMETER (NHOLES=1)
      PARAMETER (NHPROJ=NHOLES+1)
      PARAMETER (NVPROJ=NHPROJ+1)
      PARAMETER (NPERIM=NVPROJ+1)
      PARAMETER (NSUM=NPERIM+1)
      PARAMETER (NSUMC=NSUM+1)
      PARAMETER (NSUMR=NSUMC+1)
      PARAMETER (NSUMCC=NSUMR+1)
      PARAMETER (NSUMRR=NSUMCC+1)
      PARAMETER (NSUMCR=NSUMRR+1)
C
C Number of integer and real parameters for each chord
C
      INTEGER NIPAR,NRPAR
      PARAMETER (NIPAR=NVERT2)
      PARAMETER (NRPAR=NSUMCR)
C
C Maximum number of chords in chord buffer
C
      INTEGER NCHORD
      PARAMETER (NCHORD=4*(LNBUF/(NIPAR*LNINT+NRPAR*LNREAL)))
C
      INTEGER ICOVER(NCHORD,4)
      INTEGER ICBUFF(NCHORD,NIPAR)
      REAL    RCBUFF(NCHORD,NRPAR),RB3456(4*(LNBUF/LNREAL))
C
      EQUIVALENCE (ICOVER,RB1)
      EQUIVALENCE (RCBUFF,RB3456,RB3)
      EQUIVALENCE (ICBUFF,RB3456(NCHORD*NRPAR+1))
C
C Local variables
C
      INTEGER KO,NO,KN,NN,LO1,LO2,LN1,LN2,IVPROJ,I
C
C Initialise return counts
C
      NSTART=0
      NEND=0
      NOVER=0
C
C Initialise pointer and count for old and new chord
C
      KO=KOLD
      NO=1
C
      KN=KNEW
      NN=1
C
C If no old chords, any new chords must be start chords
C
      IF (NOLD.EQ.0) GOTO 20
C
C If no new chords, any old chords must be end chords
C
      IF (NNEW.EQ.0) GOTO 40
C
C Set up chord limits for first old and new chord
C Note: 1 is added to upper limits to reduce the amount of arithmetic
C
      LO1=ICBUFF(KO,NCOL1)
      LO2=ICBUFF(KO,NCOL2)+1
C
      LN1=ICBUFF(KN,NCOL1)
      LN2=ICBUFF(KN,NCOL2)+1
C
C Set up initial vertical projection for first new chord = chord length
C
      IVPROJ=LN2-LN1
C
C If old chord lies to left of new chord, old chord is an end chord
C
   10 IF (LO2.LT.LN1) THEN
C
C Record end chord
C
         NEND=NEND+1
         ICOVER(NEND,2)=KO
C
C If last old chord, remaining new chords must be start chords
C
         IF (NO.EQ.NOLD) GOTO 20
C
C Advance to next old chord
C
         NO=NO+1
         KO=KO+1
         IF (KO.GT.NCHORD) KO=1
C
C Fetch chord limits
C
         LO1=ICBUFF(KO,NCOL1)
         LO2=ICBUFF(KO,NCOL2)+1
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
C If last new chord, remaining old chords must be end chords
C
         IF (NN.EQ.NNEW) GOTO 40
C
C Advance to next new chord
C
         NN=NN+1
         KN=KN+1
         IF (KN.GT.NCHORD) KN=1
C
C Fetch chord limits
C
         LN1=ICBUFF(KN,NCOL1)
         LN2=ICBUFF(KN,NCOL2)+1
C
C Set up initial vertical projection for next new chord = chord length
C
         IVPROJ=LN2-LN1
C
C Otherwise, chords overlap
C
      ELSE
C
C Record chord pointers
C
         NOVER=NOVER+1
         ICOVER(NOVER,3)=KO
         ICOVER(NOVER,4)=KN
C
C Reduce vertical projection value by chord overlap
C
         IF (LO2.LT.LN2) THEN
            IF (LO1.LT.LN1) THEN
               IVPROJ=IVPROJ-(LO2-LN1)
            ELSE
               IVPROJ=IVPROJ-(LO2-LO1)
            ENDIF
C
C Check for last old chord
C
            IF (NO.EQ.NOLD) THEN
C
C Store final vertical projection value for new chord
C
               RCBUFF(KN,NVPROJ)=REAL(IVPROJ)
C
C Return if last new chord
C
               IF (NN.EQ.NNEW) GOTO 60
C
C Advance to next new chord
C
               NN=NN+1
               KN=KN+1
               IF (KN.GT.NCHORD) KN=1
C
C Remaining new chords must be start chords
C
               GOTO 20
            ENDIF
C
C Advance to next old chord
C
            NO=NO+1
            KO=KO+1
            IF (KO.GT.NCHORD) KO=1
C
C Fetch chord limits
C
            LO1=ICBUFF(KO,NCOL1)
            LO2=ICBUFF(KO,NCOL2)+1
C
C Check to see if new chord to lies to left of old chord
C
            IF (LN2.LT.LO1) THEN
C
C Store final vertical projection value for new chord
C
               RCBUFF(KN,NVPROJ)=REAL(IVPROJ)
C
C If last new chord, remaining old chords must be end chords
C
               IF (NN.EQ.NNEW) GOTO 40
C
C Advance to next new chord
C
               NN=NN+1
               KN=KN+1
               IF (KN.GT.NCHORD) KN=1
C
C Fetch chord limits
C
               LN1=ICBUFF(KN,NCOL1)
               LN2=ICBUFF(KN,NCOL2)+1
C
C Set up initial vertical projection for next new chord = chord length
C
               IVPROJ=LN2-LN1
            ENDIF
C
C Reduce vertical projection value by chord overlap
C
         ELSE
            IF (LO1.LT.LN1) THEN
               IVPROJ=IVPROJ-(LN2-LN1)
            ELSE
               IVPROJ=IVPROJ-(LN2-LO1)
            ENDIF
C
C Store final vertical projection value for new chord
C
            RCBUFF(KN,NVPROJ)=REAL(IVPROJ)
C
C Check for last new chord
C
            IF (NN.EQ.NNEW) THEN
C
C Return if last old chord
C
               IF (NO.EQ.NOLD) GOTO 60
C
C Advance to next old chord
C
               NO=NO+1
               KO=KO+1
               IF (KO.GT.NCHORD) KO=1
C
C Remaining old chords must be end chords
C
               GOTO 40
            ENDIF
C
C Advance to next new chord
C
            NN=NN+1
            KN=KN+1
            IF (KN.GT.NCHORD) KN=1
C
C Fetch chord limits
C
            LN1=ICBUFF(KN,NCOL1)
            LN2=ICBUFF(KN,NCOL2)+1
C
C Set up initial vertical projection for next new chord = chord length
C
            IVPROJ=LN2-LN1
C
C Check to see if old chord to lies to left of new chord
C
            IF (LO2.LT.LN1) THEN
C
C If last old chord, remaining new chords must be start chords
C
               IF (NO.EQ.NOLD) GOTO 20
C
C Advance to next old chord
C
               NO=NO+1
               KO=KO+1
               IF (KO.GT.NCHORD) KO=1
C
C Fetch chord limits
C
               LO1=ICBUFF(KO,NCOL1)
               LO2=ICBUFF(KO,NCOL2)+1
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
   20 DO 30 I=NN,NNEW
C
C Add new chord to list of start chords
C
         NSTART=NSTART+1
         ICOVER(NSTART,1)=KN
C
C Advance to next new chord
C
         KN=KN+1
         IF (KN.GT.NCHORD) KN=1
   30 CONTINUE
C
      GOTO 60
C
C No more new chords, remaining old chords are end chords
C
   40 DO 50 I=NO,NOLD
C
C Add old chord to list of end chords
C
         NEND=NEND+1
         ICOVER(NEND,2)=KO
C
C Advance to next old chord
C
         KO=KO+1
         IF (KO.GT.NCHORD) KO=1
   50 CONTINUE
C
   60 RETURN
C
C Copyright (C) 1987-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module ANALY4
C
      SUBROUTINE ANALY4(ICHORD,ICOL1,ICOL2,IROW)
      INTEGER ICHORD,ICOL1,ICOL2,IROW
C
C Initialises the parameters for a new chord.  ICHORD is the pointer
C to the new chord.  ICOL1 and ICOL2 are the start and end column
C positions for the chord and IROW is the chord's row position.
C The position and extents of the chord are used to calculate the
C chord's area, first moments, second moments, etc., thereby minimizing
C the amount of computation in determining the various particle sums.
C
      INCLUDE 'COMMON'
C
C Indices for integer chord parameters
C
      INTEGER NIDENT,NFORWP,NBACKP,NCOL1,NCOL2,NCMIN,NCMAX,NRMIN
      INTEGER NX1MIN,NX1MAX,NX2MIN,NX2MAX,NVERT1,NVERT2
      PARAMETER (NIDENT=1)
      PARAMETER (NFORWP=NIDENT+1)
      PARAMETER (NBACKP=NFORWP+1)
      PARAMETER (NCOL1=NBACKP+1)
      PARAMETER (NCOL2=NCOL1+1)
      PARAMETER (NCMIN=NCOL2+1)
      PARAMETER (NCMAX=NCMIN+1)
      PARAMETER (NRMIN=NCMAX+1)
      PARAMETER (NX1MIN=NRMIN+1)
      PARAMETER (NX1MAX=NX1MIN+1)
      PARAMETER (NX2MIN=NX1MAX+1)
      PARAMETER (NX2MAX=NX2MIN+1)
      PARAMETER (NVERT1=NX2MAX+1)
      PARAMETER (NVERT2=NVERT1+1)
C
C Indices for floating point chord parameters
C
      INTEGER NHOLES,NHPROJ,NVPROJ,NPERIM,NSUM,NSUMC,NSUMR
      INTEGER NSUMCC,NSUMRR,NSUMCR
      PARAMETER (NHOLES=1)
      PARAMETER (NHPROJ=NHOLES+1)
      PARAMETER (NVPROJ=NHPROJ+1)
      PARAMETER (NPERIM=NVPROJ+1)
      PARAMETER (NSUM=NPERIM+1)
      PARAMETER (NSUMC=NSUM+1)
      PARAMETER (NSUMR=NSUMC+1)
      PARAMETER (NSUMCC=NSUMR+1)
      PARAMETER (NSUMRR=NSUMCC+1)
      PARAMETER (NSUMCR=NSUMRR+1)
C
C Number of integer and real parameters for each chord
C
      INTEGER NIPAR,NRPAR
      PARAMETER (NIPAR=NVERT2)
      PARAMETER (NRPAR=NSUMCR)
C
C Maximum number of chords in chord buffer
C
      INTEGER NCHORD
      PARAMETER (NCHORD=4*(LNBUF/(NIPAR*LNINT+NRPAR*LNREAL)))
C
      INTEGER ICBUFF(NCHORD,NIPAR)
      REAL    RCBUFF(NCHORD,NRPAR),RB3456(4*(LNBUF/LNREAL))
C
      EQUIVALENCE (RCBUFF,RB3456,RB3)
      EQUIVALENCE (ICBUFF,RB3456(NCHORD*NRPAR+1))
C
C Local variables
C
      REAL ROW,SCOL,SCOL1,SCOL2
C
C Store chord start and end positions
C
      ICBUFF(ICHORD,NCOL1)=ICOL1
      ICBUFF(ICHORD,NCOL2)=ICOL2
C
C Set up minimum and maximum column numbers
C
      ICBUFF(ICHORD,NCMIN)=ICOL1
      ICBUFF(ICHORD,NCMAX)=ICOL2
C
C Set up minimum row number = start row number
C
      ICBUFF(ICHORD,NRMIN)=IROW
C
C Set up minimum and maximum for (column-row) and (column+row)
C
      ICBUFF(ICHORD,NX1MIN)=ICOL1-IROW
      ICBUFF(ICHORD,NX1MAX)=ICOL2-IROW
      ICBUFF(ICHORD,NX2MIN)=ICOL1+IROW
      ICBUFF(ICHORD,NX2MAX)=ICOL2+IROW
C
C Zero left and right vertical edge counts for chord
C
      ICBUFF(ICHORD,NVERT1)=0
      ICBUFF(ICHORD,NVERT2)=0
C
C Determine common floating point values
C
      SCOL=REAL(ICOL2-ICOL1+1)
      SCOL1=REAL(ICOL1)*REAL(ICOL1-1)
      SCOL2=REAL(ICOL2)*REAL(ICOL2+1)
      ROW=REAL(IROW)
C
C Zero hole count
C
      RCBUFF(ICHORD,NHOLES)=0.0
C
C Set up area of chord = chord length
C
      RCBUFF(ICHORD,NSUM)=SCOL
C
C Set up horizontal projection (1.0 for all chords)
C
      RCBUFF(ICHORD,NHPROJ)=1.0
C
C Set up tentative vertical projection value = chord length (only used
C if chord is a start chord)
C
      RCBUFF(ICHORD,NVPROJ)=SCOL
C
C Set up initial first, second and cross moments of area for chord
C
      RCBUFF(ICHORD,NSUMC)=SCOL2-SCOL1
      RCBUFF(ICHORD,NSUMR)=ROW*SCOL
      RCBUFF(ICHORD,NSUMCC)=REAL(ICOL2+ICOL2+1)*SCOL2-
     +                      REAL(ICOL1+ICOL1-1)*SCOL1
      RCBUFF(ICHORD,NSUMRR)=ROW*RCBUFF(ICHORD,NSUMR)
      RCBUFF(ICHORD,NSUMCR)=ROW*RCBUFF(ICHORD,NSUMC)
C
      RETURN
C
C Copyright (C) 1987-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
