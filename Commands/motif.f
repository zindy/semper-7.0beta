C Semper 6 processing module MOTIF
C
      SUBROUTINE MOTIF
C
C Puts in the output picture the average of the specified sub-regions
C of the source picture.  The sub-region size is specified by the keys
C SIZE and SI2 (default size is 32) and the centre position for each
C sub-region is supplied in the position list specified by the key WITH.
C Sub-regions which are not wholly inside the source picture are
C omitted.
C
      INTEGER IVAL,IVALPN
      LOGICAL SEMROW,VARSET,OPT,SEMLU,SEMOPN,SEMCON
      LOGICAL MARSET,FSINIT,FSLINE,FSMARK,FSFLUS
C
      INCLUDE 'COMMON'
C
      REAL X,X1,X2,XMAX,XMIN,Y,Y1,Y2,YMAX,YMIN
      INTEGER CCOL1,CCOL2,CROW1,CROW2,I,ICOL,J,JMAX,JMIN,JROW
      INTEGER L,L1,L2,L3,LP4,MARK,N,NCOL1,NCOL2,NROW1,NROW2
      LOGICAL ODD,EVEN,ANNOT,LREGIO
C
      INTEGER IB3(LNBUF/LNINT),IB4(LNBUF/LNINT)
C
      EQUIVALENCE (IB3,RB3),(IB4,RB4)
C
C Packed names
C
      INTEGER NWITH,NTO,NODD,NEVEN,NNUMBE,NNU2,NN,NREGIO,NVERIF
      PARAMETER (NWITH=-5181, NTO=-601, NODD=24164, NEVEN=8885)
      PARAMETER (NNUMBE=23253, NNU2=23272, NN=22400, NREGIO=29007)
      PARAMETER (NVERIF=-3419)
C
C Fault non-Plist class for picture containing sub-region positions
C
      IF (CLASSN(LP3).NE.NCLPLI) THEN
         ERROR=6
         IDERR=IVALPN(NWITH)
         GOTO 140
      ENDIF
C
C Fetch and check value of key MARK
C
      IF (MARSET(ANNOT,MARK)) GOTO 140
C
C If key MARK appropriately set, prepare for display annotation
C
      IF (ANNOT) THEN
C
C Initialise display graphics
C
         IF (FSINIT(3,MARK)) GOTO 140
C
C Annotate display picture only if 2-D image
C
         ANNOT=FSPTYP.EQ.1
C
C See if option REGION is set
C
         LREGIO=OPT(NREGIO)
      ENDIF
C
C Determine range of positions to use (specified by keys NUMBER and NU2)
C truncating rather than protesting if out of range
C
      IF (VARSET(NNUMBE)) THEN
         L1=MAX(IVAL(NNUMBE),1)
      ELSE
         L1=1
      ENDIF
C
      IF (VARSET(NNU2)) THEN
         L2=MIN(IVAL(NNU2),NCOLS(LP3))
      ELSE
         L2=NCOLS(LP3)
      ENDIF
C
C See if options ODD or EVEN are set
C
      ODD=OPT(NODD)
      EVEN=OPT(NEVEN)
C
C Adjust loop indices if one or both options set
C
      IF (ODD.EQV.EVEN) THEN
         L3=1
      ELSE
         L3=2
C
C If option ODD is set, round range inwards to nearest odd number
C
         IF (ODD) THEN
            L1=1+2*(L1/2)
            L2=1+2*((L2-1)/2)
C
C Otherwise, round to nearest even number
C
         ELSE
            L1=2*((L1+1)/2)
            L2=2*(L2/2)
         ENDIF
      ENDIF
C
C Fetch sizes and centre positions for source and output pictures
C
      NCOL1=NCOLS(LP1)
      NROW1=NROWS(LP1)
      CCOL1=CCOLN(LP1)
      CROW1=CROWN(LP1)
C
      NCOL2=NCOLS(LP2)
      NROW2=NROWS(LP2)
      CCOL2=CCOLN(LP2)
      CROW2=CROWN(LP2)
C
C Determine coordinate limits for motifs
C
      XMIN=REAL(1-CCOL1)
      XMAX=REAL(NCOL1-CCOL1)
      YMIN=REAL(CROW1-NROW1)
      YMAX=REAL(CROW1-1)
C
C Open an FP workspace picture
C
      LP4=LP2
      IF (SEMOPN(3,0,NCOL2,NROW2,1,NCLIMA,NFMFP,LP4)) GOTO 140
C
C Fetch contents of position list
C
      IF (SEMROW(1,RB1,NFMFP,1,1,LP3)) GOTO 140
      IF (SEMROW(1,RB2,NFMFP,1,2,LP3)) GOTO 140
C
C Build up list of centre positions of sub-regions wholly inside source
C picture
C
      N=0
      DO 10 L=L1,L2,L3
C
C Determine nearest integer position for centre of motif
C
         X=ANINT(RB1(L))
         Y=ANINT(RB2(L))
C
C Determine limits of motif
C
         X1=X-REAL(CCOL2-1)
         X2=X1+REAL(NCOL2-1)
         Y2=Y+REAL(CROW2-1)
         Y1=Y2-REAL(NROW2-1)
C
C If motif within limits, add it to list
C
         IF (X1.GE.XMIN.AND.X2.LE.XMAX.AND.
     +       Y1.GE.YMIN.AND.Y2.LE.YMAX) THEN
            N=N+1
            RB1(N)=RB1(L)
            RB2(N)=RB2(L)
C
C Annotate display picture if required
C
            IF (ANNOT) THEN
C
C If option REGION is set, draw box round motif
C
               IF (LREGIO) THEN
                  IF (FSLINE(X1,Y1,X1,Y2)) GOTO 140
                  IF (FSLINE(X1,Y2,X2,Y2)) GOTO 140
                  IF (FSLINE(X2,Y2,X2,Y1)) GOTO 140
                  IF (FSLINE(X2,Y1,X1,Y1)) GOTO 140
C
C Otherwise, mark centre position of motif
C
               ELSE
                  IF (FSMARK(X,Y,FSMMOD,FSMSIZ)) GOTO 140
               ENDIF
            ENDIF
         ENDIF
   10 CONTINUE
C
C Generate error if list is empty
C
      IF (N.EQ.0) THEN
         ERROR=77
         IDMESS='No motifs wholly inside source picture'
         GOTO 140
      ENDIF
C
C Flush contents of graphics buffer if required
C
      IF (ANNOT) THEN
         IF (FSFLUS()) GOTO 140
      ENDIF
C
C Sort positions by Y value
C
      CALL PEAKSR(RB1,RB3,RB2,N,.FALSE.)
C
C Set up list of column and row positions
C
      DO 20 L=1,N
         IB3(L)=CCOL1+NINT(RB1(L))
         IB4(L)=CROW1-NINT(RB2(L))
   20 CONTINUE
C
C Zero contents of output picture
C
      DO 30 I=1,NCOL2
         RB2(I)=0.0
   30 CONTINUE
C
      DO 40 J=1,NROW2
         IF (SEMROW(2,RB2,NFMFP,J,1,LP4)) GOTO 140
   40 CONTINUE
C
C Accumulate output results in one pass through source picture
C
      L1=1
      L2=0
      DO 90 J=1,NROW1
C
C Determine range of Y centre positions for which this source row can
C contribute
C
         JMIN=J+(CROW2-NROW2)
         JMAX=J+(CROW2-1)
C
C Terminate loop if past last centre position
C
   50    IF (L1.GT.N) GOTO 100
C
C If current start position out of range, move on to next position
C
         IF (IB4(L1).LT.JMIN) THEN
            L1=L1+1
            GOTO 50
         ENDIF
C
C If current end position is last one, skip to row processing loop
C
   60    IF (L2.NE.N) THEN
C
C If current end position not closest to upper limit, move on to next
C position
C
            IF (IB4(L2+1).LE.JMAX) THEN
               L2=L2+1
               GOTO 60
            ENDIF
         ENDIF
C
C Add contributions from current source row to output picture
C
         DO 80 L=L1,L2
C
C If first time round loop, fetch contents of current source row
C
            IF (L.EQ.L1) THEN
               IF (SEMROW(1,RB1,NFMFP,J,1,LP1)) GOTO 140
            ENDIF
C
C Determine output row and start column numbers
C
            ICOL=IB3(L)-CCOL2
            JROW=CROW2+(J-IB4(L))
C
C Fetch contents of output row
C
            IF (SEMROW(1,RB2,NFMFP,JROW,1,LP4)) GOTO 140
C
C Add relevant part of source row to output row
C
            DO 70 I=1,NCOL2
               ICOL=ICOL+1
               RB2(I)=RB2(I)+RB1(ICOL)
   70       CONTINUE
C
C Store contents of output row
C
            IF (SEMROW(2,RB2,NFMFP,JROW,1,LP4)) GOTO 140
   80    CONTINUE
   90 CONTINUE
C
C Normalize output results
C
  100 DO 120 J=1,NROW2
C
C Fetch contents of output row
C
         IF (SEMROW(1,RB2,NFMFP,J,1,LP4)) GOTO 140
C
C Scale contents of output row
C
         DO 110 I=1,NCOL2
            RB2(I)=RB2(I)/REAL(N)
  110    CONTINUE
C
C Store contents of output row
C
         IF (SEMROW(2,RB2,NFMFP,J,1,LP2)) GOTO 140
  120 CONTINUE
C
C If VERIFY option is set, print number of motifs averaged
C
      IF (OPT(NVERIF)) THEN
         WRITE (RECORD,130) N
  130    FORMAT (' Number of motifs averaged:',I5)
         IF (SEMCON(RECORD)) GOTO 140
      ENDIF
C
C Return number of motifs
C
      IF (SEMLU(1,NN,REAL(N))) CONTINUE
C
  140 RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
