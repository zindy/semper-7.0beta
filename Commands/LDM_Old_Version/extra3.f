C Semper 6 subsidiary module EXTRA3
C
      SUBROUTINE EXTRA3
C
C Performs general extraction by sampling pixels in source picture
C
      LOGICAL OPT,SEMROW,SEMCEN
C
      INCLUDE 'COMMON'
C
      REAL RDIV
      INTEGER*4 ILONG,IDIV,NCOL4
      INTEGER FORM,CCOL,CROW,CLAY,IMAX,JMAX,NCOL,NROW,NLAY
      INTEGER ISTEP,JSTEP,ISTART,JSTART,JREP,JROW,ICOL,INFORM
      INTEGER I,J,K,L,M
      LOGICAL AVERAG
C
      INTEGER IB1(LNBUF/LNINT),IB2(LNBUF/LNINT),FIRST(3)
      INTEGER*4 IB3(LNBUF/LNINT4)
C
      INTEGER ISAMPL
      EQUIVALENCE (IB1,RB1),(IB2,RB2),(IB3,RB3)
      EQUIVALENCE (FIRST,SMGI1),(ISAMPL,SMGI10)
C
C Packed names
C
      INTEGER NAVERA
      PARAMETER (NAVERA=2485)
C
C Fetch source picture size
C
      IMAX = NCOLS(LP1)
      JMAX = NROWS(LP1)
C
C Fetch output picture size and form
C
      NCOL = NCOLS(LP2)
      NCOL4= NCOL
      NROW = NROWS(LP2)
      NLAY = NLAYS(LP2)
      FORM = FORMN(LP2)
C
C Pixel averaging is used if option AVERAGE and a non-unit sampling
C interval are specified
C
      AVERAG = OPT(NAVERA).AND.ISAMPL.NE.1
      IF (AVERAG) THEN
         IDIV = ISAMPL*ISAMPL
         RDIV = 1.0/REAL(IDIV)
      ENDIF
C
C Reduce sampling interval (modulo source picture size)
C
      ISTEP = MOD(ISAMPL-1,IMAX)+1
      JSTEP = MOD(ISAMPL-1,JMAX)+1
C
C Reduce start column and row numbers
C
      ISTART = MOD(FIRST(1),IMAX)
      IF (ISTART.LE.0) ISTART = ISTART+IMAX
C
      JSTART = MOD(FIRST(2),JMAX)
      IF (JSTART.LE.0) JSTART = JSTART+JMAX
C
C Determine number of rows before output picture repeats itself
C
      JROW = JSTART
      DO 10 J=1,NROW
         JREP = J
         JROW = JROW+JSTEP
         IF (JROW.GT.JMAX) JROW = JROW-JMAX
         IF (JROW.EQ.JSTART) GOTO 20
   10 CONTINUE
C
C Set up control parameters according to form of output picture
C
   20 IF (FORM.EQ.NFMCOM) THEN
         INFORM = NFMCOM
         ISTART = 2*ISTART-1
         ISTEP = 2*ISTEP
         IMAX = 2*IMAX
      ELSE IF (FORM.EQ.NFMFP) THEN
         INFORM = NFMFP
      ELSE
         INFORM = NFMINT
      ENDIF
C
C Extract data from source picture, one layer at a time
C
      DO 210 K=1,NLAY
C
C Extract data from source picture, with averaging over sampling
C interval if necessary
C
         JROW = JSTART
         DO 200 J=1,JREP
C
C Check for averaging over sampling interval
C
            IF (AVERAG) THEN
C
C Set pixel sums to zero
C
               IF (INFORM.EQ.NFMINT) THEN
                  DO 30 I=1,NCOL
                     IB3(I) = 0
   30             CONTINUE
               ELSE IF (INFORM.EQ.NFMFP) THEN
                  DO 40 I=1,NCOL
                     RB2(I) = 0.0
   40             CONTINUE
               ELSE
                   DO 50 I=1,2*NCOL
                     RB2(I) = 0.0
   50             CONTINUE
               ENDIF
C
C Sum pixels over sampling interval
C
               DO 120 M=1,ISAMPL
C
C Fetch source row
C
                  IF (SEMROW(1,RB1,INFORM,JROW,K,LP1)) GOTO 220
C
C Sum pixel values from this row
C
                  ICOL = ISTART
                  IF (INFORM.EQ.NFMINT) THEN
                     DO 70 I=1,NCOL
                        DO 60 L=1,ISAMPL
                           ILONG = IB1(ICOL)
                           IB3(I) = IB3(I) + ILONG
                           ICOL = ICOL + 1
                           IF (ICOL.GT.IMAX) ICOL = ICOL - IMAX
   60                   CONTINUE
   70                CONTINUE
                  ELSE IF (INFORM.EQ.NFMFP) THEN
                     DO 90 I=1,NCOL
                        DO 80 L=1,ISAMPL
                           RB2(I) = RB2(I) + RB1(ICOL)
                           ICOL = ICOL + 1
                           IF (ICOL.GT.IMAX) ICOL = ICOL - IMAX
   80                   CONTINUE
   90                CONTINUE
                  ELSE
                     DO 110 I=1,2*NCOL,2
                        DO 100 L=1,ISAMPL
                           RB2(I) = RB2(I) + RB1(ICOL)
                           RB2(I+1) = RB2(I+1) + RB1(ICOL+1)
                           ICOL = ICOL + 2
                           IF (ICOL.GT.IMAX) ICOL = ICOL - IMAX
  100                   CONTINUE
  110                CONTINUE
                  ENDIF
C
C Increment source row number
C
                  JROW = JROW + 1
                  IF (JROW.GT.JMAX) JROW = JROW - JMAX
  120          CONTINUE
C
C Calculate average pixel values
C
               IF (INFORM.EQ.NFMINT) THEN
                  DO 130 I=1,NCOL
                     IB2(I) = IB3(I)/IDIV
  130             CONTINUE
               ELSE IF (INFORM.EQ.NFMFP) THEN
                  DO 140 I=1,NCOL
                     RB2(I) = RB2(I)*RDIV
  140             CONTINUE
               ELSE
                  DO 150 I=1,2*NCOL
                     RB2(I) = RB2(I)*RDIV
  150             CONTINUE
               ENDIF
C
C Otherwise, just re-arrange row data
C
            ELSE
C
C Fetch source row
C
               IF (SEMROW(1,RB1,INFORM,JROW,K,LP1)) GOTO 220
C
C Copy source row data to output row
C
               ICOL=ISTART
               IF (INFORM.EQ.NFMINT) THEN
                  DO 160 I=1,NCOL
                     IB2(I) = IB1(ICOL)
                     ICOL = ICOL + ISTEP
                     IF (ICOL.GT.IMAX) ICOL = ICOL - IMAX
  160             CONTINUE
               ELSE IF (INFORM.EQ.NFMFP) THEN
                  DO 170 I=1,NCOL
                     RB2(I) = RB1(ICOL)
                     ICOL = ICOL + ISTEP
                     IF (ICOL.GT.IMAX) ICOL = ICOL - IMAX
  170             CONTINUE
               ELSE
                  DO 180 I=1,2*NCOL,2
                     RB2(I) = RB1(ICOL)
                     RB2(I+1) = RB1(ICOL+1)
                     ICOL = ICOL + ISTEP
                     IF (ICOL.GT.IMAX) ICOL = ICOL - IMAX
  180             CONTINUE
               ENDIF
C
C Increment source row number
C
               JROW = JROW + JSTEP
               IF (JROW.GT.JMAX) JROW = JROW - JMAX
            ENDIF
C
C Convert data into output form if necessary
C
            IF (FORM.NE.INFORM) CALL CFORM(RB2,RB2,INFORM,FORM,NCOL4)
C
C Store result in LP2
C
            DO 190 M=J,NROW,JREP
               IF (SEMROW(2,RB2,FORM,M,K,LP2)) GOTO 220
  190       CONTINUE
  200    CONTINUE
  210 CONTINUE
C
C Determine output centre position to nearest pixel
C
      CCOL = 1 + NINT(REAL(CCOLN(LP1)-FIRST(1))/REAL(ISAMPL))
      CROW = 1 + NINT(REAL(CROWN(LP1)-FIRST(2))/REAL(ISAMPL))
      CLAY = 1 + NLAY/2
C
C Update centre position if within picture limits
C
      IF (CCOL.GE.1.AND.CCOL.LE.NCOL.AND.
     +    CROW.GE.1.AND.CROW.LE.NROW) THEN
         IF (SEMCEN(LP2,CCOL,CROW,CLAY)) GOTO 220
      ENDIF
C
  220 RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
