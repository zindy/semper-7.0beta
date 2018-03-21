C Semper 6 processing module BMMDSK
C
      LOGICAL FUNCTION BMMDSK(FLAGS,MAP,BITM,BITS,PIXEL,NM,NC,NP,NB)
C
      INTEGER   FLAGS(3),MAP(0:511),PIXEL(*),NM,NC,NP,NB
      INTEGER*4 BITM(16,NM,*),BITS(0:NC-1,0:NP+1,0:*)
C
      INTEGER IVALPN !,IPACK
      LOGICAL VARSET,SEMOPN,SEMROW,BDIFF,SEMRNG
C
      INTEGER NPIC,NCOL,NROW,NLAY,CLASS,FORM,J,M,T,LP4,LPM,LPT
      INTEGER B,NJ,JOFF,IPAD,NT,BMIN,BMAX,IMIN,IMAX
      INTEGER KSRC,KMSK,KIN,KOUT,KNX,LSRC,LIN,LOUT
      LOGICAL LMASK,CHANGE
C
      INCLUDE 'COMMON'
C
      BMMDSK=.TRUE.
C
C Create temporary source picture with 2 layers
C
      LPT=0
      IF (SEMOPN(3,0,2*NC*NP,NB,2,NCLIMA,NFMINT,LPT)) GOTO 110
C
C Set up pointers to various bit-packed data buffers
C
      KIN=0
      KSRC=1
      KOUT=2
      KMSK=3
C
C See if MASK key is set
C
      LMASK=VARSET(20859)
C
C If so, try to open the mask picture
C
      IF (LMASK) THEN
         NPIC=IVALPN(20859)
         IF (SEMOPN(1,NPIC,NCOL,NROW,NLAY,CLASS,FORM,LP4)) GOTO 110
C
C Fault mask picture size if not same as source picture size
C
         IF (NCOL.NE.NCOLS(LP1).OR.NROW.NE.NROWS(LP1)) THEN
            ERROR=5
            IDERR=NPIC
            GOTO 110
         ENDIF
C
C Create temporary mask picture
C
         LPM=0
         IF (SEMOPN(3,0,2*NC*NP,NB,1,NCLIMA,NFMINT,LPM)) GOTO 110
C
C Convert mask data to bit-packed form
C
         JOFF=0
         DO 20 B=1,NB
            NJ=MIN(NROW-JOFF,NP)
C
            DO 10 J=1,NJ
               IF (SEMROW(1,PIXEL,FORM,J+JOFF,1,LP4)) GOTO 110
               CALL BFORM(1,BITS(0,J,KMSK),PIXEL,FORM,NCOL+2)
   10       CONTINUE
C
            CALL BSHIFT(1,BITS(0,1,KMSK),BITS(0,1,KMSK),NCOL+2,NJ)
C
            IF (SEMROW(2,BITS(0,1,KMSK),NFMINT,B,1,LPM)) GOTO 110
C
            JOFF=JOFF+NP
   20    CONTINUE
      ENDIF
C
C Fetch source picture size and form
C
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      FORM=FORMN(LP1)
C
C Convert source data to bit-packed form
C
      JOFF=0
      DO 40 B=1,NB
         NJ=MIN(NROW-JOFF,NP)
C
         DO 30 J=1,NJ
            IF (SEMROW(1,PIXEL,FORM,J+JOFF,1,LP1)) GOTO 110
            CALL BFORM(1,BITS(0,J,KSRC),PIXEL,FORM,NCOL+2)
   30    CONTINUE
C
         CALL BSHIFT(1,BITS(0,1,KSRC),BITS(0,1,KSRC),NCOL+2,NJ)
C
         IF (SEMROW(2,BITS(0,1,KSRC),NFMINT,B,1,LPT)) GOTO 110
C
         JOFF=JOFF+NP
   40 CONTINUE
C
C Set up pointer to current source layer of temporary picture
C
      LSRC=1
C
C Fetch iteration count
C
      NT=FLAGS(1)
C
C Initialise iteration count
C
      FLAGS(1)=0
C
C Carry out processing for specified number of times
C
      DO 70 T=1,NT
C
C Initialise change flag
C
         CHANGE=.FALSE.
C
C Set up layer pointers for input and output data
C
         LIN=LSRC
         LOUT=MOD(LSRC,2)+1
C
C Apply each sub-iteration of mapping in turn
C
         DO 60 M=1,NM
C
C Read first block of data from temporary picture
C
            IF (SEMROW(1,BITS(0,1,KIN),NFMINT,1,LIN,LPT)) GOTO 110
C
C Process data in blocks that fill one row buffer
C
            JOFF=0
            DO 50 B=1,NB
C
C Determine number of image rows in current block
C
               NJ=MIN(NROW-JOFF,NP)
C
C Set up buffer pointer for next block of input data
C
               KNX=MOD(KIN+1,2)
C
C See if there is any more data left to read
C
               IF (B.LT.NB) THEN
C
C If so, read next block of input data
C
                  IF (SEMROW(1,BITS(0,1,KNX),NFMINT,B+1,LIN,LPT))
     +               GOTO 110
C
C Copy overlapping rows between current and next buffer
C
                  CALL BLOGIC(12,BITS(0,NP,KIN),BITS(0,0,KNX),
     +                        NCOL+2,1)
                  CALL BLOGIC(12,BITS(0,1,KNX),BITS(0,NP+1,KIN),
     +                        NCOL+2,1)
               ENDIF
C
C Fetch source edge flag
C
               IPAD=FLAGS(2)
C
C Set/clear or replicate (default) source edge pixels as appropriate
C
               IF (IPAD.NE.-1) THEN
                  CALL BFILL(IPAD,1,1,BITS(0,0,KIN),
     +                       NCOL+2,NJ+2)
                  CALL BFILL(IPAD,NCOL+2,NCOL+2,BITS(0,0,KIN),
     +                       NCOL+2,NJ+2)
C
                  IF (B.EQ.1) THEN
                     CALL BFILL(IPAD,1,NCOL+2,BITS(0,0,KIN),
     +                          NCOL+2,1)
                  ELSE IF (B.EQ.NB) THEN
                     CALL BFILL(IPAD,1,NCOL+2,BITS(0,NJ+1,KIN),
     +                          NCOL+2,1)
                  ENDIF
               ELSE
                  CALL BREP(2,1,1,BITS(0,0,KIN),
     +                      NCOL+2,NJ+2)
                  CALL BREP(NCOL+1,NCOL+2,NCOL+2,BITS(0,0,KIN),
     +                      NCOL+2,NJ+2)
C
                  IF (B.EQ.1) THEN
                     CALL BLOGIC(12,BITS(0,1,KIN),BITS(0,0,KIN),
     +                           NCOL+2,1)
                  ELSE IF (B.EQ.NB) THEN
                     CALL BLOGIC(12,BITS(0,NJ,KIN),BITS(0,NJ+1,KIN),
     +                           NCOL+2,1)
                  ENDIF
               ENDIF
C
C Convert mapping table data from bit-packed form
C
               CALL BFORM(2,BITM(1,M,1),MAP,NFMINT,512)
C
C Apply current neighbourhood mapping (result shifted one column
C to the right)
C
               CALL BMAP33(BITS(0,0,KIN),BITS(0,1,KIN),BITS(0,2,KIN),
     +                     BITS(0,1,KOUT),MAP,NCOL+2,NJ)
C
C Re-align result by shifting it one column to the left
C
               CALL BSHIFT(-1,BITS(0,1,KOUT),BITS(0,1,KOUT),NCOL+2,NJ)
C
C Fetch output edge flag
C
               IPAD=FLAGS(3)
C
C Fix up edge values if required
C
               IF (IPAD.NE.-1) THEN
                  CALL BFILL(IPAD,2,2,BITS(0,1,KOUT),
     +                       NCOL+2,NJ)
                  CALL BFILL(IPAD,NCOL+1,NCOL+1,BITS(0,1,KOUT),
     +                       NCOL+2,NJ)
C
                  IF (B.EQ.1) THEN
                     CALL BFILL(IPAD,2,NCOL+1,BITS(0,1,KOUT),
     +                          NCOL+2,1)
                  ELSE IF (B.EQ.NB) THEN
                     CALL BFILL(IPAD,2,NCOL+1,BITS(0,NJ,KOUT),
     +                          NCOL+2,1)
                  ENDIF
               ENDIF
C
C See if this is the last sub-iteration
C
               IF (M.EQ.NM) THEN
C
C Set up pointer for source data buffer
C
                  KSRC=KIN
C
C If source data has been overwritten in buffer, read it back from disc
C
                  IF (M.GT.1) THEN
                     IF (SEMROW(1,BITS(0,1,KSRC),NFMINT,B,LSRC,LPT))
     +                  GOTO 110
                  ENDIF
C
C See if mask picture specified for conditional processing
C
                  IF (LMASK) THEN
C
C Read bit-packed mask data
C
                     IF (SEMROW(1,BITS(0,1,KMSK),NFMINT,B,1,LPM))
     +                  GOTO 110
C
C Apply mask data, output = xor(source,and(mask,xor(source,result)))
C
                     CALL BLOGIC(6,BITS(0,1,KSRC),BITS(0,1,KOUT),
     +                           NCOL+2,NJ)
                     CALL BLOGIC(8,BITS(0,1,KMSK),BITS(0,1,KOUT),
     +                           NCOL+2,NJ)
                     CALL BLOGIC(6,BITS(0,1,KSRC),BITS(0,1,KOUT),
     +                           NCOL+2,NJ)
                  ENDIF
C
C Flag any changed pixels
C
                  IF (BDIFF(2,NCOL+1,BITS(0,1,KSRC),BITS(0,1,KOUT),
     +                      NCOL+2,NJ)) CHANGE=.TRUE.
               ENDIF
C
C Output current result
C
               IF (SEMROW(2,BITS(0,1,KOUT),NFMINT,B,LOUT,LPT)) GOTO 110
C
C Next buffer becomes input buffer for next block of data
C
               KIN=KNX
C
               JOFF=JOFF+NP
   50       CONTINUE
C
C Output layer becomes input layer for next sub-iteration
C
            LIN=LOUT
   60    CONTINUE
C
C If data has not changed, break out of processing loop for current
C processing phase
C
         IF (.NOT.CHANGE) GOTO 80
C
C Increment count of number of iterations carried out
C
         FLAGS(1)=FLAGS(1)+1
C
C Output layer becomes source layer for next full iteration
C
         LSRC=LOUT
   70 CONTINUE
C
C Initialise range limits for final result
C
   80 BMIN=1
      BMAX=0
C
C Convert bit-packed data into final result
C
      JOFF=0
      DO 100 B=1,NB
         NJ=MIN(NROW-JOFF,NP)
C
         IF (SEMROW(1,BITS(0,1,KOUT),NFMINT,B,LOUT,LPT)) GOTO 110
C
         CALL BSHIFT(-1,BITS(0,1,KOUT),BITS(0,1,KOUT),NCOL+2,NJ)
C
         DO 90 J=1,NJ
            CALL BFORM(2,BITS(0,J,KOUT),PIXEL,FORMN(LP2),NCOL+2)
            IF (SEMROW(2,PIXEL,FORMN(LP2),J+JOFF,1,LP2)) GOTO 110
   90    CONTINUE
C
         CALL BSCAN(1,NCOL,BITS(0,1,KOUT),NCOL+2,NJ,IMIN,IMAX)
C
         BMIN=MIN(BMIN,IMIN)
         BMAX=MAX(BMAX,IMAX)
C
         JOFF=JOFF+NP
  100 CONTINUE
C
C Update range information in output picture label
C
      IF (SEMRNG(2,REAL(BMIN),REAL(BMAX),LP2)) GOTO 110
C
      BMMDSK=.FALSE.
C
  110 RETURN
C
C Copyright (C) 1991:  Synoptics Ltd,  All Rights Reserved
C
      END
