C Semper 6 processing module BMPDSK
C
      LOGICAL FUNCTION BMPDSK(BITS,PIXEL,SE,NC,N1,NP,N2,NB)
C
      INTEGER   NC,N1,NP,N2,NB
      INTEGER*4 BITS(0:NC-1,1-N1:NP+N2,0:*)
      INTEGER   PIXEL(*),SE(*)
C
      INTEGER IVALPN !,IPACK
      LOGICAL VARSET,SEMOPN,SEMROW,BDIFF,SEMRNG
C
      INTEGER NPIC,NCOL,NROW,NLAY,CLASS,FORM,I,J,K,NJ,LPM,LPT,LP4,NK
      INTEGER B,P,T,E,IOFF,JOFF,KOFF,LOFF,IX,IY,BMIN,BMAX,IMIN,IMAX
      INTEGER KSRC,KMSK,KTRA,KIN,KOUT,KNX,LSRC,LIN,LOUT,IPAD,NT,JJ,NR
      INTEGER I1,I2,M1,M2
      LOGICAL LMASK,CHANGE
C
      INCLUDE 'COMMON'
C
      BMPDSK=.TRUE.
C
C Create temporary source picture with 2 layers
C
      LPT=0
      IF (SEMOPN(3,0,2*NC*NP,NB,2,NCLIMA,NFMINT,LPT)) GOTO 140
C
C Set up pointers to various bit-packed data buffers
C
      KIN=0
      KSRC=1
      KOUT=2
      KMSK=3
      KTRA=4
C
C See if MASK key is set
C
      LMASK=VARSET(20859)
C
C If so, try to open the mask picture
C
      IF (LMASK) THEN
         NPIC=IVALPN(20859)
         IF (SEMOPN(1,NPIC,NCOL,NROW,NLAY,CLASS,FORM,LP4)) GOTO 140
C
C Fault mask picture size if not same as source picture size
C
         IF (NCOL.NE.NCOLS(LP1).OR.NROW.NE.NROWS(LP1)) THEN
            ERROR=5
            IDERR=NPIC
            GOTO 140
         ENDIF
C
C Create temporary mask picture
C
         LPM=0
         IF (SEMOPN(3,0,2*NC*NP,NB,1,NCLIMA,NFMINT,LPM)) GOTO 140
C
C Convert mask data to bit-packed form
C
         JOFF=0
         DO 20 B=1,NB
            NJ=MIN(NROW-JOFF,NP)
C
            DO 10 J=1,NJ
               IF (SEMROW(1,PIXEL,FORM,J+JOFF,1,LP4)) GOTO 140
               CALL BFORM(1,BITS(0,J,KMSK),PIXEL,FORM,NCOL)
   10       CONTINUE
C
            IF (SEMROW(2,BITS(0,1,KMSK),NFMINT,B,1,LPM)) GOTO 140
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
            IF (SEMROW(1,PIXEL,FORM,J+JOFF,1,LP1)) GOTO 140
            CALL BFORM(1,BITS(0,J,KSRC),PIXEL,FORM,NCOL)
   30    CONTINUE
C
         IF (SEMROW(2,BITS(0,1,KSRC),NFMINT,B,1,LPT)) GOTO 140
C
         JOFF=JOFF+NP
   40 CONTINUE
C
C Fetch structuring element X limits
C
      M1=SE(1)
      M2=SE(2)
C
C Set up pointer to current source layer of temporary picture
C
      LSRC=1
C
C Carry out each processing phase in turn
C
      IOFF=5
      DO 110 P=1,SE(IOFF)
C
C Fetch iteration count
C
         IOFF=IOFF+1
         NT=SE(IOFF)
C
C Initialise iteration count
C
         SE(IOFF)=0
C
C Keep pointer into structuring element data
C
         KOFF=IOFF
C
C Carry out processing within given phase for specified number of times
C
         DO 100 T=1,NT
C
C Restore pointer into structuring element data
C
            IOFF=KOFF
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
C Fetch number of sub-iterations to carry out
C
            IOFF=IOFF+1
            NK=SE(IOFF)
C
C Apply each sub-iteration of structuring element in turn
C
            DO 90 K=1,NK
C
C Read first block of data from temporary picture
C
               IF (SEMROW(1,BITS(0,1,KIN),NFMINT,1,LIN,LPT)) GOTO 140
C
C Keep pointer into structuring element data
C
               LOFF=IOFF
C
C Process data in blocks that fill one row buffer
C
               JOFF=0
               DO 80 B=1,NB
C
C Restore pointer into structuring element data
C
                  IOFF=LOFF
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
     +                  GOTO 140
C
C Copy overlapping rows between current and next buffer
C
                     CALL BLOGIC(12,BITS(0,NP+1-N1,KIN),
     +                              BITS(0,1-N1,KNX),NCOL,N1)
                     CALL BLOGIC(12,BITS(0,1,KNX),
     +                              BITS(0,NP+1,KIN),NCOL,N2)
                  ENDIF
C
C Apply components of structuring element, one column at a time
C
                  IOFF=IOFF+1
                  DO 70 I=1,SE(IOFF)
C
C Fetch current X offset from origin of structuring element
C
                     IOFF=IOFF+1
                     IX=SE(IOFF)
C
C Shift the data by X offset to right according to whether first, last
C or intermediate block of data
C
                     IF (B.EQ.1) THEN
                        JJ=1
                        NR=NJ+N2
                     ELSE IF (B.EQ.NB) THEN
                        JJ=1-N1
                        NR=N1+NJ
                     ELSE
                        JJ=1-N1
                        NR=N1+NJ+N2
                     ENDIF
C
                     CALL BSHIFT(IX,BITS(0,JJ,KIN),BITS(0,JJ,KTRA),
     +                           NCOL,NR)
C
C Carry out source edge processing
C
                     IOFF=IOFF+1
                     DO 60 E=1,SE(IOFF)
C
C Fetch edge value
C
                        IOFF=IOFF+1
                        IPAD=SE(IOFF)
C
C See if edge values need to be set
C
                        IF (IPAD.NE.-1) THEN
C
C Clear/set edge pixels at top/bottom (if current block is first/last)
C
                           IF (B.EQ.1) THEN
                              CALL BFILL(IPAD,1,NCOL,BITS(0,1-N1,KTRA),
     +                                   NCOL,N1)
                           ELSE IF (B.EQ.NB) THEN
                              CALL BFILL(IPAD,1,NCOL,BITS(0,NJ+1,KTRA),
     +                                   NCOL,N2)
                           ENDIF
C
C Clear/set edge pixels to left (right shift) or right (left shift)
C
                           IF (IX.NE.0) THEN
                              IF (IX.GT.0) THEN
                                 I1=1
                                 I2=IX
                              ELSE
                                 I1=NCOL+IX+1
                                 I2=NCOL
                              ENDIF
C
                              CALL BFILL(IPAD,I1,I2,BITS(0,JJ,KTRA),
     +                                   NCOL,NR)
                           ENDIF
                        ENDIF
C
C Apply each component of the structuring element in turn
C
                        IOFF=IOFF+1
                        DO 50 J=1,SE(IOFF)
C
C Fetch current Y offset from origin of structuring element
C
                           IOFF=IOFF+1
                           IY=SE(IOFF)
C
C Output = op(shift(input,ix,iy),output)
C
                           IOFF=IOFF+1
                           CALL BLOGIC(SE(IOFF),BITS(0,1+IY,KTRA),
     +                                 BITS(0,1,KOUT),NCOL,NJ)
   50                   CONTINUE
   60                CONTINUE
   70             CONTINUE
C
C Fetch output edge processing flag
C
                  IOFF=IOFF+1
                  IPAD=SE(IOFF)
C
C Fix up output edge values if required
C
                  IF (IPAD.NE.-1) THEN
                     IF (B.EQ.1) THEN
                        CALL BFILL(IPAD,1,NCOL,BITS(0,1,KOUT),
     +                             NCOL,N1)
                     ELSE IF (B.EQ.NB) THEN
                        CALL BFILL(IPAD,1,NCOL,BITS(0,NJ-N2+1,KOUT),
     +                             NCOL,N2)
                     ENDIF
C
                     CALL BFILL(IPAD,1,M1,BITS(0,1,KOUT),
     +                          NCOL,NJ)
                     CALL BFILL(IPAD,NCOL-M2+1,NCOL,BITS(0,1,KOUT),
     +                          NCOL,NJ)
                  ENDIF
C
C Combine result of sub-iteration with input data
C
                  IOFF=IOFF+1
                  CALL BLOGIC(SE(IOFF),BITS(0,1,KIN),BITS(0,1,KOUT),
     +                        NCOL,NJ)
C
C See if this is the last sub-iteration
C
                  IF (K.EQ.NK) THEN
C
C Set up pointer for source data buffer
C
                     KSRC=KIN
C
C If source data has been overwritten in buffer, read it back from disc
C
                     IF (K.GT.1) THEN
                        IF (SEMROW(1,BITS(0,1,KSRC),NFMINT,B,LSRC,LPT))
     +                     GOTO 140
                     ENDIF
C
C See if mask picture specified for conditional processing
C
                     IF (LMASK) THEN
C
C Read bit-packed mask data
C
                        IF (SEMROW(1,BITS(0,1,KMSK),NFMINT,B,1,LPM))
     +                     GOTO 140
C
C Apply mask data, output = xor(source,and(mask,xor(source,result)))
C
                        CALL BLOGIC(6,BITS(0,1,KSRC),BITS(0,1,KOUT),
     +                              NCOL,NJ)
                        CALL BLOGIC(8,BITS(0,1,KMSK),BITS(0,1,KOUT),
     +                              NCOL,NJ)
                        CALL BLOGIC(6,BITS(0,1,KSRC),BITS(0,1,KOUT),
     +                              NCOL,NJ)
                     ENDIF
C
C Flag any changed pixels
C
                     IF (BDIFF(1,NCOL,BITS(0,1,KSRC),BITS(0,1,KOUT),
     +                         NCOL,NJ)) CHANGE=.TRUE.
                  ENDIF
C
C Output current result
C
                  IF (SEMROW(2,BITS(0,1,KOUT),NFMINT,B,LOUT,LPT))
     +               GOTO 140
C
C Next buffer becomes input buffer for next block of data
C
                  KIN=KNX
C
                  JOFF=JOFF+NP
   80          CONTINUE
C
C Output layer becomes input layer for next sub-iteration
C
               LIN=LOUT
   90       CONTINUE
C
C If data has not changed, break out of processing loop for current
C processing phase
C
            IF (.NOT.CHANGE) GOTO 110
C
C Increment count of number of iterations carried out
C
            SE(KOFF)=SE(KOFF)+1
C
C Output layer becomes source layer for next full iteration
C
            LSRC=LOUT
  100    CONTINUE
  110 CONTINUE
C
C Initialise range limits for final result
C
      BMIN=1
      BMAX=0
C
C Convert bit-packed data into final result
C
      JOFF=0
      DO 130 B=1,NB
         NJ=MIN(NROW-JOFF,NP)
C
         IF (SEMROW(1,BITS(0,1,KOUT),NFMINT,B,LOUT,LPT)) GOTO 140
C
         DO 120 J=1,NJ
            CALL BFORM(2,BITS(0,J,KOUT),PIXEL,FORMN(LP2),NCOL)
            IF (SEMROW(2,PIXEL,FORMN(LP2),J+JOFF,1,LP2)) GOTO 140
  120    CONTINUE
C
         CALL BSCAN(1,NCOL,BITS(0,1,KOUT),NCOL,NJ,IMIN,IMAX)
C
         BMIN=MIN(BMIN,IMIN)
         BMAX=MAX(BMAX,IMAX)
C
         JOFF=JOFF+NP
  130 CONTINUE
C
C Update range information in output picture label
C
      IF (SEMRNG(2,REAL(BMIN),REAL(BMAX),LP2)) GOTO 140
C
      BMPDSK=.FALSE.
C
  140 RETURN
C
C Copyright (C) 1991:  Synoptics Ltd,  All Rights Reserved
C
      END
