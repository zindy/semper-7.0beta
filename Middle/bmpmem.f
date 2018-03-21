C Semper 6 processing module BMPMEM
C
      LOGICAL FUNCTION BMPMEM(BITS,PIXEL,SE,NC,N1,NP,N2)
C
      INTEGER   NC,N1,NP,N2
      INTEGER*4 BITS(0:NC-1,1-N1:NP+N2,0:*)
      INTEGER   PIXEL(*),SE(*)
C
      INTEGER IVALPN !,IPACK
      LOGICAL VARSET,SEMOPN,SEMROW,BDIFF,SEMRNG,SEMBRK
C
      INTEGER NPIC,NCOL,NROW,NLAY,CLASS,FORM,I,J,K,LP4,BMIN,BMAX
      INTEGER P,T,E,I1,I2,K1,K2,IOFF,KOFF,IX,IY,IPAD,NT,M1,M2
      INTEGER KSRC,KMSK,KTRA,KIN,KOUT
      LOGICAL LMASK
C
      INCLUDE 'COMMON'
C
      BMPMEM=.TRUE.
C
C Set up pointers to various bit-packed data buffers
C
      KSRC=0
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
         IF (SEMOPN(1,NPIC,NCOL,NROW,NLAY,CLASS,FORM,LP4)) GOTO 100
C
C Fault mask picture size if not same as source picture size
C
         IF (NCOL.NE.NCOLS(LP1).OR.NROW.NE.NROWS(LP1)) THEN
            ERROR=5
            IDERR=NPIC
            GOTO 100
         ENDIF
C
C Convert mask data to bit-packed form
C
         DO 10 J=1,NROW
            IF (SEMROW(1,PIXEL,FORM,J,1,LP4)) GOTO 100
            CALL BFORM(1,BITS(0,J,KMSK),PIXEL,FORM,NCOL)
   10    CONTINUE
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
      DO 20 J=1,NROW
         IF (SEMROW(1,PIXEL,FORM,J,1,LP1)) GOTO 100
         CALL BFORM(1,BITS(0,J,KSRC),PIXEL,FORM,NCOL)
   20 CONTINUE
C
C Fetch structuring element X limits
C
      M1=SE(1)
      M2=SE(2)
C
C Carry out each processing phase in turn
C
      IOFF=5
      DO 80 P=1,SE(IOFF)
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
         DO 70 T=1,NT
C
C Restore pointer into structuring element data
C
            IOFF=KOFF
C
C Set up pointers to data buffers for this iteration
C
            K1=MOD(KSRC+1,3)
            K2=MOD(KSRC+2,3)
C
C Set up pointer to input buffer
C
            KIN=KSRC
C
C Apply each sub-iteration of structuring element in turn
C
            IOFF=IOFF+1
            DO 60 K=1,SE(IOFF)
C
C Set up pointer to output buffer
C
               IF (KIN.EQ.K1) THEN
                  KOUT=K2
               ELSE
                  KOUT=K1
               ENDIF
C
C Apply components of structuring element, one column at a time
C
               IOFF=IOFF+1
               DO 50 I=1,SE(IOFF)
C
C Fetch current X offset from origin of structuring element
C
                  IOFF=IOFF+1
                  IX=SE(IOFF)
C
C Shift the data by X offset to right
C
                  CALL BSHIFT(IX,BITS(0,1,KIN),BITS(0,1,KTRA),NCOL,NROW)
C
C Carry out source edge processing
C
                  IOFF=IOFF+1
                  DO 40 E=1,SE(IOFF)
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
C Clear/set edge pixels at top and bottom
C
                        CALL BFILL(IPAD,1,NCOL,BITS(0,1-N1,KTRA),
     +                             NCOL,N1)
                        CALL BFILL(IPAD,1,NCOL,BITS(0,NROW+1,KTRA),
     +                             NCOL,N2)
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
                           CALL BFILL(IPAD,I1,I2,BITS(0,1,KTRA),
     +                                NCOL,NROW)
                        ENDIF
                     ENDIF
C
C Apply each component of the structuring element in turn
C
                     IOFF=IOFF+1
                     DO 30 J=1,SE(IOFF)
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
     +                              BITS(0,1,KOUT),NCOL,NROW)
   30                CONTINUE
   40             CONTINUE
   50          CONTINUE
C
C Fetch output edge processing flag
C
               IOFF=IOFF+1
               IPAD=SE(IOFF)
C
C Fix up edge values if required
C
               IF (IPAD.NE.-1) THEN
                  CALL BFILL(IPAD,1,NCOL,BITS(0,1,KOUT),
     +                       NCOL,N1)
                  CALL BFILL(IPAD,1,NCOL,BITS(0,NROW-N2+1,KOUT),
     +                       NCOL,N2)
                  CALL BFILL(IPAD,1,M1,BITS(0,1,KOUT),
     +                       NCOL,NROW)
                  CALL BFILL(IPAD,NCOL-M2+1,NCOL,BITS(0,1,KOUT),
     +                       NCOL,NROW)
               ENDIF
C
C Combine result of sub-iteration with input data
C
               IOFF=IOFF+1
               CALL BLOGIC(SE(IOFF),BITS(0,1,KIN),BITS(0,1,KOUT),
     +                     NCOL,NROW)
C
C Output buffer becomes input buffer for next sub-iteration
C
               KIN=KOUT
C
C Check for abandon request
C
               IF (SEMBRK()) GOTO 100
   60       CONTINUE
C
C See if mask picture specified for conditional processing
C
            IF (LMASK) THEN
C
C Apply mask data, output = xor(source,and(mask,xor(source,result)))
C
               CALL BLOGIC(6,BITS(0,1,KSRC),BITS(0,1,KOUT),NCOL,NROW)
               CALL BLOGIC(8,BITS(0,1,KMSK),BITS(0,1,KOUT),NCOL,NROW)
               CALL BLOGIC(6,BITS(0,1,KSRC),BITS(0,1,KOUT),NCOL,NROW)
            ENDIF
C
C If no changes for this full iteration, terminate current phase of
C processing
C
            IF (.NOT.BDIFF(1,NCOL,BITS(0,1,KSRC),BITS(0,1,KOUT),
     +                     NCOL,NROW)) GOTO 80
C
C Increment count of number of iterations carried out
C
            SE(KOFF)=SE(KOFF)+1
C
C Output buffer becomes input buffer for next full iteration
C
            KSRC=KOUT
   70    CONTINUE
   80 CONTINUE
C
C Convert bit-packed data into final result
C
      DO 90 J=1,NROW
         CALL BFORM(2,BITS(0,J,KOUT),PIXEL,FORMN(LP2),NCOL)
         IF (SEMROW(2,PIXEL,FORMN(LP2),J,1,LP2)) GOTO 100
   90 CONTINUE
C
C Determine range of result
C
      CALL BSCAN(1,NCOL,BITS(0,1,KOUT),NCOL,NROW,BMIN,BMAX)
C
C Update range information in output picture label
C
      IF (SEMRNG(2,REAL(BMIN),REAL(BMAX),LP2)) GOTO 100
C
      BMPMEM=.FALSE.
C
  100 RETURN
C
C Copyright (C) 1991:  Synoptics Ltd,  All Rights Reserved
C
      END
