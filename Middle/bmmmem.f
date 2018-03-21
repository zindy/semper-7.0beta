C Semper 6 processing module BMMMEM
C
      LOGICAL FUNCTION BMMMEM(FLAGS,MAP,BITM,BITS,PIXEL,NM,NC,NP)
C
      INTEGER   FLAGS(3),MAP(0:511),PIXEL(*),NM,NC,NP
      INTEGER*4 BITM(16,NM,*),BITS(0:NC-1,0:NP+1,0:*)
C
      INTEGER IVALPN !,IPACK
      LOGICAL VARSET,SEMOPN,SEMROW,BDIFF,SEMRNG,SEMBRK
C
      INTEGER NPIC,NCOL,NROW,NLAY,CLASS,FORM,J,M,T,LP4,BMIN,BMAX
      INTEGER NT,IPAD,KSRC,KMSK,KIN,KOUT,K1,K2
      LOGICAL LMASK
C
      INCLUDE 'COMMON'
C
      BMMMEM=.TRUE.
C
C Set up pointers to various bit-packed data buffers
C
      KSRC=0
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
         IF (SEMOPN(1,NPIC,NCOL,NROW,NLAY,CLASS,FORM,LP4)) GOTO 70
C
C Fault mask picture size if not same as source picture size
C
         IF (NCOL.NE.NCOLS(LP1).OR.NROW.NE.NROWS(LP1)) THEN
            ERROR=5
            IDERR=NPIC
            GOTO 70
         ENDIF
C
C Convert mask data to bit-packed form
C
         DO 10 J=1,NROW
            IF (SEMROW(1,PIXEL,FORM,J,1,LP4)) GOTO 70
            CALL BFORM(1,BITS(0,J,KMSK),PIXEL,FORM,NCOL+2)
   10    CONTINUE
C
         CALL BSHIFT(1,BITS(0,1,KMSK),BITS(0,1,KMSK),NCOL+2,NROW)
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
         IF (SEMROW(1,PIXEL,FORM,J,1,LP1)) GOTO 70
         CALL BFORM(1,BITS(0,J,KSRC),PIXEL,FORM,NCOL+2)
   20 CONTINUE
C
      CALL BSHIFT(1,BITS(0,1,KSRC),BITS(0,1,KSRC),NCOL+2,NROW)
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
      DO 40 T=1,NT
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
C Apply each sub-iteration of mapping in turn
C
         DO 30 M=1,NM
C
C Set up pointer to output buffer
C
            IF (KIN.EQ.K1) THEN
               KOUT=K2
            ELSE
               KOUT=K1
            ENDIF
C
C Fetch source edge flag
C
            IPAD=FLAGS(2)
C
C Set/clear or replicate (default) source edge pixels as appropriate
C
            IF (IPAD.NE.-1) THEN
               CALL BFILL(IPAD,1,1,BITS(0,1,KIN),
     +                    NCOL+2,NROW)
               CALL BFILL(IPAD,NCOL+2,NCOL+2,BITS(0,1,KIN),
     +                    NCOL+2,NROW)
               CALL BFILL(IPAD,1,NCOL+2,BITS(0,0,KIN),
     +                    NCOL+2,1)
               CALL BFILL(IPAD,1,NCOL+2,BITS(0,NROW+1,KIN),
     +                    NCOL+2,1)
            ELSE
               CALL BREP(2,1,1,BITS(0,1,KIN),
     +                   NCOL+2,NROW)
               CALL BREP(NCOL+1,NCOL+2,NCOL+2,BITS(0,1,KIN),
     +                   NCOL+2,NROW)
               CALL BLOGIC(12,BITS(0,1,KIN),BITS(0,0,KIN),
     +                     NCOL+2,1)
               CALL BLOGIC(12,BITS(0,NROW,KIN),BITS(0,NROW+1,KIN),
     +                     NCOL+2,1)
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
     +                  BITS(0,1,KOUT),MAP,NCOL+2,NROW)
C
C Re-align result by shifting it one column to the left
C
            CALL BSHIFT(-1,BITS(0,1,KOUT),BITS(0,1,KOUT),NCOL+2,NROW)
C
C Fetch output edge flag
C
            IPAD=FLAGS(3)
C
C Fix up edge values if required
C
            IF (IPAD.NE.-1) THEN
               CALL BFILL(IPAD,2,2,BITS(0,1,KOUT),
     +                    NCOL+2,NROW)
               CALL BFILL(IPAD,NCOL+1,NCOL+1,BITS(0,1,KOUT),
     +                    NCOL+2,NROW)
               CALL BFILL(IPAD,2,NCOL+1,BITS(0,1,KOUT),
     +                    NCOL+2,1)
               CALL BFILL(IPAD,2,NCOL+1,BITS(0,NROW,KOUT),
     +                    NCOL+2,1)
            ENDIF
C
C Output buffer becomes input buffer for next sub-iteration
C
            KIN=KOUT
C
C Check for abandon request
C
            IF (SEMBRK()) GOTO 70
   30    CONTINUE
C
C See if mask picture specified for conditional processing
C
         IF (LMASK) THEN
C
C Apply mask data, output = xor(source,and(mask,xor(source,result)))
C
            CALL BLOGIC(6,BITS(0,1,KSRC),BITS(0,1,KOUT),NCOL+2,NROW)
            CALL BLOGIC(8,BITS(0,1,KMSK),BITS(0,1,KOUT),NCOL+2,NROW)
            CALL BLOGIC(6,BITS(0,1,KSRC),BITS(0,1,KOUT),NCOL+2,NROW)
         ENDIF
C
C If no changes for this full iteration, terminate current phase of
C processing
C
         IF (.NOT.BDIFF(2,NCOL+1,BITS(0,1,KSRC),BITS(0,1,KOUT),
     +                  NCOL+2,NROW)) GOTO 50
C
C Increment count of number of iterations carried out
C
         FLAGS(1)=FLAGS(1)+1
C
C Output buffer becomes input buffer for next full iteration
C
         KSRC=KOUT
   40 CONTINUE
C
C Convert bit-packed data into final result
C
   50 CALL BSHIFT(-1,BITS(0,1,KOUT),BITS(0,1,KOUT),NCOL+2,NROW)
C
      DO 60 J=1,NROW
         CALL BFORM(2,BITS(0,J,KOUT),PIXEL,FORMN(LP2),NCOL+2)
         IF (SEMROW(2,PIXEL,FORMN(LP2),J,1,LP2)) GOTO 70
   60 CONTINUE
C
C Determine range of result
C
      CALL BSCAN(1,NCOL,BITS(0,1,KOUT),NCOL+2,NROW,BMIN,BMAX)
C
C Update range information in output picture label
C
      IF (SEMRNG(2,REAL(BMIN),REAL(BMAX),LP2)) GOTO 70
C
      BMMMEM=.FALSE.
C
   70 RETURN
C
C Copyright (C) 1991:  Synoptics Ltd,  All Rights Reserved
C
      END
