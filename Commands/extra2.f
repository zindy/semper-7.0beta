C Semper 6 subsidiary module EXTRA2
C
      SUBROUTINE EXTRA2
C
C Performs general extraction using bi-linear interpolation at the
C array of positions contained in the position list specified by the
C key WITH.  The output picture is created to the same row,column
C size as the position list.  All positions are marked on the display
C if key MARK is appropraitely set.
C
      LOGICAL SEMOPN,SEMROW,SEMROWI,MARSET,FSINIT,FSLIST,OPT
      LOGICAL EXTRNN,EXTRCT,EXTRCB
      INTEGER IVALPN,SEMFRM
C
      REAL DUMMY(2)
      INTEGER CLASS,FORM,CCOL,CROW,INFORM,MARK,NCOL,NROW,NLAY,NPIC
      INTEGER I,J,J1,J2,K,M,N,JBUF,NBUF
      LOGICAL ANNOT,LNNEIG,LBICUB
C
      INCLUDE 'COMMON'
C
      INTEGER IB2(LNBUF/LNINT)
      REAL    CB2(2,LNBUF/LNCOMP)
      EQUIVALENCE (IB2,RB2,CB2)
C
C Packed names
C
      INTEGER NWITH,NTO,NNNEIG,NBICUB
      PARAMETER (NWITH=-5181, NTO=-601, NNNEIG=22965, NBICUB=3563)
C
C Set dummy explicitly to avoid PC COMMON block
C
      DUMMY(1) = 0.0
      DUMMY(2) = 0.0
C
C See if options NNEIGHBOUR and BICUBIC are set
C
      LNNEIG = OPT(NNNEIG)
      LBICUB = OPT(NBICUB)
C
C Fault conflicting options NNEIGHBOUR and BICUBIC
C
      IF (LNNEIG.AND.LBICUB) THEN
         ERROR  = 60
         IDERR  = NNNEIG
         IDERR2 = NBICUB
         GOTO 60
      ENDIF
C
C Open picture containing position list
C
      NPIC = IVALPN(NWITH)
      IF (SEMOPN(1,NPIC,NCOL,NROW,NLAY,CLASS,FORM,LP3)) GOTO 60
C
C Fault picture which is not a position list
C
      IF (CLASS.NE.NCLPLI) THEN
         ERROR = 6
         IDERR = NPIC
         GOTO 60
      ENDIF
C
C Fetch and check value of key MARK
C
      IF (MARSET(ANNOT,MARK)) GOTO 60
C
C If key MARK appropriately set, prepare for display annotation
C
      IF (ANNOT) THEN
C
C Initialise display graphics
C
         IF (FSINIT(3,MARK)) GOTO 60
C
C Annotate display picture only if 2-D image
C
         ANNOT = FSPTYP.EQ.1
      ENDIF
C
C Determine number of layers, class and form for output picture
C
      NLAY = NLAYS(LP1)
      CLASS= CLASSN(LP1)
      FORM = SEMFRM(FORMN(LP1))
C
C Open new output picture
C
      LP2 = LP1
      IF (SEMOPN(2,IVALPN(NTO),NCOL,NROW,NLAY,CLASS,FORM,LP2)) GOTO 60
C
C Fault re-use of source picture disc space for output
C
      IF (LP2.EQ.LP1) THEN
         ERROR = 59
         GOTO 60
      ENDIF
C
C Fetch source picture centre position
C
      CCOL = CCOLN(LP1)
      CROW = CROWN(LP1)
C
C Determine internal data form and how many output rows can be buffered
C
      IF (FORM.EQ.NFMBYT) THEN
         INFORM = NFMINT
      ELSE
         INFORM = FORM
      ENDIF
C
      IF (FORM.EQ.NFMCOM) THEN
         NBUF = (LNBUF/LNCOMP)/NCOL
      ELSE
         NBUF = (LNBUF/LNREAL)/NCOL
      ENDIF
C
C Fault too large output row length
C
      IF (NBUF.EQ.0) THEN
         ERROR = 77
         IDMESS = 'Output row length is too large to process'
         GOTO 60
      ENDIF
C
C Extract pixel values for each block of output rows in turn
C
      DO 50 J=1,NROW,NBUF
C
C Loop for each buffered output row
C
         J1 = J
         J2 = MIN(J+NBUF-1,NROW)
         N = 0
C
         DO 20 JBUF=J1,J2
C
C Fetch corresponding X and Y values from position list
C
            IF (SEMROW(1,RB1,NFMFP,JBUF,1,LP3)) GOTO 60
            IF (SEMROW(1,RB2,NFMFP,JBUF,2,LP3)) GOTO 60
C
C Mark positions if required
C
            IF (ANNOT) THEN
               IF (FSLIST(RB1,RB2,NCOL,FSMMOD,FSMSIZ)) GOTO 60
            ENDIF
C
C Convert output pixel positions into pixel coordinates with respect
C to source picture
C
            DO 10 I=1,NCOL
C
C Convert and store next output pixel position
C
               N = N + 1
               RB3(N) = REAL(CCOL) + RB1(I)
               RB4(N) = REAL(CROW) - RB2(I)
   10       CONTINUE
   20    CONTINUE
C
C Extract values from corresponding part of each source picture layer
C
         DO 40 K=1,NLAY
C
C Extract values for all pixels in buffer
C
            IF (LNNEIG) THEN
               IF (EXTRNN(LP1,N,INFORM,K,.FALSE.,DUMMY)) GOTO 60
            ELSE IF (LBICUB) THEN
               IF (EXTRCB(LP1,N,INFORM,K,.FALSE.,DUMMY)) GOTO 60
            ELSE
               IF (EXTRCT(LP1,N,INFORM,K,.FALSE.,DUMMY)) GOTO 60
            ENDIF
C
C Store results in LP2
C
            M = 1
C
            DO 30 JBUF=J1,J2
C
C Output data in appropriate form
C
               IF (INFORM.EQ.NFMINT) THEN
                  IF (SEMROWI(2,IB2(M),INFORM,JBUF,K,LP2)) GOTO 60
               ELSE IF (FORM.EQ.NFMFP) THEN
                  IF (SEMROW(2,RB2(M),INFORM,JBUF,K,LP2)) GOTO 60
               ELSE
                  IF (SEMROW(2,CB2(1,M),INFORM,JBUF,K,LP2)) GOTO 60
               ENDIF
C
C Update output buffer start address
C
               M = M + NCOL
   30       CONTINUE
   40    CONTINUE
   50 CONTINUE
C
   60 RETURN
C
C Copyright (C) 1987,1988,1989,1990: Synoptics Ltd,  All Rights Reserved
C
      END
