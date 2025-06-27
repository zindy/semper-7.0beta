C Semper 6 subsidiary module EXTRA4
C
      SUBROUTINE EXTRA4
C
C Performs general extraction, with bi-linear interpolation
C
      REAL VAL
      LOGICAL OPT,SEMROW,SEMROWI,EXTRNN,EXTRCT,EXTRCB,SEMCON
C
      REAL DUMMY(2)
      REAL UX,UY,VX,VY,XSTART,YSTART,X,Y
      INTEGER I,J,J1,J2,JBUF,K,M,N
      INTEGER FORM,INFORM,NBUF,NCOL,NROW,NLAY
      LOGICAL LNNEIG,LBICUB
C
      INCLUDE 'COMMON'
C
      INTEGER IB2(LNBUF/LNINT)
      REAL    CB2(2,LNBUF/LNCOMP)
      REAL CORNER(2),U(2),V(2)
      EQUIVALENCE (IB2,RB2,CB2),(CORNER,SMGR1),(U,SMGR3),(V,SMGR5)
C
C Packed names
C
      INTEGER NAVERA,NVERIF,NNNEIG,NBICUB,NUV,NU,NU2,NV,NV2
      PARAMETER (NAVERA=2485, NVERIF=-3419, NNNEIG=22965, NBICUB=3563)
      PARAMETER (NUV=-2481, NU=-1601, NU2=-2881, NV=-3201, NV2=-4481)
C
C Set dummy explicitly to avoid PC COMMON block
C
      DUMMY(1) = 0.0
      DUMMY(2) = 0.0
C
C Fault option AVERAGE
C
      IF (OPT(NAVERA)) THEN
         ERROR = 64
         IDERR = NAVERA
         GOTO 60
      ENDIF
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
C Determine start position
C
      XSTART = REAL(CCOLN(LP1)) + CORNER(1)
      YSTART = REAL(CROWN(LP1)) - CORNER(2)
C
C Set up X and Y lattice increments
C
      UX =  U(1)
      UY = -U(2)
      VX =  V(1)
      VY = -V(2)
C
C Fetch output picture size and form
C
      NCOL = NCOLS(LP2)
      NROW = NROWS(LP2)
      NLAY = NLAYS(LP2)
      FORM = FORMN(LP2)
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
         DO 20 M=J1,J2
C
C Initialise pixel position
C
            X = XSTART
            Y = YSTART
C
C Determine all output pixel positions
C
            DO 10 I=1,NCOL
C
C Store pixel position
C
               N = N + 1
               RB3(N) = X
               RB4(N) = Y
C
C Increment pixel position
C
               X = X + UX
               Y = Y + UY
   10       CONTINUE
C
C Increment start position for output row
C
            XSTART = XSTART + VX
            YSTART = YSTART + VY
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
C Verify data for lattice vectors if UV and VERIFY options set
C
      IF (OPT(NUV).AND.OPT(NVERIF)) THEN
         WRITE (RECORD,70) VAL(NU),VAL(NU2),VAL(NV),VAL(NV2)
         IF (SEMCON(RECORD)) GOTO 60
      ENDIF
C
   60 RETURN
C
   70 FORMAT('Lattice vectors',2(' (',F7.2,',',F7.2,')'))
C
C Copyright (C) 1987-1992: Synoptics Ltd,  All Rights Reserved
C
      END
