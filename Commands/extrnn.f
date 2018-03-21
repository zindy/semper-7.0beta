C Semper subsidiary module EXTRNN
C
      LOGICAL FUNCTION EXTRNN(LPN,N,FORM,LAYER,LBLANK,VALUE)
      REAL VALUE(2)
      INTEGER LPN,N,FORM,LAYER
      LOGICAL LBLANK
C
C Performs general extraction from picture LPN, using nearest neighbour
C extraction.  Positions of pixels must be supplied in the row
C buffers RB3 (for X) and RB4 (for Y), in pixel coordinates.  The
C positions are sorted to determine which rows in LPN have to be
C accessed.  If LBLANK is set to .FALSE., any positions outside the
C source picture are reduced to fall within the source picture by
C taking the remainder after dividing by the picture size, i.e. the
C coordinates 'wrap around'.  If LBLANK is set to .TRUE., any sample
C points that lie outside the source picture return the background
C value(s) passed in array VALUE.  Row buffers RB5 and RB6 are used
C for the purposes of sorting.  The source picture rows are each input
C just once into the row buffer RB1 and the appropriate contributions
C are placed in the row buffer RB2.  The number of pixel values to
C extract and the data form for the result in RB2 are specified by N
C and FORM.  The extraction is done in integer/floating-point/complex
C form and then the result is converted to byte form if needs be.
C The layer number for extraction is given by LAYER, so that extraction
C from a single layer of a multi-layer picture can be done.
C
      LOGICAL SEMROW
C
      REAL X,Y,XMAX,YMAX
      INTEGER IBLANK,I,J,L,NCOL,NROW,INFORM
      INTEGER*4 N4
C
      INCLUDE 'COMMON'
C
      INTEGER IB1(LNBUF/LNINT),IB2(LNBUF/LNINT)
      INTEGER IB5(0:LNBUF/LNINT),IB6(LNBUF/LNINT)
C
      EQUIVALENCE (IB1,RB1),(IB2,RB2),(IB5,RB5),(IB6,RB6)
C
      EXTRNN=.TRUE.
C
C Fetch source picture size
C
      NCOL=NCOLS(LPN)
      NROW=NROWS(LPN)
      XMAX=REAL(NCOL)
      YMAX=REAL(NROW)
C
C Fault source picture with too many rows to process
C
      IF (NROW.GT.LNBUF/LNINT) THEN
         ERROR=5
         IDERR=1000*DEVN(LPN) + PICN(LPN)
         GOTO 90
      ENDIF
C
C Determine data form for interpolation
C
      IF (FORM.EQ.NFMBYT) THEN
         INFORM=NFMINT
      ELSE
         INFORM=FORM
      ENDIF
C
C Zero source row flags
C
      DO 10 J=1,NROW
         IB5(J)=0
   10 CONTINUE
C
C Zero pointer to blanking list
C
      IBLANK=0
C
C See if blanking is required
C
      IF (LBLANK) THEN
C
C Sort pixel positions that lie inside source picture limits, remainder
C being added to separate list for setting to specified values
C
         DO 20 I=1,N
C
C Fetch next sampling position
C
            X=RB3(I)
            Y=RB4(I)
C
C If position lies outside source picture, add point to blanking list
C
            IF (X.LT.1.0.OR.X.GT.XMAX.OR.Y.LT.1.0.OR.Y.GT.YMAX) THEN
               IB6(I)=IBLANK
               IBLANK=I
C
C Otherwise, add point to sorted row list
C
            ELSE
C
C Determine corresponding source row number
C
               J=NINT(Y)
C
C Add pixel to linked list for source row and set source row flag
C
               IB6(I)=IB5(J)
               IB5(J)=I
            ENDIF
   20    CONTINUE
C
      ELSE
C
C Sort pixel positions in the Y direction, with wrap-around for points
C outside source picture limits
C
         DO 30 I=1,N
C
C Determine corresponding source row number
C
            J=NINT(MOD(RB4(I),YMAX))
            IF (J.LE.0) THEN
               J=J+NROW
               IF (J.EQ.0) J=NROW
            ENDIF
C
C Add pixel to linked list for source row and set source row flag
C
            IB6(I)=IB5(J)
            IB5(J)=I
   30    CONTINUE
      ENDIF
C
C Process all contributing source rows
C
      DO 70 J=1,NROW
C
C Fetch source row flag
C
         I=IB5(J)
C
C Read source row and interpolate if flag is set
         IF (I.NE.0) THEN
C
C Read source row from LPN
C
            IF (SEMROW(1,RB1,INFORM,J,LAYER,LPN)) GOTO 90
C
C Extract pixel values in integer form
C
            IF (INFORM.EQ.NFMINT) THEN
   40          IF (I.NE.0) THEN
                  L=NINT(MOD(RB3(I),XMAX))
                  IF (L.LE.0) THEN
                     L=L+NCOL
                     IF (L.EQ.0) L=NCOL
                  ENDIF
C
                  IB2(I)=IB1(L)
C
C Process next pixel in linked list
C
                  I=IB6(I)
                  GOTO 40
               ENDIF
C
C Extract pixel values in floating-point form
C
            ELSE IF (INFORM.EQ.NFMFP) THEN
   50          IF (I.NE.0) THEN
                  L=NINT(MOD(RB3(I),XMAX))
                  IF (L.LE.0) THEN
                     L=L+NCOL
                     IF (L.EQ.0) L=NCOL
                  ENDIF
C
                  RB2(I)=RB1(L)
C
C Process next pixel in linked list
C
                  I=IB6(I)
                  GOTO 50
               ENDIF
C
C Extract pixel values in complex form
C
            ELSE
   60          IF (I.NE.0) THEN
                  L=NINT(MOD(RB3(I),XMAX))
                  IF (L.LE.0) THEN
                     L=L+NCOL
                     IF (L.EQ.0) L=NCOL
                  ENDIF
C
                  RB2(2*I-1)=RB1(2*L-1)
                  RB2(2*I  )=RB1(2*L)
C
C Process next pixel in linked list
C
                  I=IB6(I)
                  GOTO 60
               ENDIF
            ENDIF
         ENDIF
   70 CONTINUE
C
C Process pixels in blanking list
C
   80 IF (IBLANK.NE.0) THEN
C
C Set pixel value as specified in VALUE
C
         IF (INFORM.EQ.NFMINT) THEN
            IB2(IBLANK)=NINT(VALUE(1))
         ELSE IF (INFORM.EQ.NFMFP) THEN
            RB2(IBLANK)=VALUE(1)
         ELSE
            RB2(2*IBLANK-1)=VALUE(1)
            RB2(2*IBLANK  )=VALUE(2)
         ENDIF
C
C Process next pixel in linked list
C
         IBLANK=IB6(IBLANK)
         GOTO 80
      ENDIF
C
C Convert data to output form if necessary
C
      IF (INFORM.NE.FORM) THEN
         N4=N
         CALL CFORM(RB2,RB2,INFORM,FORM,N4)
      ENDIF
C
      EXTRNN=.FALSE.
C
   90 RETURN
C
C Copyright (C) 1987,1989,1990:  Synoptics Ltd,  All Rights Reserved
C
      END
