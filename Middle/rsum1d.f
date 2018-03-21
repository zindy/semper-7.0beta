C Semper 6 subsidiary module RSUM1D
C
      SUBROUTINE RSUM1D(BOUT,BIN,N,WIDTH,STEP)
C
C 1-D running sum module convolving BIN with rect (width WIDTH)
C into BOUT; edges are processed assuming outside values continued
C repeatedly; WIDTH must be in range 0 to NCOL.
C (BIN copied inefficiently but exactly for WIDTH=0,1)
C
C Treats data as N reals if STEP=1, complexes if STEP=2
C
      REAL BOUT(*),BIN(*)
      INTEGER N,WIDTH,STEP
C
      REAL B1,T
      INTEGER I,I0,I1,I2,I3,LAST,WIDL,WIDR
C
      LAST=N*STEP
      WIDL=WIDTH/2+1
      WIDR=(WIDTH-1)/2
      I0=1
C
C Initialise running average at LH edge
C
   10 B1=BIN(I0)
      T=B1*WIDL
      I2=I0
      IF (WIDR.GE.1) THEN
         DO 20 I=1,WIDR
            T=T+BIN(I2)
            I2=I2+STEP
   20    CONTINUE
      ENDIF
C
C Set LH edge
C
      I3=I0
   30 T=BIN(I2)-B1+T
      BOUT(I3)=T
      I2=I2+STEP
      I3=I3+STEP
      IF (I3.LE.WIDL*STEP) GOTO 30
C
C Set central range
C
      I1=I0
   40 IF (I2.LE.LAST) THEN
         T=BIN(I2)-BIN(I1)+T
         BOUT(I3)=T
         I1=I1+STEP
         I2=I2+STEP
         I3=I3+STEP
         GOTO 40
      ENDIF
C
C Set RH edge
C
      B1=BIN(LAST-STEP+I0)
   50 IF (I3.LE.LAST) THEN
         T=B1-BIN(I1)+T
         BOUT(I3)=T
         I1=I1+STEP
         I3=I3+STEP
         GOTO 50
      ENDIF
C
C Repeat for imaginary part?
C
      IF (STEP.LE.1 .OR. I0.NE.1) RETURN
      I0=2
      GOTO 10
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
