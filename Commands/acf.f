C Semper 6 processing module ACF
C
      SUBROUTINE ACF
C
C Code for ACF command to determine the autocorrelation image of the
C source picture.  The command carries out the necessary transformations
C according to whether the source picture class is IMAGE, FOURIER or
C SPECTRUM.  Any other picture class is faulted.
C
      LOGICAL FT2D,INVFT2
      INTEGER IPACK,IVALPN
C
      LOGICAL MSQ
      INTEGER NPIC,CLASS
C
      INCLUDE 'COMMON'
C
C Fetch the output picture number specified with the TO key
C
      NPIC=IVALPN(-601)
C
C Fetch source picture class
C
      CLASS=CLASSN(LP1)
C
C If picture class is IMAGE, carry out forward Fourier transform
C
      IF (CLASS.EQ.NCLIMA) THEN
C
C Set flag to obtain modulus-square of Fourier transform (saves space
C because result is real rather than complex)
C
         MSQ=.TRUE.
C
C Carry out forward Fourier transform
C
         IF (FT2D(NPIC,NCLCOR,NFMFP,MSQ,.FALSE.)) GOTO 10
C
C Output picture becomes the source picture for the second stage of
C processing
C
         LP1=LP2
C
C Otherwise, if picture class is FOURIER, reset modulus-square flag
C
      ELSE IF (CLASS.EQ.NCLFOU) THEN
C
C FOURIER class picture does not contain modulus-square values
C
         MSQ=.FALSE.
C
C Otherwise, if picture class is SPECTRUM, set modulus-square flag
C
      ELSE IF (CLASS.EQ.NCLSPE) THEN
C
C SPECTRUM class picture already contains modulus-square values
C
         MSQ=.TRUE.
C
C Otherwise, fault bad source picture class
C
      ELSE
         ERROR=6
         IDERR=1000*DEVN(LP1)+PICN(LP1)
         GOTO 10
      ENDIF
C
C Invert modulus-square flag to request modulus-square calculation only
C if source picture is not in this form
C
      MSQ=.NOT.MSQ
C
C Carry out inverse Fourier transform, zeroing centre and normalising
C by final centre
C
      IF (INVFT2(NPIC,NCLCOR,MSQ,.TRUE.,-1.0)) GOTO 10
C
   10 RETURN
C
C Copyright (C) 1992:  Synoptics Ltd,  All Rights Reserved
C
      END
