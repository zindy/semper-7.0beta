C Semper 6 processing module IMAGE
C
      SUBROUTINE IMAGE
C
C Top level code for IMAGE command.  Calls subroutine for Fourier or
C Walsh transform according to the source picture class.  Any class of
C picture other than Fourier or Walsh is faulted.
C
      INTEGER CLASS
C
      INCLUDE 'COMMON'
C
C Fetch source picture class
C
      CLASS=CLASSN(LP1)
C
C If Fourier class picture, carry out inverse Fourier transform
C
      IF (CLASS.EQ.NCLFOU) THEN
         CALL FFT
C
C Otherwise, if Walsh class picture, carry out inverse Walsh transform
C
      ELSE IF (CLASS.EQ.NCLWAL) THEN
         CALL WALSH
C
C Otherwise, fault bad source picture class
C
      ELSE
         ERROR=6
         IDERR=1000*DEVN(LP1)+PICN(LP1)
      ENDIF
C
      RETURN
C
C Copyright (C) 1992:  Synoptics Ltd,  All Rights Reserved
C
      END
