C Semper 6 processing module FFT
C
      SUBROUTINE FFT
C
C Code for FOURIER/IMAGE commands to carry out forward/inverse Fourier
C Transforms.  The source picture class must be IMAGE for the FOURIER
C command and FOURIER for the IMAGE command.  Any other picture class
C is faulted.
C
      LOGICAL FT2D,INVFT2
      INTEGER IVALPN !,IPACK
C
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
C If command is FOURIER, carry out forward Fourier transform
C
      IF (VERB.EQ.10221) THEN
C
C Fault picture class if not IMAGE
C
         IF (CLASS.NE.NCLIMA) GOTO 20
C
C Carry out forward Fourier transform
C
         IF (FT2D(NPIC,NCLFOU,NFMCOM,.FALSE.,.FALSE.)) GOTO 10
C
C Otherwise, carry out inverse Fourier transform
C
      ELSE
C
C Fault picture class if not FOURIER
C
         IF (CLASS.NE.NCLFOU) GOTO 20
C
C Carry out inverse Fourier transform
C
         IF (INVFT2(NPIC,NCLIMA,.FALSE.,.FALSE.,0.0)) GOTO 10
      ENDIF
C
   10 RETURN
C
C Fault bad source picture class
C
   20 ERROR=6
      IDERR=1000*DEVN(LP1)+PICN(LP1)
      GOTO 10
C
C Copyright (C) 1992:  Synoptics Ltd,  All Rights Reserved
C
      END
