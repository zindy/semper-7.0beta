C Semper 6 processing module PS
C
      SUBROUTINE PS
C
C Code for PS command to derive the power spectrum of the source
C picture.  The command carries out the necessary transformations
C according to whether the source picture class is IMAGE, CORRELATION
C or FOURIER.  Any other picture class is faulted.  If the option LN
C is set, the natural logarithm of the power spectrum is returned in
C the output picture.
C
      LOGICAL OPT,SEMOPN,SEMROW,FT2D
      INTEGER IPACK,IVALPN
C
      LOGICAL MSQ,LN
      INTEGER NPIC,CLASS,NCOL,NROW,J
C
      INCLUDE 'COMMON'
C
C See if option LN is set
C
      LN=OPT(19760)
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
C Set flag to obtain modulus-square of Fourier transform directly
C
         MSQ=.TRUE.
C
C Carry out forward Fourier transform
C
         IF (FT2D(NPIC,NCLSPE,NFMFP,MSQ,LN)) GOTO 20
C
C If picture class is CORRELATION, carry out forward Fourier transform.
C
      ELSE IF (CLASS.EQ.NCLCOR) THEN
C
C Set flag to suppress modulus-square calculation
C
         MSQ=.FALSE.
C
C Carry out forward Fourier transform
C
         IF (FT2D(NPIC,NCLSPE,NFMFP,MSQ,LN)) GOTO 20
C
C Otherwise, if picture class is FOURIER, just carry out modulus-square
C calculation
C
      ELSE IF (CLASS.EQ.NCLFOU) THEN
C
C Fetch source picture size
C
         NCOL=NCOLS(LP1)
         NROW=NROWS(LP1)
C
C Open output picture
C
         LP2=LP1
         IF (SEMOPN(2,NPIC,NCOL,NROW,1,NCLSPE,NFMFP,LP2)) RETURN
C
C Carry out modulus-square calculation
C
         DO 10 J=1,NROW
            IF (SEMROW(1,RB1,NFMCOM,J,1,LP1)) GOTO 20
            CALL FTLMSQ(RB1,NCOL,LN)
            IF (SEMROW(2,RB1,NFMCOM,J,1,LP2)) GOTO 20
   10    CONTINUE
C
C Otherwise, fault bad source picture class
C
      ELSE
         ERROR=6
         IDERR=1000*DEVN(LP1)+PICN(LP1)
      ENDIF
C
   20 RETURN
C
C Copyright (C) 1992:  Synoptics Ltd,  All Rights Reserved
C
      END
