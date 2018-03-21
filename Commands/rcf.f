C Semper 6 processing module RCF
C
      SUBROUTINE RCF
C
C Provides verb RCF, which prepares in TO an array of x-c coefficients
C between corresponding points in fts LP1,LP3 as a function of spatial
C frequency; IF PHASE is set, the phase residuals (rms phase difference,
C with weighting proportional to modulus sum) are prepared instead
C
C Global declarations
C
      INCLUDE 'COMMON'
C
C Local declarations
C
      REAL VAL
      INTEGER IVAL,IVALPN
      LOGICAL SEMOPN,SEMROW,SEMTFC,SEMCEN,OPT
C
      REAL DELT,R,X,XI,XR,Y,YI,YR
      INTEGER I,I0,I1,I2,IR,IRMAX,IS,J,J0,JDSQ,NCOL,NCOL2,NROW,WIDTH
      LOGICAL LPHASE,HPL
C
      REAL PIBY2,PIBY4
      PARAMETER (PIBY2=PI/2.,PIBY4=PI/4.)
C
C Packed names
C
      INTEGER NFROM,NWITH,NTO,NOVER,NPHASE,NRADIU
      PARAMETER (NFROM=10335,NWITH=-5181,NTO=-601)
      PARAMETER (NOVER=24885,NPHASE=25921,NRADIU=28844)
C
C Check FROM Fourier, half-plane, from square original
C
      IDERR=IVALPN(NFROM)
      IF (CLASSN(LP1).NE.NCLFOU) GOTO 80
      IF (SEMTFC(LP1,HPL)) GOTO 70
      IF (.NOT.HPL) THEN
         ERROR=63
         GOTO 70
      ENDIF
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      IF (2*(NCOL-1).NE.NROW) GOTO 90
      IDERR=IVALPN(NWITH)
      IF (CLASSN(LP3).NE.NCLFOU) GOTO 80
      IF (NCOLS(LP3).NE.NCOL.OR.NROWS(LP3).NE.NROW) GOTO 90
C
C Initialise
C
      WIDTH=MAX(IVAL(NOVER),1)
      IF (WIDTH.GT.NCOL) THEN
         ERROR=3
         IDERR=NOVER
         GOTO 70
      ENDIF
      LPHASE=OPT(NPHASE)
      IRMAX=NCOL-2
      I0=CCOLN(LP1)
      J0=CROWN(LP1)
      NCOL2=NCOL+NCOL
C
C Open output picture with origin at left
C
      LP2=LP1
      IF (SEMOPN(2,IVALPN(NTO),NCOL,1,2,NCLCOR,NFMFP,LP2)) GOTO 70
      IF (SEMCEN(LP2,1,1,1)) GOTO 70
C
C Initialise accumulators
C
      DO 10 I=1,NCOL2
         RB1(I)=0.
         RB2(I)=0.
         RB3(I)=0.
   10 CONTINUE
C
C Begin pass through picture rows
C
      J=J0-IRMAX
   20 JDSQ=J-J0
      JDSQ=JDSQ*JDSQ
      IF (SEMROW(1,RB6,NFMCOM,J,1,LP1)) GOTO 70
      IF (SEMROW(1,RB5,NFMCOM,J,1,LP3)) GOTO 70
C
C Find range of pixels within IRMAX of centre
C
      R=IRMAX*IRMAX-JDSQ
      I=SQRT(R)
      I1=1
      I2=I+1
C
C In lower half, omit first col
C
      IF (J.GT.J0) I1=2
C
C Pass along row..
C
      DO 30 I=I1,I2
         IS=I+I-1
         XR=RB6(IS)
         XI=RB6(IS+1)
         YR=RB5(IS)
         YI=RB5(IS+1)
C
C ..working out sp freq..
C
         IR=I-I0
         R=IR*IR+JDSQ
         IR=SQRT(R)+1.5
C
C ..and accumulating either re(x.y*) in rb1
C                              x.x*  in rb2
C                          and y.y*  in rb3
C
         IF (.NOT.LPHASE) THEN
            RB1(IR)=RB1(IR)+XR*YR+XI*YI
            RB2(IR)=RB2(IR)+XR*XR+XI*XI
            RB3(IR)=RB3(IR)+YR*YR+YI*YI
C
C ..or w*(phase(x)-phase(y))**2 in rb1, with weight w=mod(x)+mod(y)
C      and w itself in rb2
C
         ELSE
            X=XR*XR+XI*XI
            Y=YR*YR+YI*YI
            IF (X.EQ.0. .OR. Y.EQ.0.) GOTO 30
            X=SQRT(X)+SQRT(Y)
            RB2(IR)=RB2(IR)+X
            Y=XR*YR+XI*YI
            XI=XI*YR-XR*YI
            DELT=ATAN2(XI,Y)
            RB1(IR)=RB1(IR)+X*(DELT*DELT)
         ENDIF
   30 CONTINUE
C
C More rows to come?
C
      J=J+1
      IF (J.LE.J0) GOTO 20
C
C Apply local average to the sums (or copy if OVER defaulted)
C
C NB: a running sum is NOT safe, given the rapid decay in magnitude left
C to right over several orders of magnitude: the rounding errors can be
C disastrous
C
      CALL RCF2(RB4,RB1,NCOL,WIDTH)
      CALL RCF2(RB5,RB2,NCOL,WIDTH)
      CALL RCF2(RB6,RB3,NCOL,WIDTH)
C
C Form cross correlation coefficient or phase residual expression
C
      DO 40 I=1,NCOL
         XR=RB5(I)
         IF (.NOT.LPHASE) XR=XR*RB6(I)
         IF (XR.NE.0.) THEN
            IF (LPHASE) THEN
               RB4(I)=SQRT(RB4(I)/XR)
            ELSE
               RB4(I)=RB4(I)/SQRT(XR)
            ENDIF
         ENDIF
   40 CONTINUE
C
C Output correlation function
C
      IF (SEMROW(2,RB4,NFMFP,1,1,LP2)) GOTO 70
C
C Note fraction of Fourier coefficients that are mutually independent
C (area of any real space mask imposed / total picture area)
C
      R = VAL(NRADIU)
      R = PI*R*R/REAL(NCOL*NCOL)
      IF (R.LE.0.) R=1.
C
C Generate 2*SD significance threshold in layer 2
C
      I0=WIDTH/2+1
      DO 50 I=I0,NCOL
C
C For RCF, twice 1/root(N) with N number of indep coeffs within band
C
         IF (.NOT.LPHASE) THEN
            I2=I-I0
            I1=I+(WIDTH-1)/2
            XR=PIBY2*(I1*I1-I2*I2)*R
            RB5(I)=2.*SQRT(1./XR)
C
C For phase residuals, simply pi/4
C
         ELSE
            RB5(I)=PIBY4
         ENDIF
   50 CONTINUE
C
C Fill in lhs and output
C
      DO 60 I=1,I0-1
         RB5(I)=RB5(I0)
   60 CONTINUE
      IF (SEMROW(2,RB5,NFMFP,1,2,LP2)) CONTINUE
   70 RETURN
C
C Errors
C
   80 ERROR=6
      GOTO 70
   90 ERROR=5
      GOTO 70
C
C Copyright (C) 1987,1989,1991:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 local subsidiary module RCF2
C
      SUBROUTINE RCF2(BOUT,BIN,N,WIDTH)
C
C Sums BIN to BOUT, over strips WIDTH elements wide, truncated at ends.
C Copies exactly if WIDTH=0,1
C
      INTEGER N
      REAL BIN(N),BOUT(N)
      INTEGER WIDTH
C
      REAL T
      INTEGER I,I1,I2,K,WIDL,WIDR
C
      WIDL=WIDTH/2
      WIDR=(WIDTH-1)/2
      DO 20 K=1,N
         I1=MAX(K-WIDL,1)
         I2=MIN(K+WIDR,N)
         T=0.
         DO 10 I=I1,I2
            T=T+BIN(I)
   10    CONTINUE
   20 BOUT(K)=T
      RETURN
C
C Copyright (C) 1991:  Synoptics Ltd,  All Rights Reserved
C
      END
