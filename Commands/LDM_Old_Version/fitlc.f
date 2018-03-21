C Semper 6  processing module FITLC
C
      SUBROUTINE FITLC
C
C Fits lattice Fourier components to local regions of FOURIER
C pictures, with two operating and two fitting modes
C
C Operating modes:
C - in default, fits a single peak around POSITION,PO2, or LINE,LI2
C   in terms of U,V if LINE set, returning result in T,IT
C - if TO is set, fits all U,V lines within RADIUS of origin,
C   placing results in new small FOURIER picture TO
C
C Fitting modes:
C - in default, estimates modulus by integrating intensity over
C   2 square subreg and subtracting estimated noise power; estimates
C   phase from 4 nearest neighbours (2 or 1 if ideal site has integral
C   coords)
C - if WIENER is set, fits a crossed sinc profile centred at the ideal
C   lattice site to a 4 square subreg of the data
C Noise is estimated from the periphery of an 8 by 8 region around
C the peak.  If TYPE or LOG is set, the details of the fit are reported
C accordingly.
C
C RB3-6 is used as a single buffer, in the multiple site mode, to
C hold the fitted values pending output when all fitting is complete
C
      REAL VAL
      INTEGER IVALPN
      LOGICAL SEMOPN,SEMROW,SEMLU,OPT,VARSET,SEMTFC,SEMCEN,MARSET
      LOGICAL FSINIT,FSMARK,FITLC2,SEMCON
C
      INCLUDE 'COMMON'
C
      REAL D,FXMAX,FYMAX,H,K,R,R2,SNR,T,UV(4),U1,U2,V1,V2,X,XP,Y,YP
      INTEGER*4 I4N,I4NC
      INTEGER NUV(4),I,IH,IKTL,ITO,IX,IY,J,JCURR,L,M,M1,MARK,MP
      INTEGER N,N1,NP,NCOL,NCOLOP,NROW,NROWOP
      LOGICAL ANNOT,HPL,WIEN,ALL,NOFIT,CONJ,LVERIF
C
C Packed names
C
      INTEGER NU,NU2,NV,NV2,NVERIF,NWIENE,NSNR,NFLC,NPOSIT,NPO2
      INTEGER NRADIU,NT,NT2,NLINE,NLI2,NSIZE,NSI2,NFROM,NTO
      PARAMETER (NU=-1601,NU2=-2881,NV=-3201,NV2=-4481)
      PARAMETER (NVERIF=-3419,NWIENE=-5166,NSNR=30978)
      PARAMETER (NFLC=10083,NPOSIT=26219,NPO2=26232,NRADIU=28844)
      PARAMETER (NT=-1,NT2=-1281,NLINE=19574,NLI2=19592)
      PARAMETER (NSIZE=30786,NSI2=30792,NFROM=10335,NTO=-601)
C
      EQUIVALENCE (UV,U1),(UV(2),U2),(UV(3),V1),(UV(4),V2)
C
      DATA NUV/NU,NU2,NV,NV2/
C
C Source must be Fourier and half-plane
C
      IDERR=IVALPN(NFROM)
      IF (CLASSN(LP1).NE.NCLFOU) THEN
         ERROR=6
         GOTO 130
      ENDIF
      IF (SEMTFC(LP1,HPL)) GOTO 130
      IF (.NOT.HPL) THEN
         ERROR=63
         GOTO 130
      ENDIF
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
C
C Fault multi-layer source
C
      IF (NLAYS(LP1).NE.1) THEN
         ERROR=62
         IDERR=NFLC
         GOTO 130
      ENDIF
C
C See if verification of results required
C
      LVERIF=OPT(NVERIF)
C
C Prepare for display annotation if necessary
C
      IF (MARSET(ANNOT,MARK)) GOTO 130
C
      IF (ANNOT) THEN
         IF (FSINIT(3,MARK)) GOTO 130
C
         ANNOT=FSPTYP.EQ.1
      ENDIF
C
C Set all-site mode if TO set
C
      ITO=IVALPN(NTO)
      ALL=ITO.GT.0
C
C Cut-off radius
C
      IF (VARSET(NRADIU)) THEN
         R=VAL(NRADIU)
         IF (R.LE.0.) THEN
            IDERR=NRADIU
            GOTO 140
         ENDIF
      ELSE
         R=.375*MIN0(NROW,2*(NCOL-1))
      ENDIF
      R2=R*R
C
C Max source pic coords for which b.l.-centred 8x8 block fits
C
      FXMAX=NCOL-5
      IF (FXMAX.GT.R) FXMAX=R
      FYMAX=NROW/2-4
      IF (FYMAX.GT.R) FYMAX=R
C
C Wiener mode?
C
      WIEN=SEMLU(-1,NWIENE,X)
      SNR=VAL(NSNR)
      IF (SNR.LE.0.) THEN
         IDERR=NSNR
         GOTO 140
      ENDIF
C
C Lattice base vectors
C
      DO 10 I=1,4
         UV(I)=VAL(NUV(I))
   10 CONTINUE
C
C Single site mode: establish fitting site
C
      IF (ALL) GOTO 20
      X=VAL(NPOSIT)
      Y=VAL(NPO2)
      IF (VARSET(NLINE)) THEN
         H=VAL(NLINE)
         K=VAL(NLI2)
         X=H*U1+K*V1
         Y=H*U2+K*V2
      ENDIF
      GOTO 60
C
C Multiple site mode: check U,V independent
C Check U,V independent
C
   20 D=U1*V2-U2*V1
      IF (D.EQ.0.) THEN
         IDERR=NUV(3)
         GOTO 140
      ENDIF
C
C Open output
C
      NCOLOP=VAL(NSIZE)
      I4NC=NCOLOP
      NROWOP=VAL(NSI2)
      X=NCOLOP
      IF (NCOLOP.EQ.0 .OR. NROWOP.EQ.0
     +   .OR. X*REAL(NROWOP).GT.REAL(4*LNBUF/LNCOMP)) THEN
         IDERR=NSIZE
         GOTO 140
      ENDIF
      LP2=LP1
      IF (SEMOPN(2,ITO,NCOLOP,NROWOP,1,NCLFOU,NFMCOM,LP2)) GOTO 130
C
C Place origin at left
C
      IF (SEMCEN(LP2,1,CROWN(LP2),1)) GOTO 130
C
C Zero o/p buffer
C
      I4N=I4NC*NROWOP*2
      DO 30 I=1,I4N
         RB3(I)=0.
   30 CONTINUE
C
C Begin loop over o/p rows
C
      K=CROWN(LP2)-1.
      IKTL=0
C
C Begin loop over o/p pixels
C
   40 IH=0
C
C Next site: ignore if lower half of first col
C
   50 H=IH
      IF (IH.EQ.0) THEN
         IF (K.LT.0.) GOTO 100
      ENDIF
C
C Map to source coords
C
      X=H*U1+K*V1
      Y=H*U2+K*V2
C
C Outside source pic?
C
   60 IF (X.LT.-FXMAX.OR.X.GT.FXMAX.OR.Y.LT.-FYMAX.OR.Y.GT.FYMAX) THEN
C
C Yes: fault if single site mode..
C
         IF (.NOT.ALL) THEN
            ERROR=9
            GOTO 130
C
C ..otherwise ignore
C
         ELSE
            GOTO 100
         ENDIF
      ENDIF
C
C Ignore if beyond cut-off radius in all-site mode
C
      IF (ALL.AND.X*X+Y*Y.GT.R2) GOTO 100
C
C X,Y now hold source pic coords for site H,K to be fitted
C
      XP=X
      YP=Y
C
C Round X,Y down
C
      IX=NINT(X)
      IF (X.LT.REAL(IX)) IX=IX-1
      X=X-REAL(IX)
      M1=IX-3
      IY=NINT(Y)
      IF (Y.LT.REAL(IY)) IY=IY-1
      Y=Y-REAL(IY)
      N1=IY-3
C
C Fetch block to local store
C
      L=1
      JCURR=0
      DO 80 N=N1,N1+7
         DO 70 M=M1,M1+7
            IF (M.GE.0) THEN
               MP=M
               NP=N
               CONJ=.FALSE.
            ELSE
               MP=-M
               NP=-N
               CONJ=.TRUE.
            ENDIF
C
C Read new row if nec
C
            J=CROWN(LP1)-NP
            IF (J.NE.JCURR) THEN
               IF (SEMROW(1,RB2,NFMCOM,J,1,LP1)) GOTO 130
               JCURR=J
            ENDIF
            RB1(L)=RB2(2*MP+1)
            T=RB2(2*MP+2)
            IF (CONJ) T=-T
            RB1(L+1)=T
            L=L+2
   70    CONTINUE
   80 CONTINUE
C
C Report position being fitted
C
      IF (LVERIF) THEN
         WRITE (RECORD,90) INT(H),INT(K),XP,YP
   90    FORMAT ('Line ',I3,',',I3,':   Position ',F8.2,', ',F8.2)
         IF (SEMCON(' ')) GOTO 130
         IF (SEMCON(RECORD)) GOTO 130
      ENDIF
C
C Call profile fitting routine
C
      IF (FITLC2(X,Y,SNR,WIEN,LVERIF,NOFIT)) GOTO 130
      IF (.NOT.ALL) GOTO 120
C
C Forget this point if no reasonable fit was possible
C
      IF (NOFIT) GOTO 100
C
C Mark position fitted
C
      IF (ANNOT) THEN
         IF (FSMARK(XP,YP,FSMMOD,FSMSIZ)) GOTO 130
      ENDIF
C
C Deposit fitted value
C
      I4N=(I4NC*IKTL+IH+1)*2
      RB3(I4N-1)=X
      RB3(I4N)=Y
C
C If (0,+K), i.e. upper half of first col, conjugate to lower half too
C
      IF (IH.EQ.0) THEN
         IF (K.GT.0.) THEN
            I4N=I4N+I4NC*4*INT(K)
            RB3(I4N-1)=X
            RB3(I4N)=-Y
         ENDIF
      ENDIF
C
C Next site?
C
  100 IF (.NOT.ALL) THEN
         ERROR=9
         GOTO 130
      ENDIF
      IH=IH+1
      IF (IH.LT.NCOLOP) GOTO 50
      K=K-1.
      IKTL=IKTL+1
      IF (IKTL.LT.NROW) GOTO 40
C
C Multiple site scan complete: output result
C
      I4N=1
C
C Break up section buffer and write rows
C
      DO 110 J=1,NROWOP
         IF (SEMROW(2,RB3(I4N),NFMCOM,J,1,LP2)) GOTO 130
         I4N=I4N+2*NCOLOP
  110 CONTINUE
C
      GOTO 130
C
C Single site mode
C
  120 IF (NOFIT) THEN
         ERROR = 77
         IDMESS = 'No significant peak present'
      ELSE
C
C Mark it
C
         IF (ANNOT) THEN
            IF (FSMARK(XP,YP,FSMMOD,FSMSIZ)) GOTO 130
         ENDIF
C
         IF (SEMLU(1,NT,X)) GOTO 130
         IF (SEMLU(1,NT2,Y)) GOTO 130
      ENDIF
C
  130 RETURN
C
C Error in parameter
C
  140 ERROR=3
      GOTO 130
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C FITLC2 fits crossed sinc profile to noisy peak neighbourhood
C
      LOGICAL FUNCTION FITLC2(XP,YP,SNR,WIEN,LVERIF,NOFIT)
      REAL XP,YP,SNR
      LOGICAL WIEN,LVERIF,NOFIT
C
C Common F supplies (complex) data values for a 8 by 8 subregion around
C the peak; XP,YP are x,y offsets of true peak posn rel to bottom left
C of central square, and should in be in range 0 to 1-e
C
      LOGICAL SEMCON
C
      INCLUDE 'COMMON'
C
      REAL CHISQ,POWER,R,SIGMA,T,T1,T2,X,Y
      REAL F(2,8,8),FCEN(2,8,4),SINCX(4),SINCY(4),NU
C
      INTEGER I,J,M2,N,N2
C
      EQUIVALENCE (RB1,F), (F(1,3,3),FCEN)
C
      FITLC2=.TRUE.
C
C Find mean peripheral noise power
C
      NU=0.
      DO 10 I=1,8
         NU=NU+F(1,I,1)**2+F(2,I,1)**2+F(1,I,8)**2+F(2,I,8)**2
   10 CONTINUE
C
      DO 20 J=2,7
         NU=NU+F(1,1,J)**2+F(2,1,J)**2+F(1,8,J)**2+F(2,8,J)**2
   20 CONTINUE
C
      NU=NU/28.
C
      SIGMA=SQRT(NU)
C
      IF (LVERIF) THEN
         WRITE (RECORD,30) NU,SIGMA
   30    FORMAT ('Estimated mean noise power ',G12.4,
     +           ' => rms noise modulus ',G12.4)
         IF (SEMCON(RECORD)) GOTO 160
      ENDIF
C
C Obtain signal power integrated over 2 by 2 block only
C
      POWER=0.
      DO 40 I=1,2
         POWER=POWER+F(I,4,4)**2+F(I,4,5)**2+F(I,5,4)**2+F(I,5,5)**2
   40 CONTINUE
      POWER=POWER-NU*4.
C
C Report integrated peak intensity
C
      T=POWER/NU
C
      IF (LVERIF) THEN
         WRITE (RECORD,50) POWER,T
   50    FORMAT ('Integrated signal power ',G12.4,
     +           ' =',F10.2,' times mean noise power')
         IF (SEMCON(RECORD)) GOTO 160
      ENDIF
C
C Return reporting insignificant peak
C
      IF (T.LE.SNR) THEN
         FITLC2=.FALSE.
         NOFIT=.TRUE.
         GOTO 160
      ENDIF
C
C Prepare ideal crossed sinc profile
C
      X=1.+XP
      Y=1.+YP
      DO 60 I=1,4
         T=1.
         IF (X.NE.0.) T=SIN(PI*X)/PI/X
         SINCX(I)=T
         T=1.
         IF (Y.NE.0.) T=SIN(PI*Y)/PI/Y
         SINCY(I)=T
         X=X-1.
         Y=Y-1.
   60 CONTINUE
C
C Estimate coefficient in simple-minded way
C
      R=SQRT(POWER)
      M2=3
      IF (XP.EQ.0.) M2=2
      N2=3
      IF (YP.EQ.0.) N2=2
      T1=0.
      T2=0.
      N=0
      DO 80 J=2,N2
         DO 70 I=2,M2
           T1=T1+FCEN(1,I,J)
           T2=T2+FCEN(2,I,J)
           N=N+1
   70    CONTINUE
   80 CONTINUE
      T=R/SQRT((T1*T1)+(T2*T2))
      XP=T1*T
      YP=T2*T
C
      IF (LVERIF) THEN
         WRITE (RECORD,90) N,XP,YP
   90    FORMAT ('Combining simple ',I1,' point phase gives'
     +           ' component ',G12.4,','G12.4)
         IF (SEMCON(RECORD)) GOTO 160
      ENDIF
C
      IF (WIEN) THEN
C
C Estimate component by Wiener LS fit, SUM(s.f)/[SUM(s.s)+nu/<c.c>]
C with <c.c> estimated via initial evaln with nu=0 (LS fit to data)
C
         T1=0.
         T2=0.
         T=0.
         DO 110 J=1,4
            DO 100 I=1,4
               T1=T1+SINCX(I)*SINCY(J)*FCEN(1,I,J)
               T2=T2+SINCX(I)*SINCY(J)*FCEN(2,I,J)
               T=T+(SINCX(I)*SINCY(J))**2
  100       CONTINUE
  110    CONTINUE
         XP=T1/T
         YP=T2/T
         T=T+NU/((XP*XP)+(YP*YP))
         XP=T1/T
         YP=T2/T
C
         IF (LVERIF) THEN
            WRITE (RECORD,120) XP,YP
  120       FORMAT ('Wiener LS fit gives component ',G12.4,',',G12.4)
            IF (SEMCON(RECORD)) GOTO 160
         ENDIF
C
C Quantify profile quality via chi-squared difference
C
         CHISQ=0.
         DO 140 J=1,4
            DO 130 I=1,4
               T1=XP*SINCX(I)*SINCY(J)-FCEN(1,I,J)
               T2=YP*SINCX(I)*SINCY(J)-FCEN(2,I,J)
               CHISQ=CHISQ+(T1*T1)+(T2*T2)
  130       CONTINUE
  140    CONTINUE
         CHISQ=CHISQ/NU
C
C ?? exact value for Chi-squared (5% level, 16 degrees)
C
         T=CHISQ/1.25/21.
C
         IF (LVERIF) THEN
            WRITE (RECORD,150) CHISQ,T
  150       FORMAT ('Chi squared data deviation ',F6.1,
     +              ' = ',F4.1,' times 5% confidence level')
            IF (SEMCON(RECORD)) GOTO 160
         ENDIF
      ENDIF
C
      FITLC2=.FALSE.
      NOFIT=.FALSE.
C
  160 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
