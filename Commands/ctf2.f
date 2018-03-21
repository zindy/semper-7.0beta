C Semper 6 subsidiary module CTF2
C
      SUBROUTINE CTF2
C
C Generates electron optical transfer functions with beam tilt,
C displaced objective apertures, coherence envelopes, specimen
C vibration and uniaxial specimen drift.
C There are 4 types of transfer functions available - WAVE, AMPLITUDE,
C PHASE and AMPLITUDE + PHASE.  For each of these, the option SQUARED
C causes the modulus squared of the transfer function to be applied and
C option CC causes the complex conjugate to be applied.
C The output picture contains by default the CTF istself.  However, the
C option ADD or MULTIPLY may be quoted to combine the CTF with a source
C picture FROM.  In the default case, the keys SIZE and SI2 determine
C the output picture size.  SI2 defaults to SIZE if unset.  The centre
C position is set to the left (half plane fourier) if option HALF is
C set.
C The key DEFOCUS and the variables STEP, APHI, SWIDTH, EWIDTH, DRIFT,
C DPHI, OARADIUS, VIBRATION, ASTIGMATISM, TILT, TPHI, OADISPLACEMENT and
C OAPHI define the characteristics of the CTF.  These are in reduced
C units unless option PHYSICAL is specified, in which case the
C variables KV and CS are used to determine the units.
C
      REAL VAL
      INTEGER IVAL,IVALPN,SEMFRM
      LOGICAL OPT,VARSET,SEMOPN,SEMCEN,SEMTFC,SEMROW,CONOPT,SEMCON
C
      INCLUDE 'COMMON'
C
      REAL A,A0MK,A0PK,ACA,APHI,ASA,ASTIG
      REAL CA,CS,CTFAI,CTFAR,CTFI,CTFPI,CTFPR,CTFR
      REAL DD,DEFOC,DELTX,DELTY,DELX,DELY,DKX,DKY,DMAB2
      REAL DPHI,DRIFT,DRIFTX,DRIFTY,DTMKY2,DTPKY2,EWI,G,G0,GL
      REAL KX,KY,KYSQ,KDY,KD
      REAL OADISP,OAPHI,OARAD,OARAD2,OAX,OAY,P
      REAL SA,SCH,SS,STEP,ST2,SWI
      REAL T1I,T1R,T2,T2I,T2R,TILT,TMK2,TMKA,TMKX,TMKY,TMKY2,TMKYSA
      REAL TPHI,TPK2,TPKA,TPKX,TPKY,TPKY2,TPKYSA,TX,TY
      REAL V,VIB,VIBKSQ,VIBRR,X
      INTEGER CLASS,CCOL,CROW,FORM,I,IX,IY,J,MODE,NCOL,NLAY,NROW
      LOGICAL ADD,MUL,WAVE,AMP,PHASE,SQU,CC,HALF
C
C Packed names
C
      INTEGER NADD,NMULTI,NSQUAR,NCC,NWAVE,NAMPLI,NPHASE,NHALF
      INTEGER NFROM,NTO,NSIZE,NSI2,NPHYSI,NKV,NCS
      INTEGER NSTEP,NST2,NAPHI,NSWIDT,NEWIDT,NDRIFT,NDPHI,NOARAD
      INTEGER NVIBRA,NDEFOC,NASTIG,NTILT,NTPHI,NOADIS,NOAPHI,NVERIF
C
      PARAMETER (NADD=1764, NMULTI=21652, NSQUAR=31101, NCC=4920)
      PARAMETER (NWAVE=-4863, NAMPLI=2136, NPHASE=25921, NHALF=12852)
      PARAMETER (NFROM=10335, NTO=-601, NSIZE=30786, NSI2=30792)
      PARAMETER (NPHYSI=25945, NKV=18480, NCS=5560)
      PARAMETER (NSTEP=31205, NST2=31232)
      PARAMETER (NAPHI=2248, NSWIDT=31329, NEWIDT=8929)
      PARAMETER (NDRIFT=7129, NDPHI=7048, NOARAD=24058)
      PARAMETER (NVIBRA=-3563, NDEFOC=6606, NASTIG=2380)
      PARAMETER (NTILT=-373, NTPHI=-649, NOADIS=24044, NOAPHI=24056)
      PARAMETER (NVERIF=-3419)
C
C See if option ADD or MULTIPLY is set
      ADD=OPT(NADD)
      MUL=OPT(NMULTI)
C Fault conflict
      IF (CONOPT(NADD,NMULTI)) GOTO 100
C
C See if option WAVE, AMPLITUDE or PHASE is set
      WAVE=OPT(NWAVE)
      AMP=OPT(NAMPLI)
      PHASE=OPT(NPHASE)
C Fault conflicts
      IF (CONOPT(NWAVE,NAMPLI)) GOTO 100
      IF (CONOPT(NWAVE,NPHASE)) GOTO 100
C Set option PHASE if options WAVE, AMPLITUDE or PHASE not set
      IF (.NOT.(WAVE.OR.AMP.OR.PHASE)) PHASE=.TRUE.
C
C Set mode for CTF
      IF (WAVE) MODE=1
      IF (AMP) MODE=2
      IF (PHASE) MODE=3
      IF (AMP.AND.PHASE) MODE=4
C
C See if option SQUARED or CC is set
      SQU=OPT(NSQUAR)
      CC=OPT(NCC)
C
C Option ADD or MULTIPLY requires source picture to be opened
      IF (ADD.OR.MUL) THEN
C
C Open source picture
         IF (SEMOPN(1,IVALPN(NFROM),NCOL,NROW,NLAY,CLASS,FORM,LP1))
     +      GOTO 100
C
C Fault multi-layer source picture
         IF (NLAY.NE.1) THEN
            ERROR=62
            IDERR=VERB
            GOTO 100
         ENDIF
C
C Fault source picture class if not fourier or power spectrum
         IF (CLASS.NE.NCLFOU.AND.CLASS.NE.NCLSPE) THEN
            ERROR=6
            IDERR=IVALPN(NFROM)
            GOTO 100
         ENDIF
C
C Fault bad centre position for source picture
         IF (SEMTFC(LP1,HALF)) GOTO 100
C
C Otherwise, determine output picture size
      ELSE
         LP1=0
C
C Output picture size determined by keys SIZE and SI2
         NCOL=IVAL(NSIZE)
C
         IF (VARSET(NSI2)) THEN
            NROW=IVAL(NSI2)
         ELSE
            NROW=NCOL
         ENDIF
C
C Output picture is half-plane fourier if option HALF is set
         HALF=OPT(NHALF)
      ENDIF
C
C Fault option WAVE if output picture is half-plane fourier
      IF (WAVE.AND.HALF) THEN
         ERROR=164
         GOTO 100
      ENDIF
C
C Open output picture
      LP2=LP1
      IF (SEMOPN(2,IVALPN(NTO),NCOL,NROW,1,NCLFOU,SEMFRM(NFMCOM),LP2))
     +   GOTO 100
C
C Update centre position if half plane fourier
      IF (HALF) THEN
         IF (SEMCEN(LP2,1,1+NROW/2,1)) GOTO 100
      ENDIF
C
C Fetch output picture centre position
      CCOL=CCOLN(LP2)
      CROW=CROWN(LP2)
C
C Initialise elecron optical parameters
      IF (OPT(NPHYSI)) THEN
C
C
         V=VAL(NKV)*1E3
         IF (V.EQ.0.0) THEN
            ERROR=3
            IDERR=NKV
            GOTO 100
         ENDIF
C
C
         CS=VAL(NCS)*1E6
         IF (CS.EQ.0.0) THEN
            ERROR=3
            IDERR=NCS
            GOTO 100
         ENDIF
C
C Wavelength/nm
         X=V*1.60206E-4/9.1083/2.99763**2
         X=6.62517/SQRT(2.0*1.60206*9.1083*V*(1.0+X/2.0))
C
C Scherzer and Glaser units/nm and Glaser/Scherzer/mrad
         SCH=SQRT(CS*X)
         GL=SQRT(SQRT(CS*X*X*X))
         A=GL/SCH*1E3
      ELSE
         SCH=1.0
         GL=1.0
         A=1.0
      ENDIF
C
C Fault zero or negative increment
      STEP=VAL(NSTEP)/GL
      IF (STEP.LE.0.0) THEN
         ERROR=3
         IDERR=NSTEP
         GOTO 100
      ENDIF
      ST2=VAL(NST2)/GL
      IF (ST2.LE.0.0) ST2=STEP
C
C Fetch remaining keys and variables
      APHI=VAL(NAPHI)
      SWI=VAL(NSWIDT)/A
      EWI=VAL(NEWIDT)/SCH
      DRIFT=VAL(NDRIFT)/GL
      DPHI=VAL(NDPHI)
      OARAD=VAL(NOARAD)/A
      VIB=VAL(NVIBRA)/GL
      DEFOC=VAL(NDEFOC)/SCH
      ASTIG=VAL(NASTIG)/SCH
      OADISP=VAL(NOADIS)/A
      OAPHI=VAL(NOAPHI)
      TILT=VAL(NTILT)/A
      TPHI=VAL(NTPHI)
C
C Determine further useful parameters
      IF (OARAD.NE.0.0) THEN
         OARAD2=OARAD*OARAD
      ELSE
         OARAD2=1E4
      ENDIF
      DMAB2=DEFOC-ASTIG/2.0
      CA=COS(APHI)
      SA=SIN(APHI)
      ACA=ASTIG*CA
      ASA=ASTIG*SA
      DRIFTX=DRIFT*COS(DPHI)
      DRIFTY=DRIFT*SIN(DPHI)
      VIBRR=(PI*VIB)**2
      SS=(PI*SWI)**2
      DD=(PI*EWI)**2/2.0
      OAX=OADISP*COS(OAPHI)
      OAY=OADISP*SIN(OAPHI)
      TX=TILT*COS(TPHI)
      TY=TILT*SIN(TPHI)
      T2=TILT*TILT
C
C Fault primary beam outside aperture
      IF (.NOT.WAVE.AND.(TX-OAX)**2+(TY-OAY)**2.GT.OARAD2) THEN
         ERROR = 77
         IDMESS = 'Primary beam outside aperture'
         GOTO 100
      ENDIF
C
C T0(0)
      TPKA=TX*CA+TY*SA
      G0=PI*((T2/2.0-DMAB2)*T2-ASTIG*TPKA*TPKA)
C
C Del(0)
      V=T2-DMAB2
      DELTX=V*TX-ACA*TPKA
      DELTY=V*TY-ASA*TPKA
C
C Determine X and Y increments
      IF (HALF) THEN
         DKX=(1.0/STEP)/REAL(2*(NCOL-1))
      ELSE
         DKX=(1.0/STEP)/REAL(NCOL)
      ENDIF
      DKY=(1.0/ST2)/REAL(NROW)
C
C Initialise Y coordinate
      IY=CROW
C
C Generate CTF over entire output picture
      DO 90 J=1,NROW
C
C Read source row from LP1 if required
         IF (ADD.OR.MUL) THEN
            IF (SEMROW(1,RB1,NFMCOM,J,1,LP1)) GOTO 100
         ENDIF
C
C Decrement Y coordinate
         IY=IY-1
         KY=REAL(IY)*DKY
C
         KYSQ=KY*KY
         KDY=KY*DRIFTY
         TPKY=TY+KY
         TPKY2=TPKY*TPKY
         TPKYSA=TPKY*SA
         DTPKY2=(TPKY-OAY)**2
         TMKY=TY-KY
         TMKY2=TMKY*TMKY
         TMKYSA=TMKY*SA
         DTMKY2=(TMKY-OAY)**2
C
C Initialise X coordinate
         IX=-CCOL
C
C Generate CTF along output picture row
         DO 80 I=1,2*NCOL,2
C
C Increment X coordinate
            IX=IX+1
            KX=REAL(IX)*DKX
C
            VIBKSQ=VIBRR*(KX*KX+KYSQ)
            T1R=0.0
            T1I=0.0
            T2R=0.0
            T2I=0.0
            CTFR=0.0
            CTFI=0.0
C
C First side band - A0(K)
            TPKX=TX+KX
            IF ((TPKX-OAX)**2+DTPKY2.GT.OARAD2) GOTO 10
            TPK2=TPKX*TPKX+TPKY2
            TPKA=TPKX*CA+TPKYSA
            V=TPK2-DMAB2
            DELX=V*TPKX-ACA*TPKA-DELTX
            DELY=V*TPKY-ASA*TPKA-DELTY
            V=TPK2-T2
            V=SS*(DELX*DELX+DELY*DELY)+DD*V*V+VIBKSQ
            IF (V.GT.7.0) GOTO 10
C
C
            A0PK=EXP(-V)
C
C T0(K) * A0(K)
            G=PI*((0.5*TPK2-DMAB2)*TPK2-ASTIG*TPKA*TPKA)-G0
            T1R=COS(G)*A0PK
            T1I=-SIN(G)*A0PK
C
C Second side band if not WAVE
   10       IF (.NOT.WAVE) THEN
C
C Second side band - A0(-K)
               TMKX=TX-KX
               IF ((TMKX-OAX)**2+DTMKY2.GT.OARAD2) GOTO 20
               TMK2=TMKX*TMKX+TMKY2
               TMKA=TMKX*CA+TMKYSA
               V=TMK2-DMAB2
               DELX=V*TMKX-ACA*TMKA-DELTX
               DELY=V*TMKY-ASA*TMKA-DELTY
               V=TMK2-T2
               V=SS*(DELX*DELX+DELY*DELY)+DD*V*V+VIBKSQ
               IF (V.GT.7.0) GOTO 20
C
C
               A0MK=EXP(-V)
C
C T0(-K) * A0(-K)
               G=PI*((0.5*TMK2-DMAB2)*TMK2-ASTIG*TMKA*TMKA)-G0
               T2R=COS(G)*A0MK
               T2I=SIN(G)*A0MK
            ENDIF
C
C Select CTF mode
   20       GOTO (30,40,50,60),MODE
C
C Mode = WAVE
   30       CTFR=T1R
            CTFI=T1I
            GOTO 70
C
C Mode = AMPLITUDE
   40       CTFR=T1R+T2R
            CTFI=T1I+T2I
            GOTO 70
C
C Mode = PHASE
   50       CTFR=T2I-T1I
            CTFI=T1R-T2R
            GOTO 70
C
C Mode = AMPLITUDE + PHASE
   60       CTFAR=T1R+T2R
            CTFAI=T1I+T2I
            CTFPR=T2I-T1I
            CTFPI=T1R-T2R
            CTFR=CTFAR*CTFPR-CTFAI*CTFPI
            CTFI=CTFAR*CTFPI+CTFAI*CTFPR
C
C Include specimen drift if specified
   70       IF (DRIFT.NE.0.0) THEN
               KD=PI*(KX*DRIFTX+KDY)
               IF (KD.NE.0.0) THEN
                  P=SIN(KD)/KD
                  CTFR=P*CTFR
                  CTFI=P*CTFI
               ENDIF
            ENDIF
C
C
            IF (CC) CTFI=-CTFI
C
            IF (SQU) THEN
               CTFR=CTFR*CTFR+CTFI*CTFI
               CTFI=0.0
            ENDIF
C
C Apply CTF
            IF (ADD) THEN
               RB2(I)=RB1(I)+CTFR
               RB2(I+1)=RB1(I+1)+CTFI
            ELSE IF (MUL) THEN
               RB2(I)=CTFR*RB1(I)-CTFI*RB1(I+1)
               RB2(I+1)=CTFR*RB1(I+1)+CTFI*RB1(I)
            ELSE
               RB2(I)=CTFR
               RB2(I+1)=CTFI
            ENDIF
   80    CONTINUE
C
C Store result in LP2
         IF (SEMROW(2,RB2,NFMCOM,J,1,LP2)) GOTO 100
   90 CONTINUE
C
C If VERIFY option is set, print out results
      IF (OPT(NVERIF)) THEN
         WRITE (RECORD,110) STEP,SWI,EWI
         IF (SEMCON(RECORD)) GOTO 100
C
         IF (OARAD.NE.0.0) THEN
            WRITE (RECORD,120) OARAD,OADISP,OAPHI
            IF (SEMCON(RECORD)) GOTO 100
         ENDIF
C
         IF (DRIFT.NE.0.0.OR.VIB.NE.0.0) THEN
            WRITE (RECORD,130) DRIFT,DPHI,VIB
            IF (SEMCON(RECORD)) GOTO 100
         ENDIF
C
         IF (TILT.NE.0.0) THEN
            WRITE (RECORD,140) TILT,TPHI
            IF (SEMCON(RECORD)) GOTO 100
         ENDIF
      ENDIF
C
  100 RETURN
C
  110 FORMAT ('Step ',F6.3,' Divergence ',F6.3,' Focus spread ',F6.3)
  120 FORMAT ('Aperture radius ',F6.3,' Displacement ',F6.3,
     +        'Direction ',F7.3)
  130 FORMAT ('Drift ',F6.3,' Direction ',F7.3,' Vibration ',F6.3)
  140 FORMAT ('Beam tilt ',F6.3,' Direction ',F7.3)
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
