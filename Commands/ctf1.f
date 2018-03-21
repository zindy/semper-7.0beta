C Semper 6 subsidiary module CTF1
C
      SUBROUTINE CTF1
C
C Generates electron optical functions for axial conditions including
C coherence envelopes, objective aperture, specimen vib'n and uniaxial
C specimen drift.
C There are 4 types of transfer functions available - WAVE, AMPLITUDE,
C PHASE and AMPLITUDE + PHASE.  For each of these, the option SQUARED
C causes the modulus squared of the transfer function to be applied and
C option CC causes the complex conjugate to be applied.
C The output picture contains by default CTF istself.  However, the
C options ADD, MULTIPLY may be quoted to combine the CTF with a source
C picture FROM.  In the default case, the keys SIZE and SI2 determine
C the output picture size.  SI2 defaults to SIZE if unset.  The centre
C position is set to the left (half plane fourier) if option HALF is
C set.
C The key DEFOCUS and the variables STEP, APHI, SWIDTH, EWIDTH, DRIFT,
C DPHI, OARADIUS, VIBRATION and ASTIGMATISM define the characteristics
C of the CTF.  These are in reduced units unless option PHYSICAL is
C  specified, in which case the variables KV, CS are used to determine
C the units.
C
      REAL VAL
      INTEGER IVAL,IVALPN,SEMFRM
      LOGICAL OPT,VARSET,SEMOPN,SEMCEN,SEMTFC,SEMROW,CONOPT,SEMCON
C
      INCLUDE 'COMMON'
C
      REAL A,AB2,APHI,ASB4,ASTIG,CA,CS,CTFI,CTFR
      REAL DD,DEFOC,DKX,DKY,DPHI,DRIFT,DRIFTX,DRIFTY
      REAL EWI,GAMMA,GL,KX,KY,KSQ,KYSQ,KYSA,KDY,KA,KD,OARAD,OARAD2
      REAL P,SA,SCH,SS,STEP,ST2,SWI,V,VIB,VIBRR,X
      INTEGER CLASS,CCOL,CROW,FORM,I,IX,IY,J,MODE,NCOL,NLAY,NROW
      LOGICAL ADD,MUL,WAVE,AMP,PHASE,SQU,CC,HALF
C
C Packed names
C
      INTEGER NADD,NMULTI,NSQUAR,NCC,NWAVE,NAMPLI,NPHASE,NHALF
      INTEGER NFROM,NTO,NSIZE,NSI2,NPHYSI,NKV,NCS
      INTEGER NSTEP,NST2,NAPHI,NSWIDT,NEWIDT,NDRIFT,NDPHI,NOARAD
      INTEGER NVIBRA,NDEFOC,NASTIG,NVERIF
C
      PARAMETER (NADD=1764, NMULTI=21652, NSQUAR=31101, NCC=4920)
      PARAMETER (NWAVE=-4863, NAMPLI=2136, NPHASE=25921, NHALF=12852)
      PARAMETER (NFROM=10335, NTO=-601, NSIZE=30786, NSI2=30792)
      PARAMETER (NPHYSI=25945, NKV=18480, NCS=5560)
      PARAMETER (NSTEP=31205, NST2=31232)
      PARAMETER (NAPHI=2248, NSWIDT=31329, NEWIDT=8929)
      PARAMETER (NDRIFT=7129, NDPHI=7048, NOARAD=24058)
      PARAMETER (NVIBRA=-3563, NDEFOC=6606, NASTIG=2380)
      PARAMETER (NVERIF=-3419)
C
C See if option ADD or MULTIPLY is set
      ADD=OPT(NADD)
      MUL=OPT(NMULTI)
C Fault conflict
      IF (CONOPT(NADD,NMULTI)) GOTO 90
C
C See if option WAVE, AMPLITUDE or PHASE is set
      WAVE=OPT(NWAVE)
      AMP=OPT(NAMPLI)
      PHASE=OPT(NPHASE)
C Fault conflicts
      IF (CONOPT(NWAVE,NAMPLI)) GOTO 90
      IF (CONOPT(NWAVE,NPHASE)) GOTO 90
C
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
     +      GOTO 90
C
C Fault multi-layer source picture
         IF (NLAY.NE.1) THEN
            ERROR=62
            IDERR=VERB
            GOTO 90
         ENDIF
C
C Fault source picture class if not fourier or power spectrum
         IF (CLASS.NE.NCLFOU.AND.CLASS.NE.NCLSPE) THEN
            ERROR=6
            IDERR=IVALPN(NFROM)
            GOTO 90
         ENDIF
C
C Fault bad centre position for source picture
         IF (SEMTFC(LP1,HALF)) GOTO 90
C
C Otherwise, determine output picture size
      ELSE
         LP1=0
         FORM=NFMFP
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
         ERROR = 164
         GOTO 90
      ENDIF
C
C Establish output form; default Complex if either source or ctf is
C complex, and Fp otherwise (ctf is complex if WAVE)
      IF (WAVE.OR.FORM.EQ.NFMCOM) THEN
         FORM=NFMCOM
      ELSE
         FORM=NFMFP
      ENDIF
C Open output picture
      LP2=LP1
      IF (SEMOPN(2,IVALPN(NTO),NCOL,NROW,1,NCLFOU,SEMFRM(FORM),LP2))
     +   GOTO 90
C
C Update centre position if half plane fourier
      IF (HALF) THEN
         IF (SEMCEN(LP2,1,1+NROW/2,1)) GOTO 90
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
            GOTO 90
         ENDIF
C
C
         CS=VAL(NCS)*1E6
         IF (CS.EQ.0.0) THEN
            ERROR=3
            IDERR=NCS
            GOTO 90
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
         GOTO 90
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
C
C Determine further useful parameters
      IF (OARAD.NE.0.0) THEN
         OARAD2=OARAD*OARAD
      ELSE
         OARAD2=1E4
      ENDIF
      AB2=ASTIG/2.0
      ASB4=AB2*AB2
      CA=COS(APHI)
      SA=SIN(APHI)
      DRIFTX=DRIFT*COS(DPHI)
      DRIFTY=DRIFT*SIN(DPHI)
      VIBRR=(PI*VIB)**2
      SS=(PI*SWI)**2
      DD=(PI*EWI)**2/2.0
      IF (EWI.GT.0.0) THEN
         KSQ=SQRT(7.0/DD)
      ELSE
         KSQ=1E4
      ENDIF
      OARAD2=MIN(OARAD2,KSQ)
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
      DO 80 J=1,NROW
C
C Read source row from LP1 if required
         IF (ADD.OR.MUL) THEN
            IF (SEMROW(1,RB1,NFMCOM,J,1,LP1)) GOTO 90
         ENDIF
C
C Decrement Y coordinate
         IY=IY-1
         KY=REAL(IY)*DKY
C
         KYSQ=KY*KY
         KYSA=KY*SA
         KDY=KY*DRIFTY
C
C Initialise X coordinate
         IX=-CCOL
C
C Generate CTF along output picture row
         DO 70 I=1,2*NCOL,2
C
C Increment X coordinate
            IX=IX+1
            KX=REAL(IX)*DKX
C
            KSQ=KX*KX+KYSQ
            CTFR=0.0
            CTFI=0.0
            IF (KSQ.GT.OARAD2) GOTO 60
C
C
            KA=KX*CA+KYSA
            KA=2.0*KA*KA-KSQ
            V=KSQ-DEFOC
            P=SS*((V*V+ASB4)*KSQ-ASTIG*V*KA)+(DD*KSQ+VIBRR)*KSQ
            IF (P.GT.7.0) GOTO 60
C
C
            P=EXP(-P)
C
C
            IF (DRIFT.NE.0.0) THEN
               KD=PI*(KX*DRIFTX+KDY)
               IF (KD.NE.0.0) P=P*SIN(KD)/KD
            ENDIF
C
C
            GAMMA=PI*(KSQ*(0.5*KSQ-DEFOC)-AB2*KA)
C
C Select CTF mode
            GOTO (10,20,30,40),MODE
C
C Mode = WAVE
   10       IF (SQU) THEN
               CTFR=P*P
            ELSE
               CTFR=P*COS(GAMMA)
               CTFI=-P*SIN(GAMMA)
               IF (CC) CTFI=-CTFI
            ENDIF
            GOTO 60
C
C Mode = AMPLITUDE
   20       CTFR=2.0*P*COS(GAMMA)
            GOTO 50
C
C Mode = PHASE
   30       CTFR=2.0*P*SIN(GAMMA)
            GOTO 50
C
C Mode = AMPLITUDE + PHASE
   40       CTFR=2.0*P*P*SIN(2.0*GAMMA)
C
C
   50       IF (SQU) CTFR=CTFR*CTFR
C
C Apply CTF
   60       IF (ADD) THEN
               RB2(I)=RB1(I)+CTFR
               RB2(I+1)=RB1(I+1)+CTFI
            ELSE IF (MUL) THEN
               IF (WAVE) THEN
                  RB2(I)=CTFR*RB1(I)-CTFI*RB1(I+1)
                  RB2(I+1)=CTFR*RB1(I+1)+CTFI*RB1(I)
               ELSE
                  RB2(I)=CTFR*RB1(I)
                  RB2(I+1)=CTFR*RB1(I+1)
               ENDIF
            ELSE
               RB2(I)=CTFR
               RB2(I+1)=CTFI
            ENDIF
   70    CONTINUE
C
C Store result in LP2
         IF (SEMROW(2,RB2,NFMCOM,J,1,LP2)) GOTO 90
   80 CONTINUE
C
C If VERIFY option is set, print out results
      IF (OPT(NVERIF)) THEN
         WRITE (RECORD,100) STEP,SWI,EWI
         IF (SEMCON(RECORD)) GOTO 90
C
         IF (OARAD.NE.0.0) THEN
            WRITE (RECORD,110) OARAD
            IF (SEMCON(RECORD)) GOTO 90
         ENDIF
C
         IF (DRIFT.NE.0.0.OR.VIB.NE.0.0) THEN
            WRITE (RECORD,120) DRIFT,DPHI,VIB
            IF (SEMCON(RECORD)) GOTO 90
         ENDIF
      ENDIF
C
   90 RETURN
C
  100 FORMAT ('Step ',F6.3,' Divergence ',F6.3,' Focus spread ',F6.3)
  110 FORMAT ('Aperture radius ',F6.3)
  120 FORMAT ('Drift ',F6.3,' Direction ',F7.3,' Vibration ',F6.3)
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
