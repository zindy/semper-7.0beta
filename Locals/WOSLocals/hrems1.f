C HREM local modules for SG Semper
C --------------------------------
C
C Semper 6 local processing module PHDBLE
C
      SUBROUTINE PHDBLE
C
C 'Phase doubles' LP1 to LP2
C
C Verb descriptor
C   PhDouble :PHDBLE >$ft
C
C Global declarations
      INCLUDE 'COMMON'
C
C Local declarations
      LOGICAL SEMROW
      REAL X,Y,T
      INTEGER I,J,L
C Loop over picture layers, rows
      DO 30 L=1,NLAYS(LP1)
      DO 20 J=1,NROWS(LP1)
C
C Fetch row
         IF (SEMROW(1,RB1,NFMCOM,J,L,LP1)) RETURN
C
C Loop over pixels, doubling phase at all points
         DO 10 I=1,NCOLS(LP1)
            X=RB1(2*I-1)
            Y=RB1(2*I)
            IF (X.NE.0.OR.Y.NE.0) THEN
               T=1./SQRT(X*X+Y*Y)
               RB1(2*I-1)=(X*X-Y*Y)*T
               RB1(2*I)=2*X*Y*T
            ENDIF
C End of loop over pixels
 10      CONTINUE
C
C Output row
         IF (SEMROW(2,RB1,NFMCOM,J,L,LP2)) RETURN
C
C End loops over rows,layers
 20   CONTINUE
 30   CONTINUE
C
      RETURN
C
      END
C
C Semper 6 processing module CTF
C
C Further recent changes by WOS
C - New var IPS=1,2 for old,new Imaging Param Set; default 1 being old
C   as documented by Synoptics; 2 being complex coeffs as follows
C   with PHYSICAL assumed (and ru = reduced units); beam tilt, and all
C   aberration coeffs taken wrt CF axis
C     A1,A12    2-fold astig     /[nm|ru]; overfocus +
C     C1        defocus          /[nm|ru]; overfocus +
C     A2,A22    3-fold astig     /[nm|ru]
C     BTILT,BT2 beam tilt        /[mrad|ru]
C     BDIV      rms beam div Xs  /[mrad|ru] NB: now in X-sect
C     FSPR      rms focus spread /[nm|ru]  exactly equiv to EWI
C     OAC,OA2   obj ap centre    /mrad
C     DRI,DR2   lin.drift cmpts  /nm|ru
C   Retained: KV,CS,OAR,VIB
C
C Reduced units may be removed from code later, though still
C supported at user level
C
C - Three-fold astigmatism added to phase shift (but not envelope)
C - for IPS=2 adds term (2pi.l^2/3)Re{A2*w*^2} to gamma, with
C   A2=A2+iA22 and w=x+iy
C - for IPS=1, adds same (not entirely consistently with old usage),
C   using keys TFAST=|A2|, and TFPHi=arg(A2)/3; highest OVERfocus in
C   dirn of TFPHi either way
C
C Substantial reworking making CTF1 redundant, and CTF2 given
C entirely different function
C - some speeding up by restoring exploitation of symmetry removed
C   in original recoding for Semper 6
C - fixing default form bug (making CTF SQU produce Fp even when
C   TILT non-zero)
C - shortened code, combining zero tilt and non-zero tilt routines
C   and calling (new) CTF2 to pick up parameters
C - general (non-rectangular) sampling vectors U,V supported
C Compatible with previous code except in this last respect.
C
      SUBROUTINE CTF
C
C Generates and applies electron microscope contrast transfer functions
C (CTFs), accounting for Cs, defocus, astigmatism, beam tilt, displaced
C objective apertures, coherence envelopes, specimen vibration and
C uniaxial specimen drift.
C
C Four types of transfer functions are available - WAVE, AMPLITUDE,
C PHASE and AMPLITUDE x PHASE.  For each of these, the option SQUARED
C causes the modulus squared of the transfer function to be applied and
C option CC causes the complex conjugate to be applied.
C
C In default, the output contains the CTF itself.  However, the options
C ADD, MULTIPLY may be quoted to combine the CTF with a source picture
C FROM.  In the default case, the key SIZE(2) determines the output
C picture size, and the origin is placed at the left (half plane fourier)
C if option HALF is set.  The output form is Complex if either source or
C CTF are complex, and otherwise Fp.
C
C The keys defining the imaging parameters are defined by CFT2 not CTF,
C so as to be common to IITGEN.
C
C Note:  Drift is rect. width DR
C        Vibration is exp(-.5r^2/v^2) in section
C        Beam divergence is exp(-.5k^2/s^2) in section
C        Focus spread is exp(-.5D^2/d^2)
C
C Global declarations
      INCLUDE 'COMMON'
C
C Local declarations
      REAL VAL
C DEBUG
c      LOGICAL SEMCON
C
      LOGICAL SEMROW,CTF2
      LOGICAL OPT,VARSET,CONOPT,SEMOPN,SEMCEN,SEMTFC
      INTEGER IVALPN,IVAL,SEMFRM
C
      REAL DMAB2,AST,A1,A2,AA1,AA2,T1,T2,TSQ
      REAL CTFR,CTFI,CTFPR,CTFPI,CTFAR,CTFAI,T1R,T1I,T2R,T2I
      REAL SS,DD,DT1,DT2,D1,D2,DR1,DR2,GT,G
      REAL K1,K2,KSQ,KA,KD,TPK1,TPK2,TPKSQ,TPKA,TMK1,TMK2,TMKSQ,TMKA
      REAL U1,U2,V1,V2,UA,V,VV2,OA1,OA2,OARSQ,VV2KSQ,TFAR,TFAI
      LOGICAL ADD,MUL,WAVE,AMP,PHASE,SQU,CC,HALF,GENERL
      INTEGER I,I1,J,J2,N,NCOL,NROW,CCOL,NLAY,CLASS,FORM,CROW,MODE
C
C Packed names
      INTEGER NADD,NMULTI,NSQUAR,NCC,NWAVE,NAMPLI,NPHASE,NHALF
      INTEGER NFROM,NTO,NSIZE,NSI2
C
      PARAMETER (NADD=1764, NMULTI=21652, NSQUAR=31101, NCC=4920)
      PARAMETER (NWAVE=-4863, NAMPLI=2136, NPHASE=25921, NHALF=12852)
      PARAMETER (NFROM=10335, NTO=-601, NSIZE=30786, NSI2=30792)
C
C Note major mode parameters and open output
C ------------------------------------------
C See if option ADD or MULTIPLY is set
      ADD=OPT(NADD)
      MUL=OPT(NMULTI)
C Fault conflict
      IF (CONOPT(NADD,NMULTI))  GOTO 90
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
C Fault source picture class if not Fourier or Spectrum
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
C Ask CTF2 for imaging parameters
C NB: we need these before deciding whether ctf is real or complex
      N=NCOL
      IF (HALF) N=(NCOL-1)*2
      IF (CTF2(DMAB2,AST,A1,A2,AA1,AA2,SS,DD,T1,T2,OARSQ,OA1,OA2,
     +   VV2,DR1,DR2,N,NROW,U1,U2,V1,V2,UA,TFAR,TFAI)) GOTO 90
C DEBUG
c      WRITE (RECORD,1) DMAB2,AST,A1,A2,AA1,AA2
c 1    FORMAT ('DMAB2,AST,A1,A2,AA1,AA2: ',9F9.4)
c      IF (SEMCON(RECORD)) RETURN
c      WRITE (RECORD,2) SS,DD,T1,T2
c 2    FORMAT ('SS,DD,T1,T2: ',9F9.4)
c      IF (SEMCON(RECORD)) RETURN
c      WRITE (RECORD,3) OARSQ,OA1,OA2,VV2,DR1,DR2
c 3    FORMAT ('OARSQ,OA1,OA2,VV2,DR1,DR2: ',G12.4,9F9.4)
c      IF (SEMCON(RECORD)) RETURN
c      WRITE (RECORD,4) U1,U2,V1,V2,UA
c 4    FORMAT ('U1,U2,V1,V2,U: :',9F9.4)
c      IF (SEMCON(RECORD)) RETURN
c      WRITE (RECORD,5) TFAR,TFAI
c 5    FORMAT ('TFAR,TFAI: ',9F9.4)
c      IF (SEMCON(RECORD)) RETURN
C
C Simple or general case?
      GENERL=T1.NE.0..OR.T2.NE.0. .OR. OA1.NE.0..OR.OA2.NE.0.
     +   .OR. TFAR.NE.0..OR.TFAI.NE.0.
C
C If focus spread envelope vanishes within objective aperture,
C reduce effective aperture radius for efficiency
      IF (.NOT.GENERL.AND.DD.GT.0.) OARSQ=MIN(OARSQ,SQRT(7./DD))
C
C Fault primary beam outside aperture
      IF (.NOT.WAVE.AND.(T1-OA1)**2+(T2-OA2)**2.GT.OARSQ) THEN
         ERROR = 77
         IDMESS = 'Primary beam outside aperture'
         GOTO 90
      ENDIF
C
C Finally, establish output form; default Complex if either source or 
C ctf is complex, and Fp otherwise; ctf is complex if WAVE unless SQU
      IF ((WAVE.OR.GENERL).AND..NOT.SQU.OR.FORM.EQ.NFMCOM) THEN
         FORM=NFMCOM
      ELSE
         FORM=NFMFP
      ENDIF
C
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
C Fetch output picture parameters
      CCOL=CCOLN(LP2)
      CROW=CROWN(LP2)
C
C Calculate wave aberration function g for primary beam:
C   g = pi { (.5k^2 - [D-.5A])k^2 - A(k.a)^2 }
      TSQ=T1**2+T2**2
      KA=T1*A1+T2*A2
      GT = PI * ( (0.5*TSQ-DMAB2)*TSQ - AST*KA*KA
     +          +.6666667*(TFAR*T1*(T1**2-3*T2**2)
     +                    +TFAI*T2*(3*T1**2-T2**2)) )
C
C Calculate aberration function d slope for primary beam:
C   d = { (k^2 - [D-.5A])k - (k.a)Aa }
      DT1 = (TSQ-DMAB2)*T1 - KA*AA1
      DT2 = (TSQ-DMAB2)*T2 - KA*AA2
C
C Generate CTF over entire output picture
      DO 80 J=1,NROW
C
C Read source row from LP1 if required
         IF (ADD.OR.MUL) THEN
            IF (SEMROW(1,RB1,NFMCOM,J,1,LP1)) GOTO 90
         ENDIF
C
C Initialise inner loop variables for incrementing inside loop:
C K vector
         K1=-REAL(CCOL)*U1+REAL(CROW-J)*V1
         K2=-REAL(CCOL)*U2+REAL(CROW-J)*V2
C KA - dot product K.a
         KA=K1*A1+K2*A2
C TPKA,TMKA - dot products (T+K).a, (t-K).a
         TPKA=(T1+K1)*A1+(T2+K2)*A2
         TMKA=(T1-K1)*A1+(T2-K2)*A2
C
C Generate CTF along output picture row
         DO 70 I=1,2*NCOL,2
C
C Increment K
            K1=K1+U1
            K2=K2+U2
C
C Switch code: simple version for untilted illumination and aperture
C central; general version otherwise
            IF (GENERL) GOTO 200
C
C Code for simple case
C --------------------
C Increment K.a
            KA=KA+UA
C
C Initialise CTF to zero
            CTFR=0.
            CTFI=0.
C
C Test whether beam outside aperture
            KSQ=K1*K1+K2*K2
            IF (KSQ.GT.OARSQ) GOTO 60
C
C Calculate envelope factor exp(-v), with vibration included:
C   v = pi^2 { s^2 |d|^2 + .5 d^2 k^4 + 2 vib^2 k^2 }
C in which vector gradient d = { (k^2 - [D-.5A])k - (k.a)Aa }
            D1 = (KSQ-DMAB2)*K1 - KA*AA1
            D2 = (KSQ-DMAB2)*K2 - KA*AA2
            V = SS*(D1**2 + D2**2) + (DD*KSQ + VV2)*KSQ
C Test whether contribution wiped out
            IF (V.LE.7.) THEN
               V=EXP(-V)
            ELSE
               GOTO 60
            ENDIF
C
C Include drift factor sin(pi.k.dr)/(pi.k.dr)
            KD=K1*DR1+K2*DR2
            IF (KD.NE.0.) V=V*SIN(KD)/KD
C
C Calculate exp{-ig}.exp(-v), with
C   g = pi { (.5k^2 - [D-.5A])k^2 - A(k.a)^2 }
            G = PI * ( (0.5*KSQ-DMAB2)*KSQ - AST*KA*KA)
C
C Select CTF mode
            GOTO (10,20,30,40),MODE
C
C Mode = WAVE
   10       IF (SQU) THEN
               CTFR=V*V
            ELSE
               CTFR=COS(G)*V
               CTFI=-SIN(G)*V
               IF (CC) CTFI=-CTFI
            ENDIF
            GOTO 60
C Mode = AMPLITUDE
   20       CTFR=2.*COS(G)*V
            GOTO 50
C Mode = PHASE
   30       CTFR=2.*SIN(G)*V
            GOTO 50
C Mode = AMPLITUDE + PHASE
   40       CTFR=2.*SIN(2.*G)*V*V
C
   50       IF (SQU) CTFR=CTFR*CTFR
            GOTO 60
C
C Code for general case
C ---------------------
C Increment dot products of T+K, T-K with a
  200       TPKA=TPKA+UA
            TMKA=TMKA-UA
C
C Initialise CTF to zero etc
            VV2KSQ=VV2*(K1*K1+K2*K2)
            T1R=0.
            T1I=0.
            T2R=0.
            T2I=0.
            CTFR=0.
            CTFI=0.
C
C First side band: f(k).exp(-g(t+k)+g(t).exp(-v(t+k,t))
            TPK1=T1+K1
            TPK2=T2+K2
C Outside aperture?
            IF ((TPK1-OA1)**2+(TPK2-OA2)**2.GT.OARSQ) GOTO 210
            TPKSQ=TPK1*TPK1+TPK2*TPK2
C Calculate mutual envelope factor exp(-v), with vibration included:
C   v = pi { s^2 |d-dt|^2 + .5 f^2 (tpk^2-t^2)^2 + 2 vib^2 k^2 }
C in which vector gradient d = { (k^2 - [D-.5A])k - (k.a)Aa }
C and d,dt refer to gradients at T+K and T
            D1 = (TPKSQ-DMAB2)*TPK1 - TPKA*AA1
            D2 = (TPKSQ-DMAB2)*TPK2 - TPKA*AA2
            V = SS*((D1-DT1)**2+(D2-DT2)**2) + DD*(TPKSQ-TSQ)**2
     +          + VV2KSQ
C Test whether contribution wiped out
            IF (V.LE.7.) THEN
               V=EXP(-V)
C Calculate wave aberration function g for T+K, less that on primary:
C   g = pi { (.5k^2 - [D-.5A])k^2 - A(k.a)^2 }
               G = PI * ( (0.5*TPKSQ-DMAB2)*TPKSQ - AST*TPKA*TPKA
     +                     +.6666667*(TFAR*TPK1*(TPK1**2-3*TPK2**2)
     +                               +TFAI*TPK2*(3*TPK1**2-TPK2**2)) )
     +             - GT
               T1R=COS(G)*V
               T1I=-SIN(G)*V
            ENDIF
C
C No second side if WAVE
  210       IF (WAVE) GOTO 220
C
C Second side band: f(k).exp(g(t-k)-g(t).exp(-v(t-k,t))
            TMK1=T1-K1
            TMK2=T2-K2
            IF ((TMK1-OA1)**2+(TMK2-OA2)**2.GT.OARSQ) GOTO 220
C Calculate envelope as before
            TMKSQ=TMK1*TMK1+TMK2*TMK2
            D1 = (TMKSQ-DMAB2)*TMK1 - TMKA*AA1
            D2 = (TMKSQ-DMAB2)*TMK2 - TMKA*AA2
            V = SS*((D1-DT1)**2+(D2-DT2)**2) + DD*(TMKSQ-TSQ)**2
     +          + VV2KSQ
            IF (V.LE.7.) THEN
               V=EXP(-V)
C Calculate wave aberration function as before
               G = PI * ( (0.5*TMKSQ-DMAB2)*TMKSQ - AST*TMKA*TMKA
     +                     +.6666667*(TFAR*TMK1*(TMK1**2-3*TMK2**2)
     +                               +TFAI*TMK2*(3*TMK1**2-TMK2**2)) )
     +             - GT
               T2R=COS(G)*V
               T2I=SIN(G)*V
            ENDIF
C
C Select CTF mode
  220       GOTO (230,230,250,260),MODE
C
C Mode = AMPLITUDE, or WAVE since T2R=T2I=0 if WAVE
  230       CTFR=T1R+T2R
            CTFI=T1I+T2I
            GOTO 270
C Mode = PHASE
  250       CTFR=T2I-T1I
            CTFI=T1R-T2R
            GOTO 270
C Mode = AMPLITUDE + PHASE
  260       CTFAR=T1R+T2R
            CTFAI=T1I+T2I
            CTFPR=T2I-T1I
            CTFPI=T1R-T2R
            CTFR=CTFAR*CTFPR-CTFAI*CTFPI
            CTFI=CTFAR*CTFPI+CTFAI*CTFPR
C
C Include specimen drift if specified
  270       KD=K1*DR1+K2*DR2
            IF (KD.NE.0.) THEN
               V=SIN(KD)/KD
               CTFR=CTFR*V
               CTFI=CTFI*V
            ENDIF
C
            IF (CC) CTFI=-CTFI
            IF (SQU) THEN
               CTFR=CTFR*CTFR+CTFI*CTFI
               CTFI=0.
            ENDIF
C
C Separated code merges here
C --------------------------
C Apply CTF
   60       IF (ADD) THEN
               RB2(I)=RB1(I)+CTFR
               RB2(I+1)=RB1(I+1)+CTFI
            ELSE IF (MUL) THEN
               IF (CTFI.NE.0.) THEN
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
C Output result
C -------------
C If conjugate-symmetric, without source, reflect to RB3 as well
C [The symmetry could be exploited when sources present as well, but
C the gain is probably insufficient to justify the length of code]
      IF (.NOT.(ADD.OR.MUL).AND..NOT.WAVE.AND..NOT.HALF) THEN
         J2=2*CROW-J
         IF (J2.LE.NROW) THEN
            IF (NCOL/2*2.EQ.NCOL) THEN
               I1=2
               RB3(1)=RB2(1)
               RB3(2)=-RB2(2)
            ELSE
               I1=1
            ENDIF
            N=2*NCOL
            DO 75 I=I1,NCOL
               RB3(N-1)=RB2(2*I-1)
               RB3(N)=-RB2(2*I)
               N=N-2
   75       CONTINUE
C Output reflected row
            IF (SEMROW(2,RB3,NFMCOM,J2,1,LP2)) GOTO 90
C Quit if central row
            IF (J2.EQ.J) GOTO 90
         ENDIF
      ENDIF
C
C Output original row
         IF (SEMROW(2,RB2,NFMCOM,J,1,LP2)) GOTO 90
   80 CONTINUE
C
   90 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module CTF2
C
      LOGICAL FUNCTION CTF2
     +   (DMAB2,AST,A1,A2,AA1,AA2,SS,DD,T1,T2,OARSQ,OA1,OA2,
     +   VV2,DR1,DR2,NCOL,NROW,U1,U2,V1,V2,UA,TFAR,TFAI)
C
C Gathers reduced electron imaging parameters for CTF and IITGEN;
C option UV enables pickup of sampling vectors U(2),V(2) as well as
C standard STEP(2)
C
C Global declarations (for ERROR,IDERR only)
      INCLUDE 'COMMON'
C
C Local declarations
C DEBUG
c      logical semcon
      REAL U,V,VAL,CS,X,SCH,GL,A,ALPH,U1,U2,V1,V2,AST,DMAB2,A1,A2
      REAL AA1,AA2,UA,SS,DD,T1,T2,OARSQ,OA1,OA2,VV2,DR1,DR2,TFAR,TFAI
      LOGICAL OPT,CIPS
      INTEGER NCOL,NROW
C
C Packed names
      INTEGER NPHYSI,NIPS,NKV
      INTEGER NDEFOC,NASTIG,NAPHI,NTFA,NTFP,NTILT,NTPHI,NCS
      INTEGER NSWIDT,NEWIDT,NOADIS,NOAPHI,NDRIFT,NDPHI
      PARAMETER (NPHYSI=25945,NIPS=15059,NKV=18480)
      PARAMETER (NDEFOC=6606,NASTIG=2380,NAPHI=2248)
      PARAMETER (NTFA=-242,NTFP=-257,NTILT=-373,NTPHI=-649,NCS=5560)
      PARAMETER (NSWIDT=31329,NEWIDT=8929,NOADIS=24044,NOAPHI=24056)
      PARAMETER (NDRIFT=7129,NDPHI=7048)
C
      INTEGER NA1,NA12,NC1,NA2,NA22,NBTILT,NBT2
      INTEGER NBDIVE,NFSPRE,NOAC,NOA2,NDR,NDR2
      PARAMETER (NA1=2840,NA12=2872,NC1=6040,NA2=2880,NA22=2912)
      PARAMETER (NBTILT=4009,NBT2=4032)
      PARAMETER (NBDIVE=3369,NFSPRE=10376)
      PARAMETER (NDR=7120,NDR2=7152,NOAC=24043,NOA2=24072)
C
      INTEGER NSTEP,NST2,NUV,NU,NU2,NV,NV2
      INTEGER NVIBRA,NOARAD
      PARAMETER (NSTEP=31205,NST2=31232)
      PARAMETER (NUV=-2481,NU=-1601,NU2=-2881,NV=-3201,NV2=-4481)
      PARAMETER (NVIBRA=-3563,NOARAD=24058)
C
      CTF2=.TRUE.
C
C Establish imaging parameter set
      CIPS=VAL(NIPS).EQ.2
C
C Establish Sch and Gl units
C ****** bug fix?
C      IF (OPT(NPHYSI).OR.CIPS) THEN
      IF (OPT(NPHYSI)) THEN
         V=VAL(NKV)*1E3
         IF (V.LE.0) THEN
            ERROR=3
            IDERR=NKV
            RETURN
         ENDIF
         CS=VAL(NCS)*1E6
         IF (CS.LE.0) THEN
            ERROR=3
            IDERR=NCS
            RETURN
         ENDIF
C Wavelength/nm
         X=V*1.60206E-4/9.1083/2.99763**2
         X=6.62517/SQRT(2.0*1.60206*9.1083*V*(1.0+X/2.0))
C Scherzer and Glaser units/nm and Glaser/Scherzer/mrad
         SCH=SQRT(CS*X)
         GL=SQRT(SQRT(CS*X*X*X))
         ALPH=GL/SCH*1E3
      ELSE
         SCH=1.
         GL=1.
         ALPH=1.
      ENDIF
C DEBUG
c      write (record,1) v,cs,sch,gl,alph
c 1    format ('CTF2: v,cs,sch,gl,alph: ',5g11.3)
c      if (semcon(record)) return
C
C Establish real space sampling vectors
      IF (OPT(NUV)) THEN
         U1=VAL(NU)/GL
         U2=VAL(NU2)/GL
         V1=VAL(NV)/GL
         V2=VAL(NV2)/GL
      ELSE
         U1=VAL(NSTEP)/GL
         IF (U1.LE.0.0) THEN
            ERROR=3
            IDERR=NSTEP
            RETURN
         ENDIF
         U2=0.
         V1=0.
         V2=VAL(NST2)/GL
         IF (V2.LE.0.0) V2=U1
      ENDIF
C
C Insist U non-zero
      U=U1*U1+U2*U2
      IF (U.EQ.0.) THEN
         ERROR=3
         IDERR=NU
      ENDIF
C
C Transform to reciprocal space sampling vectors
      IF (NROW.NE.1) THEN
C 2-D:
         X=(U1*V2-U2*V1)
         IF (X.EQ.0.) THEN
C U,V parallel
            ERROR=79
            RETURN
         ELSE
            A=V2
            V2=U1/X/REAL(NROW)
            U1=A/X/REAL(NCOL)
            A=V1
            V1=-U2/X/REAL(NROW)
            U2=-A/X/REAL(NCOL)
         ENDIF
C 1-D:
      ELSE
         U1=U1/U/REAL(NCOL)
         U2=U2/U/REAL(NCOL)
         V1=0.
         V2=0.
      ENDIF
C
C Establish defocus and astigmatism
      IF (CIPS) THEN
         V=-VAL(NC1)/SCH
         AA1=-2*VAL(NA1)/SCH
         AA2=-2*VAL(NA12)/SCH
         AST=SQRT(AA1**2+AA2**2)
         X=0.
         IF (AA1.NE.0..OR.AA2.NE.0.) X=.5*ATAN2(AA2,AA1)
      ELSE
         V=VAL(NDEFOC)/SCH
         AST=VAL(NASTIG)/SCH
         X=VAL(NAPHI)
      ENDIF
      DMAB2=V-.5*AST
      A1=COS(X)
      A2=SIN(X)
      AA1=AST*A1
      AA2=AST*A2
      UA=U1*A1+U2*A2
C
C Establish three-fold astigmatism
C - reduced unit is CS^.75 lam^.25 = sch^2/gl
      IF (CIPS) THEN
         TFAR=VAL(NA2)*GL/SCH/SCH
         TFAI=VAL(NA22)*GL/SCH/SCH
      ELSE
         X=VAL(NTFA)*GL/SCH/SCH
         V=VAL(NTFP)
         TFAR=X*COS(3*V)
         TFAI=X*SIN(3*V)
      ENDIF
C
C Establish beam tilt
      IF (CIPS) THEN
         T1=VAL(NBTILT)/ALPH
         T2=VAL(NBT2)/ALPH
      ELSE
         V=VAL(NTILT)/ALPH
         X=VAL(NTPHI)
         T1=V*COS(X)
         T2=V*SIN(X)
      ENDIF
C
C Establish source width (divergence) and energy width (focus spread)
      IF (CIPS) THEN
         SS=VAL(NBDIVE)/ALPH*SQRT(2.)
         DD=VAL(NFSPRE)/SCH
      ELSE
         SS=VAL(NSWIDT)/ALPH
         DD=VAL(NEWIDT)/SCH
      ENDIF
      SS=(PI*SS)**2
      DD=(PI*DD)**2/2
C
C Establish objective aperture
      OARSQ=(VAL(NOARAD)/ALPH)**2
      IF (OARSQ.LE.0.) OARSQ=1E6
      IF (CIPS) THEN
         OA1=VAL(NOAC)/ALPH
         OA2=VAL(NOA2)/ALPH
      ELSE
         OA2=VAL(NOADIS)/ALPH
         X=VAL(NOAPHI)
         OA1=OA2*COS(X)
         OA2=OA2*SIN(X)
      ENDIF
C
C Establish vibration and drift
      V=VAL(NVIBRA)/GL
      VV2=2.*(PI*V)**2
      IF (CIPS) THEN
         DR1=PI*VAL(NDR)/GL
         DR2=PI*VAL(NDR2)/GL
      ELSE
         V=PI*VAL(NDRIFT)/GL
         X=VAL(NDPHI)
         DR1=V*COS(X)
         DR2=V*SIN(X)
      ENDIF
C
C Normal return
      CTF2=.FALSE.
      RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 local processing module DPSMG
C
      SUBROUTINE DPSMG
C
C Fast Diffn Pattn Split/Merge: merges source DP LP1 with calculated
C   CTF SQU, splitting either side of line through origin at ANGLE
C
C - Many restrictions apply rel to CTF:
C   - square pics
C   - axial CTFs only: AST,APHI,DEF, or C1,A1[2] if IPS=2,
C     with KV,CS,STEP; PHYSICAL only
C   - simplified envelope 1/(1+w.k^4), falling to 0.1 at pic border
C
C Verb descriptor
C   DPSM :DPSMG angle= $2=dis to=$2 >Select
C
C Global declarations
      INCLUDE 'COMMON'
C
C Local declarations
      REAL VAL
C     Change: LDM, July 2005
C     Ensure that gfortran does not think of range, unpack as intrinsics
      EXTERNAL RANGE, UNPACK
      LOGICAL SEMOPN,SEMROW,SEMTFC,RANGE
      INTEGER IVALPN
      REAL CS,V,LAM,DK,CSP,DPXX,DPYY,DPXY
      REAL X,Y,SNX,SNY,X1,X2,XI
      REAL C1,A1,A12,G,W,KX,KY,KYKY,KK
      INTEGER NCOL,NROW,CCOL,I1,I2,I,J
      LOGICAL HALF
C
C Packed names
      INTEGER NFROM,NTO,NANGLE,NSTEP,NCS,NKV
      INTEGER NDEFOC,NASTIG,NAPHI,NC1,NA1,NA12,NIPS
      PARAMETER (NFROM=10335,NTO=-601,NANGLE=2167)
      PARAMETER (NSTEP=31205,NCS=5560,NKV=18480)
      PARAMETER (NDEFOC=6606,NASTIG=2380,NAPHI=2248)
      PARAMETER (NC1=6040,NA1=2840,NA12=2872,NIPS=15059)
C
C Open source picture
      IDERR=IVALPN(NFROM)
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      CCOL=CCOLN(LP1)
C Check class Fourier/Spectrum
      IF (CLASSN(LP1).NE.NCLFOU.AND.CLASSN(LP1).NE.NCLSPE) THEN
         ERROR=6
         RETURN
      ENDIF
C Fault half-plane xform
      IF (SEMTFC(LP1,HALF)) RETURN
C Fault non-square or multi-layer pic, or half-plane
      IF (NCOL.NE.NROW.OR.NLAYS(LP1).NE.1.OR.HALF) THEN
         ERROR=5
         RETURN
      ENDIF
C
C Find source picture range VMIN,VMAX
      SMGL1=.FALSE.
      IF (RANGE(1,LP1)) RETURN
      IF (VMIN.EQ.VMAX) THEN
         IDERR=IVALPN(NTO)
         ERROR=12
         RETURN
      ENDIF
C Note scale factor for theoretical DP
      DPSC=VMAX-VMIN
C
C Open output here, controlling display scaling
      LP2=LP1
      IF (SEMOPN(2,IVALPN(NTO),NCOL,NROW,1,NCLSPE,NFMFP,LP2))
     +   RETURN
C
C Initialise imaging parameters
      IF (VAL(NIPS).EQ.2) THEN
         C1=VAL(NC1)
         A1=VAL(NA1)
         A12=VAL(NA12)
      ELSE
         C1=-VAL(NDEFOC)
         V=.5*VAL(NASTIG)
         X=2.*VAL(NAPHI)
         A1=V*COS(X)
         A12=V*SIN(X)
      ENDIF
      CS=VAL(NCS)*1E6
      IF (CS.LE.0.) THEN
         IDERR=NCS
         ERROR=3
         RETURN
      ENDIF
      V=VAL(NKV)*1E3
      IF (V.LE.0.) THEN
         IDERR=NKV
         ERROR=3
         RETURN
      ENDIF
C Wavelength/nm
      X=V*1.60206E-4/9.1083/2.99763**2
      LAM=6.62517/SQRT(2*1.60206*9.1083*V*(1+.5*X))
      DK=1/VAL(NSTEP)/NCOL
C
C prepare coeffs for gamma expr
      Csp=.5*pi*Cs*lam*lam*lam
      Dpxx=pi*lam*(c1+a1)
      Dpyy=pi*lam*(c1-a1)
      Dpxy=pi*2*lam*a12
C
C establish envelope width parameter giving 10% transfer at kmax
      w=4*16*val(nstep)**4
C
C Find split line normal (SNX,SNY) - 90deg anticl from ANGLE
      X=VAL(NANGLE)
      SNX=-SIN(X)
      SNY=COS(X)
C
C Begin loop over picture rows
      X1=1-CCOL
      X2=NCOL-CCOL
      DO 80 J=1,NROW
C
C Find pixel range overwritten by CTF
C Trap horizontal split line, which does not intersect row
         Y=CROWN(LP1)-J
         IF (SNX.EQ.0.) THEN
            IF (SNY*Y.GE.0.) GOTO 3
            GOTO 4
         ENDIF
C Find where split line intersects row
         XI=-SNY*Y/SNX
         IF (SNX.LT.0.) THEN
C DP left of intersection
            IF (XI.LT.X1) GOTO 4
            IF (XI.GT.X2) GOTO 3
            I1=CCOL+XI+1
            I2=NCOL
C CTF left of intersection
         ELSE
            IF (XI.LT.X1) GOTO 3
            IF (XI.GT.X2) GOTO 4
            I1=1
            I2=CCOL+XI
         ENDIF
         GOTO 5
C
C Nothing overwritten: just copy source row
3        IF (SEMROW(1,RB1,NFMFP,J,1,LP1)) RETURN
         GOTO 11
C
C Everything overwritten: skip source row input
4        I1=1
         I2=NCOL
         GOTO 6
C
C Part-row I1:I2 overwritten
C Fetch source row
5        IF (SEMROW(1,RB1,NFMFP,J,1,LP1)) RETURN
C
C Generate CTF along output picture row
C
C Loop over pixels, generating CTF SQU scaled to VMIN,VMAX
 6       ky=(crown(lp1)-j)*dk
         kyky=ky*ky
         kx=(i1-ccol)*dk
         do 70 i=i1,i2
C generate CTF: [sin^2(g)/(1+k^2/kc^2)]^2
             kk=kx*kx+kyky
             g = Csp*kk**2
     +           + Dpxx*kx*kx + Dpyy*kyky + Dpxy*kx*ky
             rb1(i) = vmin + ( sin(g)/(1+w*kk*kk) )**2 * dpsc
             kx=kx+dk
70       continue
C
C Output resulting row
11       IF (SEMROW(2,RB1,NFMFP,J,1,LP2)) RETURN
   80 CONTINUE
C End loop over rows
C
C Normal return 
      RETURN
C
      END
C Semper 6 local module D3FIT
C
      SUBROUTINE D3FIT
C
C Tabulates XCC between model CTF^2 and supplied DP, as function of
C defocus values in (internally located) mirror axis dirns; uses
C free-standing submodules operating on memory only for speed
C
C NB: Contains relatively large local array declarations
C
C Verb descriptor:
C  DPfit :D3FIT krange=.5 kr2=4 within=300 of= of2= of3= +
C    under over >Select $2=0 to=$2 size=21 si2= verify mark=
C
C Global declarations
      INCLUDE 'COMMON'
C
C Local declarations
      LOGICAL D2FIT2,D3FMKM
      LOGICAL SEMOPN,SEMLAY,SEMLU,VARSET,SEMTFC,SEMCON,OPT,CONOPT
      LOGICAL MARSET,FSINIT,FSCIRC,ANNOT
      LOGICAL HALF
      REAL VAL
      REAL R1,R2,AMIRR,KMIN,KMAX,D1B,D1T,D2B,D2T,DD,DK,CS,V,LAM,S
      REAL C1,A1,A12,A,APHI,AM,CAM,COS45,EFL,X
      PARAMETER (COS45=.707107)
      INTEGER IVAL,IVALPN
      INTEGER NCOL,NROW,NLAY,CLASS,NCOLO,NROWO,MARK
      INTEGER*4 N4
C Max # defocus samples
      INTEGER NXM
      PARAMETER (NXM=101)
      REAL XC(NXM,NXM)
C Buffer DP is used as 2-D array, holding components of specimen wave
      INTEGER*4 MAXF
      PARAMETER (MAXF=80000)
      REAL DP(MAXF)
C Workspace for mirror axis location
      INTEGER NW,NWP2
      PARAMETER (NW=1024,NWP2=NW+2)
      REAL SEC(NWP2),OCF(NW)
C
C Packed names
      INTEGER NFROM,NTO,NSIZE,NSI2,NKRANG,NKR2
      INTEGER NOF,NOF2,NOF3,NUNDER,NOVER,NWITHI
      INTEGER NSTEP,NCS,NKV,NVERIF,NC1,NA1,NA12,NT,NEFLAG
      INTEGER NPHYSI,NIPS
      PARAMETER (NFROM=10335,NTO=-601,NSIZE=30786,NSI2=30792)
      PARAMETER (NKRANG=18321,NKR2=18352)
      PARAMETER (NOF=24240,NOF2=24272,NOF3=24273)
      PARAMETER (NUNDER=-2165,NOVER=24885,NWITHI=-5181)
      PARAMETER (NSTEP=31205,NCS=5560,NKV=18480)
      PARAMETER (NVERIF=-3419,NC1=6040,NA1=2840,NA12=2872)
      PARAMETER (NT=-1,NEFLAG=8252)
      PARAMETER (NPHYSI=25945,NIPS=15059)
C
C Fetch most parameters
      IDERR=NKRANG
      KMIN=MAX(VAL(NKRANG),0.)
      KMAX=MIN(VAL(NKR2),20.)
      IF (KMAX.LE.KMIN) GOTO 20
      IDERR=NSTEP
      S=VAL(NSTEP)
      IF (S.LE.0) GOTO 20
      IDERR=NCS
      CS=VAL(NCS)*1E6
      IF (CS.LE.0) GOTO 20
      IDERR=NKV
      V=VAL(NKV)*1E3
      IF (V.LE.0) GOTO 20
C
C Wavelength/nm
      X=V*1.60206E-4/9.1083/2.99763**2
      LAM=6.62517/SQRT(2*1.60206*9.1083*V*(1+.5*X))
C
C Source picture initialisation and loading
      NCOL=NCOLS(LP1)
      NROW=NROWS(LP1)
      NLAY=NLAYS(LP1)
      CLASS=CLASSN(LP1)
C
C Fault over-sized or wrong-shaped source
C (latter only 'cos single DK used)
      IDERR=IVALPN(NFROM)
      N4=NCOL
      IF (N4*NROW.GT.MAXF.OR.NCOL.NE.NROW/2+1) THEN
         ERROR=5
         RETURN
      ENDIF
C
C Fault multi-layer source
      IF (NLAY.NE.1) THEN
         ERROR=62
         IDERR=VERB
         RETURN
      ENDIF
C
C Insist source is Spectrum or Fourier, and half-plane
      IF (CLASS.NE.NCLSPE.AND.CLASS.NE.NCLFOU) THEN
         ERROR=6
         RETURN
      ENDIF
      IF (SEMTFC(LP1,HALF)) RETURN
      IF (.NOT.HALF) THEN
         ERROR=63
         RETURN
      ENDIF
C
C Establish table dimensions
      NCOLO=IVAL(NSIZE)
      NROWO=NCOLO
      IF (VARSET(NSI2)) NROWO=IVAL(NSI2)
      IF (NCOLO.LT.3.OR.NCOLO.GT.NXM
     +   .OR.NROWO.LT.3.OR.NROWO.GT.NXM) THEN
         ERROR=5
      RETURN
      ENDIF
C
C Output picture initialisation - if TO set
      IDERR=IVALPN(NTO)
      IF (IDERR.NE.0) THEN
         LP2=LP1
         IF (SEMOPN(2,IDERR,NCOLO,NROWO,1,NCLCOR,NFMFP,LP2))
     +   RETURN
      ENDIF
C
C Load source pic to array DP
C [Does anything trap overflow..?]
      IF (SEMLAY(1,DP,NCOL,NFMFP,1,LP1)) RETURN
C
C Truncate KMAX at picture boundary
      DK=1./NROW/S
      KMAX=MIN(KMAX,(NCOL-1.1)*DK)
C
C Annotate display?
      IF (MARSET(ANNOT,MARK)) RETURN
      IF (ANNOT) THEN
         IF (FSINIT(3,MARK)) RETURN
         IF (FSPTYP.NE.1) RETURN
C Reset clipping limits to picture border
         FSXMIN=FSBLEF
         FSXMAX=FSBRIG
         FSYMIN=FSBBOT
         FSYMAX=FSBTOP
C Mark circles at KMIN,MKAX
         IF (FSCIRC(0.,0.,KMIN/DK)) RETURN
         IF (FSCIRC(0.,0.,KMAX/DK)) RETURN
      ENDIF
C
C Find mirror axis (the one nearer +X)
      R1=KMIN/DK
      R2=KMAX/DK
      IF (R1.GT.R2) THEN
         IDERR=NKRANG
         GOTO 20
      ENDIF
      CALL FMIRRD(DP,NCOL,NCOL,NROW,AMIRR,R1,R2,SEC,OCF,NW)
C
C Establish central focus for search in each direction
      D1B=VAL(NOF)
      D2B=VAL(NOF2)
C If three values provided for OF, interpret as C1,A1[2]
      IF (VARSET(NOF3)) THEN
         C1=D1B
         A1=D2B
         A12=VAL(NOF3)
         A=SQRT(A1*A1+A12*A12)
C Mirror may indicate A1 dirn or its normal; D1 is C1+|A| if the
C former, but C1-|A| if the latter
         IF (A.NE.0.) THEN
            APHI=.5*ATAN2(A12,A1)
            CAM=COS(AMIRR)*COS(APHI)+SIN(AMIRR)*SIN(APHI)
            IF (ABS(CAM).LT.COS45) A=-A
         ENDIF
         D1B=C1+A
         D2B=C1-A
      ENDIF
C
C Set search range to WITHIN around D1B,D2B
      DD=VAL(NWITHI)
      IDERR=NWITHI
      IF (DD.LE.0) GOTO 20
      D1T=D1B+DD
      D1B=D1B-DD
      D2T=D2B+DD
      D2B=D2B-DD
C Suppress part ranges to honour UNDER,OVER
      IF (CONOPT(NUNDER,NOVER)) RETURN
      IF (OPT(NUNDER)) THEN
         D1B=MIN(D1B,0.)
         D1T=MIN(D1T,0.)
         D2B=MIN(D2B,0.)
         D2T=MIN(D2T,0.)
      ENDIF
      IF (OPT(NOVER)) THEN
         D1B=MAX(D1B,0.)
         D1T=MAX(D1T,0.)
         D2B=MAX(D2B,0.)
         D2T=MAX(D2T,0.)
      ENDIF
C
C Tabulate XCC array
C - ugly ERROR setting is to allow abandon w/o including
C   COMMON in DPFIT2
      IF (D2FIT2(DP,NCOL,NCOL,NROW,XC,NXM,NCOLO,NROWO,
     +    AMIRR,KMIN,KMAX,D1B,D1T,D2B,D2T,DK,CS,LAM,
     +    C1,A1,A12,A,EFL)) THEN
        ERROR=4
        RETURN
      ENDIF
C
C Report results
      IF (OPT(NVERIF)) THEN
         WRITE (RECORD,10) C1,A1,A12
   10 FORMAT (' Best fitting C1,A1[2]: ',3F8.2)
         IF (SEMCON(RECORD)) RETURN
      ENDIF
C
C Return fitted parameters as C1,A1[2] with xcc in T
      IF (SEMLU(1,NC1,C1)) RETURN
      IF (SEMLU(1,NA1,A1)) RETURN
      IF (SEMLU(1,NA12,A12)) RETURN
C Force PHYS and IPS=2
      IF (SEMLU(1,NPHYSI,1.)) RETURN
      IF (SEMLU(1,NIPS,2.)) RETURN
C Return xcc value
      IF (SEMLU(1,NT,A)) RETURN
C Flag xcc value at edge of search region
      IF (SEMLU(1,NEFLAG,EFL)) RETURN
C
C Write XCC array to output
      IF (IVALPN(NTO).NE.0) THEN
         IF (SEMLAY(2,XC,NXM,NFMFP,1,LP2)) RETURN
      ENDIF
C
C Mark ring maxima along principal axes on dislay
C
      IF (ANNOT) THEN
C Ensure A is dirn of max Overfocus
         AM=SQRT(A1**2+A12**2)
         IF (AM.NE.0.) THEN
            A=.5*ATAN2(A12,A1)
         ELSE
            A=0
         ENDIF
C Mark maxima along principal directions
         IF (D3FMKM(C1+AM,A,DK,KMAX,LAM,CS)) RETURN
         IF (D3FMKM(C1-AM,A+PI/2,DK,KMAX,LAM,CS)) RETURN
      ENDIF
C
      RETURN
C
C Error return(s)
C Bad value
   20 ERROR=3
      RETURN
      END
C
C Submodule D3FMKM marks CTF maxima for D3FIT
C
      LOGICAL FUNCTION D3FMKM(D,A,DK,KMAX,LAM,CS)
      REAL D,A,DK,KMAX,LAM,CS
      LOGICAL D3FMKP
      REAL G,GM,R1,R2,S
      REAL PI
      PARAMETER (PI=3.1415927)
C
C Prepare error return
      D3FMKM=.TRUE.
      S=KMAX/DK*.07
C
C Overfocus: the easier case
      IF (D.GT.0) THEN
         GM = .5*PI*LAM**3*CS*KMAX**4 + PI*LAM*D*KMAX**2
         G=PI/2
 200     IF (G.LE.GM) THEN
            R1=SQRT((-D+SQRT(D**2+2*LAM*CS*G/PI))/LAM**2/CS)
            IF (D3FMKP(R1/DK,A,S)) RETURN
            G=G+PI
            GOTO 200
         ENDIF
      ELSE
C Underforcus: the harder case
         GM=-PI/2*D**2/LAM/CS
         G=-PI/2
 210     IF (G.GE.GM) THEN
            R2=D**2+2*LAM*CS*G/PI
            IF (R1.LT.0) RETURN
            R1=SQRT((-D-SQRT(R2))/LAM**2/CS)
C mark it
            IF (R1.LE.KMAX) THEN
               IF (D3FMKP(R1/DK,A,S)) RETURN
            ENDIF
            R1=SQRT((-D+SQRT(R2))/LAM**2/CS)
            IF (R1.LE.KMAX) THEN
               IF (D3FMKP(R1/DK,A,S)) RETURN
            ENDIF
            G=G-PI
            GOTO 210
         ENDIF
      ENDIF
C
C Normal return
      D3FMKM=.FALSE.
      RETURN
C
      END
C
C Submodule D3FMKP - for D3FMKM only
      LOGICAL FUNCTION D3FMKP(R,A,S)
      LOGICAL FSLINE
      REAL R,A,S,X,Y,DX,DY
C
C Prepare error return
      D3FMKP=.TRUE.
C
      X=R*COS(A)
      Y=R*SIN(A)
      DX=-S*SIN(A)
      DY=S*COS(A)
      IF (FSLINE(X-DX,Y-DY,X+DX,Y+DY)) RETURN
      IF (FSLINE(-X-DX,-Y-DY,-X+DX,-Y+DY)) RETURN
      D3FMKP=.FALSE.
      RETURN
      END
C
C Submodule FMIRRD finds mirror axis orientation
C
      SUBROUTINE FMIRRD(A,MA,MAP,NA,AMIRR,R1,R2,SEC,OCF,NW)
C
C Cross-correlates circular sections with their own reflection;
C rings are evenly spread over radii R1,R2 at one-pixel increments,
C and individual ring XCFs are weighted in proportion to radius
C before accumulation
C
C SEC(NW+2) and OCF(NW) provide internal workspace
C
      REAL PI
      PARAMETER (PI=3.1415917)
C
      REAL A(MAP,NA),SEC(NW),OCF(NW),AMIRR,R1,R2,R,DR,C1,C2,C3,T,X
      INTEGER MAP,MA,NA,NR,NW,NP,N,I,J
C
C Number of rings
      NR=NINT(R2-R1)
C Number of sample points around rings
      NP=32
   10 IF (NP.LT.NINT(PI*R2)) THEN
         IF (NP*2.LE.NW) THEN
            NP=NP*2
            GOTO 10
         ENDIF
      ENDIF
C Initialise OCF accumulator
      DO 30 I=1,NP
   30 OCF(I)=0
C
C Begin loop over rings
      DR=(R2-R1)/(NR-1)
      R=R1
      DO 60 N=1,NR
         CALL CSECXT(A,MAP,NA,SEC,NP,1,NA/2+1,R,.FALSE.)
         CALL FT1D(SEC,NP/2,-1,.TRUE.,.FALSE.,.FALSE.)
C Suppress DC values
         SEC(1)=0
C Product of conjugate and reflected xform = squared conjugate
         DO 40 I=1,NP/2+1
            T=SEC(2*I-1)**2-SEC(2*I)**2
            SEC(2*I)=-2*SEC(2*I-1)*SEC(2*I)
            SEC(2*I-1)=T
   40 CONTINUE
         CALL FT1D(SEC,NP/2,1,.TRUE.,.FALSE.,.FALSE.)
         DO 50 I=1,NP
            OCF(I)=OCF(I)+SEC(I)*R
   50 CONTINUE
C
C End loop over rings
      R=R+DR
   60 CONTINUE
C
C Find maximum
      X=0
      DO 80 I=1,NP
         IF (OCF(I).GT.X) THEN
            X=OCF(I)
            J=I
         ENDIF
   80 CONTINUE
C
C Fit quadratic to 5 points near peak
      C1=0
      C2=0
      C3=0
      DO 100 I=1,5
         T=OCF(1+MOD(J+I-4+NP,NP))
         C3=C3+T
         C2=C2+(I-3)*T
         C1=C1+(I-3)**2*T
  100 CONTINUE
      C2=C2/10
      C1=(C1-2*C3)/14
      X=-C2/2/C1
      AMIRR=(NP/2-J-X)*PI/NP/2
      RETURN
      END
C
C CSECXT submodule extracting interpolated circular section from 2-D
C        array from A(M,N) to SEC(L); ring centre M0,N0 radius R,
C        with samples evenly spread anticlockwise from the bottom;
C        semicircle unless FULL
C        NB: no checks on arguments
C
      SUBROUTINE CSECXT(A,M,N,SEC,L,M0,N0,R,FULL)
      REAL A(M,N),SEC(1),R,DTH,COD,SID,CO,SI,X,Y,CX,CY
      INTEGER M,N,L,M0,N0,I,J,K
      LOGICAL FULL
C
      DTH=3.1415927/L
      IF (FULL) DTH=DTH+DTH
      COD=COS(DTH)
      SID=SIN(DTH)
      CO=0
      SI=-R
C
C Loop over pixels around ring
      DO 10 K=1,L
         X=M0+CO
         Y=N0-SI
         I=X
         J=Y
         X=X-I
         Y=Y-J
         CX=1-X
         CY=1-Y
C Bilinear interpolation
         SEC(K)=CX*(A(I,J)*CY+A(I,J+1)*Y)+X*(A(I+1,J)*CY+A(I+1,J+1)*Y)
         X=CO
         CO=CO*COD-SI*SID
         SI=SI*COD+X*SID
 10   CONTINUE
C
      RETURN
      END
C
C D2FIT2 tabulates XCC between model and data
C ------

      logical function d2fit2(dp,n1p,n1,n2,xc,nx1p,nx1,nx2,
     +    amirr,kmin,kmax,D1b,D1t,D2b,D2t,dk,cs,lam,
     +    c1m,a1rm,a1im,xcm,eflag)

C input args:
C   DP() array of data to be fitted over region (1:N1,1:N2)
C   N1P 1st physical dimension of DP
C   N1,N2 dimns of data DP
C   NX1P 1st physical dimension of XC
C   NX1,NX2 dimns of resulting array XCC
C   LAM wavelength/nm
C   CS spherical aberration/nm
C   DK sp.freq. sampling interval /(nm^-1)
C   AMIRR azim/mrad of (either DP mirror axis (anticl. from +X)
C   KMIN,KMAX range of sp.freq.(nm) over which data compared
C   D1B,D1T bot,top defocus range for 1st subscr of XC (overfoc +)
C   D2B,D2T ditto for 2nd subscr
C output args:
C   XC() XCC values for defocus range (D1b,D1t) and (D2b,D2t)
C   C1M,A1RM,A1IM at max element (best fit)

C Comparison is restricted to k in range (kmin,kmax) only;
C function compared is square of sin(g)/(1+w.k^4)],
C   with g = .5*pi*Cs*lam^3*k4 + pi*lam*D'*k^2
C   eff.defocus D' = D + A1Rcos(2[t-AMIRR]) + A1Isin(2[t-AMIRR)
C     for k with azim. t
C   and w chosen to make envelope 0.2 at k=kmax
C imaging parameters consistently those of IPS=2
      logical abandn
      real pi
      parameter (pi=3.1415927)
      real dp(n1p,n2),xc(nx1p,nx2)
      real amirr,dk,lam,cs,kmin,kmax,kkmin,kkmax,kx,ky,kyky,kk,c2a,s2a
      real w,sd,sdd,sn,sdm,sddm,st,stt,sdt,xcm,dx1,dx2,xcp,eflag
      real d1,d2,d1b,d1t,d2b,d2t,c1m,a1rm,a1im
      real d,a,a1r,a1i,csp,dpxx,dpyy,dpxy,g,t,x,y
      integer n1p,n1,n2,nx1p,nx1,nx2,i1,i2,ix1,ix2,ix1m,ix2m

C initialise error return
      d2fit2=.true.

      kkmin=kmin*kmin
      kkmax=kmax*kmax
      c2a=cos(2*amirr)
      s2a=sin(2*amirr)

C establish envelope width parameter giving 10% transfer at kmax
      w=4/kkmax/kkmax

C determine mean,var,num of data with comparison range
      sd=0
      sdd=0
      sn=0
      ky=(n2/2)*dk
      do 1 i2=1,n2
         kx=0
         kyky=ky*ky
         do 2 i1=1,n1
            kk=kx*kx+kyky
            if (kk.ge.kkmin.and.kk.le.kkmax) then
               sd=sd+dp(i1,i2)
               sdd=sdd+dp(i1,i2)**2
               sn=sn+1
            endif
            kx=kx+dk
    2   continue
         ky=ky-dk
    1      continue
      sdm=sd/sn
      sddm=sdd/sn

C begin loops over XC pixels
      xcm=0
      dx1=(d1t-d1b)/(nx1-1)
      dx2=(d2t-d2b)/(nx2-1)
      d2=d2t
      do 10 ix2=1,nx2
         d1=d1b
         do 20 ix1=1,nx1

C convert D1,D2,AMIRR to d,a1r,a1i
            d=.5*(d1+d2)
            a=.5*(d1-d2)
            a1r=a*c2a
            a1i=a*s2a
C prepare coeffs for gamma expr
            Csp=.5*pi*Cs*lam*lam*lam
            Dpxx=pi*lam*(d+a1r)
            Dpyy=pi*lam*(d-a1r)
            Dpxy=pi*2*lam*a1i

C initialise xc accumulators
            sdt=0
            st=0
            stt=0

C begin loops over DP pixels
            ky=(n2/2)*dk
            do 30 i2=1,n2
               kx=0
               kyky=ky*ky
               do 40 i1=1,n1

C generate CTF: [sin^2(g)/(1+k^2/kc^2)]^2
                  kk=kx*kx+kyky
                  if (kk.ge.kkmin.and.kk.le.kkmax) then
                     g = Csp*kk**2
     +                   + Dpxx*kx*kx + Dpyy*kyky + Dpxy*kx*ky
                     t = ( sin(g)/(1+w*kk*kk) )**2

C acc. cross sum, sum and sum square
                     sdt=sdt+(dp(i1,i2)-sdm)*t
                     st=st+t
                     stt=stt+t*t
                  endif

C end loops over DP pixels
                  kx=kx+dk
   40          continue
               ky=ky-dk
   30       continue

C calculate and deposit xc
            xcp=sdt/sn/sqrt((stt/sn-st*st/sn/sn)*(sddm-sdm*sdm))
            xc(ix1,ix2)=xcp

C note maximum
            if (xcp.gt.xcm) then
               xcm=xcp
               ix1m=ix1
               ix2m=ix2
            endif

C end loops over xc pixels
            d1=d1+dx1
   20    continue
C abandon?
         if (abandn(i1)) return
         d2=d2-dx2
   10 continue

C refine fitted values by fitting bi-quadratic to 3x3 region
      d1=d1b+(ix1m-1)*dx1
      d2=d2t-(ix2m-1)*dx2
      if (ix1m.eq.1.or.ix1m.eq.nx1
     +   .or.ix2m.eq.1.or.ix2m.eq.nx2) then
         eflag=1
      else
         call bqextr(xc(ix1m-1,ix2m-1),3,nx1p,x,y,xcm)
         d1=d1+x*dx1
         d2=d2+y*dx2
         eflag=0
      endif

C recast d1,d2 as focus,astig
      c1m=.5*(d1+d2)
      a=.5*(d1-d2)
      a1rm=a*c2a
      a1im=a*s2a

C normal return
      d2fit2=.false.
      return
      end
C Semper local submodule BQEXTR
C
C Locates extremum of biquadratic fitted to 3x3 / 5x5 array supplied
C and returns position as X,Y (rel. central pixel of array)
C
C Fitted function is
C   F = C + CX.X + CY.Y + CXX.X^2 + CXY.X.Y + CYY.Y^2
C
      SUBROUTINE BQEXTR(F,N,NP,X,Y,T)
      REAL F(NP,N)
      REAL X,Y,S,SX,SY,SXX,SXY,SYY,C,CX,CY,CXX,CXY,CYY,D,T
      INTEGER N,NP,I,J
C
C Ignore if N not 3 or 5
      IF (N.NE.3.AND.N.NE.5) GOTO 30
C
C Evaluate data sums
      S=0
      SX=0
      SY=0
      SXX=0
      SXY=0
      SYY=0
      Y=N/2
      DO 10 J=1,N
        X=-N/2
        DO 20 I=1,N
          D=F(I,J)
          S=S+D
          SX=SX+D*X
          SY=SY+D*Y
          SXX=SXX+D*X*X
          SXY=SXY+D*X*Y
          SYY=SYY+D*Y*Y
          X=X+1
 20     CONTINUE
        Y=Y-1
 10   CONTINUE
C
C Evaluate fit coefficients
      IF (N.EQ.3) THEN
        C=(5*S-3*SXX-3*SYY)/9
        CX=SX/6
        CY=SY/6
        CXX=(3*SXX-2*S)/6
        CXY=SXY/4
        CYY=(3*SYY-2*S)/6
      ELSE
        C=(27*S-5*SXX-5*SYY)/175
        CX=SX/50
        CY=SY/50
        CXX=(SXX-2*S)/70
        CXY=SXY/100
        CYY=(SYY-2*S)/70
      ENDIF
C
C Evaluate extremum position
      D=CXY*CXY-4*CXX*CYY
      IF (D.EQ.0) GOTO 30
      X=(2*CYY*CX-CXY*CY)/D
      Y=(2*CXX*CY-CXY*CX)/D
      T=C+CX*X+CY*Y+CXX*X*X+CXY*X*Y+CYY*Y*Y
      RETURN
C
C Error return
 30   X=0
      Y=0
      RETURN
C
      END
C Semper 6+ local module GJW  -- ?modifed OPEN stmts
C
      SUBROUTINE GJW
C
C Reads GJW multi-slice files as full plane Complex Fourier pictures.
C
C Beam indices H,K increase to right and up respectively (i.e. in the
C most natural way: no transposition is needed)
C Beams are multiplied by total number of pixels to suit Semper FT
C conventions; no conjugation is needed however
C
C Reads slices # SLICE,SL2 for each of tilts # TILT,TI2 in turn
C - in default, output is to series of one-layer pics TO,TO+1,..
C   but if SINGLE to a single multi-layer pic instead
C - reports file details on terminal unless NOVERIFY
C
C VD required:
C GJW :GJW name=' slices= sl2= tilts= ti2= $1=sel to=$1 size= si2= +
C     single verify
C
C Global declarations
      INCLUDE 'COMMON'
C
C Local declarations
      LOGICAL SEMCLS,SEMROW,SEMKTX,SEMLU,OPT,OPTNO,VARSET
      LOGICAL SEMCON,SEMDIA,SEMXA1,FILMAK,GJW2
      LOGICAL SINGLE,VERIFY,LDUM
C
      REAL AA,BB,DZ,BETA,WAVEL,T,T1,T2,TIL11,TIL22,TILLY,DELTAA
      INTEGER IVAL,IVALPN
      INTEGER LABEL(256),NCOL,NROW,NLAY,ROW,LAYER
      INTEGER H,I,J,K,L2,N,NBLANK,LHEAD
      INTEGER IT,IS,IT1,IT2,IS1,IS2,ITO
      INTEGER*4 NBEAMS,NSLS,H1,H2,K1,K2,M5,NTLTS,ISLICE,NCELL,IXL
C Changed by LDM from byte to character
C      BYTE HEAD(60),HSPACE/1H /
      CHARACTER*1 HEAD(60),HSPACE
      COMPLEX U
C Max length of file name
      INTEGER ICFILE(FILMAX)
      CHARACTER*4 DFNAM
      CHARACTER*(FILMAX) CFILE,CFILEF
C
C Packed names
      INTEGER NSLICE,NSL2,NNAME,NVERIF,NTILT,NTI2,NSIZE,NSI2
      INTEGER NTO,NSEVER,NSINGL,NH1,NH2,NK1,NK2,NNSL,NNTI,NDZ,NAA,NBB
      INTEGER NBETA,NLAMBD,NN,NT1,NT2
      PARAMETER (NSLICE=30889,NSL2=30912,NNAME=22453,NVERIF=-3419)
      PARAMETER (NTILT=-373,NTI2=-393)
      PARAMETER (NSIZE=30786,NSI2=30792,NTO=-601,NSINGL=30774)
      PARAMETER (NH1=14040,NH2=14080,NK1=18840,NK2=18880)
      PARAMETER (NNSL=23172,NNTI=23209,NDZ=7440)
      PARAMETER (NAA=1640,NBB=3280,NBETA=3420,NLAMBD=19253)
      PARAMETER (NN=22400,NT1=-1241,NT2=-1281)
CC
      EQUIVALENCE (RB4,HEAD,ICFILE),(RB1,LABEL)
      data HSPACE /' '/
C
C Initialise options
      SINGLE=OPT(NSINGL)
      VERIFY=.NOT.OPTNO(NVERIF)
C
C Establish .MS file and open it
      N=80
      IF (SEMKTX(NNAME,'File name (as textstring): ',
     +   ICFILE,N,.FALSE.)) RETURN
      IF (N .EQ. 0) RETURN
C
C Now recoded using m/c-indep extension support
      CALL SEMCHS(CFILE,ICFILE,N)
C Find significant length
      CFILE=CFILE(1:NBLANK(CFILE))//'.ms'
      CLOSE (RDWRTU)
      OPEN (RDWRTU,FILE=CFILE,STATUS='OLD',ERR=210,
     +   FORM='UNFORMATTED')
C
C Process the header
      READ (RDWRTU,END=180,ERR=180) HEAD
      READ (RDWRTU,END=180,ERR=180) NSLS,H1,K1,H2,K2,M5
      READ (RDWRTU,END=180,ERR=180) NTLTS,DZ
      DZ=DZ/10.
      READ (RDWRTU,END=180,ERR=180) AA,BB,BETA,WAVEL
      AA=AA/10.
      BB=BB/10.
      WAVEL=WAVEL/10.
      NBEAMS=(H2-H1+1)
      NBEAMS=NBEAMS*(K2-K1+1)
      IF (VERIFY) THEN
         WRITE (RECORD,21) HEAD
   21       FORMAT ('Title: ',60A1)
         IF (SEMCON(RECORD)) RETURN
         WRITE (RECORD,22) NSLS,NTLTS
   22       FORMAT
     +      ('File holds',I4,' thicknesses for each of',I4,' tilts')
         IF (SEMCON(RECORD)) RETURN
         WRITE (RECORD,23) DZ,WAVEL*1000.
   23       FORMAT ('Slice thickness ',F9.3,'nm  Wavelength ',F6.3,'pm')
         IF (SEMCON(RECORD)) RETURN
         WRITE (RECORD,24) AA,BB,BETA
   24       FORMAT
     +      ('Projected unit cell dims ',2F10.3,'nm,  angle ',F7.3)
         IF (SEMCON(RECORD)) RETURN
         WRITE (RECORD,30) H1,K1,H2,K2,NBEAMS
   30       FORMAT ('Beams filed for (h,k)=(',I5,',',I5,') to (',
     +      I5,',',I5,')  Total number ',I5)
         IF (SEMCON(RECORD)) RETURN
         IF (SEMCON(' ')) RETURN
      ENDIF
C
C Find significant length of header
      LHEAD=60
   40 IF (HEAD(LHEAD).EQ.HSPACE) THEN
         LHEAD=LHEAD-1
         IF (LHEAD.NE.0) GOTO 40
      ENDIF
C
C Save values of H1 etc
      IF (SEMLU(1,NH1,REAL(H1))) GOTO 170
      IF (SEMLU(1,NH2,REAL(H2))) GOTO 170
      IF (SEMLU(1,NK1,REAL(K1))) GOTO 170
      IF (SEMLU(1,NK2,REAL(K2))) GOTO 170
      IF (SEMLU(1,NNSL,REAL(NSLS))) GOTO 170
      IF (SEMLU(1,NNTI,REAL(NTLTS))) GOTO 170
      IF (SEMLU(1,NDZ,DZ)) GOTO 170
      IF (SEMLU(1,NAA,AA)) GOTO 170
      IF (SEMLU(1,NBB,BB)) GOTO 170
      IF (SEMLU(1,NBETA,BETA)) GOTO 170
      IF (SEMLU(1,NLAMBD,WAVEL)) GOTO 170
C
C Establish tilt loop parameters
      IT1=1
      IT2=NTLTS
      IF (VARSET(NTILT)) THEN
         IT1=IVAL(NTILT)
         IT2=IT1
      ENDIF
      IF (VARSET(NTI2)) IT2=IVAL(NTI2)
      IF (IT1.LE.0.OR.IT2.GT.NTLTS) THEN
         ERROR=3
         IDERR=NTILT
         GOTO 170
      ENDIF
C
C Establish slice loop parameters
      IS1=1
      IS2=NSLS
      IF (VARSET(NSLICE)) THEN
         IS1=IVAL(NSLICE)
         IS2=IS1
      ENDIF
      IF (VARSET(NSL2)) IS2=IVAL(NSL2)
      IF (IS1.LE.0.OR.IS2.GT.NSLS) THEN
         ERROR=3
         IDERR=NSLICE
         GOTO 170
      ENDIF
C
C Total number of beam sets
      NLAY=(IS2-IS1+1)*(IT2-IT1+1)
C
C Establish output picture size
      NCOL=2
   50 IF (H1.LT.-NCOL/2.OR.H2.GT.(NCOL-1)/2) THEN
         NCOL=2*NCOL
         GOTO 50
      ENDIF
      NROW=2
   60 IF (K1.LT.-(NROW-1)/2.OR.K2.GT.NROW/2) THEN
         NROW=2*NROW
         GOTO 60
      ENDIF
      IF (VARSET(NSIZE)) THEN
         NCOL=IVAL(NSIZE)
         NROW=NCOL
      ENDIF
      IF (VARSET(NSI2)) NROW=IVAL(NSI2)
C Normalisation factor
      T=REAL(NCOL)*REAL(NROW)
C
C Establish output picture number
      ITO=IVALPN(NTO)
C
C If SINGLE mode, open output
      IF (SINGLE) THEN
         IF (GJW2(ITO,NCOL,NROW,NLAY,HEAD,LHEAD,DZ,ISLICE,TIL11,TIL22))
     +      GOTO 170
      ENDIF                                                                    
C
C Begin level 1 loop - over tilt/output pictures
      LAYER=1
      DO 160 IT=1,IT2
C
C Begin level 2 loop - over slice/output pictures
      DO 150 IS=1,NSLS
C
C Process info for this slice
      READ (RDWRTU,END=180,ERR=180)
     +   ISLICE,NCELL,IXL,TIL11,TIL22,TILLY,DELTAA
C
C Skipping initial tilts or slices?
      IF (IT.GE.IT2 .AND. IS.GT.IS2) GOTO 170
      IF (IT.LT.IT1.OR.IS.LT.IS1.OR.IS.GT.IS2) THEN
         DO 80 K=K1,K2
            DO 70 H=H1,H2
C Read beam
               READ (RDWRTU,END=180,ERR=180) U
   70       CONTINUE
   80    CONTINUE
         GOTO 150
      ENDIF
C
      IF (VERIFY) THEN
         WRITE (RECORD,90) ISLICE,TIL11,TIL22
   90       FORMAT ('Slice number',I5,'  tilt components ',2F7.3)
         IF (SEMCON(RECORD)) RETURN
      ENDIF
      IF (SEMLU(1,NN,REAL(ISLICE))) GOTO 170
      IF (SEMLU(1,NT1,TIL11)) GOTO 170
      IF (SEMLU(1,NT2,TIL22)) GOTO 170
C
C
C Open output
      IF (.NOT.SINGLE) THEN
         IF (GJW2(ITO,NCOL,NROW,1,HEAD,LHEAD,DZ,ISLICE,TIL11,TIL22))
     +      GOTO 170
      ENDIF                                                                    
C
C Initialise output to zero
      L2=2*NCOL
      DO 110 I=1,L2
         RB1(I)=0.
  110 CONTINUE
      DO 120 N=1,NROW
         IF (SEMROW(2,RB1,NFMCOM,N,LAYER,LP1)) GOTO 170
  120 CONTINUE
C
C Prepare data pass
      ROW=0
C
C Level 3 loops: over beams filed
      DO 140 K=K1,K2
         DO 130 H=H1,H2
C
C Read beam
            READ (RDWRTU,END=180,ERR=180) U
C
C Set corresponding pixels
            I=H+CCOLN(LP1)
            IF (I.LE.0.OR.I.GT.NCOL) GOTO 130
            J=CROWN(LP1)-K
            IF (J.LE.0.OR.J.GT.NROW) GOTO 130
            IF (J.NE.ROW.AND.ROW.NE.0) THEN
               IF (SEMROW(2,RB1,NFMCOM,ROW,LAYER,LP1)) GOTO 170
               IF (SEMROW(1,RB1,NFMCOM,J,LAYER,LP1)) GOTO 170
            ENDIF
            ROW=J
            RB1(2*I-1)=REAL(U)*T
            RB1(2*I)=AIMAG(U)*T
  130    CONTINUE
  140 CONTINUE
C
C Flush last row
      IF (SEMROW(2,RB1,NFMCOM,ROW,LAYER,LP1)) GOTO 170
C If single mode close output
      IF (.NOT.SINGLE) THEN
         IF (SEMCLS(LP1)) GOTO 170
C Bump layer number (single mode only)
      ELSE
         LAYER=LAYER+1
      ENDIF
C
C End of level 2 loop
  150 CONTINUE
C
C End of level 1 loop
  160 CONTINUE
C
C Exit, closing file
  170 CLOSE (RDWRTU)
      RETURN
C
C Fortran i/o error
  180 LDUM=SEMDIA('Fortran READ/WRITE error',NSEVER)
      ERROR=10
      GOTO 170
C Fortran OPEN/CLOSE error
  210 LDUM=SEMDIA(' Fortran OPEN/CLOSE error',NSEVER)
      ERROR=10
      RETURN
C
      END
C
C Subsidary module GJW2
C
C Opens output picture ITO as LP1 and constructs title
C
      LOGICAL FUNCTION GJW2(ITO,NCOL,NROW,NLAY,HEAD,LHEAD,
     +  DZ,ISLICE,T1,T2)
C
C Local declarations
      LOGICAL SEMOPN,SEMLAB,SEMXA1,LDUM
      REAL DZ,T1,T2
      INTEGER I,N,ITO,NCOL,NROW,NLAY,LHEAD,IDUM
      INTEGER*4 ISLICE
C     Changed by LDM
      CHARACTER*1 HEAD(LHEAD)
C      BYTE HEAD(LHEAD)
      INTEGER LABEL(256)
      INCLUDE 'COMMON'
      EQUIVALENCE (RB1,LABEL)
C
C Prepare error return
      GJW2=.TRUE.
C
C Open output
      LP1=0
      IF (SEMOPN(2,ITO,NCOL,NROW,NLAY,NCLFOU,NFMCOM,LP1)) RETURN
      ITO=ITO+1
C Construct label from file header, thickness, tilt
      IF (LBLINC) THEN
         DO 100 I=1,LHEAD
C     Changed by LDM
C            LABEL(LBNCTT+I)=HEAD(I)
            LABEL(LBNCTT+I)=ICHAR(HEAD(I))
  100    CONTINUE
         N=LBNCTT+LHEAD
         LABEL(N+1)=KSPACE
         LABEL(N+2)=KLCT
         LABEL(N+3)=KLCH
         LABEL(N+4)=KLCI
         LABEL(N+5)=KLCC
         LABEL(N+6)=KLCK
         LABEL(N+7)=KLCN
         LABEL(N+8)=KLCE
         LABEL(N+9)=KLCS
         LABEL(N+10)=KLCS
         LABEL(N+11)=KSPACE
         N=N+12
         LDUM=SEMXA1(4,LABEL,256,N,DZ*REAL(ISLICE),IDUM)
         LABEL(N)=KLCN
         LABEL(N+1)=KLCM
         LABEL(N+2)=KSPACE
         LABEL(N+3)=KLCT
         LABEL(N+4)=KLCI
         LABEL(N+5)=KLCL
         LABEL(N+6)=KLCT
         LABEL(N+7)=KSPACE
         N=N+8
         LDUM=SEMXA1(4,LABEL,256,N,T1,IDUM)
         LABEL(N)=KCOMMA
         N=N+1
         LDUM=SEMXA1(4,LABEL,256,N,T2,IDUM)
         LABEL(LBNCTT)=N-LBTT1+1
         IF (SEMLAB(2,LABEL,LP1)) RETURN
      ENDIF
C
C Normal return
      GJW2=.FALSE.
      RETURN
      END
C Semper 6 processing module IITGEN   -- exactly as on VAX
C
      SUBROUTINE IITGEN
C
C NB: still ignores 3-fold astigmatism..
C
C Generates electron microscope images from waves leaving specimen,
C including 'second-order' image components, by the mutual transfer
C function method, accounting for beam tilt, displaced objective
C apertures, coherence envelopes, specimen vibration and uniaxial
C specimen drift.
C
C Expects full-plane Fourier source containing complex beams leaving
C specimen, i.e. transfer of specimen exit plane wave, and generates
C (half-plane) Fourier output containing corresponding image intensity
C components, i.e. image intensity transform.
C
C The key DEFOCUS and variables STEP(2), ASTIGMATISM,APHI, TILT,TPHI,
C SWIDTH, EWIDTH, OARADIUS,OADISPLACEMENT,OAPHI DRIFT,DPHI, and
C VIBRATION define the imaging conditions.  These are in reduced units
C unless option PHYSICAL is specified, in which case the variables
C KV/kV and CS/mm are used to determine the reduced units.
C
C GLobal declarations
      INCLUDE 'COMMON'
C
C Local declarations
      LOGICAL SEMROW,SEMOPN,SEMLAY,SEMCEN,SEMTFC,VARSET,ABANDN,CTF2
      INTEGER IVAL,IVALPN
C
      REAL K1,K2,KSQ,KI1,KI2,KISQ,KJ1,KJ2,KJSQ,KD,KIA,KJA
      REAL IMR,IMI,FAC,V,FFR,FFI,DI1,DI2,DJ1,DJ2,G,ER,EI
      REAL DMAB2,AST,A1,A2,AA1,AA2,SS,DD,T1,T2,OARSQ,OA1,OA2
      REAL VV2,DR1,DR2,U1,U2,V1,V2,UA,TFAR,TFAI
      LOGICAL HALF
      INTEGER CLASS,BOT,TOP,LEFT,RIGHT,ROW,I,I1,I2,J,J1,J2,N1,N2
      INTEGER NCOL,NROW,NLAY,CCOL,CROW,CROWO,NCOLO,NCOLO2,NROWO
      INTEGER I1MIN,I1MAX,I2MIN,I2MAX,N1MAX,N2MIN,N2MAX
      INTEGER*4 FBASE,NCOL4,NCOLO4,I4,J4,N4
C
C Packed names
      INTEGER NFROM,NTO,NSIZE,NSI2
      PARAMETER (NFROM=10335, NTO=-601, NSIZE=30786, NSI2=30792)
C
C Buffer F is used as 2-D array, holding components of specimen wave
      INTEGER*4 MAXF
C     PARAMETER (MAXF=NNBUF*LNBUF/LNREAL)
C ****** WOS ******
C Local change increasing max source pic size to 512 square for SG only
      PARAMETER (MAXF=512*512*2)
C ****** *** ******
      REAL F(MAXF)
C     EQUIVALENCE (RB1,F)
C
C Source picture initialisation
      NCOL=NCOLS(LP1)
      NCOL4=NCOL
      NROW=NROWS(LP1)
      NLAY=NLAYS(LP1)
      CLASS=CLASSN(LP1)
C
C Note centre and min,max source coords
      CCOL=CCOLN(LP1)
      CROW=CROWN(LP1)
      LEFT=1-CCOL
      RIGHT=NCOL-CCOL
      BOT=CROW-NROW
      TOP=CROW-1
C
C Fault over-sized source
      N4=NCOL
      IF (N4*NROW*2.GT.MAXF) THEN
         ERROR=5
         IDERR=IVALPN(NFROM)
         GOTO 100
      ENDIF
C
C Fault multi-layer source
      IF (NLAY.NE.1) THEN
         ERROR=62
         IDERR=VERB
         GOTO 100
      ENDIF
C
C Insist source is full-plane Fourier
      IF (CLASS.NE.NCLFOU) THEN
         ERROR=6
         IDERR=IVALPN(NFROM)
         GOTO 100
      ENDIF
      IF (SEMTFC(LP1,HALF)) GOTO 100
      IF (HALF) THEN
         ERROR=63
         IDERR=IVALPN(NFROM)
         GOTO 100
      ENDIF
C
C Output picture initialisation
C -----------------------------
C Establish and check output size
      NCOLO=NCOL/2+1
      NROWO=NROW
      IF (VARSET(NSIZE)) THEN
         NCOLO=IVAL(NSIZE)
         NROWO=NCOLO
      ENDIF
      NCOLO4=NCOLO
      NCOLO2=2*NCOLO
      IF (VARSET(NSI2)) NROWO=IVAL(NSI2)
C
C Open output picture and move origin to left
      LP2=LP1
      IF (SEMOPN(2,IVALPN(NTO),NCOLO,NROWO,1,NCLFOU,NFMCOM,LP2))
     +   GOTO 100
      CROWO=NROWO/2+1
      IF (SEMCEN(LP2,1,CROWO,1)) GOTO 100
C
C Imaging conditions initialisation
C ---------------------------------
      IF (CTF2(DMAB2,AST,A1,A2,AA1,AA2,SS,DD,T1,T2,OARSQ,OA1,OA2,
     +   VV2,DR1,DR2,NCOL,NROW,U1,U2,V1,V2,UA,TFAR,TFAI)) GOTO 100
C
C Load specimen wave components to array F(0:NCOL-1,NROW)
C -----------------------------
      IF (SEMLAY(1,F,NCOL,NFMCOM,1,LP1)) GOTO 100

C Prepare base subscript pointing to real part of primary beam
      FBASE=2*(NCOL4*TOP-LEFT)+1
C
C Apply aperture
      I4=1
      DO 4 J=1,NROW
      DO 3 I=1,NCOL
         I1=I-CCOL
         I2=CROW-J
         K1=T1+I1*U1+I2*V1
         K2=T2+I1*U2+I2*V2
         IF ((K1-OA1)**2+(K2-OA2)**2.GT.OARSQ) THEN
            F(I4)=0.
            F(I4+1)=0.
         ENDIF
         I4=I4+2
    3 CONTINUE
    4 CONTINUE
C
C Establish non-zero coord range for output
      N1MAX=MIN(NCOLO-1,NCOL-1)
      N2MIN=MAX(CROWO-NROWO,1-NROW)
      N2MAX=MIN(CROWO-1,NROW-1)
C
C Begin loop over rows of image intensity components
C --------------------------------------------------
      DO 90 ROW=1,NROWO
         N2=CROWO-ROW
C
C Initialise output row to zero
      DO 2 I=1,NCOLO2
         RB6(I)=0.
    2 CONTINUE
C
C If outside region spanned by image intensity components,
C simply output and move on to next row
         IF (N2.GT.N2MAX.OR.N2.LT.N2MIN) THEN
            IF (SEMROW(2,RB6,NFMCOM,ROW,1,LP2)) GOTO 100
            GOTO 90
         ENDIF
C
C Begin loop along row of image intensity components
      DO 80 N1=0,N1MAX
C
C Test whether component lies outside doubled aperture
         K1=N1*U1+N2*V1
         K2=N1*U2+N2*V2
         KSQ=K1*K1+K2*K2
         IF (KSQ.GT.OARSQ*4.) GOTO 83
C
C Initialise normalisation factor
         FAC=REAL((NCOLO-1)*2)*REAL(NROWO)/(REAL(NCOL)*REAL(NROW))**2
C
C Include vibration in norm factor, testing whether wiped out
         IF (VV2.NE.0.) THEN
            V=VV2*KSQ
            IF (V.LE.7.) THEN
               FAC=FAC*EXP(-V)
            ELSE
               GOTO 83
            ENDIF
         ENDIF
C Include drift in norm factor
         KD=K1*DR1+K2*DR2
         IF (KD.NE.0.) FAC=FAC*SIN(KD)/KD
C
C Component present; construct f(i)f*(i-n) sum
C --------------------------------------------
      IMR=0.
      IMI=0.
C
C Establish coord range for I-beams to be summed
      I2MIN=MAX(BOT,BOT+N2)
      I2MAX=MIN(TOP,TOP+N2)
      I1MIN=LEFT+N1
      I1MAX=RIGHT
C
C Begin summing loop over specimen beam rows
      DO 91 I2=I2MIN,I2MAX
C
C For efficiency, several inner loop variables are initialised here
C and incremented inside loop rather than calculated in full
         J2=I2-N2
C Initialise Fi and Fj subscripts
         I4=FBASE+(I1MIN-NCOL4*I2)*2
         J4=FBASE+(I1MIN-N1-NCOL4*J2)*2
C Initialise J1
         J1=I1MIN-N1
C Initialise KI,KJ (vectors Ki,Kj)
         KI1=T1+I1MIN*U1+I2*V1
         KI2=T2+I1MIN*U2+I2*V2
         KJ1=T1+J1*U1+J2*V1
         KJ2=T2+J1*U2+J2*V2
C Initialise KIA,KJA (dot products Ki.a,Kj.a)
         KIA=KI1*A1+KI2*A2
         KJA=KJ1*A1+KJ2*A2
C
C Begin (innermost) summing loop over specimen beam columns
      DO 81 I1=I1MIN,I1MAX
C
C Note: I1,2 points to beam I, and J1,2 to beam J, i.e. beam I-N
C
C Calculate Fi.Fj* as FF
         FFR=F(I4)*F(J4)+F(I4+1)*F(J4+1)
         FFI=F(I4+1)*F(J4)-F(I4)*F(J4+1)
C Test whether either beam vanishes
         IF (FFR.EQ.0.AND.FFI.EQ.0.) GOTO 82
C
C Work out k,k^2, k.a and del(g) components for each beam
         KISQ=KI1*KI1+KI2*KI2
         KJSQ=KJ1*KJ1+KJ2*KJ2
         DI1 = (KISQ-DMAB2)*KI1 - KIA*AA1
         DI2 = (KISQ-DMAB2)*KI2 - KIA*AA2
         DJ1 = (KJSQ-DMAB2)*KJ1 - KJA*AA1
         DJ2 = (KJSQ-DMAB2)*KJ2 - KJA*AA2
C
C Calculate mutual envelope factor exp(-v), with
C v(ki,kj) = pi^2 { s^2 |di-dj|^2 + .5 d^2 (ki^2-kj^2)^2 }
C in which vector gradients di,dj = { (k^2 - [D-.5A])k - (k.a)Aa }
         V = SS*((DI1-DJ1)**2 + (DI2-DJ2)**2) + DD*(KISQ-KJSQ)**2
C Test whether contribution wiped out
         IF (V.LE.7.) THEN
            V=EXP(-V)
         ELSE
            GOTO 82
         ENDIF
C
C Accumulate Fi.Fj*.exp{-i(gi-gj)}.exp(-v), with
C g(k) = pi { (.5k^2 - [D-.5A])k^2 - A(k.a)^2 }
         G = PI * ( (0.5*KJSQ-DMAB2)*KJSQ - (0.5*KISQ-DMAB2)*KISQ
     +       - AST*(KJA*KJA-KIA*KIA) )
         ER=COS(G)*V
         EI=SIN(G)*V
         IMR=IMR+FFR*ER-FFI*EI
         IMI=IMI+FFI*ER+FFR*EI
C
C Increment inner loop variables
   82    I4=I4+2
         J4=J4+2
         J1=J1+1
         KI1=KI1+U1
         KI2=KI2+U2
         KJ1=KJ1+U1
         KJ2=KJ2+U2
         KIA=KIA+UA
         KJA=KJA+UA
C
C End summing loops
   81 CONTINUE
   91 CONTINUE
C
C Deposit sum in output array, modified by vibration/drift factor
         RB6(2*N1+1)=IMR*FAC
         RB6(2*N1+2)=IMI*FAC
C
C Abandon requested?
   83 IF (ABANDN(ERROR)) GOTO 100
C
C End loop over row of image intensity components
   80 CONTINUE
C
C Output row
        IF (SEMROW(2,RB6,NFMCOM,ROW,1,LP2)) GOTO 100
   90 CONTINUE
C
C NB: Library program needed to perform functions of CTF VERIFY
C with ALL parameters, in reduced and physical values
C
  100 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 system module SEMLAY -- ** BUG FIX: i*4 N1
C
