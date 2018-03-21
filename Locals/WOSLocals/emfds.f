C Semper VI local processing module EMFIT
C
      SUBROUTINE EMFDS
C
C NB: minor infelicity at present: on entry, Abn coeffs are assumed to
C be rel. injected tilt origin, whereas on exit they are given rel. CF
C axis; this means re-entry with unchanged params reports a small rise
C in misfit (though it is quickly removed again)
C
C No provision for linear drift fitting (in shift or focus)...
C
C Fits EM imaging parameters to set EITHER of shift vectors OR
C of measured defocus,astig values for given injected tilts
C
C Expects 4, 5, or 7 layer Plist source (LIST type), containing injected
C tilt and observations in the following order:
C   lay 1: Tx1,Tx2,..   or lay 1: Tx1,Tx2,..    or lay 1: Tx1,Tx2,..
C       2: Ty1,..              2: Ty1,..               2: Ty1,..
C       3: Dx1,..  X-shift     3: C1,..    Overfoc     3: Dx1,..
C       4: Dy1,..  Y-shift     4: A1r,...  Astig       4: Dx2,..
C                              5: A1i,...              5: C1,..
C                                                      6: A1r,..
C                                                      7: A2r,..
C
C The ordering is consistent with other Plist usage, so that PLDEL etc
C work.  Entries for which injected tilt is zero or for which the shift
C is >1e5, are ignored.
C
C Depending on mode set by FIT,FIX keys (see below), fits and returns
C revised vals for IPS 2, viz
C   C1        Overfocus
C   A1,A12    2-fold Astig
C   A2,A22    3-fold astig; dirn in TFP
C   BTILT,BT2 Beam tilt wrt coma-free axis
C   CS        Spherical aberration
C Also depending on value of TFMODE, adjusts nominal tilts in one of
C four ways:
C   1  multiplication by T
C   2  rotation anticl by THE
C   3  = 1 + 2
C   4  general transformation via /  TC TC2 \
C                                 \ TC3 TC4 /
C Fit mode is set by FIT,FIX keys, which takes textstring listing
C params to be fitted, eg FIX 'C1' FIT 'BTILT,CS'; in default,
C all are fitted except for CS; fitting A1 implies fitting A12 etc
C
C Units are as for CTF PHYSICAL - mrad for tilts, nm for shifts,
C defocus etc, and mm for CS
C
C Display annotation - Shift mode only (DPs better done externally):
C Marks observed shifts in partition MARK, and fitted shifts to same
C scale in MA2 (defaulting to MARK, so that shifts superpose).  Obs
C shifts are marked as arrow vectors proportional to shift, emanating
C from posn serial marked at posn prop. to tilt; fitted vectors
C as outer half of arrows only, with dot at inj.tilt posn.
C Scale can be forced via TSCALE (=disp.pixels/tilt unit) and
C SSCALE (=disp.pixels/shift unit.  Graphics coord system set up
C is that of tilts, so that e.g. MARK POSN TX,TY works afterwards
C
C Unless NOVERIFY, verifies fitted Chi^2 achieved and fitted params
C If VERIFY, verifies data, fitted values and residuals
C
C Verb descriptor required:
C   EMFit :EMFDS verify fix=' fit=' tfmode= sds= sdd= +
C     mark= ma2= tscale= sscale= erase >select
C
C Further details of internal organisation
C ----------------------------------------
C Aberrations are defined via the following expression for the phase
C shift in a beam at angles (dirn cosines) x,y, using complex w=x+iy,
C and complex coefficients A0..
C  g = (2pi/l).Re{A0.w*        image position (or shift)
C          + (1/2)A1.w*^2      2-fold astig (1/2)|A1||w|^2.cos2(p-p22);
C          + (1/2)C1.w*.w      defocus C1.|w|^2
C          + (1/3)A2.w*^3      3-fold astig (1/3)|A2||w|^3.cos3(p-p33)
C          + (1/3)B2.W*^2.W    axial coma (1/4)|B2||w|^3.cos(p-p31)
C          + (1/4)C3.w*^2.w^2  spherical abn (1/4)C3.|w|^4
C Actually fits Re{},Im{} of these coeffs, using purely real code.
C External units: A0,A1,C1/nm; A2,B2/nm; CS/mm; angles/mrad.
C Internally, uses A0/nm and angles/glbs[=.005]; this requires
C   A1,C1/[nm/glbs]; A2,B2/[nm/glbs^2]; CS/[nm/glbs^3].
C Using glbs ensures a reasonable dynamic range for LS fit matrix.
C
C In complex form, shift s when tilted by w (i.e. A0' less A0 term) is
C   s = A1.w* + C1.w + A2.w*w* + B2*.w^2/3 + 2B2.|w|^2/3 + C3.|w|^2.w
C involving 8 real indep vars A1r,A1i,C1,A2r,A2i,B2r,B2i,C3 (in that
C order).  Effective defocus,astig are
C   C' = C1 + Re{4B2.w*/3} + 2C3|w|^2
C   A' = A1 + 2A2.w* + 2B2.w/3 + C3.w^2
C
C Inner module fits aberrn coeffs only, which is a linear and hence
C determinate process, while outer module uses resulting misfit
C as basis for minimising misalignment and tilt calibn params
C
C Inner module is EMFLF which makes linear LS fit of abn coeffs
C   to data and returns misfit for minimisation by EMFNLM; each
C   coeff can be fixed or varied individually
C Outer module calls EMFNLM, for NonLinMin of misfit reported by
C   EMFLF wrt tilt coil calibration params
C Both of these call several other lower level modules
C ----------------------------------------
C
C
C Global declarations
      INCLUDE 'COMMON'
C
C Local declarations
      INCLUDE 'EMFDCL'
C
      LOGICAL EMFDS2,SEMROW,SEMLU,OPT,OPTNO,SEMKTX,SEMCON,VARSET
      REAL EMFLF,VAL,MX,MY
      REAL D,A,AP,A3,AP3,T,M,MP,SDS,SDD
      REAL D1M,D2M,SSC,TXM,TYM,TSC,TXF,TYF
C
C Tilt matrix elements, and non-linear minimisation search dirns
      REAL V(4),SV(4,4)
C
      CHARACTER STR*6
      INTEGER IVAL,IVALPN,MODEIC(80),MARK,MA2,N,I,J,NITS,NV
      EQUIVALENCE (RB1,MODEIC)
C
C Packed names
      INTEGER NVERIF,NMARK,NMA2,NTSCAL,NSSCAL
      PARAMETER (NVERIF=-3419,NMARK=20858,NMA2=20872)
      PARAMETER (NTSCAL=-764,NSSCAL=31163)
      INTEGER NR,NFROM,NFIT,NFIX
      PARAMETER (NR=28800,NFROM=10335,NFIT=9980,NFIX=9984)
      INTEGER NC1,NA1,NA12,NA2,NA22,NBTILT,NBT2,NCS,NIPS
      PARAMETER (NC1=6040,NA1=2840,NA12=2872,NA2=2880,NA22=2912)
      PARAMETER (NBTILT=4009,NBT2=4032,NCS=5560,NIPS=15059)
      INTEGER NTFMOD,NTHETA,NT,NTCM,NTC2,NTC3,NTC4
      PARAMETER (NTFMOD=-254,NTHETA=-326,NT=-1)
      PARAMETER (NTCM=-134,NTC2=-153,NTC3=-154,NTC4=-155)
      INTEGER NSDS,NSDD
      PARAMETER (NSDS=30579,NSDD=30564)
C
C Establish fitting mode
      DO 20 I=1,7
        FITF(I)=.TRUE.
   20 CONTINUE
      FITF(8)=.FALSE.
C
      N=80
      IF (SEMKTX(NFIX,' ',MODEIC,N,.TRUE.)) RETURN
      IF (N.NE.0) THEN
         CALL SEMCHS(RECORD,MODEIC,N)
         IF (INDEX(RECORD,'C1').NE.0) FITF(3)=.FALSE.
         IF (INDEX(RECORD,'A1').NE.0) THEN
            FITF(1)=.FALSE.
            FITF(2)=.FALSE.
         ENDIF
         IF (INDEX(RECORD,'A2').NE.0) THEN
            FITF(4)=.FALSE.
            FITF(5)=.FALSE.
         ENDIF
         IF (INDEX(RECORD,'BTI').NE.0) THEN
            FITF(6)=.FALSE.
            FITF(7)=.FALSE.
         ENDIF
      ENDIF
      N=80
      IF (SEMKTX(NFIT,' ',MODEIC,N,.TRUE.)) RETURN
      IF (N.NE.0) THEN
         CALL SEMCHS(RECORD,MODEIC,N)
         IF (INDEX(RECORD,'CS').NE.0) FITF(8)=.TRUE.
      ENDIF
C
      TFMODE=IVAL(NTFMOD)
C
C Nothing to fit?
      IF (.NOT.(FITF(1).OR.FITF(2).OR.FITF(3).OR.FITF(4)
     +   .OR.FITF(5).OR.FITF(6).OR.FITF(7).OR.FITF(8))
     +   .AND.(TFMODE.LE.0.OR.TFMODE.GT.4)) THEN
            IDMESS='Nothing to fit'
            GOTO 290
      ENDIF
C
C Pick up initial aberration coefficients (/nm)
      A1R=VAL(NA1)
      A1I=VAL(NA12)
      C1=VAL(NC1)
      A2R=VAL(NA2)
      A2I=VAL(NA22)
      C3=VAL(NCS)*1E6
      IF (FITF(8)) THEN
         IF (C3.LE.0) C3=1E6
      ELSE
         IF (C3.LE.0) THEN
            IDERR=NCS
            ERROR=3
            RETURN
         ENDIF
      ENDIF
      B2R=3*C3*VAL(NBTILT)*1E-3
      B2I=3*C3*VAL(NBT2)*1E-3
C Pick up initial tilt fit parameters
      NV=0
      IF (TFMODE.EQ.1) THEN
         V(1)=VAL(NT)
         IF (V(1).LE.0) V(1)=1.
         NV=1
      ENDIF
      IF (TFMODE.EQ.2) THEN
         V(1)=VAL(NTHETA)
         NV=1
      ENDIF
      IF (TFMODE.EQ.3) THEN
         V(1)=VAL(NT)
         IF (V(1).LE.0) V(1)=1.
         V(2)=VAL(NTHETA)
         NV=2
      ENDIF
      IF (TFMODE.EQ.4) THEN
         IF (VARSET(NTCM)) THEN
            V(1)=VAL(NTCM)
            V(2)=VAL(NTC2)
            V(3)=VAL(NTC3)
            V(4)=VAL(NTC4)
         ELSE
            V(1)=1
            V(2)=0
            V(3)=0
            V(4)=1
         ENDIF
         NV=4
      ENDIF
C
C DEBUG
C      write (record,30) ab
C   30 format (' D-ab: ',3f6.1,5f9.0)
C      if (semcon(record)) return
C      write (record,50) fitf,tfmode,nv
C   50 format (' D-FITF:',8l2,' TFMODE ',i1,' nv ',i1)
C      if (semcon(record)) return
C 
C Check source class
      IF (CLASSN(LP1).NE.NCLPLI) THEN
        ERROR=6
        GOTO 280
      ENDIF
C Check size and determine data mode
      IF (NLAYS(LP1).EQ.4) THEN
         DMODE=1
      ELSE IF (NLAYS(LP1).EQ.5) THEN
         DMODE=2
      ELSE IF (NLAYS(LP1).EQ.7) THEN
         DMODE=3
      ELSE
         ERROR=5
         GOTO 280
      ENDIF
C Too many observations for local buffers?
      IF (NCOLS(LP1).GT.MAXNP) THEN
         WRITE (IDMESS,60) MAXNP
   60    FORMAT ('Too many data: max list length is ',I3)
         GOTO 290
      ENDIF
C
C Load indices and injected tilts
      IF (SEMROW(1,RB1,NFMFP,1,1,LP1)) RETURN
      IF (SEMROW(1,RB2,NFMFP,1,2,LP1)) RETURN
      DO 90 N=1,NCOLS(LP1)
         TX(N)=RB1(N)
         TY(N)=RB2(N)
         TN(N)=N-1
 90   CONTINUE
C Load measurements - 2, 3 or 5 rows
      NPL=NCOLS(LP1)
      IF (DMODE.NE.2) THEN
         IF (SEMROW(1,RB1,NFMFP,1,3,LP1)) RETURN
         IF (SEMROW(1,RB2,NFMFP,1,4,LP1)) RETURN
         DO 91 I=1,NPL
            D1(I)=RB1(I)
            D2(I)=RB2(I)
 91      CONTINUE
      ELSE
         IF (SEMROW(1,RB1,NFMFP,1,3,LP1)) RETURN
         IF (SEMROW(1,RB2,NFMFP,1,4,LP1)) RETURN
         IF (SEMROW(1,RB3,NFMFP,1,5,LP1)) RETURN
         DO 93 I=1,NCOLS(LP1)
            D1(I)=RB2(I)
            D2(I)=RB3(I)
            D3(I)=RB1(I)
 93      CONTINUE
      ENDIF
      IF (DMODE.EQ.3) THEN
         IF (SEMROW(1,RB1,NFMFP,1,5,LP1)) RETURN
         IF (SEMROW(1,RB2,NFMFP,1,6,LP1)) RETURN
         IF (SEMROW(1,RB3,NFMFP,1,7,LP1)) RETURN
         DO 95 I=1,NCOLS(LP1)
            D3(I)=RB2(I)
            D4(I)=RB3(I)
            D5(I)=RB1(I)
 95      CONTINUE
      ENDIF
C
C DEBUG
C      if (semcon('D-Data loaded; now verifying')) return
C      do 88 i=1,npl
C         if (dmode.eq.1) then
C            write (record,80) i,tx(i),ty(i),d1(i),d2(i)
C   80       format (' D: ',i2,' tx,y: ',2f7.2,' data: ',2f8.1)
C         else if (dmode.eq.2) then
C            write (record,70) i,tx(i),ty(i),d1(i),d2(i),d3(i)
C   70       format (' D: ',i2,' tx,y: ',2f7.2,' data: ',3f8.1)
C         else
C            write (record,75) i,tx(i),ty(i),
C     +         d1(i),d2(i),d3(i),d4(i),d5(i)
C 75      format (' D: ',i2,' tx,y: '2f7.2,' data:',5f8.1)
C         endif
C         if (semcon(record)) return
C 88   continue
C
C Weed mmts>1e5 or zero shifts in Shift mode
      N=1
 63   IF (DMODE.EQ.1.AND.TX(N).EQ.0.AND.TY(N).EQ.0
     +    .OR.D1(N).GE.1E5.OR.D2(N).GE.1E5) GOTO 64
      IF (DMODE.GT.1) THEN
         IF (D3(N).GE.1E5) GOTO 64
      ENDIF
      IF (DMODE.EQ.3) THEN
         IF (D4(N).GE.1E5.OR.D5(N).GE.1E5) GOTO 64
      ENDIF
      N=N+1
      GOTO 65
 64   CALL EMFCND(N)
 65   IF (N.LE.NPL) GOTO 63
C
C DEBUG
C      do 89 i=1,npl
C         if (dmode.eq.1) then
C            write (record,80) i,tx(i),ty(i),d1(i),d2(i)
C         else if (dmode.eq.2) then
C            write (record,70) i,tx(i),ty(i),d1(i),d2(i),d3(i)
C         else
C            write (record,75) i,tx(i),ty(i),
C     +         d1(i),d2(i),d3(i),d4(i),d5(i)
C         endif
C         if (semcon(record)) return
C 89   continue
C
C No suitable data?
      IF (NPL.EQ.0) THEN
         IDMESS='No data found'
         GOTO 290
      ENDIF
C
C Note mmt RMS
      RMSS=0
      RMSD=0
      DO 92 I=1,NPL
         IF (DMODE.NE.2) THEN
            RMSS=RMSS+D1(I)**2+D2(I)**2
         ELSE
            RMSD=RMSD+D1(I)**2+D2(I)**2+D3(I)**2
         ENDIF
         IF (DMODE.EQ.3) THEN
            RMSD=RMSD+D3(I)**2+D4(I)**2+D5(I)**2
         ENDIF
 92   CONTINUE
      RMSS=SQRT(RMSS/2/NPL)
      RMSD=SQRT(RMSD/3/NPL)

C Pick up mmt standard deviations
      SDS=VAL(NSDS)
      IF (SDS.LE.0) SDS=RMSS
      IF (DMODE.NE.2) WS=1/SDS**2
      SDD=VAL(NSDD)
      IF (SDD.LE.0) SDD=RMSD
C and scale DP s.d by GLBS
      SDD=SDD*GLBS
      IF (DMODE.NE.1) WD=1/SDD**2
C
C DEBUG
C      write (record,2001) rmss,rmsd,sds,sdd/glbs
C 2001 format ('D-RMSS,RMSD,SDS,SDD: ',4g12.4)
C      if (semcon(record)) return
C      write (record,2002) ws,wd
C 2002 format ('D-ws,wd in main: ',2g12.4)
C      if (semcon(record)) return
C
C Mark observed shifts on display?
C -------------------------------
      IF (DMODE.EQ.2) GOTO 120
      MARK=IVAL(NMARK)
      IF (MARK.LE.0) GOTO 120
C
C Establish max injected tilt
      TXM=0.
      TYM=0.
      DO 100 I=1,NPL
         CALL EMFMT(TX(I),TY(I),MX,MY,V)
         TXM=MAX(TXM,MX)
         TYM=MAX(TYM,MY)
  100 CONTINUE
C Establish any user-specified tilt scaling
      TSC=VAL(NTSCAL)
C Establish max shift
      D1M=0.
      D2M=0.
      DO 110 I=1,NPL
         D1M=MAX(D1M,ABS(D1(I)))
         D2M=MAX(D2M,ABS(D2(I)))
  110 CONTINUE
C Establish any user-specified arrow size scaling
      SSC=VAL(NSSCAL)
C
C Set up partition and disply shift data
      IF (EMFDS2(MARK,TX,TY,TN,V,D1,D2,NPL,
     +   TXM,TYM,TSC,D1M,D2M,SSC,.FALSE.,.FALSE.)) RETURN
C
C End of display marking
C ----------------------
  120 CONTINUE
C
C Convert to internal units
      DO 130 N=1,NPL
C Tilts
         TX(N)=TX(N)*1E-3/GLBS
         TY(N)=TY(N)*1E-3/GLBS
      
C Convert DP data to internal units
C ??shorten code with loop
         IF (DMODE.EQ.2) THEN
            D1(N)=D1(N)*GLBS
            D2(N)=D2(N)*GLBS
            D3(N)=D3(N)*GLBS
         ENDIF
         IF (DMODE.EQ.3) THEN
            D3(N)=D3(N)*GLBS
            D4(N)=D4(N)*GLBS
            D5(N)=D5(N)*GLBS
         ENDIF
  130 CONTINUE
C
C Convert aberration coefficients to internal units
      AB(1)=AB(1)*GLBS
      AB(2)=AB(2)*GLBS
      AB(3)=AB(3)*GLBS
      AB(4)=AB(4)*GLBS**2
      AB(5)=AB(5)*GLBS**2
      AB(6)=AB(6)*GLBS**2
      AB(7)=AB(7)*GLBS**2
      AB(8)=AB(8)*GLBS**3
C DEBUG
C      call emfrms(v)
C      write (record,140) RMC2
C  140 format (' RMChi^2 for initial estimates: ',F8.2)
C      if (semcon(record)) return
C
C Minimise misfit
C ---------------
C Fit abn coeffs with existing matrix
      RMC2=EMFLF(V)
C
C DEBUG
C         write (record,150) RMC2
C  150    format (' After minimisation wrt aberrn coeffs only:  ',g12.4)
C      if (semcon(record)) return
C
C Fit matrix elements if requested
      IF (TFMODE.NE.0) THEN
C Init search dirns
         DO 170 I=1,4
            DO 160 J=1,4
               SV(I,J)=0
  160       CONTINUE
            SV(I,I)=1
  170    CONTINUE
C Call minimisation module
         CALL EMFNLM(V,SV,NV,4,.001,NITS,EMFLF)
      ENDIF
C
C Restore tilt to mrad for verification
      DO 175 N=1,NPL
      TX(N)=TX(N)*GLBS*1E3
      TY(N)=TY(N)*GLBS*1E3
 175  CONTINUE
C
C Verify tilts, data and fitted values
C -----------------------------
C ??improve code with computed subscripts to D1,2,3,4,5??
C
C DEBUG
C      if (semcon(' D-About to verify misfit, Chi etc')) return
C      write (record,2003) sds,sdd,rmc2s,rmc2d,rmc2
C 2003 format (' D-SDS,SDD,RMC2S,RMC2D,RMC2: ',5g12.4)
C      if (semcon(record)) return
C
      IF (OPT(NVERIF)) THEN
         IF (DMODE.NE.2) THEN
            IF (SEMCON(' #     Nom Tilt   Fitted Tilt'//
     +'       Obs Shift      Pred Shift   |Err|'))
     +      RETURN
            DO 205 I=1,NPL
               CALL EMFMT(TX(I),TY(I),TXF,TYF,V)
               WRITE (RECORD,190) INT(TN(I)),TX(I),TY(I),TXF,TYF,
     +            D1(I),D2(I),PD1(I),PD2(I),
     +            SQRT((D1(I)-PD1(I))**2+(D2(I)-PD2(I))**2)
 190           FORMAT (I2,4(1X,F6.2),2(2X,2F7.2),2X,F5.2)
               IF (SEMCON(RECORD)) RETURN
 205        CONTINUE
            WRITE (RECORD,204) RMC2S*SDS,RMC2S,SDS
 204        FORMAT ('Shift misfit: RMS ',F5.2,'nm; RMChi^2 '
     +              ,F6.3,' with SD ',F6.2,'nm')
            IF (SEMCON(RECORD)) RETURN
         ENDIF
         IF (DMODE.EQ.2) THEN
            IF (SEMCON('                              '//
     +'   ---Defocus C1--  ------Astigmatism A1-----')) RETURN
            IF (SEMCON(
     +'       Nominal       Fitted       Obs  Fit |Err|     Obs'//
     +'      Fitted  |Err|')) RETURN
            DO 200 I=1,NPL
               CALL EMFMT(TX(I),TY(I),TXF,TYF,V)
               WRITE (RECORD,180) NINT(TN(I)),TX(I),TY(I),TXF,TYF,
     +            NINT(D3(I)/GLBS),NINT(PD3(I)/GLBS),
     +            NINT(ABS(D3(I)-PD3(I))/GLBS),
     +            NINT(D1(I)/GLBS),NINT(D2(I)/GLBS),
     +            NINT(PD1(I)/GLBS),NINT(PD2(I)/GLBS),
     +            NINT(SQRT((D1(I)-PD1(I))**2+(D2(I)-PD2(I))**2)/GLBS)
  180          FORMAT (I2,4(1X,F6.2),2X,3I5,2X,5I5)
               IF (SEMCON(RECORD)) RETURN
  200       CONTINUE
         ENDIF
         IF (DMODE.EQ.3) THEN
            IF (SEMCON('                             -'//
     +'   ---Defocus C1--  ------Astigmatism A1-----')) RETURN
            IF (SEMCON(
     +' #     Nom Tilt   Fitted Tilt     Obs  Fit |Err|     Obs'//
     +'      Fitted  |Err|')) RETURN
            DO 202 I=1,NPL
               CALL EMFMT(TX(I),TY(I),TXF,TYF,V)
               WRITE (RECORD,182) NINT(TN(I)),TX(I),TY(I),TXF,TYF,
     +            NINT(D5(I)/GLBS),NINT(PD5(I)/GLBS),
     +            NINT(ABS(D5(I)-PD5(I))/GLBS),
     +            NINT(D3(I)/GLBS),NINT(D4(I)/GLBS),
     +            NINT(PD4(I)/GLBS),NINT(PD4(I)/GLBS),
     +            NINT(SQRT((D3(I)-PD3(I))**2+(D4(I)-PD4(I))**2)/GLBS)
 182           FORMAT (I2,4(1X,F6.2),2X,3I5,2X,5I5)
               IF (SEMCON(RECORD)) RETURN
 202        CONTINUE
         ENDIF
C
C Report residual
         IF (DMODE.NE.1) THEN
            WRITE (RECORD,206) RMC2D*SDD/GLBS,RMC2D,SDD/GLBS
 206        FORMAT ('Defoc/Astig misfit: RMS ',F6.1,'nm; RMChi^2 '
     +              ,F6.3,' with SD ',F7.2,'nm')
            IF (SEMCON(RECORD)) RETURN
         ENDIF
         IF (DMODE.EQ.3) THEN
            WRITE (RECORD,208) RMC2
 208        FORMAT ('Combined RMChi^2',F6.3)
            IF (SEMCON(RECORD)) RETURN
         ENDIF
      ENDIF
C RETURN also the two SDSs to allows programmed re-entry??
      IF (SEMLU(1,NR,RMC2)) RETURN
C
C Mark fitted values on display?
C ------------------------------
      IF (DMODE.NE.2) THEN
         IF (MARK.GT.0) THEN
            MA2=IVAL(NMA2)
            IF (MA2.LE.0) MA2=MARK
C Set up partition and make annotations
            IF (EMFDS2(MA2,TX,TY,TN,V,PD1,PD2,NPL,
     +         TXM,TYM,TSC,D1M,D2M,SSC,MA2.EQ.MARK,.TRUE.)) RETURN
         ENDIF
      ENDIF

C DEBUG - verify aberrations without moving to CF axis..
C      if (semcon(' D-Fitted aberrations wrt orig origin')) return
C      write (record,1) a1r/glbs,a1i/glbs,c1/glbs,
C     +   a2r/glbs**2,a2i/glbs**2,b2r/glbs**2,b2i/glbs**2,
C     +   c3/glbs**3
C 1    format (8f10.1)
C      if (semcon(record)) return
C
C Move origin of abn coeffs to coma-free axis
C -------------------------------------------
C Find coma-free axis wrt present origin
      MX=-B2R/3/C3
      MY=-B2I/3/C3
C Adjust abn coeffs
C   A1r' = A1r + 2.A2r.x + 2.A2i.y + (2/3)B2r.x - (2/3)B2i.y + C3.(x^2-y^2)
C   A1i' = A1i + 2.A2i.x - 2.A2r.y + (2/3)B2i.x + (2/3)B2r.y +2*C3.x.y
C   C1'  = C1 + (4/3)B2r.x + (4/3)B2i.y + 2C3(x^2+y^2)
      T=A1R+2*(A2R*MX+A2I*MY)+(2./3.)*(B2R*MX-B2I*MY)
     +     +C3*(MX*MX-MY*MY)
      A1I=A1I+2*(A2I*MX-A2R*MY)+(2./3.)*(B2I*MX+B2R*MY)+2*C3*MX*MY
      A1R=T
      C1=C1+(4./3.)*(B2R*MX+B2I*MY)+2*C3*(MX*MX+MY*MY)
C A2 unchanged; B2 (coma) eliminated
      B2R=0
      B2I=0
C C3 unchanged
C
C Return fitted parameters via vars, restoring normal units
C ------------------------
C (Under)focus
      C1=C1/GLBS
      IF (FITF(3)) THEN
         IF (SEMLU(1,NC1,C1)) RETURN
      ENDIF
C Conventional astigmatism
      A1R=A1R/GLBS
      A1I=A1I/GLBS
      IF (FITF(1)) THEN
         IF (SEMLU(1,NA1,A1R)) RETURN
         IF (SEMLU(1,NA12,A1I)) RETURN
      ENDIF
C 3-fold astigmatism
      A2R=A2R/GLBS**2
      A2I=A2I/GLBS**2
      IF (FITF(4)) THEN
         IF (SEMLU(1,NA2,A2R)) RETURN
         IF (SEMLU(1,NA22,A2I)) RETURN
      ENDIF
C Beamp posn wrt coma-free axis
      MX=-MX*GLBS*1E3
      MY=-MY*GLBS*1E3
      IF (FITF(6)) THEN
         IF (SEMLU(1,NBTILT,MX)) RETURN
         IF (SEMLU(1,NBT2,MY)) RETURN
      ENDIF
C Spherical aberration
      C3=C3/GLBS**3/1E6
      IF (FITF(8)) THEN
         IF (SEMLU(1,NCS,CS)) RETURN
      ENDIF
C Tilt calibration params
      IF (TFMODE.EQ.1) THEN
         IF (SEMLU(1,NT,V(1))) RETURN
      ENDIF
      IF (TFMODE.EQ.2) THEN
         IF (SEMLU(1,NTHETA,V(1))) RETURN
      ENDIF
      IF (TFMODE.EQ.3) THEN
         IF (SEMLU(1,NT,V(1))) RETURN
         IF (SEMLU(1,NTHETA,V(2))) RETURN
      ENDIF
      IF (TFMODE.EQ.4) THEN
         IF (SEMLU(1,NTCM,V(1))) RETURN
         IF (SEMLU(1,NTC2,V(2))) RETURN
         IF (SEMLU(1,NTC3,V(3))) RETURN
         IF (SEMLU(1,NTC4,V(4))) RETURN
      ENDIF
C
C Force IPS 2
      IF (SEMLU(1,NIPS,2.)) RETURN
C
C Report fitted values on terminal
      IF (.NOT.OPTNO(NVERIF)) THEN
         STR='fixed '
         IF (FITF(3)) STR='fitted'
         WRITE (RECORD,220) STR,C1
  220    FORMAT ('      defocus C1 ',A,1X,F7.1,'nm')
         IF (SEMCON(RECORD)) RETURN
C
         STR='fixed '
         IF (FITF(1)) STR='fitted'
         WRITE (RECORD,230) STR,A1R,A1I
  230    FORMAT ('  astigmatism A1 ',A,1X,F6.1,',',F6.1,'nm ')
         IF (SEMCON(RECORD)) RETURN
C
         STR='fixed '
         IF (FITF(4)) STR='fitted'
         WRITE (RECORD,240) STR,A2R,A2I
  240    FORMAT (' 3-fold astig A2 ',A,1X,F6.0,',',F6.0,'nm ')
         IF (SEMCON(RECORD)) RETURN
C
         STR='fixed '
         IF (FITF(6)) STR='fitted'
         WRITE (RECORD,250) STR,MX,MY
  250    FORMAT ('tilt/misalig BTI ',A,1X,F6.2,',',F6.2,'mrad ')
         IF (SEMCON(RECORD)) RETURN
C
         STR='fixed '
         IF (FITF(8)) STR='fitted'
         WRITE (RECORD,270) STR,C3
  270    FORMAT ('     sph abbn CS ',A,1X,F6.2,'mm ')
         IF (SEMCON(RECORD)) RETURN
C
         IF (TFMODE.EQ.1) THEN
            WRITE (RECORD,271) V(1)
 271        FORMAT (' tilt multiplier fitted ',F6.3)
            IF (SEMCON(RECORD)) RETURN
         ELSE IF (TFMODE.EQ.2) THEN
            WRITE (RECORD,272) V(1)
 272        FORMAT ('      tilt angle fitted ', F6.3)
            IF (SEMCON(RECORD)) RETURN
         ELSE IF (TFMODE.EQ.3) THEN
            WRITE (RECORD,273) V(1),V(2)
 273        FORMAT (' tilt mult/angle fitted ',2F6.3)
            IF (SEMCON(RECORD)) RETURN
         ELSE IF (TFMODE.EQ.4) THEN
            WRITE (RECORD,274) V
 274        FORMAT (' tilt calibn TCM fitted (',4F7.3,')')
            IF (SEMCON(RECORD)) RETURN
         ELSE
            WRITE (RECORD,275)
 275        FORMAT ('           tilts fixed')
            IF (SEMCON(RECORD)) RETURN
         ENDIF
      ENDIF
C
C Warn of singularity
      IF (CONDN.GE.1E5) THEN
         IDMESS='Fit singular: min-norm solution used for abn coeffs'
         GOTO 290
      ENDIF
C
      RETURN
C
C Error returns
C
  280 IDERR=IVALPN(NFROM)
      RETURN
  290 ERROR=77
      RETURN
C
      END
C
C Semper 6 subsidiary module EMFCND
C
      SUBROUTINE EMFCND(NIN)
C
C Condenses 5 datasets in parallel omitting element NIN from each
      INCLUDE 'COMMON'
      INCLUDE 'EMFDCL'
      INTEGER NIN,L,N,I

      DO 5 L=1,8
         N=(L-1)*MAXNP+NIN
         DO 10 I=NIN+1,NPL
            TX(N)=TX(N+1)
            N=N+1
 10      CONTINUE
 5    CONTINUE
      NPL=NPL-1
      END
C
C Semper 6 subsidiary module EMFMT
C
      SUBROUTINE EMFMT(XI,YI,XO,YO,V)
C
C Maps tilt XI,YI via tilt calibration matrix to XO,YO
C taking matrix from common
C Local declarations
C
C Global declarations - only for TFMODE
      INCLUDE 'COMMON'
C Local declarations
      INCLUDE 'EMFDCL'
C
C Local declarations
      REAL XI,YI,XO,YO,V(4),T11,T12,T21,T22
C
C Default matrix (identity)
      T11=1
      T12=0
      T21=0
      T22=1
C
C Options
      IF (TFMODE.EQ.1) THEN
C one mag
         T11=V(1)
         T12=0
         T21=0
         T22=V(1)
      ELSE IF (TFMODE.EQ.2) THEN
C one angle
         T11=COS(V(1))
         T12=-SIN(V(1))
         T21=-T12
         T22=T11
      ELSE IF (TFMODE.EQ.3) THEN
C one mag and one angle
         T11=V(1)*COS(V(2))
         T12=-V(1)*SIN(V(2))
         T21=-T12
         T22=T11
      ELSE IF (TFMODE.EQ.4) THEN
C all four matrix elements
         T11=V(1)
         T12=V(2)
         T21=V(3)
         T22=V(4)
      ENDIF
C
C Apply matrix
      XO=T11*XI+T12*YI
      YO=T21*XI+T22*YI
      RETURN
C
      END
C
C Semper 6 subsidiary module EMFDS2
C
      LOGICAL FUNCTION EMFDS2(MARK,TX,TY,TN,V,SX,SY,N,
     +   TXM,TYM,TSC,SXM,SYM,SSC,NOERAS,HALFS)
C
C Marks injected tilt / image shift pattern in partn MARK, with
C scaling TSC display pixels per tilt unit and SSC display pixels
C per shift unit if these vars are non-zero, but otherwise using
C supplied maxima TXM,TYM and SXM,SYM to provide defaults
C (and resetting TSC,SSC accordingly).  Semper vars ERASE [YES]
C and VIEW [YES] honoured; erase also suppressed if NOERAS; only
C origin and outer half of vectors drawn if HALFS (allowing two
C superposed patterns to be distinguishable)
C
C Global declarations
      INCLUDE 'COMMON'
C Local declarations
      LOGICAL SEMDPD,OPTNO,SEMICS
      LOGICAL FSINIT,FSQLIM,FSVIEW,FSERAS,FSMARK,FSARRO,FSTEXT
      LOGICAL NOERAS,HALFS
      INTEGER PSN(2),NC,MARK,M,N,I,ITN
      REAL TX(N),TY(N),TN(N),V(4),SX(N),SY(N)
      REAL TXM,TYM,TSC,SXM,SYM,SSC,X1,X2,Y1,Y2
C Packed names
      INTEGER NVIEW,NERASE
      PARAMETER (NVIEW=-3566,NERASE=8721)
C
C Prepare error return
      EMFDS2=.TRUE.
C
C Set up DPD, mapping tilt range on to partition
      M=MOD(MARK,1000)
      IF (SEMDPD(1,M)) RETURN
      IF (TSC.LE.0.) TSC=.3*MIN(DPSIZ/TXM,DPSI2/TYM)
      IF (SSC.LE.0.) SSC=.2*MIN(DPSIZ/SXM,DPSI2/SYM)
      DPLEF=-(DPSIZ)/2/TSC
      DPRIG=(DPSIZ-1)/2/TSC
      DPTOP=DPSI2/2/TSC
      DPBOT=-(DPSI2-1)/2/TSC
      DPMA=TSC
      DPMB=DPTLX+DPSIZ/2
      DPMA2=-TSC
      DPMB2=DPTLY+DPSI2/2
      DPTYP=2
      DPSRC=0
      IF (SEMDPD(2,M)) RETURN
C
C Initialise partition coords
      IF (FSINIT(3,M)) RETURN
      DISPLA=1000*FS+M
C Force viewing conditions
      IF (.NOT.OPTNO(NVIEW)) THEN
         IF (FSVIEW()) RETURN
      ENDIF
C
C Erase partition
      IF (FSQLIM(X1,X2,Y1,Y2)) RETURN
      IF (.NOT.NOERAS) THEN
         IF (.NOT.OPTNO(NERASE)) THEN
            IF (FSERAS(3,X1,X2,Y1,Y2)) RETURN
         ENDIF
      ENDIF
C
C Mark arrows to represent each tilt/displacement pair
      DO 10 I=1,N
         CALL EMFMT(TX(I),TY(I),X1,Y1,V)
         ITN=TN(I)
C Encode posn serial as IC
         IF (ITN.LT.10) THEN
            PSN(1)=KZERO+ITN
            NC=1
         ELSE
            PSN(1)=KZERO+ITN/10
            PSN(2)=KZERO+ITN-ITN/10*10
            NC=2
         ENDIF
         IF (FSTEXT(PSN,NC,X1,Y1,0,0)) RETURN
         X2=SSC/TSC*SX(I)
         Y2=SSC/TSC*SY(I)
         IF (HALFS) THEN
            IF (FSMARK(X1,Y1,5,0)) RETURN
            IF (FSARRO(X1+.5*X2,Y1+.5*Y2,X1+X2,Y1+Y2)) RETURN
         ELSE
            IF (FSARRO(X1,Y1,X1+X2,Y1+Y2)) RETURN
         ENDIF
   10 CONTINUE
C
C Normal return
      EMFDS2=.FALSE.
      RETURN
C
      END
C
C Module EMFLF finds aberrn coeffs giving LS fit to data
C for various known injected tilts, and returns misfit residual.
C The fit is linear and so non-iterative; the function serves as a
C basis for non-linear minimisation wrt the tilt calibration matrix
C
      REAL FUNCTION EMFLF(V)
C
C If FSH (ie if fitting shifts), the fitting functions are:
C   Sr = A1r.x + A1i.y + C1.x + A2r.(xx-yy) + A2i.2xy
C          + B2r.(xx+yy/3) + B2i.2xy/3 + C3.(xx+yy).x
C   Si = - A1r.y + A1i.x + C1.y - A2r.2xy + A2i.(xx-yy)
C          + B2r.2xy/3 + B2i.(xx/3+yy) + C3.(xx+yy).y
C and if not (ie if fitting DPs):
C   fA1r = A1r + 2*A2r*x + 2*A2i*y + (2./3.)*B2r*x
C          - (2./3.)*B2i*y + C3*(x*x-y*y)
C   fA1i = A1i + 2*A2i*x - 2*A2r*y + (2./3.)*B2i*x
C          + (2./3.)*B2r*y + 2*C3*x*y
C   fC1  = C1 + (4./3.)*B2r*x + (4./3.)*B2i*y + 2*C3*(x*x+y*y)
C for x,y = supplied values of tx,ty after passage through calib
C matrix /v1 v2\, wrt A1r,A1i,C1,A2r,A2i,B2r,B2i,C3 as indep vars
C        \v3 v4/
C
C Global declarations
      INCLUDE 'COMMON'
C
C Local declarations
      INCLUDE 'EMFDCL'
      REAL X,Y
      INTEGER I,J,N,NIN,NFIT
C DEBUG
       logical semcon
C
      REAL V(4)
      REAL A(NABS,NABS),B(NABS),LSFV(NABS)
      REAL CD1(NABS),CD2(NABS),CD3(NABS),CD4(NABS),CD5(NABS)
      INTEGER MAP(NABS)
C
C Construct condensation map, omitting fixed vars
      NFIT=0
      DO 130 NIN=1,8
         IF (FITF(NIN)) THEN
            NFIT=NFIT+1
            MAP(NFIT)=NIN
         ENDIF
  130 CONTINUE
C
C Nothing to fit?
      IF (NFIT.EQ.0) GOTO 195
C
C Zero coeff matrix
      DO 20 I=1,8
        B(I)=0
        DO 10 J=1,8
          A(I,J)=0
   10   CONTINUE
   20 CONTINUE
C
C DEBUG
C      write (record,25) dmode
C 25   format (' D-EMFLF: dmode ',i1)
C      if (semcon(record)) return
C      write (record,30) v
C   30 format ('EMFLF: param vector ',4f9.3)
C      if (semcon(record)) return
C      write (record,40) ab
C   40 format ('   ab: ',8f9.3)
C      if (semcon(record)) return
C 
C DEBUG
C      write (6,1) (tx(i),ty(i),i=1,npl)
C 1    format (' D-EMFLF: tilts in internal units'/(1x,8f8.2))
C      if (dmode.eq.1) then
C         write (6,2) (d1(i),d2(i),i=1,npl)
C 2       format (' D1,D2 in internal units'/(1x,8f8.2))
C      else if (dmode.eq.2) then
C         write (6,3) (d1(i),d2(i),d3(i),i=1,npl)
C 3       format (' D1,2,3 in internal units'/(1x,8f8.2))
C      else
C         write (6,4) (d1(i),d2(i),d3(i),d4(i),d5(i),i=1,npl)
C 4       format (' D1,2,3,4,5 in internal units'/(1x,8f8.2))
C      endif
C      write (record,2001) ws,wd
C 2001 format ('D-EMFLF: ws,wd ',2g12.4)
C      if (semcon(record)) return
C
C
C Accumulate coeff matrix elements, summing over tilts
      DO 70 N=1,NPL
C Injected tilt for tilt n
         CALL EMFMT(TX(N),TY(N),X,Y,V)
C
C List coeffs of the 8 indep vars in exprs fitting data
         IF (DMODE.NE.2) THEN
C IMPROVE CODE LATER VIA ARRAY CD(5,NABS)
            CD1(1)=X
            CD1(2)=Y
            CD1(3)=X
            CD1(4)=(X*X-Y*Y)
            CD1(5)=2*X*Y
            CD1(6)=(X*X+Y*Y/3)
            CD1(7)=2*X*Y/3
            CD1(8)=(X*X+Y*Y)*X
            CD2(1)=-Y
            CD2(2)=X
            CD2(3)=Y
            CD2(4)=-2*X*Y
            CD2(5)=(X*X-Y*Y)
            CD2(6)=2*X*Y/3
            CD2(7)=(X*X/3+Y*Y)
            CD2(8)=(X*X+Y*Y)*Y
         ELSE
            CD1(1)=1
            CD1(2)=0
            CD1(3)=0
            CD1(4)=2*X
            CD1(5)=2*Y
            CD1(6)=(2./3.)*X
            CD1(7)=-(2/3.)*Y
            CD1(8)=(X*X-Y*Y)
            CD2(1)=0
            CD2(2)=1
            CD2(3)=0
            CD2(4)=-2*Y
            CD2(5)=2*X
            CD2(6)=(2./3.)*Y
            CD2(7)=(2./3.)*X
            CD2(8)=2*X*Y
            CD3(1)=0
            CD3(2)=0
            CD3(3)=1
            CD3(4)=0
            CD3(5)=0
            CD3(6)=(4./3.)*X
            CD3(7)=(4./3.)*Y
            CD3(8)=2*(X*X+Y*Y)
         ENDIF
         IF (DMODE.EQ.3) THEN
            CD3(1)=1
            CD3(2)=0
            CD3(3)=0
            CD3(4)=2*X
            CD3(5)=2*Y
            CD3(6)=(2./3.)*X
            CD3(7)=-(2/3.)*Y
            CD3(8)=(X*X-Y*Y)
            CD4(1)=0
            CD4(2)=1
            CD4(3)=0
            CD4(4)=-2*Y
            CD4(5)=2*X
            CD4(6)=(2./3.)*Y
            CD4(7)=(2./3.)*X
            CD4(8)=2*X*Y
            CD5(1)=0
            CD5(2)=0
            CD5(3)=1
            CD5(4)=0
            CD5(5)=0
            CD5(6)=(4./3.)*X
            CD5(7)=(4./3.)*Y
            CD5(8)=2*(X*X+Y*Y)
         ENDIF
C
C Accumulate LS fit coeff matrix and RHS,
C including weighting factor 1/SD
C IMPROVE CODE LATER
         DO 60 I=1,8
            DO 50 J=1,8
               IF (DMODE.EQ.1) THEN
                  A(I,J)=A(I,J)
     +             +( CD1(I)*CD1(J)+CD2(I)*CD2(J) )*WS
               ELSE IF (DMODE.EQ.2) THEN
                  A(I,J)=A(I,J)
     +             +( CD1(I)*CD1(J)+CD2(I)*CD2(J)+CD3(I)*CD3(J) )*WD
               ELSE
                  A(I,J)=A(I,J)
     +             +( CD1(I)*CD1(J)+CD2(I)*CD2(J) )*WS
     +             +( CD3(I)*CD3(J)+CD4(I)*CD4(J)+CD5(I)*CD5(J) )*WD
               ENDIF
   50       CONTINUE
            IF (DMODE.EQ.1) THEN
               B(I)=B(I) + ( CD1(I)*D1(N)+CD2(I)*D2(N) )*WS
            ELSE IF (DMODE.EQ.2) THEN
               B(I)=B(I)
     +          +( CD1(I)*D1(N)+CD2(I)*D2(N)+CD3(I)*D3(N) )*WD
            ELSE
               B(I)=B(I)
     +          +( CD1(I)*D1(N)+CD2(I)*D2(N) )*WS
     +          +( CD3(I)*D3(N)+CD4(I)*D4(N)+CD5(I)*D5(N) )*WD
            ENDIF
   60    CONTINUE
   70 CONTINUE
C
C DEBUG
C      write (6,'('' Uncondensed coefficient matrix:'')')
C      do 90 j=1,8
C         write (6,80) (a(j,i),i=1,8)
C   90 continue
C      write (6,100) (b(i),i=1,8)
C
C Adjust RHS to accommodate fixed variables
      DO 120 N=1,8
      IF (.NOT.FITF(N)) THEN
         DO 110 I=1,8
            B(I)=B(I)-A(I,N)*AB(N)
  110    CONTINUE
      ENDIF
  120 CONTINUE
C
C DEBUG: verify condensation map
C      write (record,150) nfit, (map(i),i=1,nfit)
C  150 format (' Var condensation map length: ',i2,' map: ',8i2)
C      if (semcon(record)) return
C
C Condense matrix and rhs via map
      DO 170 I=1,NFIT
         B(I)=B(MAP(I))
         DO 160 J=1,NFIT
            A(I,J)=A(MAP(I),MAP(J))
  160    CONTINUE
  170 CONTINUE
C
C DEBUG
C      write (6,'(''Coefficient matrix condensed to NFIT='',i1)') nfit
C      do 180 j=1,nfit
C        write (6,80) (a(j,i),i=1,nfit)
C   80    format (1x,4g12.4/5x,4g12.4)
C        if (semcon(record)) return
C  180 continue
C      write (6,100) (b(i),i=1,nfit)
C  100 format (' RHS:'/1x,4g12.4/5x4g12.4)
C      if (semcon(record)) return
C
C Call module to solve minimisation equations
      CALL LQSET(A,NABS,NFIT,B,LSFV,CONDN)
C
C DEBUG
C	if (semcon(' After LQSET: soln vector LSFV:')) return
C	write (6,182) (lsfv(i),i=1,nfit)
C182	format (1x,4g12.4))
C [The SV decompn sometimes fails to converge in 30 cycles; this
C probably doesn't matter - it's trying to converge to machine
C precision]
C
C Undo condensation: unpack fitted variables around fixed vars
      DO 190 N=1,NFIT
         AB(MAP(N))=LSFV(N)
  190 CONTINUE
C
C Find misfit and return it
 195  CALL EMFRMS(V)
      EMFLF=RMC2
C DEBUG
C      write (record,200) EMFLF,V
C  200 format (' EMFLF exitval: ',f10.3,' @V: ',4f10.7)
C      if (semcon(record)) return
C
      RETURN
      END
C
C Subsidiary module EMFRMS constructs residuals
C and misfit measure RMC2
C
      SUBROUTINE EMFRMS(V)
C
C Global declarations
      INCLUDE 'COMMON'
      INCLUDE 'EMFDCL'
C Local declarations
      REAL V(4),CH2S,CH2D,X,Y
      INTEGER N
C DEBUG
C      logical semcon
C
C DEBUG
C      write (record,1) ws,wd
C 1    format ('D-EMFRMS: ws,wd: ',2g12.4)
C      if (semcon(record)) return
C
C Find weighted residuals and summed squares
      CH2S=0
      CH2D=0
      DO 10 N=1,NPL
         CALL EMFMT(TX(N),TY(N),X,Y,V)
         IF (DMODE.NE.2) THEN
            PD1(N) = (A1R*X + A1I*Y + C1*X + A2R*(X*X-Y*Y) + A2I*2*X*Y
     +             + B2R*(X*X+Y*Y/3) + B2I*2*X*Y/3 + C3*(X*X+Y*Y)*X)
            PD2(N) = (- A1R*Y + A1I*X + C1*Y - A2R*2*X*Y + A2I*(X*X-Y*Y)
     +             + B2R*2*X*Y/3 + B2I*(X*X/3+Y*Y) + C3*(X*X+Y*Y)*Y)
            CH2S=CH2S
     +       +((D1(N)-PD1(N))**2+(D2(N)-PD2(N))**2)*WS
         ENDIF
         IF (DMODE.EQ.2) THEN
            PD1(N) = (A1R + 2*A2R*X + 2*A2I*Y + (2./3.)*B2R*X
     +             - (2./3.)*B2I*Y + C3*(X*X-Y*Y))
            PD2(N) = (A1I + 2*A2I*X - 2*A2R*Y + (2./3.)*B2I*X
     +             + (2./3.)*B2R*Y +2*C3*X*Y)
            PD3(N) = (C1 + (4./3.)*B2R*X + (4./3.)*B2I*Y
     +             + 2*C3*(X*X+Y*Y))
            CH2D=CH2D
     +       +((D1(N)-PD1(N))**2+(D2(N)-PD2(N))**2+(D3(N)-PD3(N))**2)*WD
         ENDIF
         IF (DMODE.EQ.3) THEN
            PD3(N) = (A1R + 2*A2R*X + 2*A2I*Y + (2./3.)*B2R*X
     +             - (2./3.)*B2I*Y + C3*(X*X-Y*Y))
            PD4(N) = (A1I + 2*A2I*X - 2*A2R*Y + (2./3.)*B2I*X
     +             + (2./3.)*B2R*Y +2*C3*X*Y)
            PD5(N) = (C1 + (4./3.)*B2R*X + (4./3.)*B2I*Y
     +             + 2*C3*(X*X+Y*Y))
            CH2D=CH2D
     +       +((D3(N)-PD3(N))**2+(D4(N)-PD4(N))**2+(D5(N)-PD5(N))**2)*WD
         ENDIF
C
   10 CONTINUE
C
C Find RMChi^2 in shifts, RMChi^2 in def/asts, combined RMChi^2
      IF (DMODE.NE.2) THEN
         RMC2S=SQRT(CH2S/2/NPL)
         RMC2=RMC2S
      ENDIF
      IF (DMODE.NE.1) THEN
         RMC2D=SQRT(CH2D/3/NPL)
         RMC2=RMC2D
      ENDIF
      IF (DMODE.EQ.3) RMC2=SQRT((CH2S+CH2D)/5/NPL)
C
C DEBUG
C      write (record,20) rmc2,ch2s,ch2d,rmc2s,rmc2d
C   20 format (' EMFRMS: ',f7.2,' ch2s,ch2d,rmc2s,rmc2d',4g12.4)
C      if (semcon(record)) return
C      write (record,30) ab
C   30 format ('     ab: ',8f8.4)
C      if (semcon(record)) return
C
      RETURN
      END
C
C Semper subsidiary module EMFNLM - Non-Lin-Minimisation
C
      SUBROUTINE EMFNLM(P,XI,N,NP,FTOL,ITER,FNC)
C
C Minimises fn FNC(P) wrt vector P(N) by Powell's method: successive
C line searches are made in each of N direction, initially the basis
C vectors
C
      EXTERNAL FNC
      INTEGER NMAX,NP
      PARAMETER (NMAX=20)
      REAL P(NP),XI(NP,NP),PT(NMAX),PTT(NMAX),XIT(NMAX),FTOL,FNC
      REAL FRET,FP,DEL,FPTT,T
      INTEGER I,J,N,ITER,IBIG
C
C DEBUG
C      character*80 record
C      logical semcon
C
      FRET=FNC(P)
      DO 10 J=1,N
        PT(J)=P(J)
   10 CONTINUE
      ITER=0
   20 ITER=ITER+1
      FP=FRET
      IBIG=0
      DEL=0
C
C DEBUG
C      write (record,30) iter
C   30 format (' EMFNLM beginning iteration ',i4)
C      if (semcon(record)) return
C
C Begin loop over search directions
      DO 60 I=1,N
         DO 40 J=1,N
            XIT(J)=XI(J,I)
   40    CONTINUE
C DEBUG
C      write (record,50) i
C   50 format (' ------: line search in direction ',i1)
C      if (semcon(record)) return
C
         CALL EMFLMN(FNC,P,XIT,N,FRET)
C DEBUG
C      write (record,71) p
C      if (semcon(record)) return
         IF (ABS(FP-FRET).GT.DEL) THEN
            DEL=ABS(FP-FRET)
            IBIG=I
         ENDIF
   60 CONTINUE
C End loop over search directions
C
C DEBUG: verify progress
C      write (record,70) iter,fret
C   70 format (
C     +   ' After cycle ',i2,' of line searches, misfit ',g12.4)
C      if (semcon(record)) return
C      write (record,71) p
C   71 format (' ... params: ',4f10.6)
C      if (semcon(record)) return
C
C Convergence test: exit if fn val has changed by less than a fraction
C FTOL during search cycle
      IF (2*ABS(FP-FRET).LE.FTOL*(ABS(FP)+ABS(FRET))) RETURN
      IF(ITER.EQ.200) RETURN
C
C  Update search directions
      DO 80 J=1,N
         PTT(J)=2*P(J)-PT(J)
         XIT(J)=P(J)-PT(J)
         PT(J)=P(J)
   80 CONTINUE
      FPTT=FNC(PTT)
      IF (FPTT.GE.FP) GOTO 20
      T=2.*(FP-2.*FRET+FPTT)*(FP-FRET-DEL)**2-DEL*(FP-FPTT)**2
      IF (T.GE.0.) GOTO 20
      CALL EMFLMN(FNC,P,XIT,N,FRET)
      DO 90 J=1,N
         XI(J,IBIG)=XIT(J)
   90 CONTINUE
      GOTO 20
      END
C
C Semper subsidiary module EMFLMN - Line-MiN
C
      SUBROUTINE EMFLMN(FNC,P,XI,N,FRET)
C
C Finds min of fn FNC(P+x.XI) along dirn XI(N) from P(N);
C replaces P(N) by min and XI(N) by vector moved;
C returns min value in FRET.
C
      EXTERNAL FNC
      INTEGER N,J
      REAL FNC,FRET,P(N),XI(N),AX,BX,XX,FA,FB,FX,XMIN
      REAL EMFPMS
C DEBUG
C	logical ldum,semcon
C
      AX=0
      XX=.1
      BX=.2
C
C Bracket min in dirn XI from P
      CALL EMFBLM(FNC,P,XI,N,AX,XX,BX,FA,FX,FB)
C
C DEBUG
C	ldum=semcon('...min bracketed')
C
C Locate min by fitting quadratics
      FRET=EMFPMS(FNC,P,XI,N,AX,XX,BX,.01,XMIN)
C
C Return position increment and new position
      DO 10 J=1,N
         XI(J)=XMIN*XI(J)
         P(J)=P(J)+XI(J)
   10 CONTINUE
      RETURN
      END
C
C Semper subsidary module EMFPMS - Parabolic-Min-Search
C
      FUNCTION EMFPMS(FNC,XP,XI,N,AX,BX,CX,TOL,XMIN)
C
C Finds min bracketed by AX,BX,CX of FNC(P+x.XI) on line P(N)+X.XI(N)
C with fract.tol. TOL; returns min fn val in EMFPMS, and posn in XMIN;
C fits parabolas to sets of three points.
C
      EXTERNAL FNC
      INTEGER ITMAX
      REAL CGOLD,ZEPS
      PARAMETER (ITMAX=100,CGOLD=.3819660,ZEPS=1.0E-10)
      INTEGER N,ITER
      REAL EMFPMS,FNC,EMFMDF,XP(N),XI(N),AX,BX,CX,TOL,TOL1,TOL2,XMIN
      REAL A,B,D,E,ETEMP,P,Q,R,U,V,X,W,FU,FV,FW,FX,XM
      A=MIN(AX,CX)
      B=MAX(AX,CX)
      V=BX
      W=V
      X=V
      E=0.
      FX=EMFMDF(X,FNC,XP,XI,N)
      FV=FX
      FW=FX
      DO 30 ITER=1,ITMAX
         XM=0.5*(A+B)
         TOL1=TOL*ABS(X)+ZEPS
         TOL2=2.*TOL1
         IF (ABS(X-XM).LE.TOL2-.5*(B-A)) GOTO 40
         IF (ABS(E).GT.TOL1) THEN
            R=(X-W)*(FX-FV)
            Q=(X-V)*(FX-FW)
            P=(X-V)*Q-(X-W)*R
            Q=2.*(Q-R)
            IF (Q.GT.0.) P=-P
            Q=ABS(Q)
            ETEMP=E
            E=D
            IF (ABS(P).GE.ABS(.5*Q*ETEMP).OR.P.LE.Q*(A-X).OR.
     +         P.GE.Q*(B-X)) GOTO 10
            D=P/Q
            U=X+D
            IF (U-A.LT.TOL2 .OR. B-U.LT.TOL2) D=SIGN(TOL1,XM-X)
            GOTO 20
         ENDIF
   10    IF (X.GE.XM) THEN
            E=A-X
         ELSE
            E=B-X
         ENDIF
            D=CGOLD*E
   20       IF (ABS(D).GE.TOL1) THEN
            U=X+D
         ELSE
            U=X+SIGN(TOL1,D)
         ENDIF
         FU=EMFMDF(U,FNC,XP,XI,N)
         IF (FU.LE.FX) THEN
            IF (U.GE.X) THEN
               A=X
            ELSE
               B=X
            ENDIF
            V=W
            FV=FW
            W=X
            FW=FX
            X=U
            FX=FU
         ELSE
            IF (U.LT.X) THEN
               A=U
            ELSE
               B=U
            ENDIF
            IF (FU.LE.FW .OR. W.EQ.X) THEN
               V=W
               FV=FW
               W=U
               FW=FU
            ELSE IF (FU.LE.FV .OR. V.EQ.X .OR. V.EQ.W) THEN
               V=U
               FV=FU
            ENDIF
         ENDIF
   30 CONTINUE
C
C If we get here, we have failed to converge in requested number of
C iterations; just ignore the problem
   40 XMIN=X
      EMFPMS=FX
      RETURN
      END
C
C Semper subsididary module EMFBLM - Bracket-Line-Min
C
      SUBROUTINE EMFBLM(FNC,P,XI,N,AX,BX,CX,FA,FB,FC)
C
C Searches along line P(N)+X.XI(N) and returns 3 points AX,BX,CX
C that bracket a min of FNC(P+X.XI), returning fn vals as FA,FB,FC
C
      EXTERNAL FNC
      REAL GOLD,GLIMIT,TINY
      PARAMETER (GOLD=1.618034, GLIMIT=100., TINY=1.E-20)
      INTEGER N
      REAL FNC,EMFMDF,P(N),XI(N),AX,BX,CX,FA,FB,FC
      REAL Q,R,U,ULIM,FU,DUM
      FA=EMFMDF(AX,FNC,P,XI,N)
      FB=EMFMDF(BX,FNC,P,XI,N)
      IF (FB.GT.FA) THEN
         DUM=AX
         AX=BX
         BX=DUM
         DUM=FB
         FB=FA
         FA=DUM
      ENDIF
C
      CX=BX+GOLD*(BX-AX)
      FC=EMFMDF(CX,FNC,P,XI,N)
   10 IF (FB.GE.FC) THEN
         R=(BX-AX)*(FB-FC)
         Q=(BX-CX)*(FB-FA)
         U=BX-((BX-CX)*Q-(BX-AX)*R)/(2.*SIGN(MAX(ABS(Q-R),TINY),Q-R))
         ULIM=BX+GLIMIT*(CX-BX)
         IF ((BX-U)*(U-CX).GT.0.) THEN
            FU=EMFMDF(U,FNC,P,XI,N)
            IF (FU.LT.FC) THEN
               AX=BX
               FA=FB
               BX=U
               FB=FU
               GOTO 10
            ELSE IF (FU.GT.FB) THEN
               CX=U
               FC=FU
               GOTO 10
            ENDIF
            U=CX+GOLD*(CX-BX)
            FU=EMFMDF(U,FNC,P,XI,N)
         ELSE IF ((CX-U)*(U-ULIM).GT.0.) THEN
            FU=EMFMDF(U,FNC,P,XI,N)
            IF (FU.LT.FC) THEN
               BX=CX
               CX=U
               U=CX+GOLD*(CX-BX)
               FB=FC
               FC=FU
               FU=EMFMDF(U,FNC,P,XI,N)
            ENDIF
         ELSE IF ((U-ULIM)*(ULIM-CX).GE.0.) THEN
            U=ULIM
            FU=EMFMDF(U,FNC,P,XI,N)
         ELSE
            U=CX+GOLD*(CX-BX)
            FU=EMFMDF(U,FNC,P,XI,N)
         ENDIF
         AX=BX
         BX=CX
         CX=U
         FA=FB
         FB=FC
         FC=FU
         GOTO 10
      ENDIF
      RETURN
      END
C
C Semper subsidiary module EMFMDF - Multi-Dim-Func
C
      FUNCTION EMFMDF(X,FNC,P,XI,N)
C
C Simply constructs vector P+x.XI, for vectors P(N) and XI(N),
C and returns value of FNC at that posn
C
      EXTERNAL FNC
      INTEGER N,I
C Max dim 20
      REAL FNC,EMFMDF,P(N),XI(N),V(20),X
C
      DO 10 I=1,N
         V(I)=P(I)+X*XI(I)
   10 CONTINUE
      EMFMDF=FNC(V)
      RETURN
      END
