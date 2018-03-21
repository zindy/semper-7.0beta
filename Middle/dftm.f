C Semper 6 subsidiary module DFTM
C
      LOGICAL FUNCTION DFTM(SIGN,LPI,LPO,LPW,N,HPL,MSQ,LN,ZEROC,RN,
     +                      CBS,WCOL,WROW,MAP,RBSP,CBSIZ)
C
      INTEGER SIGN,LPI,LPO,LPW,N(2),MAP(0:*)
      LOGICAL HPL,MSQ,LN,ZEROC
      REAL    RN,CBS(2,0:*),WCOL(2,0:*),WROW(2,0:*)
      INTEGER*4 RBSP,CBSIZ
C
C 2-D non-square, multi-radix (1,2,3,4,5) complex FT routine,
C with central origin in both planes
C
C [in] integer SIGN: xform direction
C         -1: perform 'forward' xform - factors exp{-2.pi.i..}
C         +1:         'inverse'                     +
C [in] integer LPI,LPO,LPW: input,output and workspace lpns; usage
C         as noted in FT2D,INVFT2
C [in] integer N(2): xform dimensions
C [in] logical HPL: flags real image (only for passing to DFTRD)
C [in] logical MSQ: take mod squared on completion if fwd
C         or at outset if inv
C [in] logical LN: requests ln mod squared...; ignored unless MSQ
C [in] logical ZEROC: zero central point of xform at outset if inv
C [in,out] real RN: normalisation factor applied if inv; usage as
C         noted in INVFT2; reset to value used if initially -1.
C [in] complex CBS: array to hold at least 6 row buffers of size RBSP
C                   (complex) - actual space given by CBSIZ
C [in] complex WCOL: array to hold X twiddle factors
C [in] complex WROW: array to hold Y twiddle factors
C [in] integer MAP: array to hold X re-ordering table
C [in] integer*4 RBSP: row buffer size (complex)
C [in} integer*4 CBSIZ: size of array CBS
C
C Passes operate on LPW, with first pass input from LPI, inserting
C reordering and normalisation, and final pass output to LPO
C
C Notes on approach taken in code:
C - xform passes follow initial reordering, regardless of xform dirn
C - col/row xforms interleaved
C - twiddles applied separately to each dirn (COULD be changed)
C - origin centring in both planes effected by initial shift / grid
C - recursive management by top level routine doing all i/o, with
C   rows packed into rows buffers and calling lower level routine DFTM1
C   which applies 2-D code to memory buffs only, so that:
C   (a) 2-D transforms as large as poss are done entirely in row buffers
C   (b) max disc access localisation achieved regardless of cache size
C - code uses sign +1 for both directions, but inserts initial X and Y
C   reflection on fwd transforms to effect sign -1 xforms
C - reordering, shift/grid, X/Y reversal embedded in DFTRD first pass
C - normalisation embedded in DFTWR last pass
C - all packing / unpacking / extra base 2 pass for real image is
C   external to this module, in DFTXB2
C
C Storage summary:
C - twiddle factors for the 2 dirns are placed in arrays WCOL and WROW
C   routine needs at least 5 row buffers (to accommodate factors of 5
C   in xform)
C - the Y-reordering map is held in array MAP
C - Semper's row buffer space is equivalenced to array CBS
C - RB1,RB2 used directly by DFTXB2
C
C Note on twiddle factor storage:
C The max element needed is not trivial to establish, being the max
C over all xform passes of [base-1].[inlen-1].n/olen; the max is NOT
C nec in the last pass.  An upper bound however is a fraction [b-1]/b
C of n for xforms with largest base b, i.e. 4n/5 here. [Unproved, but
C extensively tested]
C
C Local declarations
C
      LOGICAL SEMDIA,SEMFAC,DFTRD,DFTWR,DFTM1
C
      REAL    PH,SCF
      LOGICAL FWD,LAST
      INTEGER NP(2),NB(2),INLEN,OLEN(2),NBMAX,FNP,ICALL
      INTEGER DIR,P,PB,R,ROWN,BASE(9,2),SPTR,OFFS,FN
      INTEGER*4 BOFFS,NRSINB,O4LEN,N4(2)
C
      INTEGER*4 N41,N42,N43
      PARAMETER ( N41=1, N42=2, N43=3 )
C
C Global declarations
C
      INCLUDE 'COMMON'
C
C Stack for recursion within this routine: allows 10 levels
C
      INTEGER SMAX
      PARAMETER (SMAX=10)
C
      INTEGER STACK(SMAX,4)
C
      DFTM=.TRUE.
C
      FWD=SIGN.LT.0
C
C ----------------------------
C Phase I: Initialisation code
C ----------------------------
C NB: this phase could be broken off, if base,nb,map SAVEd
C
C Factorise X size
C
      DUMLOG=SEMFAC(N(1),BASE(1,1),NB(1))
C
C Tabulate twiddle factors: WCOL(P) contains exp(2.pi.i.p/n(1))
C
      SCF=TWOPI/REAL(N(1))
      DO 10 P=0,4*N(1)/5-1
         PH=SCF*REAL(P)
         WCOL(1,P)=COS(PH)
         WCOL(2,P)=SIN(PH)
   10 CONTINUE
C
C Factorise Y size
C
      DUMLOG=SEMFAC(N(2),BASE(1,2),NB(2))
C
C Tabulate twiddle factors: WROW(P) contains exp(2.pi.i.p/n(2))
C
      SCF=TWOPI/REAL(N(2))
      DO 20 P=0,4*N(2)/5-1
         PH=SCF*REAL(P)
         WROW(1,P)=COS(PH)
         WROW(2,P)=SIN(PH)
   20 CONTINUE
C
C Note max number of factors ( => number of transform passes)
C
      IF (NB(1).GT.NB(2)) THEN
         DIR=1
      ELSE
         DIR=2
      ENDIF
C
      NBMAX=NB(DIR)
C
C Shift shorter dirn to far end, and pre-pad with 1s
C
      R=3-DIR
      OFFS=NBMAX-NB(R)
      IF (OFFS.NE.0) THEN
         DO 30 FN=NB(R),1,-1
            BASE(FN+OFFS,R)=BASE(FN,R)
   30    CONTINUE
C
         DO 40 FN=1,OFFS
            BASE(FN,R)=1
   40    CONTINUE
      ENDIF
C
C Initialise DFTRD row reordering map
C
      IF (DFTRD(0,CBS,CBS,0,0,N,FWD,MAP,BASE,NB,HPL,.FALSE.,
     +          .FALSE.,.FALSE.)) GOTO 160
C
C ------------------------
C Phase II: transform code
C ------------------------
C
C Number of rows fitting row buffer array CBS (less one for data input)
C Guaranteed by calling routine to be at least 5
C
C     NRSINB=CBSIZ/RBSP-1
C
      NRSINB=CBSIZ/RBSP-N41
C
C Stack pointer initialisation
C
      SPTR=0
C
C Output lengths for final transform pass in each dirn
C
      OLEN(1)=N(1)
      OLEN(2)=N(2)
C
      N4(1)=N(1)
      N4(2)=N(2)
C
C Row number offset
C
      OFFS=0
C
C Column transform pass number (factor number)
C
      FN=NBMAX
C
C [Recursive] entry point for length n
C
   50 CONTINUE
C
C Compare col xform length with number of rows fitting buffer
C
C     IF (OLEN(2).GT.NRSINB) GOTO 90
C
      O4LEN=OLEN(2)
      IF (O4LEN.GT.NRSINB) GOTO 90
C
C Case I: rows fit buffers: all-in-memory transform
C -------------------------------------------------
C
C Read all relevant rows to local buffers
C
      BOFFS=RBSP
      DO 60 R=OFFS,OFFS+OLEN(2)-1
         IF (DFTRD(1,CBS,CBS(1,BOFFS),R,LPI,N,FWD,MAP,BASE,NB,
     +             HPL,.TRUE.,MSQ,ZEROC)) GOTO 160
         BOFFS=BOFFS+RBSP
   60 CONTINUE
C
C Apply xform code for levels 1:fn twiddle/butterfly combination
C
      NP(1)=N(1)
      NP(2)=OLEN(2)
      OLEN(1)=1
      OLEN(2)=1
C
      DO 70 FNP=1,FN
         OLEN(1)=OLEN(1)*BASE(FNP,1)
         OLEN(2)=OLEN(2)*BASE(FNP,2)
         IF (DFTM1(CBS(1,RBSP),WCOL,WROW,N,NP,OLEN,RBSP,FNP,BASE,
     +             .FALSE.,0)) GOTO 160
   70 CONTINUE
C
C See if this is final pass
C
      LAST=FN.EQ.NBMAX
C
C If RN<0,last,inv trap central pixel for use as norm constant
C
      IF (RN.LT.0..AND.LAST.AND..NOT.FWD) THEN
C
C        RN=1.0/CBS(1,RBSP*(1+N(2)/2)+N(1)/2)
C
         RN=1.0/CBS(1,RBSP*(N41+N4(2)/N42)+N4(1)/N42)
      ENDIF
C
C Return rows to store
C
      BOFFS=RBSP
      DO 80 R=OFFS,OFFS+OLEN(2)-1
         IF (DFTWR(CBS(1,BOFFS),R,LPO,LPW,HPL,FWD,LAST,RN,MSQ,LN))
     +      GOTO 160
         BOFFS=BOFFS+RBSP
   80 CONTINUE
C
C Quit this level
C
      GOTO 150
C
C Case II: rows do not fit buffers
C --------------------------------
C
C Effect xform by twiddle/butterfly combination of xforms at next
C level down, calling present code recursively to effect those xforms
C
C Sub-level xforms (1,2..base): loop written out longhand to
C permit recursive return jumps back into middle
C
   90 ICALL=1
C
  100 CONTINUE
C
C Recursive call to xform code
C Stack parameter locals
C
      SPTR=SPTR+1
      IF (SPTR.GT.SMAX) GOTO 170
C
      STACK(SPTR,1)=OLEN(1)
      STACK(SPTR,2)=OLEN(2)
      STACK(SPTR,3)=OFFS
      STACK(SPTR,4)=ICALL
C
C Set new parameter values
C
      OLEN(1)=OLEN(1)/BASE(FN,1)
      OLEN(2)=OLEN(2)/BASE(FN,2)
      OFFS=OFFS+(ICALL-1)*OLEN(2)
C
C Track level
C
      FN=FN-1
C
C Re-enter code
C
      GOTO 50
C
  110 CONTINUE
C
C End of loop over sub-div xforms
C
      ICALL=ICALL+1
      IF (ICALL.LE.BASE(FN,2)) GOTO 100
C
C Twiddle/butterfly combination of the resulting xforms
C
C Loop over butterflies
C
      INLEN=OLEN(2)/BASE(FN,2)
C
      DO 140 PB=0,INLEN-1
C
C Read rows - 2,3,4,5 acc to col base
C
         BOFFS=RBSP
         DO 120 R=0,BASE(FN,2)-1
            ROWN=OFFS+PB+R*INLEN
            IF (DFTRD(1,CBS,CBS(1,BOFFS),ROWN,LPW,N,FWD,MAP,BASE,NB,
     +                HPL,.FALSE.,MSQ,ZEROC)) GOTO 160
            BOFFS=BOFFS+RBSP
  120    CONTINUE
C
C Apply twiddle/butterfly code
C
         NP(1)=N(1)
         NP(2)=BASE(FN,2)
         IF (DFTM1(CBS(1,RBSP),WCOL,WROW,N,NP,OLEN,RBSP,FN,BASE,
     +             .TRUE.,PB)) GOTO 160
C
C See if this is final pass
C
         LAST=FN.EQ.NBMAX
C
C If RN<0,last,inv trap central pixel for use as norm constant
C
         IF (RN.LT.0..AND.LAST.AND..NOT.FWD.AND.PB.EQ.0) THEN
C
C           RN=1.0/CBS(1,RBSP*3+N(1)/2)
C
            RN=1.0/CBS(1,RBSP*N43+N4(1)/N42)
         ENDIF
C
C Return rows to store
C
         BOFFS=RBSP
         DO 130 R=0,BASE(FN,2)-1
            ROWN=OFFS+PB+R*INLEN
            IF (DFTWR(CBS(1,BOFFS),ROWN,LPO,LPW,HPL,FWD,LAST,RN,MSQ,LN))
     +         GOTO 160
            BOFFS=BOFFS+RBSP
  130    CONTINUE
C
C End of butterfly loop
C
  140 CONTINUE
C
C Recursive return code
C ---------------------
C Restore locals from stack
C
  150 IF (FN.NE.NBMAX) THEN
         OLEN(1)=STACK(SPTR,1)
         OLEN(2)=STACK(SPTR,2)
         OFFS   =STACK(SPTR,3)
         ICALL  =STACK(SPTR,4)
C
         SPTR=SPTR-1
C
C Track level
C
         FN=FN+1
C
C Return to calling point
C
         GOTO 110
      ENDIF
C
      DFTM=.FALSE.
C
  160 RETURN
C
C Error: stack overflow
C
  170 DUMLOG=SEMDIA('Stack overflow in DFTM: should not happen',NDIWAR)
      DUMLOG=SEMDIA('- too many factors in xform row length',NDIWAR)
      GOTO 160
C
C Copyright (C) 1992-1995:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module DFTXB2
C
C Intercoverts NxM complex xform and right half-plane of corresponding
C   2NxM real image xform
C
      LOGICAL FUNCTION DFTXB2(SIGN,LPI,LPO,N,MSQ,LN,ZEROC)
C
      INTEGER SIGN,LPI,LPO,N(2)
      LOGICAL MSQ,LN,ZEROC
C
C [in] integer SIGN: xform sign, as in DFTM
C         if -1, cvt complex xform LPI to half-plane LPO
C            +1, invert operation, from half-plane LPI to LPO
C [in] integer LPI,LPO: input,output lpns;
C         rows are o/p in dig.rev order if inv
C [in] integer N(2): cmplx xform size
C [in] logical MSQ: take mod squared on completion if fwd,
C         or at outset if inv
C [in] logical LN: take ln mod squared...
C [in] logical ZEROC: zero origin at outset if inv
C
C NB: LPI and LPO may be identical
C     Uses RB1,RB2
C     Older 1-D module did not correct normalisation on inv,
C       so returning double the value this does
C
C Procedure for fwd xform is:
C   cs  = (f + f_*)/2    | extract the two real image NxM xforms
C   cas = (f - f_*)/2i   |   (c-s,c-as parts)
C   f   =  cs + cas.W    | make 2N-pt xform from N-pt xforms
C   f_  = [cs - cas.W]*  |   (as it conveniently happens)
C                        | W = exp{2.pi.i.sign.j/N}V
C       N
C  ------------      2-D complex NxM fft;
C |            |     origin at left marked 0;
C |--f---------|     f and f_ are conjugates
C 0            | M
C |--------f_--|
C |            |
C  ------------
C
C Local declarations
C
      INTEGER MRDGRV
      LOGICAL SEMROW,SEMFAC,DFTWR
C
      REAL C1R,C1I,C2R,C2I,CSR,CSI,CASR,CASI,CWR,CWI,WR,WI,DWR,DWI,D
      INTEGER RVBASE(9),NF,R1,R2,I,J,ROWN1,ROWN2
      LOGICAL FWD
C
C Global declarations
C
      INCLUDE 'COMMON'
C
      REAL CB1(0:LNBUF/LNREAL-1)
      REAL CB2(0:LNBUF/LNREAL-1)
C
      EQUIVALENCE (CB1,RB1),(CB2,RB2)
C
      DFTXB2=.TRUE.
C
      FWD=SIGN.LT.0
C
C Prepare reversed y-size base factor list
C
      IF (SEMFAC(N(2),RVBASE,NF)) GOTO 40
C
      DO 10 I=1,NF/2
         J=RVBASE(NF+1-I)
         RVBASE(NF+1-I)=RVBASE(I)
         RVBASE(I)=J
   10 CONTINUE
C
C Twiddle increment factor
C
      D=REAL(SIGN)*PI/REAL(N(1))
      DWR=COS(D)
      DWI=SIN(D)
C
C Loop over row pairs
C
      DO 30 ROWN1=0,N(2)/2
         ROWN2=MOD(N(2)-ROWN1,N(2))
C
C Fetch two rows
C
         IF (SEMROW(1,CB1,NFMCOM,ROWN1+1,1,LPI)) GOTO 40
         IF (SEMROW(1,CB2,NFMCOM,ROWN2+1,1,LPI)) GOTO 40
C
C Take mod squared?
C
         IF (MSQ.AND..NOT.FWD) THEN
            CALL FTLMSQ(CB1,N(1)+1,.FALSE.)
            CALL FTLMSQ(CB2,N(1)+1,.FALSE.)
         ENDIF
C
C Zero centre?
C
         IF (ZEROC.AND..NOT.FWD) THEN
            IF (ROWN1.EQ.N(2)/2) THEN
               CB1(0)=0.0
               CB1(1)=0.0
               CB2(0)=0.0
               CB2(1)=0.0
            ENDIF
         ENDIF
C
C Duplicate first col before fwd xform
C
         IF (FWD) THEN
            CB1(2*N(1)  )=CB1(0)
            CB1(2*N(1)+1)=CB1(1)
            CB2(2*N(1)  )=CB2(0)
            CB2(2*N(1)+1)=CB2(1)
         ENDIF
C
C Initial twiddle factor = -sign/i = sign.i
C (includes 1/i for conj-antisymm part extraction)
C
         WR=0.0
         WI=REAL(SIGN)
C
         DO 20 I=0,N(1)
C
C Conj-symm pixel pointer
C
            J=N(1)-I
C
C Extract conj-symm,antisymm parts of f,f_, with twiddle on latter
C
            C1R=CB1(2*I)
            C1I=CB1(2*I+1)
C
            C2R=CB2(2*J)
            C2I=CB2(2*J+1)
C
            CSR=0.5*(C1R-C2R)
            CSI=0.5*(C1I+C2I)
C
            CASR=CSR*WR-CSI*WI
            CASI=CSR*WI+CSI*WR
C
            CSR=CSR+C2R
            CSI=CSI-C2I
C
C Sum to first point f
C
            CB1(2*I  )=CSR+CASR
            CB1(2*I+1)=CSI+CASI
C
C Conj diff to second point f_
C
            CB2(2*J  )=CSR-CASR
            CB2(2*J+1)=CASI-CSI
C
C Recurse twiddle factor
C
            CWR=WR
            CWI=WI
C
            WR=CWR*DWR-CWI*DWI
            WI=CWR*DWI+CWI*DWR
   20    CONTINUE
C
C Return rows to store (reordered posns if inv)
C
         IF (FWD) THEN
            R1=ROWN1
            R2=ROWN2
         ELSE
            R1=MRDGRV(MOD(ROWN1+N(2)/2,N(2)),RVBASE,NF)
            R2=MRDGRV(MOD(ROWN2+N(2)/2,N(2)),RVBASE,NF)
         ENDIF
C
         IF (DFTWR(CB1,R1,LPO,LPO,.TRUE.,FWD,FWD,0.0,MSQ,LN)) GOTO 40
         IF (DFTWR(CB2,R2,LPO,LPO,.TRUE.,FWD,FWD,0.0,MSQ,LN)) GOTO 40
   30 CONTINUE
C
      DFTXB2=.FALSE.
C
   40 RETURN
C
C Copyright (C) 1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module DFTRD
C
      LOGICAL FUNCTION DFTRD (OPC,CB1,CB,ROWN,LPN,N,FWD,MAP,
     +                        BASE,NB,HPL,FIRST,MSQ,ZEROC)
C
      INTEGER OPC,ROWN,LPN,N(2),MAP(0:*),BASE(9,2),NB(2)
      REAL    CB1(0:*),CB(0:*)
      LOGICAL FWD,HPL,FIRST,MSQ,ZEROC
C
C Row input module for DFTM, including Y-reordering and all
C adjustments necessary for origins, etc.
C
C [in] integer OPC: opcode - see below
C [in] complex CB1: input buffer array
C [out] complex CB: destination buffer for row data
C [in] integer ROWN: row number, rel to 0
C [in] integer LPN: source picture lpn
C [in] integer N: ......why?? what??
C [in] logical FWD: flags forward (sign=-1) xform
C [in] integer MAP: reordering map
C [in] integer BASE(9,2): xform x,y-size factors; for MRDGRV
C [in] integer NB(2): number of non-trivial x,y-factors; for MRDGRV
C [in] logical HPL: flags real image
C [in] logical FIRST: flags first xform pass
C [in] logical MSQ: take mod squared of data if...
C
C OPC=0 initialises mapping array MAP given N,BASE,HPL
C OPC=1 reads source row ROWN from LPN to CB;
C       if FIRST, reads via CB1 with extra processing:
C       - wrt Y: reversal; grid; origin shift; dig.rev
C            but if hpl-inv: grid only - dftxb2 reorders on o/p
C       - wrt X: if fwd: reversal; grid if cmplx;shift;dig.rev
C                if inv: reversal; grid; shift if cmplx; dig.rev
C If msq-inv, takes mod squared of data after first input
C If zeroc-inv, zeros central pixel at outset
C
C Notes on reordering:
C (a) making initial plane origin central is effected directly
C (b) final plane origin shift is effected via 2-D sign change grid
C     in initial plane, radiating from centre [??] in both dirns
C (c) real images need X-origin shift in real space only, achieved
C     as in (a) on fwd xforms, but as in (b) on inv xforms
C (d) digit reversal in both dirns happens AFTER other changes
C Note that Shift and Reflect commute
C
C NB: BASE,NB are only needed for passing to MRDGRV; the factors
C themselves are supplied right-justified in the first MAX(NB(1),NB(2)
C elements (with leading ones, to be stripped in transit)
C
C Local declarations
C
      LOGICAL SEMROW
      INTEGER MRDGRV
C
      INTEGER I,R,S,RROWN,FORM,NBMAX
C
C Global declarations
C
      INCLUDE 'COMMON'
C
      DFTRD=.TRUE.
C
      NBMAX=MAX(NB(1),NB(2))
C
C OPC=0: Initialise X-reordering map
C ----------------------------------
C
      IF (OPC.EQ.0) THEN
C
C Include shift unless hpl & inv
C
         IF (HPL.AND..NOT.FWD) THEN
            S=0
         ELSE
            S=N(1)/2
         ENDIF
C
C Prepare X-reordering map: shift by S, reflect if fwd, then digit
C reversal; MAP(I) indicates where element I comes FROM, so order of
C adjustments appears reversed here
C
         DO 10 I=0,N(1)-1
C
C [in 0 to 3N/2]
C
            R=MRDGRV(I,BASE(NBMAX-NB(1)+1,1),NB)+S
C
C [2N ensures non-neg]
C
            IF (FWD) R=2*N(1)-R
            MAP(I)=MOD(R,N(1))
   10    CONTINUE
C
C OPC=1: Read row
C ---------------
C
      ELSE
C
C If not first, simply read as stored directly to CB
C
         IF (.NOT.FIRST) THEN
            IF (SEMROW(1,CB,FORMN(LPN),ROWN+1,1,LPN)) GOTO 50
         ELSE
C
C Code for first pass only
C
C Force form conversion (trivial if hpl-inv)
C
            IF (HPL) THEN
               FORM=NFMFP
            ELSE
               FORM=NFMCOM
            ENDIF
C
C Y-reordering
C
C RROWN is where row ROWN comes FROM, so order of adjustments appears
C reversed here
C
            RROWN=MRDGRV(ROWN,BASE(NBMAX-NB(2)+1,2),NB(2))+N(2)/2
            IF (FWD) RROWN=2*N(2)-RROWN
            RROWN=MOD(RROWN,N(2))
C
C Read row to CB1, suppressing reorder if hpl-inv
C
            IF (HPL.AND..NOT.FWD) THEN
               R=ROWN
            ELSE
               R=RROWN
            ENDIF
C
            IF (SEMROW(1,CB1,FORM,R+1,1,LPN)) GOTO 50
C
C Take mod squared?
C
            IF (MSQ.AND..NOT.FWD) THEN
               CALL FTLMSQ(CB1,N,.FALSE.)
            ENDIF
C
C X/Y grids
C
C Count rows from centre to current
C
            R=N(2)/2-RROWN
C
C Real image, fwd: Y grid only
C
            IF (HPL.AND.FWD) THEN
C
C Negate row if R odd
C
               IF (R/2*2.NE.R) THEN
                  DO 20 I=0,N(1)-1
                     CB1(2*I  )=-CB1(2*I  )
                     CB1(2*I+1)=-CB1(2*I+1)
   20             CONTINUE
               ENDIF
C
C General case: X/Y grid
C
            ELSE
C
C Add number of pixels from left to origin iff complex;
C for real,inv origin is at left
C
               IF (.NOT.HPL) R=R+N(1)/2
C
C Negate 0:N alternately from 1,0 acc to R even,odd
C
               IF (R/2*2.EQ.R) THEN
                  S=1
               ELSE
                  S=0
               ENDIF
C
               DO 30 I=S,N(1)-1,2
                  CB1(2*I  )=-CB1(2*I  )
                  CB1(2*I+1)=-CB1(2*I+1)
   30          CONTINUE
            ENDIF
C
C Move to CB via prepared map
C
            DO 40 I=0,N(1)-1
               CB(2*I  )=CB1(2*MAP(I))
               CB(2*I+1)=CB1(2*MAP(I)+1)
   40       CONTINUE
C
C Zero centre if ZEROC
C
            IF (ZEROC.AND..NOT.FWD) THEN
               IF (ROWN.EQ.0) THEN
                  CB(0)=0.0
                  CB(1)=0.0
               ENDIF
            ENDIF
         ENDIF
      ENDIF
C
      DFTRD=.FALSE.
C
   50 RETURN
C
C Copyright (C) 1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module DFTWR
C
      LOGICAL FUNCTION DFTWR(CB,R,LPO,LPW,HPL,FWD,LAST,RN,MSQ,LN)
C
      REAL    CB(0:*),RN
      INTEGER R,LPO,LPW
      LOGICAL HPL,FWD,LAST,MSQ,LN
C
C Outputs rows for DFTM, including possible [ln] mod squared
C
C [in/out] complex CB: row data for output
C [in]     integer R: output row number, rel 0
C [in]     integer LPW: output lpn unless LAST
C [in]     integer LPO: output lpn if LAST
C [in]     logical HPL: CB form is fp if LAST,inv
C [in]     logical FWD: flags forward (sign=-1) xform
C [in]     logical LAST: current xform pass is last
C [in]     real RN: normalisation factor applied if LAST,inv
C [in]     logical MSQ: take mod squared of CB in situ if LAST,fwd
C [in]     logical LN: take ln mod squared if MSQ,LAST,fwd
C
C Local declarations
C
      LOGICAL SEMROW
C
      INTEGER LPN,I,FORM,NCOL
C
C Global declarations
C
      INCLUDE 'COMMON'
C
      DFTWR=.TRUE.
C
C Special consideration for last pass output
C
      IF (LAST) THEN
C
C Local form is normally complex, but fp if inv-hpl
C
         IF (.NOT.FWD.AND.HPL) THEN
            FORM=NFMFP
            NCOL=NCOLS(LPW)/2
         ELSE
            FORM=NFMCOM
            NCOL=NCOLS(LPW)
         ENDIF
C
C Output to LPO
C
         LPN=LPO
C
C Normalise if inv
C
         IF (.NOT.FWD) THEN
            DO 10 I=0,NCOL-1
               CB(2*I  )=RN*CB(2*I)
               CB(2*I+1)=RN*CB(2*I+1)
   10       CONTINUE
         ENDIF
C
C If not last pass, simply write as stored
C
      ELSE
         FORM=FORMN(LPW)
         NCOL=NCOLS(LPW)
C
C Output to LPW
C
         LPN=LPW
      ENDIF
C
C Take [ln] mod squared? on last fwd pass
C
      IF (MSQ.AND.LAST.AND.FWD) CALL FTLMSQ(CB,NCOL,LN)
C
C Output row
C
      IF (SEMROW(2,CB,FORM,R+1,1,LPN)) GOTO 20
C
      DFTWR=.FALSE.
C
   20 RETURN
C
C Copyright (C) 1992:  Synoptics Ltd,  All Rights Reserved
C
      END
