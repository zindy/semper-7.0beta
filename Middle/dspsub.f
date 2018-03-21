C Semper 6 subsidiary module DSPSUB
C
      LOGICAL FUNCTION DSPSUB(PNF,PND,DEST,NOLET,FORM,MAGN,M1,M2,N1,N2)
C
      INTEGER PNF,PND,DEST,FORM,MAGN,M1,M2,N1,N2
      LOGICAL NOLET
C
C Establishes source subregion parameters, source range, opens display,
C and writes picture title, number, size and range.
C If DEST is non-zero, output is to console output stream (DEST = 1) or
C log output stream (DEST = 2).
C
      INTEGER IVAL,IVALPN,SEMFRM,LNBLNK
C     Change: LDM, July 2005
C     Ensure that gfortran does not think of range, unpack as intrinsics
      EXTERNAL RANGE, UNPACKF
      LOGICAL SEMOPN,SEMROW,SEMLU,RANGE,MEANSD,SEMXA1,TSTSRG,MRKREG
      LOGICAL SEMLAB,SEMDPD,OPT,OPTNO,SUBREG,FSINIT,FSTEXT,SEMCON,SEMLOG
      LOGICAL SEMTIT
C
      INCLUDE 'COMMON'
C
      REAL BL,WH,RMAGN,XD,T,Y,V1,V2
      INTEGER SIZSTR(5),RANSTR(6),BWSTR(4),TEXT(RECLEN),LABEL(256)
      INTEGER CLASS,PTR,CHSIZ2,DEVICE,N,MAXCH,NDIS,NCALL
      CHARACTER*(LBTT2-LBTT1+1) TITLE
C
C Packed names
C
      INTEGER NFROM,NTO,NTIMES,NPRESE,NSURVE,NFULL,NNEGAT,NSCALE
      PARAMETER (NFROM=10335,NTO=-601,NTIMES=-374,NPRESE=26325)
      PARAMETER (NSURVE=31258,NFULL=10452,NNEGAT=22607,NSCALE=30521)
C
      EQUIVALENCE (LABEL,RB1),(TEXT,RB2),(TITLE,RECORD)
      EQUIVALENCE (SMGR1,BL),(SMGR2,WH)
      EQUIVALENCE (SMGL1,SUBREG)
C
      DATA SIZSTR /KUCS,KLCI,KLCZ,KLCE,KSPACE/
      DATA RANSTR /KUCR,KLCA,KLCN,KLCG,KLCE,KSPACE/
      DATA BWSTR  /KUCB,KCOMMA,KUCW,KSPACE/
C
C Fault bad magnification factor
C
      IF (MAGN.LE.0) THEN
         ERROR = 3
         IDERR = NTIMES
         GOTO 120
      ENDIF
C
C Initialise
C
      PNF = IVALPN(NFROM)
      PND = IVAL(NTO)
C
C Force device 1 pro tem
C
      NDIS = PND - PND/1000*1000
      PND = 1000+NDIS
      DEVICE = 1
C
C Establish subregion displayed, for normal ...
C
      CLASS = CLASSN(LP1)
      FORM = SEMFRM(FORMN(LP1))
      IF (CLASS .NE. NCLHIS) THEN
         IF (TSTSRG(1,LP1)) GOTO 120
         IF (VERB.NE.NSURVE) THEN
            IF (MRKREG(0)) GOTO 120
         ENDIF
C
C ... and histogram case
C
      ELSE
         SMGI1=1
         SMGI4=NCOLS(LP1)-2
         SMGI2=1
         SMGI5=1
         SMGI3=1
         SMGI6=1
         SMGI9=1
         SUBREG=.TRUE.
      ENDIF
C
C If HISTOGRAM, suppress options PRESET, NEGATE and NOSCALE
C
      IF (CLASS.EQ.NCLHIS) THEN
         IF (SEMLU(2,NPRESE,0.0)) GOTO 120
         IF (SEMLU(2,NNEGAT,0.0)) GOTO 120
         IF (SEMLU(2,NSCALE,1.0)) GOTO 120
      ENDIF
C
C Establish pixel range (separate code for SURVEY)
C
      IF (VERB.EQ.NSURVE) THEN
         IF (OPT(NFULL)) THEN
            IF (MEANSD(LP1)) GOTO 120
         ELSE
            IF (RANGE(1,LP1)) GOTO 120
         ENDIF
      ELSE
C
C Option NOSCALE forces black/white levels for unit scaling
C
         IF (OPTNO(NSCALE)) THEN
            BL = 0.0
            WH = REAL(LUTLEN-1)
         ELSE
C
C Get pixel range (PRESET option causes RANGE to leave VMIN,VMAX alone)
C
            IF (RANGE(1,LP1)) GOTO 120
C
C Fault zero pixel range
C
            IF (VMAX.EQ.VMIN) THEN
               ERROR = 12
               IDERR = PNF
               GOTO 120
            ENDIF
C
C Establish black/white levels, taking NEGATE option into account
C
            IF (OPT(NNEGAT)) THEN
               BL = VMAX
               WH = VMIN
            ELSE
               BL = VMIN
               WH = VMAX
            ENDIF
         ENDIF
      ENDIF
C
      ERROR = 0
C
C Adapt subregion limits to accommodate TIMES
C
      IF (MAGN .LE. 1) THEN
         M1=SMGI1
         M2=SMGI4
         N1=SMGI2
         N2=SMGI5
      ELSE
         M1=1
         M2=(SMGI4-SMGI1)*MAGN+1
         N1=1
         N2=(SMGI5-SMGI2)*MAGN+1
      ENDIF
C
C For histograms, fetch source picture pixel range
C
      IF (CLASS.EQ.NCLHIS) THEN
         IF (SEMROW(1,RB1,2,1,1,LP1)) GOTO 120
         VMIN = RB1(SMGI4+1)
         VMAX = RB1(SMGI4+2)
      ENDIF
C
C Divert if character mode (or SURVEY), setting line width (size of
C output buffer)
C
      IF (DEST.NE.0) THEN
         MAXCH=RECLEN
         GOTO 10
      ENDIF
C
C Open display
C
      LP2 = LP1
      IF (SEMOPN(2,PND,M2-M1+1,N2-N1+1,SMGI9,CLASS,FORM,LP2)) GOTO 120
      REQFSF = .TRUE.
C
C Fault undersampling if magnifying as well
C
      IF (MAGN.GT.1) THEN
         IF (PXSAM(LP2).NE.1) THEN
            ERROR = 54
            GOTO 120
         ENDIF
      ENDIF
C
C Fetch DPD; adjust map, limits, black/white levels and note source
C picture number
C
      IF (SEMDPD(1,NDIS)) GOTO 120
      RMAGN = MAGN
      XD = DPMA*DPLEF + DPMB
      DPMA = DPMA*RMAGN
      T = DPLEF
      DPLEF = SMGI1 - CCOLN(LP1)
      DPRIG = DPLEF + (DPRIG-T)/RMAGN
      DPMB = XD - DPMA*DPLEF
      XD = DPMA2*DPTOP + DPMB2
      DPMA2 = DPMA2*RMAGN
      T = DPTOP
      DPTOP = CROWN(LP1) - SMGI2
      DPBOT = DPTOP - (T-DPBOT)/RMAGN
      DPMB2 = XD - DPMA2*DPTOP
      DPMIN = BL
      DPMAX = WH
      DPSRC = PNF
      IF (SEMDPD(2,NDIS)) GOTO 120
C
C Note black/white levels in picture table
C
      GSMIN(LP2) = BL
      GSMAX(LP2) = WH
C
C Make necessary adjustments to range record ...
C If PRESET or NOSCALE option set, delete range record, otherwise,
C set range record to surveyed values
C
      IF (OPT(NPRESE).OR.OPTNO(NSCALE)) THEN
         NCALL=3
      ELSE
         NCALL=4
      ENDIF
C
      IF (RANGE(NCALL,LP2)) GOTO 120
C
C Set up graphics coordinates for output to display partition
C
      IF (FSINIT(2,PND)) GOTO 120
C
C Prepare text posn,limits (+1 simply allows a little leeway)
C
      CHSIZ2 = CHSI2(DEVICE)+1
      MAXCH = DPSIZ/CHSIZ(DEVICE)-1
      Y = FSBTOP - 2
C
C Fetch source picture label
C
   10 IF (SEMLAB(1,LABEL,LP1)) GOTO 120
C
C Encode picture number
C
      IF (NOLET) GOTO 100
      PTR=1
      DUMLOG=SEMXA1(4,TEXT,256,PTR,REAL(PNF),N)
C
C Append a space character
C
      TEXT(PTR)=KSPACE
      PTR=PTR+1
C
C Fetch picture title
C
      IF (SEMTIT(1,TITLE,LP1)) GOTO 120
C
C Copy title string (if any) into annotation array
C
      N=LNBLNK(TITLE)
C
      CALL SEMICS(TITLE,TEXT(PTR),N)
C
      N=N+PTR-1
C
C Truncate to last non-fitting space
C
   20 IF (N.GT.MAXCH) THEN
   30    N=N-1
         IF (N.EQ.0) GOTO 100
         IF (TEXT(N+1).EQ.KSPACE) GOTO 20
         GOTO 30
      ENDIF
C
C Output it
C
      NCALL=1
      GOTO 90
C
C Encode size (bracketed if subregion)
C
   40 DO 50 PTR=1,5
         TEXT(PTR)=SIZSTR(PTR)
   50 CONTINUE
C
      IF (SUBREG) THEN
         TEXT(PTR)=KBRA
         PTR=PTR+1
      ENDIF
C
      DUMLOG=SEMXA1(4,TEXT,256,PTR,REAL(SMGI4-SMGI1+1),N)
      IF (N2.NE.N1) THEN
         TEXT(PTR)=KCOMMA
         PTR=PTR+1
         DUMLOG=SEMXA1(4,TEXT,256,PTR,REAL(SMGI5-SMGI2+1),N)
      ENDIF
C
      IF (SUBREG) THEN
         TEXT(PTR) = KKET
         PTR = PTR+1
      ENDIF
C
      N=PTR-1
C
C Output it
C
      NCALL=2
      GOTO 90
C
C Encode range or black/white levels
C
   60 IF (VERB.EQ.NSURVE.OR.CLASS.EQ.NCLHIS) THEN
         DO 70 PTR=1,6
            TEXT(PTR)=RANSTR(PTR)
   70    CONTINUE
C
         V1=VMIN
         V2=VMAX
      ELSE
         DO 80 PTR=1,4
            TEXT(PTR)=BWSTR(PTR)
   80    CONTINUE
C
         V1=BL
         V2=WH
      ENDIF
C
      DUMLOG=SEMXA1(4,TEXT,256,PTR,V1,N)
C
      TEXT(PTR)=KCOMMA
      PTR=PTR+1
C
      DUMLOG=SEMXA1(4,TEXT,256,PTR,V2,N)
      N=PTR-1
C
C Output it
C
      NCALL=3
C
C Internal routine for text output
C
   90 IF (DEST.EQ.0) THEN
         IF (N.LE.MAXCH) THEN
            IF (Y.LT.FSBTOP/3.0) GOTO 100
            IF (FSTEXT(TEXT,N,0.0,Y,0,1)) GOTO 120
            Y=Y-REAL(CHSIZ2)
         ENDIF
      ELSE
         CALL SEMCHS(RECORD,TEXT,N)
C
         IF (DEST.EQ.1) THEN
            IF (SEMCON(RECORD)) GOTO 120
         ENDIF
C
         IF (DEST.EQ.2) THEN
            IF (SEMLOG(RECORD)) GOTO 120
         ENDIF
      ENDIF
C
      IF (NCALL.EQ.1) THEN
         GOTO 40
      ELSE IF (NCALL.EQ.2) THEN
         GOTO 60
      ENDIF
C
C Normal return
C
  100 DSPSUB = .FALSE.
C
  110 RETURN
C
C Error returns
C
  120 DSPSUB = .TRUE.
      GOTO 110
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
