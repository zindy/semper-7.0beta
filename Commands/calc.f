C Semper 6 processing module CALC
C
      SUBROUTINE CALC
C
C Provides picture arithmetic, using code similar to SEMEXP's to
C interpret an expression supplied via key FROM; constructions of
C the form :n and d:n (names or numbers for d and n) are taken to
C denote pixel rather than constant values, and X,Y,Z and RR are
C taken to denote coordinates x,y,z and their summed square
C
C The expression provided is interpreted once per row, with data being
C stacked (in the six row buffers) a row at a time, and constants
C being repeated NCOL times to fill a row.  This achieves more or less
C arbitrary picture/constant arithmetic without multiple data passes
C or high interpretion overheads.
C
C Syntax differs from SEMEXP in minor ways only, to allow complex as
C well as real operands:
C   : is differently treated
C   Extra function C(X,Y) generates complex
C   Extra function CC(Z) generates conjugate of complex
C   RE,IM,PHASE,MOD,MSQ accept one (normally compl) arg only
C   P() and SET() are not recognised
C
C Picture references in the expression are identified in a first pass,
C and the output is opened accordingly; data processing begins in the
C second pass, but if the final form of the expression proves to show
C a real/complex mismatch with the output, the output is re-opened
C appropriately and processing recommenced again
C
      INTEGER IVAL,IVALPN,SEMFRM
      LOGICAL SEMLU,SEMROW,SEMOPN,SEMXA1
C
      REAL T,VALUE,V1,V2,V3,V4,X,Y,Z
      INTEGER I,INFORM,LAYER,LPN,N,NOP,NOPEN
      INTEGER MODE,NAME,NARGS,NEWPRI,NCN,NRN,NCOL,NROW,NLAY
C
      INTEGER PRIOR(33:126)
      INTEGER CH,OPCODE,PTR1,PTR,OSPTR,VSPTR,CCOL,CROW,CLAY
      INTEGER CLASS,FORM,ROW
      INTEGER*4 N1,N2,I4,N4,N22,LBSEP,NC4
C
      LOGICAL ENDFND,OPEXP,USANT,UNANT,PASS1,PNFLAG,LREAL
C
C Packed names
C
      INTEGER NDLLR1,NTO,NX,NY,NZ,NRR,NC,NRE,NIM,NCC
      INTEGER NPHASE,NMOD,NMSQ,NMIN,NMAX,NRAD,NDEG,NCOS
      INTEGER NACOS,NSIN,NASIN,NTAN,NROOT,NEXP,NLN,NFIX
      INTEGER NROUND,NAND,NOR,NNOT,NIFELS,NREM
      PARAMETER (NDLLR1=-12441,NTO=-601)
      PARAMETER (NX=-6401,NY=-8001,NZ=-9601,NRR=29520)
      PARAMETER (NC=4800,NRE=29000,NIM=14920,NCC=4920)
      PARAMETER (NPHASE=25921,NMOD=21404,NMSQ=21577)
      PARAMETER (NMIN=21174,NMAX=20864,NRAD=28844,NDEG=6607)
      PARAMETER (NCOS=5419,NACOS=1735,NSIN=30774,NASIN=2369)
      PARAMETER (NTAN=-55,NROOT=29415,NEXP=8976,NLN=19760)
      PARAMETER (NFIX=9984,NROUND=29421,NIFELS=14645,NREM=29013)
      PARAMETER (NAND=2164,NOR=24720,NNOT=23020)
C
C Operator priorities
C
      INTEGER MBRA,MCOMMA,MKET,MBAR,MAMPER,MTILDE,MABRA,MAKET
      INTEGER MEQUAL,MPLUS,MMINUS,MSTAR,MSLASH,MUP,MCOLON,MSPARE
      PARAMETER (MBRA=1,MCOMMA=2,MKET=3,MBAR=4,MAMPER=5,MTILDE=6)
      PARAMETER (MABRA=7,MAKET=7,MEQUAL=7,MPLUS=8,MMINUS=8)
      PARAMETER (MSTAR=9,MSLASH=9,MUP=10,MCOLON=11)
      PARAMETER (MSPARE=0)
C
C Last functions accepting three, two (real) args and complex arg
C
      INTEGER LASTF3,LASTF2,LASTFC
      PARAMETER (LASTF3=1,LASTF2=7,LASTFC=13)
C
C Total number of functions recognised
C
      INTEGER NFUNS
      PARAMETER (NFUNS=26)
      INTEGER FUNS(NFUNS)
C
      INCLUDE 'COMMON'
C
      INTEGER VSMAX,OSMAX
      PARAMETER ( VSMAX = NNBUF, OSMAX = 4*NNBUF )
C
      INTEGER OPN(NLPS),OLPN(NLPS),LFORM(VSMAX),OSTACK(OSMAX)
C
      PARAMETER (LBSEP=LNBUF/LNREAL+LNEDGE*2)
C
      EQUIVALENCE (PRIOR(KUCA),FUNS)
C
C Operator priorities; function names reordered wrt SEMEXP, to
C accommodate adjustments in permitted arg numbers
C
      DATA PRIOR/0,0,0,MSPARE,0,MAMPER,0,
     +   MBRA,MKET,MSTAR,MPLUS,MCOMMA,MMINUS,0,MSLASH,
     +   MABRA,MAKET,MEQUAL,7*MSPARE,MCOLON,0,MABRA,MEQUAL,MAKET,0,
     +   0,NIFELS,NMIN,NMAX,NREM,NAND,NOR,NC,NPHASE,NMOD,NMSQ,NCC,
     +   NRE,NIM,NCOS,NACOS,NSIN,NASIN,NTAN,NROOT,NEXP,NLN,NFIX,NROUND,
     +   NNOT,NRAD,NDEG,0,0,0,MUP,0,0,26*MSPARE,0,MBAR,0,MTILDE/
C
C Initialise
C
      PASS1=.TRUE.
      PTR1=IVAL(NDLLR1)
      NOPEN=0
      LP2=0
      LAYER=1
C
C Zero stacks: expect value first
C
   10 ROW=1
   20 OSPTR=0
      VSPTR=0
      OPEXP=.FALSE.
      USANT=.TRUE.
      UNANT=.TRUE.
      ENDFND=.FALSE.
      PTR=PTR1-1
C
C Classify next object: pick up next non-space
C
   30 PNFLAG=.FALSE.
   40 PTR=PTR+1
   50 IF (SEMXA1(0,LINBUF,LINLEN,PTR,VALUE,CH)) THEN
C
C Text exhausted: fault if operator outstanding, otherwise terminate
C
         IF (.NOT.OPEXP) GOTO 1150
         GOTO 230
      ENDIF
C
      VALUE=0.
C
C Sort character: numeric?
C
      NOP=2
      IF (CH.GE.KZERO.AND.CH.LE.KNINE) GOTO 70
      IF (CH.EQ.KDOT) GOTO 70
      IF (CH.EQ.KCOLON) THEN
         VALUE=CD
         GOTO 60
      ENDIF
C
C Alphabetic?
C
      NOP=1
      IF (CH.GE.KUCA.AND.CH.LE.KUCZ) GOTO 70
      IF (CH.GE.KLCA.AND.CH.LE.KLCZ) GOTO 70
      IF (CH.EQ.KDOLLA) GOTO 70
C
C Other
C
      GOTO 160
C
C Special processing for :
C Set pic num flag, save dev num and get next val
C
   60 IF (PNFLAG) GOTO 1150
      PNFLAG=.TRUE.
      V1=VALUE
      GOTO 40
C
C Number and name decoding
C ------------------------
C
   70 IF (OPEXP) GOTO 230
      IF (SEMXA1(NOP,LINBUF,LINLEN,PTR,VALUE,NAME)) CONTINUE
      IF (ERROR.NE.0) GOTO 1230
C
C Get next character
C
      IF (SEMXA1(0,LINBUF,LINLEN,PTR,X,CH)) CONTINUE
      IF (ERROR.NE.0) GOTO 1230
C
C For names, divert if function ref
C
      IF (NOP.EQ.1) THEN
         IF (CH.EQ.KBRA) GOTO 140
C
C Else, obtain value, trapping X,Y,Z and RR if not pic num
C
         IF (.NOT.PNFLAG) THEN
            IF (NAME.EQ.NX) GOTO 80
            IF (NAME.EQ.NY) GOTO 80
            IF (NAME.EQ.NZ) GOTO 80
            IF (NAME.EQ.NRR) GOTO 80
         ENDIF
         IF (.NOT.SEMLU(-1,NAME,VALUE)) GOTO 1210
      ENDIF
C
C See if colon precedes value
C
      IF (PNFLAG) THEN
C
C Combine two values to form picture number, first checking for valid
C device and picture numbers
C
         V1=ANINT(V1)
         VALUE=ANINT(VALUE)
         IF (V1.GT.999.0) V1=AINT(V1/1000.0)
         IF (VALUE.GT.999.0) VALUE=AMOD(VALUE,1000.0)
         IF (V1.LT.1.0.OR.V1.GT.REAL(NDVS).OR.VALUE.LT.1.0) GOTO 1190
         N=NINT(V1*1000.+VALUE)
      ELSE
C
C If colon following, set pic num flag and get another value
C
         IF (CH.EQ.KCOLON) GOTO 60
      ENDIF
C
C Value stacking
C --------------
C On pass 1, simply open picture and note or check dims
C
   80 VSPTR=VSPTR+1
      IF (VSPTR.GT.VSMAX) GOTO 1200
C
      IF (PASS1) THEN
C
C Pass 1: deal with only if pic number ...
C
         IF (PNFLAG) THEN
C
C Have we opened it already?
C
            DO 90 I=1,NOPEN
               IF (OPN(I).EQ.N) GOTO 130
   90       CONTINUE
C
C No: do so now
C
            IF (SEMOPN(1,N,NCN,NRN,CH,I,NAME,LPN)) GOTO 1230
C
C Note dimensions if first picture, otherwise check dimensions
C
            IF (NOPEN.EQ.0) THEN
               NCOL=NCN
               NROW=NRN
               NLAY=CH
               CLASS=I
               FORM=NAME
               CCOL=CCOLN(LPN)
               CROW=CROWN(LPN)
               CLAY=CLAYN(LPN)
            ELSE
               IF (NCN.NE.NCOL .OR. NRN.NE.NROW .OR. CH.NE.NLAY) THEN
                  ERROR = 5
                  IDERR = N
                  GOTO 1230
               ENDIF
            ENDIF
C
            NOPEN=NOPEN+1
            IF (NOPEN.GT.NLPS) THEN
               ERROR = 56
               GOTO 1230
            ENDIF
C
            OPN(NOPEN)=N
            OLPN(NOPEN)=LPN
         ENDIF
C
C Later passes: set buffer pointer
C If pic num, find LPN and stack row
C
      ELSE
         N1=LBSEP
         N1=(VSPTR-1)*N1+1
C
         IF (PNFLAG) THEN
            DO 100 I=1,NOPEN
               IF (OPN(I).EQ.N) LPN=OLPN(I)
  100       CONTINUE
C
C Forms fp or complex internally
C
            IF (FORMN(LPN).EQ.NFMCOM) THEN
               INFORM=NFMCOM
            ELSE
               INFORM=NFMFP
            ENDIF
C
C Read in data
C
            IF (SEMROW(1,RB1(N1),INFORM,ROW,LAYER,LPN)) GOTO 1230
C
C Note form
C
            LFORM(VSPTR)=INFORM
C
C If not pic num, stack constant value or coord
C
         ELSE
            X=REAL(1-CCOL)
            Y=REAL(CROW-ROW)
            Z=REAL(LAYER-CLAY)
            T=(Y*Y)+(Z*Z)
            N2=N1+NCOL-1
            IF (NAME.EQ.NX .OR. NAME.EQ.NRR) THEN
               DO 110 I4=N1,N2
                  IF (NAME.EQ.NX) THEN
                     RB1(I4) = X
                  ELSE
                     RB1(I4) = (X*X)+T
                  ENDIF
                  X = X + 1.
  110          CONTINUE
            ELSE
               IF (NAME.EQ.NY) VALUE=Y
               IF (NAME.EQ.NZ) VALUE=Z
               DO 120 I4=N1,N2
                  RB1(I4) = VALUE
  120          CONTINUE
            ENDIF
C
C Note form
C
            LFORM(VSPTR)=NFMFP
         ENDIF
      ENDIF
C
C Expect op next
C
  130 OPEXP=.TRUE.
      USANT=.FALSE.
      UNANT=.FALSE.
      GOTO 50
C
C Function ref - name known?
C ------------
C
  140 DO 150 CH=1,NFUNS
      IF (NAME.EQ.FUNS(CH)) THEN
C
C Yes : stack opcode
C
         PTR = PTR - 1
         GOTO 220
      ENDIF
  150 CONTINUE
      ERROR = 16
      GOTO 1220
C
C Operator decoding/stacking
C --------------------------
C
  160 IF (OPEXP) GOTO 170
C
C Not expected - but could be bra..
C
      IF (CH.EQ.KBRA) GOTO 220
C
C ..or unary plus or minus (stack zero and reprocess as dyadic)
C
      IF (CH.EQ.KPLUS.OR.CH.EQ.KMINUS) THEN
         IF (USANT) THEN
            VALUE=0.
            NAME=0
            GOTO 80
         ENDIF
C
C .. or unary not (stack operator)
C
      ELSE IF (CH.EQ.KTILDE) THEN
         IF (UNANT) GOTO 220
C
C .. or equal following < or > (modify opcode and keep scanning)
C
      ELSE IF (CH.EQ.KEQUAL) THEN
         IF (OSTACK(OSPTR).EQ.KABRA) THEN
            OSTACK(OSPTR)=KZERO
            GOTO 30
         ELSE IF (OSTACK(OSPTR).EQ.KAKET) THEN
            OSTACK(OSPTR)=KONE
            GOTO 30
         ENDIF
      ENDIF
C
C Bad syntax
C
      GOTO 1150
C
C Expected: fault ~ (unless followed by =), terminate if not recognised
C
  170 IF (CH.EQ.KTILDE) THEN
         I=PTR+1
         IF (SEMXA1(0,LINBUF,LINLEN,I,VALUE,N)) CONTINUE
         IF (ERROR.NE.0) GOTO 1230
         IF (N.EQ.KEQUAL) THEN
            CH=KTWO
            PTR=I
         ELSE
            GOTO 1150
         ENDIF
      ENDIF
      IF (CH.EQ.KBRA) GOTO 230
      NEWPRI=PRIOR(CH)
      IF (NEWPRI.EQ.0) GOTO 230
C
C Central stack/destack decision
C ------------------------------
C Destack if new operator CH is (loosely) of lower priority than
C current stack top; control returns here after destacking to repeat
C the priority comparison
C
  180 IF (OSPTR.LE.0) GOTO 190
      OPCODE=OSTACK(OSPTR)
      IF (OPCODE.LT.31) GOTO 240
C
C Special consideration of commas
C
      IF (CH.EQ.KCOMMA.AND.OPCODE.EQ.KCOMMA) GOTO 190
      IF (NEWPRI.LE.PRIOR(OPCODE)) GOTO 240
C
C No further destacking for the present: trap comma and ket
C
  190 IF (CH.NE.KCOMMA) GOTO 200
C
C Stack comma if within scope of multi-argument function call;
C otherwise terminate
C
      OPCODE=OSPTR-1
      IF (OPCODE.LE.0) GOTO 230
      OPCODE=OSTACK(OPCODE)
      IF (OPCODE.LE.LASTF2) GOTO 220
      OPCODE=OSPTR-2
      IF (OPCODE.LE.0) GOTO 230
      OPCODE=OSTACK(OPCODE)
      IF (OPCODE.LE.LASTF3) GOTO 220
      GOTO 230
C
C If ket, establish number of arguments in case function call,
C remove matching bra, and proceed to next item (which will cause
C immediate destacking)
C
  200 IF (CH.NE.KKET) GOTO 220
      NARGS=1
      IF (OSPTR.LE.0) GOTO 230
      IF (OSTACK(OSPTR).NE.KCOMMA) GOTO 210
      NARGS=2
      OSPTR=OSPTR-1
      IF (OSTACK(OSPTR).NE.KCOMMA) GOTO 210
      NARGS=3
      OSPTR=OSPTR-1
C
C Require bra to match
C
  210 IF (OSTACK(OSPTR).NE.KBRA) GOTO 1150
      OSPTR=OSPTR-1
      OPEXP=.TRUE.
      GOTO 30
C
C Stack operator
C
  220 OSPTR=OSPTR+1
      IF (OSPTR.GT.OSMAX) GOTO 1200
      OSTACK(OSPTR)=CH
      OPEXP=.FALSE.
C
C Anticipate unary plus or minus if stacking bra, comma,
C < = or >, ~ & or |
C
      USANT=CH.EQ.KBRA.OR.CH.EQ.KCOMMA.OR.IABS(CH-KEQUAL).LE.1.OR.
     +      CH.EQ.KTILDE.OR.CH.EQ.KAMPER.OR.CH.EQ.KBAR.OR.CH.EQ.KTWO
C
C Anticipate unary not if stacking bra, comma, & or |
C
      UNANT=CH.EQ.KBRA.OR.CH.EQ.KCOMMA.OR.CH.EQ.KAMPER.OR.CH.EQ.KBAR
      GOTO 30
C
C Expression terminated
C
  230 ENDFND=.TRUE.
C
C Destacking: apply stacked operator to stacked operand(s)
C ----------
C Make normal exit if operator stack empty
C
  240 IF (OSPTR.EQ.0) GOTO 1130
C
C Pick up last stacked operator
C
      OPCODE=OSTACK(OSPTR)
      OSPTR=OSPTR-1
C
C Pick up last stacked items (NARGS if function, 2 if operator, 1 if ~)
C
      IF (OPCODE.GT.NFUNS) NARGS=2
      IF (OPCODE.EQ.KTILDE) NARGS=1
      VSPTR=VSPTR-NARGS+1
C
C Check for stack exhaustion
C
      IF (VSPTR.LE.0) GOTO 1150
      N1=LBSEP
      N1=(VSPTR-1)*N1+1
      N2=N1+NCOL-1
      NC4=NCOL
C
C On pass 1, skip actual operator application
C
      IF (PASS1) THEN
         VALUE=0.0
         GOTO 1120
      ENDIF
C
      LREAL=LFORM(VSPTR).NE.NFMCOM
C
C Form checking for functions
C
      IF (OPCODE.GT.NFUNS) GOTO 270
      IF (OPCODE.LE.LASTF3) THEN
         IF (NARGS.LT.3) GOTO 1150
         IF (LFORM(VSPTR+1).EQ.NFMCOM.OR.
     +       LFORM(VSPTR+2).EQ.NFMCOM) GOTO 1160
         GOTO 250
      ELSE IF (OPCODE.LE.LASTF2) THEN
         IF (NARGS.LT.2) GOTO 1150
         IF (LFORM(VSPTR+1).EQ.NFMCOM) GOTO 1160
         GOTO 250
      ENDIF
      IF (OPCODE.LE.LASTFC) GOTO 260
  250 IF (.NOT.LREAL) GOTO 1160
C
C Code switch
C
  260 CONTINUE
      GOTO (290,330,350,310,370,390,410,470,530,500,450,430,430,
     +      560,580,600,620,640,660,680,700,740,720,760,780,800),
     +      OPCODE
C
C Dyadic operators: if either complex..
C
  270 IF (LREAL.AND.LFORM(VSPTR+1).NE.NFMCOM) GOTO 280
C
C ..force both complex for *,+,-,/
C
      IF (OPCODE.GE.KSTAR.AND.OPCODE.LE.KSLASH) THEN
         IF (LREAL) THEN
            CALL CFORM(RB1(N1),RB1(N1),NFMFP,NFMCOM,NC4)
            LFORM(VSPTR)=NFMCOM
         ENDIF
         IF (LFORM(VSPTR+1).NE.NFMCOM) THEN
            CALL CFORM(RB2(N1),RB2(N1),NFMFP,NFMCOM,NC4)
            LFORM(VSPTR+1)=NFMCOM
         ENDIF
         LREAL=.FALSE.
         N22=N2+NCOL
      ELSE
C
C Otherwise, fault complex operands
C
         GOTO 1160
      ENDIF
  280 IF (OPCODE.EQ.KUP) GOTO 820
      IF (OPCODE.EQ.KBAR) GOTO 1080
      IF (OPCODE.EQ.KTILDE) GOTO 1100
      IF (OPCODE.EQ.KABRA) GOTO 940
      IF (OPCODE.EQ.KEQUAL) GOTO 960
      IF (OPCODE.EQ.KAKET) GOTO 980
      OPCODE=OPCODE-KAMPER+1
      GOTO (1060,1150,1150,1150,840,870,
     +      1150,890,840,910,1000,1020,1040),
     +      OPCODE
C
C Code for applying functions
C ---------------------------
C Ifelse
C
  290 DO 300 I4=N1,N2
         IF (RB1(I4).NE.0.0) THEN
            RB1(I4)=RB2(I4)
         ELSE
            RB1(I4)=RB3(I4)
         ENDIF
  300 CONTINUE
      GOTO 1120
C
C Rem
C
  310 DO 320 I4=N1,N2
         V2 = RB2(I4)
         IF (V2 .EQ. 0.0) GOTO 1170
         RB1(I4)=MOD(RB1(I4),V2)
  320 CONTINUE
      GOTO 1120
C
C Min
C
  330 DO 340 I4=N1,N2
         RB1(I4)=MIN(RB1(I4),RB2(I4))
  340 CONTINUE
      GOTO 1120
C
C Max
C
  350 DO 360 I4=N1,N2
         RB1(I4)=MAX(RB1(I4),RB2(I4))
  360 CONTINUE
      GOTO 1120
C
C And
C
  370 DO 380 I4=N1,N2
         RB1(I4)=REAL(IAND(NINT(RB1(I4)),NINT(RB2(I4))))
  380 CONTINUE
      GOTO 1120
C
C Or
C
  390 DO 400 I4=N1,N2
         RB1(I4)=REAL(IOR(NINT(RB1(I4)),NINT(RB2(I4))))
  400 CONTINUE
      GOTO 1120
C
C Complex
C
  410 N4=N2+(N2-N1+1)
      DO 420 I4=N2,N1,-1
         RB1(N4)=RB2(I4)
         RB1(N4-1)=RB1(I4)
         N4=N4-2
  420 CONTINUE
      LFORM(VSPTR)=NFMCOM
      GOTO 1120
C
C Re/Im
C
  430 IF (LREAL) GOTO 1160
C
C Following opcode is a magic number! - see computed goto above to
C determine value if new opcodes added
C
      IF (OPCODE.EQ.12) THEN
         N4=N1
      ELSE
         N4=N1+1
      ENDIF
      CALL CFORM(RB1(N4),RB1(N1),NFMCOM,NFMFP,NC4)
  440 LFORM(VSPTR)=NFMFP
      GOTO 1120
C
C Cc
C
  450 IF (.NOT.LREAL) THEN
         N4=N1+1
         DO 460 I=N1,N2
            RB1(N4)=-RB1(N4)
            N4=N4+2
  460    CONTINUE
      ENDIF
      GOTO 1120
C
C Phase
C
  470 IF (LREAL) THEN
         DO 480 I4=N1,N2
            RB1(I4)=ATAN2(RB1(I4),1.0)
  480    CONTINUE
      ELSE
         N4=N1
         DO 490 I4=N1,N2
            V1=RB1(N4)
            V2=RB1(N4+1)
            IF (V1.NE.0.0.OR.V2.NE.0.0) THEN
               RB1(I4)=ATAN2(V2,V1)
            ELSE
               RB1(I4)=0.0
            ENDIF
            N4=N4+2
  490    CONTINUE
      ENDIF
      GOTO 440
C
C Msq
C
  500 IF (LREAL) THEN
         DO 510 I4=N1,N2
            V1 = RB1(I4)
            RB1(I4)= V1*V1
  510    CONTINUE
      ELSE
         N4=N1
         DO 520 I4=N1,N2
            V1 = RB1(N4)
            V2 = RB1(N4+1)
            RB1(I4) = (V1*V1)+(V2*V2)
            N4 = N4+2
  520    CONTINUE
      ENDIF
      GOTO 440
C
C Mod
C
  530 IF (LREAL) THEN
         DO 540 I4=N1,N2
            RB1(I4)=ABS(RB1(I4))
  540    CONTINUE
      ELSE
         N4=N1
         DO 550 I4=N1,N2
            V1 = RB1(N4)
            V2 = RB1(N4+1)
            RB1(I4) = SQRT((V1*V1)+(V2*V2))
            N4 = N4+2
  550    CONTINUE
      ENDIF
      GOTO 440
C
C Cos
C
  560 DO 570 I4=N1,N2
         RB1(I4)=COS(RB1(I4))
  570 CONTINUE
      GOTO 1120
C
C Acos
C
  580 DO 590 I4=N1,N2
         V1 = RB1(I4)
         IF (ABS(V1) .GT. 1.0) GOTO 1180
         RB1(I4) = ACOS(V1)
  590 CONTINUE
      GOTO 1120
C
C Sin
C
  600 DO 610 I4=N1,N2
         RB1(I4)=SIN(RB1(I4))
  610 CONTINUE
      GOTO 1120
C
C Asin
C
  620 DO 630 I4=N1,N2
         V1 = RB1(I4)
         IF (ABS(V1) .GT. 1.0) GOTO 1180
         RB1(I4)=ASIN(V1)
  630 CONTINUE
      GOTO 1120
C
C Tan
C
  640 DO 650 I4=N1,N2
         RB1(I4)=TAN(RB1(I4))
  650 CONTINUE
      GOTO 1120
C
C Root
C
  660 DO 670 I4=N1,N2
         V1 = RB1(I4)
         IF (V1 .LT. 0.0) THEN
            ERROR = 96
            GOTO 1230
         ENDIF
         RB1(I4)=SQRT(V1)
  670 CONTINUE
      GOTO 1120
C
C Exp
C
  680 DO 690 I4=N1,N2
         RB1(I4)=EXP(RB1(I4))
  690 CONTINUE
      GOTO 1120
C
C Ln
C
  700 DO 710 I4=N1,N2
         V1 = RB1(I4)
         IF (V1 .LE. 0.0) THEN
            ERROR = 97
            GOTO 1230
         ENDIF
         RB1(I4)=ALOG(V1)
  710 CONTINUE
      GOTO 1120
C
C Round
C
  720 DO 730 I4=N1,N2
         RB1(I4)=ANINT(RB1(I4))
  730 CONTINUE
      GOTO 1120
C
C Int
C
  740 DO 750 I4=N1,N2
         RB1(I4)=AINT(RB1(I4))
  750 CONTINUE
      GOTO 1120
C
C Not
C
  760 DO 770 I4=N1,N2
         RB1(I4)=REAL(NOT(NINT(RB1(I4))))
  770 CONTINUE
      GOTO 1120
C
C Rad
C
  780 DO 790 I4=N1,N2
         RB1(I4)=PI*RB1(I4)/180.0
  790 CONTINUE
      GOTO 1120
C
C Deg
C
  800 DO 810 I4=N1,N2
         RB1(I4)=180.0*RB1(I4)/PI
  810 CONTINUE
      GOTO 1120
C
C Code for applying operators
C ---------------------------
C ^
C
  820 DO 830 I4=N1,N2
         V1 = RB1(I4)
         IF (V1 .NE. 0.0) THEN
            V2 = RB2(I4)
            IF (V2 .EQ. AINT(V2)) THEN
               IF (INT(V2) .EQ. 0) THEN
                  RB1(I4)=1.0
               ELSE
                  RB1(I4)=V1**INT(V2)
               ENDIF
            ELSE
               IF (V1 .LT. 0.0) THEN
                  ERROR = 99
                  GOTO 1230
               ELSE
                  RB1(I4)=EXP(V2 * ALOG(V1))
               ENDIF
            ENDIF
         ELSE
            RB1(I4) = 0.0
         ENDIF
  830 CONTINUE
      GOTO 1120
C
C *
C
  840 IF (LREAL) THEN
         DO 850 I4=N1,N2
            RB1(I4)=RB1(I4)*RB2(I4)
  850    CONTINUE
      ELSE
C
C Complex vector multiply
C
         DO 860 I4=N1,N22,2
            V1=RB1(I4)
            V2=RB1(I4+1)
            V3=RB2(I4)
            V4=RB2(I4+1)
            RB1(I4) = (V1*V3)-(V2*V4)
            RB1(I4+1) = (V1*V4)+(V2*V3)
  860    CONTINUE
      ENDIF
      GOTO 1120
C
C +
C
  870 IF (.NOT.LREAL) N2=N22
      DO 880 I4=N1,N2
         RB1(I4)=RB1(I4)+RB2(I4)
  880 CONTINUE
      GOTO 1120
C
C -
C
  890 IF (.NOT.LREAL) N2=N22
      DO 900 I4=N1,N2
         RB1(I4)=RB1(I4)-RB2(I4)
  900 CONTINUE
      GOTO 1120
C
C /
C
  910 IF (LREAL) THEN
         DO 920 I4=N1,N2
            V2 = RB2(I4)
            IF (V2 .EQ. 0.0) GOTO 1170
            RB1(I4)=RB1(I4)/V2
  920    CONTINUE
      ELSE
C
C Complex vector divide
C
         DO 930 I4=N1,N22,2
            V3=RB2(I4)
            V4=RB2(I4+1)
            T=(V3*V3)+(V4*V4)
            IF (T.EQ.0.0) GOTO 1170
            V1=RB1(I4)
            V2=RB1(I4+1)
            RB1(I4)=((V1*V3)+(V2*V4))/T
            RB1(I4+1)=((V2*V3)-(V1*V4))/T
  930    CONTINUE
      ENDIF
      GOTO 1120
C
C <
C
  940 DO 950 I4=N1,N2
         IF (RB1(I4).LT.RB2(I4)) THEN
            RB1(I4) = 1.0
         ELSE
            RB1(I4) = 0.0
         ENDIF
  950 CONTINUE
      GOTO 1120
C
C =
C
  960 DO 970 I4=N1,N2
         IF (RB1(I4).EQ.RB2(I4)) THEN
            RB1(I4) = 1.0
         ELSE
            RB1(I4) = 0.0
         ENDIF
  970 CONTINUE
      GOTO 1120
C
C >
C
  980 DO 990 I4=N1,N2
         IF (RB1(I4).GT.RB2(I4)) THEN
            RB1(I4) = 1.0
         ELSE
            RB1(I4) = 0.0
         ENDIF
  990 CONTINUE
      GOTO 1120
C
C <=
C
 1000 DO 1010 I4=N1,N2
         IF (RB1(I4).LE.RB2(I4)) THEN
            RB1(I4) = 1.0
         ELSE
            RB1(I4) = 0.0
         ENDIF
 1010 CONTINUE
      GOTO 1120
C
C >=
C
 1020 DO 1030 I4=N1,N2
         IF (RB1(I4).GE.RB2(I4)) THEN
            RB1(I4) = 1.0
         ELSE
            RB1(I4) = 0.0
         ENDIF
 1030 CONTINUE
      GOTO 1120
C
C ~=
C
 1040 DO 1050 I4=N1,N2
         IF (RB1(I4).NE.RB2(I4)) THEN
            RB1(I4) = 1.0
         ELSE
            RB1(I4) = 0.0
         ENDIF
 1050 CONTINUE
      GOTO 1120
C
C &
C
 1060 DO 1070 I4=N1,N2
         IF (RB1(I4) .NE. 0.0) THEN
            IF (RB2(I4) .NE. 0.0) THEN
               RB1(I4) = 1.0
            ELSE
               RB1(I4) = 0.0
            ENDIF
         ENDIF
 1070 CONTINUE
      GOTO 1120
C
C |
C
 1080 DO 1090 I4=N1,N2
         IF (RB1(I4) .EQ. 0.0) THEN
            IF (RB2(I4) .NE. 0.0) THEN
               RB1(I4) = 1.0
            ENDIF
         ELSE
            RB1(I4) = 1.0
         ENDIF
 1090 CONTINUE
      GOTO 1120
C
C ~
C
 1100 DO 1110 I4=N1,N2
         IF (RB1(I4).EQ.0.0) THEN
            RB1(I4) = 1.0
         ELSE
            RB1(I4) = 0.0
         ENDIF
 1110 CONTINUE
C
C Return to central logic so as to make next stack/destack decision
C
 1120 IF (.NOT.ENDFND) GOTO 180
      GOTO 240
C
C Expression scan completed
C -------------------------
C Output already opened?
C
 1130 IF (LP2.EQ.0) THEN
C
C No.  Number forced?
C
         N=IVALPN(NTO)
C
C If no inputs..
C
         IF (NOPEN.EQ.0) THEN
C
C ..output defaults to SELECT and is OLD: open now
C
            IF (N.LE.0) N=SELECT
            MODE=1
         ELSE
C
C Else, output defaults to first opened pic, and is NEW; however, form
C is in doubt until first row has been processed (because of possible
C real-complex switch); if pass 1, then, go back and process a row;
C else, open output now
C
            IF (PASS1) GOTO 1140
            LP2=OLPN(1)
            IF (N.EQ.0) N=1000*DEVN(LP2)+PICN(LP2)
            MODE=2
            CLASS=CLASSN(LP2)
            IF (LFORM(1).EQ.NFMCOM) THEN
               FORM=NFMCOM
            ELSE IF (FORMN(LP2).EQ.NFMCOM.AND.LFORM(1).EQ.NFMFP) THEN
               FORM=NFMFP
            ELSE
               FORM=FORMN(LP2)
            ENDIF
            FORM=SEMFRM(FORM)
         ENDIF
C
         IF (SEMOPN(MODE,N,NCOL,NROW,NLAY,CLASS,FORM,LP2)) GOTO 1230
         CCOL=CCOLN(LP2)
         CROW=CROWN(LP2)
         CLAY=CLAYN(LP2)
C
C Finally begin processing in earnest
C
 1140    PASS1=.FALSE.
         GOTO 10
      ENDIF
C
C Output resulting row
C
      IF (SEMROW(2,RB1,LFORM(1),ROW,LAYER,LP2)) GOTO 1230
C
C More rows to come?
C
      ROW=ROW+1
      IF (ROW.LE.NROW) GOTO 20
      LAYER=LAYER+1
      IF (LAYER.LE.NLAY) GOTO 10
      GOTO 1230
C
C Errors
C
 1150 ERROR=20
      GOTO 1230
 1160 ERROR=22
      GOTO 1230
 1170 ERROR=95
      GOTO 1230
 1180 ERROR=98
      GOTO 1230
 1190 ERROR=116
      GOTO 1230
 1200 ERROR=18
      GOTO 1230
 1210 ERROR=25
 1220 IDERR=NAME
C
 1230 RETURN
C
C Copyright (C) 1987-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
