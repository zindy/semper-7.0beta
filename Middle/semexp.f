C Semper 6 system module SEMEXP
C
      LOGICAL FUNCTION SEMEXP(BUFFER,LENGTH,PTR,RESULT,SCANMD)
C
      INTEGER BUFFER(*),LENGTH,PTR
      REAL RESULT
      LOGICAL SCANMD
C
C Evaluates expression held in internal code at PTR in BUFFER(LENGTH).
C PTR is incremented beyond expression (or to error if error found).
C
C SEMEXP uses two stacks; one holding values (VSTACK) and one holding
C coded operators (OSTACK). Numbers and variable values are stacked as
C they are encountered; operators are stacked when their 'priority'
C exceeds that of the current stack top; otherwise, 'destacking' takes
C place, i.e. the current operator is applied (recursively) to the value
C stack, and both stacks are shortened correspondingly.  Destacking is
C also forced by an end-of-record condition or by an unrecognised (and
C therefore zero priority) operator.
C
C Functions are stacked invariably, with their corresponding BRAs,
C and KETs cause a logic check (that a corresponding BRA, and possibly
C a function call, precedes); the operator stack is destacked to the
C function code itself, and decoding proceeds to the next item, which
C invariably causes application of the function operator via further
C destacking. Further special logic checks are used to allow commas
C delimiting function arguments to be stacked as operators.
C
C The array PRIOR establishes (via non-zero priorities) the operators
C that are recognised; it provides in element CH the priority for
C operator CH, and gives their relative priorities, namely:
C
C     <-------highest-----------lowest------>
C     Functions  :  ^  *  +  <  ~  &  |  )  ,  (
C                      /  -  >
C                            =
C                            ~=
C                            <=
C                            >=
C
C Note:  Characters such as ~ and | are not supported by some terminals.
C        In this case, the implementor will have to provide some escape
C        mechanism (typically via INKEY) to enable all of the required
C        characters to appear in expressions.  To put it another way,
C        it should be possible to input all Ascii printing characters,
C        and it MUST be possible to input all alphanumerics, $ and the
C        special characters that are a part of Semper's command line
C        syntax.
C
C The list of function names, and the operator stack itself, are
C equivalenced on to PRIOR simply to save space.  The recognised
C function names are (with SET invariably last):
C
C    P=RE IM (with 1, 2 or 3 args)
C    IFELSE (with 3 args)
C    MIN MAX REM AND OR (with 2 args)
C    PHASE MOD MSQ (with 1 or 2 args)
C    COS ACOS SIN ASIN TAN ROOT EXP LN FIX ROUND NOT (with 1 arg)
C    RADIAN DEGREE (with 1 arg)
C    SET special - causes enclosed var refs to generate 1 or 0 acc to
C        whether set or unset, rather than actual value
C
C C (with 1 or 2 args) and CC (with 1 arg) are in fact also accepted and
C treated as null
C
      LOGICAL SEMLU,SEMROW,SEMOPN,SEMXA1,SEMDCR
C
      INCLUDE 'COMMON'
C
      INTEGER VSMAX,OSMAX
      PARAMETER ( VSMAX = NNBUF, OSMAX = 4*NNBUF )
C
      REAL VALUE,VSTACK(VSMAX),V(3),V1,V2,V3
      INTEGER OSTACK(OSMAX),LASTF(2),PRIOR(33:126)
      INTEGER CH,CLASS,FORM,I,IR,J,K,L,LASTF2,LASTF3
      INTEGER NARGS,NEWPRI,OPCODE,OSPTR,VSPTR
      LOGICAL LSET,ENDFND,OPEXP,USANT,UNANT
C
C Packed names
C
      INTEGER NMIN,NMAX,NC,NP,NRE,NIM,NCC,NPHASE,NMOD,NMSQ,NRADIA,NDEGRE
      INTEGER NCOS,NACOS,NSIN,NASIN,NTAN,NROOT,NEXP,NLN,NFIX,NROUND,NSET
      INTEGER NAND,NOR,NNOT,NIFELS,NREM
      PARAMETER (NMIN=21174,NMAX=20864,NC=4800)
      PARAMETER (NP=25600,NRE=29000,NIM=14920,NCC=4920,NPHASE=25921)
      PARAMETER (NMOD=21404,NMSQ=21577,NRADIA=28844,NDEGRE=6607)
      PARAMETER (NCOS=5419,NACOS=1735,NSIN=30774,NASIN=2369)
      PARAMETER (NTAN=-55,NROOT=29415,NEXP=8976,NLN=19760,NREM=29013)
      PARAMETER (NFIX=9984,NROUND=29421,NIFELS=14645,NSET=30620)
      PARAMETER (NAND=2164,NOR=24720,NNOT=23020)
C
C Operator priorities
C
      INTEGER MBRA,MCOMMA,MKET,MBAR,MAMPER,MTILDE,MABRA,MAKET,MEQUAL
      INTEGER MPLUS,MMINUS,MSTAR,MSLASH,MUP,MCOLON,MSPARE
      PARAMETER (MBRA=1,MCOMMA=2,MKET=3,MBAR=4,MAMPER=5,MTILDE=6)
      PARAMETER (MABRA=7,MAKET=7,MEQUAL=7,MPLUS=8,MMINUS=8)
      PARAMETER (MSTAR=9,MSLASH=9,MUP=10,MCOLON=11)
      PARAMETER (MSPARE=0)
C
C Total number of functions recognised
C
      INTEGER NFUNS
      PARAMETER (NFUNS=26)
      INTEGER FUNS(NFUNS)
C
      INTEGER IB1(LNBUF/LNINT)
      EQUIVALENCE (RB1,IB1)
C
      EQUIVALENCE (PRIOR(KUCA),FUNS)
      EQUIVALENCE (LASTF(1),LASTF2),(LASTF(2),LASTF3)
      EQUIVALENCE (V,V1),(V(2),V2),(V(3),V3)
C
C Last functions accepting 3 and 2 args respectively
C
      DATA LASTF3/3/,LASTF2/11/
C
C Operator priorities, with function names and operator stack
C equivalenced to save space.  Elements currently spare are so named.
C
      DATA PRIOR/0,0,0,MSPARE,0,MAMPER,0,
     +   MBRA,MKET,MSTAR,MPLUS,MCOMMA,MMINUS,0,MSLASH,
     +   MABRA,MAKET,MEQUAL,7*MSPARE,MCOLON,0,MABRA,MEQUAL,MAKET,0,
     +   0,NP,NRE,NIM,NMIN,NMAX,NAND,NOR,NPHASE,NMOD,NMSQ,NC,NCC,
     +   NCOS,NACOS,NSIN,NASIN,NTAN,NROOT,NEXP,NLN,NFIX,NROUND,NNOT,
     +   NRADIA,NDEGRE,NSET,0,0,0,MUP,0,0,26*MSPARE,0,MBAR,0,MTILDE/
C
C Zero stacks: expect value first
C
      OSPTR=0
      VSPTR=0
      VSTACK(1)=0.0
C
C OPEXP denotes an operator expected next
C
      OPEXP=.FALSE.
C
C USANT denotes a unary minus anticipated
C
      USANT=.TRUE.
C
C UNANT denotes a unary not anticipated
C
      UNANT=.TRUE.
      ENDFND=.FALSE.
      LSET=.FALSE.
      GOTO 20
C
C Classify next object: pick up next non-space
C
   10 PTR=PTR+1
   20 IF (SEMXA1(0,BUFFER,LENGTH,PTR,VALUE,CH)) THEN
C
C Text exhausted: fault if operator not outstanding, otherwise terminate
C
         IF (.NOT.OPEXP) GOTO 620
         GOTO 170
      ENDIF
C
C Sort character: numeric?
C
      IF (CH.GE.KZERO .AND. CH.LE.KNINE) GOTO 30
      IF (CH.EQ.KDOT) GOTO 30
C
C Alphabetic?
C
      IF (CH.GE.KUCA .AND. CH.LE.KUCZ) GOTO 50
      IF (CH.GE.KLCA .AND. CH.LE.KLCZ) GOTO 50
      IF (CH.EQ.KDOLLA) GOTO 50
C
C Other
C
      GOTO 100
C
C Number decoding/stacking
C ------------------------
C
   30 IF (OPEXP) GOTO 170
      IF (SEMXA1(2,BUFFER,LENGTH,PTR,VALUE,J)) CONTINUE
C
C Stack value
C
   40 VSPTR=VSPTR+1
      IF (VSPTR.GT.VSMAX) GOTO 640
      VSTACK(VSPTR)=VALUE
C
C Expect op next
C
      OPEXP=.TRUE.
      USANT=.FALSE.
      UNANT=.FALSE.
      GOTO 20
C
C Name decoding/stacking
C ----------------------
C
   50 IF (OPEXP) GOTO 170
      IF (SEMXA1(1,BUFFER,LENGTH,PTR,VALUE,J)) CONTINUE
      IF (ERROR.NE.0) GOTO 650
C
C BRA following name? (identifies function ref)
C
      IF (SEMXA1(0,BUFFER,LENGTH,PTR,VALUE,K)) GOTO 60
      IF (K.EQ.KBRA) GOTO 70
C
C Variable ref
C
   60 IF (SEMLU(-1,J,VALUE)) THEN
C
C Variable set: if within SET use 1.
C
         IF (LSET) VALUE = 1.
         GOTO 40
      ENDIF
C
C Variable unset: if within SET, or scan mode, use zero
C
      VALUE=0.
      IF (LSET.OR.SCANMD) GOTO 40
C
C Otherwise, fault it
C
      ERROR = 25
      IDERR = J
      GOTO 650
C
C Function ref - name known?
C
   70 DO 80 CH=1,NFUNS
         IF (J .EQ. FUNS(CH)) GOTO 90
   80 CONTINUE
C
      CH=NFUNS+1
      IF (J.EQ.NIFELS) GOTO 90
      CH=NFUNS+2
      IF (J.EQ.NREM) GOTO 90
C
C Function unknown
C
      ERROR = 16
      IDERR = J
      GOTO 650
C
C Yes - if SET, check nesting
C
   90 IF (CH.EQ.NFUNS) THEN
         IF (LSET) GOTO 620
         LSET = .TRUE.
      ENDIF
      PTR = PTR-1
C
C Stack function opcode
C
      GOTO 160
C
C Operator decoding/stacking
C --------------------------
C
  100 IF (OPEXP) GOTO 110
C
C Not expected - but could be bra..
C
      IF (CH.EQ.KBRA) GOTO 160
C
C ..or unary : (stack CD and reprocess as dyadic)..
C
      IF (CH .EQ. KCOLON) THEN
         VALUE = CD
         GOTO 40
C
C ..or unary plus or minus (stack zero and reprocess as dyadic)
C
      ELSE IF (CH.EQ.KPLUS.OR.CH.EQ.KMINUS) THEN
         IF (USANT) THEN
            VALUE = 0.
            GOTO 40
         ENDIF
C
C ..or unary not (stack operator)
C
      ELSE IF (CH.EQ.KTILDE) THEN
         IF (UNANT) GOTO 160
C
C ..or equal following < or > (modify opcode and keep scanning)
C
      ELSE IF (CH.EQ.KEQUAL) THEN
         IF (OSTACK(OSPTR).EQ.KABRA) THEN
            OSTACK(OSPTR)=KZERO
            GOTO 10
         ELSE IF (OSTACK(OSPTR).EQ.KAKET) THEN
            OSTACK(OSPTR)=KONE
            GOTO 10
         ENDIF
      ENDIF
C
C Bad syntax
C
      GOTO 620
C
C Expected: fault ~ (unless followed by =), terminate if not recognised
C
  110 IF (CH.EQ.KTILDE) THEN
         I=PTR+1
         IF (SEMXA1(0,LINBUF,LINLEN,I,VALUE,J)) CONTINUE
         IF (ERROR.NE.0) GOTO 650
         IF (J.EQ.KEQUAL) THEN
            CH=KTWO
            PTR=I
         ELSE
            GOTO 620
         ENDIF
      ENDIF
      IF (CH.EQ.KBRA) GOTO 170
      NEWPRI=PRIOR(CH)
      IF (NEWPRI.EQ.0) GOTO 170
C
C Central logic of module: potential new operator now in CH, but such
C an operator cannot be applied until the next one is found, in case
C that proves to have higher priority.  Accordingly, the new operator
C is compared with the last stacked, so that 'destacking' (application
C of the operator(s) previously stacked, can take place if the new
C operator is (loosely) of lower priority.
C
C Specifically: destack if current stack top is a function
C               destack if current stack top has equal or higher
C                       priority, except that commas require
C                                 special treatment as noted below
C
C Control returns here after a destacking operation to repeat the
C priority comparison
C
  120 IF (OSPTR.LE.0) GOTO 130
      OPCODE=OSTACK(OSPTR)
      IF (OPCODE.LT.31) GOTO 180
C
C Special consideration of commas: avoid destacking when two commas are
C compared
C
      IF (CH.EQ.KCOMMA .AND. OPCODE.EQ.KCOMMA) GOTO 130
      IF (NEWPRI.LE.PRIOR(OPCODE)) GOTO 180
C
C No further destacking for the present: trap comma and ket
C
  130 IF (CH.NE.KCOMMA) GOTO 150
C
C Stack comma if within scope of multi-argument function call;
C otherwise terminate
C
      DO 140 IR=1,2
         OPCODE=OSPTR-IR
         IF (OPCODE.LE.0) GOTO 170
         OPCODE=OSTACK(OPCODE)
         IF (OPCODE.LE.LASTF(IR)) GOTO 160
         IF (OPCODE.EQ.NFUNS+1) GOTO 160
         IF (OPCODE.EQ.NFUNS+2) GOTO 160
  140 CONTINUE
      GOTO 170
C
C If ket, establish number of arguments in case function call,
C remove matching bra, and proceed to next item (which will cause
C immediate destacking)
C
  150 IF (CH.NE.KKET) GOTO 160
      NARGS=1
      IF (OSPTR.LE.0) GOTO 170
      IF (OSTACK(OSPTR).EQ.KCOMMA) THEN
         NARGS = 2
         OSPTR = OSPTR-1
         IF (OSTACK(OSPTR).EQ.KCOMMA) THEN
            NARGS = 3
            OSPTR = OSPTR-1
         ENDIF
      ENDIF
C
C Require bra to match
C
      IF (OSTACK(OSPTR).NE.KBRA) GOTO 620
      OSPTR = OSPTR-1
      OPEXP = .TRUE.
      GOTO 10
C
C Stack operator
C
  160 OSPTR=OSPTR+1
      IF (OSPTR.GT.OSMAX) GOTO 640
      OSTACK(OSPTR)=CH
      OPEXP=.FALSE.
C
C Anticipate unary plus or minus if stacking bra, comma,
C < = > or ~=, ~ & or |
C
      USANT=CH.EQ.KBRA.OR.CH.EQ.KCOMMA.OR.IABS(CH-KEQUAL).LE.1.OR.
     +      CH.EQ.KTILDE.OR.CH.EQ.KAMPER.OR.CH.EQ.KBAR.OR.CH.EQ.KTWO
C
C Anticipate unary not if stacking bra, comma, & or |
C
      UNANT=CH.EQ.KBRA.OR.CH.EQ.KCOMMA.OR.CH.EQ.KAMPER.OR.CH.EQ.KBAR
      GOTO 10
C
C Expression terminated
C
  170 ENDFND=.TRUE.
C
C Destacking: apply stacked operator to stacked operand(s)
C ----------
C Make normal exit if operator stack empty
C
  180 IF (OSPTR.EQ.0) GOTO 650
C
C Pick up last stacked operator
C
      OPCODE=OSTACK(OSPTR)
      OSPTR=OSPTR-1
C
C Pick up last stacked items (NARGS if function, 2 if operator, 1 if ~)
C
      IF (OPCODE.GT.NFUNS+2) NARGS=2
      IF (OPCODE.EQ.KTILDE) NARGS=1
      VSPTR=VSPTR-NARGS+1
C
C Check for stack exhaustion
C
      IF (VSPTR.LE.0) GOTO 620
      IR=VSPTR
      DO 190 I=1,NARGS
         V(I)=VSTACK(IR)
         IR=IR+1
  190 CONTINUE
C
C If scan mode, skip operation itself
C
      VALUE=V1
      IF (SCANMD) GOTO 610
C
C Function C must have just two arguments
C
      IF (OPCODE.EQ.11.AND.NARGS.NE.2) GOTO 620
C
C Otherwise, select appropriate code
C
      GOTO (200,200,200,210,220,230,240,300,320,310,610,610,250,260,
     +      270,280,290,330,340,350,370,360,380,390,400,430,410,420),
     +     OPCODE
      IF (OPCODE.EQ.KUP) GOTO 440
      IF (OPCODE.EQ.KBAR) GOTO 580
      IF (OPCODE.EQ.KTILDE) GOTO 590
      IF (OPCODE.EQ.KCOLON) GOTO 600
      IF (OPCODE.EQ.KABRA) GOTO 490
      IF (OPCODE.EQ.KEQUAL) GOTO 500
      IF (OPCODE.EQ.KAKET) GOTO 510
      OPCODE=OPCODE-KAMPER+1
      GOTO (570,620,620,620,450,460,620,470,450,480,520,530,540),
     +     OPCODE
C
C Functions
C ---------
C P/Re/Im - open SELECT if necessary
C
  200 IDERR = SELECT
      IF (BUFROW .EQ. 0) THEN
         IF (SEMOPN(1,IDERR,J,K,L,CLASS,FORM,BUFLPN)) GOTO 650
C
C Override byte form
C
         IF (FORM .EQ. NFMBYT) FORM = NFMINT
      ELSE
         FORM = BUFFRM
      ENDIF
C
C Default coordinates
C
      IF (NARGS.LT.3) V3 = 0.
      IF (NARGS.LT.2) V2 = 0.
C
C Check X within range
C
      K = CCOLN(BUFLPN) + NINT(V1)
      IF (K .LT. 1 .OR. K .GT. NCOLS(BUFLPN)) THEN
C
C Outside picture
C
         ERROR = 9
         GOTO 650
      ENDIF
C
C Fetch row, unless already buffered
C
      J = CROWN(BUFLPN) - NINT(V2)
      L = CLAYN(BUFLPN) + NINT(V3)
      IF (BUFROW .EQ. 0 .OR. J .NE. IABS(BUFROW) .OR.
     +    L .NE. BUFLAY .OR. BUFFRM .NE. FORM) THEN
C
C New current row needed; dump present one if written-to
C
         IF (SEMDCR(0)) GOTO 650
         IF (SEMROW(1,RB1,FORM,J,L,BUFLPN)) GOTO 650
C
C Record new current row
C
         BUFROW = J
         BUFLAY = L
         BUFFRM = FORM
      ENDIF
C
C Now extract value
C
      IF (BUFFRM .EQ. NFMINT) THEN
         IF (OPCODE .EQ. 3) THEN
            VALUE = 0.0
         ELSE
            VALUE = REAL(IB1(K))
         ENDIF
      ELSE
         IF (BUFFRM .EQ. NFMCOM) THEN
            K = K + K
            IF (OPCODE .NE. 3) K = K - 1
         ENDIF
         VALUE = RB1(K)
      ENDIF
      GOTO 610
C
C Min
C
  210 IF (NARGS .NE. 2) GOTO 620
      VALUE = MIN(V1,V2)
      GOTO 610
C
C Max
C
  220 IF (NARGS .NE. 2) GOTO 620
      VALUE = MAX(V1,V2)
      GOTO 610
C
C And
C
  230 IF (NARGS .NE. 2) GOTO 620
      VALUE = REAL(IAND(NINT(V1),NINT(V2)))
      GOTO 610
C
C Or
C
  240 IF (NARGS .NE. 2) GOTO 620
      VALUE = REAL(IOR(NINT(V1),NINT(V2)))
      GOTO 610
C
C Cos
C
  250 VALUE = COS(V1)
      GOTO 610
C
C Acos
C
  260 IF (ABS(V1) .GT. 1.0) GOTO 630
      VALUE = ACOS(V1)
      GOTO 610
C
C Sin
C
  270 VALUE = SIN(V1)
      GOTO 610
C
C Asin
C
  280 IF (ABS(V1) .GT. 1.0) GOTO 630
      VALUE = ASIN(V1)
      GOTO 610
C
C Tan
C
  290 VALUE = TAN(V1)
      GOTO 610
C
C Phase
C
  300 IF (NARGS .EQ. 1) THEN
         VALUE = ATAN2(V1,1.0)
      ELSE
         IF (V1.NE.0.0 .OR. V2.NE.0.0) THEN
            VALUE = ATAN2(V2,V1)
         ELSE
            VALUE = 0.0
         ENDIF
      ENDIF
      GOTO 610
C
C Msq
C
  310 VALUE = V1*V1
      IF (NARGS .NE. 1) VALUE = VALUE + V2*V2
      GOTO 610
C
C Mod
C
  320 IF (NARGS .EQ. 1) THEN
         VALUE = ABS(V1)
      ELSE
         VALUE = SQRT(V1*V1+V2*V2)
      ENDIF
      GOTO 610
C
C Root
C
  330 IF (V1 .LT. 0.0) THEN
C
C Root of negative number
C
         ERROR = 96
         GOTO 650
      ENDIF
      VALUE = SQRT(V1)
      GOTO 610
C
C Exp
C
  340 VALUE = EXP(V1)
      GOTO 610
C
C Ln
C
  350 IF (V1 .LE. 0.0) THEN
C
C Ln of negative or zero
C
         ERROR = 97
         GOTO 650
      ENDIF
      VALUE = ALOG(V1)
      GOTO 610
C
C Round
C
  360 VALUE = ANINT(V1)
      GOTO 610
C
C Int
C
  370 VALUE = AINT(V1)
      GOTO 610
C
C Not
C
  380 VALUE = REAL(NOT(NINT(V1)))
      GOTO 610
C
C Radian
C
  390 VALUE = PI*V1/180.0
      GOTO 610
C
C Degree
C
  400 VALUE = 180.0*V1/PI
      GOTO 610
C
C Ifelse
C
  410 IF (NARGS.NE.3) GOTO 620
      IF (V1 .NE. 0.0) THEN
         VALUE = V2
      ELSE
         VALUE = V3
      ENDIF
      GOTO 610
C
C Rem
C
  420 IF (NARGS.NE.2) GOTO 620
      VALUE=MOD(V1,V2)
      GOTO 610
C
C Special function SET
C
  430 LSET = .FALSE.
      GOTO 610
C
C Operators
C ---------
C ^
C
  440 IF (V1 .NE. 0.0) THEN
         IF (V2 .EQ. AINT(V2)) THEN
            IF (INT(V2) .EQ. 0) THEN
               VALUE = 1.0
            ELSE
               VALUE = V1**INT(V2)
            ENDIF
         ELSE
            IF (V1.LT.0.0) THEN
C
C Fractional power of negative number
C
               ERROR = 99
               GOTO 650
            ENDIF
            VALUE = EXP(V2*ALOG(V1))
         ENDIF
      ELSE
         VALUE = 0.0
      ENDIF
      GOTO 610
C
C *
C
  450 VALUE = V1*V2
      GOTO 610
C
C +
C
  460 VALUE = V1+V2
      GOTO 610
C
C -
C
  470 VALUE = V1-V2
      GOTO 610
C
C /
C
  480 IF (V2 .EQ. 0.0) THEN
C
C Divide by zero
C
         ERROR = 95
         GOTO 650
      ENDIF
      VALUE = V1/V2
      GOTO 610
C
C <
C
  490 IF (V1.LT.V2) GOTO 560
      GOTO 550
C
C =
C
  500 IF (V1.EQ.V2) GOTO 560
      GOTO 550
C
C >
C
  510 IF (V1.GT.V2) GOTO 560
      GOTO 550
C
C <=
C
  520 IF (V1.LE.V2) GOTO 560
      GOTO 550
C
C >=
C
  530 IF (V1.GE.V2) GOTO 560
      GOTO 550
C
C ~=
C
  540 IF (V1.NE.V2) GOTO 560
C
C Logical FALSE = zero VALUE = 0
C
  550 VALUE = 0.0
      GOTO 610
C
C Logical TRUE = non-zero value = 1
C
  560 VALUE = 1.0
      GOTO 610
C
C &
C
  570 IF (V1.EQ.0.0 .OR. V2.EQ.0.0) GOTO 550
      GOTO 560
C
C |
C
  580 IF (V1.NE.0.0 .OR. V2.NE.0.0) GOTO 560
      GOTO 550
C
C ~
C
  590 IF (V1.EQ.0.0) GOTO 560
      GOTO 550
C
C :
C
  600 V1 = ANINT(V1)
      IF (V1.GT.999.0) V1 = AINT(V1/1000.0)
      V2 = ANINT(V2)
      IF (V2.GT.999.0) V2 = AMOD(V2,1000.0)
      IF (V1.LT.1.0 .OR. V1.GT.REAL(NDVS) .OR. V2.LT.0.0) THEN
C
C Device or picture number out of range
C
         ERROR = 116
         GOTO 650
      ENDIF
      VALUE=1000.0*V1+V2
C
C Return result to stack
C
  610 VSTACK(VSPTR)=VALUE
C
C Return to central logic so as to make next stack/destack decision
C
      IF (.NOT.ENDFND) GOTO 120
      GOTO 180
C
C Errors: bad expression
C
  620 ERROR = 20
      GOTO 650
C
C ACOS/ASIN out of range
C
  630 ERROR = 98
      GOTO 650
C
C Expression stack overflow
C
  640 ERROR = 18
C
C Finish
C
  650 RESULT = VSTACK(1)
      SEMEXP = ERROR .NE. 0
      RETURN
C
C Copyright (C) 1987-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
