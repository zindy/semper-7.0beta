C Semper 6 processing module LMANU
C
      SUBROUTINE LMANU
C
C Look-up table utility: verb LUT in all its different guises
C
      LOGICAL VARSET,SEMLUT,FSLU61,SEMOPN,SEMROW,SEMDPD,SEMDPN,LMANUR
      LOGICAL OPT,SEMLU
      LOGICAL INKEY,SEMCON,SEMPRO,SEMSOP
      INTEGER IVAL,IVALPN
      REAL VAL
C
      INCLUDE 'COMMON'
C
      LOGICAL LCREAT,LCOPY,LFROM,LINPUT,LHIGHL
      LOGICAL LNAME(4),LSCALE
      INTEGER LUTCOL(3),LUTROW(3),LUTLAY(3)
      INTEGER CLASS,FORM,DEVICE,PARTN
      INTEGER INAME(4),CNAME(4),OPTSEL,RGBSEL
      LOGICAL LADJUS,LCHANG,LCONTI,LFINE,LSELEC
      LOGICAL LOFF(3),NEWLHI(3),OLDLHI(3)
      INTEGER LUTCHN(4)
      INTEGER NEWLR1(3),NEWLR2(3),NEWLH1(3),NEWLH2(3)
      INTEGER OLDLR1(3),OLDLR2(3),OLDLH1(3),OLDLH2(3)
      INTEGER AMODE,SMODE
      REAL OLDB(3),OLDC(3),NEWB(3),NEWC(3)
      CHARACTER*5 COLSTR(4)
      CHARACTER*10 MODSTR(3)
      CHARACTER*11 ADJSTR(4)
      CHARACTER*12 SELSTR(4)
C
      INTEGER IB1(LNBUF/LNINT),LUT(3*LUTSIZ)
C
      EQUIVALENCE (IB1,RB1),(LUT,RB4)
      EQUIVALENCE (LCREAT,LNAME(1)),(LCOPY,LNAME(2)),(LFROM,LNAME(3))
      EQUIVALENCE (LINPUT,LNAME(4))
C
      INTEGER NUMBER,I,LR1,LR2,MODE,NPIC,NCOL,NROW,NLAY,L,K,J
      INTEGER LH1,LH2
      REAL B,C
      INTEGER IDIR,ISTEP,ISTEP1,ISTEP2,KEY,L1,L2,LSTEP
      REAL BSTEP,CSTEP,H1,H2,R1,R2
C
C Packed names
C
      INTEGER NNUMBE,NDELET,NTO,NCREAT,NCOPY,NFROM,NINPUT,NBRIGH
      INTEGER NCONTR,NRESET,NINVER,NZERO,NKEYS,NSCALE,NRANGE,NRA2
      INTEGER NHIGHL,NHI2,NMONOC,NFALSE,NCOLOU,NRED,NGREEN,NBLUE,NALL
      INTEGER NB,NC,NR,NR2,NH,NH2,NENQUI,NLSIZE,NLMAX
      PARAMETER (NNUMBE=23253, NENQUI=8577, NDELET=6612, NTO=-601)
      PARAMETER (NCREAT=5525, NCOPY=5416, NFROM=10335, NINPUT=14976)
      PARAMETER (NBRIGH=3929, NCONTR=5414, NRESET=29019, NINVER=14982)
      PARAMETER (NZERO=-9819, NKEYS=17825, NSCALE=30521)
      PARAMETER (NRANGE=28854, NRA2=28872, NHIGHL=13167, NHI2=13192)
      PARAMETER (NMONOC=21414, NFALSE=9652, NCOLOU=5412)
      PARAMETER (NRED=29004, NGREEN=11925, NBLUE=3701, NALL=2092)
      PARAMETER (NB=3200, NC=4800, NLSIZE=19969, NLMAX=19721)
      PARAMETER (NR=28800, NR2=30080, NH=12800, NH2=14080)
C
C Key assignments
C
      INTEGER KLBRIG,KLCONT,KLRANG,KLHIGH,KLMEAN,KLSEPA,KLUPPE,KLLOWE
      INTEGER KLINVE,KLZERO,KLOFF,KLFINE,KLDELE,KLVERI,KLEXIT,KLQUIT
      INTEGER KLPLUS,KLMINU,KLHELP,KLRED,KLGREE,KLBLUE,KLALL,KLRETU
      INTEGER KLRIGH,KLLEFT
      PARAMETER (KLBRIG=KUCB, KLCONT=KUCC, KLRANG=KUCR, KLHIGH=KUCH)
      PARAMETER (KLMEAN=KUCM, KLSEPA=KUCS, KLUPPE=KUCU, KLLOWE=KUCL)
      PARAMETER (KLINVE=KUCI, KLZERO=KUCZ, KLOFF =KUCO, KLFINE=KUCF)
      PARAMETER (KLDELE=KUCD, KLVERI=KUCV, KLEXIT=KUCE, KLQUIT=KUCQ)
      PARAMETER (KLPLUS=KAKET, KLMINU=KABRA, KLHELP=KQUEST)
      PARAMETER (KLRED=KONE, KLGREE=KTWO, KLBLUE=KTHREE, KLALL=KUCA)
      PARAMETER (KLRETU=KBRET ,KLRIGH=KBRITE, KLLEFT=KBLEFT)
C
      DATA LUTCOL,LUTROW,LUTLAY /1,3,3,1,3,1,1,1,3/
      DATA INAME /NCREAT,NCOPY,NFROM,NINPUT/
      DATA CNAME /NRED,NGREEN,NBLUE,NALL/
      DATA LUTCHN /1,2,3,1/
      DATA MODSTR /'monochrome','false','colour'/
      DATA COLSTR /'red','green','blue','all'/
      DATA SELSTR /'brightness','contrast','range','highlighting'/
      DATA ADJSTR /'mean','separation','upper limit','lower limit'/
C
C Code for option ENQUIRE
C
      IF (OPT(NENQUI)) THEN
C
C Return values for look-up table size and maximum output value in
C variables LSIZE and LMAX
C
         IF (SEMLU(1,NLSIZE,REAL(LUTLEN))) GOTO 240
         IF (SEMLU(1,NLMAX,REAL(LUTMAX))) GOTO 240
         GOTO 240
      ENDIF
C
C Fault illegal look-up table number
C
      NUMBER=IVAL(NNUMBE)
      IF (NUMBER.LT.1.OR.NUMBER.GT.NLUTS) THEN
         ERROR=68
         IDERR=NUMBER
         GOTO 240
      ENDIF
C
C Code for option DELETE
C
      IF (OPT(NDELET)) THEN
C
C Set LUT mode as undefined (marks LUT as non-existent)
C
         LUTMOD(NUMBER)=0
C
         GOTO 240
      ENDIF
C
C See if options CREATE or INPUT, or keys COPY or FROM are set
C
      LCREAT=OPT(NCREAT)
      LCOPY=VARSET(NCOPY)
      LFROM=VARSET(NFROM)
      LINPUT=OPT(NINPUT)
C
C Fault conflicts between any of the above options and keys
C
      OPTSEL=0
      DO 10 I=1,4
         IF (LNAME(I)) THEN
            IF (OPTSEL.EQ.0) THEN
               OPTSEL=I
            ELSE
               ERROR=60
               IDERR=INAME(OPTSEL)
               IDERR2=INAME(I)
               GOTO 240
            ENDIF
         ENDIF
   10 CONTINUE
C
C Process key SCALED
C
      LSCALE=VARSET(NSCALE)
      IF (LSCALE) THEN
C
C See if key SCALED is set to valid display partition number
C
         IF (SEMDPN(IVAL(NSCALE),DEVICE,PARTN)) GOTO 240
C
C Fetch required DPD from work disc
C
         IF (SEMDPD(1,PARTN)) GOTO 240
C
C Fault display partition not containing a 2-D picture
C
         IF (DPTYP.NE.1) THEN
            ERROR=48
            IDERR=1000*DEVICE+PARTN
            GOTO 240
         ENDIF
      ENDIF
C
C Process keys RANGE and RA2
C
      IF (LMANUR(NRANGE,NRA2,LR1,LR2)) GOTO 240
C
C Process keys BRIGHTNESS (range 0.0 to 1.0) and CONTRAST (range -1.0
C to 1.0)
C
      B=MAX(0.0,MIN(VAL(NBRIGH),1.0))
      C=MAX(-1.0,MIN(VAL(NCONTR),1.0))
C
C Process options MONOCHROME, FALSE and COLOUR
C
      IF (OPT(NMONOC)) THEN
         MODE=1
      ELSE IF (OPT(NFALSE)) THEN
         MODE=2
      ELSE IF (OPT(NCOLOU)) THEN
         MODE=3
      ELSE
         MODE=0
      ENDIF
C
C Code for option CREATE
C
      IF (LCREAT) THEN
C
C LUT mode defaults to MONOCHROME if no LUT mode options set
C
C       LDM Change, let's go 21st century and default to colour
C         IF (MODE.EQ.0) MODE=1
         IF (MODE.EQ.0) MODE=3
C
C Set up new LUT values according to specified mode
C
         IF (MODE.EQ.1) THEN
            CALL LMANU2(LUT,B,C,LR1,LR2)
         ELSE IF (MODE.EQ.2) THEN
            CALL LMANUF(LUT,LR1,LR2)
         ELSE
            DO 20 I=1,3
               CALL LMANU2(LUT(1+(I-1)*LUTLEN),B,C,LR1,LR2)
   20       CONTINUE
         ENDIF
C
C Code for key COPY
C
      ELSE IF (LCOPY) THEN
C
C Fetch LUT specified by COPY key from work disc
C
         IF (SEMLUT(1,IVAL(NCOPY),MODE,LUT)) GOTO 240
C
C Code for key FROM
C
      ELSE IF (LFROM) THEN
C
C Open old picture containing LUT
C
         NPIC=IVALPN(NFROM)
         IF (SEMOPN(1,NPIC,NCOL,NROW,NLAY,CLASS,FORM,LP1)) GOTO 240
C
C Fault non-LUT class source picture
C
         IF (CLASS.NE.NCLLUT) THEN
            ERROR=6
            IDERR=NPIC
            GOTO 240
         ENDIF
C
C Determine LUT mode from source picture dimensions
C
         MODE=0
         IF (NCOL.EQ.LUTLEN) THEN
            DO 30 I=1,3
               IF (NROW.EQ.LUTROW(I).AND.NLAY.EQ.LUTLAY(I)) MODE=I
   30       CONTINUE
         ENDIF
C
C Fault bad size for LUT class source picture
C
         IF (MODE.EQ.0) THEN
            ERROR=5
            IDERR=NPIC
            GOTO 240
         ENDIF
C
C Copy LUT data from source picture into temporary buffer
C
         L=1
         DO 60 K=1,NLAY
            DO 50 J=1,NROW
C
C Read row from source picture
C
               IF (SEMROW(1,RB1,NFMINT,J,K,LP1)) GOTO 240
C
C Copy LUT data from row buffer into temporary LUT buffer
C
               DO 40 I=1,NCOL
                  LUT(L)=IB1(I)
                  L=L+1
   40          CONTINUE
   50       CONTINUE
   60    CONTINUE
C
C Code for option INPUT
C
      ELSE IF (LINPUT) THEN
C
C Try to read LUT from hardware
C
         IF (FSLU61(1,NUMBER,MODE,LUT,ERROR)) GOTO 240
C
C Otherwise, just fetch LUT from work disc
C
      ELSE
         IF (SEMLUT(1,NUMBER,MODE,LUT)) GOTO 240
      ENDIF
C
C Fault non-existent LUT
C
      IF (MODE.EQ.0) THEN
         ERROR=69
         IF (LCOPY) THEN
            IDERR=IVAL(NCOPY)
         ELSE
            IDERR=NUMBER
         ENDIF
         GOTO 240
      ENDIF
C
C Process options ALL, RED, GREEN and BLUE
C
      RGBSEL=0
C
C See if any of these options set, faulting any conflicts
C
      DO 70 I=1,4
         IF (OPT(CNAME(I))) THEN
            IF (MODE.EQ.1) THEN
               ERROR=64
               IDERR=CNAME(I)
               GOTO 240
            ENDIF
C
            IF (RGBSEL.EQ.0) THEN
               RGBSEL=I
            ELSE
               ERROR=60
               IDERR=CNAME(RGBSEL)
               IDERR2=CNAME(I)
               GOTO 240
            ENDIF
         ENDIF
   70 CONTINUE
C
C If none of these options set, default is ALL
C
      IF (RGBSEL.EQ.0) RGBSEL=4
C
C Process option RESET
C
      IF (OPT(NRESET)) THEN
C
C Set required LUT channels to linear grey-scale
C
         DO 80 I=1,LUTCOL(MODE)
            IF (I.EQ.RGBSEL.OR.RGBSEL.EQ.4) THEN
               CALL LMANU2(LUT(1+(I-1)*LUTLEN),B,C,LR1,LR2)
            ENDIF
   80    CONTINUE
      ENDIF
C
C Process option INVERT
C
      IF (OPT(NINVER)) THEN
C
C Invert required LUT channels
C
         DO 100 I=1,LUTCOL(MODE)
            IF (I.EQ.RGBSEL.OR.RGBSEL.EQ.4) THEN
               DO 90 L=1+(I-1)*LUTLEN,I*LUTLEN
                  LUT(L)=LUTMAX-LUT(L)
   90          CONTINUE
            ENDIF
  100    CONTINUE
      ENDIF
C
C Process option ZERO
C
      IF (OPT(NZERO)) THEN
C
C Zero required LUT channels
C
         DO 120 I=1,LUTCOL(MODE)
            IF (I.EQ.RGBSEL.OR.RGBSEL.EQ.4) THEN
               DO 110 L=1+(I-1)*LUTLEN,I*LUTLEN
                  LUT(L)=0
  110          CONTINUE
            ENDIF
  120    CONTINUE
      ENDIF
C
C Code for keys HIGHLIGHT and HI2
C
      LHIGHL=VARSET(NHIGHL)
      IF (LHIGHL) THEN
C
C Process keys HIGHLIGHT and HI2
C
         IF (LMANUR(NHIGHL,NHI2,LH1,LH2)) GOTO 240
C
C Set highlighting in LUT
C
         DO 130 I=1,LUTCOL(MODE)
            IF (I.EQ.RGBSEL.OR.RGBSEL.EQ.4) THEN
               CALL LMANU3(LUT(1+(I-1)*LUTLEN),LH1,LH2,LUTMAX)
            ENDIF
  130    CONTINUE
      ENDIF
C
C Write LUT to work disc
C
      IF (SEMLUT(2,NUMBER,MODE,LUT)) GOTO 240
C
C Code for option KEYS
C
      IF (OPT(NKEYS)) THEN
C
C Fault LUT not currently active
C
         IF (NUMBER.NE.CLUT) THEN
            ERROR=163
            IDERR=NUMBER
            GOTO 240
         ENDIF
C
C Examine and tidy up contents of LUT
C
         DO 140 I=1,LUTCOL(MODE)
C
C Determine parameters for LUT channel
C
            CALL LMANU4(LUT(1+(I-1)*LUTLEN),B,C,LR1,LR2,LH1,LH2,LHIGHL)
C
C Save initial LUT parameters
C
            OLDB(I)=B
            OLDC(I)=C
            OLDLR1(I)=LR1
            OLDLR2(I)=LR2
            OLDLH1(I)=LH1
            OLDLH2(I)=LH2
            OLDLHI(I)=LHIGHL
C
C Initialise copy of current LUT parameters
C
            NEWB(I)=B
            NEWC(I)=C
            NEWLR1(I)=LR1
            NEWLR2(I)=LR2
            NEWLH1(I)=LH1
            NEWLH2(I)=LH2
            NEWLHI(I)=LHIGHL
  140    CONTINUE
C
C Initialise parameters and flags
C
         SMODE=1
         AMODE=1
         LFINE=.FALSE.
         DO 150 I=1,LUTCOL(MODE)
            LOFF(I)=.FALSE.
  150    CONTINUE
C
C Output KEYS prompt message
C
         IF (SEMPRO('Keys active: ? for help, E or RETURN to exit'))
     +      GOTO 240
C
C Set LUT channel select flag to start things off
C
         LSELEC=.TRUE.
C
C If LUT channel selected, bring in LUT parameters for that channel
C
  160    IF (LSELEC) THEN
C
C Determine channel number
C
            I=LUTCHN(RGBSEL)
C
C Fetch LUT parameters for that channel
C
            B=NEWB(I)
            C=NEWC(I)
            LR1=NEWLR1(I)
            LR2=NEWLR2(I)
            LH1=NEWLH1(I)
            LH2=NEWLH2(I)
            LHIGHL=NEWLHI(I)
C
C Flag change of LUT parameters
C
            LCHANG=.TRUE.
         ENDIF
C
C If any changes, regenerate LUT and pass on changes to display hardware
C
         IF (LCHANG) THEN
C
C Process required LUT channels
C
            DO 180 I=1,LUTCOL(MODE)
               IF (I.EQ.RGBSEL.OR.RGBSEL.EQ.4) THEN
C
C If LUT channel switched off, set all LUT values to zero
C
                  IF (LOFF(I)) THEN
                     DO 170 L=1+(I-1)*LUTLEN,I*LUTLEN
                        LUT(L)=0
  170                CONTINUE
C
C Otherwise, regenerate proper LUT values
C
                  ELSE
C
C Regenerate basic grey-scale
C
                     CALL LMANU2(LUT(1+(I-1)*LUTLEN),B,C,LR1,LR2)
C
C Add highlighting band if required
C
                     IF (LHIGHL) THEN
                        CALL LMANU3(LUT(1+(I-1)*LUTLEN),LH1,LH2,LUTMAX)
                     ENDIF
                  ENDIF
C
C Update copy of current LUT parameters
C
                  NEWB(I)=B
                  NEWC(I)=C
                  NEWLR1(I)=LR1
                  NEWLR2(I)=LR2
                  NEWLH1(I)=LH1
                  NEWLH2(I)=LH2
                  NEWLHI(I)=LHIGHL
               ENDIF
  180       CONTINUE
C
C Pass on any changes to display hardware
C
            IF (FSLU61(2,NUMBER,MODE,LUT,ERROR)) GOTO 240
         ENDIF
C
C Reset line count for terminal output
C
         IF (SEMSOP()) GOTO 240
C
C Read next key from keyboard
C
         IF (INKEY(KEY,ERROR)) GOTO 240
C
C Convert lower-case to upper-case
C
         IF (KEY.GE.KLCA.AND.KEY.LE.KLCZ) KEY=KEY+(KUCA-KLCA)
C
C Initialise flags
C
         LADJUS=.FALSE.
         LSELEC=.FALSE.
         LCHANG=.FALSE.
         LCONTI=.TRUE.
C
C Key = <Right angle bracket> or <Right arrow> = Increment value
C
         IF (KEY.EQ.KLPLUS.OR.KEY.EQ.KLRIGH) THEN
            IDIR=1
            LADJUS=.TRUE.
C
C Key = <Left angle bracket> or <Left arrow> = Decrement value
C
         ELSE IF (KEY.EQ.KLMINU.OR.KEY.EQ.KLLEFT) THEN
            IDIR=-1
            LADJUS=.TRUE.
C
C Key = <B>rightness
C
         ELSE IF (KEY.EQ.KLBRIG) THEN
            SMODE=1
C
C Key = <C>ontrast
C
         ELSE IF (KEY.EQ.KLCONT) THEN
            SMODE=2
C
C Key = <R>ange
C
         ELSE IF (KEY.EQ.KLRANG) THEN
            SMODE=3
C
C Key = <H>ighlighting
C
         ELSE IF (KEY.EQ.KLHIGH) THEN
C
C If highlighting currently selected, switching highlighting on/off
C
            IF (SMODE.EQ.4) THEN
               LCHANG=.TRUE.
               LHIGHL=.NOT.LHIGHL
C
C Otherwise, switch highlighting on
C
            ELSE
               LCHANG=.NOT.LHIGHL
               LHIGHL=.TRUE.
            ENDIF
C
            SMODE=4
C
C Key = <M>ean
C
         ELSE IF (KEY.EQ.KLMEAN) THEN
            AMODE=1
C
C Key = <S>eparation
C
         ELSE IF (KEY.EQ.KLSEPA) THEN
            AMODE=2
C
C Key = <U>pper
C
         ELSE IF (KEY.EQ.KLUPPE) THEN
            AMODE=3
C
C Key = <L>ower
C
         ELSE IF (KEY.EQ.KLLOWE) THEN
            AMODE=4
C
C Key = <1> = Red
C
         ELSE IF (KEY.EQ.KLRED) THEN
            IF (MODE.NE.1) THEN
               RGBSEL=1
               LSELEC=.TRUE.
            ENDIF
C
C Key = <2> = Green
C
         ELSE IF (KEY.EQ.KLGREE) THEN
            IF (MODE.NE.1) THEN
               RGBSEL=2
               LSELEC=.TRUE.
            ENDIF
C
C Key = <3> = Blue
C
         ELSE IF (KEY.EQ.KLBLUE) THEN
            IF (MODE.NE.1) THEN
               RGBSEL=3
               LSELEC=.TRUE.
            ENDIF
C
C Key = <A>ll
C
         ELSE IF (KEY.EQ.KLALL) THEN
            IF (MODE.NE.1) THEN
               RGBSEL=4
               LSELEC=.TRUE.
            ENDIF
C
C Key = <I>nvert
C
         ELSE IF (KEY.EQ.KLINVE) THEN
            B=1.0-B
            C=-C
            LCHANG=.TRUE.
C
C Key = <Z>ero
C
         ELSE IF (KEY.EQ.KLZERO) THEN
            B=0.0
            C=1.0
            LCHANG=.TRUE.
C
C Key = <O>ff
C
         ELSE IF (KEY.EQ.KLOFF)  THEN
C
C Switch required LUT channels on/off
C
            DO 190 I=1,LUTCOL(MODE)
               IF (I.EQ.RGBSEL.OR.RGBSEL.EQ.4) THEN
                  LOFF(I)=.NOT.LOFF(I)
               ENDIF
  190       CONTINUE
C
            LCHANG=.TRUE.
C
C Key = <D>elete
C
         ELSE IF (KEY.EQ.KLDELE) THEN
C
C Determine channel number
C
            I=LUTCHN(RGBSEL)
C
C Reset LUT parameters to their original settings
C
            B=OLDB(I)
            C=OLDC(I)
            LR1=OLDLR1(I)
            LR2=OLDLR2(I)
            LH1=OLDLH1(I)
            LH2=OLDLH2(I)
            LHIGHL=OLDLHI(I)
C
            LCHANG=.TRUE.
C
C Key = <F>ine
C
         ELSE IF (KEY.EQ.KLFINE) THEN
            LFINE=.NOT.LFINE
C
C Key = <E>xit or <Return>
C
         ELSE IF (KEY.EQ.KLEXIT.OR.KEY.EQ.KLRETU) THEN
C
C If LUT channel temporarily switched off, restore its contents
C
            DO 200 I=1,LUTCOL(MODE)
               IF (I.EQ.RGBSEL.OR.RGBSEL.EQ.4) THEN
                  IF (LOFF(I)) THEN
                     CALL LMANU2(LUT(1+(I-1)*LUTLEN),B,C,LR1,LR2)
                     IF (LHIGHL) THEN
                        CALL LMANU3(LUT(1+(I-1)*LUTLEN),LH1,LH2,LUTMAX)
                     ENDIF
                  ENDIF
               ENDIF
  200       CONTINUE
C
C Store changed LUT on work disc
C
            IF (SEMLUT(2,NUMBER,MODE,LUT)) GOTO 240
C
C Return basic LUT parameters in varibles B, C, LR1 and LR2, scaling
C LR1 and LR2 if necessary
C
            IF (SEMLU(1,NB,B)) GOTO 240
            IF (SEMLU(1,NC,C)) GOTO 240
C
            IF (LSCALE) THEN
               R1=DPMIN+(DPMAX-DPMIN)*REAL(LR1-1)/REAL(LUTLEN-1)
               R2=DPMIN+(DPMAX-DPMIN)*REAL(LR2-1)/REAL(LUTLEN-1)
            ELSE
               R1=REAL(LR1-1)
               R2=REAL(LR2-1)
            ENDIF
C
            IF (SEMLU(1,NR,R1)) GOTO 240
            IF (SEMLU(1,NR2,R2)) GOTO 240
C
C If highlighting also present, return values in variables LH1 and LH2,
C scaling them if necessary
C
            IF (LHIGHL) THEN
               IF (LSCALE) THEN
                  H1=DPMIN+(DPMAX-DPMIN)*REAL(LH1-1)/REAL(LUTLEN-1)
                  H2=DPMIN+(DPMAX-DPMIN)*REAL(LH2-1)/REAL(LUTLEN-1)
               ELSE
                  H1=REAL(LH1-1)
                  H2=REAL(LH2-1)
               ENDIF
C
               IF (SEMLU(1,NH,H1)) GOTO 240
               IF (SEMLU(1,NH2,H2)) GOTO 240
            ENDIF
C
            LCONTI=.FALSE.
C
C Key = <Q>uit
C
         ELSE IF (KEY.EQ.KLQUIT) THEN
C
C Fetch original LUT values from work disc
C
            IF (SEMLUT(1,NUMBER,MODE,LUT)) GOTO 240
C
            LCONTI=.FALSE.
C
C Key = <V>erify
C
         ELSE IF (KEY.EQ.KLVERI) THEN
C
C Construct and output status string
C
            RECORD='Status:'
C
            CALL LMANU5(RECORD,MODSTR(MODE))
C
            IF (MODE.NE.1) CALL LMANU5(RECORD,COLSTR(RGBSEL))
C
            IF (LOFF(LUTCHN(RGBSEL))) THEN
               CALL LMANU5(RECORD,'off')
            ELSE
               CALL LMANU5(RECORD,'on')
            ENDIF
C
            CALL LMANU5(RECORD,SELSTR(SMODE))
C
            IF (SMODE.GE.3) CALL LMANU5(RECORD,ADJSTR(AMODE))
C
            IF (LFINE) THEN
               CALL LMANU5(RECORD,'fine')
            ELSE
               CALL LMANU5(RECORD,'coarse')
            ENDIF
C
            IF (SEMCON(RECORD)) GOTO 240
C
C Determine range values
C
            IF (LSCALE) THEN
               R1=DPMIN+(DPMAX-DPMIN)*REAL(LR1-1)/REAL(LUTLEN-1)
               R2=DPMIN+(DPMAX-DPMIN)*REAL(LR2-1)/REAL(LUTLEN-1)
            ELSE
               R1=REAL(LR1-1)
               R2=REAL(LR2-1)
            ENDIF
C
C Output brightness, contrast and range values
C
            WRITE (RECORD,250) B,C,R1,R2
            IF (SEMCON(RECORD)) GOTO 240
C
C Output highlight range, if highlighting enabled
C
            IF (LHIGHL) THEN
               IF (LSCALE) THEN
                  H1=DPMIN+(DPMAX-DPMIN)*REAL(LH1-1)/REAL(LUTLEN-1)
                  H2=DPMIN+(DPMAX-DPMIN)*REAL(LH2-1)/REAL(LUTLEN-1)
               ELSE
                  H1=REAL(LH1-1)
                  H2=REAL(LH2-1)
               ENDIF
C
               WRITE (RECORD,260) H1,H2
               IF (SEMCON(RECORD)) GOTO 240
            ENDIF
C
C Key = <Question mark> = Help listing
C
         ELSE IF (KEY.EQ.KLHELP) THEN
            IF (SEMCON(' ')) GOTO 240
            IF (SEMCON(
     +         '> <    Adjust: up/down (ditto right/left arrows)'))
     +         GOTO 240
            IF (SEMCON('B      Brightness (default)')) GOTO 240
            IF (SEMCON('C      Contrast')) GOTO 240
            IF (SEMCON('R      Range')) GOTO 240
            IF (SEMCON('H      Highlighting: select, then on/off'))
     +         GOTO 240
            IF (SEMCON(
     +         'M      Mean (default): move both limits up/down'))
     +         GOTO 240
            IF (SEMCON(
     +         'S      Separation: move both limits apart/together'))
     +         GOTO 240
            IF (SEMCON('U      Upper limit: move just upper limit'))
     +         GOTO 240
            IF (SEMCON('L      Lower limit: move just lower limit'))
     +         GOTO 240
            IF (SEMCON('1 2 3  Select red or green or blue channel'))
     +         GOTO 240
            IF (SEMCON(
     +         'A      All: select red and green and blue channels'))
     +         GOTO 240
            IF (SEMCON('I      Invert')) GOTO 240
            IF (SEMCON('Z      Zero')) GOTO 240
            IF (SEMCON(
     +         'O      On/Off: turn on/off - contents unchanged'))
     +         GOTO 240
            IF (SEMCON(
     +         'D      Delete: discard any adjustments'))
     +         GOTO 240
            IF (SEMCON(
     +         'F      Fine/Coarse: switch to fine/coarse adjustment'))
     +         GOTO 240
            IF (SEMCON(
     +         'E      Exit: finish - LUT updated (ditto RETURN)'))
     +         GOTO 240
            IF (SEMCON('Q      Quit: finish - LUT unchanged')) GOTO 240
            IF (SEMCON(
     +         'V      Verify: print summary of current settings'))
     +         GOTO 240
            IF (SEMCON(
     +         '?      Help: ..... this printout .....'))
     +         GOTO 240
         ENDIF
C
C See if LUT parameters need adjusting
C
         IF (LADJUS) THEN
C
C Determine increments
C
            IF (LFINE) THEN
               BSTEP=0.005
               CSTEP=0.01
               LSTEP=1
            ELSE
               BSTEP=0.05
               CSTEP=0.1
               LSTEP=LUTLEN/32
            ENDIF
C
C Adjust brightness
C
            IF (SMODE.EQ.1) THEN
               B=MAX(0.0,MIN(B+SIGN(BSTEP,REAL(IDIR)),1.0))
C
C Adjust contrast
C
            ELSE IF (SMODE.EQ.2) THEN
               C=MAX(-1.0,MIN(C+SIGN(CSTEP,REAL(IDIR)),1.0))
C
C Otherwise, adjust range/highlighting
C
            ELSE
C
C Determine size and direction of adjustment
C
               ISTEP=SIGN(LSTEP,IDIR)
C
C Determine required adjustment
C
               IF (AMODE.EQ.1) THEN
                  ISTEP1=ISTEP
                  ISTEP2=ISTEP
               ELSE IF (AMODE.EQ.2) THEN
                  ISTEP1=-ISTEP
                  ISTEP2=ISTEP
               ELSE IF (AMODE.EQ.3) THEN
                  ISTEP1=0
                  ISTEP2=ISTEP
               ELSE
                  ISTEP1=ISTEP
                  ISTEP2=0
               ENDIF
C
C Adjust range
C
               IF (SMODE.EQ.3) THEN
C
C Determine adjusted values
C
                  L1=LR1+ISTEP1
                  L2=LR2+ISTEP2
C
C Make adjustment if values are o.k.
C
                  IF (L1.GE.1.AND.L2.GE.L1.AND.L2.LE.LUTLEN) THEN
                     LR1=L1
                     LR2=L2
                  ENDIF
C
C Adjust highlighting
C
               ELSE
C
C Determine adjusted values
C
                  L1=LH1+ISTEP1
                  L2=LH2+ISTEP2
C
C Make adjustment if values are o.k.
C
                  IF (L1.GE.1.AND.L2.GE.L1.AND.L2.LE.LUTLEN) THEN
                     LH1=L1
                     LH2=L2
                  ENDIF
               ENDIF
            ENDIF
C
C Flag adjustment as change to LUT
C
            LCHANG=.TRUE.
         ENDIF
C
C If not end of KEYS session, branch back to output any changes and to
C read any more keystrokes
C
         IF (LCONTI) GOTO 160
      ENDIF
C
C Pass on LUT changes to display hardware
C
      IF (FSLU61(2,NUMBER,MODE,LUT,ERROR)) GOTO 240
C
C Code for key TO
C
      IF (VARSET(NTO)) THEN
C
C Open new picture to hold LUT
C
         NPIC=IVALPN(NTO)
         NCOL=LUTLEN
         NROW=LUTROW(MODE)
         NLAY=LUTLAY(MODE)
         LP1=0
         IF (SEMOPN(2,NPIC,NCOL,NROW,NLAY,NCLLUT,NFMINT,LP1)) GOTO 240
C
C Copy LUT data to output picture
C
         L=1
         DO 230 K=1,NLAY
            DO 220 J=1,NROW
C
C Copy LUT data into row buffer
C
               DO 210 I=1,NCOL
                  IB1(I)=LUT(L)
                  L=L+1
  210          CONTINUE
C
C Store result in LP1
C
               IF (SEMROW(2,RB1,NFMINT,J,K,LP1)) GOTO 240
  220       CONTINUE
  230    CONTINUE
      ENDIF
C
  240 RETURN
C
  250 FORMAT ('Brightness',F10.4,4X,'Contrast',F10.4,4X,
     +           'Range',G12.5,',',G12.5)
  260 FORMAT ('Highlighting',G12.5,',',G12.5)
C
C Copyright (C) 1987-1995:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module LMANU2
C
      SUBROUTINE LMANU2(LUT,B,C,L1,L2)
C
C Initialise LUT channel with linear grey-scale in the range L1 to L2,
C based on relative brightness and contrast B and C.  B ranges from 0.0
C to 1.0 and c ranges from -1.0 to 1.0.
C
      INTEGER LUT(*)
      REAL B,C
      INTEGER L1,L2
C
      INCLUDE 'COMMON'
C
      INTEGER L
      REAL DB,GREY1,GREY2,GREY,DGREY
C
C Determine start and finish grey levels for linear grey-scale
C
      DB=C*(0.5-ABS(B-0.5))
      GREY1=(B-DB)*REAL(LUTMAX)
      GREY2=(B+DB)*REAL(LUTMAX)
C
C Set first part of LUT channel to GREY1
C
      DO 10 L=1,L1
         LUT(L)=NINT(GREY1)
   10 CONTINUE
C
C Set middle part to grey-scale
C
      GREY=GREY1
      DGREY=(GREY2-GREY1)/REAL(L2-L1)
C
      DO 20 L=L1+1,L2-1
         GREY=GREY+DGREY
         LUT(L)=NINT(GREY)
   20 CONTINUE
C
C Set last part to GREY2
C
      DO 30 L=L2,LUTLEN
         LUT(L)=NINT(GREY2)
   30 CONTINUE
C
      RETURN
C
C Copyright (C) 1987-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
