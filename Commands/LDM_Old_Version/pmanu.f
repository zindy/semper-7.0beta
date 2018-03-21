C Semper 6 processing module PMANU
C
      SUBROUTINE PMANU
C
C Provides verbs CREATE, DELETE, RECLASS, ORIGIN, TITLE, WP and PCB
C - picture management utilities operating primarily at
C   the level of SEMOPN/SEMDEL/labels
C
      REAL VAL
      INTEGER IVAL,IVALPN,SEMFRM !,IPACK
C     Change: LDM, July 2005
C     Ensure that gfortran does not think of range as an intrinsic
      EXTERNAL RANGE
      LOGICAL SEMROW,SEMOPN,SEMCLS,SEMLU,SEMDEL,SEMLAB,TSTSRG,RANGE
      LOGICAL SEMMED,OPT,SEMCEN,SEMKTX,VARSET,PRUFLS,PRUIND,PRUENT
      LOGICAL SEMLNF,SEMCON,SEMDIA,OPTNO,CONOPT
C
      INCLUDE 'COMMON'
C
      REAL T1,T2,X,Y
      INTEGER*4 I4N
      INTEGER IB1(256),LABEL(256),OUT(256),NSIZE(3),PSIZE(3)
      INTEGER CLNS(10),CLASS,FORM,DEVICE,I,ILL,IPLTYP,ISL,ITL,ITY
      INTEGER IWP,IWSTAT,J,LAST,LENOUT,LNFORM
      INTEGER MEDIUM,N,N1,N2,NCL,NCOL,NLAY,NPIC,NROW,NUMBER,OPC
C
      CHARACTER*(PRNACT) PROG
C
      EQUIVALENCE (RB1,IB1,LABEL),(RB5,OUT)
      EQUIVALENCE (PSIZE,NCOL),(PSIZE(2),NROW),(PSIZE(3),NLAY)
C
C Packed names
C
      INTEGER NIMAGE,NDUMMY,NFOURI,NSPECT,NCORRE,NUNDEF
      INTEGER NWALSH,NPLIST,NHISTO,NLUT
      PARAMETER (NIMAGE=14921,NDUMMY=-30000,NFOURI=10221)
      PARAMETER (NSPECT=31045,NCORRE=5418,NUNDEF=-2165)
      PARAMETER (NWALSH=-4853,NPLIST=26089,NHISTO=13179,NLUT=20060)
C
C CLNS contains names IMA,dummy,FOU,SPE,COR,UND,WAL,PLI,HIS,LUT
C
      DATA CLNS/NIMAGE,NDUMMY,NFOURI,NSPECT,NCORRE,NUNDEF,NWALSH,
     +          NPLIST,NHISTO,NLUT/
C
      NSIZE(1) = 30786
      NSIZE(2) = 30792
      NSIZE(3) = 30793
C
C Initialise - check for program text options first
C
      IF (VERB.EQ.6612 .AND. VARSET(26335)) THEN
         LENOUT=PRNACT
         IF (SEMKTX(26335,'Program name (as textstring): ',
     +              OUT,LENOUT,.FALSE.)) GOTO 270
         IF (LENOUT .EQ. 0) GOTO 270
C
         CALL SEMCHS(PROG,OUT,LENOUT)
C
C Find a matching entry
C
         DEVICE=0
         IF (PRUENT(DEVICE,ISL,LENOUT,OUT)) GOTO 270
         IF (ISL .EQ. 0) THEN
            ERROR = 125
            IDMESS = PROG(1:LENOUT)
            GOTO 270
         ENDIF
C
         IF (PROTN(DEVICE) .NE. 0) THEN
            ERROR = 41
            IDERR = DEVICE
            GOTO 270
         ENDIF
C
C Check if active slot
C
         IF (INPLEV .NE. 0) THEN
            DO 10 N = 1,INPLEV
               IF (DEVICE .EQ. INPDEV(N) .AND.
     +                ISL .EQ. INPSLT(N)) THEN
                  ERROR = 127
                  IDMESS = PROG(1:LENOUT)
                  GOTO 270
               ENDIF
   10       CONTINUE
         ENDIF
C
C Perform the deletion and report the operation (unless option
C NOVERIFY is set)
C
         IF (PRUIND(1,DEVICE,ISL,I4N,I4N,ITY,ITL,ILL,LENOUT,OUT))
     +      GOTO 270
         ITY = 1
         IF (PRUIND(2,DEVICE,ISL,I4N,I4N,ITY,ITL,ILL,LENOUT,OUT))
     +      GOTO 270
         IF (PRUFLS(DEVICE)) GOTO 270
C
         IF (.NOT.OPTNO(-3419)) THEN
            CALL SEMCHS(PROG,OUT,LENOUT)
            WRITE (RECORD,20) PROG(1:LENOUT),DEVICE
   20       FORMAT (' Program ''',A,''' deleted from device',I3)
            IF (SEMCON(RECORD)) GOTO 270
         ENDIF
C
         GOTO 270
C
      ENDIF
C
      N1=IVALPN(-12441)
      IDERR=N1
      IF (N1.LE.0) THEN
         ERROR=28
         GOTO 270
      ENDIF
      DEVICE=N1/1000
      NUMBER=N1-1000*DEVICE
      IF (SEMMED(DEVICE,MEDIUM)) GOTO 270
C
C Switch code acc to VERB
C
      IF (VERB.EQ.-381) THEN
          GOTO 200
      ELSE IF (VERB.EQ.6612 .OR. VERB.EQ.-5441) THEN
C
C DELETE/WP mode
C --------------
C
         N2=IVALPN(-12473)
         IF (N2.GE.N1) THEN
            IF (N2.GE.(DEVICE+1)*1000) THEN
               IDERR = N2
               ERROR = 28
               GOTO 270
            ENDIF
         ENDIF
         IF (VERB.EQ.-5441) GOTO 250
C
         OPC=1
         IF (OPT(20852)) OPC=4
         DO 40 NPIC=N1,N2
            IF (SEMDEL(OPC,NPIC)) THEN
               IF (ERROR.NE.41 .OR. N1.EQ.N2) GOTO 270
               WRITE (RECORD,30) NPIC
   30          FORMAT ('Write-protected device/picture:',I5)
               IF (SEMDIA(RECORD,NDIWAR)) GOTO 270
               ERROR=0
            ENDIF
   40    CONTINUE
         GOTO 270
C
      ELSE IF (VERB.EQ.25722) THEN
C
C PCB mode
C --------
C
         IF (SEMLU(1,22535,REAL(NCOLS(LP1)))) GOTO 270
         IF (SEMLU(1,23135,REAL(NROWS(LP1)))) GOTO 270
         IF (SEMLU(1,22881,REAL(NLAYS(LP1)))) GOTO 270
         IF (SEMLU(1,5281,REAL(CLASSN(LP1)))) GOTO 270
         IF (SEMLU(1,10218,REAL(FORMN(LP1)))) GOTO 270
         IF (WSTAT(LP1).EQ.-1) THEN
            N=1
         ELSE
            N=0
         ENDIF
         IF (SEMLU(1,-5447,REAL(N))) GOTO 270
         N=CCOLN(LP1)
         IF (SEMLU(1,-7641,REAL(1-N))) GOTO 270
         IF (SEMLU(1,-7681,REAL(NCOLS(LP1)-N))) GOTO 270
         N=CROWN(LP1)
         IF (SEMLU(1,-9241,REAL(N-NROWS(LP1)))) GOTO 270
         IF (SEMLU(1,-9281,REAL(N-1))) GOTO 270
         N=CLAYN(LP1)
         IF (SEMLU(1,-10841,REAL(1-N))) GOTO 270
         IF (SEMLU(1,-10881,REAL(NLAYS(LP1)-N))) GOTO 270
         GOTO 270
      ENDIF
C
C Pick up any new class required
C
      DO 50 NCL=1,10
        IF (OPT(CLNS(NCL))) GOTO 60
   50 CONTINUE
      NCL=0
C
C If option PLIST, determine position list type also
C
   60 IF (NCL .EQ. NCLPLI) THEN
         IF (OPT(5658)) THEN
            IF (OPT(5295)) THEN
               IPLTYP = 3
            ELSE
               IPLTYP = 2
            ENDIF
         ELSE
            IPLTYP = 1
         ENDIF
      ENDIF
C
      IF (VERB.NE.5525) GOTO 200
C
C CREATE mode - set up defaults for size, class and form
C -----------
C
      IF (SEMOPN(1,NINT(SELECT),NCOL,NROW,NLAY,CLASS,FORM,LP1)) THEN
         ERROR=0
         NCOL=0
         NROW=0
         NLAY=1
      ELSE
         IF (SEMCLS(LP1)) GOTO 270
      ENDIF
      CLASS=NCLIMA
      FORM=NFMFP
C
C Modify size,class,form if specified
C
      DO 70 I=1,3
         IF (VARSET(NSIZE(I))) THEN
            PSIZE(I)=IVAL(NSIZE(I))
            IF (I.EQ.1) THEN
               NROW=NCOL
               NLAY=1
            ENDIF
         ENDIF
   70 CONTINUE
      IF (NCL.NE.0) CLASS=NCL
      FORM=SEMFRM(FORM)
C
C Trap tapes for special treatment
C
C
C Open the new picture (preventing erase, and selecting)
C
         LP1=0
         IF (SEMLU(2,8721,0.)) GOTO 270
         SELOPN=.TRUE.
         IF (SEMOPN(2,N1,NCOL,NROW,NLAY,CLASS,FORM,LP1)) GOTO 270
C
C If creating position list, store position list type in picture label
C
         IF (SEMLAB(1,LABEL,LP1)) GOTO 270
         LABEL(LBPLTY) = IPLTYP
         IF (SEMLAB(2,LABEL,LP1)) GOTO 270
C
C Initialise pixel values?
C
         IF (.NOT.SEMLU(-1,-3253,X)) GOTO 270
         Y=VAL(-3273)
C
C Set one row
C
         N1=2*LNREAL
         DO 80 I=1,N1
            RB1(2*I-1)=X
            RB1(2*I)=Y
   80    CONTINUE
C
C Convert to required form and duplicate as necessary
C
         I4N=N1
         CALL CFORM(RB1,RB1,3,FORM,I4N)
         N1=2*LNREAL/LNINT+1
         IF (SEMLNF(FORM,LNFORM)) GOTO 270
         LAST=(NCOL*LNFORM-1)/LNINT+1
         I=1
   90    IB1(N1)=IB1(I)
         IF (N1.LT.LAST) THEN
            N1=N1+1
            I=I+1
            GOTO 90
         ENDIF
C
C Write out to all rows of picture
C
         DO 110 N1=1,NLAY
            DO 100 J=1,NROW
               IF (SEMROW(2,RB1,FORM,J,N1,LP1)) GOTO 270
  100       CONTINUE
  110    CONTINUE
C
C Record range
C
         T1=VMIN
         T2=VMAX
         IF (FORM.NE.NFMCOM) Y=X
         VMIN=AMIN1(X,Y)
         VMAX=AMAX1(X,Y)
         IF (.NOT.RANGE(4,LP1)) WSTAT(LP1)=0
         VMIN=T1
         VMAX=T2
      GOTO 270
C
C Other modes: check not tape and not WP
C
  200 CONTINUE
      IF (WSTAT(LP1).NE.0) THEN
         ERROR=41
         GOTO 270
      ENDIF
      IF (VERB.EQ.29003) THEN
C
C RECLASS mode
C ------------
C Force new class
C
         IF (NCL.EQ.0) NCL=1
         LABEL(LBCLAS)=NCL
C
C Set in PCB too
C
         CLASSN(LP1)=NCL
C
C If new class is position list, set type in picture label
C
         IF (NCL.EQ.NCLPLI) LABEL(LBPLTY)=IPLTYP
      ELSE IF (VERB.EQ.24729) THEN
C
C ORIGIN mode
C -----------
C Check for option RESET
C
         IF (OPT(29019)) THEN
C
C Reset origin to default
C
            SMGI1=1+NCOLS(LP1)/2
            SMGI2=1+NROWS(LP1)/2
            SMGI3=1+NLAYS(LP1)/2
            IF (CLASSN(LP1).EQ.NCLHIS.OR.CLASSN(LP1).EQ.NCLPLI
     +      .OR.CLASSN(LP1).EQ.NCLLUT) THEN
               SMGI1=1
               SMGI2=1
               SMGI3=1
            ENDIF
         ELSE
C
C Set keys SIZE,SI2,SI3 to 1,1,1
C
            DO 210 I=1,3
               IF (SEMLU(2,NSIZE(I),1.0)) GOTO 270
  210       CONTINUE
C
C Ask TSTSRG for new origin position
C
            IF (TSTSRG(1,LP1)) GOTO 270
         ENDIF
C
C Update all the necessary data structures
C
         IF (SEMCEN(LP1,SMGI1,SMGI2,SMGI3)) CONTINUE
         GOTO 270
      ELSE
C
C TITLE mode
C ----------
C Label available?
C
         IF (.NOT.LBLINC) THEN
            IDERR=IVALPN(-12441)
            ERROR=87
            GOTO 270
         ENDIF
C
C Collect text from another picture..
C
         IF (VARSET(10335)) THEN
            N1=IVALPN(10335)
            IF (SEMOPN(1,N1,NCOL,NROW,NLAY,CLASS,FORM,LP2)) GOTO 270
            IF (LBLINC) THEN
               LENOUT=LABEL(LBNCTT)
               N=LBTT1
               DO 220 I=1,LENOUT
                  OUT(I)=LABEL(N)
                  N=N+1
  220          CONTINUE
            ELSE
               IDERR=N1
               ERROR=87
               GOTO 270
            ENDIF
C
C (Recover label to be updated)
C
            IF (SEMLAB(1,LABEL,LP1)) GOTO 270
C
C ..or from key TEXT
C
         ELSE
            LENOUT=256
            IF (SEMKTX(-225,'Title text (as textstring): ',
     +                 OUT,LENOUT,.FALSE.)) GOTO 270
         ENDIF
C
C Update label
C
         N=LBTT1
         IF (OPT(1764)) N=N+LABEL(LBNCTT)
         LABEL(LBNCTT)=N-LBTT1
         DO 230 I=1,LENOUT
            IF (N.GT.LBTT2) THEN
C
C Warn about title overflow
C
               IF (SEMDIA('Title truncated',NDIWAR)) GOTO 270
               GOTO 240
            ENDIF
            LABEL(N)=OUT(I)
            LABEL(LBNCTT)=LABEL(LBNCTT)+1
            N=N+1
  230    CONTINUE
  240    CONTINUE
      ENDIF
C
C Return label to disc (TITLE and RECLASS)
C
      IF (SEMLAB(2,LABEL,LP1)) CONTINUE
      GOTO 270
C
C WP mode
C -------
C
  250 IF (CONOPT(24560,24246)) GOTO 270
C
      IF (OPT(24246)) THEN
         IWP=0
         IWSTAT=0
      ELSE
         IWP=255
         IWSTAT=-1
      ENDIF
C
      DO 260 NPIC=N1,N2
         IF (SEMOPN(1,NPIC,NCOL,NROW,NLAY,NCL,FORM,LP1)) THEN
C
C Suppress error 30 (non-existent picture) unless single pic
C
            IF (ERROR.NE.30 .OR. N1.EQ.N2) GOTO 270
            ERROR=0
         ELSE
C
C Picture found: is it on disc?
C
            IF (.NOT.(MEDIUM.EQ.MEDDC.OR.MEDIUM.EQ.MEDVM)) GOTO 280
C
C WP state already correct?
C
            IF (LABEL(LBWP).NE.IWP) THEN
C
C Force WP flag in label..
C
               LABEL(LBWP)=IWP
C
C ..and in PCB too
C
               WSTAT(LP1)=IWSTAT
C
C Return label to disc
C
               IF (SEMLAB(2,LABEL,LP1)) GOTO 270
            ENDIF
C
C Close picture
C
            IF (SEMCLS(LP1)) GOTO 270
         ENDIF
  260 CONTINUE
C
  270 RETURN
C
C Error
C
  280 ERROR=29
      IDERR=DEVICE
      GOTO 270
C
C Copyright (C) 1987-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
