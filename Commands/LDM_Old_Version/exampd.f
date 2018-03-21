C Semper 6 processing module EXAMPD
C
      SUBROUTINE EXAMPD
C
C Verb EXAMINE: prints picture dimensions/class/form/title etc.
C
C Disc examine code could usefully include a directory sort, using a
C second row buffer to hold segment sizes, before scans for pictures
C begin..
C
      LOGICAL OPT,DISC,ABANDN,CONOPT,ASSNAM
      LOGICAL SEMMED,SEMDPD,SEMTEX,EXA2,VARSET,SEMCON
      INTEGER IVAL,SEMPPN,SEMFRM !,IPACK
C
C Packed names
C
      INTEGER NIMAGE,NMACRO,NFOURI,NSPECT,NCORRE
      INTEGER NUNDEF,NWALSH,NPLIST,NHISTO,NLUT
      PARAMETER (NIMAGE=14921,NMACRO=20843,NFOURI=10221,NSPECT=31045)
      PARAMETER (NCORRE=5418,NUNDEF=-2165,NWALSH=-4853,NPLIST=26089)
      PARAMETER (NHISTO=13179,NLUT=20060)
C
      INCLUDE 'COMMON'
C
      REAL SPACE,X
      INTEGER*4 SDL(256),BLKN,I42,SEGA,SSIZE,LNDIR4
      PARAMETER (I42=2)
      INTEGER LABEL(256),CLNS(10),RTEXT(20)
      INTEGER RTEXTL,DIRSZE,RCLASS,RFORM,RPLTYP,DEVICE
      INTEGER I,L,N,N0,N1,N2,MEDIUM,NPIC,NSD
      LOGICAL ALL,LSEAR,HDR,FULL,BRIEF
      LOGICAL LDEVIC,LSINGL,LTEXT,LSKIP,LMATCH
      CHARACTER*10 DPDESC(2:4)
      CHARACTER*1  FACT
      CHARACTER*(RECLEN) HEAD
C
      EQUIVALENCE (RB1,LABEL),(RB2,SDL),(RB3,RTEXT)
      EQUIVALENCE (SMGI1,RCLASS),(SMGI2,RFORM),(SMGI3,RTEXTL)
      EQUIVALENCE (SMGI4,RPLTYP)
C
C CLNS contains names IMA,MAC,FOU,SPE,COR,UND,WAL,PLI,HIS,LUT
C
      DATA CLNS/ NIMAGE,NMACRO,NFOURI,NSPECT,NCORRE,
     +           NUNDEF,NWALSH,NPLIST,NHISTO,NLUT /
C
C Display type descriptions for type 2 to 4
C
      DATA DPDESC / 'Line Graph',' Histogram',' Y-modulus'/
C
C Initialise
C Establish mode (BRIEF, FULL, class/form/ restrictions)
C
      BRIEF = OPT(3929)
      IF (BRIEF) THEN
         IF (CONOPT(10452,3929)) GOTO 150
      ELSE
         FULL = OPT(10452)
      ENDIF
      RCLASS=-1
      DO 10 I=1,10
         IF (OPT(CLNS(I))) RCLASS=I
   10 CONTINUE
      RFORM=SEMFRM(-1)
C
C If class is position list, look for further restiction on plist type
C
      IF (RCLASS.EQ.NCLPLI) THEN
         IF (OPT(19579)) THEN
            RPLTYP=1
         ELSE IF (OPT(5658)) THEN
            IF (OPT(24645)) THEN
               RPLTYP=2
            ELSE IF (OPT(5295)) THEN
               RPLTYP=3
            ELSE
               RPLTYP=4
            ENDIF
         ELSE
            RPLTYP=0
         ENDIF
      ENDIF
C
C Prevent any automatic selection
C
      SELOPN=.FALSE.
C
C If TEXT, evaluate reference text string
C
      LTEXT=VARSET(-225)
      IF (LTEXT) THEN
         RTEXTL=20
         IF (SEMTEX(-225,RTEXT,RTEXTL)) GOTO 150
      ENDIF
C
      LDEVIC = VARSET(6622)
      ALL = OPT(2092) .OR. LDEVIC
      HDR = ALL
      LSEAR = RFORM .GE. 0 .OR. RCLASS .GE. 0 .OR. LTEXT
C
C Establish range within device
C
C If $1 not given default to SELECT only if no class/form/text/device
C is given.
C
      IDERR = -12441
      N0 = IVAL(-12441)
      N1 = MOD(N0,1000)
C
      IF (N1 .LT. 0) THEN
         ERROR = 3
         GOTO 150
      ENDIF
C
      N2 = MOD(IVAL(-12473),1000)
      IF (N1 .EQ. 0 .AND. .NOT. (LSEAR .OR. LDEVIC)) THEN
         N0 = INT(SELECT)
         N1 = MOD(N0,1000)
      ENDIF
C
      IF (N1 .EQ. 0) THEN
C
C Fault $12 with invalid $1
C
         IF (N2 .NE. 0) THEN
            ERROR = 3
            GOTO 150
         ENDIF
      ENDIF
C
C If $12 not given default to first value
C
      LSINGL = N2 .EQ. 0
      IF (LSINGL) N2 = N1
C
C Fault invalid range
C
      IF (N2 .LT. N1) THEN
         IDERR = -12473
         ERROR = 3
         GOTO 150
      ENDIF
C
C Force ALL if explicitly requested, or device requested
C or if class/form/text restriction with no $1/$12
C
      ALL = ALL .OR. (LSEAR .AND. N1.EQ.0)
C
C Establish device - first from $1..
C
      N = SEMPPN(N0)
      IDERR = N
      IF (N .LT. 0) THEN
         ERROR = 28
         GOTO 150
      ENDIF
      DEVICE = N/1000
C
C ..then from ALL..
C
      IF (ALL) THEN
         DEVICE = CD
C
C ..also set range and mode
C
         N1 = 1
         N2 = 999
         LSINGL = .FALSE.
      ENDIF
C
C ..then from DEVICE
C
      IF (LDEVIC) DEVICE = IVAL(6622)
      IF (SEMMED(DEVICE,MEDIUM)) GOTO 150
C
C Check for picture storage device
C
      IF (DVTYP(DEVICE) .NE. FLTPIC) THEN
         IDERR = DEVICE
         ERROR = 90
         GOTO 150
      ENDIF
C
      N0 = DEVICE*1000
C
C Set no match initially
C
      LMATCH = .FALSE.
C
C Prepare device name (if any)
C
      IF (ASSNAM(1,DEVICE,LABEL)) GOTO 150
C
C Switch acc to device
C
      IF (MEDIUM .EQ. MEDDC .OR. MEDIUM .EQ. MEDVM) THEN
C
C Examine disc/memory device
C --------------------------
C Read directory
C
         DIRSZE=DRSIZ(DEVICE)
         LNDIR4=DIRSZE*LNBLK
         IF (DISC(1,DEVICE,LNDIR4,SDL,I42,NFMBYT,NFMBYT)) GOTO 150
C
C Make header
C
         IF (HDR) THEN
            IF (MEDIUM .EQ. MEDVM) THEN
               WRITE (HEAD,20) 'Memory',DEVICE
   20          FORMAT (A,' device',I3,': ')
               N=18
            ELSE
               IF (DVSCR(DEVICE) .EQ. 0) THEN
                  WRITE (HEAD,20) 'Disc',DEVICE
                  N=16
               ELSE
                  WRITE (HEAD,20) 'Temporary disc',DEVICE
                  N=26
               ENDIF
               CALL SEMCHS(HEAD(N+1:),LABEL(2),LABEL(1))
            ENDIF
         ENDIF
C
C Search for each picture in turn
C
         DO 50 N=N1,N2
            NPIC=N0+N
            IF (ABANDN(ERROR)) GOTO 150
            NSD=1
   30       L=SDL(NSD)
            IF (L.NE.N) THEN
               IF (L.GE.1000) GOTO 50
               NSD=NSD+2
               GOTO 30
            ENDIF
C
C Found - note length
C
            SEGA=SDL(NSD+1)
            SSIZE=SDL(NSD+3)-SEGA
C
C Fetch label
C
            IF (DISC(1,DEVICE,LNLAB4,LABEL,SEGA,NFMINT,NFMBYT)) GOTO 150
C
C Verify label info
C
            IF (EXA2(BRIEF,FULL,LTEXT,NPIC,LSKIP,LMATCH,
     +               HDR,HEAD)) GOTO 150
            IF (LSKIP) GOTO 50
C
C Verify size and address if FULL
C
            IF (FULL) THEN
               SPACE = REAL(SSIZE) * REAL(LNBLK)
               CALL EXA5(SPACE,FACT)
               WRITE (RECORD,40) SPACE,FACT,SSIZE
   40          FORMAT (6X,'Space occupied ',F7.1,A,'b  ',I8,' blocks')
               IF (SEMCON(RECORD)) GOTO 150
            ENDIF
   50    CONTINUE
      ELSE IF (MEDIUM .EQ. MEDDS) THEN
C
C Examine display
C ---------------
C Make header
C
         IF (HDR) THEN
            WRITE (HEAD,60) DEVICE
   60       FORMAT ('Display device',I3)
         ENDIF
C
C Search for each picture in turn
C
         DO 90 N=N1,N2
            NPIC=N0+N
            IF (N.GT.NDPDS) GOTO 90
            IF (ABANDN(ERROR)) GOTO 150
C
C Fetch DPD
C
            IF (SEMDPD(1,N)) GOTO 150
            IF (DPTYP.LE.0) GOTO 90
            IF (DPTYP.EQ.1) THEN
C
C Fetch label (can't use SEMLAB since may not be open)
C
               BLKN = WRKDPD + (N-1)*(DPDSZE+LABSZE) + DPDSZE
               IF (DISC(1,WRKDEV,LNLAB4,LABEL,BLKN,NFMINT,NFMBYT))
     +                  GOTO 150
               IF (EXA2(BRIEF,FULL,LTEXT,NPIC,LSKIP,LMATCH,
     +                  HDR,HEAD)) GOTO 150
               IF (LSKIP) GOTO 90
               X=GPSIZ(1)/DPMA
               IF (FULL) THEN
                  WRITE (RECORD,70) X,DPMIN,DPMAX
   70             FORMAT (6X,'Sampling',F6.2,
     +                    '  Black,white levels ',G13.5,G13.5)
                  IF (SEMCON(RECORD)) GOTO 150
               ENDIF
            ELSE
               IF (DPTYP .LE. 4) THEN
                  WRITE (RECORD,80) NPIC,DPDESC(DPTYP)
   80             FORMAT (I5,' is a ',A,' display')
               ELSE
                  WRITE (RECORD,80) NPIC,'unknown'
               ENDIF
               IF (SEMCON(RECORD)) GOTO 150
            ENDIF
   90    CONTINUE
      ENDIF
C
C Select iff single picture mode
C
      IF (LSINGL) CALL SEMSZZ(NPIC)
      GOTO 140
C
C No pictures found?
C
  140 IF (.NOT.LMATCH) THEN
         IF (LSEAR) THEN
            ERROR = 138
         ELSE IF (LSINGL) THEN
            ERROR = 30
            IDERR = N0+N1
         ELSE
            ERROR = 58
         ENDIF
      ENDIF
C
  150 RETURN
C
C Copyright (C) 1987-1996:  Synoptics Ltd, All Rights Reserved
C
      END
C
C Semper 6 subsidiary module EXA2
C
      LOGICAL FUNCTION EXA2(BRIEF,FULL,LTEXT,NPIC,LSKIP,LMATCH,HDR,HEAD)
C
C Prints details from picture label LABEL to console output stream
C
      INTEGER NPIC
      LOGICAL BRIEF,FULL,LTEXT,LSKIP,LMATCH,HDR
      CHARACTER*(*) HEAD
C
      LOGICAL SEMOP2,TEXTU2,TEXTU3,SEMCON
      LOGICAL SEMLNF
      INTEGER LNFORM
C
      INCLUDE 'COMMON'
C
      REAL SPACE
      INTEGER LABEL(256),MPARS(6),RTEXT(20)
      INTEGER CCOL,CROW,CLAY,CLASS,FORM,RCLASS,RFORM,RTEXTL
      INTEGER RPLTYP,I,NCOL,NROW,NLAY
      INTEGER N
      CHARACTER*11 CLASTR
      CHARACTER*11 DATSTR
      CHARACTER*8  TIMSTR
      CHARACTER*7  FORSTR
      CHARACTER*12 PLTYPE(4)
      CHARACTER*2 WP
      CHARACTER*1 FACT
C
      EQUIVALENCE (RB1,LABEL)
      EQUIVALENCE (RB3,RTEXT)
      EQUIVALENCE (SMGI1,RCLASS),(SMGI2,RFORM),(SMGI3,RTEXTL)
      EQUIVALENCE (SMGI4,RPLTYP)
      EQUIVALENCE (MPARS,NCOL),(MPARS(2),NROW),(MPARS(3),NLAY)
      EQUIVALENCE (MPARS(4),CCOL),(MPARS(5),CROW),(MPARS(6),CLAY)
C
      DATA PLTYPE / 'list','open curve','closed curve','unknown' /
C
      EXA2=.TRUE.
C
C Check label valid
C
      IF (SEMOP2(LABEL)) THEN
C
C Malformed label (should never happen)
C
         WRITE (RECORD,10) NPIC
   10    FORMAT (I5,6X,'m a l f o r m e d   l a b e l',I14)
         IF (SEMCON(RECORD)) GOTO 50
         GOTO 60
      ENDIF
C
C Class/form/text restrictions matched?
C
      CLASS = LABEL(LBCLAS)
      FORM = LABEL(LBFORM)
      IF (RCLASS.GE.0) THEN
         IF (CLASS.NE.RCLASS) GOTO 60
         IF (RCLASS.EQ.NCLPLI) THEN
            IF (RPLTYP.NE.0) THEN
               IF (RPLTYP.GT.3) THEN
                  IF (LABEL(LBPLTY).EQ.1) GOTO 60
               ELSE
                  IF (LABEL(LBPLTY).NE.RPLTYP) GOTO 60
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      IF (RFORM.GE.0) THEN
         IF (FORM.NE.RFORM) GOTO 60
      ENDIF
      IF (LTEXT) THEN
         IF (.NOT.TEXTU3(LABEL(LBTT1),LABEL(LBNCTT),1,RTEXT,RTEXTL,I))
     +       GOTO 60
      ENDIF
C
      LMATCH = .TRUE.
C
C Print header if required
C
      IF (HDR) THEN
         IF (SEMCON(' ')) GOTO 50
         IF (SEMCON(HEAD)) GOTO 50
         HDR = .FALSE.
      ENDIF
C
C Print main parameters
C
      N=LBNC1
      DO 20 I=1,6
         MPARS(I)=256*LABEL(N)+LABEL(N+1)
         N=N+2
   20 CONTINUE
      CALL EXA3(CLASTR,CLASS)
      CALL EXA4(FORSTR,FORM)
      IF (LABEL(LBWP).EQ.0) THEN
         WP='  '
      ELSE
         WP='wp'
      ENDIF
      IF (SEMLNF(FORM,LNFORM)) GOTO 50
      SPACE=REAL(NCOL)*REAL(NROW)*REAL(NLAY)*REAL(LNFORM)
      CALL EXA5(SPACE,FACT)
      WRITE (RECORD,30) NPIC,NCOL,NROW,NLAY,SPACE,FACT,CLASTR,FORSTR,WP
   30 FORMAT (I5,' Size',2(I5,','),I5,1X,F7.1,A,'b ',A,1X,A,2X,A)
C
      IF (SEMCON(RECORD)) GOTO 50
C
C Quit now if BRIEF
C
      IF (.NOT.BRIEF) THEN
C
C Print title, if any
C
         IF (TEXTU2(LABEL(LBTT1),LABEL(LBNCTT))) GOTO 50
C
C Give more if FULL requested
C
         IF (FULL) THEN
C
C Print data range, if recorded
C
            IF (LABEL(LBNCRR).GT.0) THEN
               RECORD='      Range:'
               CALL SEMCHS(RECORD(14:),LABEL(LBRR1),LABEL(LBNCRR))
               IF (SEMCON(RECORD)) GOTO 50
            ENDIF
C
C Print picture origin
C
            WRITE (RECORD,40) CCOL,CROW,CLAY
   40       FORMAT (6X,'Origin column,row,layer:',3I5)
            IF (SEMCON(RECORD)) GOTO 50
C
C If position list, print position list type
C
            IF (CLASS.EQ.NCLPLI) THEN
               I = LABEL(LBPLTY)
               IF (I .LT. 1 .OR. I .GT. 3) I = 4
               RECORD = '      Plist type:'
               RECORD(19:) = PLTYPE(I)
               IF (SEMCON(RECORD)) GOTO 50
            ENDIF
C
C Encode created date/time
C
            LABEL(LBYEAR)=LABEL(LBYEAR)+1900
            RECORD( 1:15)='      Created: '
            RECORD(16:26)=DATSTR(LABEL(LBYEAR+2),LABEL(LBYEAR+1),
     +                           LABEL(LBYEAR))
            RECORD(27:27)=' '
            RECORD(28:35)=TIMSTR(LABEL(LBYEAR+3),LABEL(LBYEAR+4),
     +                           LABEL(LBYEAR+5))
            IF (SEMCON(RECORD(1:35))) GOTO 50
         ENDIF
      ENDIF
C
      EXA2=.FALSE.
      LSKIP=.FALSE.
C
   50 RETURN
C
C This entry to be skipped
C
   60 EXA2=.FALSE.
      LSKIP=.TRUE.
      GOTO 50
C
C Copyright (C) 1987,1988,1989,1990:  Synoptics Ltd, All Rights Reserved
C
      END
C
C Semper 6 subsidiary module EXA3
C
      SUBROUTINE EXA3(STRING,CLASS)
C
C Sets STRING to character string for class number CLASS
C
      CHARACTER*(*) STRING
      INTEGER CLASS
C
      INCLUDE 'COMMON'
C
      IF (CLASS .EQ. NCLIMA) THEN
         STRING = '   Image   '
      ELSE IF (CLASS .EQ. NCLMAC) THEN
         STRING = '   Macro   '
      ELSE IF (CLASS .EQ. NCLFOU) THEN
         STRING = '  Fourier  '
      ELSE IF (CLASS .EQ. NCLSPE) THEN
         STRING = '  Spectrum '
      ELSE IF (CLASS .EQ. NCLCOR) THEN
         STRING = 'Correlation'
      ELSE IF (CLASS .EQ. NCLUND) THEN
         STRING = ' Undefined '
      ELSE IF (CLASS .EQ. NCLWAL) THEN
         STRING = '   Walsh   '
      ELSE IF (CLASS .EQ. NCLPLI) THEN
         STRING = '   Plist   '
      ELSE IF (CLASS .EQ. NCLHIS) THEN
         STRING = ' Histogram '
      ELSE IF (CLASS .EQ. NCLLUT) THEN
         STRING = '    Lut    '
      ELSE
         STRING = '  Unknown  '
      ENDIF
C
      RETURN
C
C Copyright (C) 1989,1990:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module EXA4
C
      SUBROUTINE EXA4(STRING,FORM)
C
C Sets STRING to character string for form number FORM
C
      CHARACTER*(*) STRING
      INTEGER FORM
C
      INCLUDE 'COMMON'
C
      IF (FORM .EQ. NFMBYT) THEN
         STRING = '  Byte '
      ELSE IF (FORM .EQ. NFMINT) THEN
         STRING = 'Integer'
      ELSE IF (FORM .EQ. NFMFP) THEN
         STRING = '   Fp  '
      ELSE IF (FORM .EQ. NFMCOM) THEN
         STRING = 'Complex'
      ELSE
         STRING = 'Unknown'
      ENDIF
C
      RETURN
C
C Copyright (C) 1989,1990:  Synoptics Ltd,  All Rights Reserved
C
      END
      SUBROUTINE EXA5(SPACE,FACT)
      REAL SPACE
      CHARACTER*1 FACT
C
      SPACE = SPACE / 1024.0
      IF (SPACE .LT. 10000.0) THEN
         FACT = 'k'
      ELSE
         SPACE = SPACE / 1024.0
         IF (SPACE .LT. 10000.0) THEN
            FACT = 'M'
         ELSE
            SPACE = SPACE / 1024.0
            FACT = 'G'
         ENDIF
      ENDIF
      RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
