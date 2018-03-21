C Semper 6 processing subsidiary module HPLJ
C
      SUBROUTINE HPLJ
C
C Writes a picture, with title, or display partition incl. overlay,
C in Hewlett-Packard LaserJet+ printer form to a file
C
C Macro line required:
C Hplj :HPLJ partition negated preset $1= name=' old new
C
C Notes
C (i) Tested with Kyocra model F-1200 in HP-LaserJet+ emulation mode
C     Also tested on HPLJ II-P and NEC LC890
C (ii) Speed achieved only through using assembler code routine LPTPUT
C performing direct o/p to the printer port; calling the MS-DOS handler
C was extremely slow; Fortran unformatted write (access=transparent)
C is moderate at best.
C
      INTEGER IVAL,IVALPN,LNBLNK
      LOGICAL ABANDN,CONOPT,FILMAK,FILSEA,FILSTR,FSOI61,FSRI61,FSINIT
      LOGICAL FORTDE
C     Change: LDM, July 2005
C     Ensure that gfortran does not think of range as an intrinsic
      EXTERNAL RANGE
      LOGICAL HPBWRT,HPBFLU,OPT,RANGE
      LOGICAL SEMIOE,SEMLAB,SEMOPN,SEMROW,SEMXA1,VARSET
C
C     'C' functions
C
      LOGICAL EIKOPE,EIKCLO
C
C     File descriptor
C
      INTEGER FD
      COMMON /HPLJFD/ FD
C
      CHARACTER*80 TEXT
      CHARACTER*11 DATSTR
      CHARACTER*8 TIMSTR
      CHARACTER*4 DFEXT
      CHARACTER*3 STAT
      CHARACTER*2 CRLF
      CHARACTER*1 ESC,FORMFD
      REAL A,B,T,PMIN,PMAX,GUESS
C
C
      INTEGER IB1(256),LABEL(256),DATIME(7),P(512)
      INTEGER MASK(0:14,4)
      INTEGER I,INFORM,IOS,J,N,N1,N2,NF,NM,NR,NUM
      INTEGER NCOL,NCOLP,NROW,NLAY,CLASS,FORM
      INTEGER RESOL,DPI,IGUESS
      LOGICAL EXISTS,LNEW,LOLD,LPARTI,TOFILE
C
C     FORTRAN/C file i/o
C
      LOGICAL INFORT
C
C Packed names
C
      INTEGER NFROM,NPARTI,NDOLL1,NNEGAT,NNAME,NNEW,NOLD,NDPI
      PARAMETER (NFROM=10335,NPARTI=25658,NDOLL1=-12441,NNEGAT=22607)
      PARAMETER (NNAME=22453,NNEW=22623,NOLD=24484,NDPI=7049)
C
      INCLUDE 'COMMON'
C
      INTEGER LUWB,LUWBM1,NBW
      PARAMETER (LUWB=LNBUF/LNINT,LUWBM1=LUWB-1)
      INTEGER UWB(LUWB),IB3(LUWB),MAP(0:LUWBM1)
C
      CHARACTER*(FILMAX) FILE,FILENM
C
      EQUIVALENCE (RB1,IB1,LABEL,DATIME),(RB2,MAP),(RB3,IB3)
      EQUIVALENCE (RB4,P),(RB6,UWB),(SMGI10,NBW),(SMGL3,TOFILE)
C
C Grey level dot masks
C
      DATA MASK/0,0,0,0,0,0,0,0,0,0,1,15,15,15,15,
     +          0,0,2,6,6,6,6,6,6,7,7, 7,15,15,15,
     +          0,2,2,2,6,6,6,6,7,7,7, 7, 7,15,15,
     +          0,0,0,0,0,4,6,7,7,7,7, 7, 7, 7,15/
C
      ESC=CHAR(27)
      CRLF=CHAR(13)//CHAR(10)
      FORMFD=CHAR(12)
      LPARTI=OPT(NPARTI)
C
C Open picture ..
C
      IF (.NOT.LPARTI) THEN
         NUM=IVALPN(NDOLL1)
         IF (NUM.LE.0) NUM=SELECT
         IF (SEMOPN(1,NUM,NCOL,NROW,NLAY,CLASS,FORM,LP1)) GOTO 90
C
C .. establish range ..
C
         SMGL1=.FALSE.
         IF (RANGE(1,LP1)) GOTO 90
         PMIN = VMIN
         PMAX = VMAX
         IF (PMIN.EQ.PMAX) THEN
            ERROR=12
            IDERR=IVAL(NFROM)
            GOTO 90
         ENDIF
C
C ..Dimensions..
C
         NCOL=NCOLS(LP1)
         NROW=NROWS(LP1)
C
C ..and form
C
         INFORM=FORMN(LP1)
         IF (INFORM.EQ.NFMBYT) INFORM=NFMINT
         IF (INFORM.EQ.NFMCOM) INFORM=NFMFP
C
C alternatively, open PARTITION and establish range,dims,form
C
      ELSE
         NUM=IVAL(NDOLL1)
         IF (NUM.LE.0) NUM=DISPLA
         IF (FSINIT(2,NUM)) GOTO 90
         IGUESS = LUTLEN-1
         GUESS = REAL(IGUESS)
         PMIN=0.
         PMAX=GUESS
         NCOL=DPSIZ
         NROW=DPSI2
         INFORM=NFMINT
      ENDIF
C
C check resolution if specified
C
      IF (VARSET(NDPI)) THEN
         DPI = IVAL(NDPI)
         IF (DPI .NE. 75 .AND. DPI .NE. 100 .AND. DPI .NE. 150 .AND.
     +       DPI .NE. 300) THEN
            ERROR = 3
            IDERR = NDPI
            GOTO 90
         ENDIF
      ELSE
         IF (NCOL .GT. 256 .OR. NROW .GT. 256) THEN
            DPI = 300
         ELSE IF (NCOL .GT. 170 .OR. NROW .GT. 170) THEN
            DPI = 150
         ELSE IF (NCOL .GT. 128 .OR. NROW .GT. 128) THEN
            DPI = 100
         ELSE
            DPI = 75
         ENDIF
      ENDIF
      RESOL = INT(REAL(DPI)*(512.0/300.0))
C
C check size
C
      IF (NCOL.GT.RESOL .OR. NROW.GT.RESOL) THEN
         ERROR=5
         IDERR=IVAL(NFROM)
         GOTO 90
      ENDIF
C
C round to even row length if nec with zero pixel
C
      NCOLP=(NCOL+1)/2*2
C
C prepack intermediate buffer with trailing zero in case
C
      P(NCOLP)=0
C
C option NEGATE?
C
      IF (OPT(NNEGAT)) THEN
         T=PMIN
         PMIN=PMAX
         PMAX=T
      ENDIF
C
      TOFILE = .TRUE.
C
      IF (TOFILE) THEN
         IF (OPT(NNEW)) THEN
            IF (CONOPT(NOLD,NNEW)) GOTO 90
            LNEW = .TRUE.
         ELSE
            LNEW = .FALSE.
            LOLD = OPT(NOLD)
         ENDIF
         DFEXT = '.hpl'
         CALL OUTDEF(DFEXT)
         IF (FILSTR(' ',FILE,NF,.TRUE.)) GOTO 90
C
C Check name length
C
         IF (NF .EQ. 0) GOTO 90
C
C See if file exists
C
         IF (FILMAK(FILE,DFEXT,FILENM)) GOTO 90
         FILE = FILENM
         IF (FILSEA(FILE,DFEXT,FILENM,EXISTS)) GOTO 90
         IF (EXISTS) THEN
            FILE = FILENM
         ENDIF
         NF = LNBLNK(FILE)
C
C If file already exists, delete it if NEW given
C
         STAT = 'NEW'
         INFORT = .TRUE.
         IF (EXISTS) THEN
            IF (LNEW) THEN
               IF (FORTDE(RDWRTU,FILE,IOS)) GOTO 110
            ELSE IF (LOLD) THEN
               STAT = 'OLD'
            ELSE
               IDMESS = FILE(1:NF)
               ERROR = 135
               GOTO 90
            ENDIF
         ENDIF
C
C Try to open the file dynamically
C
C
C     Try to open the file
C
         INFORT = .FALSE.
         IF ( EIKOPE ( 2, FD, FILE(1:NF) ) ) GOTO 100
      ENDIF
C
C Transmit prolog initialising printer, setting dot resolution and
C initial cursor position, and starting raster graphics mode
C
      NBW = 0
      IF (DPI .EQ. 300) THEN
         IF (HPBWRT(ESC//'*t300R',IOS)) GOTO 110
      ELSE IF (DPI .EQ. 150) THEN
         IF (HPBWRT(ESC//'*t150R',IOS)) GOTO 110
      ELSE IF (DPI .EQ. 100) THEN
         IF (HPBWRT(ESC//'*t100R',IOS)) GOTO 110
      ELSE
         IF (HPBWRT(ESC//'*t75R',IOS)) GOTO 110
      ENDIF
C
C Resolution for centring
C
      RESOL = 600/DPI
      IF (HPBWRT(ESC//'*p',IOS)) GOTO 110
         NBW=NBW+1
         IF (SEMXA1(4,UWB,LUWB,NBW,REAL(1200-(RESOL*NCOLP)),I)) CONTINUE
         NBW=NBW-1
      IF (HPBWRT('X'//ESC//'*p',IOS)) GOTO 110
         NBW=NBW+1
         IF (SEMXA1(4,UWB,LUWB,NBW,REAL(1200-(RESOL*NROW)),I)) CONTINUE
         NBW=NBW-1
      IF (HPBWRT('Y'//ESC//'*r1A',IOS)) GOTO 110
C
C set scaling parameters
C
      A=14.99/(PMIN-PMAX)
      B=-A*PMAX
C
C prepare scaling map?
C
      IF (INFORM.EQ.NFMINT) THEN
         N1=MIN(PMIN,PMAX)
         N2=MAX(PMIN,PMAX)
         IF (REAL(N2)-REAL(N1).GT.REAL(LUWBM1)) THEN
            INFORM=NFMFP
         ELSE
            NM=0
            DO 10 N=N1,N2
               MAP(NM)=MAX(MIN(A*REAL(N)+B,14.99),0.)
               NM=NM+1
   10       CONTINUE
         ENDIF
      ENDIF
C
C pass through picture rows
C
      DO 70 J=1,NROW
C
C fetch data
C
         IF (.NOT.LPARTI) THEN
            IF (SEMROW(1,RB1,INFORM,J,1,LP1)) GOTO 90
         ELSE
C
C frame 1 assumed pro tem
C
            IF (ABANDN(ERROR)) GOTO 90
            IF (FSRI61(IB1,NCOL,NFMINT,DPTLX,DPTLY+J-1,DPFRA,
     +                 0.,GUESS,0,ERROR)) GOTO 90
C
C include overlay read-back/merge
C
            IF (OVLIND(FSDEV)) THEN
               IF (FSOI61(IB3,NCOL,DPTLX,DPTLY+J-1,DPFRA,
     +                    0,ERROR)) GOTO 90
            ELSE
               IF (FSOI61(IB3,NCOL,DPTLX,DPTLY+J-1,0,0,ERROR)) GOTO 90
            ENDIF
            DO 20 I=1,NCOL
               IF (IB3(I).NE.0) IB1(I)=IGUESS
   20       CONTINUE
C
         ENDIF
C
C scale (and fix) to p
C
         IF (INFORM.EQ.NFMINT) THEN
            DO 30 I=1,NCOL
               P(I)=MAP(IB1(I)-N1)
   30       CONTINUE
         ELSE
            DO 40 I=1,NCOL
               P(I)=MAX(MIN(A*RB1(I)+B,14.99),0.)
   40       CONTINUE
         ENDIF
C
C Loop 4 times over pixels to generate four rows of raster data
C
         DO 60 NR=1,4
C
C generate line of raster data: check space..
C
            IF (HPBFLU(NCOLP+7,IOS)) GOTO 110
C
C ..output byte count..
C
            IF (HPBWRT(ESC//'*b',IOS)) GOTO 110
               NBW=NBW+1
               IF (SEMXA1(4,UWB,LUWB,NBW,REAL(NCOLP/2),I)) CONTINUE
               NBW=NBW-1
            IF (HPBWRT('W',IOS)) GOTO 110
C
C ..and data
C
            DO 50 I=1,NCOLP,2
C
C Deposit nibbles for two pixels
C
               NBW=NBW+1
               UWB(NBW)=16*MASK(P(I),NR)+MASK(P(I+1),NR)
   50       CONTINUE
C
C end of pixel loops
C
   60    CONTINUE
C
C end of row loop
C
   70 CONTINUE
C
C Exit raster graphics
C
      IF (HPBWRT(ESC//'*rB',IOS)) GOTO 110
C
C Set position for text and select font 2 (Times-Roman 10 point)
C
      IF (HPBWRT(ESC//'*t300R',IOS)) GOTO 110
      IF (HPBWRT(ESC//'*p300X'//ESC//'*p3150Y',IOS)) GOTO 110
C
C write title
C
      IF (.NOT.LPARTI) THEN
         IF (SEMLAB(1,LABEL,LP1)) GOTO 90
         N=LABEL(LBNCTT)
         IF (N.NE.0) THEN
            N=MIN(N,80)
            CALL SEMCHS(TEXT,LABEL(LBTT1),N)
            IF (HPBWRT(TEXT(1:N)//CRLF,IOS)) GOTO 110
         ENDIF
         DO 80 I=1,7
            DATIME(I)=LABEL(LBYEAR+(I-1))
   80    CONTINUE
         DATIME(1)=DATIME(1)+1900
      ELSE
         CALL MCTIME(DATIME)
      ENDIF
C
C write date
C
      TEXT( 1:11)=DATSTR(DATIME(3),DATIME(2),DATIME(1))
      TEXT(12:12)=' '
      TEXT(13:20)=TIMSTR(DATIME(4),DATIME(5),DATIME(6))
      IF (HPBWRT(TEXT(1:20),IOS)) GOTO 110
C
C terminate page
C
      IF (HPBWRT(FORMFD,IOS)) GOTO 110
      IF (HPBFLU(0,IOS)) GOTO 110
      IF (TOFILE) THEN
         IF ( EIKCLO ( FD ) ) GOTO 110
      ENDIF
   90 RETURN
C
  100 IDMESS = 'Error opening file '//FILE(1:NF)
      ERROR = 77
      GOTO 90
C
C INQUIRE, OPEN, CLOSE, REWIND and WRITE errors
C
  110 IF (.NOT.TOFILE) THEN
C
C printer i/o error
C
         IF (ERROR .EQ. 0) THEN
            ERROR = 77
            IDMESS = 'Printer output error'
         ENDIF
      ELSE
         IF (INFORT) THEN
            IF (SEMIOE(IOS,RDWRTU,FILE(1:NF))) GOTO 120
C
C Close Fortran unit attached to the output file (just in case the file
C is still open)
C
  120       CLOSE (RDWRTU,ERR=90)
         ELSE
            IDMESS = 'Error writing file '//FILE(1:NF)
            ERROR = 77
C
C     Closing a file after an error (ignore any error on closing file)
C
            IF ( EIKCLO ( FD ) ) GOTO 90
         ENDIF
      ENDIF
      GOTO 90
C
C Copyright (C) 1988-1996 Synoptics Ltd.
C
      END
C
C Semper 6 subsidiary module HPBWRT
C ---------------------------------
C : buffered write from character :
C ---------------------------------
C
C EITHER pass character strings to HPBWRT (convenient):
C
C     IF (HPBWRT('STRING',IOS)) RETURN  (repeated as necessary;
C                                        length unrestricted)
C
C OR deposit byte values directly in (int) buffer uwb (efficient):
C
C     INCLUDE 'COMMON'
C     PARAMETER (LUWB=LNBUF/LNINT)
C     INTEGER UWB(LUWB)
C     EQUIVALENCE (RB6,UWB),(SMGI10,NBW)
C     IF (HPBFLU(N)) RETURN       (check buffer space left)
C     DO 10 I=1,N
C        NBW=NBW+1
C        UWB(NBW)=..  (repeated as necessary; n < buffer length luwb)
C  10 CONTINUE
C
      LOGICAL FUNCTION HPBWRT(STR,IOS)
C
      CHARACTER STR*(*)
      INTEGER IOS
C
      LOGICAL HPBFLU
C
      INCLUDE 'COMMON'
C
      INTEGER LUWB
      PARAMETER (LUWB=LNBUF/LNINT)
      INTEGER UWB(LUWB),NBW,NXF
      EQUIVALENCE (RB6,UWB),(SMGI10,NBW)
C
      HPBWRT=.TRUE.
C
      DO 10 NXF = 1,LEN(STR)
         IF (NBW.GE.LUWB) THEN
C
C Output buffer to printer
C
            IF (HPBFLU(0,IOS)) GOTO 20
         ENDIF
C
         NBW = NBW + 1
         UWB(NBW) = ICHAR(STR(NXF:NXF))
   10 CONTINUE
      HPBWRT = .FALSE.
   20 RETURN
C
C Copyright (C) 1988-1996 Synoptics Ltd.
C
      END
C
C Semper 6 subsidiary module HPBFLU
C ----------------------------
C : conditional buffer flush :
C ----------------------------
C
      LOGICAL FUNCTION HPBFLU(NUM,IOS)
C
C HPBFLU(num): flushes buffer if < num bytes left (unconditionally
C              if num=0; should be used once to terminate)
C
      INTEGER NUM,IOS
      LOGICAL HPBPUT
C
      INCLUDE 'COMMON'
C
      INTEGER LUWB
      PARAMETER (LUWB=LNBUF/LNINT)
      LOGICAL TOFILE
C
      INTEGER UWB(LUWB),NBW
      EQUIVALENCE (RB6,UWB),(SMGI10,NBW),(SMGL3,TOFILE)
C
      HPBFLU = .FALSE.
      IF (NUM.NE.0) THEN
         IF (NBW+NUM.LE.LUWB) GOTO 10
      ENDIF
      IF (NBW.NE.0) THEN
C
C Output buffer to printer
C
         IF (TOFILE) THEN
            IF (HPBPUT(UWB,NBW,IOS)) GOTO 20
         ENDIF
         NBW=0
      ENDIF
C
   10 RETURN
   20 HPBFLU = .TRUE.
      GOTO 10
C
C Copyright (C) 1988-1996 Synoptics Ltd.
C
      END
C
C Semper 6 local routine HPBPUT
C
      LOGICAL FUNCTION HPBPUT(BUF,N,IOSPAR)
      INTEGER N,BUF(N),IOSPAR
C
C     'C' function
C
      LOGICAL EIKBYA
C
C     File descriptor
C
      INTEGER FD
      COMMON /HPLJFD/ FD
C
C     Size of conversion array
C
      INTEGER*4 N4
C
      INCLUDE 'COMMON'
C
      INTEGER IOS
C
      HPBPUT = .TRUE.
      IOS = 0
      N4 = N
      CALL CFORM(BUF,BUF,NFMINT,NFMBYT,N4)
      HPBPUT = EIKBYA ( 2, FD, BUF, N )
   10 IOSPAR = IOS
      RETURN
C
C Copyright (C) 1988-1996 Synoptics Ltd.
C
      END
