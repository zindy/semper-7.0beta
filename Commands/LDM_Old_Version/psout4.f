C Sub-processing module PSOUT4
C
      LOGICAL FUNCTION PSOUT4(NCOL,NROW,NLAY,CLASS,FORM,CCOL,CROW,
     +                        LDISP,LCOMPL,LBORDE,LORIGI,LCAPTN,LHEADE)
      INTEGER NCOL,NROW,NLAY,CLASS,FORM,CCOL,CROW
      LOGICAL LDISP,LCOMPL,LBORDE,LORIGI,LCAPTN,LHEADE
C
      LOGICAL SEMXA1,SEMLAB,SEMTEX,OPTNO,OPT,CONOPT
      LOGICAL PSOUTB,PSDEFI,PSDEFF,PSDEFS,PSDEFL
      INTEGER IVAL
      REAL VAL
C
      CHARACTER*11 DATSTR
      CHARACTER*8  TIMSTR
      INTEGER LABEL(256),DATIME(7),ITEXT(13)
      INTEGER CLEN(10),FLEN(0:3)
      INTEGER I,N,N1,N2,N3,ICOL1,ICOL2,IROW1,IROW2
      INTEGER IR1LEN,IR2LEN,NCOPY
      REAL TIMES
      CHARACTER*6 CLANDS,CCOMPL,CABOVE
      CHARACTER*11 CSTR(10)
      CHARACTER*7  FSTR(0:3)
      CHARACTER*13 RA1STR,RA2STR
      CHARACTER*11 CRDSTR,CUDSTR
      CHARACTER*8 CRTSTR,CUTSTR
      CHARACTER*156 TITLE
      CHARACTER*128 STRING
C
      INTEGER TEXMAX
      PARAMETER (TEXMAX=150)
C
      INTEGER A1TEXT(TEXMAX)
      CHARACTER*(TEXMAX) TEXT
C
      INCLUDE 'COMMON'
C
      EQUIVALENCE (ICOL1,SMGI1),(ICOL2,SMGI4)
      EQUIVALENCE (IROW1,SMGI2),(IROW2,SMGI5)
      EQUIVALENCE (A1TEXT,LABEL,DATIME,ITEXT,RB1)
C
C Packed names
C
      INTEGER NCOPIE,NTIMES,NPORTR,NLANDS,NBORDE,NORIGI,NTEXT,NABOVE
      INTEGER NBELOW,NHEADE
      PARAMETER (NCOPIE=5416, NTIMES=-374)
      PARAMETER (NPORTR=26218, NLANDS=19254, NBORDE=3818, NORIGI=24729)
      PARAMETER (NTEXT=-225, NABOVE=1695, NBELOW=3412, NHEADE=13001)
C
      DATA CSTR /'Image      ',
     +           'Macro      ',
     +           'Fourier    ',
     +           'Spectrum   ',
     +           'Correlation',
     +           'Undefined  ',
     +           'Walsh      ',
     +           'Plist      ',
     +           'Histogram  ',
     +           'Lut        '/
      DATA CLEN /5,5,7,8,11,9,5,5,9,3/
C
      DATA FSTR /'Byte   ',
     +           'Integer',
     +           'Fp  ',
     +           'Complex'/
      DATA FLEN /4,7,2,7/
C
C Determine number of copies to output
C
      NCOPY = IVAL(NCOPIE)
C
C Fault zero or negative number
C
      IF (NCOPY.LE.0) THEN
         ERROR=3
         IDERR=NCOPIE
         GOTO 10
      ENDIF
C
C Fetch value for TIMES key
C
      TIMES = VAL(NTIMES)
C
C Fault zero or negative value
C
      IF (TIMES.LE.0.0) THEN
         ERROR=3
         IDERR=NTIMES
         GOTO 10
      ENDIF
C
C Fault conflicting options PROTRAIT and LANDSCAPE
C
      IF (CONOPT(NPORTR,NLANDS)) GOTO 10
C
C See if option LANDSCAPE is set (default option is PORTRAIT)
C
      IF (OPT(NLANDS)) THEN
         CLANDS=' true'
      ELSE
         CLANDS=' false'
      ENDIF
C
C See if option BORDER is set (default option is BORDER)
C
      LBORDE=.NOT.OPTNO(NBORDE)
C
C See if option ORIGIN is set (default option is NOORIGIN)
C
      LORIGI=OPT(NORIGI)
C
C If either option set ...
C
      IF (LBORDE.OR.LORIGI) THEN
C
C ... see if outputting complex picture
C
         IF (LCOMPL) THEN
            CCOMPL=' true'
         ELSE
            CCOMPL=' false'
         ENDIF
      ENDIF
C
C See if TEXT key is set
C
      N=TEXMAX
      IF (SEMTEX(NTEXT,A1TEXT,N)) GOTO 10
      LCAPTN=N.NE.0
      IF (LCAPTN) THEN
C
C Fetch text string from key TEXT
C
         CALL SEMCHS(TEXT,A1TEXT,N)
         N1=MAX(1,N/3)
         N2=N1+N1
         N3=MAX(1,N-N2)
C
C Fault conflicting options ABOVE and BELOW
C
         IF (CONOPT(NABOVE,NBELOW)) GOTO 10
C
C See if option ABOVE is set (default option is BELOW)
C
         IF (OPT(NABOVE)) THEN
            CABOVE=' true'
         ELSE
            CABOVE=' false'
         ENDIF
      ENDIF
C
C See if option HEADER is set (default option is HEADER) - if display
C output, force NOHEADER
C
      LHEADE=.NOT.OPTNO(NHEADE)
      IF (LHEADE) THEN
C
C See if not dumping from screen
C
         IF (.NOT.LDISP) THEN
C
C Set up range strings
C
            IR1LEN=1
            IF (SEMXA1(4,ITEXT,13,IR1LEN,VMIN,I)) THEN
               IR1LEN=3
               RA1STR='***'
            ELSE
               IR1LEN=IR1LEN-1
               CALL SEMCHS(RA1STR,ITEXT,IR1LEN)
            ENDIF
C
            IR2LEN=1
            IF (SEMXA1(4,ITEXT,13,IR2LEN,VMAX,I)) THEN
               IR2LEN=3
               RA2STR='***'
            ELSE
               IR2LEN=IR2LEN-1
               CALL SEMCHS(RA2STR,ITEXT,IR2LEN)
            ENDIF
C
C Fetch picture label
C
            IF (SEMLAB(1,LABEL,LP1)) GOTO 10
C
C Extract creation date and time string from the picture label
C
            CRDSTR=DATSTR(LABEL(LBDAY),LABEL(LBMON),LABEL(LBYEAR)+1900)
            CRTSTR=TIMSTR(LABEL(LBHOUR),LABEL(LBMIN),LABEL(LBSEC))
C
C Extract title string from the picture label
C
            TITLE=' '
            N=LABEL(LBNCTT)
            IF (N.GT.0) CALL SEMCHS(TITLE,LABEL(LBTT1),N)
         ENDIF
C
C Set up current date and time string
C
         CALL MCTIME(DATIME)
         CUDSTR=DATSTR(DATIME(3),DATIME(2),DATIME(1))
         CUTSTR=TIMSTR(DATIME(4),DATIME(5),DATIME(6))
      ENDIF
C
C Write out this information
C
      IF (PSOUTB()) GOTO 30
      IF (PSDEFI('/#copies',NCOPY)) GOTO 30
      IF (PSOUTB()) GOTO 30
      IF (PSDEFF('/times',TIMES)) GOTO 30
      IF (PSDEFS('/landscape',CLANDS)) GOTO 30
C
      IF (LBORDE.OR.LORIGI) THEN
         IF (PSDEFS('/complex',CCOMPL)) GOTO 30
      ENDIF
C
      IF (LORIGI.OR.LHEADE) THEN
         IF (PSDEFI('/icol1',ICOL1)) GOTO 30
         IF (PSDEFI('/icol2',ICOL2)) GOTO 30
         IF (PSDEFI('/irow1',IROW1)) GOTO 30
         IF (PSDEFI('/irow2',IROW2)) GOTO 30
      ENDIF
C
      IF (LORIGI) THEN
         IF (PSDEFI('/ccol',CCOL)) GOTO 30
         IF (PSDEFI('/crow',CROW)) GOTO 30
      ENDIF
C
      IF (LCAPTN) THEN
         CALL PSOUT2(TEXT(1:N1),STRING,N)
         IF (PSDEFL('/caption1string',STRING(1:N))) GOTO 30
C
         CALL PSOUT2(TEXT(N1+1:N2),STRING,N)
         IF (PSDEFL('/caption2string',STRING(1:N))) GOTO 30
C
         CALL PSOUT2(TEXT(N2+1:N2+N3),STRING,N)
         IF (PSDEFL('/caption3string',STRING(1:N))) GOTO 30
C
         IF (PSDEFS('/above',CABOVE)) GOTO 30
      ENDIF
C
      IF (LHEADE) THEN
         IF (PSDEFL('/currentdatestring',CUDSTR)) GOTO 30
         IF (PSDEFL('/currenttimestring',CUTSTR)) GOTO 30
         IF (PSDEFI('/ncol',NCOL)) GOTO 30
         IF (PSDEFI('/nrow',NROW)) GOTO 30
C
         IF (.NOT.LDISP) THEN
            IF (PSDEFI('/nlay',NLAY)) GOTO 30
            IF (PSDEFL('/classstring',CSTR(CLASS)(1:CLEN(CLASS))
     +          )) GOTO 30
            IF (PSDEFL('/formstring',FSTR(FORM)(1:FLEN(FORM)))) GOTO 30
            IF (PSDEFL('/range1string',RA1STR(1:IR1LEN))) GOTO 30
            IF (PSDEFL('/range2string',RA2STR(1:IR2LEN))) GOTO 30
            IF (PSDEFL('/createdatestring',CRDSTR)) GOTO 30
            IF (PSDEFL('/createtimestring',CRTSTR)) GOTO 30
C
            CALL PSOUT2(TITLE(1:52),STRING,N)
            IF (PSDEFL('/title1string',STRING(1:N))) GOTO 30
C
            CALL PSOUT2(TITLE(53:104),STRING,N)
            IF (PSDEFL('/title2string',STRING(1:N))) GOTO 30
C
            CALL PSOUT2(TITLE(105:156),STRING,N)
            IF (PSDEFL('/title3string',STRING(1:N))) GOTO 30
         ENDIF
      ENDIF
C
   10 PSOUT4 = .FALSE.
   20 RETURN
C
   30 PSOUT4 = .TRUE.
      GOTO 20
C
C Copyright (C) 1988-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
