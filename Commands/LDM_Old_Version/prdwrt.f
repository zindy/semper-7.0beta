C Semper 6 processing module PRDWRT
C
      SUBROUTINE PRDWRT
C
C Provides verbs ADD/LIST read/write program data. The file
C name is supplied by means of the text key NAME.
C
      INTEGER LNBLNK !,IPACK
      LOGICAL CONOPT,FILMAK,FILSEA,FILSTR
      LOGICAL FORTDE,FORTRD,FORTWR,OPT,SEMIOE,VARSET
C
      INCLUDE 'COMMON'
C
      LOGICAL EXISTS,LNAME,LWRITE,LNEW,LOLD,OLDFIL
      CHARACTER*(FILMAX) FILE,FILENM
      CHARACTER*4 DFNAM
C
      INTEGER MYIOS
C
      INTEGER IOS,NF
C
      LWRITE = VERB.EQ.19579
      LNAME = VARSET(22453)
      IF (OPT(22623)) THEN
         IF (CONOPT(24484,22623)) GOTO 10
         LNEW = .TRUE.
         LOLD = .FALSE.
      ELSE
         LNEW = .FALSE.
         LOLD = OPT(24484)
      ENDIF
C
C Fetch file name from key NAME
C
      IF (LNAME) THEN
         DFNAM = '.spl'
         IF (LWRITE) THEN
            CALL OUTDEF(DFNAM)
         ELSE
            CALL INPDEF(DFNAM)
         ENDIF
         IF (FILSTR(' ',FILE,NF,LWRITE)) GOTO 10
         IF (NF .EQ. 0) GOTO 10
C
C If output then build full name
C
         IF (LWRITE) THEN
            IF (FILMAK(FILE,DFNAM,FILENM)) GOTO 10
            FILE = FILENM
         ENDIF
C
C See if file exists
C
         IF (FILSEA(FILE,DFNAM,FILENM,EXISTS)) GOTO 10
         IF (EXISTS) THEN
            FILE = FILENM
         ENDIF
C
         NF = LNBLNK(FILE)
C
         IF (LWRITE) THEN
C
C If WRITE and file already exists, delete it if option NEW
C
            OLDFIL = LOLD
C
            IF (EXISTS) THEN
               IF (LNEW) THEN
                  IF (FORTDE(RDWRTU,FILE,IOS)) GOTO 20
               ELSE IF (.NOT.LOLD) THEN
                  IDMESS = FILE(1:NF)
                  ERROR = 135
                  GOTO 10
               ENDIF
            ENDIF
         ELSE
C
C Fault non-existing source file for ADD
C
            IF (.NOT.EXISTS) THEN
               ERROR = 130
               IDMESS = FILE(1:NF)
               GOTO 10
            ENDIF
         ENDIF
C
C Try to open the file dynamically
C
         IF (LWRITE) THEN
            IF (FORTWR(RDWRTU,FILE,OLDFIL,IOS)) GOTO 20
         ELSE
            IF (FORTRD(RDWRTU,FILE,IOS)) GOTO 20
         ENDIF
      ENDIF
C
      CALL PRDWR2(LNAME,LWRITE)
C
C ****** CHANGE ******
C
C Close file
C
      IF (LNAME) CLOSE (RDWRTU,ERR=30,IOSTAT=MYIOS)
C
C ****** ****** ******
C
   10 RETURN
C
C I/O errors
C
   20 IF (SEMIOE(IOS,RDWRTU,FILE(1:NF))) GOTO 10
      GOTO 10
C
C I/O error with IOSTAT=
C
   30 IOS = MYIOS
      GOTO 20
C
C Copyright (C) 1987-1996 : Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 processing module PRDWR2
C
      SUBROUTINE PRDWR2(LNAME,LWRITE)
C
C Add new procedure to library from file or keyboard, or
C dump named procedure (all procedures if PROGRAM not quoted)
C
      INCLUDE 'COMMON'
C
      INTEGER IVAL
      LOGICAL PRUADD,PRUIND,PRUENT,PRUSHO,PRUSOR,SEMKTX,SEMMED,ABANDN
      LOGICAL LWRITE,OPT,VARSET,LNAME,LALL
C
      INTEGER OUT(PRNACT),LENOUT,ISL,I,ITYPE,ITL,ILL
      INTEGER DEVICE,ISORT,NSORT,IDEV
C
      CHARACTER*(PRNACT) PROG
C
      INTEGER*4 I4N,I4N1,NDSLOT
C
      INTEGER LNSORT
      PARAMETER (LNSORT=LNBUF/LNINT)
      INTEGER SORTB(LNSORT),SORTI(LNSORT)
C
      INTEGER MEDIUM
C
C Packed names
C
      INTEGER NDEVIC,NPROGR,NALL
      PARAMETER (NDEVIC=6622,NPROGR=26335,NALL=2092)
C
      EQUIVALENCE (RB1,OUT),(RB3,SORTB),(RB4,SORTI)
C
      LALL = OPT(NALL)
C
C Check conflicting options
C
      IF (LALL .AND. VARSET(NPROGR)) THEN
         ERROR = 60
         IDERR = NALL
         IDERR2= NPROGR
         GOTO 60
      ENDIF
C
C If DEVICE is quoted then use it
C
      IF (VARSET(NDEVIC)) THEN
         DEVICE = IVAL(NDEVIC)
         IDERR=DEVICE
         IF (SEMMED(DEVICE,MEDIUM)) GOTO 60
         IF (MEDIUM .EQ. MEDDC .OR. MEDIUM .EQ. MEDVM) THEN
            IF (DVTYP(DEVICE) .NE. FLTRUN) THEN
               ERROR=35
               GOTO 60
            ENDIF
         ELSE
            ERROR=29
            GOTO 60
         ENDIF
         IF (LWRITE) THEN
C
C Force ALL option if DEVICE key specified without PROGRAM key
C
            IF (.NOT. VARSET(NPROGR)) LALL = .TRUE.
         ELSE
C
C If reading a program to the device then must have access
C
            IF (PROTN(DEVICE) .NE. 0) THEN
               ERROR = 41
               GOTO 60
            ENDIF
         ENDIF
      ELSE
         IF (LWRITE) THEN
C
C Search for suitable device later
C
            DEVICE = 0
         ELSE
C
C Find first R/W device
C
            IF (PTNUMB .NE. 0) THEN
               DO 10 I = 1,PTNUMB
                  DEVICE = PTPRIO(I)
                  IF (PROTN(DEVICE) .EQ. 0) GOTO 20
   10          CONTINUE
            ENDIF
C
C No R/W Device found
C
            ERROR = 77
            IDMESS = 'No writeable program library devices found'
            GOTO 60
         ENDIF
      ENDIF
C
   20 CONTINUE
C
C Check for WRITE
C
      IF (LWRITE) THEN
C
C If program quoted then use it
C
         IF (.NOT.LALL) THEN
            LENOUT = PRNACT
            IF (SEMKTX(NPROGR,'Program name (as textstring): ',
     +                 OUT,LENOUT,.FALSE.)) GOTO 60
            IF (LENOUT .EQ. 0) GOTO 60
            CALL SEMCHS(PROG,OUT,LENOUT)
C
C Find a first entry
C
            IF (PRUENT(DEVICE,ISL,LENOUT,OUT)) GOTO 60
            IF (ISL .EQ. 0) THEN
C
C Not found ?
C
               ERROR = 125
               IDMESS = PROG(1:LENOUT)
               GOTO 60
            ELSE
               IF (PRUSHO(LNAME,DEVICE,ISL,0)) GOTO 60
            ENDIF
         ELSE
C
C Dump all programs
C
            IDEV = DEVICE
            DO 50 I = 1,PTNUMB
               IF (IDEV .NE. 0) THEN
                  DEVICE = IDEV
               ELSE
                  DEVICE = PTPRIO(I)
               ENDIF
C
C Ask for a sorted list of all keys
C
               IF (PRUSOR(DEVICE,NDSLOT,SORTB,SORTI,NSORT)) GOTO 60
               ISORT = 0
C
C Get next entry, switching code acc to whether sorted
C
   30          CONTINUE
               IF (NSORT.GE.0) THEN
C
C Sorted: refer to pointer list
C
                  IF (ISORT.GE.NSORT) GOTO 40
                  ISORT = ISORT + 1
                  ISL = SORTB(ISORT)
               ELSE
C
C Unsorted: refer to entry directly
C
                  I4N = ISORT
                  IF (I4N.GE.NDSLOT) GOTO 40
                  ISORT = ISORT + 1
                  ISL = ISORT
               ENDIF
C
C Get key and see if active
C
               IF (PRUIND(1,DEVICE,ISL,I4N,I4N1,
     +                    ITYPE,ITL,ILL,LENOUT,OUT)) GOTO 60
               IF (ITYPE.EQ.2) THEN
C
C Write active entry
C
                  IF (PRUSHO(LNAME,DEVICE,ISL,0)) GOTO 60
C
C Abandon or other error (e.g. previous abandon!)
C
                  IF (ABANDN(ERROR)) GOTO 60
                  IF (ERROR .NE. 0) GOTO 60
               ENDIF
               GOTO 30
   40          IF (IDEV .NE. 0) GOTO 60
   50       CONTINUE
         ENDIF
      ELSE
C
         IF (PRUADD(LNAME,DEVICE)) GOTO 60
      ENDIF
C
   60 RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
