      LOGICAL FUNCTION UIFREA ( NAME, LENGTH )
C     ========================================
C
      CHARACTER*(*) NAME
      INTEGER LENGTH
C
      INCLUDE 'COMMON'
      INCLUDE 'UIFCOM'
      INCLUDE 'UIXCOM'
      INCLUDE 'STORECOM'
      INCLUDE 'LOGINDEX'
C
C     LOCAL VARIABLES:
C
C     Return status
C
      LOGICAL STATUS
C
C     Loop counters
C
      INTEGER I, J
C
C     Serial number check vector
C
      INTEGER ISER(34)
C
C     Integer character value
C
      INTEGER IVALUE
C
C     Value of real variable
C
      REAL VALUE
C
C     CALLLED FUNCTIONS:
C
C     Opens a file
C
      LOGICAL OPEFIL
C
C     Gets data in from a file and validates
C
      LOGICAL GCHKFL
C
C     Gets data in from a file
C
      LOGICAL GETFIL,GETFIN,GETFLG
C
C     Sets the value of an application variable
C
C      LOGICAL APPACK
C
C     Sets the value of a Semper variable
C
      LOGICAL SEMLU
C
C     Closes a file
C
      LOGICAL CLOFIL
C
C     Open the requested file
C
      UIFBPR = 512
      STATUS = OPEFIL ( NAME, LENGTH, .FALSE. )
      IF ( .NOT. STATUS ) THEN
         STATUS = .TRUE.
C
C        Read and check the crucial ID data
C
         IF ( GCHKFL ( UIFVID ) ) GOTO 160
         IF ( GCHKFL ( MINPAN ) ) GOTO 160
         IF ( GCHKFL ( MXNPAN ) ) GOTO 160
         IF ( GCHKFL ( MXNSCR ) ) GOTO 160
         IF ( GCHKFL ( MXNCEL ) ) GOTO 160
         IF ( GCHKFL ( MXNMEN ) ) GOTO 160
         IF ( GCHKFL ( MXNTEX ) ) GOTO 160
         IF ( GCHKFL ( NDEVIC ) ) GOTO 160
         IF ( GCHKFL ( STKSIZ ) ) GOTO 160
C
         CALL UIXSER ( ISER )
C
C        Check serial number in file - host only
C
         DO 10 I = 15,25
            IF ( GCHKFL ( ISER(I) ) ) GOTO 160
   10    CONTINUE
C
C        Skip actual details
C
         DO 20 I = 26,34
            IF ( GETFIN ( ISER(I) ) ) GOTO 160
   20    CONTINUE
C
C        Read in all object data
C
         DO 30 I = MINOBJ, MAXOBJ
            IF ( GETFIN ( OBJNAM(I) ) ) GOTO 160
            IF ( GETFIN ( OBJXPO(I) ) ) GOTO 160
            IF ( GETFIN ( OBJYPO(I) ) ) GOTO 160
            IF ( GETFIN ( OBJXSI(I) ) ) GOTO 160
            IF ( GETFIN ( OBJYSI(I) ) ) GOTO 160
            IF ( GETFIN ( OBJFGC(I) ) ) GOTO 160
            IF ( GETFIN ( OBJBGC(I) ) ) GOTO 160
            IF ( GETFIN ( OBJHOR(I) ) ) GOTO 160
            IF ( GETFIN ( OBJVER(I) ) ) GOTO 160
   30    CONTINUE
C
C        Read in all window data
C
         DO 40 I = MINWIN, MAXWIN
            IF ( GETFIN ( WINODE(I) ) ) GOTO 160
            IF ( GETFLG ( WINISS(I) ) ) GOTO 160
            IF ( GETFLG ( WINISC(I) ) ) GOTO 160
            IF ( GETFIN ( WINWID(I) ) ) GOTO 160
            IF ( GETFLG ( WINOBS(I) ) ) GOTO 160
            IF ( GETFIN ( WINSTK(I) ) ) GOTO 160
            IF ( GETFIN ( WINBWI(I) ) ) GOTO 160
            IF ( GETFLG ( WINEXS(I) ) ) GOTO 160
            IF ( GETFIN ( WINESX(I) ) ) GOTO 160
            IF ( GETFIN ( WINESY(I) ) ) GOTO 160
   40    CONTINUE
C
C        Read in all panel data
C
         DO 50 I = MINPAN, MAXPAN
            IF ( GETFLG ( PANAUT(I) ) ) GOTO 160
            IF ( GETFLG ( PANFIX(I) ) ) GOTO 160
            IF ( GETFLG ( PANMAN(I) ) ) GOTO 160
   50    CONTINUE
C
C        Read in all element data
C
         DO 60 I = MINELE, MAXELE
            IF ( GETFIN ( ELEOPA(I) ) ) GOTO 160
            IF ( GETFIN ( ELEBAC(I) ) ) GOTO 160
            IF ( GETFIN ( ELECAC(I) ) ) GOTO 160
            IF ( GETFIN ( ELEEAC(I) ) ) GOTO 160
            IF ( GETFLG ( ELEEXS(I) ) ) GOTO 160
            IF ( GETFIN ( ELEESX(I) ) ) GOTO 160
            IF ( GETFIN ( ELEESY(I) ) ) GOTO 160
   60    CONTINUE
C
C        Read in all cell data
C
         DO 70 I = MINCEL, MAXCEL
            IF ( GETFIN ( CELTYP(I) ) ) GOTO 160
            IF ( GETFIN ( CELCON(I) ) ) GOTO 160
            IF ( GETFIN ( CELSTY(I) ) ) GOTO 160
            IF ( GETFIN ( CELCYC(I) ) ) GOTO 160
            IF ( GETFIN ( CELOME(I) ) ) GOTO 160
            IF ( GETFIN ( CELROW(I) ) ) GOTO 160
            IF ( GETFIN ( CELCOL(I) ) ) GOTO 160
            IF ( GETFIN ( CELXOF(I) ) ) GOTO 160
            IF ( GETFIN ( CELYOF(I) ) ) GOTO 160
            IF ( GETFLG ( CELDRO(I) ) ) GOTO 160
            IF ( GETFLG ( CELBOX(I) ) ) GOTO 160
   70    CONTINUE
C
C        Read in all menu data
C
         DO 80 I = MINMEN, MAXMEN
            IF ( GETFIN ( MENTYP(I) ) ) GOTO 160
            IF ( GETFIN ( MENSTY(I) ) ) GOTO 160
            IF ( GETFLG ( MENISA(I) ) ) GOTO 160
            IF ( GETFIN ( MENCID(I) ) ) GOTO 160
            IF ( GETFIN ( MENPAN(I) ) ) GOTO 160
   80    CONTINUE
C
C        Read in all textfield data
C
         DO 90 I = MINTEX, MAXTEX
            IF ( GETFLG ( TEXNUM(I) ) ) GOTO 160
            IF ( GETFLG ( TEXWP(I) ) ) GOTO 160
            IF ( GETFIN ( TEXCON(I) ) ) GOTO 160
            IF ( GETFIN ( TEXCLE(I) ) ) GOTO 160
            IF ( GETFIN ( TEXLEN(I) ) ) GOTO 160
            IF ( GETFIN ( TEXCPO(I) ) ) GOTO 160
            IF ( GETFIN ( TEXMIR(I) ) ) GOTO 160
            IF ( GETFIN ( TEXMAR(I) ) ) GOTO 160
            IF ( GETFIN ( TEXMIO(I) ) ) GOTO 160
            IF ( GETFIN ( TEXMAO(I) ) ) GOTO 160
            IF ( GETFIN ( TEXOOR(I) ) ) GOTO 160
            IF ( GETFIN ( TEXTAF(I) ) ) GOTO 160
   90    CONTINUE
C
C        Read in all global data
C
C         IF ( GETFLG ( UIFISI ) ) GOTO 160
         IF ( GETFIN ( CURDEV ) ) GOTO 160
         IF ( GETFIN ( CURPID ) ) GOTO 160
         IF ( GETFIN ( CURID ) ) GOTO 160
         IF ( GETFLG ( MPANSH ) ) GOTO 160
         IF ( GETFIN ( MPANID ) ) GOTO 160
         IF ( GETFLG ( UIFAAC ) ) GOTO 160
         IF ( GETFIN ( BEFACT ) ) GOTO 160
         IF ( GETFIN ( AFTACT ) ) GOTO 160
         DO 100 I = 1, NDEVIC
            IF ( GETFIN ( DEVXSI(I) ) ) GOTO 160
            IF ( GETFIN ( DEVYSI(I) ) ) GOTO 160
            IF ( GETFIN ( DEVCXS(I) ) ) GOTO 160
            IF ( GETFIN ( DEVCYS(I) ) ) GOTO 160
            IF ( GETFIN ( DEVMXS(I) ) ) GOTO 160
            IF ( GETFIN ( DEVMYS(I) ) ) GOTO 160
            IF ( GETFIN ( DEVXMI(I) ) ) GOTO 160
            IF ( GETFIN ( DEVYMI(I) ) ) GOTO 160
            IF ( GETFIN ( DEVXOF(I) ) ) GOTO 160
            IF ( GETFIN ( DEVYOF(I) ) ) GOTO 160
            IF ( GETFLG ( DEVHAP(I) ) ) GOTO 160
            IF ( GETFIN ( DEVNCO(I) ) ) GOTO 160
  100    CONTINUE
         IF ( GETFIN ( VERPOS ) ) GOTO 160
         IF ( GETFIN ( HORPOS ) ) GOTO 160
         IF ( GETFIN ( CLIPR ) ) GOTO 160
         IF ( GETFIN ( CLIPL ) ) GOTO 160
         IF ( GETFIN ( CLIPT ) ) GOTO 160
         IF ( GETFIN ( CLIPB ) ) GOTO 160
         IF ( GETFIN ( SELTEX ) ) GOTO 160
         IF ( GETFIN ( ACTDEV ) ) GOTO 160
         IF ( GETFIN ( LMBACT ) ) GOTO 160
         IF ( GETFIN ( CMBACT ) ) GOTO 160
         IF ( GETFIN ( RMBACT ) ) GOTO 160
C
         DO 120 I = 1, NDEVIC
C            IF ( GETFIN ( CURMOX(I) ) ) GOTO 160
C            IF ( GETFIN ( CURMOY(I) ) ) GOTO 160
            IF ( GETFIN ( CURXPO(I) ) ) GOTO 160
            IF ( GETFIN ( CURYPO(I) ) ) GOTO 160
            IF ( GETFIN ( STKTOP(I) ) ) GOTO 160
            DO 110 J = 1, STKSIZ
               IF ( GETFIN ( STKXPO(I,J) ) ) GOTO 160
               IF ( GETFIN ( STKYPO(I,J) ) ) GOTO 160
  110       CONTINUE
            IF ( GETFIN ( PIDL(I) ) ) GOTO 160
            IF ( GETFIN ( EIDL(I) ) ) GOTO 160
  120    CONTINUE
C
C        Now read in all the dynamic memory stuff
C
         DO 130 I = 1, MXSTOR
            IF ( GETFIN ( IVALUE ) ) GOTO 160
            CSTORE(i:i) = CHAR( IVALUE)
  130    CONTINUE
C
         DO 140 I = 1, MXLTAB
            IF ( GETFIN ( LITAB(I) ) ) GOTO 160
            IF ( GETFIN ( LSTAB(I) ) ) GOTO 160
  140    CONTINUE
C
         IF ( GETFLG ( ISINIT ) ) GOTO 160
         IF ( GETFIN ( HILIND ) ) GOTO 160
         IF ( GETFIN ( HIPIND ) ) GOTO 160
Cc
Cc        Now the values of some UIF variables
Cc
C         IF ( GETFIN ( VALUE ) ) GOTO 160
C         IF ( APPACK ( 26175, REAL ( VALUE ) ) ) GOTO 160
C         IF ( GETFIN ( VALUE ) ) GOTO 160
C         IF ( APPACK ( 8575, REAL ( VALUE ) ) ) GOTO 160
C
C        Recover the state of all Semper non-fixed variables
C
         IF ( GETFIN ( I ) ) GOTO 160
         DO 150 J = 1,I
            IF ( GETFIN ( IVALUE ) ) GOTO 160
            IF ( GETFIL ( 3, VALUE ) ) GOTO 160
            IF ( SEMLU ( 1, IVALUE, VALUE ) ) GOTO 160
  150    CONTINUE
         STATUS = .FALSE.
C
C        All done, close the file
C
  160    CONTINUE
C
         IF ( CLOFIL ( NAME, LENGTH, .FALSE. ) ) STATUS = .TRUE.
      ENDIF
C
      UIFREA = STATUS
C
      RETURN
      END
C
      LOGICAL FUNCTION GCHKFL ( VALUE )
C     =================================
      INTEGER VALUE
C
      LOGICAL GETFIN
C
      INCLUDE 'COMMON'
      INCLUDE 'UIFCOM'
      INCLUDE 'UIFERR'
C
      INTEGER I
      LOGICAL STATUS
C
      STATUS = GETFIN ( I )
      IF (.NOT. STATUS) THEN
         IF (I .NE. VALUE) THEN
            IDMESS = 'Wrong version UIF file'
            UIFERR = 77 - UIFEBA
            STATUS = .TRUE.
         ENDIF
      ENDIF
C
      GCHKFL = STATUS
      RETURN
C
      END
C
      LOGICAL FUNCTION GETFIN ( VALUE )
C     =================================
C
      INTEGER VALUE(*)
C
C Read an integer from file
C
      LOGICAL GETFIL
      GETFIN = GETFIL ( 1, VALUE )
      RETURN
      END
C
      LOGICAL FUNCTION GETFLG ( VALUE )
C     =================================
C
      INTEGER VALUE(*)
C
C Read a logical from file
C
      LOGICAL GETFIL
      GETFLG = GETFIL ( 2, VALUE )
      RETURN
      END
C
      LOGICAL FUNCTION GETFIL ( TYPE, VALUE )
C     =======================================
C
      INTEGER TYPE
      INTEGER VALUE(*)
C
      INCLUDE 'COMMON'
      INCLUDE 'UIFCOM'
      INCLUDE 'UIXCOM'
      INCLUDE 'UIFERR'
C
C     LOCAL VARIABLES:
C
C     Return status
C
      LOGICAL STATUS
C
C     index variable
C
      INTEGER IND
C
C     PARAMETERS
C
C     number of integers that hold a real
C
      INTEGER INTINR
      PARAMETER (INTINR=LNREAL/LNINT)
C
C     CALLED FUNCTIONS:
C
C     Reads data into uifbuf from disc
C
      LOGICAL REAFIL
C
      IF ( TYPE .EQ. 1 .OR. TYPE .EQ. 2 ) THEN
C
C        If the uifbuf is empty, read into it
C
         IF ( UIFBPR + 1 .GT. UBUFSZ ) THEN
            STATUS = REAFIL ( )
         ELSE
            STATUS = .FALSE.
         ENDIF
C
C        And pass back the data
C
         IF ( .NOT. STATUS ) THEN
            UIFBPR = UIFBPR + 1
            VALUE(1) = UIFBUF(UIFBPR)
         ENDIF
      ELSE IF ( TYPE .EQ. 3 ) THEN
C
C        If the uifbuf is empty, read into it
C
         IF ( UIFBPR + INTINR .GT. UBUFSZ ) THEN
            STATUS = REAFIL ( )
         ELSE
            STATUS = .FALSE.
         ENDIF
C
C        And pass back the data
C
         IF ( .NOT. STATUS ) THEN
            DO 10 IND = 1,INTINR
               UIFBPR = UIFBPR + 1
               VALUE(IND) = UIFBUF(UIFBPR)
   10       CONTINUE
         ENDIF
      ENDIF
C
      GETFIL = STATUS
C
      RETURN
      END
C
      LOGICAL FUNCTION REAFIL ( )
C     ===========================
C
      INCLUDE 'COMMON'
      INCLUDE 'UIFCOM'
      INCLUDE 'UIXCOM'
      INCLUDE 'UIFERR'
C
C     LOCAL VARIABLES:
C
C     Return status
C
      LOGICAL STATUS
C
C     I/O status
C
      INTEGER IOS
C
C     Filename
C
      CHARACTER*80 FILENM
C
C     CALLED FUNCTIONS:
C
C     Diagnoses file i/o error
C
      LOGICAL SEMIOE
C
      STATUS = .FALSE.
      IF ( UIFBPR .GE. UBUFSZ-1 ) THEN
         READ ( RDWRTU, ERR=10, END=10, IOSTAT=IOS ) UIFBUF
C
C        Re-zero the uifbuf pointer
C
         UIFBPR = 0
         GOTO 20
C
   10    CONTINUE
         INQUIRE ( UNIT=RDWRTU, NAME=FILENM )
         STATUS = SEMIOE ( IOS, RDWRTU, FILENM )
         IF (ERROR .EQ. 0) ERROR = 10
         UIFERR = ERROR - UIFEBA
         STATUS = .TRUE.
      ENDIF
C
   20 REAFIL = STATUS
C
      RETURN
      END
