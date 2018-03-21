      LOGICAL FUNCTION UIFSAV ( NAME, LENGTH )
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
C     Variable value table pointer
C
      REAL VARTAB(NVARS)
      EQUIVALENCE (ROVVS,VARTAB)
C
C     CALLED FUNCTIONS:
C
C     Opens a file
C
      LOGICAL OPEFIL
C
C     Puts data items out to a file
C
      LOGICAL PUTFIL,PUTFIN,PUTFLG
C
C     Flushes output to a file
C
      LOGICAL FLUFIL
C
C     Closes a file
C
      LOGICAL CLOFIL
C
C     Compresses memory
C
      LOGICAL COMPM
C
C     Open the requested file
C
      UIFBPR = 0
      STATUS = OPEFIL ( NAME, LENGTH, .TRUE. )
      IF ( .NOT. STATUS ) THEN
         STATUS = .TRUE.
         IF ( COMPM ( ) ) GOTO 150
C
C        Write out the crucial ID data
C
         IF ( PUTFIN ( UIFVID ) ) GOTO 150
         IF ( PUTFIN ( MINPAN ) ) GOTO 150
         IF ( PUTFIN ( MXNPAN ) ) GOTO 150
         IF ( PUTFIN ( MXNSCR ) ) GOTO 150
         IF ( PUTFIN ( MXNCEL ) ) GOTO 150
         IF ( PUTFIN ( MXNMEN ) ) GOTO 150
         IF ( PUTFIN ( MXNTEX ) ) GOTO 150
         IF ( PUTFIN ( NDEVIC ) ) GOTO 150
         IF ( PUTFIN ( STKSIZ ) ) GOTO 150
C
C        Write out the serial number part
C
         CALL UIXSER(ISER)
         DO 10 I = 15,34
            IF ( PUTFIN ( ISER(I) ) ) GOTO 150
   10    CONTINUE
C
C        Write out all object data
C
         DO 20 I = MINOBJ, MAXOBJ
            IF ( PUTFIN ( OBJNAM(I) ) ) GOTO 150
            IF ( PUTFIN ( OBJXPO(I) ) ) GOTO 150
            IF ( PUTFIN ( OBJYPO(I) ) ) GOTO 150
            IF ( PUTFIN ( OBJXSI(I) ) ) GOTO 150
            IF ( PUTFIN ( OBJYSI(I) ) ) GOTO 150
            IF ( PUTFIN ( OBJFGC(I) ) ) GOTO 150
            IF ( PUTFIN ( OBJBGC(I) ) ) GOTO 150
            IF ( PUTFIN ( OBJHOR(I) ) ) GOTO 150
            IF ( PUTFIN ( OBJVER(I) ) ) GOTO 150
   20    CONTINUE
C
C        Write out all window data
C
         DO 30 I = MINWIN, MAXWIN
            IF ( PUTFIN ( WINODE(I) ) ) GOTO 150
            IF ( PUTFLG ( WINISS(I) ) ) GOTO 150
            IF ( PUTFLG ( WINISC(I) ) ) GOTO 150
            IF ( PUTFIN ( WINWID(I) ) ) GOTO 150
            IF ( PUTFLG ( WINOBS(I) ) ) GOTO 150
            IF ( PUTFIN ( WINSTK(I) ) ) GOTO 150
            IF ( PUTFIN ( WINBWI(I) ) ) GOTO 150
            IF ( PUTFLG ( WINEXS(I) ) ) GOTO 150
            IF ( PUTFIN ( WINESX(I) ) ) GOTO 150
            IF ( PUTFIN ( WINESY(I) ) ) GOTO 150
   30    CONTINUE
C
C        Write out all panel data
C
         DO 40 I = MINPAN, MAXPAN
            IF ( PUTFLG ( PANAUT(I) ) ) GOTO 150
            IF ( PUTFLG ( PANFIX(I) ) ) GOTO 150
            IF ( PUTFLG ( PANMAN(I) ) ) GOTO 150
   40    CONTINUE
C
C        Write out all element data
C
         DO 50 I = MINELE, MAXELE
            IF ( PUTFIN ( ELEOPA(I) ) ) GOTO 150
            IF ( PUTFIN ( ELEBAC(I) ) ) GOTO 150
            IF ( PUTFIN ( ELECAC(I) ) ) GOTO 150
            IF ( PUTFIN ( ELEEAC(I) ) ) GOTO 150
            IF ( PUTFLG ( ELEEXS(I) ) ) GOTO 150
            IF ( PUTFIN ( ELEESX(I) ) ) GOTO 150
            IF ( PUTFIN ( ELEESY(I) ) ) GOTO 150
   50    CONTINUE
C
C        Write out all cell data
C
         DO 60 I = MINCEL, MAXCEL
            IF ( PUTFIN ( CELTYP(I) ) ) GOTO 150
            IF ( PUTFIN ( CELCON(I) ) ) GOTO 150
            IF ( PUTFIN ( CELSTY(I) ) ) GOTO 150
            IF ( PUTFIN ( CELCYC(I) ) ) GOTO 150
            IF ( PUTFIN ( CELOME(I) ) ) GOTO 150
            IF ( PUTFIN ( CELROW(I) ) ) GOTO 150
            IF ( PUTFIN ( CELCOL(I) ) ) GOTO 150
            IF ( PUTFIN ( CELXOF(I) ) ) GOTO 150
            IF ( PUTFIN ( CELYOF(I) ) ) GOTO 150
            IF ( PUTFLG ( CELDRO(I) ) ) GOTO 150
            IF ( PUTFLG ( CELBOX(I) ) ) GOTO 150
   60    CONTINUE
C
C        Write out all menu data
C
         DO 70 I = MINMEN, MAXMEN
            IF ( PUTFIN ( MENTYP(I) ) ) GOTO 150
            IF ( PUTFIN ( MENSTY(I) ) ) GOTO 150
            IF ( PUTFLG ( MENISA(I) ) ) GOTO 150
            IF ( PUTFIN ( MENCID(I) ) ) GOTO 150
            IF ( PUTFIN ( MENPAN(I) ) ) GOTO 150
   70    CONTINUE
C
C        Write out all textfield data
C
         DO 80 I = MINTEX, MAXTEX
            IF ( PUTFLG ( TEXNUM(I) ) ) GOTO 150
            IF ( PUTFLG ( TEXWP(I) ) ) GOTO 150
            IF ( PUTFIN ( TEXCON(I) ) ) GOTO 150
            IF ( PUTFIN ( TEXCLE(I) ) ) GOTO 150
            IF ( PUTFIN ( TEXLEN(I) ) ) GOTO 150
            IF ( PUTFIN ( TEXCPO(I) ) ) GOTO 150
            IF ( PUTFIN ( TEXMIR(I) ) ) GOTO 150
            IF ( PUTFIN ( TEXMAR(I) ) ) GOTO 150
            IF ( PUTFIN ( TEXMIO(I) ) ) GOTO 150
            IF ( PUTFIN ( TEXMAO(I) ) ) GOTO 150
            IF ( PUTFIN ( TEXOOR(I) ) ) GOTO 150
            IF ( PUTFIN ( TEXTAF(I) ) ) GOTO 150
   80    CONTINUE
C
C        Write out all global data
C
C         IF ( PUTFLG ( UIFISI ) ) GOTO 150
         IF ( PUTFIN ( CURDEV ) ) GOTO 150
         IF ( PUTFIN ( CURPID ) ) GOTO 150
         IF ( PUTFIN ( CURID ) ) GOTO 150
         IF ( PUTFLG ( MPANSH ) ) GOTO 150
         IF ( PUTFIN ( MPANID ) ) GOTO 150
         IF ( PUTFLG ( UIFAAC ) ) GOTO 150
         IF ( PUTFIN ( BEFACT ) ) GOTO 150
         IF ( PUTFIN ( AFTACT ) ) GOTO 150
         DO 90 I = 1, NDEVIC
            IF ( PUTFIN ( DEVXSI(I) ) ) GOTO 150
            IF ( PUTFIN ( DEVYSI(I) ) ) GOTO 150
            IF ( PUTFIN ( DEVCXS(I) ) ) GOTO 150
            IF ( PUTFIN ( DEVCYS(I) ) ) GOTO 150
            IF ( PUTFIN ( DEVMXS(I) ) ) GOTO 150
            IF ( PUTFIN ( DEVMYS(I) ) ) GOTO 150
            IF ( PUTFIN ( DEVXMI(I) ) ) GOTO 150
            IF ( PUTFIN ( DEVYMI(I) ) ) GOTO 150
            IF ( PUTFIN ( DEVXOF(I) ) ) GOTO 150
            IF ( PUTFIN ( DEVYOF(I) ) ) GOTO 150
            IF ( PUTFLG ( DEVHAP(I) ) ) GOTO 150
            IF ( PUTFIN ( DEVNCO(I) ) ) GOTO 150
   90    CONTINUE
         IF ( PUTFIN ( VERPOS ) ) GOTO 150
         IF ( PUTFIN ( HORPOS ) ) GOTO 150
         IF ( PUTFIN ( CLIPR ) ) GOTO 150
         IF ( PUTFIN ( CLIPL ) ) GOTO 150
         IF ( PUTFIN ( CLIPT ) ) GOTO 150
         IF ( PUTFIN ( CLIPB ) ) GOTO 150
         IF ( PUTFIN ( SELTEX ) ) GOTO 150
         IF ( PUTFIN ( ACTDEV ) ) GOTO 150
         IF ( PUTFIN ( LMBACT ) ) GOTO 150
         IF ( PUTFIN ( CMBACT ) ) GOTO 150
         IF ( PUTFIN ( RMBACT ) ) GOTO 150
C
         DO 110 I = 1, NDEVIC
C            IF ( PUTFIN ( CURMOX(I) ) ) GOTO 150
C            IF ( PUTFIN ( CURMOY(I) ) ) GOTO 150
            IF ( PUTFIN ( CURXPO(I) ) ) GOTO 150
            IF ( PUTFIN ( CURYPO(I) ) ) GOTO 150
            IF ( PUTFIN ( STKTOP(I) ) ) GOTO 150
            DO 100 J = 1, STKSIZ
               IF ( PUTFIN ( STKXPO(I,J) ) ) GOTO 150
               IF ( PUTFIN ( STKYPO(I,J) ) ) GOTO 150
  100       CONTINUE
            IF ( PUTFIN ( PIDL(I) ) ) GOTO 150
            IF ( PUTFIN ( EIDL(I) ) ) GOTO 150
  110    CONTINUE
C
C        Now write out all the dynamic memory stuff
C
         DO 120 I = 1, MXSTOR
            J = ICHAR ( CSTORE(I:I) )
            IF ( PUTFIN ( J ) ) GOTO 150
  120    CONTINUE
C
         DO 130 I = 1, MXLTAB
            IF ( PUTFIN ( LITAB(I) ) ) GOTO 150
            IF ( PUTFIN ( LSTAB(I) ) ) GOTO 150
  130    CONTINUE
C
         IF ( PUTFLG ( ISINIT ) ) GOTO 150
         IF ( PUTFIN ( HILIND ) ) GOTO 150
         IF ( PUTFIN ( HIPIND ) ) GOTO 150
Cc
Cc        Now the values of all Semper variables
Cc
C         IF ( APPGET ( 'PNO', 3, VALUE ) ) GOTO 150
C         J = INT ( VALUE )
C         IF ( PUTFIN ( J ) ) GOTO 150
C         IF ( APPGET ( 'ENO', 3, VALUE ) ) GOTO 150
C         J = INT ( VALUE )
C         IF ( PUTFIN ( J ) ) GOTO 150
C
C        Save the current state of all the Semper variables
C        Loop looking at all the possible variable names
C
         I = NSEMVE - NFIXED
         IF ( PUTFIN ( I ) ) GOTO 150
         DO 140 I = NFIXED+1,NSEMVE
C
C           Save the variable name and value
C
            IF ( PUTFIN ( VNAMES(I) ) ) GOTO 150
            IF ( PUTFIL ( 3, VARTAB(I) ) ) GOTO 150
  140    CONTINUE
         STATUS = .FALSE.
C
C        All done, close the file
C
  150    CONTINUE
C
         IF ( FLUFIL ( ) ) STATUS = .TRUE.
         IF ( CLOFIL ( NAME, LENGTH, .FALSE. ) ) STATUS = .TRUE.
      ENDIF
C
      UIFSAV = STATUS
C
      RETURN
      END
C
      LOGICAL FUNCTION PUTFIN ( VALUE )
C
C Put integer to file
C
      INTEGER VALUE(*)
      LOGICAL PUTFIL
C
      PUTFIN = PUTFIL( 1, VALUE )
      RETURN
      END
C
      LOGICAL FUNCTION PUTFLG ( VALUE )
C
C Put logical to file
C
      INTEGER VALUE(*)
      LOGICAL PUTFIL
C
      PUTFLG = PUTFIL( 2, VALUE )
      RETURN
      END
C
