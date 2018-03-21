C Semper 6 processing module FRDWRF
C
      LOGICAL FUNCTION FRDWRF(FILE,OPENED,LWRITE,EOF,IOS)
      CHARACTER*(*) FILE
      LOGICAL OPENED,LWRITE,EOF
      INTEGER IOS
C
C Provides part of commands READ,WRITE:  Fortran-oriented picture
C data i/o, with flexible format.
C Reads/writes pictures from/to a sequential that is dynamically opened.
C The format for WRITE may be specified by the text key FORMAT.
C
      INTEGER IVALPN,SEMFRM !,IPACK
      LOGICAL FORTRD,FORTWR,OPT,SEMLAB,SEMOPN,SEMROW,SEMTEX
C
      INCLUDE 'COMMON'
C
      INTEGER FMTMAX
      PARAMETER (FMTMAX=20)
      INTEGER CLASS,FORM,I,IFORM,INFORM,J,K,L,M,N,NN
      INTEGER NCOL,NLAY,NROW,FLAG
C
      INTEGER MYIOS
C
      CHARACTER*(FMTMAX) FORMT
C
      INTEGER IB1(LNBUF/LNINT),LABEL(256),A1FORM(FMTMAX)
C
      EQUIVALENCE (RB1,IB1,LABEL),(RB5,A1FORM)
C
      FRDWRF = .FALSE.
      OPENED = .FALSE.
C
C If WRITE command, process FORMAT key
C
      IF (LWRITE) THEN
C
C Fetch dimensions, etc.
C
         NCOL=NCOLS(LP1)
         NROW=NROWS(LP1)
         NLAY=NLAYS(LP1)
         CLASS=CLASSN(LP1)
         FORM=FORMN(LP1)
C
C See if output is to be specially formatted
C
         NN = FMTMAX
         IF (SEMTEX(10218,A1FORM,NN)) GOTO 60
C
C If still no joy, set form dependent format
C
         IF (NN .EQ. 0) THEN
            IF (FORM.EQ.NFMBYT) THEN
               FORMT='(1X,24I3)'
            ELSE IF (FORM.EQ.NFMINT) THEN
               FORMT='(1X,12I6)'
            ELSE
               FORMT='(1X,1P6E12.5)'
            ENDIF
C
C Otherwise, set up specified format string
C
         ELSE
C
C Fault format string that is too long
C
            IF (NN .GT. FMTMAX) THEN
               ERROR = 77
               IDMESS = 'Specified FORMAT string is too long'
               GOTO 60
            ENDIF
C
C Upper case string
C
            CALL SEMUPP(A1FORM,NN)
C
C Copy format string into CHARACTER variable
C
            CALL SEMCHS(FORMT,A1FORM,NN)
         ENDIF
      ENDIF
C
C Try to open the file dynamically
C
      IF (LWRITE) THEN
         IF (FORTWR(RDWRTU,FILE,.FALSE.,IOS)) GOTO 80
      ELSE
         IF (FORTRD(RDWRTU,FILE,IOS)) GOTO 80
      ENDIF
C
      OPENED = .TRUE.
C
C Switch code on command name
C
      IF (LWRITE) THEN
C
C Code for WRITE
C ---------------
C
         IF (FORM.EQ.NFMBYT) THEN
            IFORM = 0
         ELSE IF (FORM.EQ.NFMINT) THEN
            IFORM = 1
         ELSE IF (FORM.EQ.NFMFP) THEN
            IFORM = 2
         ELSE IF (FORM.EQ.NFMCOM) THEN
            IFORM = 3
         ENDIF
C
C See if picture label and title available
C
         L = 0
         IF (LBLINC) THEN
            IF (.NOT.OPT(-2173)) L = 1
            N = LABEL(LBNCTT)
         ELSE
            N = 0
         ENDIF
         M = 2
C
         FLAG = N + 1000*L + 10000*M
C
C Output header line
C
   10    FORMAT (6I6,1X,A20)
         WRITE (RDWRTU,10,ERR=90,IOSTAT=MYIOS)
     +         NCOL,NROW,NLAY,CLASS,IFORM,FLAG,FORMT
C
C Write picture title if available
C
         IF (N .NE. 0) THEN
            CALL SEMCHS(RECORD(1:N),LABEL(LBTT1),N)
C
C This method used to allow wrap-around if title > 80 chars
C
   20       FORMAT (80A1)
            WRITE (RDWRTU,20,ERR=90,IOSTAT=MYIOS) (RECORD(I:I),I=1,N)
         ENDIF
C
C Write picture label if available
C
         IF (L .NE. 0) THEN
   30       FORMAT (16I4)
            WRITE (RDWRTU,30,ERR=90,IOSTAT=MYIOS) LABEL
         ENDIF
      ELSE
C
C Code for READ
C --------------
C
C Read header line
C
         READ (RDWRTU,10,END=70,ERR=90,IOSTAT=MYIOS)
     +        NCOL,NROW,NLAY,CLASS,IFORM,FLAG,FORMT
C
         IF (IFORM.EQ.0) THEN
            FORM = NFMBYT
         ELSE IF (IFORM.EQ.1) THEN
            FORM = NFMINT
         ELSE IF (IFORM.EQ.2) THEN
            FORM = NFMFP
         ELSE IF (IFORM.EQ.3) THEN
            FORM = NFMCOM
         ENDIF
C
         M = FLAG/10000
         L = (FLAG - M*10000)/1000
         N = FLAG - (L*1000) - (M*10000)
C
C Open picture
C
         LP1=0
         IF (SEMOPN(2,IVALPN(-601),NCOL,NROW,NLAY,CLASS,
     +              SEMFRM(FORM),LP1)) GOTO 60
C
C Read title if present
C
         IF (N.NE.0) THEN
            IF (M.EQ.0) THEN
               READ (RDWRTU,20,END=70,ERR=90,IOSTAT=MYIOS)
     +              (LABEL(I),I=LBNCTT+1,LBNCTT+N)
C
               CALL A1CONV(LABEL(LBTT1),N)
            ELSE
               READ (RDWRTU,20,END=70,ERR=90,IOSTAT=MYIOS)
     +              (RECORD(I:I),I=1,N)
C
               CALL SEMICS(RECORD(1:N),LABEL(LBTT1),N)
            ENDIF
C
            LABEL(LBNCTT)=N
         ENDIF
C
C Read picture label if present
C
         IF (L.NE.0) THEN
            READ (RDWRTU,30,END=70,ERR=90,IOSTAT=MYIOS) LABEL
         ENDIF
C
C Update label if possible
C
         IF ((N.NE.0.OR.L.NE.0).AND.LBLINC) THEN
            IF (SEMLAB(2,LABEL,LP1)) GOTO 60
         ENDIF
      ENDIF
C
C Establish local row form and length
C
      INFORM=FORM
      IF (FORM.EQ.NFMBYT) INFORM=NFMINT
      IF (FORM.EQ.NFMCOM) NCOL=2*NCOL
C
C Loop over layers
C
      DO 50 K=1,NLAY
C
C Loop over rows
C
         DO 40 J=1,NROW
C
C Switch code on command name
C
            IF (LWRITE) THEN
C
C Read source row from LP1
C
               IF (SEMROW(1,RB1,INFORM,J,K,LP1)) GOTO 60
C
C Write source row to file
C
               IF (INFORM.EQ.NFMINT) THEN
                  WRITE (RDWRTU,FORMT,ERR=90,IOSTAT=MYIOS)
     +                  (IB1(I),I=1,NCOL)
               ELSE
                  WRITE (RDWRTU,FORMT,ERR=90,IOSTAT=MYIOS)
     +                  (RB1(I),I=1,NCOL)
               ENDIF
            ELSE
C
C Read source row from file
C
               IF (INFORM.EQ.NFMINT) THEN
                  READ (RDWRTU,FORMT,END=70,ERR=90,IOSTAT=MYIOS)
     +                 (IB1(I),I=1,NCOL)
               ELSE
                  READ (RDWRTU,FORMT,END=70,ERR=90,IOSTAT=MYIOS)
     +                 (RB1(I),I=1,NCOL)
               ENDIF
C
C Store source row in LP1
C
               IF (SEMROW(2,RB1,INFORM,J,K,LP1)) GOTO 60
            ENDIF
   40    CONTINUE
   50 CONTINUE
C
C If READ, reset WSTAT to prevent range from being deleted on return
C to the command interpreter
C
      IF (.NOT.LWRITE) WSTAT(LP1)=0
C
   60 RETURN
C
C End-of-file error - set flag
C
   70 EOF = .TRUE.
C
C General I/O error - set flag
C
   80 FRDWRF = .TRUE.
      GOTO 60
C
C I/O error with IOSTAT=
C
   90 IOS = MYIOS
      GOTO 80
C
C Copyright (C) 1987-1996: Synoptics Ltd,  All Rights Reserved
C
      END
