C Semper 6 system module FORURD
C
C Attempts to open a FORTRAN unformatted unit to a file readonly
C Returns .TRUE. on error and sets IOS to the run time I/O error
C
      LOGICAL FUNCTION FORURD(IU,FILENM,IOSPAR)
C
      INTEGER IU,IOSPAR
      CHARACTER*(*) FILENM
C
      INTEGER IOS
C
      FORURD = .TRUE.
C
C     LDM CHANGE: removed READONLY
      OPEN (IU,FILE=FILENM,STATUS='OLD',ERR=10,FORM='UNFORMATTED',
     +         IOSTAT=IOS)
      REWIND(IU,IOSTAT=IOS,ERR=20)
      FORURD = .FALSE.
   10 IOSPAR = IOS
      RETURN
C
C Close file on rewind failure
C
   20 CLOSE(IU,ERR=10)
      GOTO 10
C
C Copyright (C) 1987-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 system module FORUWR
C
C Attempts to open a FORTRAN unformatted unit to a file for writing
C If OLD is set then tries to use an existing file...
C Returns .TRUE. on error and sets IOS to the run time I/O error
C
      LOGICAL FUNCTION FORUWR(IU,FILENM,OLD,IOSPAR)
C
      INTEGER IU,IOSPAR
      LOGICAL OLD
      CHARACTER*(*) FILENM
C
      INTEGER IOS
C
      CHARACTER*3 STAT
C
      INCLUDE 'COMMON'
C
      FORUWR = .TRUE.
C
      IF (OLD) THEN
         STAT = 'OLD'
      ELSE
         STAT = 'NEW'
      ENDIF
C
      OPEN (IU,FILE=FILENM,STATUS=STAT,ERR=10,FORM='UNFORMATTED',
     +      IOSTAT=IOS)
      REWIND(IU,IOSTAT=IOS,ERR=20)
      FORUWR = .FALSE.
   10 IOSPAR = IOS
      RETURN
C
C Close file on rewind failure
C
   20 CLOSE(IU,ERR=10)
      GOTO 10
C
C Copyright (C) 1987-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 processing module FRDWRU
C
      LOGICAL FUNCTION FRDWRU(FILE,OPENED,LWRITE,EOF,IOS)
      CHARACTER*(*) FILE
      LOGICAL OPENED,LWRITE,EOF
      INTEGER IOS
C
C Provides part of commands READ,WRITE:  Fortran-oriented picture
C data i/o with (FORTRAN) unformatted data files.
C Reads/writes pictures from/to a sequential that is dynamically opened.
C
      INTEGER IVALPN,SEMFRM !,IPACK
      LOGICAL FORURD,FORUWR,OPT,SEMLAB,SEMOPN,SEMROW
C
      INCLUDE 'COMMON'
C
      INTEGER MYIOS
C
      INTEGER CLASS,FORM,I,IFORM,INFORM,J,K,L,M,N
      INTEGER NCOL,NLAY,NROW,FLAG
C
      INTEGER IB1(LNBUF/LNINT),LABEL(256)
C
      EQUIVALENCE (RB1,IB1,LABEL)
C
      FRDWRU = .FALSE.
      OPENED = .FALSE.
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
      ENDIF
C
C Try to open the file dynamically
C
      IF (LWRITE) THEN
         IF (FORUWR(RDWRTU,FILE,.FALSE.,IOS)) GOTO 50
      ELSE
         IF (FORURD(RDWRTU,FILE,IOS)) GOTO 50
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
         WRITE(RDWRTU,ERR=60,IOSTAT=MYIOS)
     +                NCOL,NROW,NLAY,CLASS,IFORM,FLAG
C
C Write picture title if available
C
         IF (N .NE. 0) THEN
            CALL SEMCHS(RECORD(1:N),LABEL(LBTT1),N)
            WRITE (RDWRTU,ERR=60,IOSTAT=MYIOS) RECORD(1:N)
         ENDIF
C
C Write picture label if available
C
         IF (L .NE. 0) WRITE (RDWRTU,ERR=60,IOSTAT=MYIOS) LABEL
      ELSE
C
C Code for READ
C --------------
C
C Read header line
C
         READ (RDWRTU,END=40,ERR=60,IOSTAT=MYIOS)
     +                NCOL,NROW,NLAY,CLASS,IFORM,FLAG
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
     +              SEMFRM(FORM),LP1)) GOTO 30
C
C Read title if present
C
         IF (N.NE.0) THEN
            IF (M.EQ.0) THEN
               READ (RDWRTU,END=40,ERR=60,IOSTAT=MYIOS)
     +                      (LABEL(I),I=LBNCTT+1,LBNCTT+N)
               CALL A1CONV(LABEL(LBTT1),N)
            ELSE
               READ (RDWRTU,END=40,ERR=60,IOSTAT=MYIOS) RECORD(1:N)
               CALL SEMICS(RECORD(1:N),LABEL(LBTT1),N)
            ENDIF
C
            LABEL(LBNCTT)=N
         ENDIF
C
C Read picture label if present
C
         IF (L.NE.0) READ (RDWRTU,END=40,ERR=60,IOSTAT=MYIOS) LABEL
C
C Update label if possible
C
         IF ((N.NE.0.OR.L.NE.0).AND.LBLINC) THEN
            IF (SEMLAB(2,LABEL,LP1)) GOTO 30
         ENDIF
      ENDIF
C
C Establish local row form and length
C
      INFORM=FORM
      IF (FORM.EQ.NFMBYT) THEN
         IF (M.GE.2) THEN
            NCOL=(NCOL+(LNINT-1))/LNINT
         ELSE
            INFORM=NFMINT
         ENDIF
      ENDIF
      IF (FORM.EQ.NFMCOM) NCOL=2*NCOL
C
C Loop over layers
C
      DO 20 K=1,NLAY
C
C Loop over rows
C
         DO 10 J=1,NROW
C
C Switch code on command name
C
            IF (LWRITE) THEN
C
C Read source row from LP1
C
               IF (SEMROW(1,RB1,INFORM,J,K,LP1)) GOTO 30
C
C Write source row to file
C
               IF (INFORM.EQ.NFMBYT.OR.INFORM.EQ.NFMINT) THEN
                  WRITE (RDWRTU,ERR=60,IOSTAT=MYIOS)
     +                  (IB1(I),I=1,NCOL)
               ELSE
                  WRITE (RDWRTU,ERR=60,IOSTAT=MYIOS)
     +                  (RB1(I),I=1,NCOL)
               ENDIF
            ELSE
C
C Read source row from file
C
               IF (INFORM.EQ.NFMBYT.OR.INFORM.EQ.NFMINT) THEN
                  READ (RDWRTU,END=40,ERR=60,IOSTAT=MYIOS)
     +                 (IB1(I),I=1,NCOL)
               ELSE
                  READ (RDWRTU,END=40,ERR=60,IOSTAT=MYIOS)
     +                 (RB1(I),I=1,NCOL)
               ENDIF
C
C Store source row in LP1
C
               IF (SEMROW(2,RB1,INFORM,J,K,LP1)) GOTO 30
            ENDIF
   10    CONTINUE
   20 CONTINUE
C
C If READ, reset WSTAT to prevent range from being deleted on return
C to the command interpreter
C
      IF (.NOT.LWRITE) WSTAT(LP1)=0
C
   30 RETURN
C
C End-of-file error - set flag
C
   40 EOF = .TRUE.
C
C General I/O error - set flag
C
   50 FRDWRU = .TRUE.
      GOTO 30
C
C I/O error with IOSTAT=
C
   60 IOS = MYIOS
      GOTO 50
C
C Copyright (C) 1987-1996: Synoptics Ltd,  All Rights Reserved
C
      END
