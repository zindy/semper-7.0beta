        PROGRAM HELPMAN
C Semper 6 Help Library Management Utility
C
C Provides facilities for managing a library of help file texts,
C allowing addition/replacement/deletion/extraction of modules,
C entry list generation, library compression and status reporting
C
C File structure is as follows:
C
C [idblock] := S e m p e r . h e l p version release
C              [filesize*3] [directorysize*2] (reserved to eob)
C
C [key] := keycharcount keychar1 keychar2..
C [keylist] := [key] or [keylist] [key]
C [entrylength] := number of bytes occupied by this entry,
C                  excluding the length itself
C [entry] := [entrylength] [keylist] [textblockpointer*3]
C         or [entrylength] 0 undefined
C [entrylist] := [entry] or [entrylist] [entry]
C [directory] := [freeblockpointer*3] [entrylist] 0
C
C [line] := linecharcount linechar1 linechar2..
C [linelist] := [line] or [linelist] [line]
C [file] := [numberofblocks*3] [linelist] 255
C [filelist] := null or [filelist] [file]
C
C [library] := [idblock] [directory] [filelist]
C
C example (from block 2):
C   0,  0, 10  free blocks pointer
C  13,  3,  O,  N,  E,  0,  4,  F,  O,  U,  R,  0,  0,  4  active entry
C  10,  0,  D,  I,  S,  P,  L,  0,  0,  0,  8  inactive entry
C   0  terminator
C
C   0,  0,  4  file 1 - block count used only by COMPRESS, STATUS
C  14,  T,  h,  i,  s,   ,  i,  s,   ,  l,  i,  n,  e,   ,  1
C   1,  $ - marks end of file unless in FULL mode
C   8,  L,  i,  n,  e,   ,  t,  w,  o
C  255
C   0,  0,  2  file 2
C   8,  F,  i,  l,  e,   ,  t,  w,  o
C  255
C
C Data are stored as bytes, organised in fixed length blocks numbered
C from 1; data are spanned as necessary except that the directory and
C all files are block-aligned
C
C Length to which $ control lines can be continued, and length of
C buffers used for alphabetic sorting - code gives up gracefully if
C it runs out, rather than crashing.
C
      INTEGER LNCOMM,LNSORT
      PARAMETER (LNCOMM=256,LNSORT=1024)
C
C Packed names
C
      INTEGER NTYP,NINI,NADD,NDEL,NLOG,NCOM,NEND,NSTO,NSTA,NRUN,NFULL
      PARAMETER (NTYP=-1017,NINI=14969,NADD=1764,NDEL=6612,NLOG=19807)
      PARAMETER (NCOM=5413,NEND=8564,NSTO=31215,NSTA=31201,NRUN=29654)
      PARAMETER (NFULL=10452)
C
      INTEGER*4 I4BLK1,I4BLK2
      PARAMETER (I4BLK1=256*256, I4BLK2=256)
C
      INTEGER NVERBS
      PARAMETER (NVERBS=10)
C
      LOGICAL HELPM2,HELPM3,HELPM4,HELPM5,HELPM6,HELPM7,HELPM8,HELPM9
      LOGICAL SX11IN
      LOGICAL SEMXA1,HELPUO,ABANDN,DISC,SEMCON,SEMDIA,READLN
      LOGICAL EQINIT,EQSETS,EQTERM,SEMTCR,SEMTLF,SEMTFL
      LOGICAL FOUND,FULL,PASS1,TERMIN,MASTER,MATCHM,EOF
      REAL V,X,ACTDIR,DELDIR,ACTFIL,DELFIL,FILKB,DIRKB
      INTEGER*4 SORTB(LNSORT),I40,I43,I4N,I4N1,LASTFB
      INTEGER*4 IBLK,BBLK,BLK,ABLK,CBLK,DBLK,EBLK,FBLK,OBLK,TBLK
      PARAMETER (I40=0,I43=3)
      INTEGER COMM(LNCOMM),TEXT(81),KEY(60),FULLMS(15)
      INTEGER DNAME(60),NAME(61)
      INTEGER SORTI(LNSORT),VERBS(NVERBS)
C
      INTEGER LNBLNK
C
      INTEGER*4 ZERO,ONE
      PARAMETER (ZERO=0,ONE=1)
C
      INTEGER LOGFLE
      PARAMETER (LOGFLE=7)
C
      INTEGER AITEM,CH,CITEM,COMLEN
      INTEGER DEVICE,DITEM,EITEM,ECOUNT
      INTEGER FIRST,OPCODE,PTR,OPTR,I,ICH,IITEM,ISORT,ITEM,IV
      INTEGER KEYLEN,KPTR,L,LAST,LASTDB,N,N1,N2,N3,NITEM,NSORT
C
      CHARACTER*60 SFNAME
      CHARACTER*3 HPROMP
C
      INCLUDE 'COMMON'
      INCLUDE 'EVENTS'
C
      INTEGER BUFF2(LNBLK),BUFF3(LNBLK)
C
      EQUIVALENCE (SMGR2,BBLK),(SMGR3,BLK),(SMGI5,ITEM)
      EQUIVALENCE (SMGR1,IBLK),(SMGI8,IITEM),(SMGI6,DEVICE)
C
C ****** CHANGE ******
C DNAME should be initialised to contain the (internal code) chars
C of a locally chosen default library file name, with an initial
C char count
C
C semper.hlb
C
      DATA DNAME/10,KLCS,KLCE,KLCM,KLCP,KLCE,KLCR,KDOT,
     +           KLCH,KLCL,KLCB,49*0/
C
C ****** ****** ******
C
C Packed names TYPE,INIT,ADD,DELETE,LOG,COMPRESS,END,STOP,STATUS
C
      DATA VERBS/NTYP,NINI,NADD,NDEL,NLOG,NCOM,NEND,NSTO,NSTA,NRUN/
C
      DATA FULLMS/KUCF,KLCU,KLCL,KLCL,KSPACE,KUCE,KLCN,KLCT,KLCR,
     +   KLCY,KSPACE,KUCL,KLCI,KLCS,KLCT/
C
C ****** CHANGE ******
C Initialise CBNUM, CBSIZE, TERWID and TERLEN as in SEMINI
      CBSIZE=128
      CBNUM=8
      TERWID=79
      TERLEN=24
C
      OPEN (LOGFLE,FILE='help.log',STATUS='UNKNOWN')
C
C ****** ****** ******
      IF (SX11IN(.FALSE.)) STOP
C
C Initialise common
C
      DEVICE=1
      MEDN(DEVICE)=0
      BBLK=0
      INPUT=TERM1
      MONIT=.FALSE.
      TERCNT=0
      TERCR=.TRUE.
      TERLF=.TRUE.
      TERXCR = .FALSE.
      TERXLF = .FALSE.
C
C Initialise keyboard buffers
C
      KBUFN = 0
      KBUFP = 0
C
      ERROR = 0
C
C Initialise events
C
      IF (EQINIT(0)) THEN
         IF (SEMDIA('Events initialisation failed',NDIFAT)) GOTO 870
         GOTO 870
      ENDIF
C
      IF (EQSETS(MBREAK,QRUN)) THEN
         IF (SEMDIA('BREAK handler failed',NDIFAT)) GOTO 870
         GOTO 870
      ENDIF
C
C Announce yourself
C
      IF (SEMCON('Semper 6 Help Library Manager')) GOTO 870
      IF (SEMCON('-----------------------------')) GOTO 870
C
C Type default library file name and accept alternative
C
      RECORD(1:22)='Default Library file: '
      CALL SEMCHS(RECORD(23:),DNAME(2),DNAME(1))
      IF (SEMCON(RECORD)) GOTO 870
C
      NAME(1)=60
      IF (READLN(NAME(2),NAME(1),
     +           'Enter alternative file or RETURN: ',ERROR)) GOTO 870
C
      IF (NAME(1).EQ.0) THEN
         DO 10 I=1,DNAME(1)+1
            NAME(I)=DNAME(I)
   10    CONTINUE
      ENDIF
C
C Initialise cache buffer management
C
       IF (DISC(0,0,I40,BUFF3,I40,0,0)) GOTO 850
C
C Assign library file (read/write)
C
      IF (HELPM9(.FALSE.,NAME,FILKB,DIRKB)) THEN
         IF (SEMDIA('Unable to open/initialise library: INIT needed?',
     +      NDIERR)) GOTO 870
      ENDIF
      LASTFB=FLSIZ(DEVICE)
      LASTDB=DRSIZ(DEVICE)
C
C Solicit next command
C --------------------
C
   20 FIRST=1
      ERROR=0
C
C Generate prompt
C
      HPROMP = 'H$ '
      GOTO 40
C
   30 CONTINUE
      HPROMP = 'H$+'
C
   40 COMLEN=MIN0(FIRST+79,LNCOMM)
C
C Read (next part of) command line
C
      IF (INPUT.EQ.TERM1) THEN
         N=COMLEN-FIRST+1
         IF (READLN(COMM(FIRST),N,HPROMP,ERROR)) GOTO 870
         COMLEN=FIRST+N-1
      ELSE
         READ (INPUT,60,ERR=180,END=180) (COMM(I),I=FIRST,COMLEN)
         CALL A1CONV(COMM(FIRST),COMLEN-FIRST+1)
   50    IF (COMLEN.GE.FIRST) THEN
            IF (COMM(COMLEN).EQ.KSPACE) THEN
               COMLEN=COMLEN-1
               GOTO 50
            ENDIF
         ENDIF
      ENDIF
   60 FORMAT (80A1)
C
C If blank line, go back for next command line
C
   70 IF (COMLEN.LT.1) GOTO 20
C
C If it ends with +, go back for more
C
      IF (COMM(COMLEN).EQ.KPLUS) THEN
         FIRST=COMLEN
         GOTO 30
      ENDIF
C
      IF (ABANDN(ERROR)) GOTO 890
C
      TERCNT=0
C
C Extract verb name from command
C
      PTR=1
      IF (COMM(1).EQ.KDOLLA) PTR=2
      IF (SEMXA1(1,COMM,COMLEN,PTR,V,VERB)) GOTO 20
C
C Known?
C
      IF (VERB.EQ.0) GOTO 90
      DO 80 OPCODE=1,NVERBS
      IF (VERB.EQ.VERBS(OPCODE)) GOTO 100
   80 CONTINUE
C
C No: grumble and try again
C
   90 IF (SEMCON(' ')) GOTO 870
      IF (SEMCON('The following commands are recognised:')) GOTO 870
      IF (SEMCON(
     +   '$TYPE name     type named entry ($TYPE ? for full list)'
     +   )) GOTO 870
      IF (SEMCON('$TYPE FULL ..  type in full(er) form')) GOTO 870
      IF (SEMCON('$STOP or $END  exit')) GOTO 870
      IF (SEMCON(' ')) GOTO 870
      IF (SEMCON('$ADD name [alias] [alias]..  add/replace named entry'
     +   )) GOTO 870
      IF (SEMCON('               from subsequent lines (until $..)'
     +   )) GOTO 870
      IF (SEMCON(
     +   '$LOG name      print named entry in log file, prefaced by'
     +   )) GOTO 870
      IF (SEMCON('               $ADD command; * logs all entries'
     +   )) GOTO 870
      IF (SEMCON('$DELETE name   delete named entry')) GOTO 870
      IF (SEMCON('$COMPRESS      compress library')) GOTO 870
      IF (SEMCON(
     +   '$INIT [f] [d]  create/initialise library file, total size'
     +   )) GOTO 870
      IF (SEMCON('               f KB [1000], directory size d KB [25]'
     +   )) GOTO 870
      IF (SEMCON('$STATUS        report directory/file space used/free'
     +   )) GOTO 870
      IF (SEMCON(
     +   '$RUN filename  execute instructions in file (until $END)'
     +   )) GOTO 870
      GOTO 20
C
C Switch code according to verb
C
  100 GOTO (470,200,230,670,390,690,180,180,790,110), OPCODE
C
C Run commands from subsidiary file
C ---------------------------------
C
  110 IF (HELPM5(COMM,COMLEN,PTR,KEY,KEYLEN)) GOTO 20
      IF (KEYLEN.EQ.0) GOTO 20
      SFNAME=' '
      DO 120 I=1,KEYLEN
C
         N = KEY(I)
C
C Force to lower case
C
         IF (N.GE.KUCA.AND.N.LE.KUCZ) N = N + (KLCA-KUCA)
         SFNAME(I:I)=CHAR(N)
  120 CONTINUE
C
C ****** CHANGE ******
C Adjust/extend parameters if necessary locally - only read access is
C needed, and this should be specified if possible to allow
C access to as wide a range of system files as possible
      I=1
  140 N=INDEX(SFNAME(I:),'/')
      IF (N .NE. 0) THEN
          I = I + N
          GOTO 140
      ENDIF
      IF (INDEX(SFNAME(I:),'.') .EQ. 0)
     +    SFNAME=SFNAME(1:LNBLNK(SFNAME))//'.shl'
      OPEN (RUNFLE,FILE=SFNAME(1:LNBLNK(SFNAME)),ERR=160,
     +      STATUS='OLD')
      REWIND (RUNFLE,ERR=170)
C
C ****** ****** ******
C
      INPUT=RUNFLE
      GOTO 20
  160 IF (SEMDIA('Unable to open file',NDIERR)) GOTO 870
      GOTO 20
  170 IF (SEMDIA('Unable to rewind input file',NDIERR)) GOTO 870
      CLOSE(RUNFLE)
      GOTO 20
C
C Quit (or eof on input)
C ----
C If subsidiary file, close and revert to terminal
C
  180 IF (INPUT.NE.TERM1) THEN
         CLOSE(RUNFLE)
         INPUT=TERM1
         GOTO 20
      ENDIF
C
C Final shut down
C
      WRITE (LOGFLE,190)
  190 FORMAT ('$end')
      GOTO 860
C
C Create/initialise library
C -------------------------
C Check whether already created
C
  200 IF (MEDN(DEVICE).EQ.0) GOTO 210
      IF (SEMCON('Library already established')) GOTO 870
      GOTO 20
C
C Read file size and directory size; defaults 1000KB, 25KB
C
  210 IF (SEMXA1(2,COMM,COMLEN,PTR,FILKB,IV)) FILKB=1000.
      IF (SEMXA1(2,COMM,COMLEN,PTR,DIRKB,IV)) DIRKB=25.
C
C Create it, assign it and initialise header
C
      IF (HELPM9(.TRUE.,NAME,FILKB,DIRKB)) GOTO 910
      LASTFB=FLSIZ(DEVICE)
      LASTDB=DRSIZ(DEVICE)
C
C Output empty directory
C
      BLK=2
      ITEM=1
C
C Write free block pointer to library
C
      I4N=LASTDB+1
      IF (HELPM4(2,I4N)) GOTO 850
C
C Zero remainder of block (to prevent overflow on fixing)
C
      DO 220 I=4,LNBLK
         IF (HELPM2(2,0)) GOTO 850
  220 CONTINUE
C
C Flush buffer
C
      IF (HELPM2(3,0)) GOTO 850
C
C Verify completion
C
      IF (SEMCON('Library created, initialised and empty')) GOTO 870
      GOTO 20
C
C Add new entry
C -------------
C Extract master key, forcing to lower case
C
  230 KPTR=PTR
      IF (HELPM5(COMM,COMLEN,PTR,KEY,KEYLEN)) GOTO 20
C
C Note last directory block and free block pointer
C
  240 BLK=2
      ITEM=1
      IF (HELPM4(1,FBLK)) GOTO 850
C
C Search directory for master key
C
      DBLK=0
C
C Note entry start, checking for terminator
C
  250 ABLK=BLK
      AITEM=ITEM
      IF (HELPM2(1,ECOUNT)) GOTO 850
      EBLK=BLK
      EITEM=ITEM
      IF (ECOUNT.EQ.0) GOTO 280
C
C Entry active?
C
      IF (HELPM2(1,N)) GOTO 850
      IF (N.EQ.0) GOTO 270
C
C Yes: compare it with KEY
C
      IF (N.EQ.KEYLEN) THEN
         DO 260 I=1,KEYLEN
            IF (HELPM2(1,CH)) GOTO 850
            IF (KEY(I).NE.CH) GOTO 270
  260    CONTINUE
C
C Entry found: if LOG, return to appropriate code
C
         IF (VERB.EQ.NLOG) GOTO 410
C
C Note entry for later deletion
C
         DBLK=EBLK
         DITEM=EITEM
      ENDIF
C
C Continue scanning so as to find end of directory
C
C Advance to next entry
C
  270 BLK=EBLK
      ITEM=EITEM+ECOUNT
      GOTO 250
C
C End of directory
C
  280 IF (VERB.EQ.NDEL) GOTO 680
      IF (VERB.EQ.NLOG) GOTO 460
C
C Extend directory with new entry, reducing HELPM2 access limit
C while doing so
C
      FLSIZ(DEVICE)=LASTDB
      ECOUNT=0
      PTR=KPTR
C
C Copy next key chars
C
  290 IF (SEMXA1(0,COMM,COMLEN,PTR,V,IV)) GOTO 310
C
C Note pointers to keychar count for later insertion
C
      CBLK=BLK
      CITEM=ITEM
C
C Step over the count itself
C
      ITEM=ITEM+1
      N=0
  300 IF (PTR.LE.COMLEN) THEN
         CH = COMM(PTR)
         IF (CH.NE.KSPACE) THEN
            IF (CH.GE.KUCA.AND.CH.LE.KUCZ) CH = CH + (KLCA-KUCA)
            IF (N.NE.60) THEN
               N=N+1
               IF (HELPM2(2,CH)) GOTO 370
               ECOUNT=ECOUNT+1
            ENDIF
            PTR=PTR+1
            GOTO 300
         ENDIF
      ENDIF
C
C Insert keychar count before key
C
      I4N=BLK
      N2=ITEM
      BLK=CBLK
      ITEM=CITEM
      IF (HELPM2(2,N)) GOTO 370
      ECOUNT=ECOUNT+1
      BLK=I4N
      ITEM=N2
      GOTO 290
C
C Terminate list with zero and block pointer to text
C
  310 IF (HELPM2(2,0)) GOTO 370
      IF (HELPM4(2,FBLK)) GOTO 370
C
C Terminate directory with zero
C
      IF (HELPM2(2,0)) GOTO 370
C
C Restore normal HELPM2 access limit
C
      FLSIZ(DEVICE)=LASTFB
C
C Copy new text to free space at end of file
C
      BLK=FBLK
      TBLK=BLK
      ITEM=4
      EOF=.FALSE.
C
C Read line from keyboard/help file
C
  320 CONTINUE
      IF (INPUT.EQ.TERM1) THEN
         LAST=80
         IF (READLN(COMM,LAST,'.. ',ERROR)) GOTO 870
      ELSE
         READ (INPUT,60,ERR=350,END=350) (COMM(I),I=1,80)
         CALL A1CONV(COMM,80)
         LAST=80
  330    IF (LAST.GE.1) THEN
            IF (COMM(LAST).EQ.KSPACE) THEN
               LAST=LAST-1
               GOTO 330
            ENDIF
         ENDIF
      ENDIF
C
C Does it begin with $ ?
C
      IF (LAST.GT.1) THEN
         IF (COMM(1).EQ.KDOLLA) GOTO 360
      ENDIF
C
C Deposit length
C
      IF (HELPM2(2,LAST)) GOTO 380
C
C Deposit line chars
C
      DO 340 I=1,LAST
         IF (HELPM2(2,COMM(I))) GOTO 380
  340 CONTINUE
      GOTO 320
C
C New text exhausted: deposit terminator
C
  350 EOF=.TRUE.
  360 IF (HELPM2(2,255)) GOTO 380
C
C Record number of blocks in file text
C
      I4N=BLK-FBLK+1
      FBLK=BLK+1
      BLK=TBLK
      ITEM=1
      IF (HELPM4(2,I4N)) GOTO 380
C
C Record updated free block pointer
C
      BLK=2
      ITEM=1
      IF (HELPM4(2,FBLK)) GOTO 380
C
C All clear: delete any old entry and activate new
C
      IF (DBLK.NE.0) THEN
         BLK=DBLK
         ITEM=DITEM
         IF (HELPM2(2,0)) GOTO 850
      ENDIF
      BLK=ABLK
      ITEM=AITEM
      IF (ECOUNT+4.GE.256) GOTO 920
      IF (HELPM2(2,ECOUNT+4)) GOTO 380
C
C Flush buffer
C
      IF (HELPM2(3,0)) GOTO 850
C
C Verify completion
C
      IF (DBLK.EQ.0) THEN
         RECORD(1:13)='Entry added: '
         CALL SEMCHS(RECORD(14:),KEY,KEYLEN)
      ELSE
         RECORD(1:16)='Entry replaced: '
         CALL SEMCHS(RECORD(17:),KEY,KEYLEN)
      ENDIF
      IF (SEMCON(RECORD)) GOTO 870
C
C Break back into command processing code
C
      IF (EOF) GOTO 180
      FIRST=1
      COMLEN=LAST
      GOTO 70
C
C HELPM2 error returns
C
  370 IF (ERROR.NE.50) GOTO 850
      IF (SEMDIA('Library directory space full: $COMPRESS may help',
     +   NDIERR)) GOTO 870
      GOTO 860
  380 IF (ERROR.NE.50) GOTO 850
      IF (SEMDIA('Library file space full: $COMPRESS may help',
     +   NDIERR)) GOTO 870
      GOTO 860
C
C Print entry in log
C ------------------
C Get next entry name
C
  390 IF (HELPM5(COMM,COMLEN,PTR,KEY,KEYLEN)) GOTO 20
C
C Unless *, search directory via ADD code
C
      FULL=.FALSE.
      IF (KEYLEN.NE.1 .OR. KEY(1).NE.KSTAR) GOTO 240
      FULL=.TRUE.
C
C Otherwise, ask for a sorted list of all master keys
C
      IF (HELPM7(SORTB,SORTI,LNSORT,NSORT)) GOTO 880
      IF (NSORT.LT.0) THEN
         IF (SEMDIA('Too many entries to sort alphabetically',NDIMES))
     +      GOTO 870
      ENDIF
C
C Code here passes through all entries, logging text
C
      BLK=2
      ITEM=4
      ISORT=1
C
C Get next entry, switching code acc to whether sorted
C
  400 IF (NSORT.GE.0) THEN
C
C Sorted: refer to pointer list
C
         IF (ISORT.GT.NSORT) GOTO 390
         EBLK=SORTB(ISORT)
         EITEM=SORTI(ISORT)
         ISORT=ISORT+1
      ELSE
C
C Unsorted: refer to entry directly
C Note entry start, checking for terminator
C
         IF (HELPM2(1,ECOUNT)) GOTO 850
         EBLK=BLK
         EITEM=ITEM
         IF (ECOUNT.EQ.0) GOTO 460
C
C Entry active?
C
         IF (HELPM2(1,N)) GOTO 850
         IF (N.EQ.0) GOTO 450
      ENDIF
C
C Log entry at EBLK, EITEM
C
  410 TEXT(1)=KDOLLA
      TEXT(2)=KLCA
      TEXT(3)=KLCD
      TEXT(4)=KLCD
      TEXT(5)=KSPACE
      LAST=5
      MASTER=.TRUE.
      BLK=EBLK
      ITEM=EITEM
C
C Transcribe key list prefaced by $ADD command
C
  420 IF (HELPM2(1,N)) GOTO 850
      IF (N.EQ.0) GOTO 440
      IF (LAST+N+2.GT.80) THEN
C
C Flush output line buffer
C
         LAST=LAST+1
         TEXT(LAST)=KPLUS
         IF (HELPUO(TEXT,LAST,LOGFLE,ICH)) CONTINUE
         LAST=0
      ENDIF
C
C Copy key chars, inserting space unless master
C
      IF (.NOT.MASTER) THEN
         LAST=LAST+1
         TEXT(LAST)=KSPACE
      ENDIF
      DO 430 I=1,N
         IF (HELPM2(1,CH)) GOTO 850
         LAST=LAST+1
         TEXT(LAST)=CH
         IF (MASTER) KEY(I)=CH
  430 CONTINUE
      IF (MASTER) KEYLEN=N
      MASTER=.FALSE.
      GOTO 420
C
C Print text
C
  440 IF (LAST.NE.0) THEN
         IF (HELPUO(TEXT,LAST,LOGFLE,ICH)) CONTINUE
      ENDIF
      IF (HELPM6(TEXT,LOGFLE,.FALSE.,.FALSE.)) GOTO 880
      RECORD(1:14)='Entry logged: '
      CALL SEMCHS(RECORD(15:),KEY,KEYLEN)
      IF (SEMCON(RECORD)) GOTO 870
C
C If * mode, advance to next entry; otherwise quit
C
  450 IF (.NOT.FULL) GOTO 390
      BLK=EBLK
      ITEM=EITEM+ECOUNT
      GOTO 400
C
C Directory terminator reached: complain unless * mode
C
  460 IF (.NOT.FULL) THEN
         RECORD(1:17)='Entry not found: '
         CALL SEMCHS(RECORD(18:),KEY,KEYLEN)
         IF (SEMCON(RECORD)) GOTO 870
      ENDIF
      GOTO 390
C
C Type entry
C ----------
C FULL mode?
C
  470 I=PTR
      IF (SEMXA1(1,COMM,COMLEN,PTR,V,VERB)) GOTO 20
      FULL=.TRUE.
      IF (VERB.EQ.NFULL) GOTO 480
      FULL=.FALSE.
      PTR=I
C
C Extract required key, forcing to lower case
C
  480 IF (HELPM5(COMM,COMLEN,PTR,KEY,KEYLEN)) GOTO 20
C
C Entry list?
C
      IF (KEYLEN.EQ.1) THEN
         IF (KEY(1).EQ.KQUEST) GOTO 580
      ENDIF
C
C Search directory for key then alias
C
      FOUND=.FALSE.
      PASS1=.TRUE.
  490 BLK=2
      ITEM=4
C
C Note entry start, checking terminator
C
  500 IF (HELPM2(1,ECOUNT)) GOTO 850
      EBLK=BLK
      EITEM=ITEM
      IF (ECOUNT.EQ.0) GOTO 570
C
C No: flag master key
C
      MASTER=.TRUE.
      MATCHM=.FALSE.
C
C Entry active / another key present?
C
  510 IF (HELPM2(1,N)) GOTO 850
      IF (N.EQ.0) GOTO 560
C
C Yes: note start of key
C
      CBLK=BLK
      CITEM=ITEM
C
C Match if key begins with KEY
C
      IF (N.LT.KEYLEN) GOTO 550
      DO 520 I=1,KEYLEN
         IF (HELPM2(1,CH)) GOTO 850
         IF (CH.NE.KEY(I)) GOTO 550
  520 CONTINUE
C
C Match found: ignore on pass 2 if master or matching master
C
      FOUND=.TRUE.
      IF (.NOT.PASS1) THEN
         IF (MASTER) MATCHM=.TRUE.
         IF (MATCHM) GOTO 550
      ENDIF
C
C Insert master key in header line
C
      BLK=EBLK
      ITEM=EITEM
      IF (HELPM2(1,N1)) GOTO 850
      DO 530 I=1,N1
      IF (HELPM2(1,CH)) GOTO 850
C
C Force first letter to upper case
C
      IF (I.EQ.1) THEN
         IF (CH.GE.KLCA.AND.CH.LE.KLCZ) CH=CH-32
      ENDIF
  530 TEXT(I)=CH
C
C If alias, insert in header too
C
      IF (.NOT.MASTER) THEN
         TEXT(N1+1)=KSPACE
         TEXT(N1+2)=KEQUAL
         TEXT(N1+3)=KSPACE
         N1=N1+3
         BLK=CBLK
         ITEM=CITEM
         DO 540 I=1,N
            N1=N1+1
            IF (HELPM2(1,TEXT(N1))) GOTO 850
  540    CONTINUE
      ENDIF
C
C Print header line
C
      IF (HELPM3(TEXT,N1,TERWID,ICH)) GOTO 890
      IF (ICH.EQ.KUCN.OR.ICH.EQ.KLCN) GOTO 560
C
C Locate text and print it
C
      BLK=EBLK
      ITEM=EITEM+ECOUNT-3
      IF (HELPM6(TEXT,TERM2,.TRUE.,FULL)) GOTO 880
      GOTO 560
C
C Advance to next key
C
  550 IF (PASS1) GOTO 560
      BLK=CBLK
      ITEM=CITEM+N
      MASTER=.FALSE.
      GOTO 510
C
C Advance to next entry
C
  560 BLK=EBLK
      ITEM=EITEM+ECOUNT
      GOTO 500
C
C End of directory: second pass for aliases?
C
  570 IF (PASS1) THEN
         PASS1=.FALSE.
         GOTO 490
      ENDIF
      IF (.NOT.FOUND) THEN
         RECORD(1:17)='Entry not found: '
         CALL SEMCHS(RECORD(18:),KEY,KEYLEN)
         IF (SEMCON(RECORD)) GOTO 870
      ENDIF
      GOTO 480
C
C List active entries (with aliases)
C ----------------------------------
C
  580 IF (FULL) THEN
         IF (HELPM3(FULLMS,15,TERWID,ICH)) GOTO 890
      ELSE
         IF (HELPM3(FULLMS(6),10,TERWID,ICH)) GOTO 890
      ENDIF
      IF (ICH.EQ.KUCN.OR.ICH.EQ.KLCN) GOTO 480
C
C Ask for a sorted list of all master keys
C
      IF (HELPM7(SORTB,SORTI,LNSORT,NSORT)) GOTO 880
      IF (NSORT.LT.0) THEN
         IF (SEMDIA('Too many entries to sort alphabetically',NDIMES))
     +      GOTO 870
      ENDIF
C
C Scan directory, printing names
C
      BLK=2
      ITEM=4
      ISORT=1
      OPTR=0
      TERMIN=.FALSE.
C
C Get next entry, switching code acc to whether sorted
C
  590 IF (ABANDN(ERROR)) GOTO 890
      IF (NSORT.GE.0) THEN
C
C Sorted: refer to pointer list
C
         IF (ISORT.GT.NSORT) GOTO 650
         BLK=SORTB(ISORT)
         ITEM=SORTI(ISORT)
         ISORT=ISORT+1
      ELSE
C
C Unsorted: refer to entry directly
C Note entry start, checking terminator
C
         IF (HELPM2(1,ECOUNT)) GOTO 850
         EBLK=BLK
         EITEM=ITEM
         IF (ECOUNT.EQ.0) GOTO 650
      ENDIF
C
C Get master key, provided entry active
C
      MASTER=.TRUE.
  600 IF (HELPM2(1,N)) GOTO 850
      IF (N.EQ.0) GOTO 640
C
C Force new output line on each master key if FULL
C
      IF (.NOT.(FULL.OR.MASTER)) GOTO 610
      IF (OPTR.NE.0) GOTO 660
C
C Prefill line buffer with spaces if necessary
C
  610 IF (OPTR.EQ.0) THEN
         DO 620 I=1,TERWID
            TEXT(I)=KSPACE
  620    CONTINUE
C
C Indent additional lines of aliases
C
         IF (.NOT.MASTER) OPTR=8
      ENDIF
C
C Is there space for the key on the current line?
C
      N1=OPTR
C
C Allow for two spaces after each item
C
      IF (N1.NE.0) N1=N1+2
      N1=(N1+9)/10*10
      N2=N1+N
      IF (N2.GT.TERWID) GOTO 660
C
C It fits now, at least
C
      OPTR=N1
C
C Copy key chars
C
      DO 630 I=1,N
      OPTR=OPTR+1
      IF (HELPM2(1,CH)) GOTO 850
C
C Force to upper case if upper case
C
      IF (MASTER) THEN
         IF (I.EQ.1) THEN
            IF (CH.GE.KLCA.AND.CH.LE.KLCZ) CH=CH-32
         ENDIF
      ENDIF
  630 TEXT(OPTR)=CH
      MASTER=.FALSE.
      IF (FULL) GOTO 600
C
C Advance to next entry
C
  640 BLK=EBLK
      ITEM=EITEM+ECOUNT
      GOTO 590
C
C End of directory: flush line buffer and quit
C
  650 TERMIN=.TRUE.
C
C Map and print line
C
  660 IF (OPTR.NE.0) THEN
         IF (HELPUO(TEXT,OPTR,TERM2,ICH)) GOTO 890
         IF (ICH.EQ.KUCN.OR.ICH.EQ.KLCN) GOTO 480
      ENDIF
      OPTR=0
      IF (.NOT.TERMIN) GOTO 610
      GOTO 480
C
C Delete entry
C ------------
C Establish key required
C
  670 IF (HELPM5(COMM,COMLEN,PTR,KEY,KEYLEN)) GOTO 20
      GOTO 240
C
C Perform the deletion if found, and report the operation
C
  680 IF (DBLK.NE.0) THEN
         BLK=DBLK
         ITEM=DITEM
         IF (HELPM2(2,0)) GOTO 850
C
C Flush buffer
C
         IF (HELPM2(3,0)) GOTO 850
         RECORD(1:15)='Entry deleted: '
         CALL SEMCHS(RECORD(16:),KEY,KEYLEN)
      ELSE
C
         RECORD(1:17)='Entry not found: '
         CALL SEMCHS(RECORD(18:),KEY,KEYLEN)
      ENDIF
      IF (SEMCON(RECORD)) GOTO 870
      GOTO 670
C
C Compress library
C ----------------
C
  690 BLK=2
      ITEM=4
      OBLK=LASTDB+1
C
C Initialise serial i/p routine HELPM8, and skip first 3 bytes
C
      IBLK=1
      IITEM=LNBLK+1
      DO 700 I=1,3
         IF (HELPM8(BUFF2,N1)) GOTO 850
  700 CONTINUE
C
C Refuse abandon requests politely
C
  710 IF (ABANDN(ERROR)) THEN
         IF (SEMDIA('Sorry - cannot abandon',NDIMES)) GOTO 870
         ERROR=0
      ENDIF
C
C Next directory entry: terminator?
C
      IF (HELPM8(BUFF2,ECOUNT)) GOTO 850
      IF (ECOUNT.EQ.0) GOTO 780
C
C Is this entry active?
C
      IF (HELPM8(BUFF2,N1)) GOTO 850
      IF (N1.EQ.0) GOTO 760
C
C Yes: note start of current entry, but zero it pro tem
C
      EBLK=BLK
      EITEM=ITEM
      IF (HELPM2(2,0)) GOTO 850
C
C Copy entry as far as alias list terminator
C
      IF (HELPM2(2,N1)) GOTO 850
  720 IF (HELPM8(BUFF2,L)) GOTO 850
      IF (HELPM2(2,L)) GOTO 850
      IF (L.NE.0) GOTO 720
C
C Read file text pointer
C
      IF (HELPM8(BUFF2,N1)) GOTO 850
      IF (HELPM8(BUFF2,N2)) GOTO 850
      IF (HELPM8(BUFF2,N3)) GOTO 850
      I4N1=(N1)*I4BLK1+
     +     (N2)*I4BLK2+
     +     (N3)
C
C Write revised pointer
C
      IF (HELPM4(2,OBLK)) GOTO 850
C
C Pick up number of blocks in file
C
      IF (DISC(1,DEVICE,LNBLK4,BUFF3,I4N1,0,0)) GOTO 850
      CALL CFORM(BUFF3,BUFF3,0,1,I43)
      I4N=(BUFF3(1))*I4BLK1+
     +    (BUFF3(2))*I4BLK2+
     +    (BUFF3(3))
C
C Trivial copy?
C
      IF (I4N1.NE.OBLK) GOTO 730
      OBLK=OBLK+I4N
      GOTO 750
C
C Copy text blocks
C
  730 DO 740 I=1,I4N
         IF (DISC(1,DEVICE,LNBLK4,BUFF3,I4N1,0,0)) GOTO 850
         IF (DISC(2,DEVICE,LNBLK4,BUFF3,OBLK,0,0)) GOTO 850
         I4N1=I4N1+1
         OBLK=OBLK+1
  740 CONTINUE
C
C Copy OK: activate the entry
C
  750 I4N=BLK
      NITEM=ITEM
      BLK=EBLK
      ITEM=EITEM
      IF (HELPM2(2,ECOUNT)) GOTO 850
      IF (HELPM2(3,0)) GOTO 850
      BLK=I4N
      ITEM=NITEM
      GOTO 710
C
C Entry inactive; skip it
C
  760 N=ECOUNT-1
      DO 770 I=1,N
         IF (HELPM8(BUFF2,N1)) GOTO 850
  770 CONTINUE
      GOTO 710
C
C Terminator
C
  780 IF (HELPM2(2,0)) GOTO 850
C
C Insert new free block pointer
C
      BLK=2
      ITEM=1
      IF (HELPM4(2,OBLK)) GOTO 850
C
C Flush buffer
C
      IF (HELPM2(3,0)) GOTO 850
C
C Verify completion
C
      IF (SEMCON('Library compressed')) GOTO 870
C
C Report status
C -------------
C
  790 N1=0
      N2=0
      ACTDIR=0.
      DELDIR=0.
      ACTFIL=0.
      DELFIL=0.
      BLK=2
      ITEM=4
C
C Note entry start, checking terminator
C
  800 IF (HELPM2(1,ECOUNT)) GOTO 850
      EBLK=BLK
      EITEM=ITEM
      IF (ECOUNT.EQ.0) GOTO 810
C
C Note number of blocks in file
C
      BLK=EBLK
      ITEM=EITEM+ECOUNT-3
      IF (HELPM4(1,BLK)) GOTO 850
      ITEM=1
      IF (HELPM4(1,I4N)) GOTO 850
C
C Entry active?
C
      BLK=EBLK
      ITEM=EITEM
      IF (HELPM2(1,N)) GOTO 850
      IF (N.NE.0) THEN
C
C Yes
C
         N1=N1+1
         ACTDIR=ACTDIR+ECOUNT+1
         ACTFIL=ACTFIL+I4N
      ELSE
C
C No
C
         N2=N2+1
         DELDIR=DELDIR+ECOUNT+1
         DELFIL=DELFIL+I4N
      ENDIF
C
C Advance to next entry
C
      BLK=EBLK
      ITEM=EITEM+ECOUNT
      GOTO 800
C
C End of directory
C
  810 V=LASTDB
      X=(V*LNBLK-7.)/1024.
      ACTDIR=ACTDIR/1024.
      DELDIR=DELDIR/1024.
      V=X-ACTDIR-DELDIR
      IF (SEMCON('                      Active   Deleted      Free'))
     +   GOTO 870
      WRITE (RECORD,820) N1,N2
  820 FORMAT ('Number of entries',2I10)
      IF (SEMCON(RECORD)) GOTO 870
      WRITE (RECORD,830) ACTDIR,DELDIR,V
  830 FORMAT ('Directory space  ',3(F8.1,'kb'))
      IF (SEMCON(RECORD)) GOTO 870
      V=REAL(LNBLK)/1024.
      ACTFIL=ACTFIL*V
      DELFIL=DELFIL*V
      V=LASTFB*V
      V=V-X-ACTFIL-DELFIL
      WRITE (RECORD,840) ACTFIL,DELFIL,V
  840 FORMAT ('File space       ',3(F8.1,'kb'))
      IF (SEMCON(RECORD)) GOTO 870
      IF (SEMCON(' ')) GOTO 870
      GOTO 20
C
C Errors
C ------
C
  850 IF (SEMDIA('Input/output error on library file',NDIERR)) GOTO 870
C
C Attempt flush
C
  860 IF (HELPM2(3,0)) CONTINUE
C
C Shut down events
C
      IF (EQSETS(MBREAK,QCLOSE)) GOTO 870
      IF (EQTERM(1)) GOTO 870
C
C Force start-of-line
C
      IF (TERXCR) THEN
         IF (SEMTCR()) GOTO 870
      ENDIF
C
      IF (TERXLF) THEN
         IF (SEMTLF()) GOTO 870
      ENDIF
C
C Flush out all output to terminal
C
      IF (SEMTFL()) GOTO 870
C
  871 CONTINUE
      CALL SX11EX
C     Use pause, safer and more useful
C      PAUSE
C      CALL WAITS(1.0)
      CALL EXIT(ZERO)
C
  880 IF (ERROR.EQ.8) GOTO 850
  890 IF (ERROR.NE.0) THEN
         IF (SEMDIA('Abandoned',NDIERR)) GOTO 870
      ENDIF
  900 IF (INPUT.NE.TERM1) THEN
         CLOSE(RUNFLE)
         INPUT=TERM1
      ENDIF
      GOTO 20
  910 IF (SEMDIA('Unable to create library',NDIFAT)) GOTO 870
      CALL SX11EX
      CALL WAITS(1.0)
      CALL EXIT(ONE)
  920 IF (SEMDIA('Too many characters in entry name / cross reference',
     +   NDIERR)) GOTO 870
      RECORD(1:17)='names for entry: '
      CALL SEMCHS(RECORD(18:),KEY,KEYLEN)
      IF (SEMDIA(RECORD,NDIERR)) GOTO 870
      GOTO 900
C
C Copyright (C) 1987-1993:  Synoptics Ltd,  All Rights Reserved
C
C     Short-circuit problem
870   write(6,*)'*** Unexpected Termination of Helpman ***'
      PAUSE
      GOTO 871
      END
C
C Semper 6 subsidiary module HELPM2
C
      LOGICAL FUNCTION HELPM2(OPCODE,VALUE)
C
C Provides buffered byte i/o to library file,
C using block/item numbers as position pointers, bumped
C by HELPM2 after each access
C
      LOGICAL DISC
      INTEGER BUFF1(256),OPCODE,VALUE,ITEM,WRTO,DEVICE
      INTEGER*4 BBLK,BLK,I40
      PARAMETER (I40=0)
C
      INCLUDE 'COMMON'
C
      EQUIVALENCE (RB1,BUFF1)
      EQUIVALENCE (SMGR2,BBLK),(SMGR3,BLK),(SMGI5,ITEM)
      EQUIVALENCE (SMGI6,DEVICE),(SMGI7,WRTO)
C
      HELPM2=.FALSE.
C
C Flush?
C
      IF (OPCODE.EQ.3) GOTO 20
C
C Normalise BLK,ITEM if necessary
C
   10 IF (ITEM.GT.LNBLK) THEN
         ITEM=ITEM-LNBLK
         BLK=BLK+1
         GOTO 10
      ENDIF
C
C Is required block already buffered?
C
      IF (BLK.EQ.BBLK) GOTO 40
C
C Dump present block if necessary..
C
   20 IF (BBLK.EQ.0 .OR. WRTO.EQ.0) GOTO 30
      IF (DISC(2,DEVICE,LNBLK4,BUFF1,BBLK,1,0)) GOTO 60
C
C .. and fetch new one
C
   30 BBLK=0
      IF (OPCODE.EQ.3) GOTO 50
      IF (DISC(1,DEVICE,LNBLK4,BUFF1,BLK,1,0)) GOTO 60
      BBLK=BLK
      WRTO=0
C
C Switch on opcode
C
   40 IF (OPCODE.EQ.2) THEN
C
C Deposit value
C
         BUFF1(ITEM)=VALUE
         WRTO=1
      ELSE
C
C Pick up value
C
         VALUE=BUFF1(ITEM)
      ENDIF
C
C Bump position
C
      ITEM=ITEM+1
      RETURN
C
C DISC flush
C
   50 IF (DISC(3,-1,I40,BUFF1,I40,0,0)) GOTO 60
      RETURN
C
C Disc error
C
   60 HELPM2=.TRUE.
C
      RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module HELPM3
C
      LOGICAL FUNCTION HELPM3(TEXT,N,WIDTH,ICH)
C
C Writes argument array embedded in dashes
C
      LOGICAL HELPUO
      INTEGER PTR,TEXT(*),N,WIDTH,HELWID,HELMAX,I,ICH
      PARAMETER (HELMAX=120)
      INTEGER LINE(HELMAX)
C
      INCLUDE 'PARAMS'
C
      HELWID = MIN(WIDTH,HELMAX) - 2
      IF (HELWID .LE. 0) HELWID = 72
      DO 10 I=1,HELWID
         LINE(I)=KMINUS
   10 CONTINUE
C
      IF (N.GT.0) THEN
         PTR=(HELWID-N)/2
         LINE(PTR)=KSPACE
         DO 20 I=1,N
            PTR=PTR+1
            LINE(PTR)=TEXT(I)
   20    CONTINUE
         LINE(PTR+1)=KSPACE
      ENDIF
C
C Blank lines were originally output before and after this line..
C
      HELPM3 = HELPUO(LINE,HELWID,TERM2,ICH)
C
      RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module HELPM4
C
      LOGICAL FUNCTION HELPM4(OPCODE,I4N)
C
C Reads/writes integers as 3-byte sequences via HELPM2
C
      INTEGER OPCODE
      INTEGER*4 I4N
C
      INCLUDE 'COMMON'
      LOGICAL HELPM2
C
      INTEGER*4 I4BLK1,I4BLK2
      PARAMETER (I4BLK1=256*256, I4BLK2=256)
C
      INTEGER N1,N2,N3
C
      HELPM4=.TRUE.
C
      IF (OPCODE.EQ.1) THEN
C
C Read value
C
         IF (HELPM2(1,N1)) GOTO 10
         IF (HELPM2(1,N2)) GOTO 10
         IF (HELPM2(1,N3)) GOTO 10
         I4N=(N1)*I4BLK1+
     +       (N2)*I4BLK2+
     +       (N3)
      ELSE
C
C Write value
C
         N1=I4N/I4BLK1
         N2=(I4N-(N1)*I4BLK1)/I4BLK2
         N3=I4N-(N1)*I4BLK1-(N2)*I4BLK2
         IF (HELPM2(2,N1)) GOTO 10
         IF (HELPM2(2,N2)) GOTO 10
         IF (HELPM2(2,N3)) GOTO 10
      ENDIF
C
      HELPM4=.FALSE.
C
   10 RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module HELPM5
C
      LOGICAL FUNCTION HELPM5(COMM,LENGTH,PTR,KEY,KEYLEN)
C
C Reads a key from the supplied line buffer, forcing to lower case
C
      LOGICAL SEMXA1
      INTEGER LENGTH
      INTEGER COMM(LENGTH),KEY(*),CH,PTR
C
      INCLUDE 'PARAMS'
C
      REAL V
      INTEGER IV,KEYLEN
C
      HELPM5=.FALSE.
      IF (SEMXA1(0,COMM,LENGTH,PTR,V,IV)) GOTO 20
C
      KEYLEN=0
   10 IF (PTR.GT.LENGTH) GOTO 30
      CH=COMM(PTR)
      IF (CH.EQ.KSPACE) GOTO 30
      IF (CH.GE.KUCA.AND.CH.LE.KUCZ) CH = CH + (KLCA-KUCA)
      IF (KEYLEN.LT.60) THEN
         KEYLEN=KEYLEN+1
         KEY(KEYLEN)=CH
      ENDIF
      PTR=PTR+1
      GOTO 10
C
   20 HELPM5=.TRUE.
C
   30 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module HELPM6
C
      LOGICAL FUNCTION HELPM6(TEXT,UNIT,TOTERM,FULL)
C
C Reads a block number at BLK,ITEM (equivalenced in COMMON) and prints
C the text beginning at that block on UNIT
C
      LOGICAL HELPM4,HELPM2,HELPUO,ABANDN,TOTERM,FULL
C
      INTEGER*4 BLK
      INTEGER LMMESS
      PARAMETER (LMMESS=22)
      INTEGER MMESS(LMMESS),TEXT(81),UNIT
      INTEGER I,ICH,ITEM,N
C
      INCLUDE 'COMMON'
C
      EQUIVALENCE (SMGR3,BLK),(SMGI5,ITEM)
C
      DATA MMESS/KSBRA,KLCM,KLCO,KLCR,KLCE,KSPACE,KLCV,KLCI,KLCA,KSPACE,
     +   KUCH,KUCE,KUCL,KUCP,KSPACE,KUCF,KUCU,2*KUCL,2*KDOT,KSKET/
C
      HELPM6=.TRUE.
C
C Read the block number
C
      IF (HELPM4(1,BLK)) GOTO 50
      ITEM=4
C
C Prepare CCC
C
   10 IF (ABANDN(ERROR)) GOTO 50
      IF (HELPM2(1,N)) GOTO 50
      IF (N.EQ.255) GOTO 40
C
C Copy line chars to local buffer
C
      DO 20 I=1,N
         IF (HELPM2(1,TEXT(I))) GOTO 50
   20 CONTINUE
C
C If terminal output, dollar separator means..
C
      IF (TOTERM.AND.TEXT(1).EQ.KDOLLA) THEN
C
C ..blank..
C
         IF (FULL) THEN
            N=0
C
C ..or output 'more via HELP FULL' message (paging) and quit
C
         ELSE
            DO 30 I=1,LMMESS
               TEXT(I)=MMESS(I)
   30       CONTINUE
            IF (HELPUO(TEXT,LMMESS,UNIT,ICH)) GOTO 50
            GOTO 40
         ENDIF
      ENDIF
C
C Otherwise, output TEXT (paging)
C
      IF (HELPUO(TEXT,N,UNIT,ICH)) GOTO 50
C
C Fetch next line, unless next requested
C
      IF (.NOT.(ICH.EQ.KUCN.OR.ICH.EQ.KLCN)) GOTO 10
C
C Normal exit
C
   40 HELPM6=.FALSE.
C
   50 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module HELPM7
C
      LOGICAL FUNCTION HELPM7(SORTB,SORTI,SORTL,NSORT)
C
C Prepares a list of entry pointers, sorted into alphabetical order
C by first keys
C
      LOGICAL HELPM2
      INTEGER SORTI(256),KEY(60)
      INTEGER CH,ECOUNT,EITEM,I,I2,ITEM,J,KEYLEN,N,N1,N2,NSORT,PTR,SORTL
      INTEGER*4 SORTB(256),BLK,EBLK,I4N
C
      INCLUDE 'COMMON'
C
      EQUIVALENCE (SMGR3,BLK),(SMGI5,ITEM)
C
      HELPM7=.FALSE.
      BLK=2
      ITEM=4
      NSORT=0
C
C Note entry start, checking terminator
C
   10 IF (HELPM2(1,ECOUNT)) GOTO 110
      EBLK=BLK
      EITEM=ITEM
      IF (ECOUNT.EQ.0) GOTO 30
C
C Entry active?
C
      IF (HELPM2(1,N)) GOTO 110
      IF (N.NE.0) THEN
C
C Yes: note it
C
         NSORT=NSORT+1
         IF (NSORT.GT.SORTL) GOTO 20
         SORTB(NSORT)=EBLK
         SORTI(NSORT)=EITEM
      ENDIF
C
C Advance to next entry
C
      BLK=EBLK
      ITEM=EITEM+ECOUNT
      GOTO 10
C
C Too many entries to sort
C
   20 NSORT=-1
      GOTO 120
C
C End of directory terminator found: Shell-sort list
C
   30 IF (NSORT.EQ.0) GOTO 120
      N2=0
   40 N2=N2+N2+1
      IF (N2.LT.NSORT) GOTO 40
   50 N2=N2/2
      IF (N2.EQ.0) GOTO 120
      N1=NSORT-N2
      DO 100  J=1,N1
      I=J
   60 I2=I+N2
C
C Compare the two keys: fetch key I to local store
C
      BLK=SORTB(I)
      ITEM=SORTI(I)
      IF (HELPM2(1,KEYLEN)) GOTO 110
      DO 70 PTR=1,KEYLEN
         IF (HELPM2(1,KEY(PTR))) GOTO 110
   70 CONTINUE
C
C Get length of key I2 and compare with key I
C
      BLK=SORTB(I2)
      ITEM=SORTI(I2)
      IF (HELPM2(1,ECOUNT)) GOTO 110
      N=MIN0(KEYLEN,ECOUNT)
C
C Compare key characters
C
      DO 80 PTR=1,N
         IF (HELPM2(1,CH)) GOTO 110
         IF (KEY(PTR)-CH)100,80,90
   80 CONTINUE
C
C Key chars present match: shorter sorts low
C
      IF (N.GE.KEYLEN) GOTO 100
C
C     IF (S1.LE.S2) GOTO 160
C Swap the key pointers
C
   90 I4N=SORTB(I)
      SORTB(I)=SORTB(I2)
      SORTB(I2)=I4N
      PTR=SORTI(I)
      SORTI(I)=SORTI(I2)
      SORTI(I2)=PTR
      I=I-N2
      IF (I.GT.0) GOTO 60
  100 CONTINUE
      GOTO 50
C
  110 HELPM7=.TRUE.
C
  120 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module HELPM8
C
      LOGICAL FUNCTION HELPM8(BUFFER,N)
C
C Provides serial byte input from library
C
      LOGICAL DISC
      INTEGER BUFFER(256),DEVICE,IITEM,N
      INTEGER*4 IBLK
C
      INCLUDE 'COMMON'
C
      EQUIVALENCE (SMGR1,IBLK),(SMGI8,IITEM),(SMGI6,DEVICE)
C
      HELPM8=.FALSE.
C
      IF (IITEM.GT.LNBLK) THEN
C
C Fetch new block
C
         IBLK=IBLK+1
         IF (DISC(1,DEVICE,LNBLK4,BUFFER,IBLK,1,0)) GOTO 10
         IITEM=1
      ENDIF
C
      N=BUFFER(IITEM)
      IITEM=IITEM+1
      RETURN
C
   10 HELPM8=.TRUE.
C
      RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module HELPM9
C
C Opens library file, initialising DCB appropriately; iff NEW,
C creates files size FILKB KB, and reserves directory size DIRKB KB
C
      LOGICAL FUNCTION HELPM9(NEW,NAME,FILKB,DIRKB)
C
      LOGICAL DISC,NEW
      REAL DIRKB,FILKB
      INTEGER NAME(256),TEXT(256),IDTAG(13)
      INTEGER DEVICE,DIRSZE,NUM,I,NCBS
      INTEGER*4 DSIZE,I41,I4BLK1,I4BLK2
      PARAMETER (I41=1, I4BLK1=256*256, I4BLK2=256)
C
      INCLUDE 'COMMON'
C
      EQUIVALENCE (RB1,TEXT),(SMGI6,DEVICE)
C
C Help Library file id tag (Chars Semper.help + vers,rel)
C
      DATA IDTAG/83,101,109,112,101,114,46,104,101,108,112,6,1/
C
      HELPM9=.FALSE.
C
C Code for OLD mode
C -----------------
C
      IF (.NOT.NEW) THEN
C
C Assign
C
         CALL MCDC61(3,DEVICE,DSIZE,NUM,NAME,ERROR)
         IF (ERROR.NE.0) GOTO 40
C
C Activate provisionally
C
         MEDN(DEVICE)=MEDDC
         FLSIZ(DEVICE)=1
         PROTN(DEVICE)=0
         DVTYP(DEVICE)=FLTHEL
C
C Check header
C
         IF (DISC(1,DEVICE,LNBLK4,RB1,I41,1,0)) GOTO 40
C
C Check id tag
C
         DO 10 I=1,11
            IF (TEXT(I).NE.IDTAG(I)) GOTO 40
   10    CONTINUE
C
C Transfer file size and directory size to device table
C
         FLSIZ(DEVICE)=(TEXT(14))*I4BLK1+
     +                 (TEXT(15))*I4BLK2+
     +                 (TEXT(16))
         DRSIZ(DEVICE)=256*TEXT(17)+TEXT(18)
      ELSE
C
C Code for NEW mode
C -----------------
C Convert FILKB from MB to cache buffers
C
         NCBS=NINT(FILKB*1024.0/REAL(CBSIZE)/REAL(LNBLK))
         IF (NCBS.LE.0) GOTO 40
         DSIZE=NCBS
         DSIZE=DSIZE*CBSIZE
         CALL MCDC61(5,DEVICE,DSIZE,NUM,NAME,ERROR)
         IF (ERROR.NE.0) GOTO 40
C
C Activate
C
         MEDN(DEVICE)=MEDDC
         FLSIZ(DEVICE)=DSIZE
         PROTN(DEVICE)=0
         DVTYP(DEVICE)=FLTHEL
C
C Initialise header
C
         DIRSZE=INT((DIRKB*1024.0+REAL(LNBLK)*2.0)/REAL(LNBLK))
         DRSIZ(DEVICE)=DIRSZE
C
C Initialise header block
C
         DO 20 I=1,32
            TEXT(I)=0
   20    CONTINUE
C
C Insert id tag
C
         DO 30 I=1,13
            TEXT(I)=IDTAG(I)
   30    CONTINUE
C
C Deposit file size and directory size
C
         TEXT(14)=DSIZE/I4BLK1
         TEXT(15)=(DSIZE-(TEXT(14))*I4BLK1)/I4BLK2
         TEXT(16)=DSIZE-(TEXT(14))*I4BLK1-
     +                  (TEXT(15))*I4BLK2
         TEXT(17)=DIRSZE/256
         TEXT(18)=DIRSZE-TEXT(17)*256
C
C Output header block
C
         IF (DISC(2,DEVICE,LNBLK4,RB1,I41,1,0)) GOTO 40
      ENDIF
      RETURN
C
C Errors
C
   40 HELPM9=.TRUE.
      RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module SEMXA1
C
      LOGICAL FUNCTION SEMXA1(IOP,BUFFER,LENGTH,PTR,VALUE,IVALUE)
      INTEGER IOP,BUFFER(*),LENGTH,PTR,IVALUE
      REAL VALUE
C
C Cut down version of SEMXA1
C Decodes character input and produces character output,
C reading and writing names (with indices) and numbers.
C
C IOP
C  0    reads next non-space from BUFFER(PTR..) to IVALUE
C  1    reads/packs a name from BUFFER(PTR..) to IVALUE
C  2    reads a number from BUFFER(PTR..) to VALUE
C
C Return FALSE unless BUFFER overflows before item can be transferred
C
C Opcode 0 advances PTR up to item; others advance it beyond item
C (to LENGTH+1 if buffer overflows)
C
      INCLUDE 'COMMON'
C
      REAL RADIX,SIGN,V
      INTEGER S(4),CH,EX,CNV,FLD,INPTR,I,K,IP,RADCH
      LOGICAL EXPON,INDX,FRPT,NST
      LOGICAL BINARY,HEX,OCTAL,VALID
C
      EQUIVALENCE (EXPON,INDX)
C
      SEMXA1 = .FALSE.
      S(4) = 0
C
C Input modes - check buffer length and ignore spaces
C
   10 VALUE = 0.
      IVALUE = 0
      IF (PTR .GT. LENGTH) GOTO 120
      I = BUFFER(PTR)
      IF (I .EQ. KSPACE) THEN
         PTR = PTR + 1
         GOTO 10
      ENDIF
C
C Non-space found: switch on IOP
C
      IF (IOP .EQ. 0) THEN
         IVALUE = I
         GOTO 110
      ELSE IF (IOP .EQ. 1) THEN
C
C IOP=1: read name
C ----------------
C
         INDX = .FALSE.
   20    S(1) = 0
         S(2) = 0
         S(3) = 0
         K = 1
   30    CH = BUFFER(PTR)
C
C Test for alphanumeric (treating lc as uc) and map to R50
C
         IF (CH.GE.KLCA) CH = CH + (KUCA - KLCA)
C
C Alphabetic (or dollar)
C
         IF (CH.GE.KUCA .AND. CH.LE.KUCZ) THEN
C
C Map alphabet
C
            CH = CH - 64
         ELSE IF (CH.EQ.KDOLLA) THEN
C
C Map dollar
C
            CH = 27
         ELSE
C
C Check for digits (still require alphabetic in first position)
C
            IF (CH.LT.KZERO .OR. CH.GT.KNINE .OR. K.EQ.1) GOTO 40
C
C Map digits
C
            CH = CH - 18
         ENDIF
C
C Ignore characters after 3rd
C
         IF (K.LE.3) THEN
            S(K) = CH
            K = K + 1
         ENDIF
         PTR = PTR + 1
         IF (PTR.LE.LENGTH) GOTO 30
C
C Assemble in pseudo-R50 packing
C
   40    NST = S(1).GE.20
         IF (NST) S(1) = S(1)-20
         IVALUE = (S(1)*40 + S(2))*40 + S(3)
         IF (NST) IVALUE = -IVALUE-1
         IF (.NOT.INDX) THEN
            IF (PTR .GT. LENGTH) GOTO 110
            IF (BUFFER(PTR) .NE. KHASH) GOTO 110
C
C Read index name
C
            IF (K.EQ.1 .OR. K.GT.3) GOTO 130
            PTR = PTR + 1
            IF (PTR.GT.LENGTH) GOTO 130
            IP = PTR
            INDX = .TRUE.
            GOTO 20
         ENDIF
C
C Obtain index value
C
         IF (IP.EQ.PTR) GOTO 130
         IDERR = IVALUE
         ERROR = 25
         GOTO 120
      ELSE IF (IOP .EQ. 2) THEN
C
C IOP=2: read number
C ------------------
C
         INPTR = PTR
         FLD = INPTR + 1
         SIGN = 1.
         RADIX = 10.
         RADCH = KNINE
         V = 0.
         EX = 0
         K = 0
         BINARY = .FALSE.
         HEX = .FALSE.
         OCTAL = .FALSE.
         EXPON = .FALSE.
         FRPT = .FALSE.
         NST = .FALSE.
         GOTO 70
C
C Next character
C
   50    NST = .TRUE.
   60    PTR = PTR+1
         IF (PTR .GT. LENGTH) GOTO 80
C
   70    CH = BUFFER(PTR)
         IF (CH .GE. KLCA .AND. CH .LE. KLCZ) CH = CH + (KUCA - KLCA)
C
         CNV = KZERO
         IF (HEX .AND. (CH .GE. KUCA .AND. CH .LE. KUCF)) THEN
            CNV = KUCA-10
            VALID = .TRUE.
         ELSE
            VALID = CH .GE. KZERO .AND. CH .LE. RADCH
         ENDIF
C
         IF (VALID) THEN
            IF (EXPON) THEN
C
C Exponent handling
C
               I = SIGN
               EX = EX*10 + (CH-KZERO)*I
            ELSE
               V = V*RADIX + FLOAT((CH-CNV))*SIGN
               IF (FRPT) K = K - 1
            ENDIF
            GOTO 50
         ENDIF
C
         IF (PTR .EQ. INPTR) GOTO 100
         IF (FLD .EQ. PTR .AND. V .EQ. 0.0) THEN
C
C Check for Radix changes: input field will be of form
C                 0rnnnedd
C
C      Where r is the radix (b/B=binary,o/O=octal,x/X/h/H=hex)
C            nnnn is first part (unsigned)
C         Note that no exponent is allowed with a radix change
C
            IF (.NOT.(BINARY .OR. HEX. OR. OCTAL)) THEN
               IF (CH .EQ. KUCX .OR. CH .EQ. KUCH .OR.
     +             CH .EQ. KLCX .OR. CH .EQ. KLCH) THEN
                  HEX = .TRUE.
                  RADIX = 16.
                  GOTO 60
               ELSE IF (CH .EQ. KUCB .OR. CH .EQ. KLCB) THEN
                  BINARY = .TRUE.
                  RADIX = 2.
                  RADCH = KONE
                  GOTO 60
               ELSE IF (CH .EQ. KUCO .OR. CH .EQ. KLCO) THEN
                  OCTAL = .TRUE.
                  RADIX = 8.
                  RADCH = KSEVEN
                  GOTO 60
               ENDIF
            ENDIF
         ENDIF
C
C Exponent?
C
         IF (CH .NE. KUCE) GOTO 100
C
C Already defined?
C
         IF (.NOT.(EXPON .OR. BINARY .OR. HEX .OR. OCTAL)) THEN
            SIGN = 1.
            EXPON = .TRUE.
            NST = .FALSE.
            GOTO 60
         ENDIF
C
C Terminated
C
   80    EX = EX + K
   90    IF (EX .LT. 0) THEN
            V = V/10.
            EX = EX + 1
         ELSE IF (EX .GT. 0) THEN
            V = V*10.
            EX = EX - 1
         ELSE
            VALUE = V
            GOTO 110
         ENDIF
         GOTO 90
C
C + - .
C
  100    IF (CH .EQ. KPLUS .OR. CH .EQ. KMINUS) THEN
            IF (NST) GOTO 80
            IF (CH.EQ.KMINUS) SIGN=-1.
            GOTO 60
         ENDIF
         IF (CH .NE. KDOT) GOTO 80
         IF (EXPON .OR. FRPT) GOTO 80
         FRPT = .TRUE.
         GOTO 50
      ENDIF
C
  110 RETURN
C
C Buffer exhausted
C
  120 SEMXA1 = .TRUE.
      GOTO 110
C
C Subscript syntax error
C
  130 ERROR = 17
      GOTO 120
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module HELPUO
C
      LOGICAL FUNCTION HELPUO(TEXT,N,UNIT,ICH)
C
C     ========================================
C
      INTEGER TEXT(*),UNIT,LOGFLE,ICH,N
      PARAMETER (LOGFLE=7)
C
      LOGICAL TERMOP
      INCLUDE 'COMMON'
C
      HELPUO=.TRUE.
C
C If terminal, output via pager
C
      IF (UNIT .EQ. TERM2) THEN
         HELPUO = TERMOP(TEXT,N,ICH)
C
C Else, to log
C
      ELSE
         CALL SEMCHS(RECORD,TEXT,N)
   10    FORMAT (A)
         WRITE (LOGFLE,10) RECORD(1:N)
         ICH = 0
         HELPUO = .FALSE.
      ENDIF
C
      RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module TERMOP
C
      LOGICAL FUNCTION TERMOP(TEXT,N,ICH)
C
C     ===================================
C
C Outputs line of i/c text from TEXT(1 -> N) to terminal, counting lines
C and paging terminal appropriately.
C
      LOGICAL ABANDN,INKEY
      LOGICAL SEMTCR,SEMSOL,SEMTOU,SEMTNV,SEMTPS,SEMTRV,SEMEOL,SEMTFL
      INTEGER TEXT(*)
      INTEGER M, N, ICH, IWID, ILEN
C
      INCLUDE 'COMMON'
C
      TERMOP=.TRUE.
      ICH=0
C
C     Get the current terminal window size
C
      IF (SEMTPS(IWID,ILEN)) GOTO 30
C
C Check that prompting is O.K.
C
      IF (INPUT.NE.TERM1) GOTO 10
C
C     Increment line count
C
      TERCNT=TERCNT+1
C
C     Page height exhausted?
C
      IF (TERCNT.EQ.ILEN) THEN
C
C        Output prompt string
C
         IF (SEMSOL()) GOTO 30
         IF (SEMTRV()) GOTO 30
         IF (SEMTOU('RETURN=line, SPACE=page, N=next, Q=quit')) GOTO 30
         IF (SEMTNV()) GOTO 30
         IF (SEMTFL()) GOTO 30
C
         TERCR=.TRUE.
         TERLF=.TRUE.
C
C        Wait for keystroke from terminal
C
         IF (INKEY(ICH,ERROR)) GOTO 30
C
C        Remove all trace of prompt string
C
         IF (SEMTCR()) GOTO 30
         IF (SEMTOU('                                       ')) GOTO 30
         IF (SEMTCR()) GOTO 30
C
C        Check for signal to abandon
C
         IF (ABANDN(ERROR)) GOTO 30
C
C        No abandon, so see what the character typed was
C
         IF (ICH.EQ.KSPACE) THEN
C
C           SPACE: allow further page
C
            TERCNT=1
         ELSE IF (ICH.EQ.KBRET) THEN
C
C           RETURN: allow one more line
C
            TERCNT=TERCNT-1
         ELSE IF (ICH.EQ.KUCN.OR.ICH.EQ.KLCN) THEN
C
C           N: next (return back to calling program for action)
C
            TERCNT=0
            GOTO 20
         ELSE IF (ICH.EQ.KUCQ.OR.ICH.EQ.KLCQ) THEN
C
C           Q: quit
C
            GOTO 30
         ENDIF
      ENDIF
C
C     Truncate length of output text to terminal width
C
   10 M=MIN(N,IWID-1)
C
C     Output text to terminal
C
      CALL SEMCHS(RECORD,TEXT,M)
      IF (SEMSOL()) GOTO 30
      IF (SEMTOU(RECORD(1:M))) GOTO 30
      IF (SEMEOL()) GOTO 30
      IF (SEMTFL()) GOTO 30
C
   20 TERMOP=.FALSE.
C
C     All done
C
   30 RETURN
C
C Copyright (C) 1987-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module SEMCON
C
      LOGICAL FUNCTION SEMCON(TEXT)
C
C Cut-down version of console output routine
C Outputs everything to the terminal
C
      CHARACTER*(*) TEXT
C
      LOGICAL SEMSOL,SEMTOU,SEMEOL,SEMTFL
      INTEGER LNBLNK
C
      SEMCON = .TRUE.
C
C Output line of text
C
      IF (SEMSOL()) GOTO 10
      IF (SEMTOU(TEXT(1:MAX(LNBLNK(TEXT),1)))) GOTO 10
      IF (SEMEOL()) GOTO 10
      IF (SEMTFL()) GOTO 10
C
      SEMCON = .FALSE.
C
   10 RETURN
C
C Copyright (C) 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module SEMDIA
C
      LOGICAL FUNCTION SEMDIA(TEXT,ISEVER)
C
C Cut-down version of diagnostic output routine
C Outputs everything to the terminal via SEMCON
C
      CHARACTER*(*) TEXT
      INTEGER ISEVER
C
      LOGICAL SEMCON
C
      SEMDIA = SEMCON(TEXT)
C
      RETURN
C
C Copyright (C) 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module SEMDIA
C
      LOGICAL FUNCTION SEMMON(TEXT,MODULE,ICLASS)
C
C Cut-down version of monitor output routine
C Outputs nothing
C
      CHARACTER*(*) TEXT,MODULE
      INTEGER ICLASS
C
      SEMMON = .FALSE.
C
      RETURN
C
C Copyright (C) 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 system module SEMSOL
C
      LOGICAL FUNCTION SEMSOL()
C
C Generate sequence of carriage-control characters to ensure the start
C of a new line for subsequent terminal output
C Cut down version omitting reference to UIFSSU
C
      LOGICAL SEMTCR,SEMTLF
C
      INCLUDE 'COMMON'
C
      SEMSOL = .TRUE.
C
C Output CR if one has not already been output
C
      IF (.NOT.TERCR) THEN
         IF (SEMTCR()) GOTO 10
      ENDIF
C
C Output LF if one has not already been output
C
      IF (.NOT.TERLF) THEN
         IF (SEMTLF()) GOTO 10
      ENDIF
C
C Reset CR and LF flags
C
      TERCR = .FALSE.
      TERLF = .FALSE.
C
      SEMSOL = .FALSE.
C
   10 RETURN
C
C Copyright (C) 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C----------------------------------------------------------------------
C
C      LOGICAL FUNCTION READLN ( BUFFER, LENGTH, PROMPT, IERROR )
C      ----------------------------------------------------------
C
C      PARAMETERS:
C
C      integer(*) buffer : OUTPUT - The data read from the keyboard.
C
C      integer length : INPUT/OUTPUT - On input, the length of the
C                       buffer into which to read the data.  On output,
C                       the length of the buffer actually filled.
C
C      character prompt : INPUT - prompt string to use
C
C      integer ierror : OUTPUT - Error code.
C
C      Reads data from the terminal (strictly the keyboard event queue),
C      handling any erase and kill processsing.
C
C      Function returns FALSE if sucessful, otherwise TRUE with error
C      code set to 4 if control-C is pressed.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION READLN ( BUFFER, LENGTH, PROMPT, IERROR )
C
C     ==========================================================
C
      INTEGER BUFFER(*), LENGTH, IERROR
      CHARACTER*(*) PROMPT
C
      LOGICAL KLINE
C
      INTEGER LEN2,NS
      CHARACTER*255 STRING
C
      READLN = .TRUE.
C
      LEN2 = LENGTH
      IF (LEN2 .GT. 255) LEN2 = 255
C
      NS = 0
      IF (KLINE(PROMPT,.TRUE.,STRING(1:LEN2),NS)) THEN
         READLN = .TRUE.
      ELSE
         CALL SEMICS(STRING,BUFFER,NS)
         LENGTH = NS
         READLN = .FALSE.
      ENDIF
      RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Dummy Semper main program for handler
C
      SUBROUTINE SEMAIN
      STOP
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
