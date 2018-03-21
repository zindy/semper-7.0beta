C Semper 6 processing module SEMHEL
C
      SUBROUTINE SEMHEL
C
C Provides second level HELP facilities, by interrogating the help
C library
C
      INTEGER IVAL
      LOGICAL ABANDN,SEMCON,SEMHE2,SEMHE3,SEMHE4,SEMLOG,SEMTPS
C
      INCLUDE 'COMMON'
C
      INTEGER*4 SORTB(256),BBLK,BLK,CBLK,EBLK
      INTEGER BUFF1(256),TEXT(81),KEY(80),SORTI(256)
C
      INTEGER CH,CITEM,DEVICE,EITEM,ECOUNT,I,I1,I2,IPASS,ISORT,ITEM
      INTEGER N,N1,N2,N3,NK,NSORT,OPTR,SORTL,IWID,ILEN
C
      LOGICAL FULL,HEAD,LLOG,TOPICS,HTERM
      LOGICAL FOUND,FOUNDL,MASTER,TERMIN
C
      INTEGER KMAX
      PARAMETER (KMAX=40)
      INTEGER KEY1(KMAX),KEY2(KMAX)
C
      EQUIVALENCE (RB1,BUFF1),(RB2,SORTB),(RB3,SORTI)
      EQUIVALENCE (RB4,TEXT),(RB4(82),KEY)
      EQUIVALENCE (SMGR2,BBLK),(SMGR3,BLK),(SMGI5,ITEM),(SMGI6,DEVICE)
C
C Packed names
C
      INTEGER NDOLLR
      PARAMETER (NDOLLR=-11201)
C
      INTEGER*4 I4BLK1,I4BLK2
      PARAMETER (I4BLK1=256*256, I4BLK2=256)
C
C Fetch current terminal page size
C
      IF (SEMTPS(IWID,ILEN)) GOTO 310
C
C Fetch start position of first item in the command line
C
      I=IVAL(NDOLLR)
C
C If zero start position, rest of line is blank, so output fixed help
C text to console output stream
C
      IF (I.EQ.0) THEN
         IF (SEMCON(' ')) GOTO 310
         RECORD(1:20) = 'HELP topic topic..  '
         RECORD(21:64)= 'types brief information about given topic(s)'
         IF (SEMCON(RECORD(1:64))) GOTO 310
         RECORD(1:43)  = 'A file will be opened if information exists'
         IF (SEMCON(RECORD(1:43))) GOTO 310
         RECORD(1:20) = 'HELP/MON  topic     '
         RECORD(27:68)=   'outputs help directly to the screen       '
         IF (SEMCON(RECORD(1:68))) GOTO 310
         RECORD(1:20) = 'HELP/FULL topic     '
         RECORD(27:54)=   'extended form of information'
         IF (SEMCON(RECORD(1:54))) GOTO 310
         RECORD(1:20) = 'HELP/HEADER topic   '
         RECORD(27:68)=   'header lines only of all relevant topic(s)'
         IF (SEMCON(RECORD(1:68))) GOTO 310
         IF (SEMCON(' ')) GOTO 310
         RECORD(1:20) = 'HELP/TOPICS         '
         RECORD(21:65)= 'lists topic names of all help library entries'
         IF (SEMCON(RECORD(1:65))) GOTO 310
         RECORD(12:16) = '/FULL'
         RECORD(39:68) = 'with alias (alternative) names'
         IF (SEMCON(RECORD(1:68))) GOTO 310
         RECORD(12:17) = ' topic'
         RECORD(39:72) = 'beginning with indicated name only'
         IF (SEMCON(RECORD(1:72))) GOTO 310
         IF (SEMCON(' ')) GOTO 310
         RECORD = 'You can ask about several topics at once.'//
     +            '  Topic names can be abbreviated,'
         IF (SEMCON(RECORD)) GOTO 310
         RECORD = 'e.g. H matches HELP, but all the letters you give '//
     +            'are treated as significant'
         IF (SEMCON(RECORD)) GOTO 310
         RECORD = '(i.e. LOGICAL does not match LOGFILE).'//
     +            '  A full-stop causes matching to resume'
         IF (SEMCON(RECORD)) GOTO 310
         RECORD = 'after the next full stop in the topic name, '//
     +            'e.g. D.S matches DISPLAY.SCALING.'
         IF (SEMCON(RECORD)) GOTO 310
         RECORD = 'Alternative names (aliases) are recorded for '//
     +            'most topics.  Option /LOG sends'
         IF (SEMCON(RECORD)) GOTO 310
         RECORD = 'output to the log output stream; HELP/LOG on '//
     +            'its own outputs the entire text'
         IF (SEMCON(RECORD)) GOTO 310
         IF (SEMCON('of all assigned libraries.')) GOTO 310
         IF (SEMCON(' ')) GOTO 310
         RECORD = 'If the message ''No help library assigned'' '//
     +            'appears, did you run make help?'
         IF (SEMCON(RECORD)) GOTO 310
         RECORD = 'to find out how to use ASSIGN to assign one; '//
     +            'at worst, you may find the SYNTAX'
         IF (SEMCON(RECORD)) GOTO 310
         RECORD = 'command which lists the keys and options '//
     +            'recognised with each command'
         IF (SEMCON(RECORD)) GOTO 310
         RECORD = 'useful, and typing SHOW will allow you to find '//
     +            'out about the current state'
         IF (SEMCON(RECORD)) GOTO 310
         IF (SEMCON('of your session.')) GOTO 310
         GOTO 310
      ENDIF
C
C Initialise key count
C
      NK=0
C
C Initialise option flags
C
      FULL=.FALSE.
      HEAD=.FALSE.
      LLOG=.FALSE.
      TOPICS=.FALSE.
      HTERM=.FALSE.
C
C If more characters in command line, search for next keyword
C
   10 IF (I.LE.COMLIM) THEN
C
C Search for next non-blank character in command line
C
   20    IF (LINBUF(I).EQ.KSPACE) THEN
C
C Move on to next character (if any)
C
            I=I+1
            IF (I.LE.COMLIM) GOTO 20
C
C Rest of command line was blank
C
            GOTO 40
         ENDIF
C
C Record start of non-blank string
C
         I1=I
C
C If first character is '/', move on to next character (if any)
C
         IF (LINBUF(I1).EQ.KSLASH) I=I+1
C
C Search for next blank or '/' in command line
C
   30    IF (I.LE.COMLIM) THEN
            CH = LINBUF(I)
            IF (CH.NE.KSPACE .AND. CH.NE.KSLASH) THEN
C
C Convert upper-case character to lower-case
C
               IF (CH.GE.KUCA .AND. CH.LE.KUCZ) THEN
                  LINBUF(I) = CH + (KLCA - KUCA)
               ENDIF
C
C Move on to next character (if any)
C
               I = I + 1
               GOTO 30
            ENDIF
         ENDIF
C
C Record end of non-blank string
C
         I2 = I - 1
C
C If first letter in string is slash '/', string is an option
C
         IF (LINBUF(I1).EQ.KSLASH) THEN
C
C Look for options FULL, LOG and TOPICS
C
            CH = LINBUF(I1+1)
            IF (CH.EQ.KLCF) THEN
               FULL=.TRUE.
            ELSE IF (CH.EQ.KLCH) THEN
               HEAD=.TRUE.
            ELSE IF (CH.EQ.KLCL) THEN
               LLOG=.TRUE.
            ELSE IF (CH.EQ.KLCT) THEN
               TOPICS=.TRUE.
            ELSE IF (CH.EQ.KLCM) THEN
               HTERM=.TRUE.
            ENDIF
C
C Otherwise, string is keyword
C
         ELSE
C
C Record position of keyword in command line
C
            IF (NK.LT.KMAX) THEN
               NK=NK+1
               KEY1(NK)=I1
               KEY2(NK)=I2
            ENDIF
         ENDIF
C
C Move on to next keyword
C
         GOTO 10
      ENDIF
C
C Initialise
C
   40 SORTL=LNBUF/LNINT4
C
C  Insert here
      if( (.not.full).and.(.not.topics).and.(.not.llog)
     1  .and.(.not.hterm) )then
      call Helpfl6(linbuf(i1),i2-i1+1,found)
      if(found)return
      endif

C Topic list?
C
      IF (TOPICS) GOTO 190
C
C Type entry
C ----------
C
      FOUND=.FALSE.
      FOUNDL=.FALSE.
C
C Loop over all devices looking for help libraries
C
      DO 180 DEVICE=1,NDVS
C
         IF (.NOT.(MEDN(DEVICE).EQ.MEDDC.OR.MEDN(DEVICE).EQ.MEDVM).OR.
     +       DVTYP(DEVICE).NE.FLTHEL) GOTO 180
C
         FOUNDL=.TRUE.
         BBLK=0
C
         DO 170 IPASS=1,2
C
C Search directory for key
C
            BLK=2
            ITEM=4
C
C Note entry start, checking terminator
C
   50       IF (SEMHE2(ECOUNT)) GOTO 310
            IF (ECOUNT.EQ.0) GOTO 170
C
            EBLK=BLK
            EITEM=ITEM
C
C No: flag master key
C
            MASTER=.TRUE.
C
C Entry active / another key present?
C
   60       IF (SEMHE2(N)) GOTO 310
            IF (N.EQ.0) GOTO 160
C
C Yes: note start of key
C
            CBLK=BLK
            CITEM=ITEM
C
C Fetch key name string
C
            DO 70 I=1,N
               IF (SEMHE2(KEY(I))) GOTO 310
   70       CONTINUE
C
C See if string matches any of the keywords on the command line
C
            IF (.NOT.SEMHE3(1,KEY,N,KEY1,KEY2,NK)) GOTO 150
C
C Match found
C
            FOUND=.TRUE.
C
C If pass 2, ignore entry if matched key is master key
C
            IF (IPASS.EQ.2.AND.MASTER) GOTO 160
C
C Insert master key in header line
C
            TEXT(1)=KSPACE
            BLK=EBLK
            ITEM=EITEM
            IF (SEMHE2(N1)) GOTO 310
            N1=N1+1
            DO 80 I=2,N1
               IF (SEMHE2(CH)) GOTO 310
C
C Capitalise first letter
C
               IF (I.EQ.2) THEN
                  IF (CH.GE.KLCA .AND. CH.LE.KLCZ) CH = CH + (KUCA-KLCA)
               ENDIF
               TEXT(I)=CH
   80       CONTINUE
C
C If alias, insert in header line too
C
            IF (.NOT.MASTER) THEN
               TEXT(N1+1)=KSPACE
               TEXT(N1+2)=KEQUAL
               TEXT(N1+3)=KSPACE
               N1=N1+3
               BLK=CBLK
               ITEM=CITEM
               DO 90 I=1,N
                  N1=N1+1
                  IF (SEMHE2(TEXT(N1))) GOTO 310
   90          CONTINUE
            ENDIF
            N1=N1+1
            TEXT(N1)=KSPACE
C
C Output header line
C
            DO 100 I=1,IWID
               RECORD(I:I)='-'
  100       CONTINUE
C
            IF (N1.GT.IWID) THEN
               CALL SEMCHS(RECORD,TEXT,N1)
               N=N1
            ELSE
               N=1+IWID/2
               CALL SEMCHS(RECORD(N-N1/2:N+(N1-1)/2),TEXT,N1)
               N=IWID
            ENDIF
C
            IF (LLOG) THEN
               IF (SEMLOG(RECORD(1:N))) GOTO 310
            ELSE
               IF (SEMCON(RECORD(1:N))) GOTO 310
            ENDIF
C
            IF (HEAD) GOTO 160
C
C Locate text and print it
C
            BLK=EBLK
            ITEM=EITEM+ECOUNT-3
C
C Read the block number
C
            IF (SEMHE2(N1)) GOTO 310
            IF (SEMHE2(N2)) GOTO 310
            IF (SEMHE2(N3)) GOTO 310
C
C Combine 3 bytes into block number
C
            BLK=(N1)*I4BLK1 +
     +          (N2)*I4BLK2 +
     +          (N3)
            ITEM=4
C
C Check for abandon
C
  110       IF (ABANDN(ERROR)) GOTO 310
C
C Fetch character count
C
            IF (SEMHE2(N)) GOTO 310
            IF (N.EQ.255) GOTO 160
C
C Copy line chars to local buffer
C
            DO 120 I=1,N
               IF (SEMHE2(TEXT(I))) GOTO 310
  120       CONTINUE
C
C Chech for dollar separator
C
            IF (TEXT(1).EQ.KDOLLA) THEN
               IF (FULL) THEN
                  RECORD(1:1)=' '
                  N=1
               ELSE
                  RECORD(1:23)='(more via HELP /FULL..)'
                  N=23
               ENDIF
            ELSE
               CALL SEMCHS(RECORD,TEXT,N)
            ENDIF
C
C Output line of text
C
            IF (LLOG) THEN
               IF (SEMLOG(RECORD(1:N))) GOTO 310
            ELSE
  130          IF (N .GT. IWID) THEN
                  I = IWID
  140             IF (I .GT. 1) THEN
                     IF (RECORD(I:I) .NE. ' ') THEN
                        I = I - 1
                        GOTO 140
                     ENDIF
                  ENDIF
                  IF (SEMCON(RECORD(1:I))) GOTO 310
                  RECORD = RECORD(I+1:)
                  N = N - I
                  GOTO 130
               ENDIF
               IF (SEMCON(RECORD(1:N))) GOTO 310
            ENDIF
C
            IF (TEXT(1).NE.KDOLLA.OR.FULL) GOTO 110
C
            GOTO 160
C
C Advance to next key
C
  150       IF (IPASS.EQ.2) THEN
               BLK=CBLK
               ITEM=CITEM+N
               MASTER=.FALSE.
               GOTO 60
            ENDIF
C
C Advance to next entry
C
  160       BLK=EBLK
            ITEM=EITEM+ECOUNT
            GOTO 50
  170    CONTINUE
  180 CONTINUE
C
C Fault no help libraries or no help topics
C
      IF (.NOT.FOUND) THEN
         IF (FOUNDL) THEN
            ERROR=115
         ELSE
            ERROR=114
         ENDIF
      ENDIF
C
      GOTO 310
C
C List active entries (with aliases)
C ----------------------------------
C
  190 FOUNDL=.FALSE.
C
C Loop over all devices looking for help libraries
C
      DO 300 DEVICE=1,NDVS
C
         IF (.NOT.(MEDN(DEVICE).EQ.MEDDC.OR.MEDN(DEVICE).EQ.MEDVM).OR.
     +       DVTYP(DEVICE).NE.FLTHEL) GOTO 300
C
         FOUNDL=.TRUE.
         BBLK=0
C
C Ask for a sorted list of all master keys
C
         IF (SEMHE4(SORTB,SORTI,SORTL,KEY1,KEY2,NK,NSORT)) GOTO 310
C
C Skip to next device if no selected entries found
C
         IF (NSORT.EQ.0) GOTO 300
C
C Output header
C
         DO 200 I=1,IWID
            RECORD(I:I)='-'
  200    CONTINUE
C
         IF (IWID.LT.17) THEN
            IF (LLOG) THEN
               RECORD='Full Entry List'
               N=15
            ELSE
               RECORD='Entry List'
               N=10
            ENDIF
         ELSE
            N=1+IWID/2
            IF (LLOG) THEN
               RECORD(N-8:N+8)=' Full Entry List '
            ELSE
               RECORD(N-6:N+5)=' Entry List '
            ENDIF
            N=IWID
         ENDIF
C
         IF (LLOG) THEN
            IF (SEMLOG(RECORD(1:N))) GOTO 310
         ELSE
            IF (SEMCON(RECORD(1:N))) GOTO 310
         ENDIF
C
C Scan directory, printing names
C
         BLK=2
         ITEM=4
         ISORT=0
         OPTR=0
         TERMIN=.FALSE.
C
C Get next entry, switching code acc to whether sorted
C
  210    IF (ABANDN(ERROR)) GOTO 310
C
         IF (NSORT.LT.0) THEN
C
C Unsorted: refer to entry directly
C Note entry start, checking terminator
C
            IF (SEMHE2(ECOUNT)) GOTO 310
            IF (ECOUNT.EQ.0) GOTO 280
            EBLK=BLK
            EITEM=ITEM
            IF (SEMHE2(N)) GOTO 310
            IF (N.EQ.0) GOTO 270
            DO 220 I=1,N
               IF (SEMHE2(KEY(I))) GOTO 310
  220       CONTINUE
            IF (.NOT.SEMHE3(2,KEY,N,KEY1,KEY2,NK)) GOTO 270
            BLK=EBLK
            ITEM=EITEM
C
C Sorted: refer to pointer list
C
         ELSE
            ISORT=ISORT+1
            IF (ISORT.GT.NSORT) GOTO 280
            BLK=SORTB(ISORT)
            ITEM=SORTI(ISORT)
         ENDIF
C
C Get master key, provided entry active
C
         MASTER=.TRUE.
  230    IF (SEMHE2(N)) GOTO 310
         IF (N.EQ.0) GOTO 270
C
C Suppress entries with names beginning with question marks..
C
         CBLK=BLK
         CITEM=ITEM
         IF (SEMHE2(CH)) GOTO 310
         IF (CH.EQ.KQUEST) GOTO 270
         BLK=CBLK
         ITEM=CITEM
C
C Force new output line on each master key if FULL
C
         IF (FULL.AND.MASTER.AND.OPTR.NE.0) GOTO 290
C
C Prefill line buffer with spaces if necessary
C
  240    IF (OPTR.EQ.0) THEN
            DO 250 I=1,IWID
               TEXT(I)=KSPACE
  250       CONTINUE
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
         IF (N2.GT.IWID) GOTO 290
C
C It fits now, at least
C
         OPTR=N1
C
C Copy key chars
C
         DO 260 I=1,N
            OPTR=OPTR+1
            IF (SEMHE2(CH)) GOTO 310
C
C Capitalise first letter if master
C
            IF (MASTER) THEN
               IF (I.EQ.1) THEN
                  IF (CH.GE.KLCA .AND. CH.LE.KLCZ) CH = CH + (KUCA-KLCA)
               ENDIF
            ENDIF
            TEXT(OPTR)=CH
  260    CONTINUE
         MASTER=.FALSE.
         IF (FULL) GOTO 230
C
C Advance to next entry
C
  270    BLK=EBLK
         ITEM=EITEM+ECOUNT
         GOTO 210
C
C End of directory: flush line buffer and quit
C
  280    TERMIN=.TRUE.
C
C Map and print line
C
  290    IF (OPTR.NE.0) THEN
            CALL SEMCHS(RECORD,TEXT,OPTR)
            IF (LLOG) THEN
               IF (SEMLOG(RECORD(1:OPTR))) GOTO 310
            ELSE
               IF (SEMCON(RECORD(1:OPTR))) GOTO 310
            ENDIF
            OPTR=0
         ENDIF
C
         IF (.NOT.TERMIN) GOTO 240
C
C List complete: any more devices?
C
  300 CONTINUE
C
      IF (.NOT.FOUNDL) ERROR=114
C
  310 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module SEMHE2
C
      LOGICAL FUNCTION SEMHE2(VALUE)
      INTEGER VALUE
C
C Provides buffered byte input from library file,
C using block/item numbers as position pointers, bumped
C by SEMHE2 after each access
C
      LOGICAL DISC
C
      INCLUDE 'COMMON'
C
      INTEGER*4 BBLK,BLK
      INTEGER IB1(256),DEVICE,ITEM
C
      EQUIVALENCE (RB1,IB1)
      EQUIVALENCE (SMGR2,BBLK),(SMGR3,BLK),(SMGI5,ITEM)
      EQUIVALENCE (SMGI6,DEVICE)
C
      SEMHE2=.TRUE.
C
C Normalise BLK,ITEM if necessary
C
   10 IF (ITEM.GT.LNBLK) THEN
         ITEM=ITEM-LNBLK
         BLK=BLK+1
         GOTO 10
      ENDIF
C
C If required block not already buffered, fetch new block
C
      IF (BLK.NE.BBLK) THEN
         BBLK=0
         IF (DISC(1,DEVICE,LNBLK4,IB1,BLK,NFMINT,NFMBYT)) GOTO 20
         BBLK=BLK
      ENDIF
C
C Pick up value
C
      VALUE=IB1(ITEM)
C
C Bump position
C
      ITEM=ITEM+1
C
      SEMHE2=.FALSE.
C
   20 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module SEMHE3
C
      LOGICAL FUNCTION SEMHE3(OPC,KEY,N,KEY1,KEY2,NK)
      INTEGER OPC,KEY(*),N,KEY1(*),KEY2(*),NK
C
C Compares N character specimen string in array KEY (in i/c form) with
C each of the NK strings contained in the command line buffer
C LINBUF.  The pointers to the start and end of each match string are
C contained in arrays KEY1 and KEY2.  SEMHE3 returns a value .TRUE.
C if a suitable match is found, and .FALSE. otherwise.
C If no match strings are given (NK = 0), SEMHE3 returns .TRUE.
C A straight match is found if all of the characters in a match string
C agree with the corresponding characters in the specimen string, e.g.
C match strings 'a', 'ar', 'are' and 'area' all agree with the specimen
C string 'area'.  To avoid difficulties with upper and lower-case
C characters, all upper-case characters in the specimen and match string
C should be converted to lower-case before SEMHE3 is called.
C The period character '.' provides for a more clever, multi-level
C matching scheme.  If a '.' is encountered in a match string, matching
C of characters resumes at the next '.' in the specimen string.  If no
C further '.' is found in the specimen string, there is no match.
C Examples:     specimen string = 'area.option'
C                  match   "    = 'b','a..','a.k' => no match
C                    "     "    = 'a','are','area.o','a.o','a.' => match
C If OPC is set to 1 instead of 2, unmatched specimen characters are
C ignored as long as there is no '.' amongst them.  If a '.' is
C encountered, there is no match.  In the above example, match strings
C 'a' and 'are' would not match if OPC is set to 1, but would match if
C OPC is set to 2.  The different matching schemes are provided so that
C HELP /TOPICS (OPC = 2) will list all topics beginning with one of the
C match strings, regardless of any '.' in the specimen string, whereas,
C plain HELP (OPC = 1) will list only the master topic and no
C sub-topics (topics with an embedded '.').
C
      INCLUDE 'COMMON'
C
      INTEGER I,J,K
C
      SEMHE3=.TRUE.
C
C No match strings treated as successful match
C
      IF (NK.EQ.0) GOTO 50
C
C Compare specimen string with each match string in turn
C
      DO 40 I=1,NK
C
C Initialise pointer to next character in specimen string
C
         K=0
C
C Step through match string
C
         DO 20 J=KEY1(I),KEY2(I)
C
C Increment pointer into specimen string, no match if end of string
C
            K=K+1
            IF (K.GT.N) GOTO 40
C
C Check for '.' in match string
C
            IF (LINBUF(J).EQ.KDOT) THEN
C
C If so, step forwards to next '.' in specimen string
C
   10          IF (KEY(K).NE.KDOT) THEN
C
C Increment pointer into specimen string, no match if end of string
C
                  K=K+1
                  IF (K.GT.N) GOTO 40
                  GOTO 10
               ENDIF
            ENDIF
C
C No match if corresponding characters in specimen and match strings
C are different
C
            IF (KEY(K).NE.LINBUF(J)) GOTO 40
   20    CONTINUE
C
C If OPC is set to 1, see if rest of specimen string has a '.' in it
C
         IF (OPC.EQ.1) THEN
   30       K=K+1
C
            IF (K.LE.N) THEN
               IF (KEY(K).NE.KDOT) GOTO 30
               GOTO 40
            ENDIF
         ENDIF
C
C Specimen string successfully matched
C
         GOTO 50
   40 CONTINUE
C
C Specimen string does not match any of the match strings
C
      SEMHE3=.FALSE.
C
   50 RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary module SEMHE4
C
      LOGICAL FUNCTION SEMHE4(SORTB,SORTI,SORTL,KEY1,KEY2,NK,NSORT)
      INTEGER*4 SORTB(*)
      INTEGER SORTI(*),SORTL,KEY1(*),KEY2(*),NK,NSORT
C
C Prepares a list of entry pointers, sorted into alphabetical order
C by first keys
C
      LOGICAL SEMHE2,SEMHE3
C
      INCLUDE 'COMMON'
C
      INTEGER*4 BLK,EBLK,I4N
      INTEGER CH,ECOUNT,EITEM,I,I2,ITEM,J,KEY(80),KEYLEN,N,N1,N2,PTR
C
      EQUIVALENCE (SMGR3,BLK),(SMGI5,ITEM)
C
      SEMHE4=.TRUE.
C
C Initialise selected entry count
C
      NSORT=0
C
C Initialise pointers to start of help directory
C
      BLK=2
      ITEM=4
C
C Fetch entry length
C
   10 IF (SEMHE2(ECOUNT)) GOTO 110
C
C If length non-zero, process next entry
C
      IF (ECOUNT.NE.0) THEN
C
C Note pointers to start of entry
C
         EBLK=BLK
         EITEM=ITEM
C
C Fetch character count
C
         IF (SEMHE2(N)) GOTO 110
C
C If count non-zero, entry is active
C
         IF (N.NE.0) THEN
C
C Fetch entry name
C
            DO 20 I=1,N
               IF (SEMHE2(KEY(I))) GOTO 110
   20       CONTINUE
C
C If name matches any keywords on command line, add entry to list
C
            IF (SEMHE3(2,KEY,N,KEY1,KEY2,NK)) THEN
C
C Increment entry count
C
               NSORT=NSORT+1
C
C Return if entry list full
C
               IF (NSORT.GT.SORTL) THEN
                  NSORT=-1
                  GOTO 100
               ENDIF
C
C Add entry pointers to list
C
               SORTB(NSORT)=EBLK
               SORTI(NSORT)=EITEM
            ENDIF
         ENDIF
C
C Advance to next entry
C
         BLK=EBLK
         ITEM=EITEM+ECOUNT
         GOTO 10
      ENDIF
C
C Return if no entries to sort
C
      IF (NSORT.EQ.0) GOTO 100
C
C Shell-sort list
C
      N2=0
   30 N2=N2+N2+1
      IF (N2.LT.NSORT) GOTO 30
C
   40 N2=N2/2
      IF (N2.EQ.0) GOTO 100
      N1=NSORT-N2
      DO 90  J=1,N1
         I=J
   50    I2=I+N2
C
C Compare the two keys: fetch key I to local store
C
         BLK=SORTB(I)
         ITEM=SORTI(I)
         IF (SEMHE2(KEYLEN)) GOTO 110
         DO 60  PTR=1,KEYLEN
            IF (SEMHE2(KEY(PTR))) GOTO 110
   60    CONTINUE
C
C Get length of key I2 and compare with key I
C
         BLK=SORTB(I2)
         ITEM=SORTI(I2)
         IF (SEMHE2(ECOUNT)) GOTO 110
         N=MIN(KEYLEN,ECOUNT)
C
C Compare key characters
C
         DO 70 PTR=1,N
            IF (SEMHE2(CH)) GOTO 110
            IF (KEY(PTR)-CH)90,70,80
   70    CONTINUE
C
C Key chars present match: shorter sorts low
C
         IF (N.GE.KEYLEN) GOTO 90
C
C Swap the key pointers
C
   80    I4N=SORTB(I)
         SORTB(I)=SORTB(I2)
         SORTB(I2)=I4N
         PTR=SORTI(I)
         SORTI(I)=SORTI(I2)
         SORTI(I2)=PTR
         I=I-N2
         IF (I.GT.0) GOTO 50
   90 CONTINUE
      GOTO 40
C
  100 SEMHE4=.FALSE.
C
  110 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
