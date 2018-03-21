C Semper 6 plus System Generation Program SEMGEN
C
      PROGRAM SEMGEN
C
C Generates Semper main program source code from command description
C document
C
C Main program contains calls to all processing modules, preceded by a
C call to SEMINX and a computed GOTO selecting a processing module call
C
C The processing command descriptors are contained in a long
C integer array, comprising five lists - the command list, the key
C list, the open request list, the string list and the named macro
C list - preceded by four numbers
C
C The command list contains for each separate command descriptor -
C (1) The command name
C (2) The names of any synonyms
C (3) An offset pointer to the 1st element of the next c.d.
C     (or to zero on the last c.d.)
C (4) A series of items, which may be any of
C     (A) An option name
C     (B) An offset pointer to a key definition in the key list
C     (C) An offset pointer to an open defn in the open defn list
C     (D) An offset pointer to a continuation earlier in the list
C         (always to the last of any synonyms)
C     (E) Minus the number of a processing module to be called
C     (F) An offset pointer to zero, denoting a continuation return
C All names are packed and other items are offset by -25000 so as
C to be distinguishable from names
C
C The key list contains for each key -
C (1) The key name; if the key is numerical and has no default, then
C     no second item appears; otherwise..
C (2) An offset pointer to the default expression in the string list
C Names are packed and pointers offset as in the command list
C
C The open defn list contains for each open defn -
C (1) A four decimal digit integer, the digits of which are -
C     (A) An integer (1-3) selecting one of LP1/2/3 for assignment
C     (B) An opening mode (1 for old, 2 for new)
C     (C) An integer (0-3) indicating one of LP1/2/3 (if any) for use
C         in setting size/class defaults
C     (D) An integer (0-1) flagging a general class change vis a vis
C         item (C)
C (2) A pointer (not offset) to the picture number expression in the
C     string list
C
C The string list contains for each expression string
C (1) The number of characters in the string (which may be zero)
C (2) The characters themselves (internal code), unpacked so that
C     SEMEXP can operate on them directly
C
C The named macro list consists of a series of macro definitions,
C followed by a terminating zero; each definition has the form
C     (1) Packed macro name
C     (2) Number of characters in replacement text
C     (3) Replacement text characters (internal code)
C
C The four numbers at the beginning of the array are
C (1) The last element of the command list
C (2) The last element of the key list
C (3) The 1st element of the 1st command to be logged
C (4) The 1st element of the named macro list
C
C Internally to this program various lists are stored separately.
C The command list is chained as above with command/synonym names
C followed by an offset pointer to the next c.d. and a series of
C pointers to items in the other lists, classification being made
C through various offsets as follows:
C         0 => jump (points back into same array)
C      5000 => key
C     10000 => open request
C     15000 => option
C and processing module numbers appearing negated
C
      INCLUDE 'ICSET'
C
C ****** CHANGE ******
C The MAX parameters determine the size of various workspace arrays, and
C the initial values are more than adequate for most users; any eventual
C run-time overflow generates an error message indicating which one
C needs to be increased
C
      INTEGER MAXCOM,MAXOPT,MAXKEY,MAXSTR,MAXOPD,MAXERN,MAXMAC
      PARAMETER (MAXCOM=2800,MAXOPT=600,MAXKEY=1000,MAXSTR=500)
      PARAMETER (MAXOPD=30,MAXERN=300,MAXMAC=300)
C
C LNTEXT controls the number of characters to which lines can be
C continued, and is unlikely to need altering in normal practice
C
      INTEGER LNTEXT
      PARAMETER (LNTEXT=512)
C
C INPUT is the unit assigned to the command description document
C
      INTEGER INPUT
      PARAMETER (INPUT=5)
C
C OUTPUT is the unit to output generated code on
C
      INTEGER OUTPUT
      PARAMETER (OUTPUT=7)
C
C TERM2 is the terminal output unit (verification, errors)
C
      INTEGER TERM2
      PARAMETER (TERM2=6)
C
C ****** ****** ******
      INTEGER*4 ZERO,ONE
      PARAMETER (ZERO=0,ONE=1)
C
C Packed names
C
      INTEGER NEND,NLP2,NOPEN,NNEW,NLOG,NOLD,NVER,NOPT,NKEY,NSTR
      INTEGER NOPD,NERN,NMAC
      PARAMETER (NEND=8564,NLP2=19872,NOPEN=24645)
      PARAMETER (NNEW=22623,NLOG=19807,NOLD=24484,NVER=-3419,NOPT=24660)
      PARAMETER (NKEY=17825,NSTR=31218,NOPD=24644,NERN=8734,NMAC=20843)
C
C MXGOTO is the maximum number of elements per computed GOTO
C (including limitations on continuation lines)
C
      INTEGER MXGOTO
      PARAMETER (MXGOTO=241)
C
C MOSTOP & MOSTKE are (arbitrary, but fixed) limits enforced on the
C number of options and keys any single command may have.  These must
C not exceed Semper's own limits determined by the parameters MAXOPT
C and MAXKEY in PARAMS.  Allowance also has to be made for general keys
C and options which depend on the Semper parameters NGOPTS and NGKEYS
C and for the presence of a count in the local syntax arrays.  This
C leaves us with the following constraints for MOSTOP and MOSTKE:
C
C    MOSTOP <= MAXOPT - NGOPTS - 1
C    MOSTKE <= MAXKEY - NGKEYS - 1
C
C Note that these limits do not take into account keys and options
C defined in syntax routines which a command may invoke, because SEMGEN
C does not count these when checking against MOSTOP and MOSTKE.  If
C Semper's run-time limit is exceeded, Semper will simply refuse to
C execute the command and output error 84 instead.
C
      INTEGER MOSTOP,MOSTKE
      PARAMETER ( MOSTOP = 50 - 8 - 1, MOSTKE = 50 - 0 - 1 )
C
      INTEGER LNBLNK
C
      LOGICAL SEMGE2,SYNON,JMPDEF,OPDEF
      INTEGER COMM(MAXCOM),OPTION(MAXOPT),KEY(MAXKEY),NULLS(MAXKEY)
      INTEGER STRING(MAXSTR),OPDS(5,MAXOPD),MACRO(MAXMAC)
      CHARACTER*11 ERNAME(MAXERN),TNAME
      INTEGER OPDV(5),A1NAME(3),SPTR(5),TEXT(LNTEXT),STR(80)
      INTEGER VNAME,PTR,CH,TLEN,STRPTR,KPIND,KPTR,KPRMAX
      PARAMETER (KPRMAX=45)
      INTEGER KEEP(KPRMAX),HSPACE
C
      CHARACTER*80 FILENM
C
      REAL V,X
      INTEGER ICMND,IOPT,IKEY,ISTR,IERNS,IOPDS,IMAC,LPTR,NCMD
      INTEGER I,IC,IND,J,K,L,M,KV,NV,KLINK,NLINK,NAME,NPTR,NULDEF
      INTEGER KKEY,LENDS,MOPT,MKEY,MOPDS,MROUT,MSTR,MMAC
      INTEGER INLEN,LENGTH
      EQUIVALENCE (SPTR(2),ICMND),(SPTR(3),MKEY)
      EQUIVALENCE (SPTR(5),MMAC),(NULLS,TEXT)
C      DATA HSPACE/' '/
      HSPACE=ICHAR(' ')
C
C Initialise
C
      ICMND = 0
      IOPT = 0
      IKEY = 0
      ISTR = 0
      IERNS = 0
      IOPDS = 0
      IMAC = 0
      VNAME = 0
      SPTR(1) = 4
      LPTR = 0
      SPTR(4) = 0
      NCMD = 0
      SYNON = .FALSE.
C
      CALL GETCL(FILENM)
      I = LNBLNK(FILENM)
      IF (I .NE. 0) THEN
         J = 1
   10    IF (J .LT. I .AND. FILENM(J:J) .EQ. ' ') THEN
            J = J + 1
            GOTO 10
         ENDIF
         IF (J .NE. 1) FILENM = FILENM(J:)
      ELSE
C
C No filename given on command line
C
         FILENM='semper.syn'
      ENDIF
      OPEN(INPUT,FILE=FILENM,ERR=820,
     +     STATUS='OLD')
      REWIND(INPUT,ERR=830)
      OPEN(OUTPUT,FILE='semper.f',STATUS='OLD',ERR=30)
      CLOSE(OUTPUT,STATUS='delete',ERR=850)
C
   30 OPEN(OUTPUT,FILE='semper.f',STATUS='UNKNOWN',ERR=850)
C
C Phase 1: descriptor decoding
C ----------------------------
C
C Print header
C
      WRITE (TERM2,940) 'Semper 7 Beta System Generation'
      WRITE (TERM2,940) '-------------------------------'
C
C Read input line
C
   60 PTR=1
   70 INLEN=PTR+79
      IF (INLEN.GT.LNTEXT) GOTO 760
      READ (INPUT,860,END=840) (TEXT(I),I=PTR,INLEN)
C     Debug Line *** LDM ***
C      write(6,860)(TEXT(I),I=PTR,INLEN)
      CALL A1CONV(TEXT(PTR),80)
C
C Trap empty line
C
      IF (SEMGE2(5,TEXT,INLEN,PTR,X,K)) GOTO 60
C
C Continuation line (last char = '+')?
C
      IF (TEXT(INLEN).EQ.KPLUS) THEN
C
C Yes: overwrite it and fetch another line
C
         TEXT(INLEN)=KSPACE
         PTR=INLEN+1
         GOTO 70
      ENDIF
C
C Classify first item on line
C
      PTR=1
      JMPDEF=.FALSE.
C
C If first char is !, the line is a comment
C
      IF (SEMGE2(0,TEXT,INLEN,PTR,V,CH)) CONTINUE
      IF (CH.EQ.KPLING) GOTO 60
C
C If @, a macro definition
C
      IF (CH.EQ.KAT) GOTO 400
C
C Otherwise, there should be a name
C
      IF (SEMGE2(1,TEXT,INLEN,PTR,V,VNAME)) CONTINUE
      IF (VNAME.EQ.0) GOTO 720
C
C Otherwise, the name introduces a new c.d.
C
C Establish new command: has it been defined previously?
C ---------------------
C
      SYNON=.FALSE.
      IF (LPTR.EQ.0) GOTO 100
   80 I=1
   90 K=COMM(I)
C
C Double test to avoid possible overflow..
C
      IF (K .LT. 0) THEN
         IF (K .LT. -20000) THEN
            I = K + 25000
            GOTO 90
         ENDIF
      ENDIF
      IF (K .NE. 0) THEN
         IF (K.EQ.VNAME) GOTO 730
         I=I+1
         GOTO 90
      ENDIF
C
C No: count it (unless name begins with $)..
C
  100 IF (VNAME.GE.-11200.AND.VNAME.NE.NEND) NCMD=NCMD+1
C
C ..and continue chain
C
      IF (.NOT.SYNON) ICMND=ICMND+1
      IF (ICMND.GE.MAXCOM) GOTO 610
      IF (.NOT.SYNON.AND.LPTR.NE.0) COMM(LPTR)=ICMND-25000
      COMM(ICMND)=VNAME
      ICMND=ICMND+1
      COMM(ICMND)=0
      LPTR=ICMND
C
C Any (more) synonyms?
C
      IF (SEMGE2(0,TEXT,INLEN,PTR,V,CH)) GOTO 380
      IF (CH.EQ.KCOMMA) THEN
         PTR=PTR+1
         IF (SEMGE2(1,TEXT,INLEN,PTR,V,VNAME)) CONTINUE
         SYNON=.TRUE.
         GOTO 80
      ENDIF
C
C Decode rest of line
C
  110 IF (SEMGE2(0,TEXT,INLEN,PTR,V,CH)) GOTO 380
      IF (CH.EQ.KAKET.OR.CH.EQ.KCOLON) PTR=PTR+1
C
C Note current pointer to enable subsequent recovery of 6 char name
C
      NPTR=PTR
      IF (SEMGE2(1,TEXT,INLEN,PTR,V,NAME)) CONTINUE
C
C Classify item: jump / module call / open defn / key / option
C
      OPDEF=.FALSE.
      IF (CH.EQ.KAKET) GOTO 270
      IF (NAME.EQ.0) GOTO 720
      IF (CH.EQ.KCOLON) GOTO 230
      IF (PTR.LE.INLEN) THEN
         CH = TEXT(PTR)
         IF (NAME.EQ.NOPEN .AND. CH.EQ.KBRA) GOTO 310
         IF (CH.EQ.KEQUAL) GOTO 150
      ENDIF
C
C Option found: already stored?
C ------------
C
      IF (IOPT.NE.0) THEN
         DO 120 I=1,IOPT
            IF (OPTION(I).EQ.NAME) GOTO 130
  120    CONTINUE
      ENDIF
C
      IF (IOPT.GE.MAXOPT) GOTO 620
      IOPT=IOPT+1
      I=IOPT
      OPTION(I)=NAME
  130 IF (ICMND.GE.MAXCOM) GOTO 610
      ICMND=ICMND+1
      COMM(ICMND)=I+15000
      GOTO 110
C
C Key found: read expression string for default to temporary store
C ---------
C (this code also used for open requests)
C
  140 PTR=PTR+1
  150 TLEN=0
  160 PTR=PTR+1
      IF (PTR.LE.INLEN) THEN
         CH=TEXT(PTR)
C
C Force string chars to lower case
C
         IF (CH.GE.KUCA.AND.CH.LE.KUCZ) CH=CH+32
         IF (CH .NE. KSPACE) THEN
            TLEN=TLEN+1
            STR(TLEN)=CH
            GOTO 160
         ENDIF
      ENDIF
C
C Trap zero length defaults
C
      IF (TLEN.EQ.0) THEN
C
C No default: code internally as zero pointer
C
         IF (OPDEF) GOTO 720
         STRPTR=0
         GOTO 200
      ENDIF
C
C Establish string STR(LLEN): if it exists already, simply set
C pointer appropriately; otherwise, add it to list first
C
      STRPTR=1
  170 IF (STRPTR.GT.ISTR) THEN
C
C String not stored yet: store it at STRPTR
C
         IF (ISTR+TLEN.GE.MAXSTR) GOTO 640
         STRPTR=ISTR+1
         ISTR=STRPTR
         STRING(STRPTR)=TLEN
         IF (TLEN.NE.0) THEN
            DO 180 I=1,TLEN
               ISTR=ISTR+1
               STRING(ISTR)=STR(I)
  180       CONTINUE
         ENDIF
      ELSE
         L=STRING(STRPTR)
         IF (L.NE.TLEN) THEN
            STRPTR = STRPTR + L + 1
            GOTO 170
         ENDIF
C
C Length matched; now contents
C
         IF (L.NE.0) THEN
            K=STRPTR
            DO 190 I=1,L
               K=K+1
               IF (STRING(K).NE.STR(I)) THEN
                  STRPTR = STRPTR + L + 1
                  GOTO 170
               ENDIF
  190       CONTINUE
         ENDIF
C
C STR matched with an existing string
C
      ENDIF
C
C If storing number for open defn, quit
C
      IF (OPDEF) GOTO 330
C
C Establish key NAME with pointer to default string at STRPTR
C if it exists already, simply set pointer appropriately; otherwise
C add it to list first
C
  200 IF (IKEY.NE.0) THEN
         DO 210 I=1,IKEY,2
            IF (KEY(I).NE.NAME) GOTO 210
            IF (KEY(I+1).EQ.STRPTR) GOTO 220
  210    CONTINUE
C
C No: make new entry
C
         IF (IKEY.GE.MAXKEY) GOTO 630
      ENDIF
      I=IKEY+1
      IKEY=I+1
      KEY(I)=NAME
      KEY(IKEY)=STRPTR
C
C Insert pointer to this key in command list
C
  220 IF (ICMND.GE.MAXCOM) GOTO 610
      ICMND=ICMND+1
      COMM(ICMND)=I+5000
      GOTO 110
C
C Processing module call found: read name to local store
C ----------------------------
C Fault module name with more than 6 characters
C
  230 IF (PTR-NPTR.GT.6) GOTO 750
C
      IC=0
      TNAME=' '
      DO 240 I=NPTR,PTR-1
         CH=TEXT(I)
         IF (CH.GE.KLCA.AND.CH.LE.KLCZ) CH=CH-32
         IC=IC+1
         TNAME(IC:IC)=CHAR(CH)
  240 CONTINUE
C
C Fault module name not terminated by space or string '(SYN)'
C
      IF (TEXT(PTR).EQ.KBRA) THEN
         PTR=PTR+1
         IF (TEXT(PTR).NE.KUCS .AND. TEXT(PTR).NE.KLCS) GOTO 750
         PTR=PTR+1
         IF (TEXT(PTR).NE.KUCY .AND. TEXT(PTR).NE.KLCY) GOTO 750
         PTR=PTR+1
         IF (TEXT(PTR).NE.KUCN .AND. TEXT(PTR).NE.KLCN) GOTO 750
         PTR=PTR+1
         IF (TEXT(PTR).NE.KKET) GOTO 750
         PTR=PTR+1
         TNAME(IC+1:IC+5)='(SYN)'
      ELSE
         IF (TEXT(PTR).NE.KSPACE) GOTO 750
      ENDIF
C
C Call already stored?
C
      DO 250 I=1,IERNS
         IF (TNAME.EQ.ERNAME(I)) GOTO 260
  250 CONTINUE
C
C No: make new entry
C
      IF (IERNS.GE.MAXERN) GOTO 660
      IERNS=IERNS+1
      ERNAME(IERNS)=TNAME
      I=IERNS
C
C Yes: add module call to command list
C
  260 IF (ICMND.GE.MAXCOM) GOTO 610
      ICMND=ICMND+1
      COMM(ICMND)=-I
      GOTO 110
C
C Jump found: return?
C
  270 IF (NAME.NE.0) THEN
C
C Jump: check destination exists
C
         JMPDEF=.TRUE.
         I=1
  280    K=COMM(I)
C
C Double test again..
C
         IF (K .EQ. 0) GOTO 710
         IF (K .LT. 0) THEN
            IF (K.LT.-20000) THEN
               I=K+25000
               GOTO 280
            ENDIF
         ENDIF
         IF (K.NE.NAME) THEN
            I=I+1
            GOTO 280
         ENDIF
C
C Advance destination to last synonym
C Double test again..
C
  290    IF (COMM(I+1).LT.0) THEN
            IF (COMM(I+1).LT.-20000) GOTO 300
         ENDIF
         I=I+1
         GOTO 290
      ENDIF
C
C Return
C
      I=0
C
C Check no jump earlier in c.d.
C
      IF (JMPDEF) GOTO 740
C
C Add jump to command list
C
  300 IF (ICMND.GE.MAXCOM) GOTO 610
      ICMND=ICMND+1
      COMM(ICMND)=I
      IF (PTR.GT.INLEN) GOTO 380
      GOTO 110
C
C Open request found: copy parameters to local vector
C ---------------------
C
  310 OPDEF=.TRUE.
C
C Copy open vector to temporary store
C
      DO 320 J=1,5
         OPDV(J)=0
  320 CONTINUE
      PTR=PTR+1
C
C Get LPN
C
      IF (SEMGE2(1,TEXT,INLEN,PTR,V,NAME)) GOTO 720
      I=NAME-NLP2
      IF (IABS(I).GT.1) GOTO 720
      OPDV(1)=I+2
C
C Get disp (OLD or NEW)
C
      PTR=PTR+1
      IF (SEMGE2(1,TEXT,INLEN,PTR,V,NAME)) CONTINUE
      IF (PTR.GT.INLEN) GOTO 720
      J=0
      IF (NAME.EQ.NOLD) THEN
         J=1
         IF (TEXT(PTR).NE.KKET) GOTO 720
      ENDIF
      IF (NAME.EQ.NNEW) J=2
      IF (J.EQ.0) GOTO 720
      OPDV(2)=J
      IF (TEXT(PTR).NE.KKET) THEN
         PTR=PTR+1
C
C Get any comparison LPN
C
         IF (SEMGE2(1,TEXT,INLEN,PTR,V,NAME)) CONTINUE
         IF (PTR.GT.INLEN) GOTO 720
         I=NAME-NLP2
         IF (IABS(I).GT.1) GOTO 720
         OPDV(3)=I+2
         IF (TEXT(PTR).NE.KKET) GOTO 720
      ENDIF
C
C Read expression for picture number
C
      GOTO 140
C
C Return from storing number expression
C
  330 OPDV(5)=STRPTR
C
C Open request already stored?
C
      IF (IOPDS.NE.0) THEN
         DO 350 I=1,IOPDS
            DO 340 K=1,5
               IF (OPDV(K).NE.OPDS(K,I)) GOTO 350
  340       CONTINUE
            GOTO 370
  350    CONTINUE
      ENDIF
C
C No: store it
C
      IF (IOPDS.GE.MAXOPD) GOTO 650
      IOPDS=IOPDS+1
      I=IOPDS
      DO 360 K=1,5
         OPDS(K,IOPDS)=OPDV(K)
  360 CONTINUE
C
C Insert pointer to this open request in command list
C
  370 IF (ICMND.GE.MAXCOM) GOTO 610
      ICMND=ICMND+1
      COMM(ICMND)=I+I+9999
      GOTO 110
C
C Command descriptor post-processing: check number of items
C ----------------------------------
C
  380 MOPT=0
      MKEY=0
      MOPDS=0
      MROUT=0
      KV=LPTR+1
      NV=ICMND+1
      KLINK=0
      NLINK=0
C
C Pick up next item
C
  390 IF (KV.LT.NV) THEN
         K=COMM(KV)
C
C Sort
C
         IF (K.LT.0) THEN
C
C Module
C
            MROUT=MROUT+1
         ELSE IF (K.EQ.0) THEN
C
C Return
C
            KV=KLINK
            NV=NLINK
         ELSE
            IF (K.LE.5000) THEN
C
C Continuation
C
               KLINK=KV
               NLINK=NV
               KV=K+1
               NV=COMM(KV)+25000
            ELSE
C
C Option, key or open request
C
               K=K/5000
               IF (K .LT. 2) THEN
                  MKEY=MKEY+1
               ELSE IF (K .EQ. 2) THEN
                  MOPDS=MOPDS+1
               ELSE
                  MOPT=MOPT+1
               ENDIF
            ENDIF
         ENDIF
         KV=KV+1
         GOTO 390
      ENDIF
C
C Items for last command now counted: check totals
C
      IF (MOPT.GT.MOSTOP) GOTO 770
      IF (MKEY.GT.MOSTKE) GOTO 780
      IF (MOPDS.GT.3) GOTO 790
      IF (MROUT.GT.1) GOTO 800
C
C command OK: go back for next one
C
      IF (VNAME.EQ.NEND) GOTO 420
      GOTO 60
C
C Macro definition found
C ----------------------
C Read macro name
C
  400 PTR=PTR+1
      IF (SEMGE2(1,TEXT,INLEN,PTR,X,VNAME)) GOTO 720
      IF (VNAME.EQ.0) GOTO 720
C
C Establish length of macro text
C
      IF (SEMGE2(0,TEXT,INLEN,PTR,X,CH)) GOTO 720
      TLEN=INLEN-PTR+1
C
C Will it fit?
C
      IF (IMAC+TLEN+2.GT.MAXMAC) GOTO 670
C
C Yes: add name, length and text to MACRO
C
      IMAC=IMAC+2
      MACRO(IMAC-1)=VNAME
      MACRO(IMAC)=TLEN
      DO 410 I=PTR,INLEN
         IMAC=IMAC+1
         MACRO(IMAC)=TEXT(I)
  410 CONTINUE
      GOTO 60
C
C END found: processing of command descriptors complete
C
C Count null key defaults
C
  420 K=0
      DO 430 I=1,IKEY,2
         NULLS(I)=K
         IF (KEY(I+1).EQ.0) K=K+1
  430 CONTINUE
C
      NULDEF=K
C
      IKEY=IKEY-NULDEF
      ICMND=ICMND+4
      LENDS=ICMND+IKEY+IOPDS+IOPDS+ISTR+IMAC
      IF (IMAC.NE.0) LENDS=LENDS+1
C
C Output main program
C -------------------
C
      WRITE (OUTPUT,870)      'C Semper 6 plus main program'
      WRITE (OUTPUT,880)
C
C ****** CHANGE ******
C Amend output to generate INCLUDE statement suiting local compiler
C and file naming conventions
C
      WRITE (OUTPUT,890)            'INCLUDE ''COMMON'''
C
C ****** ****** ******
C
      WRITE (OUTPUT,880)
      WRITE (OUTPUT,890)            'CALL SEMHAN'
      WRITE (OUTPUT,880)
      WRITE (OUTPUT,870)      'C Final STOP (never reached ?)'
      WRITE (OUTPUT,880)
      WRITE (OUTPUT,890)            'STOP'
      WRITE (OUTPUT,880)
      WRITE (OUTPUT,890)            'END'
C
      WRITE (OUTPUT,870)      'C Semper 6 main loop'
      WRITE (OUTPUT,880)
      WRITE (OUTPUT,890)            'SUBROUTINE SEMAIN'
      WRITE (OUTPUT,880)
      WRITE (OUTPUT,890)            'LOGICAL SEMSIG'
      WRITE (OUTPUT,890)            'INTEGER SYNLEN'
      WRITE (OUTPUT,900) '      PARAMETER (SYNLEN=',LENDS,')'
      WRITE (OUTPUT,880)
      WRITE (OUTPUT,890)            'INTEGER SYN(SYNLEN)'
      WRITE (OUTPUT,890)            'INTEGER I'
      WRITE (OUTPUT,880)
C
C ****** CHANGE ******
C Amend output to generate INCLUDE statement suiting local compiler
C and file naming conventions
C
      WRITE (OUTPUT,890)            'INCLUDE ''COMMON'''
C
C ****** ****** ******
C
      WRITE (OUTPUT,880)
      WRITE (OUTPUT,870)      'C LASTVB, LASTKY, LOGST and MACST'
C
C Reset pointers to final absolute values
C
      MKEY=ICMND+IKEY
      MOPDS=MKEY+IOPDS+IOPDS
      MSTR=MOPDS+ISTR
      MMAC=IMAC
      IF (MMAC.NE.0) MMAC=MSTR+1
      KPIND=1
      KPTR=0
      DO 440 I=2,5
         KPTR=KPTR+1
         KEEP(KPTR)=SPTR(I)
  440 CONTINUE
      CALL SEMGE3(OUTPUT,KEEP,KPIND,KPTR)
      K=KPIND
      WRITE (OUTPUT,880)
      WRITE (OUTPUT,870)      'C Chained command list'
      WRITE (OUTPUT,880)
C
C Write command/item chain: command/synonym name first
C
  450 NAME=COMM(K-4)
C
C Double test again..
C
      IF (NAME .NE. 0) THEN
         IF (NAME .LT. 0) THEN
            IF (NAME.LT.-20000) THEN
C
C Write link to next command
C
               NV=NAME+25004
               GOTO 460
            ENDIF
         ENDIF
         L=1
         A1NAME(2)=HSPACE
         A1NAME(3)=HSPACE
         IF (SEMGE2(8,A1NAME,3,L,X,NAME)) CONTINUE
         IF (KPTR .NE. 0) CALL SEMGE3(OUTPUT,KEEP,KPIND,KPTR)
         WRITE (OUTPUT,910)  'C ',A1NAME
         KPTR = KPTR + 1
         KEEP(KPTR) = NAME
         IF (KPTR .EQ. KPRMAX) CALL SEMGE3(OUTPUT,KEEP,KPIND,KPTR)
         K=K+1
         GOTO 450
      ENDIF
      NV=0
  460 KKEY=NV-25000
      KPTR = KPTR + 1
      KEEP(KPTR) = KKEY
      IF (KPTR .EQ. KPRMAX) CALL SEMGE3(OUTPUT,KEEP,KPIND,KPTR)
C
C Last command?
C
  470 IF (K.NE.ICMND) THEN
         K=K+1
C
C This command finished?
C
         IF (K.EQ.NV) GOTO 450
C
C Next item number
C
         LPTR=COMM(K-4)
C
C Sort out category
C
         L=LPTR/5000
         I=LPTR-5000*L
C
C Reset pointers to final values
C
         IF (I.LE.0) I=I-4
         LPTR=SPTR(L+1)+I-25000
         IF (L.EQ.1) LPTR=LPTR-NULLS(I)
         IF (L.GT.2) LPTR=OPTION(I)
         KPTR = KPTR + 1
         KEEP(KPTR) = LPTR
         IF (KPTR .EQ. KPRMAX) CALL SEMGE3(OUTPUT,KEEP,KPIND,KPTR)
         GOTO 470
      ENDIF
C
C Write keyword list
C
      LPTR=0
      IF (KPTR .NE. 0) CALL SEMGE3(OUTPUT,KEEP,KPIND,KPTR)
      WRITE (OUTPUT,880)
      WRITE (OUTPUT,870)      'C Key list'
C
  480 IF (K.LT.MKEY) THEN
         K=K+1
         KPTR = KPTR + 1
         KEEP(KPTR) = KEY(LPTR+1)
         IF (KPTR .EQ. KPRMAX) CALL SEMGE3(OUTPUT,KEEP,KPIND,KPTR)
         LPTR=LPTR+2
         I=KEY(LPTR)
         IF (I .NE. 0) THEN
            L=I+MOPDS-25000
            K=K+1
            KPTR = KPTR + 1
            KEEP(KPTR) = L
            IF (KPTR .EQ. KPRMAX) CALL SEMGE3(OUTPUT,KEEP,KPIND,KPTR)
         ENDIF
         GOTO 480
      ENDIF
C
C Write open list
C
      LPTR=0
      IF (KPTR .NE. 0) CALL SEMGE3(OUTPUT,KEEP,KPIND,KPTR)
      WRITE (OUTPUT,880)
      WRITE (OUTPUT,870)      'C Open request list'
C
  490 IF (K.LT.MOPDS) THEN
         LPTR=LPTR+1
         L=0
         DO 500 I=1,4
            L=L*10+OPDS(I,LPTR)
  500    CONTINUE
         K=K+1
         KPTR = KPTR + 1
         KEEP(KPTR) = L
         IF (KPTR .EQ. KPRMAX) CALL SEMGE3(OUTPUT,KEEP,KPIND,KPTR)
         K=K+1
         L=OPDS(5,LPTR)+MOPDS
         KPTR = KPTR + 1
         KEEP(KPTR) = L
         IF (KPTR .EQ. KPRMAX) CALL SEMGE3(OUTPUT,KEEP,KPIND,KPTR)
         GOTO 490
      ENDIF
C
C Write expression string list
C
      LPTR=0
      IF (KPTR .NE. 0) CALL SEMGE3(OUTPUT,KEEP,KPIND,KPTR)
      WRITE (OUTPUT,880)
      WRITE (OUTPUT,870)      'C Expression list'
C
  510 IF (K.LT.MSTR) THEN
         LPTR=LPTR+1
         LENGTH=STRING(LPTR)
         K=K+1
         KPTR = KPTR + 1
         KEEP(KPTR) = LENGTH
         IF (KPTR .EQ. KPRMAX) CALL SEMGE3(OUTPUT,KEEP,KPIND,KPTR)
         IF (LENGTH.NE.0) THEN
            DO 520 I=1,LENGTH
               LPTR=LPTR+1
               K=K+1
               KPTR = KPTR + 1
               KEEP(KPTR) = STRING(LPTR)
               IF (KPTR .EQ. KPRMAX) CALL SEMGE3(OUTPUT,KEEP,KPIND,KPTR)
  520       CONTINUE
         ENDIF
         GOTO 510
      ENDIF
C
C Write macro definition list
C
      IF (IMAC.NE.0) THEN
         IF (KPTR .NE. 0) CALL SEMGE3(OUTPUT,KEEP,KPIND,KPTR)
         WRITE (OUTPUT,880)
         WRITE (OUTPUT,870)      'C Macro definition list'
C
         DO 530 I=1,IMAC
            K=K+1
            KPTR = KPTR + 1
            KEEP(KPTR) = MACRO(I)
            IF (KPTR .EQ. KPRMAX) CALL SEMGE3(OUTPUT,KEEP,KPIND,KPTR)
  530    CONTINUE
         K=K+1
         KPTR = KPTR + 1
         KEEP(KPTR) = 0
         IF (KPTR .EQ. KPRMAX) CALL SEMGE3(OUTPUT,KEEP,KPIND,KPTR)
      ENDIF
C
      IF (KPTR .NE. 0) CALL SEMGE3(OUTPUT,KEEP,KPIND,KPTR)
C
      WRITE (OUTPUT,880)
      WRITE (OUTPUT,870)   ' 1000 IF (SEMSIG(SSIGIN,RECORD)) GOTO 2000'
      WRITE (OUTPUT,870)   '      CALL SEMINX(SYN)'
      WRITE (OUTPUT,890)         'IF (NPMODU.EQ.0) GOTO 2000'
      WRITE (OUTPUT,890)         'IF (SEMSIG(SSIGPR,RECORD)) GOTO 2000'
C
C See if any processing modules called
C
      IF (IERNS.NE.0) THEN
C
C Write computed GOTO(s)
C
         WRITE (OUTPUT,880)
         K = IERNS
         M = 1
C
  540    CONTINUE
            L = MIN(K,MXGOTO)
            IF (L .NE. K) THEN
C
C Intermediate block - is it the first ?
C
               IF (M .EQ. 1) THEN
                  WRITE(OUTPUT,550) L+1
  550             FORMAT('      IF (NPMODU .LT.',I3,') THEN')
               ELSE
                  WRITE(OUTPUT,560) L+M
  560             FORMAT('      ELSE IF (NPMODU .LT.',I4,') THEN')
               ENDIF
            ELSE
C
C End block - was there one before ?
C
               IF (M .NE. 1) THEN
                  WRITE(OUTPUT,890)       'ELSE'
               ENDIF
            ENDIF
            WRITE (OUTPUT,890)         '   GOTO ('
C
            IND = M+L-1
            DO 580 I=M,IND-1,15
  570          FORMAT (A,15(I3,','))
               WRITE (OUTPUT,570) '     + ',(J,J=I,MIN(I+14,IND-1))
  580       CONTINUE
C
            IF (M .EQ. 1) THEN
               WRITE (OUTPUT,920)    '     + ',IND,'   ) ,NPMODU'
            ELSE
               WRITE (OUTPUT,920)    '     + ',IND
               WRITE (OUTPUT,920)    '     +    ) ,NPMODU - ',M-1
            ENDIF
            K = K - L
            M = M + L
            IF (K .NE. 0) GOTO 540
C
C Insert final ENDIF if required
C
         IF (M .GT. MXGOTO) THEN
            WRITE(OUTPUT,890)        'ENDIF'
         ENDIF
C
         WRITE (OUTPUT,890)          'GOTO 1000'
         WRITE (OUTPUT,880)
         WRITE (OUTPUT,870)    ' 2000 CALL SEMEND'
         WRITE (OUTPUT,890)          'STOP'
         WRITE (OUTPUT,880)
C
C Write processing module calls
C
         DO 590 I=1,IERNS
            IC = INDEX(ERNAME(I),' ')
            IF (IC.EQ.0) IC=12
            WRITE (OUTPUT,920) '  ',I,' CALL ',ERNAME(I)(1:IC-1)
            WRITE (OUTPUT,890)       'GOTO 1000'
  590    CONTINUE
      ENDIF
C
      WRITE (OUTPUT,880)
      WRITE (OUTPUT,890)             'END'
C
C Verify successful conclusion
C
      IKEY=(IKEY+NULDEF)/2
      IF (IMAC.NE.0) IMAC=IMAC+1
      WRITE (TERM2,930) FILENM(1:LNBLNK(FILENM)),NCMD,IKEY,NULDEF,
     +                  IOPT,IOPDS,IERNS,ISTR,IMAC,LENDS
C
C Invoke compiler etc.?
C
      CLOSE(OUTPUT)
      CLOSE(INPUT)
      STOP
C
C
  600 CONTINUE
C
C Error completion
C
      CLOSE (OUTPUT,STATUS='DELETE')
      CALL EXIT(ONE)
C
C Errors: workspace overflows first
C
  610 NAME=NVER
      GOTO 680
C
  620 NAME=NOPT
      GOTO 680
C
  630 NAME=NKEY
      GOTO 680
C
  640 NAME=NSTR
      GOTO 680
C
  650 NAME=NOPD
      GOTO 680
C
  660 NAME=NERN
      GOTO 680
C
  670 NAME=NMAC
C
  680 K=1
      IF (SEMGE2(8,A1NAME,3,K,X,NAME)) CONTINUE
      WRITE (TERM2,950)
     +    'Internal table overflow: recompile SEMGEN with larger MAX',
     +    A1NAME
C
C Error context printout
C
  690 WRITE (TERM2,940)
     +   'Error detected while processing descriptor line:'
  700 IF (SEMGE2(7,TEXT,INLEN,K,X,TERM2)) CONTINUE
      GOTO 600
C
C Syntax, bad structures etc.
C
  710 WRITE (TERM2,940) 'Jump destination not found'
      GOTO 690
C
  720 WRITE (TERM2,940) 'Syntax error'
      GOTO 690
C
  730 WRITE (TERM2,940) 'Command already defined'
      GOTO 690
C
  740 WRITE (TERM2,940) 'Illegal return jump'
      GOTO 690
C
  750 WRITE (TERM2,940) 'Bad processing module name'
      GOTO 690
C
  760 WRITE (TERM2,940) 'Too many continuation lines'
      GOTO 690
C
C Too many items in a c.d.
C
  770 WRITE (TERM2,940) 'Too many options'
      GOTO 810
C
  780 WRITE (TERM2,940) 'Too many keys'
      GOTO 810
C
  790 WRITE (TERM2,940) 'Too many open requests'
      GOTO 810
C
  800 WRITE (TERM2,940) 'More than one processing module call'
C
  810 A1NAME(2)=HSPACE
      A1NAME(3)=HSPACE
      K=1
      IF (SEMGE2(8,A1NAME,3,K,X,COMM(LPTR-1))) CONTINUE
      WRITE (TERM2,950) 'in the ',A1NAME,
     +                             ' descriptor before the line'
      GOTO 700
C
  820 CONTINUE
      WRITE (TERM2,940) 'Error opening syntax file ',FILENM(1:LNBLNK(FIL
     &ENM))
      GOTO 600
C
  830 CONTINUE
      WRITE (TERM2,940) 'Error rewinding syntax file ',FILENM(1:LNBLNK(F
     &ILENM))
      GOTO 600
C
  840 CONTINUE
      WRITE (TERM2,940) 'No END statement in syntax file',FILENM(1:LNBLN
     &K(FILENM))
      GOTO 600
C
  850 WRITE (TERM2,940) 'Error opening output file ',
     +                        'semper.f'
      GOTO 600
C
  860 FORMAT (80A1)
  870 FORMAT (A)
  880 FORMAT ('C')
  890 FORMAT (6X,A)
  900 FORMAT (A,I5,A)
  910 FORMAT (A,3A1)
  920 FORMAT (A,I3,2A)
  930 FORMAT (/1X,'System Generation Complete'/
     +        /1X,A,' contained:'
     +        /17X,I4,' Commands'
     +        /17X,I4,' Keys (',I3,' with null defaults)'
     +        /17X,I4,' Options'
     +        /17X,I4,' Open requests'
     +        /17X,I4,' Processing module calls'
     +        /17X,I4,' Expression string items'
     +        /17X,I4,' Macro definition items'
     +        //1X,'Total size of language array',I5,' items'/)
  940 FORMAT (1X,2A)
  950 FORMAT (1X,A,3A1,A)
C
C Copyright (C) 1987-1993:  Synoptics Ltd, All Rights Reserved
C
      END
C
C Semper 6 system generation subprogram SEMGE2
C
C - Stripped down version of SEMXA1
C
      LOGICAL FUNCTION SEMGE2(IOP,BUFFER,LENGTH,PTR,VALUE,IVALUE)
      INTEGER IOP,BUFFER(*),LENGTH,PTR,IVALUE
      REAL VALUE
C
      INCLUDE 'PARAMS'
C
      REAL RADIX,SIGN,V
      INTEGER S(4),CH,EX,CNV,FLD,INPTR,I,K,RADCH
      LOGICAL EXPON,FRPT,NST,BINARY,HEX,OCTAL,VALID
C
C
      INTEGER MAKEA1(96)
C      DATA MAKEA1/1H ,1H!,1H",1H#,1H$,1H%,1H&,1H',1H(,1H),1H*,1H+,
C     +   1H,,1H-,1H.,1H/,1H0,1H1,1H2,1H3,1H4,1H5,1H6,1H7,
C     +   1H8,1H9,1H:,1H;,1H<,1H=,1H>,1H?,1H@,1HA,1HB,1HC,
C     +   1HD,1HE,1HF,1HG,1HH,1HI,1HJ,1HK,1HL,1HM,1HN,1HO,
C     +   1HP,1HQ,1HR,1HS,1HT,1HU,1HV,1HW,1HX,1HY,1HZ,1H[,
C     +   '\\',1H],1H^,1H_,1H`,1Ha,1Hb,1Hc,1Hd,1He,1Hf,1Hg,
C     +   1Hh,1Hi,1Hj,1Hk,1Hl,1Hm,1Hn,1Ho,1Hp,1Hq,1Hr,1Hs,
C     +   1Ht,1Hu,1Hv,1Hw,1Hx,1Hy,1Hz,1H{,1H|,1H},1H~,1H /
C       Changed, LDM
      CHARACTER*1 CMAKEA1(96)
      DATA CMAKEA1/' ','!','"','#','$','%','&','''','(',')','*','+',
     +   ',','-','.','/','0','1','2','3','4','5','6','7',
     +   '8','9',':',';','<','=','>','?','@','A','B','C',
     +   'D','E','F','G','H','I','J','K','L','M','N','O',
     +   'P','Q','R','S','T','U','V','W','X','Y','Z','[',
     +   '\\',']','^','_','`','a','b','c','d','e','f','g',
     +   'h','i','j','k','l','m','n','o','p','q','r','s',
     +   't','u','v','w','x','y','z','{','|','}','~',' '/
        DO j=1,96
                MAKEA1(J)=ICHAR(CMAKEA1(J))
        ENDDO
C
      SEMGE2 = .FALSE.
      S(4) = 0
      IF (IOP .EQ. 8) THEN
         GOTO 100
      ELSE IF (IOP .EQ. 6 .OR. IOP .EQ. 7) THEN
         GOTO 130
      ELSE IF (IOP .EQ. 5) THEN
         GOTO 120
      ENDIF
C
C Input modes - check buffer length and ignore spaces
C
   10 VALUE = 0.
      IVALUE = 0
      IF (PTR .GT. LENGTH) GOTO 170
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
         GOTO 160
      ELSE IF (IOP .EQ. 1) THEN
C
C IOP=1: read name
C ----------------
C
         S(1) = 0
         S(2) = 0
         S(3) = 0
         K = 1
   20    CH = BUFFER(PTR)
C
C Test for alphanumeric (treating lc as uc) and map to R50
C
         IF (CH.GE.KLCA) CH = CH - (KLCA - KUCA)
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
            IF (CH.LT.KZERO .OR. CH.GT.KNINE .OR. K.EQ.1) GOTO 30
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
         IF (PTR.LE.LENGTH) GOTO 20
C
C Assemble in pseudo-R50 packing
C
   30    NST = S(1).GE.20
         IF (NST) S(1) = S(1)-20
         IVALUE = (S(1)*40 + S(2))*40 + S(3)
         IF (NST) IVALUE = -IVALUE-1
         GOTO 160
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
         GOTO 60
C
C Next character
C
   40    NST = .TRUE.
   50    PTR = PTR+1
         IF (PTR .GT. LENGTH) GOTO 70
C
   60    CH = BUFFER(PTR)
         IF (CH .GE. KLCA .AND. CH .LE. KLCZ) CH = CH - KLCA + KUCA
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
            GOTO 40
         ENDIF
C
         IF (PTR .EQ. INPTR) GOTO 90
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
                  GOTO 50
               ELSE IF (CH .EQ. KUCB .OR. CH .EQ. KLCB) THEN
                  BINARY = .TRUE.
                  RADIX = 2.
                  RADCH = KONE
                  GOTO 50
               ELSE IF (CH .EQ. KUCO .OR. CH .EQ. KLCO) THEN
                  OCTAL = .TRUE.
                  RADIX = 8.
                  RADCH = KSEVEN
                  GOTO 50
               ENDIF
            ENDIF
         ENDIF
C
C Exponent?
C
         IF (CH .NE. KUCE) GOTO 90
C
C Already defined?
C
         IF (.NOT.(EXPON .OR. BINARY .OR. HEX .OR. OCTAL)) THEN
            SIGN = 1.
            EXPON = .TRUE.
            NST = .FALSE.
            GOTO 50
         ENDIF
C
C Terminated
C
   70    EX = EX + K
   80    IF (EX .LT. 0) THEN
            V = V/10.
            EX = EX + 1
         ELSE IF (EX .GT. 0) THEN
            V = V*10.
            EX = EX - 1
         ELSE
            VALUE = V
            GOTO 160
         ENDIF
         GOTO 80
C
C + - .
C
   90    IF (CH .EQ. KPLUS .OR. CH .EQ. KMINUS) THEN
            IF (NST) GOTO 70
            IF (CH.EQ.KMINUS) SIGN=-1.
            GOTO 50
         ENDIF
         IF (CH .NE. KDOT) GOTO 70
         IF (EXPON .OR. FRPT) GOTO 70
         FRPT = .TRUE.
         GOTO 40
      ENDIF
C
C IOP=8: write name
C -------------------
C Dismantle pseudo-R50 packing
C
  100 I = IVALUE
      NST = I.LT.0
      IF (NST) I = -I-1
      S(2) = I/40
      S(3) = I - S(2)*40
      S(1) = S(2)/40
      S(2) = S(2) - S(1)*40
      IF (NST) S(1) = S(1)+20
C
C Map to internal code and deposit in buffer
C
      K = 1
  110 I = S(K)
C
C Exit on space
C
      IF (I .EQ. 0) GOTO 160
      IF (I .LT. 27) THEN
C
C Map alphabet (I+64 gives upper case if preferred)
C
         I = I + 96
      ELSE IF (I .GT. 27) THEN
C
C Map digits
C
         I = I + 18
      ELSE
C
C Map dollar
C
         I = 36
      ENDIF
C
C Buffer deposit routine
C
      I = MAKEA1(I-31)
      IF (PTR .GT. LENGTH) GOTO 170
      BUFFER(PTR) = I
      PTR = PTR + 1
      K = K + 1
      GOTO 110
C
C IOP=5: strip trailing spaces from BUFFER
C ----------------------------------------
C
  120 IF (LENGTH .LE. 0) GOTO 170
      IF (BUFFER(LENGTH) .EQ. KSPACE) THEN
         LENGTH = LENGTH - 1
         GOTO 120
      ENDIF
      GOTO 160
C
C IOP=6/7: map to A1, and output if 7
C -----------------------------------
C
  130 IF (LENGTH .NE. 0) THEN
         DO 140 I = 1,LENGTH
            K = BUFFER(I)
            BUFFER(I) = MAKEA1(K-31)
  140    CONTINUE
      ENDIF
      IF (IOP .EQ. 7) THEN
         IF (LENGTH .EQ. 0) THEN
            WRITE (IVALUE,150)
  150       FORMAT (1X,70A1)
         ELSE
            WRITE (IVALUE,150) (BUFFER(I),I=1,LENGTH)
         ENDIF
      ENDIF
C
  160 RETURN
C
C Buffer exhausted
C
  170 SEMGE2 = .TRUE.
      GOTO 160
C
C Copyright (C) 1987-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 system generation subprogram SEMGE3
C
C Writes syntax data
C
      SUBROUTINE SEMGE3(IUNIT,NAMES,NPTR,NUM)
      INTEGER IUNIT,NPTR,NUM,NAMES(NUM)
      INTEGER IND,IPTR
      IPTR = NPTR + NUM - 1
      IF (NUM .EQ. 1) THEN
   10    FORMAT('      DATA SYN(',I4,') /',I6,'/')
         WRITE(IUNIT,10) NPTR,NAMES(1)
      ELSE
   20    FORMAT ('      DATA (SYN(I),I=',I4,',',I4,')/'/
     +        10('     +',9(I6,:,',')/) )
   30    FORMAT ('     +/')
         WRITE(IUNIT,20) NPTR,IPTR,(NAMES(IND),IND=1,NUM)
         WRITE(IUNIT,30)
      ENDIF
      NPTR = NPTR + NUM
      NUM = 0
      RETURN
C
C Copyright (C) 1988-1992: Synoptics Ltd,  All Rights Reserved
C
      END
