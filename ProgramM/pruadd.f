C Semper 6 system module PRUADD
C
      LOGICAL FUNCTION PRUADD(LNAME,DEVICE)
C
C Finds a procedure heading, allocates a slot for the procedure
C then scans and copies the procedure.  The program source is read
C from the file opened on Fortran unit RDWRTU if LNAME is set to
C .TRUE., and from the terminal otherwise.
C
C Builds the program index in memory:
C   Each index slot consists of four values. Entry types are:
C      TIDLIN + Line Number + Relative block in text + Length of line
C        -- Pointer to start of a line
C      TIDFOR + Line Number + Offset + Variable
C        -- Pointer to start of FOR loop and variables used
C      TIDLOO + Line Number + Offset + Variable
C        -- Pointer to end of FOR loop and variables used
C      TIDLAB + Line Number + Offset + Label
C        -- Pointer to a label and the packed form
C
C   By keeping track of the matching TIDLOO on starting each FOR loop
C   it is possible to determine LABEL scope, action NEXT and BREAK, and
C   fault missing LOOPs before execution starts.
C
C   The index ends on an entry TIDEND.
C
C   Any commands requiring a macro expansion (@) are stored on a line
C   of their own.
C
      INTEGER IVAL,LNBLNK !,IPACK
      LOGICAL ABANDN,DISC,OPTNO,PRUCAS,PRUINF,PRUIND,PRUSLT,PRULAB
      LOGICAL PRUTXT,SEMBEE,SEMCON,SEMDIA,SEMINP,SEMKTX,SEMPRG
      LOGICAL SEMMAC,SEMXA1,VARSET
C
      INCLUDE 'COMMON'
C
      LOGICAL INLABL,INQUOT,SPACED,MACROD,MACROS
      LOGICAL RFOUND,NOMORE,REPLAC,LPROGR
C
      LOGICAL LNAME
      INTEGER DEVICE
      INTEGER*4 FRADDR,FRSIZE,I40,I4N,I4N1
      INTEGER*4 TXTBLK,LABBLK,NDSLOT
      INTEGER OLSLOT,FRSLOT,LABLEN,CHKFOR
      PARAMETER (I40=0)
C
      INTEGER NAMLEN,NAME(PRNACT),REPLEN,REPNAM(PRNACT),ENDCOM,SLTYPE
      CHARACTER*(PRNACT) MYNAME
      CHARACTER*6 STYLE
C
C Current command delimiters
C
      INTEGER COMBEG,COMEND
C
C Current input line and pointer, current text line being built in
C memory, pointer within current line. Current line number.
C
      INTEGER INTEXT(LNLINB),INPTR,PRTEXT(LNLINB)
      INTEGER TXTPTR,CURLIN
C
C Saved text pointer and text block number (relative to TXTBLK)
C
      INTEGER SAVPTR,OUTBLK
C
C Program index area, size of an index entry and maximum labels per
C buffer, current label slot number and saved position.
C
      INTEGER LABINF(LNINDX),LABENT,LABMAX,LABNUM,SAVNUM
      PARAMETER (LABENT=4,LABMAX=(LNINDX/LABENT)-1)
C
      EQUIVALENCE (LABINF,RB2),(INTEXT,RB4),(PRTEXT,RB5)
C
      REAL X
C
C End of line, last and next semi-colon and various temp vars.
C
      INTEGER LAST,SCLAST,SCNEXT,I,J,IVERB,IVAR,KEY,OLDKEY,UNIT,ADJQTE
C
      INTEGER KLCONV
      PARAMETER (KLCONV=KLCA-KUCA)
C
      CHARACTER*15 PROMPT
      CHARACTER*31 PROMPP
C
      PRUADD = .TRUE.
C
      IF ((INPUT .EQ. TERM1) .AND. INPLEV .NE. 0) THEN
         ERROR = 77
         IDMESS = 'Interactive ADD not allowed from within a program'
         GOTO 190
      ENDIF
C
      IF (LNAME) THEN
         UNIT = RDWRTU
      ELSE
         UNIT = TERM1
      ENDIF
C
      MACROS = VARSET(20843)
      LPROGR = VARSET(26335)
C
C Set up prompt strings
C
      PROMPT = 'Program Input->'
      PROMPP = 'Program name (as textstring): '
C
      REPLAC = LNAME .AND. LPROGR .AND. .NOT.MACROS
      IF (REPLAC) THEN
         REPLEN = PRNACT
         IF (SEMKTX(26335,PROMPP,
     +              REPNAM,REPLEN,.FALSE.)) GOTO 190
         IF (REPLEN .EQ. 0) GOTO 190
         IF (PRUCAS(REPNAM,REPLEN)) GOTO 190
         RFOUND = .FALSE.
      ELSE
         RFOUND = .TRUE.
      ENDIF
C
      NOMORE = .FALSE.
      LAST = 0
C
   10 LABNUM = 1
      SAVNUM = 0
      OUTBLK = 0
      TXTPTR = 1
      CURLIN = 1
      CHKFOR = 0
C
C If device is becoming full attempt a compress ?
C    Use logical function PRUCOM(DEVICE)
C
C If interactive then get program name into buffer
C
      IF (.NOT.LNAME) THEN
         LAST = PRNACT
         IF (SEMKTX(26335,PROMPP,
     +              INTEXT,LAST,.FALSE.)) GOTO 190
         IF (LAST .EQ. 0) GOTO 190
         INTEXT(LAST+1) = KBRA
         INTEXT(LAST+2) = KKET
         LAST = LAST + 2
      ENDIF
C
C Scan forward to procedure start -
C    a non-comment line containing:
C     name: comment OR name() comment
C
   20 IF (LAST .LE. 0) THEN
         LAST = LNLINB
         IF (SEMPRG(UNIT,INTEXT,LAST,.TRUE.,PROMPT)) GOTO 190
C
C Reflect line of input
C
         IF (LAST.EQ.0) THEN
            IF (SEMINP(' ')) GOTO 190
         ELSE
            CALL SEMCHS(RECORD,INTEXT,LAST)
            IF (SEMINP(RECORD(1:LAST))) GOTO 190
         ENDIF
C
         GOTO 20
      ENDIF
C
      INPTR=1
C
C Skip any leading spaces - INTEXT will contain some non-space
C characters since LAST is non-zero.
C
   30 IF (INTEXT(INPTR) .EQ. KSPACE) THEN
          INPTR = INPTR+1
          GOTO 30
      ENDIF
C
C Ignore possible comment lines
C
      IF (INTEXT(INPTR).EQ.KPLING) THEN
          LAST = 0
          GOTO 20
      ENDIF
C
C Let us see what we have here
C
      NAMLEN=0
   40 KEY = INTEXT(INPTR)
      INPTR = INPTR + 1
      IF (KEY.NE.KCOLON .AND. KEY.NE.KBRA) THEN
          IF (NAMLEN .LT. PRNACT) THEN
             NAMLEN = NAMLEN + 1
             IF (KEY.GE.KUCA .AND. KEY.LE.KUCZ) KEY = KEY + KLCONV
             NAME(NAMLEN) = KEY
          ENDIF
          IF (INPTR .LE. LAST) GOTO 40
          IF (NAMLEN.GE.3) THEN
             IF (NAME(1).EQ.KLCE .AND. NAME(2).EQ.KLCN .AND.
     +           NAME(3).EQ.KLCD) THEN
                PRUADD = .FALSE.
                GOTO 190
             ENDIF
          ENDIF
C
          ERROR = 77
          IDMESS = 'Program name missing'
          GOTO 190
      ELSE
          IF (KEY.EQ.KBRA) THEN
C
C Scan for KET
C
   50        IF (INTEXT(INPTR).NE.KKET) THEN
                INPTR = INPTR + 1
                IF (INPTR .LE. LAST) GOTO 50
                CALL SEMCHS(MYNAME,NAME,NAMLEN)
                RECORD = 'Warning: Missing bracket in '''
                RECORD(30:) = MYNAME(1:NAMLEN)//''''
                IF (SEMDIA(RECORD,NDIWAR)) GOTO 190
             ELSE
                INPTR = INPTR + 1
             ENDIF
         ENDIF
C
         IF (PRUCAS(NAME,NAMLEN)) GOTO 190
C
         IF (REPLAC) THEN
            IF (.NOT.RFOUND) THEN
               IF (REPLEN .NE. NAMLEN) GOTO 70
               DO 60 J = 1,REPLEN
                  IF (NAME(J) .NE. REPNAM(J)) GOTO 70
   60          CONTINUE
               RFOUND = .TRUE.
            ENDIF
         ENDIF
C
         IF (RFOUND) THEN
C
C Allocate and fill name into directory slot
C
            FRSLOT = 0
            OLSLOT = 0
            IF (PRUSLT(DEVICE,FRSLOT,OLSLOT,FRADDR,FRSIZE,
     +                 NDSLOT,NAMLEN,NAME)) GOTO 190
C
C Note start of free space for writing
C
            TXTBLK = FRADDR
            CALL SEMCHS(MYNAME,NAME,NAMLEN)
         ENDIF
C
C Skip procedure name
C
   70    IF (INPTR .LE. LAST) THEN
            J = 0
            DO 80 I = INPTR,LAST
               J = J + 1
               INTEXT(J) = INTEXT(I)
   80       CONTINUE
            LAST = LAST - INPTR + 1
         ELSE
            LAST = -1
         ENDIF
      ENDIF
C
C Now read the body of the procedure. The procedure is
C considered to end on:
C   (a) an END statement
C   (b) End Of File
C   (c) a new procedure heading being recognised (in this case
C       END will be inserted)
C
      SCLAST = 0
   90 IF (SCLAST .GT. LAST) THEN
         LAST = LNLINB
         IF (MACROS) THEN
            IF (NOMORE) THEN
               LAST = 3
               INTEXT(1) = KUCE
               INTEXT(2) = KUCN
               INTEXT(3) = KUCD
            ELSE
               LAST = 2
               X = REAL(IVAL(20843))
               IF (SEMXA1(4,INTEXT,LAST,LAST,X,I)) GOTO 190
               INTEXT(1) = KAT
               I = 1
               IF (SEMMAC(1,J,I,INTEXT,INTEXT,LAST,LNLINB)) GOTO 190
               LAST = LAST - 1
               NOMORE = .TRUE.
            ENDIF
         ELSE
            IF (SEMPRG(UNIT,INTEXT,LAST,.TRUE.,PROMPT)) GOTO 190
C
C Reflect line of input
C
            IF (LAST.EQ.0) THEN
               IF (SEMINP(' ')) GOTO 190
            ELSE
               CALL SEMCHS(RECORD,INTEXT,LAST)
               IF (SEMINP(RECORD(1:LAST))) GOTO 190
            ENDIF
         ENDIF
         IF (ABANDN(ERROR)) GOTO 190
         SCLAST = 0
         GOTO 90
      ENDIF
      INQUOT = .FALSE.
      MACROD = .FALSE.
      INPTR = SCLAST + 1
C
C Strip leading spaces
C
  100 IF (INPTR .LT. LAST) THEN
         IF (INTEXT(INPTR) .EQ. KSPACE) THEN
            INPTR = INPTR + 1
            GOTO 100
         ENDIF
      ENDIF
C
C Remember beginning of command
C
      ENDCOM = INPTR
      COMBEG = ENDCOM
      OLDKEY = KSPACE
      INLABL = .TRUE.
C
C Scan for end of command
C
  110 IF (ENDCOM .LE. LAST) THEN
         KEY = INTEXT(ENDCOM)
         IF (KEY .EQ. KQUOTE) INQUOT = .NOT.INQUOT
         IF (.NOT.INQUOT) THEN
            IF (KEY .EQ. KSEMIC) GOTO 120
            IF (OLDKEY .EQ. KCOLON) THEN
C
C Fudge to allow labels to be treated as entire command
C
               IF (INLABL) THEN
                  ENDCOM = ENDCOM - 1
                  GOTO 120
               ENDIF
            ELSE
               IF (.NOT.((KEY .EQ. KDOLLA .OR.
     +                   (KEY .GE. KUCA .AND. KEY .LE. KUCZ) .OR.
     +                   (KEY .GE. KLCA .AND. KEY .LE. KLCZ)) .OR.
     +                   (ENDCOM .NE. INPTR .AND.
     +                    (KEY .EQ. KCOLON .OR.
     +                    (KEY .GE. KZERO .AND. KEY .LE. KNINE))))) THEN
C
C Only characters allowed in a label are $,A-Z,a-z,and :,0-9 after first
C
                      INLABL = .FALSE.
               ENDIF
            ENDIF
C
C If macro present then must flag for later line-break
C
            IF (KEY .EQ. KAT) MACROD = .TRUE.
         ENDIF
         OLDKEY = KEY
         ENDCOM = ENDCOM + 1
         GOTO 110
      ENDIF
C
C End of command or line
C
  120 SCNEXT = ENDCOM
C
C Include the colon if this is a label
C
      IF ((OLDKEY .NE. KCOLON) .OR. .NOT.INLABL) ENDCOM = ENDCOM - 1
C
C Remove trailing spaces
C
  130 IF (ENDCOM .GT. COMBEG) THEN
         IF (INTEXT(ENDCOM) .EQ. KSPACE) THEN
            ENDCOM = ENDCOM - 1
            GOTO 130
         ENDIF
      ENDIF
      COMEND = ENDCOM
C
C If the line ended while still in a quoted string then flag it
C
      IF (INQUOT) THEN
         RECORD = 'Warning: unbalanced quotes in program '''
         RECORD(39:) = MYNAME(1:NAMLEN)//''''
         IF (SEMDIA(RECORD,NDIWAR)) GOTO 190
         ADJQTE = 1
      ELSE
         ADJQTE = 0
      ENDIF
C
      IF ((MACROD .AND. TXTPTR .NE. 1) .OR.
     +    (TXTPTR + ADJQTE + (COMEND+1-COMBEG)) .GE. LNLINB) THEN
C
C Flush text block and write text
C (PRULAB will adjust TXTPTR and CURLIN)
C
         IF (PRULAB(SAVNUM,LABINF,TIDLIN,CURLIN,OUTBLK,TXTPTR)) GOTO 190
         SAVNUM = LABNUM
         LABNUM = LABNUM + 1
         IF (PRUTXT(DEVICE,TXTBLK,FRSIZE,TXTPTR,OUTBLK,RFOUND)) GOTO 190
      ENDIF
C
C Write text from COMBEG to COMEND, including an initial ';' if not
C the first entry in the buffer and compressing multiple spaces
C
      IF (TXTPTR .NE. 1 .OR. COMEND .LT. COMBEG) THEN
         PRTEXT(TXTPTR) = KSEMIC
         TXTPTR = TXTPTR + 1
      ENDIF
      SAVPTR = TXTPTR
      SPACED = .FALSE.
      INQUOT = .FALSE.
      IF (COMEND .GE. COMBEG) THEN
         DO 140 I = COMBEG,COMEND
            KEY = INTEXT(I)
            IF (KEY .EQ. KQUOTE) INQUOT = .NOT. INQUOT
            IF (INQUOT) SPACED = .FALSE.
            IF (.NOT. SPACED .OR. KEY .NE. KSPACE) THEN
               PRTEXT(TXTPTR) = KEY
               TXTPTR = TXTPTR + 1
               SPACED = KEY .EQ. KSPACE .AND. .NOT. INQUOT
            ENDIF
  140    CONTINUE
         IF (SPACED) TXTPTR = TXTPTR - 1
      ENDIF
C
      IF (ADJQTE .NE. 0) THEN
         PRTEXT(TXTPTR) = KQUOTE
         TXTPTR = TXTPTR + 1
      ENDIF
C
C Check if a comment
C
      IF (INTEXT(INPTR) .EQ. KPLING) GOTO 150
      IF (SEMXA1(1,INTEXT,ENDCOM,INPTR,X,IVERB)) GOTO 150
      IF (ERROR .EQ. 25) ERROR = 0
      IF (ERROR .NE. 0) GOTO 150
C
C See if label
C
      IF (INPTR .LE. ENDCOM) THEN
         IF (INTEXT(INPTR) .EQ. KCOLON) THEN
C
C Label - if this is the first entry on  line, see if there is a
C         comment after and treat it as an old style procedure entry
C
            I = INPTR + 1
            IF (SEMXA1(0,INTEXT,ENDCOM,I,X,KEY)) KEY = 0
            IF (SCLAST .EQ. 0 .AND. KEY .EQ. KPLING) GOTO 160
C
C IVERB is label - remember line number, offset and label
C
            IF (PRULAB(LABNUM,LABINF,TIDLAB,
     +                 CURLIN,SAVPTR,IVERB)) GOTO 190
            IF (KEY .EQ. 0) GOTO 150
            SCNEXT = I - 1
            TXTPTR = SAVPTR + I - COMBEG
            GOTO 150
         ENDIF
      ENDIF
C
C Check for FOR or LOOP verbs
C
      IF ((IVERB .EQ. 10218 .AND. INPTR .LT. ENDCOM) .OR.
     +     IVERB .EQ. 19815) THEN
         I = INPTR + 1
         IVAR = 0
         IF (.NOT.SEMXA1(0,INTEXT,ENDCOM,I,X,KEY)) THEN
            IF (KEY .NE. KSEMIC) THEN
               IF (SEMXA1(1,INTEXT,ENDCOM,I,X,IVAR)) CONTINUE
            ENDIF
            IF (IVERB .EQ. 10218) THEN
               IF (IVAR .EQ. 0) GOTO 150
               IF (SEMXA1(0,INTEXT,ENDCOM,I,X,KEY)) GOTO 150
            ENDIF
         ENDIF
C
C IVERB is FOR or LOOP, IVAR is packed variable (or 0)
C  - remember line number, offset and label
C
         IF (IVERB .EQ. 10218) THEN
            IF (IVAR .EQ. 0) GOTO 150
            I = TIDFOR
            CHKFOR = CHKFOR + 1
         ELSE
            I = TIDLOO
            CHKFOR = CHKFOR - 1
         ENDIF
         IF (PRULAB(LABNUM,LABINF,I,CURLIN,SAVPTR,IVAR)) GOTO 190
      ENDIF
C
C Check for END
C
      IF (IVERB .EQ. 8564) GOTO 160
C
  150 IF (MACROD) THEN
C
C Flush macro text block
C (PRULAB will adjust TXTPTR and CURLIN)
C
         IF (PRULAB(SAVNUM,LABINF,TIDLIN,CURLIN,OUTBLK,TXTPTR)) GOTO 190
         SAVNUM = LABNUM
         LABNUM = LABNUM + 1
         IF (PRUTXT(DEVICE,TXTBLK,FRSIZE,TXTPTR,OUTBLK,RFOUND)) GOTO 190
      ENDIF
C
C Report errors (if any)
C
      IF (ERROR .NE. 0) THEN
         CALL SEMERR(RECORD)
         I = LNBLNK(RECORD)
         IF (I .NE. 0) THEN
            IF (SEMDIA(RECORD(1:I),NDIWAR)) GOTO 190
         ENDIF
         CALL SEMCTX(INTEXT,LNLINB,INPTR,RECORD(1:60))
         IF (SEMDIA(RECORD(1:60),NDIWAR)) GOTO 190
         IF (SEMDIA('... remainder of line ignored',NDIWAR)) GOTO 190
         IF (SEMBEE()) GOTO 190
         TXTPTR = SAVPTR
         IF (TXTPTR .GT. 2) TXTPTR = TXTPTR - 1
         LAST = 0
      ENDIF
C
      SCLAST = SCNEXT
      GOTO 90
C
C Here on end of procedure (END, xxx( or xxx: )
C
  160 CONTINUE
C
C Forget last text entered
C
      TXTPTR = SAVPTR
      IF (TXTPTR .GT. 2) TXTPTR = TXTPTR - 1
C
C Dump assumed line to file - check enough room first
C
      IF ((TXTPTR + 4) .GE. LNLINB) THEN
C
C Flush text block and write text
C (PRULAB will adjust TXTPTR and CURLIN)
C
         IF (PRULAB(SAVNUM,LABINF,TIDLIN,CURLIN,OUTBLK,TXTPTR)) GOTO 190
         SAVNUM = LABNUM
         LABNUM = LABNUM + 1
         IF (PRUTXT(DEVICE,TXTBLK,FRSIZE,TXTPTR,OUTBLK,RFOUND)) GOTO 190
      ENDIF
C
C Insert END
C
      IF (TXTPTR .NE. 1) THEN
         PRTEXT(TXTPTR) = KSEMIC
         TXTPTR = TXTPTR + 1
      ENDIF
      PRTEXT(TXTPTR) = KLCE
      PRTEXT(TXTPTR+1) = KLCN
      PRTEXT(TXTPTR+2) = KLCD
      TXTPTR = TXTPTR + 3
C
C Dump final text (if any)
C (PRULAB will adjust TXTPTR and CURLIN)
C
      IF (PRULAB(SAVNUM,LABINF,TIDLIN,CURLIN,OUTBLK,TXTPTR)) GOTO 190
      IF (PRUTXT(DEVICE,TXTBLK,FRSIZE,TXTPTR,OUTBLK,RFOUND)) GOTO 190
C
C Write label block - first add end marker
C
      IF (PRULAB(LABNUM,LABINF,TIDEND,0,0,0)) GOTO 190
      IF (CHKFOR .NE. 0) THEN
         RECORD = 'Warning: unmatched FOR-LOOPs in program '''
         RECORD(41:) = MYNAME(1:NAMLEN)//''''
         IF (SEMDIA(RECORD,NDIWAR)) GOTO 190
      ENDIF
C
      IF (RFOUND) THEN
C
         FRADDR = FRADDR + OUTBLK
C
         I4N = LABNUM
         I4N = LABNUM*LABENT
         I4N1 = I4N * LNINT
         I4N1 = 1 + ((I4N1 - 1)/LNBLK4)
         IF (I4N1 .GE. FRSIZE) THEN
            IDERR = DEVICE
            ERROR = 101
            GOTO 190
         ENDIF
C
C Number of label blocks in I4N1, number of integers in I4N
C
         LABBLK = FRADDR
         LABLEN = I4N1
         FRADDR = LABBLK + I4N1
         FRSIZE = FRSIZE - I4N1
C
         IF (DISC(2,DEVICE,I4N,LABINF,LABBLK,NFMINT,NFMINT)) GOTO 190
C
C Write the header
C
         SLTYPE = 2
         IF (PRUIND(2,DEVICE,FRSLOT,TXTBLK,LABBLK,SLTYPE,
     +                OUTBLK,LABLEN,NAMLEN,NAME)) GOTO 190
C
C Update free address and size
C
         IF (PRUINF(2,DEVICE,FRADDR,FRSIZE,NDSLOT)) GOTO 190
C
C Delete any old entry of the same name
C
         IF (OLSLOT.NE.0) THEN
            IF (PRUIND(1,DEVICE,OLSLOT,TXTBLK,LABBLK,SLTYPE,
     +                   OUTBLK,LABLEN,NAMLEN,NAME)) GOTO 190
            SLTYPE=1
            IF (PRUIND(2,DEVICE,OLSLOT,TXTBLK,LABBLK,SLTYPE,
     +                   OUTBLK,LABLEN,NAMLEN,NAME)) GOTO 190
         ENDIF
C
         IF (.NOT.OPTNO(-3419)) THEN
            CALL SEMCHS(MYNAME,NAME,NAMLEN)
C
            IF (OLSLOT.EQ.0) THEN
               STYLE = 'insert'
            ELSE
               STYLE = 'replac'
            ENDIF
C
            WRITE(RECORD,170) MYNAME(1:NAMLEN),STYLE,DEVICE
  170       FORMAT('Program ''',A,''' ',A,'ed on device',I3)
            IF (SEMCON(RECORD)) GOTO 190
         ENDIF
      ENDIF
C
      IF (IVERB .EQ. 8564) SCLAST = SCNEXT
      IF (SCLAST .NE. 0) THEN
         IF (SCLAST .LT. LAST) THEN
            DO 180 I = SCLAST,LAST
               INTEXT(I-SCLAST) = INTEXT(I)
  180       CONTINUE
            LAST = LAST - SCLAST
         ELSE
            LAST = 0
         ENDIF
         SCLAST = 0
      ENDIF
C
      IF (IVERB .NE. 8564 .OR. LNAME) THEN
         IF (.NOT.ABANDN(ERROR)) THEN
            IF (.NOT.(REPLAC .AND. RFOUND)) GOTO 10
         ENDIF
      ENDIF
C
  190 RETURN
C
C Copyright (C) 1987-1993:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 system module PRUTXT
C
      LOGICAL FUNCTION PRUTXT(DEVICE,TXTBLK,FRSIZE,TXTPTR,OUTBLK,ACTION)
C
      LOGICAL DISC
C
C Writes a text block to disc - checks for overflow
C
      INCLUDE 'COMMON'
C
      INTEGER DEVICE
      INTEGER*4 BLKN,N4,TXTBLK,FRSIZE
C
C Current text line being built in and current pointer within it.
C Current line number.
C
      INTEGER PRTEXT(LNLINB),TXTPTR
C
C Text block number (relative to TXTBLK) and blocks used by TXTPTR
C
      INTEGER OUTBLK,BLKCNT
C
C Flag to indicate if store to disc required
C
      LOGICAL ACTION
C
      EQUIVALENCE (PRTEXT,RB5)
      IF (ACTION) THEN
C
C Check for overflow
C
         BLKCNT = 1 + (TXTPTR-1)/LNBLK
         BLKN = BLKCNT
         IF (BLKN .GE. FRSIZE) THEN
            IDERR = DEVICE
            ERROR = 101
            PRUTXT = .TRUE.
         ELSE
            FRSIZE = FRSIZE - BLKN
C
C Call DISC using TXTPTR as size and OUTBLK as relative Block pointer
C
            N4 = TXTPTR
            BLKN = TXTBLK + OUTBLK
C
C Calculate new block counter etc. before DISC call
C
            OUTBLK = OUTBLK + BLKCNT
            TXTPTR = 1
            PRUTXT = DISC(2,DEVICE,N4,PRTEXT,BLKN,NFMINT,NFMBYT)
         ENDIF
      ELSE
C
C Dummy call
C
         TXTPTR = 1
         PRUTXT = .FALSE.
      ENDIF
      RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
