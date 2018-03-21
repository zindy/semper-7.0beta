C Semper 6 system module SEMINX
C
      SUBROUTINE SEMINX (SYN)
      INTEGER SYN(*)
C
C Command interpreter
C - provides intrinsic verbs and facilities
C - organises processing module calls
C
C LINBUF contains current input line (several commands in general)
C LASTSC points to the semi-colon before the current command
C COMLIM points to the last non-space in the current command
C NEXTSC to the next character after the current command
C LINLEN points to the last non-space in the line
C If LASTSC>LINLEN, LINBUF needs filling
C PTR scans through LINBUF item by item
C
C Max number of assumed keys ($$1,$$2..); can be reset freely here
C
      INCLUDE 'COMMON'
      INCLUDE 'EVENTS'
C
      INTEGER NASKYS
      PARAMETER (NASKYS=9)
C
      INTEGER IVAL,SEMFRM,SEMPPN,IPACK,LNBLNK
C     Change: LDM, July 2005
C     Ensure that gfortran does not think of range, unpack as intrinsics
      EXTERNAL RANGE, UNPACKF
      LOGICAL ABANDN,DISC,EQFLUS,FSFLUS,RANGE,SEMLU
      LOGICAL SEMBEE,SEMCOM,SEMDCR,SEMDEL
      LOGICAL SEMDIA,SEMENV,SEMEXP,SEMFVD,SEMJMP,SEMLIN,SEMMAC
      LOGICAL SEMOPN,SEMRET,SEMTBK,SEMTYP,SEMUNW,SEMXPL,SEMXA1,VARSET
      CHARACTER*3 UNPACKF
C
      REAL VX(9),X
      INTEGER REFMAX
      PARAMETER (REFMAX=80)
      INTEGER TEXT(80),KEYNME(3)
      LOGICAL USEDLP(3)
      INTEGER LPNS(3),PTR,CH,CLASS,FORM,REFLEN
      CHARACTER*(REFMAX) REFSTR
C
      INTEGER LPN,NAME,NCOL,NLAY,NROW,IASKEY,NPTR
      INTEGER I,J,K,L,N
C
      INTEGER LABNUM,LABPTR,LABENT
      PARAMETER (LABENT=4)
C
      INTEGER*4 I41
      PARAMETER (I41=1)
C
      LOGICAL MACFND,INQUOT,NOSYN,PANIC,FATAL,EXTVRB,SETRC
C
      CHARACTER*3 STRING
      CHARACTER*9 DIGITS
C
C Packed names
C
      INTEGER NNOA,NNOZ,NDLLR0
      PARAMETER (NNOA=23001,NNOZ=23026,NDLLR0=-12401)
C
      INTEGER OPTION(MAXOPT),KEY(MAXKEY),DEF(MAXKEY),OPENR(1+5*MAXOPE)
      INTEGER LSTKEY,LSTOPT,NOPENR
C
      EQUIVALENCE (RB4(1),OPTION,LSTOPT)
      EQUIVALENCE (RB4(1+MAXOPT),KEY,LSTKEY)
      EQUIVALENCE (RB4(1+MAXOPT+MAXKEY),DEF)
      EQUIVALENCE (RB4(1+MAXOPT+2*MAXKEY),OPENR,NOPENR)
      EQUIVALENCE (RB4(1+MAXOPT+2*MAXKEY+1+5*MAXOPE),TEXT)
      EQUIVALENCE (LPNS,LP1)
C
      DATA DIGITS / '123456789' /
C
      SETRC = .TRUE.
C
C Note external entry - controls action on error trapping etc.
C
      EXTVRB = NPMODU .NE. 0
C
C Clear fatal error flag
C
      FATAL = .FALSE.
C
C Nothing to reflect initially
C
      REFLEN = 0
C
C Disable automatic selection by SEMOPN
C
   10 SELOPN = .FALSE.
C
C PANIC is set only if there has already been an error reported on
C this re-entry, to avoid looping should there be errors in post-command
C processing
C
      PANIC = .FALSE.
C
C Effect command reflection if not already performed
C - this will not work for reentry from condition handlers on the VAX...
C
      IF (SEMCOM(REFSTR,REFLEN)) GOTO 10
C
C If fatal error, report error and terminate Semper session
C
      IF (ERROR .EQ. FATERR) THEN
         PTR = LASTSC - 1
         GOTO 290
      ENDIF
C
C Flush keyboard queue if break detected
C
      IF (ERROR .EQ. BRKERR) THEN
         IF (EQFLUS(MKEY)) THEN
            ERROR = 161
            IF (SEMDIA('** Error in keyboard flush **',NDIERR)) GOTO 10
            GOTO 10
         ENDIF
      ENDIF
C
C Error checking - xfer ERROR to RC, and ignore if trapped
C
      IF (SETRC) RC = REAL(ERROR)
      IF (ERROR .NE. 0) THEN
C
         IF (.NOT. EXTVRB) LASTSC = NEXTSC
C
         I = INPLEV
   20    IF (INTRAP(I) .GE. 0) THEN
            IF (ERROR .NE. INTRAP(I)) THEN
C
C Not trapped at this level - try the next one
C
               IF (I .NE. 0) THEN
                  INTRAP(I) = 0
                  I = I - 1
                  GOTO 20
               ENDIF
C
C Error not trapped - check here for Quit in response to page prompt
C
               IF (ERROR .EQ. PAGERR) THEN
                  ERROR = 0
                  GOTO 300
C
C Otherwise, move context pointer back to end of previous command and
C report error
C
               ELSE
                  PTR = LASTSC - 1
                  GOTO 290
               ENDIF
            ENDIF
         ENDIF
C
C Clear trap in case of error recursion
C
         TRAP = 0.0
         INTRAP(I) = 0
         CALL SEMERR(RECORD)
         IF (LNBLNK(RECORD) .GT. 0) TRPREC = RECORD
         IF (I .NE. INPLEV) THEN
            IF (SEMRET(I)) GOTO 10
         ENDIF
C
         ERROR = 0
      ENDIF
C
C Command post-processing
C -----------------------
C
C See if SEMSEL called
C
      IF (PICSEL.EQ.0) THEN
C
C If not, Update SELECT from OPLPN if nonzero, non-temp
C
         IF (OPLPN.NE.0) THEN
            IF (PICN(OPLPN).GT.0) THEN
               CALL SEMSZZ(1000*DEVN(OPLPN)+PICN(OPLPN))
            ENDIF
         ENDIF
C
C Otherwise, set SELECT if new picture number is defined
C
      ELSE IF (PICSEL.GT.0) THEN
         SELECT = PICSEL
      ENDIF
C
C Reset picture selection flags
C
      OPLPN = 0
      PICSEL = 0
C
C Delete temporary pictures (SEMDEL resets REQDTS)
C
   30 IF (REQDTS) THEN
         IF (SEMDEL(2,0)) GOTO 290
      ENDIF
C
C Delete range records
C
      IF (REQDRR) THEN
         REQDRR = .FALSE.
         DO 40 LPN=1,NLPS
            IF (WHEN(LPN).NE.0) THEN
               IF (WSTAT(LPN).GT.0) THEN
                  IF (RANGE(3,LPN)) GOTO 290
                  WSTAT(LPN) = 0
               ENDIF
            ENDIF
   40    CONTINUE
      ENDIF
C
C Flush disc(s)
C
      IF (REQDCF) THEN
         REQDCF = .FALSE.
         IF (DISC(3,-1,I41,RB1,I41,0,0)) GOTO 290
      ENDIF
C
C If necessary, flush display graphics buffer (FSFLUS resets REQFSF)
C
      IF (REQFSF) THEN
         IF (FSFLUS()) GOTO 290
      ENDIF
C
C Undo local variable settings - restore in reverse order, then unset
C
      IF (SEMUNW(VRBLEV)) GOTO 290
C
      ERROR = 0
C
C Abandon requested?
C
   50 IF (ABANDN(ERROR)) GOTO 10
C
C Find next command
C -----------------
C
      VERB = 0
C
C End of current line?
C
      IF (LASTSC .GE. LINLEN) THEN
C
C Fetch next line from current input unit
C
         LINLEN = LNLINB
         IF (SEMLIN(LINBUF,LINLEN,SYN)) THEN
            COMLIM = LINLEN
            PTR = COMLIM
            GOTO 290
         ENDIF
C
         IF (LINLEN.EQ.0) GOTO 50
C
C NOERCN notes conditions allowing context suppression in
C the event of a later error - suppress for now as common variables
C are not guaranteed correct
C
         NOERCN = .TRUE.
C
C MACFND => macro call found in current line
C
         MACFND = .FALSE.
         LASTSC = 0
      ENDIF
C
C If jumping then check for label in index
C
      IF (JMPDST .NE. 0) THEN
         IF (.NOT.SEMJMP(JMPDST,LABNUM,0,.TRUE.)) THEN
            LABPTR = LABNUM*LABENT
            IF (.NOT.SEMENV(LINDEV,LINSLT,LINDEX(LABPTR+2))) THEN
               LASTSC = LINDEX(LABPTR+3) - 1
               JMPDST = 0
            ENDIF
            GOTO 10
C
C If line longer than 'END' then skip it
C
         ELSE IF (LINLEN .GT. 3) THEN
            LINLEN = 0
            GOTO 10
         ENDIF
      ENDIF
C
C Scan LINBUF for semicolon, expanding macro calls
C
      REFLEN = 0
      INQUOT = .FALSE.
      PTR = LASTSC + 1
      COMLIM = PTR
C
C Set error handling flag to indicate internal actions
C
      EXTVRB = .FALSE.
C
C Allow context now?
C
      NOERCN = .FALSE.
C
   60 CH = LINBUF(COMLIM)
      IF (CH.EQ.KQUOTE) INQUOT=.NOT.INQUOT
      IF (.NOT.INQUOT) THEN
         IF (CH.EQ.KSEMIC) GOTO 70
         IF (CH.EQ.KAT) THEN
C
C Macro call found
C
            MACFND = .TRUE.
            IF (SEMMAC(1,J,COMLIM,SYN,LINBUF,LINLEN,LNLINB)) GOTO 10
            GOTO 60
         ENDIF
      ENDIF
      IF (REFLEN.LT.REFMAX) THEN
         REFLEN = REFLEN + 1
         REFSTR(REFLEN:REFLEN) = CHAR(CH)
      ENDIF
      COMLIM = COMLIM + 1
      IF (COMLIM.LE.LINLEN) GOTO 60
C
C End of line: set NOERCN if this command also the first on the
C line, provided operation is interactive and without macro
C expansions
C
      NOERCN = LASTSC.EQ.0 .AND. INPUT.EQ.TERM1 .AND. .NOT.MACFND
C
C NEXTSC points one char beyond current command
C
   70 NEXTSC = COMLIM
      COMLIM = COMLIM-1
C
C Prepare current command scan
C ----------------------------
C
C NPMODU = module to be called before next command processed
C
      NPMODU = 0
C
C ERROR = error code on re-entry
C
      ERROR = 0
C
C TRAP = code for error to be ignored on re-entry (-1 for all)
C
      TRAP = 0.0
      INTRAP(INPLEV) = 0
C
      SETRC = .FALSE.
C
C *** Formerly, TO was unset here; OK?
C
C Remove trailing space(s) from command
C
      IF (SEMXA1(5,LINBUF,COMLIM,I,X,J)) GOTO 270
C
C Prefix decoding
C ---------------
C Processing returns here after each prefix stripping
C
C Comment?
C
   80 IF (SEMXA1(0,LINBUF,COMLIM,PTR,X,CH)) GOTO 270
      IF (CH.EQ.KPLING) GOTO 270
C
C Get first name
C
      IF (SEMXA1(1,LINBUF,COMLIM,PTR,X,VERB)) GOTO 270
C
      SETRC = .TRUE.
C
      IF (ERROR.EQ.25) ERROR=0
      IF (ERROR.NE.0) GOTO 10
      CH=KSPACE
C
C Get next non-space and determine whether label/assignment
C
      I=PTR
      IF (SEMXA1(0,LINBUF,COMLIM,PTR,X,CH)) GOTO 90
C
C Label?
C
      IF (CH.EQ.KCOLON .AND. I.EQ.PTR) THEN
C
C Label processing
C ----------------
C Label found - resume command scan after prefix
C
         PTR=PTR+1
         GOTO 80
      ENDIF
C
C No label present: try next command if jumping (unless END!)
C
   90 IF (JMPDST.NE.0) THEN
         IF (VERB.EQ.8564) THEN
C
C Label not found
C
            ERROR = 7
            IDERR = JMPDST
C
C (Rewind offline units so that END trap will work again)
C
            IF (INPUT .NE. TERM1) REWIND INPUT
            GOTO 10
         ENDIF
C
C Kill reflection and forget command
C
         REFLEN = 0
         GOTO 270
      ENDIF
C
C Assignment?
C
      IF (CH .NE. KEQUAL) THEN
C
C Check for conditional now
C
         IF (VERB.NE.14640 .AND. VERB.NE.-2173) GOTO 110
C
C Conditional processing - IF/UNLESS present?
C ----------------------
C Evaluate expression
C
         IF (SEMEXP(LINBUF,COMLIM,PTR,X,.FALSE.)) GOTO 10
C
C Honour conditional
C
         IF (X .EQ. 0.) THEN
            NAME = -2173
         ELSE
            NAME = 14640
         ENDIF
C
C If condition true, strip prefix and revert to command scan
C
         IF (VERB .EQ. NAME) GOTO 80
C
C Condition false: kill reflection and forget command
C
         REFLEN = 0
         GOTO 270
      ENDIF
C
C Assignment processing - read value
C ---------------------
C
      PTR=PTR+1
      NAME=VERB
      L=9
      IF (SEMXPL(LINBUF,COMLIM,PTR,VX,L)) GOTO 10
      N=1
C
C Set variable
C
  100 IF (SEMLU(1,NAME,VX(N))) GOTO 10
      IF (NAME .EQ. -722) THEN
C
C Copy trap to stack
C
         INTRAP(INPLEV) = TRAP
      ENDIF
      N = N + 1
      IF (N.GT.L) GOTO 80
      X = VX(1)
C
C Generate associated name (suffix N) for further value
C
      IF (N.LE.2) THEN
         K = 1
         IF (SEMXA1(3,KEYNME,3,K,X,NAME)) CONTINUE
         IF (K .GT. 3) K = 3
      ENDIF
C
      KEYNME(K) = KZERO+N
      I = 1
      IF (SEMXA1(1,KEYNME,K,I,X,NAME)) CONTINUE
      GOTO 100
C
C Verb interpretation: search intrinsic list, then processing
C ----------------------------------------------------------
C
  110 CONTINUE
C
C Preserve command start for FOR loop handling etc.
C
      PREVSC = LASTSC
C
      IF (VERB .EQ. 8564) THEN
C
C END processing - special treatment is required for
C                  END in program or run file
C
         IF (INPLEV .EQ. 0 .AND. INPUT .EQ. TERM1) THEN
C
C Really is the end ! - Effect command reflection locally
C
            IF (SEMCOM(REFSTR,REFLEN)) GOTO 280
C
C Terminate the session (NPMODU = 0, SEMEND called in main program)
C
            GOTO 280
         ENDIF
C
C Force RETURN
C
         VERB = 29020
      ENDIF
C
C Dump current row in anticipation, except for P/PIXEL command and
C language element commands.
C
      IF (VERB .NE. 25600 .AND. VERB .NE. 25984 .AND.
     +    VERB .NE. 10218 .AND. VERB .NE. 19815 .AND.
     +    VERB .NE. 22624 .AND. VERB .NE. 3925 .AND.
     +    VERB .NE. 16853 .AND. VERB .NE. 29020) THEN
         IF (SEMDCR(0)) GOTO 10
      ENDIF
C
C Access processing verb list via SEMFVD
C
      IF (VERB .EQ. 0) VERB = 23252
C
      IF (SEMFVD(SYN,VERB,NPMODU,NOSYN)) GOTO 10
C
C If $ option specified, normal syntax checking is bypassed,
C so find start of first item and store pointer value is $ key
C (zero if rest of line is blank), otherwise, proceed with
C normal syntax checking
C
      IF (NOSYN) THEN
         IF (SEMXA1(0,LINBUF,COMLIM,PTR,X,N)) THEN
            DOLLAR = 0.0
         ELSE
            DOLLAR = REAL(PTR)
         ENDIF
         GOTO 270
      ENDIF
C
C Processing command interpretation
C ---------------------------------
C
C Decode options and keys: initialise count of assumed keys
C
      IASKEY = 0
C
C Clear the dollar key. This fixed variable is used to pass the
C command line pointer to commands with their own internal syntax. It
C is a fixed variable to ensure that commands will still work even if
C the variable table is full. (This is essential for the UNSET command!)
C
      DOLLAR = 0.0
C
C Find start of current item and make a note of it
C
  120 IF (SEMXA1(0,LINBUF,COMLIM,PTR,X,N)) GOTO 220
      NPTR = PTR
C
C Fetch item - to see if option/key name
C
      IF (SEMXA1(1,LINBUF,COMLIM,PTR,X,NAME)) GOTO 220
      IF (ERROR.NE.0) GOTO 10
C
C Skip search through option/key lists if item not terminated with
C blank or end-of-line
C
      IF (PTR.LE.COMLIM) THEN
         IF (LINBUF(PTR).NE.KSPACE) GOTO 160
      ENDIF
C
C If valid name, look for it in option/key lists
C
      IF (NAME.NE.0) THEN
C
C Search option list
C
         X=1.0
         DO 130 I = 2,LSTOPT
            IF (NAME .EQ. OPTION(I)) GOTO 180
  130    CONTINUE
C
C Search key list
C
         DO 140 I = 2,LSTKEY
            IF (NAME .EQ. KEY(I)) THEN
C
C Valid key: advance to next item and branch to process key value
C
               IF (SEMXA1(0,LINBUF,COMLIM,PTR,X,N)) THEN
C
C Syntax error
C
                  ERROR = 17
                  GOTO 10
               ENDIF
               GOTO 190
            ENDIF
  140    CONTINUE
C
C If name starts with NO, try negated option
C
         IF (NAME.GE.NNOA .AND. NAME.LE.NNOZ) THEN
C
C Advance pointer by 2 characters and decode option name
C
            PTR = NPTR + 2
            IF (SEMXA1(1,LINBUF,COMLIM,PTR,X,NAME)) GOTO 220
C
C Search option list again
C
            X=0.0
            DO 150 I = 2,LSTOPT
               IF (NAME .EQ. OPTION(I)) GOTO 180
  150       CONTINUE
         ENDIF
      ENDIF
C
C Try next unused dollar key
C
  160 IF (IASKEY.LT.NASKYS) THEN
C
C Set up dollar key name
C
         IASKEY = IASKEY + 1
         NAME= NDLLR0 - 40*IASKEY
C
C Search key list again
C
         DO 170 I = 2,LSTKEY
            IF (NAME .EQ. KEY(I)) THEN
C
C Valid key: reset pointer to current item and branch to process
C            key value
C
               PTR = NPTR
               GOTO 190
            ENDIF
  170    CONTINUE
      ENDIF
C
C Syntax error - item does not match any allowed keys/options
C
      ERROR = 17
      GOTO 10
C
C Option match: remove it from list, set option to YES/NO and branch
C               to process next item
C
  180 OPTION(I) = 0
      IF (SEMLU(2,NAME,X)) GOTO 10
      GOTO 120
C
C Key match: remove it from list
C
  190 KEY(I) = 0
C
C If textual key, key should be set to point to start of text string
C
      IF (DEF(I).NE.0) THEN
         IF (SYN(DEF(I)+1).EQ.KQUOTE) THEN
C
C Textual value: set key to pointer value, advance past text and branch
C                to process next item
C
            IF (SEMLU(2,NAME,REAL(PTR))) GOTO 10
C
            N = 80
            IF (SEMTYP(LINBUF,COMLIM,PTR,TEXT,N,.TRUE.)) GOTO 10
            GOTO 120
         ENDIF
      ENDIF
C
C Numerical value: read one or more expressions as numerical key values
C
      N = 9
      IF (SEMXPL(LINBUF,COMLIM,PTR,VX,N)) GOTO 10
C
C Set named key to first key value
C
      IF (SEMLU(2,NAME,VX(1))) GOTO 10
C
C Unpack key name into character string
C
      STRING = UNPACKF(NAME)
C
C Process remaining key values
C
      DO 210 I = 2,N
C
C Set up next key name (with suffix I)
C
         IF (STRING(2:2).EQ.' ') THEN
            STRING(2:2) = DIGITS(I:I)
         ELSE
            STRING(3:3) = DIGITS(I:I)
         ENDIF
C
         NAME = IPACK(STRING)
C
C Search key list
C
         DO 200 J = 2,LSTKEY
C
C Valid key: remove it from list and set key value
C
            IF (NAME.EQ.KEY(J)) THEN
               KEY(J) = 0
               IF (SEMLU(2,NAME,VX(I))) GOTO 10
               GOTO 210
            ENDIF
  200    CONTINUE
C
C Illegal item: no corresponding multi-valued key allowed
C
         IDERR = NAME
C
C Illegal item
C
         ERROR = 57
         GOTO 10
  210 CONTINUE
C
C Branch to process next item
C
      GOTO 120
C
C Command exhausted: process key defaults
C
  220 DO 230 I=NGKEYS+2,LSTKEY
C
C Skip if key already set
C
         NAME = KEY(I)
         IF (NAME.EQ.0) GOTO 230
C
C Skip if no default provided
C
         J = DEF(I)
         IF (J.EQ.0) GOTO 230
C
C Locally unset textual keys (to avoid picking up value of Semper
C variable as string pointer)
C
         IF (SYN(J+1).EQ.KQUOTE) THEN
            IF (SEMLU(3,NAME,0.)) GOTO 10
            IF (SEMLU(0,NAME,0.)) GOTO 230
C
C Otherwise, process numerical default
C
         ELSE
C
C Skip if key set globally (Semper variable of same name is set)
C
            IF (VARSET(NAME)) GOTO 230
C
C Evaluate default and set
C
            L=J+1
            IF (SEMEXP(SYN,J+SYN(J),L,X,.FALSE.)) GOTO 10
            IF (SEMLU(2,NAME,X)) GOTO 10
         ENDIF
  230 CONTINUE
C
C Enable selection by SEMOPN, note current priority, and
C open any specified pictures
C
      SELOPN = .TRUE.
      BASEWH = CURRWH
C
C (Update source line pointer in case opens fail)
C
      LASTSC = NEXTSC
C
      DO 240 I = 1,3
         USEDLP(I) = .FALSE.
  240 CONTINUE
C
      DO 250 I = 1,NOPENR
         L = OPENR(I+(1+MAXOPE))
         IF (L .EQ. 2) GOTO 250
C
C Determine picture number
C
         J=OPENR(I+(1+4*MAXOPE))
         K=J+1
         N=J+SYN(J)
         IF (SEMEXP(SYN,N,K,X,.FALSE.)) GOTO 10
C
C Open picture
C
         N=X
         J=OPENR(I+1)
C
C Unset comparison LPN, set used LP array
C
         IF (USEDLP(J)) THEN
            IDERR = J
            ERROR = 145
            GOTO 10
         ELSE
            USEDLP(J) = .TRUE.
         ENDIF
         LPNS(J) = 0
C
         IF (SEMOPN(L,SEMPPN(N),NCOL,NROW,NLAY,CLASS,
     +              FORM,LPNS(J))) GOTO 10
  250 CONTINUE
C
      DO 260 I = 1,NOPENR
         L = OPENR(I+(1+MAXOPE))
         IF (L .NE. 2) GOTO 260
C
C Any comparison picture indicated?
C
         J=OPENR(I+(1+2*MAXOPE))
         IF (J .GT. 0) THEN
            IF (.NOT.USEDLP(J)) THEN
               IDERR = J
               ERROR = 146
               GOTO 10
            ENDIF
         ENDIF
C
         IF (J .NE. 0) THEN
C
C Yes: set corresponding characteristics
C
            LPN=LPNS(J)
            NCOL=NCOLS(LPN)
            NROW=NROWS(LPN)
            NLAY=NLAYS(LPN)
            CLASS=CLASSN(LPN)
            FORM=FORMN(LPN)
         ELSE
C
C No: set default characteristics
C
            LPN=0
            NCOL=0
            NROW=0
            NLAY=1
            CLASS=NCLIMA
            FORM=NFMFP
         ENDIF
C
C Modify dimensions by SIZE/SI2/SI3
C
         IF (VARSET(30786)) THEN
            NCOL=IVAL(30786)
            NROW=NCOL
         ENDIF
         IF (VARSET(30792)) NROW=IVAL(30792)
         IF (VARSET(30793)) NLAY=IVAL(30793)
C
C Modify FORM by BYTE/INTEGER/FP/COMPLEX
C
         FORM=SEMFRM(FORM)
C
C Determine picture number
C
         J=OPENR(I+(1+4*MAXOPE))
         K=J+1
         N=J+SYN(J)
         IF (SEMEXP(SYN,N,K,X,.FALSE.)) GOTO 10
C
C Open picture
C
         N=X
         J=OPENR(I+1)
C
C Set comparison LPN in case title needed
C
         LPNS(J) = LPN
C
C (Update source line pointer in case open fails)
C
         IF (SEMOPN(L,SEMPPN(N),NCOL,NROW,NLAY,CLASS,
     +              FORM,LPNS(J))) GOTO 10
  260 CONTINUE
C
C Command termination
C -------------------
C
C Update end of command pointer
C
  270 LASTSC = NEXTSC
C
C Override any row selection caused by current row dumping
C
      OPLPN = 0
C
C Reflect command on terminal/log
C
      IF (SEMCOM(REFSTR,REFLEN)) GOTO 10
C
C Unless command intrinsic, call external module by returning
C
      IF (NPMODU .EQ. 0) GOTO 10
C
  280 RETURN
C
C Error recovery - output message with optional context,
C --------------   revert to (fresh) interactive input
C
  290 CONTINUE
C
C FATAL is set only if a fatal error code is detected
C
      FATAL = ERROR .EQ. FATERR
C
C Reflect command on terminal/log (if already done then REFLEN = 0)
C
      IF (SEMCOM(REFSTR,REFLEN)) THEN
         IF (.NOT. FATAL) GOTO 10
      ENDIF
C
C Print error message, suppressing context if NOERCN and error at end
C of command
C
      CALL SEMERR(RECORD)
      N = LNBLNK(RECORD)
      IF (N .NE. 0) THEN
C
C Preserve error record
C
         ERRREC = RECORD
         IF (SEMDIA(ERRREC(1:N),NDIERR)) THEN
C
C Trouble - suppress context
C
            NOERCN = .TRUE.
            IF (.NOT. FATAL) GOTO 10
         ENDIF
      ENDIF
C
      IF (.NOT.(NOERCN.AND.PTR.GE.COMLIM)) THEN
C
C Suppress recursive context in the event of further errors
C
         NOERCN = .TRUE.
         CALL SEMCTX(LINBUF,LINLEN,PTR,RECORD(1:60))
         IF (SEMDIA(RECORD(1:60),NDIERR)) THEN
            IF (.NOT. FATAL) GOTO 10
         ENDIF
      ENDIF
C
C Print traceback
C
      IF (SEMTBK()) THEN
         IF (.NOT. FATAL) GOTO 10
      ENDIF
C
C Sound the bell
C
      IF (SEMBEE()) THEN
         IF (.NOT. FATAL) GOTO 10
      ENDIF
C
C Return to interactive unit, kill loops/jumps, force new line
C
  300 INPLEV = 0
      IF (INPUT .NE. TERM1) THEN
         IF (SEMRET(0)) GOTO 310
      ENDIF
C
C Delete all LOCAL variables
C
  310 IF (SEMUNW(0)) GOTO 320
C
  320 FORLEV = 0
C
C Kill jump
C
      JMPDST = 0
      LINLEN = 0
C
C Ultimately perhaps, some form of reinstatement of temporaries could
C be attempted here, in preference to the SEMDEL(2) call now used to
C delete them..
C
      IF (FATAL) THEN
         NPMODU = 0
         GOTO 280
      ENDIF
C
      IF (PANIC) THEN
         GOTO 50
      ELSE
         PANIC = .TRUE.
         GOTO 30
      ENDIF
C
C Copyright (C) 1987-1990:  Synoptics Ltd,  All Rights Reserved
C
      END
