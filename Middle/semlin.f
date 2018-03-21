C Semper 6 system module SEMLIN
C
      LOGICAL FUNCTION SEMLIN(BUFFER,LAST,VD)
C
C Gets the next input line, checking FOR loop matching if interactive
C
      INTEGER BUFFER(*),LAST,VD(*)
C
      LOGICAL PRULAB,SEMENV,SEMMAC,SEMXA1,SEMUNW,SEMPRG
C
      INCLUDE 'COMMON'
C
      REAL DUMMY
      INTEGER*4 I4N
      INTEGER BUFPTR,BUFEND
      INTEGER LABNUM,INPTR,TXTPTR,CHKFOR,COMBEG,ENDCOM,KEY
      INTEGER SAVPTR,SCLAST,SCNEXT,I,J,IVAR,IVERB
      LOGICAL INQUOT,SPACED
      CHARACTER*10 PROMPT
C
C Packed names
C
      INTEGER NEND,NFOR,NLOOP
      PARAMETER (NEND=8564,NFOR=10218,NLOOP=19815)
C
      BUFPTR = 1
      IF (INPDEV(INPLEV) .NE. 0) THEN
C
C Attempt to read next text line from INPDEV(INPLEV)
C
         INPLIN(INPLEV) = INPLIN(INPLEV) + 1
C
         IF (SEMENV(INPDEV(INPLEV),INPSLT(INPLEV),INPLIN(INPLEV))) THEN
            IF (ERROR .EQ. 0) GOTO 130
         ELSE
            BUFPTR = LAST + 1
         ENDIF
      ELSE
         IF (OBEYME) THEN
C
C Copy and parse OBEY line from obey buffer
C
            BUFEND = OBEYPT
            I4N = BUFEND
            CALL CFORM(OBEYLN,BUFFER,NFMINT,NFMINT,I4N)
         ELSE
C
C End of an obey command ? - force a return if so
C
            IF (INPLEV .GT. 0) GOTO 130
C
C Read next interactive line
C
            BUFEND = LAST
            IF (SEMPRG(INPUT,BUFFER(BUFPTR),BUFEND,.TRUE.,'S$')) GOTO 12
     +0
         ENDIF
C
C Forget all local variables
C
         IF (SEMUNW(0)) GOTO 120
C
C Construct index for line (loosely based on PRUADD)
C
         LABNUM = 1
         TXTPTR = 1
         CHKFOR = 0
         INPTR = 1
         SCLAST = 0
         IF (BUFEND .EQ. 0) GOTO 100
C
   10    IF (SCLAST .GT. BUFEND) THEN
C
C May need to read another line iff CHKFOR > 0
C
            IF (CHKFOR .LE. 0) GOTO 100
C
            IF (OBEYME) THEN
C
C Can't ask for another line !
C
               ERROR = 106
               GOTO 120
            ENDIF
C
C Allow space for interleaving semicolon (inserted later)
C
            BUFPTR = TXTPTR + 1
   20       BUFEND = LAST - TXTPTR
            IF (BUFEND .LE. 0) THEN
               ERROR = 77
               IF (CHKFOR .EQ. 0) THEN
                  IDMESS = '(Continued) line too long'
               ELSE
                  IDMESS = 'FOR loop too long'
               ENDIF
               GOTO 110
            ENDIF
            I = MIN(CHKFOR,9) + KZERO
C
C This mechanism used to avoid problems with compilers that do
C not allow concatenated strings as subroutine arguments (e.g. SUN,SEL)
C
            PROMPT = '(For x) S$'
            PROMPT(6:6) = CHAR(I)
            IF (SEMPRG(INPUT,BUFFER(BUFPTR),BUFEND,.TRUE.,PROMPT))
     +         GOTO 120
            IF (BUFEND .EQ. 0) GOTO 20
            BUFEND = BUFEND + TXTPTR
            SCLAST = TXTPTR
         ENDIF
         INQUOT = .FALSE.
         INPTR = SCLAST + 1
C
C Skip leading spaces in command
C
   30    IF (INPTR .LT. BUFEND) THEN
            IF (BUFFER(INPTR) .EQ. KSPACE) THEN
               INPTR = INPTR + 1
               GOTO 30
            ENDIF
         ENDIF
C
C Remember beginning of command
C
         ENDCOM = INPTR
         COMBEG = ENDCOM
C
C Scan for end of command
C
   40    IF (ENDCOM .LE. BUFEND) THEN
            KEY = BUFFER(ENDCOM)
            IF (KEY .EQ. KQUOTE) INQUOT = .NOT. INQUOT
            IF (.NOT. INQUOT) THEN
               IF (KEY .EQ. KSEMIC) GOTO 50
               IF (KEY .EQ. KAT) THEN
C
C Macro call found - expand it!
C
                  IF (SEMMAC(1,J,ENDCOM,VD,BUFFER,BUFEND,LAST))
     +               GOTO 120
                  GOTO 40
               ENDIF
            ENDIF
            ENDCOM = ENDCOM + 1
            GOTO 40
         ENDIF
C
C End of command or line
C
   50    SCNEXT = ENDCOM
         ENDCOM = ENDCOM - 1
         J = ENDCOM
C
C Remove trailing spaces
C
   60    IF (ENDCOM .GT. INPTR) THEN
            IF (BUFFER(ENDCOM) .EQ. KSPACE) THEN
               ENDCOM = ENDCOM - 1
               GOTO 60
            ENDIF
         ENDIF
         IVERB = 0
C
C Skip comment
C
         IF (BUFFER(INPTR) .EQ. KPLING) GOTO 80
         IF (SEMXA1(1,BUFFER,ENDCOM,INPTR,DUMMY,IVERB)) GOTO 80
         IF (ERROR .EQ. 25) ERROR = 0
         IF (ERROR .NE. 0) GOTO 80
C
C Establish save pointer for PRULAB calls
C
         SAVPTR = TXTPTR
         IF (TXTPTR .GT. 1) SAVPTR = SAVPTR + 1
C
C See if label
C
         IF (BUFFER(INPTR) .EQ. KCOLON) THEN
            IF (PRULAB(LABNUM,LINDEX,TIDLAB,
     +                 0,SAVPTR,IVERB)) GOTO 120
C
C Get real verb (if there is one)
C
            I = INPTR + 1
            IF (SEMXA1(0,BUFFER,ENDCOM,I,DUMMY,KEY)) GOTO 80
            IF (KEY .EQ. 0) GOTO 80
            IF (SEMXA1(1,BUFFER,ENDCOM,I,DUMMY,IVERB)) GOTO 80
            IF (ERROR .EQ. 25) ERROR = 0
            IF (ERROR .NE. 0) GOTO 80
            INPTR = I
         ENDIF
C
C Skip assigment, first passing over any itervening blanks
C
         I = INPTR
   70    IF (I .LE. ENDCOM) THEN
            IF (BUFFER(I) .EQ. KSPACE) THEN
               I = I + 1
               GOTO 70
            ELSE IF (BUFFER(I) .EQ. KEQUAL) THEN
               GOTO 80
            ENDIF
         ENDIF
C
C Now check for FOR or LOOP
C
         IF ((IVERB .EQ. NFOR .AND. INPTR .LT. ENDCOM)
     +        .OR. IVERB .EQ. NLOOP) THEN
            I = INPTR + 1
            IVAR = 0
            IF (.NOT.SEMXA1(0,BUFFER,ENDCOM,I,DUMMY,KEY)) THEN
               IF (KEY .NE. KSEMIC) THEN
                  IF (SEMXA1(1,BUFFER,ENDCOM,I,DUMMY,IVAR)) CONTINUE
               ENDIF
               IF (IVERB .EQ. NFOR) THEN
                  IF (IVAR .EQ. 0) GOTO 80
                  IF (SEMXA1(0,BUFFER,ENDCOM,I,DUMMY,KEY)) GOTO 80
               ENDIF
            ENDIF
C
C IVERB is NFOR or NLOOP, IVAR is packed variable (or 0)
C
            IF (IVERB .EQ. NFOR) THEN
               IF (IVAR .EQ. 0) GOTO 80
               I = TIDFOR
               CHKFOR = CHKFOR + 1
            ELSE
               I = TIDLOO
               CHKFOR = CHKFOR - 1
            ENDIF
            IF (PRULAB(LABNUM,LINDEX,I,0,SAVPTR,IVAR)) GOTO 120
         ENDIF
C
   80    CONTINUE
C
C Copy command back into buffer - include an initial ';' if not
C the first command inserted (won't overwrite as there must be
C a semi-colon in the original line anyway!
C
         IF (TXTPTR .NE. 1 .OR. COMBEG .GT. ENDCOM) THEN
            BUFFER(TXTPTR) = KSEMIC
            TXTPTR = TXTPTR + 1
         ENDIF
         SAVPTR = TXTPTR
         IF (COMBEG .LE. ENDCOM) THEN
C
C Copy, compressing multiple spaces
C
            INQUOT = .FALSE.
            SPACED = .FALSE.
            DO 90 I = COMBEG,ENDCOM
               J = BUFFER(I)
               IF (J .EQ. KQUOTE) INQUOT = .NOT. INQUOT
               IF (INQUOT) SPACED = .FALSE.
               IF (.NOT. SPACED .OR. J .NE. KSPACE) THEN
                  BUFFER(TXTPTR) = J
                  TXTPTR = TXTPTR + 1
                  SPACED = J .EQ. KSPACE .AND. .NOT. INQUOT
               ENDIF
   90       CONTINUE
            IF (SPACED) TXTPTR = TXTPTR - 1
         ENDIF
         IF (IVERB .EQ. NEND) GOTO 100
C
         SCLAST = SCNEXT
         GOTO 10
C
  100    I = 0
         J = 0
         BUFPTR = TXTPTR
         LINPTR = BUFPTR - 1
         IF (PRULAB(I,LINDEX,TIDLIN,J,0,TXTPTR)) GOTO 120
         IF (PRULAB(LABNUM,LINDEX,TIDEND,0,0,0)) GOTO 120
         LINDEV = 0
         LINSLT = 0
         LINNUM = 0
      ENDIF
  110 LAST = BUFPTR - 1
  120 OBEYME = .FALSE.
      SEMLIN = ERROR .NE. 0
      RETURN
C
C Force an end/return
C
  130 BUFFER(1)=KLCE
      BUFFER(2)=KLCN
      BUFFER(3)=KLCD
      BUFPTR = 4
      LINSLT = -1
      GOTO 110
C
C Copyright (C) 1987-1990:  Synoptics Ltd,  All Rights Reserved
C
      END
