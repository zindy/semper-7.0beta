C Semper 6 system module SEMKTX
C
      LOGICAL FUNCTION SEMKTX(NAME,PROMPT,ITEXT,LENOUT,UC)
C
C Evaluates textual value of key NAME in command line buffer in
C ITEXT(1-LENOUT); returns length 0 if key not set; forces result to
C upper case if UC
C
C if key unset.. if PROMPT not ' ', prompts at terminal
C                                   trapping errors locally
C                else returns length 0
C
      INTEGER NAME,ITEXT(*),LENOUT
      CHARACTER*(*) PROMPT
      LOGICAL UC
C
      LOGICAL VARSET,SEMTYP,KLINE,SEMDIA,SEMBEE,SEMINP,SEMINT
      INTEGER IVAL,LNBLNK
C
      INTEGER N,IN(80),LENIN,IPTR
      CHARACTER*80 STRING
C
      INCLUDE 'COMMON'
C
      SEMKTX=.TRUE.
C
      IF (VARSET(NAME)) THEN
C
C Key present: evaluate text in command line buffer
C
         IPTR=IVAL(NAME)
         N=LENOUT
         IF (SEMTYP(LINBUF,LINLEN,IPTR,ITEXT,N,.FALSE.)) THEN
            ERROR=3
            IDERR=NAME
            GOTO 20
         ENDIF
      ELSE
C
C Key absent: if blank prompt supplied, or in batch or UIF mode, return
C             null result
C
         IF (PROMPT.EQ.' '.OR. .NOT.SEMINT(.FALSE.)) THEN
            N = 0
         ELSE
C
C Output prompt and read line of input
C
   10       LENIN = 0
            IF (KLINE(PROMPT,.TRUE.,STRING,LENIN)) GOTO 20
C
C Reflect line of input
C
            IF (LENIN.EQ.0) THEN
               IF (SEMINP(' ')) GOTO 20
            ELSE
               IF (SEMINP(STRING(1:LENIN))) GOTO 20
            ENDIF
C
C Evaluate text in input buffer
C
            CALL SEMICS(STRING,IN,LENIN)
C
            IPTR=1
            N=LENOUT
            IF (SEMTYP(IN,LENIN,IPTR,ITEXT,N,.FALSE.)) THEN
C
C Report error locally and then go back to prompt for more input
C
               CALL SEMERR(STRING)
               N=LNBLNK(STRING)
               IF (N.NE.0) THEN
                  IF (SEMDIA(STRING(1:N),NDIWAR)) GOTO 20
               ENDIF
               CALL SEMCTX(IN,LENIN,IPTR,STRING(1:60))
               IF (SEMDIA(STRING(1:60),NDIWAR)) GOTO 20
               IF (SEMBEE()) GOTO 20
               GOTO 10
            ENDIF
         ENDIF
      ENDIF
C
C Force to upper case?
C
      IF (UC) CALL SEMUPP(ITEXT,N)
C
      LENOUT = N
C
      SEMKTX=.FALSE.
C
   20 RETURN
C
C Copyright (C) 1987-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 system module SEMTEX
C
      LOGICAL FUNCTION SEMTEX(NAME,ITEXT,LENOUT)
C
C Evaluates textual value of key NAME in command line buffer in
C ITEXT(1-LENOUT); returns length 0 if key not set
C
C if key unset returns length 0
C
C (Really a version of SEMKTX with no prompt or upper-casing)
C
      INTEGER NAME,ITEXT(*),LENOUT
C
      LOGICAL VARSET,SEMTYP
      INTEGER IVAL
C
      INTEGER IPTR
C
      INCLUDE 'COMMON'
C
      SEMTEX = .TRUE.
C
      IF (VARSET(NAME)) THEN
C
C Key present: evaluate text in command line buffer
C
         IPTR = IVAL(NAME)
         IF (SEMTYP(LINBUF,LINLEN,IPTR,ITEXT,LENOUT,.FALSE.)) THEN
            ERROR = 3
            IDERR = NAME
            GOTO 10
         ENDIF
      ELSE
C
C Key absent: return null result
C
         LENOUT = 0
      ENDIF
C
      SEMTEX = .FALSE.
C
   10 RETURN
C
C Copyright (C) 1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 system module SEMUPP
C
      SUBROUTINE SEMUPP(ITEXT,ILEN)
C
C Upper cases ILEN internal characters in ITEXT
C
      INTEGER ITEXT(*),ILEN
C
      INTEGER I,IPTR
C
      INCLUDE 'ICSET'
C
      INTEGER LCTOUC
      PARAMETER (LCTOUC=(KUCA-KLCA))
C
C Force to upper case?
C
      IF (ILEN .GT. 0) THEN
         DO 10 I=1,ILEN
            IPTR = ITEXT(I)
            IF (IPTR.GE.KLCA .AND. IPTR.LE.KLCZ) THEN
               ITEXT(I) = IPTR + LCTOUC
            ENDIF
   10    CONTINUE
      ENDIF
C
      RETURN
C
C Copyright (C) 1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 system module FILKTX
C
      LOGICAL FUNCTION FILKTX(DESC,NAME,NAMLEN,ISWRIT)
      CHARACTER*(*) DESC
      INTEGER NAME(*),NAMLEN
      LOGICAL ISWRIT
C
      LOGICAL SEMKTX
C
      INTEGER PRMLEN
      PARAMETER (PRMLEN=80)
      CHARACTER*80 PROMPT
      INTEGER PLEN
C
      IF (DESC(1:1) .NE. ' ') THEN
         PROMPT = DESC
         PLEN = LEN(DESC)
      ELSE
         IF (ISWRIT) THEN
            PROMPT = 'Output file name'
            PLEN = 16
         ELSE
            PROMPT = 'Input file name (as textstring):'
            PLEN = 15
         ENDIF
      ENDIF
C
C Add text string reminder
C
      IF (PLEN + 16 .LT. PRMLEN) THEN
         PROMPT(PLEN+1:PLEN+16) = ' (as textstring)'
         PLEN = PLEN + 16
      ENDIF
C
C Add ':'
C
      IF (PLEN + 1 .LT. PRMLEN) THEN
         PLEN = PLEN + 1
         PROMPT(PLEN:PLEN) = ':'
      ENDIF
C
C Add space
C
      IF (PLEN + 1 .LT. PRMLEN) PLEN = PLEN + 1
C
      FILKTX = SEMKTX(22453,PROMPT(1:PLEN),NAME,NAMLEN,.FALSE.)
      RETURN
C
C Copyright (C) 1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 system module FILSTR
C
      LOGICAL FUNCTION FILSTR(DESC,NAME,NAMLEN,ISWRIT)
      CHARACTER*(*) DESC,NAME
      INTEGER NAMLEN
      LOGICAL ISWRIT
C
      INTEGER LNBLNK !,IPACK
      LOGICAL OPT
C
      INCLUDE 'COMMON'
C
      INTEGER PLEN,NLEN
C
      INTEGER MAXNAM
      PARAMETER (MAXNAM=255)
      INTEGER A1NAME(MAXNAM)
C
      LOGICAL FILKTX
C
      NLEN = LEN(NAME)
      IF (OPT(1881) .AND. LIOVLD .AND. .NOT.ISWRIT) THEN
         PLEN = LNBLNK(IONAME)
         IF (NLEN .LT. PLEN) PLEN = NLEN
         NAME = IONAME(1:PLEN)
         FILSTR = .FALSE.
      ELSE
         PLEN = NLEN
         IF (PLEN .GT. MAXNAM) PLEN = MAXNAM
         IF (FILKTX(DESC,A1NAME,PLEN,ISWRIT)) THEN
            NAME = ' '
            PLEN = 0
C
C Invalidate stored name as well
C
            IF (.NOT.ISWRIT) LIOVLD = .FALSE.
            FILSTR = .TRUE.
         ELSE
C
C Copy file name into CHARACTER variable
C
            CALL SEMCHS(NAME,A1NAME,PLEN)
C
C If open for read copy name to stored name
C
            IF (PLEN .LE. LEN(IONAME) .AND. .NOT.ISWRIT) THEN
               IONAME = NAME(1:PLEN)
               LIOVLD = .TRUE.
            ENDIF
            FILSTR = .FALSE.
         ENDIF
      ENDIF
      NAMLEN = PLEN
C
      RETURN
C
C Copyright (C) 1992-1994:  Synoptics Ltd,  All Rights Reserved
C
      END
