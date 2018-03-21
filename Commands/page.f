C Semper 6 processing module PAGE
C
      SUBROUTINE PAGE
C
C Provides PAGE verb.  Allows the user to change the default page size
C for terminal output by means of the WIDTH and LENGTH keys and also
C the default aspect ratio.  Some commands require a terminal width and
C an aspect ratio to work with.  The page size controls in the first
C place how terminal output is paginated.  If the WRAP option is active,
C overlong lines wrap onto the next line.  Otherwise, lines are
C truncated.  The default at start-up is NOWRAP.  If the PROMPT option
C is active, output is suspended and a page prompt output if a page full
C of text is about to be output since the last terminal prompt appeared.
C The default at start-up is PROMPT.  If the QUIT option is active, the
C 'Quit' option will appear in the page prompt and it will be acted on.
C The default at start-up is QUIT.
C
      LOGICAL OPT,OPTNO,VARSET,SEMTPS,SEMLU,SETPPR
      INTEGER IVAL
      REAL    VAL
C
      INTEGER IWID,ILEN
      REAL    ASPR
C
      INCLUDE 'COMMON'
C
C Packed names
C
      INTEGER NWIDTH,NLENGT,NASPEC,NPROMP,NWRAP,NENQUI,NPWIDT,NPLENG
      INTEGER NQUIT
      PARAMETER (NWIDTH=-5165, NLENGT=19414, NASPEC=2376, NPROMP=26335)
      PARAMETER (NWRAP=-5522, NENQUI=8577, NPWIDT=26529, NPLENG=26085)
      PARAMETER (NQUIT=28049)
C
C If WIDTH key is set, fetch new default terminal page width
C
      IF (VARSET(NWIDTH)) THEN
         IWID = IVAL(NWIDTH)
C
C Fault zero or negative value
C
         IF (IWID.LE.0) THEN
            ERROR = 3
            IDERR = NWIDTH
            GOTO 10
         ENDIF
      ELSE
         IWID = TERWID
      ENDIF
C
C If LENGTH key is set, fetch new default terminal page length
C
      IF (VARSET(NLENGT)) THEN
         ILEN = IVAL(NLENGT)
C
C Fault zero or negative value
C
         IF (ILEN.LE.0) THEN
            ERROR = 3
            IDERR = NLENGT
            GOTO 10
         ENDIF
      ELSE
         ILEN = TERLEN
      ENDIF
C
C If ASPECT key is set, fetch new default terminal aspect ratio
C
      IF (VARSET(NASPEC)) THEN
         ASPR = VAL(NASPEC)
C
C Fault zero or negative value
C
         IF (ASPR.LE.0.0) THEN
            ERROR = 3
            IDERR = NASPEC
            GOTO 10
         ENDIF
      ELSE
         ASPR = TERASP
      ENDIF
C
C Alter terminal page size and aspect ratio
C
      TERWID = IWID
      TERLEN = ILEN
      TERASP = ASPR
C
C Alter pagination prompt flag according to option PROMPT/NOPROMPT
C
      IF (OPT(NPROMP)) THEN
         TERPRO = .TRUE.
      ELSE IF (OPTNO(NPROMP)) THEN
         TERPRO = .FALSE.
      ENDIF
C
C Alter pagination wrap-around flag according to option WRAP/NOWRAP
C
      IF (OPT(NWRAP)) THEN
         TERWRA = .TRUE.
      ELSE IF (OPTNO(NWRAP)) THEN
         TERWRA = .FALSE.
      ENDIF
C
C Alter pagination QUIT enable flag according to option QUIT/NOQUIT
C
      IF (OPT(NQUIT)) THEN
         TERQUI = .TRUE.
      ELSE IF (OPTNO(NQUIT)) THEN
         TERQUI = .FALSE.
      ENDIF
C
C Set up the page prompt string again, just in case the QUIT enable
C flag has changed
C
      IF (SETPPR()) GOTO 10
C
C If option ENQUIRE is set, return current terminal page size in
C variables PLENGTH and PWIDTH
C
      IF (OPT(NENQUI)) THEN
C
C Fetch current terminal page size
C
         IF (SEMTPS(IWID,ILEN)) GOTO 10
C
C Set variables PLENGTH and PWIDTH
C
         IF (SEMLU(1,NPWIDT,REAL(IWID))) GOTO 10
         IF (SEMLU(1,NPLENG,REAL(ILEN))) GOTO 10
      ENDIF
C
   10 RETURN
C
C Copyright (C) 1989-1995:  Synoptics Ltd,  All Rights Reserved
C
      END
