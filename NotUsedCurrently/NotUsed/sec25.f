      SUBROUTINE LINEDT
C
C  exerciser for KLINE
C
C  SYNTAX   Line  :LINEDT  prompt='  length=80  zero
C
C  SEMPER routines
      LOGICAL KLINE,SEMCON,SEMKTX,OPT
      INTEGER IPACK,IVAL
C
      INCLUDE 'COMMON'
      INTEGER NPROMT,LENPRO,LENSTR,ITEXT(20),NS,NL
      CHARACTER*(20)  PROMPT
      CHARACTER*(120) STRING
      LOGICAL ZERO
C
C  see whether the option ZERO has ben set
C
      ZERO=OPT(IPACK('ZERO'))
C
C  length of input line string, length prompt string
C
      LENSTR=IVAL(IPACK('LENGTH'))
      IF(LENSTR .GT. 120 .OR. LENSTR .LT. 1) THEN
         ERROR=3
         IDERR=IPACK('LENGTH')
         RETURN
      ENDIF
C
      LENPRO=20
C
C  get prompt string as integer array ITEXT
C
      IF(.NOT. ZERO) THEN
         NPROMT=IPACK('PROMPT')
         IF(SEMKTX(NPROMT,'Prompt for KLINE ? ',ITEXT,LENPRO,.FALSE.))
     +                                                           RETURN
C
C  convert to character form
C
         CALL SEMCHS(PROMPT,ITEXT,LENPRO)
C
C  Now have LENPRO = NBLANK(PROMPT).
C
      ELSE
C
         IF(ZERO) LENPRO=0
C
      ENDIF
C
      STRING='default string'
      NS=MIN(LENSTR,14)
C
C  main loop
C
   10 CONTINUE
C
C  issue prompt without trailing spaces,
C  input line in buffer of size LENSTR
C
         NL=MAX(1,LENPRO)
         IF(KLINE(PROMPT(1:NL),.NOT. ZERO,STRING(1:LENSTR),NS)) RETURN
C
C  NS is useful length of STRING (note LEN(RECORD) > 120 = max(NS) )
C
         IF(SEMCON(' ')) RETURN
         WRITE(RECORD,20) NS,LENPRO
   20    FORMAT(' Length of input line ',I3,'    Prompt length ',I3)
         IF(SEMCON(RECORD)) RETURN
C
C  if string was of zero length give up since subsequent string
C  handling unsafe
C
         IF(NS .EQ. 0) GOTO 10
C
         IF(SEMCON(STRING(1:NS))) RETURN
C
C  way out
C
         IF(STRING(1:1).EQ.'Q' .OR. STRING(1:1).EQ.'q') RETURN
C
         IF(SEMCON(' ')) RETURN
C
C  no default line
C
         NS=0
C
C
      GOTO 10
C
      END
