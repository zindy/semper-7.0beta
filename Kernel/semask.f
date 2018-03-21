C Semper 6 system module SEMASK
C
      LOGICAL FUNCTION SEMASK(IN,LENIN,PTR)
C
C Processes ASK argument string in IN, outputting prompt and accepting
C string of expressions for variable list
C
      INTEGER IN(*),LENIN,PTR
C
      LOGICAL KLINE,SEMXPL,SEMTYP,SEMLU,SEMXA1,SEMDIA
      LOGICAL SEMBEE,SEMINP,SEMINT
      INTEGER LNBLNK
C
      INTEGER NTEXT,NEXPR
      PARAMETER (NTEXT=80)
      PARAMETER (NEXPR=10)
C
      INTEGER ITEXT(NTEXT),VNAME(NEXPR)
      INTEGER ICH,NAME,I,L,N,NP,NV,IPTR,NE
      CHARACTER*(NTEXT) PROMPT,LINE
      REAL VALUE(NEXPR),X
C
      INCLUDE 'COMMON'
C
      EQUIVALENCE (RB2,ITEXT)
C
      SEMASK = .TRUE.
C
C Fault batch or UIF input (RUN and PROGRAM are okay)
C
      IF (.NOT.SEMINT(.FALSE.)) THEN
         ERROR=77
         IDMESS='ASK can not be used in batch or UIF mode'
         GOTO 50
      ENDIF
C
C Look for prompt, returning with no error if rest of line is blank
C
      IF (SEMXA1(0,IN,LENIN,PTR,X,ICH)) GOTO 40
      IF (ICH .EQ. KQUOTE) THEN
C
C Process prompt via SEMTYP
C
         NP = NTEXT
         IF (SEMTYP(IN,LENIN,PTR,ITEXT,NP,.FALSE.)) GOTO 50
C
         CALL SEMCHS(PROMPT,ITEXT,NP)
      ELSE
C
C Make variable list into prompt
         NP = MIN(LENIN - PTR + 1,NTEXT-2)
         CALL SEMCHS(PROMPT,IN(PTR),NP)
C
         PROMPT(NP+1:NP+2) = ': '
         NP = NP + 2
      ENDIF
C
C Decode list of variable names
C
      N = 0
C
C If not first name, check for separating comma
C
   10 IF (N .NE. 0) THEN
         IF (SEMXA1(0,IN,LENIN,PTR,X,ICH)) GOTO 20
         IF (ICH .NE. KCOMMA) GOTO 60
         PTR = PTR + 1
      ENDIF
C
C Read variable name, ignoring any beyond the tenth one
C
      IF (.NOT.SEMXA1(1,IN,LENIN,PTR,X,NAME)) THEN
         IF (NAME.EQ.0) GOTO 60
C
         IF (N .LT. NEXPR) THEN
            N = N + 1
            VNAME(N) = NAME
         ENDIF
         GOTO 10
      ENDIF
C
C Output prompt string and read line from terminal
C
   20 L=0
      IF (KLINE(PROMPT(1:NP),.TRUE.,LINE,L)) GOTO 50
C
C Reflect line of input
C
      IF (L .EQ. 0) THEN
         IF (SEMINP(' ')) GOTO 50
      ELSE
         IF (SEMINP(LINE(1:L))) GOTO 50
      ENDIF
C
C Decode expression list, if required
C
      IF (L .EQ. 0 .OR. N .EQ. 0) THEN
         NV = 0
      ELSE
         CALL SEMICS(LINE,ITEXT,L)
         IPTR = 1
         NV = N
         IF (SEMXPL(ITEXT,L,IPTR,VALUE,NV)) THEN
C
C Report any syntax error locally and then go back and try again
C
            CALL SEMERR(LINE)
            NE=LNBLNK(LINE)
            IF (NE.NE.0) THEN
               IF (SEMDIA(LINE(1:NE),NDIWAR)) GOTO 50
            ENDIF
            CALL SEMCTX(ITEXT,L,IPTR,LINE(1:60))
            IF (SEMDIA(LINE(1:60),NDIWAR)) GOTO 50
            IF (SEMBEE()) GOTO 50
            GOTO 20
         ENDIF
      ENDIF
C
C Set/unset listed names
C
      DO 30 I=1,N
         IF (I .GT. NV) THEN
            IF (SEMLU(0,VNAME(I),X)) GOTO 50
         ELSE
            IF (SEMLU(1,VNAME(I),VALUE(I))) GOTO 50
         ENDIF
   30 CONTINUE
C
   40 SEMASK = .FALSE.
C
   50 RETURN
C
C Syntax error
C
   60 ERROR = 17
      GOTO 50
C
C Copyright (C) 1987-1991:  Synoptics Ltd,  All Rights Reserved
C
      END
