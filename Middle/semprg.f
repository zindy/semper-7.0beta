C Semper 6 system module SEMPRG
C
      LOGICAL FUNCTION SEMPRG(UNIT,BUFFER,BUFSIZ,DUMMY,PROMPT)
C
C Reads a program line
C
      INTEGER UNIT,BUFFER(*),BUFSIZ
      LOGICAL DUMMY, IJUNK
      CHARACTER*(*) PROMPT
C
      INTEGER LNBLNK
      LOGICAL READSI,UIFINP,KLINE,ABANDN,SEMSOP
C
      INTEGER I,N,FIRST,LAST
      INTEGER IOS
      CHARACTER*200 TEXT
      CHARACTER*20 PSTR
C
      INCLUDE 'COMMON'
C
   10 FORMAT (A)
C
      SEMPRG = .TRUE.
C
      FIRST = 1
C
C See if input is from file (RUN or program file)
C
   20 IF (UNIT .NE. TERM1) THEN
C
C Read input from given file
C
         READ (UNIT,10,END=70,ERR=80,IOSTAT=IOS) TEXT
C
         N = LNBLNK(TEXT)
C
C Otherwise, read input from appropriate source
C
      ELSE
C
C See if Semper is running in batch mode
C
         IF (BATCH .NE. 0.0) THEN
C
C Read input from standard input stream
C
            IF (READSI(TEXT,N)) THEN
               IDERR = 2
               IDERR2 = 0
               GOTO 100
            ENDIF
C
C Check for end-of-file condition
C
            IF (N .LT. 0) GOTO 70
C
C Determine non-blank length of input string
C
            IF (N .GT. 0) N = LNBLNK(TEXT(1:N))
C
C Otherwise, we require interactive input
C
         ELSE
C
C Reset line count for paging of terminal output
C
            IF (SEMSOP()) GOTO 60
C
C See if UIF is running
C
            IF (UIF .NE. 0.0) THEN
C
C If so, read the next batch of commands from UIF
C
               IF (UIFINP(TEXT,N)) GOTO 60
C
C Otherwise, read input directly from the terminal
C
            ELSE
C
C Construct appropriate prompt string
C
               I = MIN(LEN(PROMPT)+1,LEN(PSTR))
C
               PSTR(1:I-1) = PROMPT
C
               IF (FIRST.EQ.1) THEN
                  PSTR(I:I) = ' '
               ELSE
                  PSTR(I:I) = '+'
               ENDIF
C
C Output prompt string and read input from keyboard
C
               N=0
               IF (KLINE(PSTR(1:I),.TRUE.,TEXT,N)) THEN
                  IF (ERROR.EQ.BRKERR) THEN
                     GOTO 60
                  ELSE
                     IDERR = 3
                     IDERR2 = ERROR
                     GOTO 100
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
C
C Check for breaks
C
   40 IF (ABANDN(ERROR)) GOTO 60
C
C Fault command buffer overflow
C
      LAST = FIRST + N - 1
C
      IF (LAST .GT. BUFSIZ) THEN
         ERROR=21
         GOTO 60
      ENDIF
C
C Copy characters (if any) to return buffer
C
      CALL SEMICS(TEXT,BUFFER(FIRST),N)
C
C Check for line continuation character "+"
C
      IF (LAST .GT. 0) THEN
         IF (BUFFER(LAST) .EQ. KPLUS) THEN
C
C Read another line and concatenate
C
            FIRST = LAST
            GOTO 20
         ENDIF
      ENDIF
C
C Convert any TAB characters to spaces
C
      DO 50 I = 1,LAST
         IF (BUFFER(I) .EQ. KBTAB) BUFFER(I) = KSPACE
   50 CONTINUE
C
C Return number of characters in command buffer
C
      BUFSIZ = LAST
C
      SEMPRG = .FALSE.
C
   60 RETURN
C
C End-of-file reached
C
   70 IF (FIRST .EQ. 1) THEN
         TEXT(1:3) = 'END'
         N=3
      ELSE
         TEXT(1:4) = ';END'
         N=4
      ENDIF
      GOTO 40
C
C Fortran READ error detected
C
   80 ERROR = 77
      WRITE (IDMESS,90) IOS
   90 FORMAT ('Fortran error on reading file, status = ',I6)
      GOTO 60
C
C Genuine error detected during interactive/batch input, in which case
C Semper cannot continue, so flag this as fatal error
C
  100 ERROR = FATERR
      IDMESS = 'SEMPRG'
      GOTO 60
C
C Copyright (C) 1987-1996:  Synoptics Ltd, All Rights Reserved
C
      IJUNK = DUMMY
      END
