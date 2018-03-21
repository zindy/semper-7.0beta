C Semper 6 system module SEMMOR
C
      LOGICAL FUNCTION SEMMOR(TEXT,N)
C
      CHARACTER*(*) TEXT
      INTEGER N
C
C Outputs text to the terminal, outputting a page prompt if the line
C about to be output would fill the terminal page. The page prompt can
C be turned on or off with the command PAGE PROMPT or PAGE NOPROMPT.
C After the page prompt appears, a key or button press will cause the
C prompt to disappear.  The key stroke/button press determines whether
C this line and any subsequent lines up to the next command are output.
C The page size is determined by calling SEMTPS.  This will return the
C size set by the PAGE command, unless the terminal window size changes
C dynamically and can be obtained by interrogating the host system or
C terminal, in which case the size is truncated to the hardware size.
C
      LOGICAL INKEY,SEMBEE
      LOGICAL SEMSOL,SEMTOU,SEMEOL,SEMTFL,SEMTCR,SEMTNV,SEMTRV,SEMTPS
C
      INTEGER IWID,ILEN,KEY
C
      INCLUDE 'COMMON'
C
      CHARACTER*68 BLANKS
      DATA BLANKS / ' ' /
C
      SEMMOR = .TRUE.
C
C Fetch current terminal page size
C
      IF (SEMTPS(IWID,ILEN)) GOTO 20
C
C Increment line count for terminal output
C
      TERCNT = TERCNT + 1
C
C See if this line would fill terminal page
C
      IF (TERCNT .GE. ILEN-1) THEN
C
C See if page prompt is enabled
C
         IF (TERPRO) THEN
C
C Output start-of-line sequence
C
            IF (SEMSOL()) GOTO 20
C
C Output escape sequence to turn on inverse video mode
C
            IF (SEMTRV()) GOTO 20
C
C Output prompt string
C
            IF (SEMTOU(TERSTR(1:MIN(TERPLN,IWID)))) GOTO 20
C
C Output escape sequence to turn off inverse video mode
C
            IF (SEMTNV()) GOTO 20
C
C Force output of text to the terminal
C
            IF (SEMTFL()) GOTO 20
C
C Wait for keystroke (or button press), with special check for abandon
C
   10       IF (INKEY(KEY,ERROR)) THEN
               IF (ERROR .EQ. BRKERR) THEN
                  TERCNT = 0
               ELSE
                  GOTO 20
               ENDIF
            ELSE
C
C And then process key code
C
               IF (KEY.EQ.KSPACE .OR. KEY.EQ.KMBUT+TERLBN) THEN
C
C SPACE - allow another page full
C
                  TERCNT = 1
               ELSE IF (KEY.EQ.KBRET .OR. KEY.EQ.KMBUT+TERPBN) THEN
C
C RETURN - allow one more line
C
                  TERCNT = TERCNT - 1
               ELSE IF (TERQUI .AND. (KEY.EQ.KUCQ .OR.
     +                                KEY.EQ.KLCQ .OR.
     +                                KEY.EQ.KMBUT+TERQBN)) THEN
C
C (Q)uit - reset the line count and return a special error code
C (successful return to command interpreter depends on .TRUE. return
C being handled correctly throughout all levels of Semper!)
C
                  TERCNT = 0
                  ERROR = PAGERR
               ELSE
C
C Anything else - sound bell and ignore key press
C
                  IF (SEMBEE()) GOTO 20
                  GOTO 10
               ENDIF
            ENDIF
C
C Erase page prompt string
C
            IF (SEMTCR()) GOTO 20
            IF (SEMTOU(BLANKS(1:MIN(TERPLN,IWID)))) GOTO 20
            IF (SEMTCR()) GOTO 20
            IF (SEMTFL()) GOTO 20
C
C Set carriage-control flags to suppress CR/LF (to cancel the effect of
C any subsequent call to SEMSOL because we are already at start-of-line)
C
            TERCR=.TRUE.
            TERLF=.TRUE.
C
C Return if abandon or Quit
C
            IF (ERROR .NE. 0) GOTO 20
         ELSE
C
C No page prompt - set line count to allow another page full
C
            TERCNT=1
         ENDIF
      ENDIF
C
C Output line of text to terminal
C
      IF (SEMSOL()) GOTO 20
C
      IF (N.GT.0) THEN
         IF (SEMTOU(TEXT(1:N))) GOTO 20
      ENDIF
C
      IF (SEMEOL()) GOTO 20
C
C Force output of text to the terminal
C
      IF (SEMTFL()) GOTO 20
C
      SEMMOR = .FALSE.
C
   20 RETURN
C
C Copyright (C) 1989-1993:  Synoptics Ltd,  All Rights Reserved
C
      END
