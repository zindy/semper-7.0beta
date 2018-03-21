C Semper 6 utility routine KLINE
C
      LOGICAL FUNCTION KLINE(PROMPT,LP,STRING,NS)
C
      CHARACTER*(*) PROMPT,STRING
      LOGICAL LP
      INTEGER NS
C
C Obtains a line of input from the keyboard.  If NS is greater than
C zero, that number of characters from STRING are displayed as a
C default input string.  Also implements line editing and line recall
C facilities.  If LP is TRUE, the prompt string in PROMPT is output at
C the start of the line.  The result is returned in character buffer
C STRING and the significant length of the string is returned in NS.
C Keyboard input is only buffered up to the maximum length of STRING.
C
C KLINE responds to key presses in the following way:
C
C Key code       Action
C ========       ======
C
C KBRET       Terminate input
C KBDEL       Backspace and delete character
C KBKILL      Delete entire line
C KBINS       Toggle between insert/replace modes
C KBHOME      Move to start of line
C KBEND       Move to end of line
C KBREFL      Redisplay the line
C KBUP        Recall previous line from buffer
C KBDOWN      Recall next line from buffer
C KBLEFT      Move one character to left
C KBRITE      Move one character to right
C KSPACE      ]
C   to        ] Insert/replace character at current position
C KTILDE      ]
C
C Any other key press is ignored and causes the bell to be sounded.
C
C On entry to KLINE, line editing works in replace mode.  Pressing
C KBINS toggles into and out of insert mode.
C
C The line buffer is a linear buffer of lines previously input.  Any
C attempt to step beyond the top or bottom of the buffer is ignored.
C Stepping up and then down past the last line previously input causes
C the current input line to be cleared.
C
      LOGICAL EQNQRE,EQSETS,EQNEXT,EQREAD
      LOGICAL KMOVE,SEMSOL,SEMEOL,SEMTOU,SEMTFL,SEMBEE,SEMSOP
      INTEGER LNBLNK
C
      LOGICAL QKSET,LBREAK,DONE,INSERT
      INTEGER I,KEY,POS,NEWPOS,STATEK,IDUM,QUEUE,BUFI,BUFP
C
      INCLUDE 'COMMON'
      INCLUDE 'EVENTS'
C
      KLINE=.TRUE.
C
      QKSET=.FALSE.
      LBREAK=.FALSE.
      DONE=.FALSE.
      INSERT=.FALSE.
      BUFI=KBUFN+1
C
      NS=MIN(MAX(0,NS),LEN(STRING))
      IF (NS.LT.LEN(STRING)) THEN
         STRING(NS+1:)=' '
      ENDIF
      POS=NS+1
C
C Flag start of a new page - resets line count for paging
C
      IF (SEMSOP()) GOTO 40
C
C Output start-of-line sequence
C
      IF (SEMSOL()) GOTO 40
C
C Output prompt string (if any)
C
      IF (LP) THEN
         IF (SEMTOU(PROMPT)) GOTO 40
      ENDIF
C
C Output default input string (if any)
C
      IF (NS.GT.0) THEN
         IF (SEMTOU(STRING(1:NS))) GOTO 40
      ENDIF
C
C Flush output to terminal
C
      IF (SEMTFL()) GOTO 40
C
C Save keyboard event queue state
C
      IF (EQNQRE(MKEY,STATEK,IDUM,IDUM)) GOTO 50
C
C Activate keyboard event queue
C
      IF (EQSETS(MKEY,QRUN)) GOTO 50
      QKSET=.TRUE.
C
C Interrogate break and keyboard event queues
C
   10 IF (EQNEXT(QUEUE)) THEN
C
C All queues are empty - try again after short wait
C
         CALL WAITS(0.05)
      ELSE
         IF (QUEUE.EQ.MBREAK) THEN
C
C Break detected, clean out break queue and flag break
C
            IF (EQREAD(MBREAK,QTAKE,IDUM,IDUM,IDUM,IDUM)) GOTO 50
            ERROR=4
            LBREAK=.TRUE.
         ELSE IF (QUEUE.EQ.MKEY) THEN
C
C Key pressed, fetch key code
C
            IF (EQREAD(MKEY,QTAKE,IDUM,KEY,IDUM,IDUM)) GOTO 50
C
C Process key code
C
            IF (KEY.EQ.KBRET) THEN
C
C Finished
C
               DONE=.TRUE.
C
C Strip any trailing blanks
C
               NS=LNBLNK(STRING)
C
C Store the result in the keyboard buffer if it is non-blank and it
C differs from the last line to be input
C
C       LDM Change to prevent unitialized KBUFP problems
        KBUFP = MAX( KBUFP , 1)
               IF (NS.NE.0.AND.STRING.NE.KBUFF(KBUFP)) THEN
                  IF (KBUFN.LT.KBUFM) KBUFN=KBUFN+1
                  IF (KBUFP.LT.KBUFM) THEN
                     KBUFP=KBUFP+1
                  ELSE
                     KBUFP=1
                  ENDIF
                  KBUFF(KBUFP)=STRING
               ENDIF
            ELSE IF (KEY.EQ.KBDEL) THEN
C
C Backspace and delete one character
C
               IF (POS.GT.1) THEN
                  DO 20 I=POS,NS
                     STRING(I-1:I-1)=STRING(I:I)
   20             CONTINUE
                  STRING(NS:NS)=' '
                  NEWPOS=POS-1
                  IF (KMOVE(PROMPT,LP,STRING,POS,POS-1)) GOTO 40
                  IF (KMOVE(PROMPT,LP,STRING,POS-1,NS+1)) GOTO 40
                  IF (KMOVE(PROMPT,LP,STRING,NS+1,NEWPOS)) GOTO 40
                  NS=NS-1
                  POS=NEWPOS
               ENDIF
            ELSE IF (KEY.EQ.KBKILL) THEN
C
C Delete entire line
C
               STRING=' '
               NEWPOS=1
               IF (KMOVE(PROMPT,LP,STRING,POS,1)) GOTO 40
               IF (KMOVE(PROMPT,LP,STRING,1,NS+1)) GOTO 40
               IF (KMOVE(PROMPT,LP,STRING,NS+1,NEWPOS)) GOTO 40
               NS=0
               POS=NEWPOS
            ELSE IF (KEY.EQ.KBINS) THEN
C
C Insert/overstrike toggle
C
               INSERT=.NOT.INSERT
            ELSE IF (KEY.EQ.KBHOME) THEN
C
C Move to start of line
C
               NEWPOS=1
               IF (KMOVE(PROMPT,LP,STRING,POS,NEWPOS)) GOTO 40
               POS=NEWPOS
            ELSE IF (KEY.EQ.KBEND) THEN
C
C Move to end of line
C
               NEWPOS=MIN(NS+1,LEN(STRING)+1)
               IF (KMOVE(PROMPT,LP,STRING,POS,NEWPOS)) GOTO 40
               POS=NEWPOS
            ELSE IF (KEY.EQ.KBREFL) THEN
C
C Refresh line
C
               IF (KMOVE(PROMPT,LP,STRING,POS,NS+1)) GOTO 40
               IF (SEMEOL()) GOTO 40
               IF (SEMSOL()) GOTO 40
               IF (LP) THEN
                  IF (SEMTOU(PROMPT)) GOTO 40
               ENDIF
               IF (KMOVE(PROMPT,LP,STRING,1,NS+1)) GOTO 40
               IF (KMOVE(PROMPT,LP,STRING,NS+1,POS)) GOTO 40
            ELSE IF (KEY.EQ.KBUP) THEN
C
C Display previous line from keyboard buffer
C
               IF (KBUFN.GT.0) THEN
                  IF (BUFI.GT.1) THEN
                     BUFI=BUFI-1
C
                     BUFP=KBUFP+BUFI
                     IF (BUFP.GT.KBUFN) BUFP=BUFP-KBUFN
                     STRING=KBUFF(BUFP)
                     NEWPOS=LNBLNK(STRING)+1
                     IF (KMOVE(PROMPT,LP,STRING,POS,1)) GOTO 40
                     IF (KMOVE(PROMPT,LP,STRING,1,NS+1)) GOTO 40
                     IF (KMOVE(PROMPT,LP,STRING,NS+1,NEWPOS)) GOTO 40
                     NS=LNBLNK(STRING)
                     POS=NEWPOS
                  ENDIF
               ENDIF
            ELSE IF (KEY.EQ.KBDOWN) THEN
C
C Display next line from keyboard buffer
C
               IF (KBUFN.GT.0) THEN
                  IF (BUFI.LT.KBUFN) THEN
                     BUFI=BUFI+1
C
                     BUFP=KBUFP+BUFI
                     IF (BUFP.GT.KBUFN) BUFP=BUFP-KBUFN
                     STRING=KBUFF(BUFP)
                     NEWPOS=LNBLNK(STRING)+1
                     IF (KMOVE(PROMPT,LP,STRING,POS,1)) GOTO 40
                     IF (KMOVE(PROMPT,LP,STRING,1,NS+1)) GOTO 40
                     IF (KMOVE(PROMPT,LP,STRING,NS+1,NEWPOS)) GOTO 40
                     NS=LNBLNK(STRING)
                     POS=NEWPOS
                  ELSE
                     BUFI=KBUFN+1
C
                     STRING=' '
                     NEWPOS=1
                     IF (KMOVE(PROMPT,LP,STRING,POS,1)) GOTO 40
                     IF (KMOVE(PROMPT,LP,STRING,1,NS+1)) GOTO 40
                     IF (KMOVE(PROMPT,LP,STRING,NS+1,NEWPOS)) GOTO 40
                     NS=0
                     POS=NEWPOS
                  ENDIF
               ENDIF
            ELSE IF (KEY.EQ.KBLEFT) THEN
C
C Move cursor one position to the left
C
               IF (POS.GT.1) THEN
                  NEWPOS=POS-1
                  IF (KMOVE(PROMPT,LP,STRING,POS,NEWPOS)) GOTO 40
                  POS=NEWPOS
               ENDIF
            ELSE IF (KEY.EQ.KBRITE) THEN
C
C Move cursor one position to the right
C
               IF (POS.LE.NS) THEN
                  NEWPOS=POS+1
                  IF (KMOVE(PROMPT,LP,STRING,POS,NEWPOS)) GOTO 40
                  POS=NEWPOS
               ENDIF
            ELSE IF (KEY.GE.KSPACE.AND.KEY.LE.KTILDE) THEN
C
C Printing Ascii key code - insert or replace at current position
C
               IF (POS.LE.LEN(STRING)) THEN
                  NEWPOS=POS+1
                  IF (INSERT) THEN
                     IF (NS.EQ.LEN(STRING)) NS=NS-1
                     DO 30 I=NS,POS,-1
                        STRING(I+1:I+1)=STRING(I:I)
   30                CONTINUE
                     STRING(POS:POS)=CHAR(KEY)
                     IF (KMOVE(PROMPT,LP,STRING,POS,NS+2)) GOTO 40
                     IF (KMOVE(PROMPT,LP,STRING,NS+2,NEWPOS)) GOTO 40
                     NS=NS+1
                  ELSE
                     STRING(POS:POS)=CHAR(KEY)
                     IF (KMOVE(PROMPT,LP,STRING,POS,NEWPOS)) GOTO 40
                     IF (POS.GT.NS) NS=NS+1
                  ENDIF
                  POS=NEWPOS
               ENDIF
            ELSE
C
C Not a recognised key code - sound the bell
C
               IF (SEMBEE()) GOTO 40
            ENDIF
         ELSE
C
C Some other queue (pointer?) - remove entry
C
            IF (EQREAD(QUEUE,QTAKE,IDUM,IDUM,IDUM,IDUM)) GOTO 50
         ENDIF
      ENDIF
C
C Go back for more?
C
      IF (.NOT.(DONE.OR.LBREAK)) GOTO 10
C
C Tidy up the screen
C
      IF (KMOVE(PROMPT,LP,STRING,POS,NS+1)) GOTO 40
      IF (SEMEOL()) GOTO 40
      IF (SEMTFL()) GOTO 40
C
C Restore keyboard event queue state
C
      QKSET=.FALSE.
      IF (EQSETS(MKEY,STATEK)) GOTO 50
C
      KLINE=LBREAK
C
   40 RETURN
C
C Events error
C
   50 ERROR=161
C
      IF (QKSET) THEN
         IF (EQSETS(MKEY,STATEK)) GOTO 40
      ENDIF
C
      GOTO 40
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 utility routine KMOVE
C
      LOGICAL FUNCTION KMOVE(PROMPT,LP,STRING,POS,NEWPOS)
C
      CHARACTER*(*) PROMPT,STRING
      LOGICAL LP
      INTEGER POS,NEWPOS
C
C Moves text cursor from position POS to NEWPOS.  Forward positioning is
C done by outputing text.  Backward positioning is done by backspacing
C or using carriage-return + forward positioning, whichever requires
C fewer output characters
C
      LOGICAL SEMTCR,SEMTBS,SEMTOU,SEMTFL
C
      KMOVE=.TRUE.
C
      IF (NEWPOS.LT.POS) THEN
         IF (POS.GT.LEN(PROMPT)+2*NEWPOS) THEN
            IF (SEMTCR()) GOTO 10
            IF (LP) THEN
               IF (SEMTOU(PROMPT)) GOTO 10
            ENDIF
            IF (NEWPOS .GT. 1) THEN
               IF (SEMTOU(STRING(1:NEWPOS-1))) GOTO 10
            ENDIF
         ELSE
            IF (SEMTBS(POS-NEWPOS)) GOTO 10
         ENDIF
      ELSE IF (NEWPOS.GT.POS) THEN
         IF (SEMTOU(STRING(POS:NEWPOS-1))) GOTO 10
      ENDIF
C
      IF (SEMTFL()) GOTO 10
C
      KMOVE=.FALSE.
C
   10 RETURN
C
C Copyright (C) 1990,1991:  Synoptics Ltd,  All Rights Reserved
C
      END
