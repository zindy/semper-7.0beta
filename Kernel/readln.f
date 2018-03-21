C Semper 6 utility routine READLN
C
      LOGICAL FUNCTION READLN(TEXT,N,IERROR)
C
      INTEGER TEXT(*),N,IERROR
C
C Cobbled together code taken directly from KLINE simply to maintain
C support for READLN in version 6.3.  Will disappear in later versions.
C
      LOGICAL EQNQRE,EQSETS,EQNEXT,EQREAD
      LOGICAL MOVELN,SEMSOL,SEMEOL,SEMTFL,SEMBEE
      INTEGER LNBLNK
C
      LOGICAL QKSET,LBREAK,DONE,INSERT
      INTEGER I,KEY,POS,NEWPOS,STATEK,IDUM,QUEUE,BUFI,BUFP,NS
      CHARACTER*80 STRING
C
      INCLUDE 'COMMON'
      INCLUDE 'EVENTS'
C
      READLN=.TRUE.
C
      QKSET=.FALSE.
      LBREAK=.FALSE.
      DONE=.FALSE.
      INSERT=.FALSE.
      BUFI=KBUFN+1
      NS=0
      POS=1
      STRING=' '
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
            IERROR=4
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
C
C Return string data
C
               N=NS
               CALL SEMICS(STRING,TEXT,N)
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
                  IF (MOVELN(STRING,POS,POS-1)) GOTO 40
                  IF (MOVELN(STRING,POS-1,NS+1)) GOTO 40
                  IF (MOVELN(STRING,NS+1,NEWPOS)) GOTO 40
                  NS=NS-1
                  POS=NEWPOS
               ENDIF
            ELSE IF (KEY.EQ.KBKILL) THEN
C
C Delete entire line
C
               STRING=' '
               NEWPOS=1
               IF (MOVELN(STRING,POS,1)) GOTO 40
               IF (MOVELN(STRING,1,NS+1)) GOTO 40
               IF (MOVELN(STRING,NS+1,NEWPOS)) GOTO 40
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
               IF (MOVELN(STRING,POS,NEWPOS)) GOTO 40
               POS=NEWPOS
            ELSE IF (KEY.EQ.KBEND) THEN
C
C Move to end of line
C
               NEWPOS=MIN(NS+1,N+1)
               IF (MOVELN(STRING,POS,NEWPOS)) GOTO 40
               POS=NEWPOS
            ELSE IF (KEY.EQ.KBREFL) THEN
C
C Refresh line
C
               IF (MOVELN(STRING,POS,NS+1)) GOTO 40
               IF (SEMEOL()) GOTO 40
               IF (SEMSOL()) GOTO 40
               IF (MOVELN(STRING,1,NS+1)) GOTO 40
               IF (MOVELN(STRING,NS+1,POS)) GOTO 40
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
                     IF (MOVELN(STRING,POS,1)) GOTO 40
                     IF (MOVELN(STRING,1,NS+1)) GOTO 40
                     IF (MOVELN(STRING,NS+1,NEWPOS)) GOTO 40
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
                     IF (MOVELN(STRING,POS,1)) GOTO 40
                     IF (MOVELN(STRING,1,NS+1)) GOTO 40
                     IF (MOVELN(STRING,NS+1,NEWPOS)) GOTO 40
                     NS=LNBLNK(STRING)
                     POS=NEWPOS
                  ELSE
                     BUFI=KBUFN+1
C
                     STRING=' '
                     NEWPOS=1
                     IF (MOVELN(STRING,POS,1)) GOTO 40
                     IF (MOVELN(STRING,1,NS+1)) GOTO 40
                     IF (MOVELN(STRING,NS+1,NEWPOS)) GOTO 40
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
                  IF (MOVELN(STRING,POS,NEWPOS)) GOTO 40
                  POS=NEWPOS
               ENDIF
            ELSE IF (KEY.EQ.KBRITE) THEN
C
C Move cursor one position to the right
C
               IF (POS.LE.NS) THEN
                  NEWPOS=POS+1
                  IF (MOVELN(STRING,POS,NEWPOS)) GOTO 40
                  POS=NEWPOS
               ENDIF
            ELSE IF (KEY.GE.KSPACE.AND.KEY.LE.KTILDE) THEN
C
C Printing Ascii key code - insert or replace at current position
C
               IF (POS.LE.N) THEN
                  NEWPOS=POS+1
                  IF (INSERT) THEN
                     IF (NS.EQ.N) NS=NS-1
                     DO 30 I=NS,POS,-1
                        STRING(I+1:I+1)=STRING(I:I)
   30                CONTINUE
                     STRING(POS:POS)=CHAR(KEY)
                     IF (MOVELN(STRING,POS,NS+2)) GOTO 40
                     IF (MOVELN(STRING,NS+2,NEWPOS)) GOTO 40
                     NS=NS+1
                  ELSE
                     STRING(POS:POS)=CHAR(KEY)
                     IF (MOVELN(STRING,POS,NEWPOS)) GOTO 40
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
         ENDIF
      ENDIF
C
C Go back for more?
C
      IF (.NOT.(DONE.OR.LBREAK)) GOTO 10
C
C Tidy up the screen
C
      IF (MOVELN(STRING,POS,NS+1)) GOTO 40
      IF (SEMEOL()) GOTO 40
      IF (SEMTFL()) GOTO 40
C
C Restore keyboard event queue state
C
      QKSET=.FALSE.
      IF (EQSETS(MKEY,STATEK)) GOTO 50
C
      READLN=LBREAK
C
   40 RETURN
C
C Events error
C
   50 IERROR=161
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
C Semper 6 utility routine MOVELN
C
      LOGICAL FUNCTION MOVELN(STRING,POS,NEWPOS)
C
      CHARACTER*(*) STRING
      INTEGER POS,NEWPOS
C
C Moves text cursor from position POS to NEWPOS.  Forward positioning is
C done by outputing text.  Backward positioning is done by backspacing.
C
      LOGICAL SEMTBS,SEMTOU,SEMTFL
C
      MOVELN=.TRUE.
C
      IF (NEWPOS.LT.POS) THEN
         IF (SEMTBS(POS-NEWPOS)) GOTO 10
      ELSE IF (NEWPOS.GT.POS) THEN
         IF (SEMTOU(STRING(POS:NEWPOS-1))) GOTO 10
      ENDIF
C
      IF (SEMTFL()) GOTO 10
C
      MOVELN=.FALSE.
C
   10 RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
