      SUBROUTINE KEYPRS
C
C  kpress exerciser program
C
C   SYNTAX    Key :KEYPRS
C
      LOGICAL KPRESS,SEMCON
      INCLUDE 'COMMON'
      INTEGER KEY,MOUSE,KFUNC,IERROR
C
C  KMBUT is a parameter in ICSET indicating mouse button pressed
C  KBFUNC indicates a function key
C
      MOUSE=KMBUT
      KFUNC=KBFUNC
C
      IF(SEMCON(' Kpress exerciser program ')) RETURN
      IF(SEMCON(' ')) RETURN
C
   10 CONTINUE
C
C  get button press
C
         IF(KPRESS(KEY)) THEN
C
C  error: tell us about it
C
            IERROR=ERROR
            ERROR=0
            IF(SEMCON(' *** Kpress returned .TRUE. ')) RETURN
            ERROR=IERROR
            RETURN
         ENDIF
C
C  Check if special key
C
         IF(KEY .EQ. KBRET) THEN
            IF(SEMCON('RETURN key pressed')) RETURN
         ELSE IF(KEY .EQ. KSPACE) THEN
            IF(SEMCON('Space bar pressed')) RETURN
         ELSE IF(KEY .EQ. KBTAB) THEN
            IF(SEMCON('TAB key pressed')) RETURN
         ELSE IF(KEY .EQ. KBDEL) THEN
            IF(SEMCON('DELETE key pressed')) RETURN
         ELSE IF(KEY .EQ. KBKILL) THEN
            IF(SEMCON('DELETE-LINE key pressed')) RETURN
         ELSE IF(KEY .EQ. KBINS) THEN
            IF(SEMCON('INSERT/OVERWRITE toggle key pressed')) RETURN
         ELSE IF(KEY .EQ. KBHOME) THEN
            IF(SEMCON('HOME key pressed')) RETURN
         ELSE IF(KEY .EQ. KBEND) THEN
            IF(SEMCON('END key pressed')) RETURN
         ELSE IF(KEY .EQ. KBREFL) THEN
            IF(SEMCON('REFRESH-LINE key pressed')) RETURN
         ELSE IF(KEY .EQ. KBUP) THEN
            IF(SEMCON('CURSOR-UP key pressed')) RETURN
         ELSE IF(KEY .EQ. KBDOWN) THEN
            IF(SEMCON('CURSOR-DOWN key pressed')) RETURN
         ELSE IF(KEY .EQ. KBLEFT) THEN
            IF(SEMCON('CURSOR-LEFT key pressed')) RETURN
         ELSE IF(KEY .EQ. KBRITE) THEN
            IF(SEMCON('CURSOR-RIGHT key pressed')) RETURN
         ELSE IF(KEY .LT. KMBUT .AND. KEY .LT. KBFUNC) THEN
C
C  no-special keyboard input
C
            WRITE(RECORD,20) KEY
   20       FORMAT('                          Key code ',I4,' detected')
            IF(SEMCON(RECORD)) RETURN
C
         ELSE
C
C  check for mouse / function keys
C
            IF(KMBUT .GT. KBFUNC) THEN
C  mouse key
               IF(KEY .GT. KMBUT) GOTO 30
C  function key
               IF(KEY .GT. KBFUNC) GOTO 50
C
            ELSE
C  function key
               IF(KEY .GT. KBFUNC) GOTO 50
C  mouse key
                IF(KEY .GT. KMBUT) GOTO 30
            ENDIF
C
C  mouse button
C
   30       CONTINUE
            WRITE(RECORD,40) KEY-MOUSE,KEY,MOUSE,KEY-MOUSE
   40       FORMAT('Mouse button ',I2,' pressed.  Key code ',I4,'=',
     +                                                       I4,'+',I2)
            IF(SEMCON(RECORD)) RETURN
            GOTO 70
C
C  function key
C
   50       CONTINUE
            WRITE(RECORD,60) KEY-KFUNC,KEY,KFUNC,KEY-KFUNC
   60       FORMAT('Function key ',I2,' pressed.  Key code ',I4,'=',
     +                                                       I4,'+',I2)
            IF(SEMCON(RECORD)) RETURN
            GOTO 70
C
C
         ENDIF
   70    CONTINUE
C
      GOTO 10
C
      END
