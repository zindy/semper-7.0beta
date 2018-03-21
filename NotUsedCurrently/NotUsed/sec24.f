      SUBROUTINE VAMPR2
C
C  SYNTAX:   Va2 :VAMPR2  verify  keyboard
C
      LOGICAL FSINIT,FSFLUS,SEMCON,VAMDRW,VAMERA,OPT,VARSET
      LOGICAL KPRESS,EVFLUS,KREAD,EVOPEN,EVCLOS
      INTEGER IPACK,TIME(7)
C
      INCLUDE 'COMMON'
      REAL X,Y,SEED
      INTEGER DUMMY,KERAS,KNOERA,KEY,NKBD
      LOGICAL ERASE,KBD,VERIFY
C
C  Key codes for erase/noerase options
C
      IF(SEMCON(' Press a key to define erase function ')) RETURN
      IF(EVFLUS(.TRUE.,.TRUE.,.FALSE.)) RETURN
      IF(KPRESS(KERAS)) RETURN
      IF(SEMCON(' Press a key to define noerase        ')) RETURN
      IF(EVFLUS(.TRUE.,.TRUE.,.FALSE.)) RETURN
      IF(KPRESS(KNOERA)) RETURN
C
C  pick up verify/keyboard/erase options
C
      VERIFY=OPT(IPACK('VERIFY'))
C
      NKBD=IPACK('KEYBOARD')
      IF(VARSET(NKBD)) THEN
         KBD=OPT(NKBD)
      ELSE
         KBD=.TRUE.
      ENDIF
C
      IF(SEMCON(' Please choose erase/noerase key')) RETURN
      IF(EVFLUS(.TRUE.,.TRUE.,.FALSE.)) RETURN
      IF(KPRESS(KEY)) RETURN
      IF(KEY .EQ. KERAS) THEN
         ERASE=.TRUE.
      ELSE IF(KEY .EQ. KNOERA) THEN
         ERASE=.FALSE.
      ELSE
         ERROR=10
         RETURN
      ENDIF
C
C  initialise graphics frame 1
C
      IF(FSINIT(1,1)) RETURN
C
C  initial vampire at origin
C
      X=0.0
      Y=0.0
      IF(VAMDRW(X,Y)) RETURN
      IF(FSFLUS()) RETURN
C
C  initial seed for random number generator between 0 and 1
C
      CALL MCTIME(TIME)
      IF(TIME(7) .NE. 0) THEN
        SEED=REAL(TIME(7))/100.0+REAL(TIME(6))/60.0/100.0
      ELSE
        SEED=REAL(TIME(6))/60.0+REAL(TIME(5))/60.0/100.0
      ENDIF
C
C  start interactive key/mouse button
C
      IF(EVOPEN(KBD,.TRUE.,.FALSE.)) RETURN
C
   10 CONTINUE
C
C
         IF(ERASE) THEN
            IF(VAMERA(X,Y)) GOTO 30
         ENDIF
C  move
         CALL VAMINC(X,Y,SEED,.NOT. ERASE)
C  draw
         IF(VAMDRW(X,Y)) GOTO 30
         IF(FSFLUS()) GOTO 30
C  delay if 'animation'
         IF(ERASE) THEN
            IF(VAMDRW(X,Y)) GOTO 30
         ENDIF
C
C  key processing
C
         IF(KREAD(KEY)) GOTO 30
C
C  show key presses
C
         IF(VERIFY) THEN
            WRITE(RECORD,20) KEY
   20       FORMAT( 'Key press = ',I4)
            IF(SEMCON(RECORD)) GOTO 30
         ENDIF
C
         IF(KEY .EQ. KERAS) THEN
            ERASE=.TRUE.
         ELSE IF(KEY .EQ. KNOERA) THEN
            ERASE=.FALSE.
         ENDIF
C
C
      GOTO 10
C
   30 CONTINUE
C
C  error occurred after EVOPEN: restore original event status
C
      DUMMY=ERROR
      ERROR=0
      DUMLOG=EVCLOS()
      ERROR=DUMMY
C
      RETURN
      END
