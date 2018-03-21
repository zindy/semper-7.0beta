      SUBROUTINE VAMPIR
C
C  SYNTAX:   Vampire :VAMPIR
C
      LOGICAL FSINIT,FSFLUS,OPT,SEMBRK,VAMDRW,VAMERA
      INTEGER IPACK,TIME(7),NBREAK
C
      INCLUDE 'COMMON'
      REAL X,Y,SEED
      LOGICAL ERASE
C
C  pick up erase option
C
      ERASE=OPT(IPACK('ERASE'))
C
C  number of abandon requests
C
      NBREAK=0
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
C
   10 CONTINUE
C
C
         IF(ERASE) THEN
            IF(VAMERA(X,Y)) RETURN
         ENDIF
C  move
         CALL VAMINC(X,Y,SEED,.NOT. ERASE)
C  draw
         IF(VAMDRW(X,Y)) RETURN
         IF(FSFLUS()) RETURN
C  delay if 'animation'
         IF(ERASE) THEN
            IF(VAMDRW(X,Y)) RETURN
         ENDIF
C  break processing (vampire has 16 lives)
         IF(SEMBRK()) THEN
            IF(ERROR .NE. 4) RETURN
            NBREAK=NBREAK+1
            IF(NBREAK .GT. 15) RETURN
            ERROR=0
         ENDIF
C
C
      GOTO 10
C
      END
C
C
C
      LOGICAL FUNCTION VAMDRW(X,Y)
C
C  draw overlay vampire
C
      LOGICAL FSCURV,FSCIRC
C
      REAL X,Y,XX(6),YY(6)
      INTEGER I
C
      VAMDRW=.TRUE.
C
C  make teeth
C
      DO 10 I=1,6
         YY(I)=Y-8.0
   10 CONTINUE
      XX(1)=X-9.0
      XX(2)=X+9.0
      XX(3)=X+7.0
      YY(3)=Y-15.0
      XX(4)=X+5.0
      XX(5)=X-5.0
      XX(6)=X-7.0
      YY(6)=Y-15.0
C
C  draw head
C
      IF(FSCIRC(X,Y,20.0)) RETURN
C
C  draw eyes
C
      IF(FSCIRC(X-5.0,Y+7.0,4.0)) RETURN
      IF(FSCIRC(X+5.0,Y+7.0,4.0)) RETURN
C
C  (bloodshot)
C
      IF(FSCIRC(X-5.0,Y+7.0,2.0)) RETURN
      IF(FSCIRC(X+5.0,Y+7.0,2.0)) RETURN
C
C
C  draw teeth
C
      IF(FSCURV(XX,YY,6,.TRUE.)) RETURN
C
      VAMDRW=.FALSE.
C
      RETURN
      END
C
C
C
      LOGICAL FUNCTION VAMERA(X,Y)
C
C  erase vampire at X,Y
C
      LOGICAL FSERAS
      REAL X,Y
C
      VAMERA=.TRUE.
C
      IF(FSERAS(2,X-20.0,X+20.0,Y-20.0,Y+20.0)) RETURN
C
      VAMERA=.FALSE.
C
      RETURN
      END
C
C
      SUBROUTINE VAMINC(X,Y,SEED,JUMP)
C
C  change X,Y a little
C
      REAL X,Y,DX,DY,SEED,TEMP
      LOGICAL JUMP
C
C .. cheap and nasty chaotic number generator
C
      IF(SEED .GE. 1.0) SEED=.999
      IF(SEED .LE. 0.0) SEED=.001
      SEED=4.0*SEED*(1.0-SEED)
      TEMP=2.0*(SEED-0.5)
      DX=10.0*TEMP
C
C  if jump then make increment much larger
C
      IF(JUMP) DX=8.0*DX
C
C  do not head off into hyperspace
C
      IF(ABS(X+DX) .GT. 100.0) THEN
         X=X-DX
      ELSE
         X=X+DX
      ENDIF
C
C  use second significant figure of SEED for Y increment
C
      TEMP=SEED*10.0
      TEMP=TEMP-AINT(TEMP)
      TEMP=2.0*(TEMP-0.5)
      DY=10.0*TEMP
C  big jump?
      IF(JUMP) DY=8.0*DY
C  no hyperspace
      IF(ABS(Y+DY) .GT. 100.0) THEN
         Y=Y-DY
      ELSE
         Y=Y+DY
      ENDIF
C
      RETURN
      END
