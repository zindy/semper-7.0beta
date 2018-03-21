C Semper 6 processing module SEMFOR
C---------------------------------------------------------------------
C
C      SUBROUTINE SEMFOR
C      -----------------
C
C      PARAMETERS:
C
C      None.
C
C      Handles command FOR
C
C---------------------------------------------------------------------
C
      SUBROUTINE SEMFOR
C     =================
C
      INTEGER IVAL
      LOGICAL SEMENV,SEMLU,SEMXA1,SEMXPL
C
      INCLUDE 'COMMON'
C
      INTEGER NDOLLR
      PARAMETER (NDOLLR=-11201)
C
      REAL VX(3),START,FINAL,STEP,SPAN,X
      EQUIVALENCE (START,VX(1)),(FINAL,VX(2)),(STEP,VX(3))
C
      INTEGER*4 COUNT
C
      INTEGER IPTR,N,NAME,LABFOR,LABNUM,LABPTR,LABENT
      PARAMETER (LABENT=4)
      LOGICAL FORFND
C
      IPTR = IVAL(NDOLLR)
      IF (IPTR .EQ. 0) GOTO 30
C
C FOR processing - establish a loop beginning next command
C
      IF (SEMXA1(1,LINBUF,COMLIM,IPTR,X,NAME)) CONTINUE
      IF (ERROR .NE. 0) GOTO 20
C
C Skip any equals sign present
C
      IF (SEMXA1(0,LINBUF,COMLIM,IPTR,X,N)) GOTO 30
      IF (N .EQ. KEQUAL) IPTR=IPTR+1
C
C Read loop limits and step
C
      N = 3
      IF (SEMXPL(LINBUF,COMLIM,IPTR,VX,N)) GOTO 20
      IF (IPTR .LE. COMLIM) GOTO 30
      IF (N .LT. 2 .OR. (N .GT. 2 .AND. STEP .EQ. 0.)) THEN
C
C Bad FOR loop
C
         ERROR = 36
         GOTO 20
      ENDIF
C
      IF (N .EQ. 2) THEN
C
C Supply default step
C
         IF (START .GT. FINAL) THEN
            STEP = -1.
         ELSE
            STEP =  1.
         ENDIF
      ENDIF
C
C Establish loop
C
      IF (FORLEV .EQ. FORMAX) THEN
         ERROR = 109
         GOTO 20
      ENDIF
      FORLEV = FORLEV + 1
      FORDEV(FORLEV) = LINDEV
      FORSLT(FORLEV) = LINSLT
      FORLIN(FORLEV) = LINNUM
      FORLOC(FORLEV) = LOCLEV
      FORVAR(FORLEV) = NAME
C
C Save loop variable and set first value
C
      IF (SEMLU(2,NAME,START)) GOTO 20
      VRBLEV = LOCLEV
C
C Set loop count (integer) and step
C
      SPAN = (FINAL-START)
      FORSTP(FORLEV) = STEP
      X = SPAN/STEP
C
C Check for non-zero span with increment of opposite sign
C
      IF (SPAN .NE. 0) THEN
         IF (SPAN .LT. 0.0) THEN
            IF (STEP .GT. 0.0) THEN
               COUNT = -1
            ELSE
               COUNT = X
            ENDIF
         ELSE
            IF (STEP .LT. 0.0) THEN
               COUNT = -1
            ELSE
               COUNT = X
            ENDIF
         ENDIF
      ELSE
         COUNT = 0
      ENDIF
      FORCNT(FORLEV) = COUNT
C
C Recalculate end value based on step and count
C
      X = START + (REAL(COUNT) * STEP)
      IF (STEP .GT. 0.0) THEN
         IF (X .LT. FINAL) FINAL = X
      ELSE
         IF (X .GT. FINAL) FINAL = X
      ENDIF
      FOREND(FORLEV) = FINAL
      FOROFF(FORLEV) = NEXTSC
C
C Now find the matching LOOP instruction
C
      LABNUM = 0
      FORFND = .FALSE.
   10 LABPTR = LABNUM*LABENT
      N = LINDEX(LABPTR+1)
      IF (N .EQ. TIDEND) THEN
         ERROR = 106
         GOTO 20
      ELSE IF (N .EQ. TIDFOR) THEN
         IF (FORFND) THEN
            LABFOR = LABFOR + 1
         ELSE IF (LINDEX(LABPTR+2) .EQ. LINNUM .AND.
     +            LINDEX(LABPTR+3) .EQ. PREVSC+1) THEN
            FORFND = .TRUE.
            FORFSL(FORLEV) = LABNUM
            LABFOR = 0
         ENDIF
      ELSE IF (N .EQ. TIDLOO) THEN
         IF (FORFND) THEN
            IF (LABFOR .EQ. 0) THEN
C
C We've found a (matching ?) LOOP
C
               N = LINDEX(LABPTR+4)
               IF (N .EQ. 0 .OR. N .EQ. NAME) THEN
                  FORLSL(FORLEV) = LABNUM
C
C Check for no trip loops
C
                  IF (FORCNT(FORLEV) .LT. 0) THEN
C
C Establish environment to point to LOOP
C
                     LABNUM = FORLSL(FORLEV)
                     LABPTR = LABNUM*LABENT
                     IF (.NOT.SEMENV(FORDEV(FORLEV),
     +                               FORSLT(FORLEV),
     +                               LINDEX(LABPTR+2)))
     +                                   LASTSC = LINDEX(LABPTR+3)-1
                  ENDIF
                  GOTO 20
               ENDIF
            ELSE
               LABFOR = LABFOR - 1
            ENDIF
         ENDIF
      ENDIF
      LABNUM = LABNUM + 1
      GOTO 10
C
   20 RETURN
C
C Syntax error
C
   30 ERROR = 17
      GOTO 20
C
C Copyright (C) 1987,1988,1989,1991:  Synoptics Ltd, All Rights Reserved
C
      END
