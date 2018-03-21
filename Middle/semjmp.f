C Semper 6 system module SEMJMP
C
      LOGICAL FUNCTION SEMJMP(NAME,LABRET,LABONE,STRICT)
C
      INTEGER NAME,LABRET,LABONE
      LOGICAL STRICT
C
C Searches for label NAME in LINDEX, starting from slot LABONE
C Search is restricted to start FOR level if STRICT is TRUE
C Matched entry number is returned in LABRET
C
      INCLUDE 'COMMON'
C
      INTEGER I,LABNUM,LABPTR,LABENT,LABFOR
      PARAMETER (LABENT=4)
C
      SEMJMP = .TRUE.
      LABNUM = LABONE
      LABFOR = 0
   10 LABPTR = LABNUM*LABENT
      I = LINDEX(LABPTR+1)
      IF (I .EQ. TIDFOR) THEN
         IF (STRICT) LABFOR = LABFOR + 1
      ELSE IF (I .EQ. TIDLOO) THEN
         IF (STRICT) LABFOR = LABFOR - 1
      ELSE IF (I .EQ. TIDLAB) THEN
         IF (LABFOR .EQ. 0) THEN
            IF (LINDEX(LABPTR+4) .EQ. NAME) THEN
               SEMJMP = .FALSE.
               LABRET = LABNUM
               GOTO 20
            ENDIF
         ENDIF
      ENDIF
      IF (LABFOR .GE. 0 .AND. I .NE. TIDEND) THEN
         LABNUM = LABNUM + 1
         GOTO 10
      ENDIF
C
   20 RETURN
C
C Copyright (C) 1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
