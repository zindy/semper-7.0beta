C Semper 6 system module PRUCAS
C
      LOGICAL FUNCTION PRUCAS(NAME,NAMLEN)
C
C Change case of characters in NAME to those used by Program manager
C Also reset NAMLEN to reflect significant size of NAME
C
      INCLUDE 'COMMON'
C
      INTEGER NAMLEN,NAME(PRNACT)
      INTEGER I,CH,FIRST,LAST
C
      LOGICAL BAD
      CHARACTER*(PRNACT) ERR
C
      PRUCAS = .TRUE.
C
      BAD = .FALSE.
C
      IF (NAMLEN .GT. 0) THEN
         FIRST = 0
         LAST = 0
         DO 10 I = 1,NAMLEN
            CH = NAME(I)
            IF (CH .GE. KUCA .AND. CH .LE. KUCZ)
     +          NAME(I) = CH - KUCA + KLCA
            IF (CH .NE. KSPACE) THEN
               LAST = I
               IF (FIRST .EQ. 0) FIRST = I
            ENDIF
   10    CONTINUE
         NAMLEN = LAST
         IF (FIRST .GT. 1 .AND. LAST .NE. 0) THEN
            CH = 1
            DO 20 I = FIRST,LAST
               NAME(CH) = NAME(I)
               CH = CH + 1
   20       CONTINUE
            NAMLEN = LAST - FIRST + 1
         ENDIF
      ENDIF
C
      IF (NAMLEN .EQ. 0) THEN
         ERROR = 77
         IDMESS = 'Empty program name'
         GOTO 40
      ELSE
         DO 30 I = 1,NAMLEN
            CH = NAME(I)
            IF (CH .GE. KLCA .AND. CH .LE. KLCZ) GOTO 30
            IF (CH .GE. KZERO .AND. CH .LE. KNINE) GOTO 30
            IF (CH .NE. KDOLLA) BAD = .TRUE.
   30    CONTINUE
C
         IF (BAD) THEN
            CALL SEMCHS(ERR,NAME,NAMLEN)
            ERROR = 133
            IDMESS = ERR(1:NAMLEN)
            GOTO 40
         ENDIF
      ENDIF
C
      PRUCAS = .FALSE.
C
   40 RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
