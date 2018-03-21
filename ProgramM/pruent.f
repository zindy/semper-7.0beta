C Semper 6 system module PRUENT
C
      LOGICAL FUNCTION PRUENT(DEVICE,OLSLOT,NAMLEN,NAME)
C
      INCLUDE 'COMMON'
C
      LOGICAL PRUSLT,PRUCAS
      INTEGER DEVICE,OLSLOT
      INTEGER NAMLEN,NAME(PRNACT)
      INTEGER IND,MYDEV,MYDEV1,MYDEV2,I
      INTEGER*4 I4N
C
C Locate a directory entry on any device if DEVICE = 0, else
C use DEVICE
C
      PRUENT = .TRUE.
C
C Truncate NAMLEN if too long
C
      IF (NAMLEN .GT. PRNACT) NAMLEN = PRNACT
C
C Set case of name
C
      IF (PRUCAS(NAME,NAMLEN)) GOTO 30
      IF (NAMLEN .LE. 0) GOTO 20
C
      IF (DEVICE .NE. 0) THEN
         IDERR = DEVICE
         IF (.NOT.(MEDN(DEVICE).EQ.MEDDC.OR.MEDN(DEVICE).EQ.MEDVM)) THEN
            ERROR = 29
            GOTO 30
         ELSE IF (DVTYP(DEVICE) .NE. FLTRUN) THEN
            ERROR = 35
            GOTO 30
         ENDIF
         MYDEV1=1
         MYDEV2=1
      ELSE
         MYDEV1=1
         MYDEV2=PTNUMB
      ENDIF
      I=1
      OLSLOT=0
      IF (MYDEV2 .NE. 0) THEN
         DO 10 IND = MYDEV1,MYDEV2
            IF (DEVICE .EQ. 0) THEN
               MYDEV = PTPRIO(IND)
            ELSE
               MYDEV = DEVICE
            ENDIF
            IF (PRUSLT(MYDEV,I,OLSLOT,I4N,I4N,I4N,NAMLEN,NAME)) GOTO 30
            IF (OLSLOT .NE. 0) THEN
               IF (DEVICE .EQ. 0) DEVICE = MYDEV
               GOTO 20
            ENDIF
   10    CONTINUE
      ENDIF
   20 PRUENT = .FALSE.
   30 RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
