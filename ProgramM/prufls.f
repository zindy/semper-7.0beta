C Semper 6 system module PRUFLS
C
      LOGICAL FUNCTION PRUFLS(DEVICE)
C
C Flush any buffered blocks and check DEVICE if non zero
C
      LOGICAL DISC
      INTEGER DEVICE,PGMDEV
      INTEGER*4 I40
      PARAMETER (I40=0)
C
      INCLUDE 'COMMON'
C
      INTEGER BUFF(LNBLK)
      EQUIVALENCE (RB6,BUFF)
C
      PRUFLS = .TRUE.
C
C FLUSH program discs
C
      IF (PTNUMB .GT. 0) THEN
         DO 10 PGMDEV = 1,PTNUMB
            IF (DISC(3,PTPRIO(PGMDEV),I40,BUFF,I40,0,0)) GOTO 20
   10    CONTINUE
      ENDIF
C
C Validate DEVICE if specified
C
      IF (DEVICE .NE. 0) THEN
         IDERR = DEVICE
         IF (.NOT.(MEDN(DEVICE).EQ.MEDDC.OR.MEDN(DEVICE).EQ.MEDVM)) THEN
            ERROR = 29
         ELSE IF (DVTYP(DEVICE) .NE. FLTRUN) THEN
            ERROR = 35
         ELSE
            PRUFLS = .FALSE.
         ENDIF
      ELSE
         PRUFLS = .FALSE.
      ENDIF
C
   20 RETURN
C
C Copyright (C) 1987-1991:  Synoptics Ltd,  All Rights Reserved
C
      END
