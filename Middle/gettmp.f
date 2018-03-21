C Semper 6 primitive module GETTMP
C
      LOGICAL FUNCTION GETTMP(TMPDIR)
C
C Returns the temporary directory prefix
C Try SEMPER$TEMP first, then TEMPDIR, then TEMPFILES, then TEMP
C
      CHARACTER*(*) TMPDIR
C
      INTEGER PTHLEN
C
      INCLUDE 'COMMON'
C
      INTEGER ILEN,LNBLNK
C
      TMPDIR = ' '
      PTHLEN = LEN(TMPDIR)
      CALL GETENV('SEMPER$TEMP',TMPDIR)
      ILEN = LNBLNK(TMPDIR)
      IF (ILEN .EQ. 0) THEN
         CALL GETENV('TEMPDIR',TMPDIR)
         ILEN = LNBLNK(TMPDIR)
      ENDIF
      IF (ILEN .EQ. 0) THEN
         CALL GETENV('TEMPFILES',TMPDIR)
         ILEN = LNBLNK(TMPDIR)
      ENDIF
      IF (ILEN .EQ. 0) THEN
         CALL GETENV('TEMP',TMPDIR)
         ILEN = LNBLNK(TMPDIR)
      ENDIF
C
      IF (ILEN .GT. 0) THEN
         IF (TMPDIR(ILEN:ILEN) .NE. '/') THEN
            ILEN = ILEN + 1
            IF (ILEN .GT. PTHLEN) GOTO 50
            TMPDIR(ILEN:ILEN) = '/'
         ENDIF
      ENDIF
      GETTMP = .FALSE.
   40 CONTINUE
      RETURN
C
C TMPDIR overflow
C
   50 ERROR = 77
      IDMESS = 'Environment list item is too long'
      GETTMP = .TRUE.
      GOTO 40
C
C Copyright (C) 1989-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
