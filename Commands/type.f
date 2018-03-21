C Semper 6 processing module TYPE
C---------------------------------------------------------------------
C
C      SUBROUTINE TYPE
C      ---------------
C
C      PARAMETERS:
C
C      None.
C
C      Handles commands TYPE, LOG and DIAGNOSTIC
C
C---------------------------------------------------------------------
C
      SUBROUTINE TYPE
C
C     ===============
C
      INTEGER IPACK,IVAL
      LOGICAL SEMCON,SEMLOG,SEMDIA,SEMTYP
C
      INCLUDE 'COMMON'
C
      INTEGER TEXLEN
      PARAMETER (TEXLEN=RECLEN)
C
      INTEGER N,TEXT(TEXLEN),IPTR
C
      EQUIVALENCE (RB4,TEXT)
C
C TYPE/LOG processing
C
      IPTR = IVAL(-11201)
      IF (IPTR .EQ. 0) THEN
         RECORD(1:1) = ' '
         N = 1
      ELSE
C
C Evaluate args
C
         N = TEXLEN
         IF (SEMTYP(LINBUF,COMLIM,IPTR,TEXT,N,.FALSE.)) GOTO 10
C
C Ensure command text exhausted
C
         IF (IPTR .LE. COMLIM) THEN
            ERROR = 17
            GOTO 10
         ENDIF
C
C Pack string
C
         CALL SEMCHS(RECORD,TEXT,N)
      ENDIF
C
C Output string
C
      IF (VERB .EQ. -1017) THEN
         IF (SEMCON(RECORD(1:N))) GOTO 10
      ELSE IF (VERB .EQ. 19807) THEN
         IF (SEMLOG(RECORD(1:N))) GOTO 10
      ELSE IF (VERB .EQ. 6761) THEN
         IF (SEMDIA(RECORD(1:N),NDIWAR)) GOTO 10
      ENDIF
C
   10 RETURN
C
C Copyright (C) 1987-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
