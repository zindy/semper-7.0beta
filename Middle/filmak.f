C Semper 6 primitive module FILMAK
C
      LOGICAL FUNCTION FILMAK(NAM,DEF,PATH)
C
C Builds a file path name for subsequent use by FILOPN,MCDC61 etc.
C
      CHARACTER*(*) NAM,DEF,PATH
C
      INTEGER LNBLNK
      LOGICAL FILDCD,FILDIR
C
      INTEGER ILEN,PATLEN,PATEND,PTHLEN
C
      CHARACTER*130 PRENAM,PREDEF
      CHARACTER*80 NAMNAM,NAMDEF
      CHARACTER*40 EXTNAM,EXTDEF
C
      INCLUDE 'COMMON'
C
      FILMAK = .TRUE.
      PTHLEN = LEN(PATH)
C
C Open for write - break up supplied names
C
      IF (FILDCD(NAM,PRENAM,NAMNAM,EXTNAM)) GOTO 10
      IF (FILDCD(DEF,PREDEF,NAMDEF,EXTDEF)) GOTO 10
C
      IF (LNBLNK(EXTNAM) .EQ. 0) EXTNAM = EXTDEF
      IF (LNBLNK(NAMNAM) .EQ. 0) NAMNAM = NAMDEF
      IF (LNBLNK(PRENAM) .EQ. 0) PRENAM = PREDEF
C
      IF (LNBLNK(PRENAM) .EQ. 0) THEN
         IF (LNBLNK(NAMNAM) .EQ. 0 .AND. LNBLNK(EXTNAM) .EQ. 0) THEN
            ERROR = 136
            GOTO 10
         ELSE
C
C If no prefix then use current directory
C
            IF (FILDIR(PRENAM)) GOTO 10
         ENDIF
      ENDIF
C
C Construct full path
C
      PATLEN = LNBLNK(PRENAM)
      IF (PATLEN .GT. PTHLEN) GOTO 20
      IF (PATLEN .EQ. 0) THEN
         PATH = ' '
      ELSE
         PATH(1:PATLEN) = PRENAM(1:PATLEN)
      ENDIF
C
      ILEN = LNBLNK(NAMNAM)
      PATEND = PATLEN + ILEN
      IF (PATEND .GT. PTHLEN) GOTO 20
      PATH(PATLEN+1:) = NAMNAM(1:ILEN)
      PATLEN = PATEND
C
      ILEN = LNBLNK(EXTNAM)
C
C Extension may be blank - check before assigning
C
      IF (ILEN .NE. 0) THEN
         PATEND = PATLEN + ILEN
         IF (PATEND .GT. PTHLEN) GOTO 20
         PATH(PATLEN+1:) = EXTNAM(1:ILEN)
      ENDIF
C
      FILMAK = .FALSE.
C
   10 RETURN
C
   20 ERROR = 137
      GOTO 10
C
C Copyright (C) 1989,1990:  Synoptics Ltd,  All Rights Reserved
C
      END
