C Semper 6 primitive module GETPAT
C
      LOGICAL FUNCTION GETPAT(PATHS,ICOUNT)
C
C Returns the current search path with semi-colon as the delimiter
C ICOUNT is used to return the number of path elements
C
      CHARACTER*(*) PATHS
      INTEGER ICOUNT
C
      INTEGER PTHLEN
C
      INCLUDE 'COMMON'
C
      INTEGER IND,ILEN,LNBLNK
      CHARACTER*1 CH
C
      PATHS = ' '
      CALL GETENV('PATH',PATHS)
C      write(6,*)'From getenv ',paths
      PTHLEN = LEN(PATHS)
C      write(6,*)'Length of paths ',pthlen
      ICOUNT = 0
      IF (LNBLNK(PATHS) .EQ. 0) THEN
         ILEN = 0
      ELSE
         ILEN = LNBLNK(PATHS)
         IF (PATHS(ILEN:ILEN) .NE. ':') THEN
            IF (PTHLEN .EQ. ILEN) GOTO 60
            PATHS(ILEN+1:) = ':'
            ILEN = ILEN + 1
         ENDIF
C
C Preserve case and count items
C
         DO 40 IND = 1,ILEN
            CH = PATHS(IND:IND)
            IF (CH .EQ. ':') THEN
               ICOUNT = ICOUNT + 1
               PATHS(IND:IND) = ';'
            ENDIF
   40    CONTINUE
      ENDIF
C
C Add other useful items if not present and room available
C
C     IF (INDEX(PATHS,'/usr/semper6;') .EQ. 0) THEN
C        IF (PTHLEN .LT. ILEN+13) GOTO 50
C        PATHS(ILEN+1:ILEN+13) = '/usr/semper6;'
C        ILEN = ILEN + 13
C        ICOUNT = ICOUNT + 1
C     ENDIF
C     IF (INDEX(PATHS,'/usr/people/semper6;') .EQ. 0) THEN
C        IF (PTHLEN .LT. ILEN+20) GOTO 50
C        PATHS(ILEN+1:ILEN+20) = '/usr/people/semper6;'
C        ILEN = ILEN + 20
C        ICOUNT = ICOUNT + 1
C     ENDIF
      GETPAT = .FALSE. ! Was 50
      RETURN
C
C Pathname overflow
C
   60 ERROR = 77
      IDMESS = 'Environment PATH list is too long'
      GETPAT = .TRUE.
      RETURN
C
C Copyright (C) 1989-1993:  Synoptics Ltd,  All Rights Reserved
C
      END
