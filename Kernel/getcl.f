C Semper 6 routine GETCL emulate Lahey GETCL
C
C     Getcl - return contents of command line less command string itself
C
C        Calling sequence:   call getcl(line)
C
C        Arguments:   character*(*) line      command line string
C
      SUBROUTINE GETCL(LINE)
C
      CHARACTER*(*) LINE
C
      INTEGER ARGLEN
      PARAMETER (ARGLEN=80)
      CHARACTER*(ARGLEN) ANARG
C
      INTEGER IARGC
      INTEGER LNBLNK
C
      INTEGER*4 I
      INTEGER J,L
C
      LINE = ' '
      L = LEN(LINE)
C
C collect together tokens making up command line
C
      DO 10 I=1,IARGC()
C
C determine start position for next token
C
         J = LNBLNK(LINE) + 2
         IF (J .EQ. 2) J = 1
C
C return if end of command line buffer reached
C
         IF (J.GT.L) GOTO 20
C
C fetch next command line token
C
         CALL GETARG(I,ANARG)
         LINE(J:) = ANARG
   10 CONTINUE
C
   20 RETURN
C
C Copyright (C) 1988-1993 Synoptics Ltd., All rights reserved.
C
      END
