C Semper 6 system module SEMPAG
C
      LOGICAL FUNCTION SEMPAG(TEXT,N)
C
      CHARACTER*(*) TEXT
      INTEGER N
C
C Outputs a complete line of text to the terminal.  If the line of text
C is output, it will wrap onto the next line or else be truncated,
C according to whether the command PAGE WRAP or PAGE NOWRAP is current.
C The page size is determined by calling SEMTPS.  This will return the
C size set by the PAGE command, unless the terminal window size changes
C dynamically and can be obtained by interrogating the host system or
C terminal, in which case the size is truncated to the hardware size.
C
      LOGICAL SEMTPS,SEMMOR
C
      INTEGER IWID,ILEN,NPOS,NCHA
C
      INCLUDE 'COMMON'
C
      SEMPAG=.TRUE.
C
C Fetch current terminal page size
C
      IF (SEMTPS(IWID,ILEN)) GOTO 20
C
C Set up start character position for output string
C
      NPOS=1
C
C Determine number of characters to output for this pass (up to
C current terminal page width)
C
   10 NCHA=MIN(N-NPOS+1,IWID)
C
C Output line of text to the terminal
C
      IF (SEMMOR(TEXT(NPOS:),NCHA)) GOTO 20
C
C Increment the start character position
C
      NPOS=NPOS+IWID
C
C If lines wrap-around and end of line not passed, go back for more
C
      IF (TERWRA.AND.NPOS.LE.N) GOTO 10
C
      SEMPAG=.FALSE.
C
   20 RETURN
C
C Copyright (C) 1989,1993:  Synoptics Ltd,  All Rights Reserved
C
      END
