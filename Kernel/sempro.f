C Semper 6 system module SEMPRO
C
      LOGICAL FUNCTION SEMPRO(TEXT)
C
C Outputs a line of text contained in the character variable TEXT to
C the prompt output stream.  No end-of-line sequence is output after
C the prompt string.  Trailing blanks are not removed from the output
C text.
C
      CHARACTER*(*) TEXT
C
      LOGICAL SEMSOL,SEMTOU,SEMTFL
C
      SEMPRO=.TRUE.
C
C Output start-of-line sequence
C
      IF (SEMSOL()) GOTO 10
C
C Output text to terminal (for now)
C
      IF (SEMTOU(TEXT)) GOTO 10
C
C Force text to be output
C
      SEMPRO = SEMTFL()
C
   10 RETURN
C
C Copyright (C) 1989-1993:  Synoptics Ltd,  All Rights Reserved
C
      END
