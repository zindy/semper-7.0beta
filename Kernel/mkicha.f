C Semper 6 prototype primitive routine MKICHA
C
      INTEGER FUNCTION MKICHA(CHA)
C
C Returns i/c corresponding to fisrt character of supplied argument
C
      CHARACTER*(*) CHA
C
      MKICHA=ICHAR(CHA)
C
      RETURN
C
C Copyright (C) 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
