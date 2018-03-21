C Semper 6 prototype primitive routine MKCHAR
C
      CHARACTER*1 FUNCTION MKCHAR(CH)
C
C Returns *1 character corresponding to i/c char supplied as arg
C
      INTEGER CH
C
      MKCHAR=CHAR(CH)
C
      RETURN
C
C Copyright (C) 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
