C Semper 6 system module IVALPN
C
      INTEGER FUNCTION IVALPN(NAME)
C
C Returns picture number given by Semper variable NAME, with
C device CD inserted if original value less than 1000
C
      INTEGER NAME
C
      INTEGER IVAL,SEMPPN
C
      IVALPN = SEMPPN(IVAL(NAME))
      RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
