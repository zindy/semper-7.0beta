C Semper 6 system module SEMSER
C
      LOGICAL FUNCTION SEMSER(TEXT,N)
C
      CHARACTER*(*) TEXT
      INTEGER N
C
C Outputs a line of N characters to the standard error stream.
C Although SEMSER has identical function to the primitive output
C routine WRITSE, SEMSER has been retained to identify the place
C within the high-level part of Semper where all output to the
C standard error stream passes.
C
      LOGICAL WRITSE
C
      SEMSER=WRITSE(TEXT,N)
C
      RETURN
C
C Copyright (C) 1993:  Synoptics Ltd,  All Rights Reserved
C
      END
