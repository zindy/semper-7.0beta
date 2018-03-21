C Semper 6 subsidiary module OUTDEF
C
C Set default file extension for output search (if available)
C
      SUBROUTINE OUTDEF(DEFEXT)
      CHARACTER*(*) DEFEXT
C
      RETURN
C
C Copyright (C) 1993-1996:  Synoptics Ltd,  All Rights Reserved
C
      IDUMMY = ICHAR(DEFEXT)
      END
