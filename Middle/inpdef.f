C Semper 6 subsidiary module INPDEF
C
C Set default file extension for input search (if available)
C
      SUBROUTINE INPDEF(DEFEXT)
      CHARACTER*(*) DEFEXT
C
      RETURN
C
C Copyright (C) 1993-1996:  Synoptics Ltd,  All Rights Reserved
C
      IDUMMY = ICHAR(DEFEXT)
      END
