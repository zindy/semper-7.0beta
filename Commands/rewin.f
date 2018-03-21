C Semper 6 processing module REWIN
C
      SUBROUTINE REWIN
C
C - rewinds tapes (without waiting or deassigning)
C
C Syntax:
C     Rewind :REWIN device=cd
C
C
      INCLUDE 'COMMON'
      IDMESS = 'No TAPE support in this system'
      ERROR = 77
      RETURN
C
C Copyright (C) 1987-1996 : Synoptics Ltd, All Rights Reserved
C
      END
