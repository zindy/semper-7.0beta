C Semper 6 subsidiary processing module ASSTAP
C
      LOGICAL FUNCTION ASSTAP(DEVICE)
C
      INTEGER DEVICE
C
C Code for ASSIGN TAPE command
C
C
      INCLUDE 'COMMON'
C
C
      ASSTAP=.TRUE.
      ERROR = 77
      IDMESS = 'No TAPE support in this system'
      RETURN
      IDUMMY=DEVICE
C
C Copyright (C) 1993-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
