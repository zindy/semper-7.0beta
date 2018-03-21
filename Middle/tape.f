C Semper 6 system module TAPE
C
      LOGICAL FUNCTION TAPE ()
C     LOGICAL FUNCTION TAPE (IOP,FORM,DEVICE,FILE,BLOCK,LENGTH,ARRAY)
C
C Tape positioning, management and error handling
C
C     INTEGER IOP,FORM,DEVICE,FILE,LENGTH,ARRAY(*)
C     INTEGER*4 BLOCK
C
      INCLUDE 'COMMON'
      ERROR = 77
      IDMESS = 'No TAPE support in this system'
      TAPE = .TRUE.
      RETURN
C
C Copyright (C) 1987-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
