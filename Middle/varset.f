C Semper 6 system module VARSET
C
      LOGICAL FUNCTION VARSET(NAME)
      INTEGER NAME
C
C Returns .TRUE. if variable NAME is set
C
      LOGICAL SEMLU
C
      REAL VALUE
C
      VARSET = SEMLU(-1,NAME,VALUE)
C
      RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
