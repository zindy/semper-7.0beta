C Semper 6 system module SETVAR
C
      LOGICAL FUNCTION SETVAR(NAME,VALUE)
C
C  Calls SEMLU to set variable NAME to value VALUE
C
C  Returns .TRUE. if the variable table is full or variable is protected
C
      LOGICAL SEMLU
      INTEGER NAME
      REAL VALUE
C
      SETVAR=SEMLU(1,NAME,VALUE)
C
      RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
