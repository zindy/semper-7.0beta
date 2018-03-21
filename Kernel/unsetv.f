C Semper 6 system module UNSETV
C
      LOGICAL FUNCTION UNSETV(NAME)
C
C  Calls SEMLU to unset variable NAME
C
C  Returns .TRUE. iff variable is protected
C
      LOGICAL SEMLU
      INTEGER NAME
      REAL DUMMY
C
      UNSETV=SEMLU(0,NAME,DUMMY)
C
      RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
