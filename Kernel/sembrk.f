C Semper 6 utility routine SEMBRK
C
      LOGICAL FUNCTION SEMBRK ( )
C
C Checks if a break has been signalled via the event queue interface.
C
      LOGICAL ABANDN
C
      INCLUDE 'COMMON'
C
      SEMBRK = ABANDN(ERROR)
C
      RETURN
C
C Copyright (C) 1990: Synoptics Ltd, All Rights Reserved
C
      END
