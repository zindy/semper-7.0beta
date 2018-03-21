C Semper 6 display primitive routine FSOQ61
C
      LOGICAL FUNCTION FSOQ61(LOVIND,IERROR)
C
C
C LOVIND should be set .TRUE. if each frame of the framestore has an
C individual overlay.
C If the individual frames share a common overlay then LOVIND should
C be set .FALSE.
C
C Arguments
C
      INTEGER IERROR
      LOGICAL LOVIND
C
      LOVIND = .TRUE.
      FSOQ61 = .FALSE.
      IDUMMY = IERROR
C
      RETURN
C
C Copyright (C) 1990-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
