C Semper 6 system module SEMBEE
C
      LOGICAL FUNCTION SEMBEE()
C
C Generate audible sound at the terminal
C
      CALL BUZZER
C
      SEMBEE = .FALSE.
C
      RETURN
C
C Copyright (C) 1989-1993:  Synoptics Ltd,  All Rights Reserved
C
      END
