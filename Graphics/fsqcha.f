C Semper 6 system module FSQCHA
C
      LOGICAL FUNCTION FSQCHA(CHORIZ,CVERT)
C
C Returns dimensions of characters in graphics coordinates
C
      REAL CHORIZ,CVERT
C
      INCLUDE 'COMMON'
C
      CHORIZ=ABS(FLOAT(CHSIZ(FSDEV))/FSXSCA)
      CVERT=ABS(FLOAT(CHSI2(FSDEV))/FSYSCA)
C
      FSQCHA = .FALSE.
C
      RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
