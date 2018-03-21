C Semper 6 system module FSQBOR
C
      LOGICAL FUNCTION FSQBOR(XXLEF,XXRIG,YYBOT,YYTOP)
C
C Returns border limits in graphics coordinates
C
      REAL XXLEF,XXRIG,YYTOP,YYBOT
C
      INCLUDE 'COMMON'
C
      XXLEF=FSBLEF
      XXRIG=FSBRIG
      YYBOT=FSBBOT
      YYTOP=FSBTOP
C
      FSQBOR=.FALSE.
C
      RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
