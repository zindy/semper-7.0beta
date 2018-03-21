C Semper 6 system module FSQTRA
C
      LOGICAL FUNCTION FSQTRA(SCALEX,XOFF,SCALEY,YOFF)
C
C  Returns transformation between graphics coordinates and display
C  coordinates
C
C  X         =  SCALEX * X           +   XOFF
C   display               graphics
C
C
C  Y         =  SCALEY * Y           +   YOFF
C   display               graphics
C
      REAL SCALEX,SCALEY,XOFF,YOFF
C
      INCLUDE 'COMMON'
C
      SCALEX=FSXSCA
      SCALEY=FSYSCA
      XOFF=FSXOFF
      YOFF=FSYOFF
C
      FSQTRA=.FALSE.
C
      RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
