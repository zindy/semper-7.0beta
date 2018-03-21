C Semper 6 system module FSQLIM
C
      LOGICAL FUNCTION FSQLIM(XMIN,XMAX,YMIN,YMAX)
C
C Returns clipping limits in graphics coordinates
C
      REAL XMIN,XMAX,YMIN,YMAX
C
      INCLUDE 'COMMON'
C
      XMIN=FSXMIN
      XMAX=FSXMAX
      YMIN=FSYMIN
      YMAX=FSYMAX
C
      FSQLIM=.FALSE.
C
      RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
