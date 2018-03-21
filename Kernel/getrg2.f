C Semper 6 system module GETRG2
C
      LOGICAL FUNCTION GETRG2(X,Y,LAY1,LAY2,U1,U2,V1,V2,NU,NV,LPN)
C
C  Tests subregion keys for a general rotated subregion and defines:
C
C  The picture coordinates of the top-left corner of the region (X,Y)
C  The first and last picture layers LAY1,LAY2
C  The vector (in picture coordinates) corresponding to one step accross
C  the region (U1,U2), and one step up the region (V1,V2).
C  The number of steps in the 'U-direction' NU, and 'V-direction' NV
C  defining the subregion for logical picture number LPN.
C
C  GETRG2 returns .TRUE. in case of error
C
      INTEGER LAY1,LAY2,NU,NV,LPN
      REAL X,Y,U1,U2,V1,V2
C
      LOGICAL TSTSRG
C
      INCLUDE 'COMMON'
C
      GETRG2=TSTSRG(4,LPN)
C
      X=SMGR1
      Y=SMGR2
      U1=SMGR3
      U2=SMGR4
      V1=SMGR5
      V2=SMGR6
      NU=SMGI7
      NV=SMGI8
      LAY1=SMGI3
      LAY2=SMGI6
C
      RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
