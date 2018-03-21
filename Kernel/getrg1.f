C Semper 6 system module GETRG1
C
      LOGICAL FUNCTION GETRG1(COL1,ROW1,LAY1,COL2,ROW2,LAY2,LPN)
C
C  Tests subregion keys for a rectangular subregion and defines the
C  limit row/column/layers defining the subregion for logical picture
C  number LPN.
C
C  Parameter values are truncated at the picture boundaries.
C
C  GETRG1 returns .FALSE. if the defined subregion overlaps part of
C         the picture.
C
C  GETRG1 returns .TRUE. in case of error (e.g. region completely
C         outside picture; ERROR=9)
C
      INTEGER COL1,ROW1,LAY1,COL2,ROW2,LAY2,LPN
C
      LOGICAL TSTSRG
C
      INCLUDE 'COMMON'
C
      GETRG1=TSTSRG(1,LPN)
C
      COL1=SMGI1
      ROW1=SMGI2
      LAY1=SMGI3
      COL2=SMGI4
      ROW2=SMGI5
      LAY2=SMGI6
C
      RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
