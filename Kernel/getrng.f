C Semper 6 system module GETRNG
C
      LOGICAL FUNCTION GETRNG(PMIN,PMAX,LPN)
C
C Returns in PMIN and PMAX the range of values in picture LPN
C
      REAL    PMIN,PMAX
      INTEGER LPN
C
C     Change: LDM, July 2005
C     Ensure that gfortran does not think of range, unpack as intrinsics
      EXTERNAL RANGE, UNPACKF
      LOGICAL RANGE
C
      REAL TMIN,TMAX
C
      INCLUDE 'COMMON'
C
      GETRNG=.TRUE.
C
C Save current values of Semper variables MIN and MAX
C
      TMIN=VMIN
      TMAX=VMAX
C
C Set flag to obtain range for entire picture
C
      SMGL1=.FALSE.
C
C Fetch picture range - from the picture label if there, otherwise
C                       RANGE scans the entire picture to find out
C
      IF (RANGE(1,LPN)) GOTO 10
C
C Return picture range values - RANGE puts these in Semper variables
C                               MIN and MAX
C
      PMIN=VMIN
      PMAX=VMAX
C
C Restore values of Semper variables MIN and MAX
C
      VMIN=TMIN
      VMAX=TMAX
C
      GETRNG=.FALSE.
C
   10 RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
