C Semper 6 system module SEMRNG
C
      LOGICAL FUNCTION SEMRNG(IOP,PMIN,PMAX,LPN)
C
      INTEGER IOP,LPN
      REAL    PMIN,PMAX
C
C Provides access to range information in the picture label of the
C specified picture LPN.  If IOP = 1, the range information is returned
C in PMIN and PMAX.  If IOP = 2, the values in PMIN and PMAX are used
C to update the range information in the picture label.  If PMIN > PMAX,
C the range information is deleted.
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
      SEMRNG=.TRUE.
C
C Save current values of Semper variables MIN and MAX
C
      TMIN=VMIN
      TMAX=VMAX
C
C If IOP = 1, return range information
C
      IF (IOP.EQ.1) THEN
C
C Set flag to obtain range for entire picture
C
         SMGL1=.FALSE.
C
C Fetch picture range - from the picture label if there, otherwise
C                       RANGE scans the entire picture to find out
C
         IF (RANGE(1,LPN)) GOTO 10
         IF (ERROR.EQ.4) GOTO 10
C
         PMIN=VMIN
         PMAX=VMAX
C
C Otherwise, update the range information
C
      ELSE
C
C If PMIN > PMAX, delete range information
C
         IF (PMIN.GT.PMAX) THEN
            IF (RANGE(3,LPN)) GOTO 10
C
C Otherwise, record range information as specified
C
         ELSE
            VMIN=PMIN
            VMAX=PMAX
            IF (RANGE(4,LPN)) GOTO 10
         ENDIF
      ENDIF
C
      SEMRNG=.FALSE.
C
C Restore values of Semper variables MIN and MAX
C
   10 VMIN=TMIN
      VMAX=TMAX
C
      RETURN
C
C Copyright (C) 1991,1993:  Synoptics Ltd,  All Rights Reserved
C
      END
