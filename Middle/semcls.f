C Semper 6 system module SEMCLS
C
      LOGICAL FUNCTION SEMCLS(LPN)
      INTEGER LPN
C
C Closes picture LPN, releasing PCB
C
C This allows proc mods to discard unwanted PCBs to make way for
C further opens they may need; the ideal facility would be a 'mark
C available for re-use' rather than a 'release now', as tape PCBs
C are expensive to reconstruct.  A partial solution is to update
C BASEWH, which marks all PCBs with lower WHEN available but does
C actually releasing them - but this applies of course to all pics
C up to BASEWH, and does not allow you to return to the pool the
C PCBs of intermediate pictures without losing your source.
C
      INCLUDE 'COMMON'
C     Change: LDM, July 2005
C     Ensure that gfortran does not think of range, unpack as intrinsics
      EXTERNAL RANGE, UNPACKF
      LOGICAL SEMMON,RANGE
C
      SEMCLS = .TRUE.
C
      IF (MONIT) THEN
         WRITE (RECORD,10) LPN
   10    FORMAT ('!-SEMCLS called: lpn',I3)
         IF (SEMMON(RECORD,'SEMCLS',2)) GOTO 20
      ENDIF
C
C Delete range record if necessary
C
      IF (WSTAT(LPN).GT.0) THEN
         IF (RANGE(3,LPN)) GOTO 20
      ENDIF
C
C Close picture
C
      WHEN(LPN) = 0
      SEMCLS = .FALSE.
   20 RETURN
C
C Copyright (C) 1987,1989,1990:  Synoptics Ltd,  All Rights Reserved
C
      END
