C Semper 6 subsidiary processing module ASSMEM
C
      LOGICAL FUNCTION ASSMEM(DEVICE,FLTYPE,FLSIZE,DRSIZE)
C
      INTEGER DEVICE,FLTYPE,DRSIZE
      INTEGER*4 FLSIZE
C
C Code for ASSIGN MEMORY command
C
      LOGICAL ASSHDR,PRUPRI
C
      INTEGER*4 I4
C
      INCLUDE 'COMMON'
C
      ASSMEM=.TRUE.
C
C Create the memory buffer
C
      CALL MCDC61(9,DEVICE,FLSIZE*LNBLK4,0,RB1,ERROR)
C
      IF (ERROR.NE.0) GOTO 10
C
C Record memory parameters in device table
C
      DVTYP(DEVICE)=FLTYPE
      FLSIZ(DEVICE)=FLSIZE
      DRSIZ(DEVICE)=DRSIZE
      PROTN(DEVICE)=0
      TPSID(DEVICE)=0
C
C Record memory assignment
C
      MEDN(DEVICE)=MEDVM
C
C Write header and create empty directory
C
      IF (ASSHDR(2,DEVICE)) GOTO 20
C
C If program device, add it to head of program priority queue
C
      IF (FLTYPE.EQ.FLTRUN) THEN
         IF (PRUPRI(1,DEVICE)) GOTO 20
      ENDIF
C
      ASSMEM=.FALSE.
C
   10 RETURN
C
C Error recovery from call to ASSHDR
C
C Free the memory buffer
C
   20 CALL MCDC61(10,DEVICE,I4,0,RB1,ERROR)
C
C Cancel memory assignment
C
      MEDN(DEVICE)=0
C
      GOTO 10
C
C Copyright (C) 1993:  Synoptics Ltd,  All Rights Reserved
C
      END
