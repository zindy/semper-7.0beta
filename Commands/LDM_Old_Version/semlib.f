C Semper 6 processing module SEMLIB
C---------------------------------------------------------------------
C
C      SUBROUTINE SEMLIB
C      -----------------
C
C      PARAMETERS:
C
C      None.
C
C      Handles command LIBRARY
C
C---------------------------------------------------------------------
C
      SUBROUTINE SEMLIB
C
      INTEGER IVAL
      LOGICAL DISC,PRUENT
C
C Locates and attaches a program
C
      INCLUDE 'COMMON'
C
      INTEGER IND,IPTR,I,ICH,ISL,DEVICE
      INTEGER PRNAME(PRNACT)
      INTEGER*4 BLKN,N4
      CHARACTER*(PRNACT) PROG
C
      INTEGER NDOLLR
      PARAMETER (NDOLLR=-11201)
C
      IPTR = IVAL(NDOLLR)
      IF (IPTR .EQ. 0) GOTO 40
C
      IF (INPLEV .EQ. INPMAX) THEN
         ERROR = 108
         GOTO 40
      ENDIF
C
C Save interactive line ...
C
      IF (INPDEV(INPLEV) .EQ. 0) THEN
         BLKN = WRKLIN + (INPLEV*LINSZE)
         N4 = LINLEN
         IF (DISC(2,WRKDEV,N4,LINBUF,BLKN,NFMINT,NFMBYT)) GOTO 40
         BLKN = WRKIND + (INPLEV*IDXSZE)
         N4 = LNINDX
         IF (DISC(2,WRKDEV,N4,LINDEX,BLKN,NFMINT,NFMINT)) GOTO 40
      ENDIF
C
C Read name from LINBUF(IPTR) to LINBUF(COMLIM)
C
      IND = IPTR
   10 IF (IND .LT. COMLIM .AND. LINBUF(IND) .EQ. KSPACE) THEN
         IND = IND + 1
         GOTO 10
      ENDIF
C
      IPTR = 0
      DO 20 I = IND,COMLIM
         ICH = LINBUF(I)
         IF (ICH .EQ. KSPACE) GOTO 30
         IPTR = IPTR + 1
         PRNAME(IPTR) = ICH
         IF (IPTR .EQ. PRNACT) GOTO 30
   20 CONTINUE
C
   30 DEVICE=0
      CALL SEMCHS(PROG,PRNAME,IPTR)
      IF (PRUENT(DEVICE,ISL,IPTR,PRNAME)) GOTO 40
C
      IF (ISL .EQ. 0) THEN
         ERROR = 125
         IDMESS = PROG(1:IPTR)
         GOTO 40
      ENDIF
C
C Save current line length and command position
C
      INPLEN(INPLEV) = LINLEN
      INPNXT(INPLEV) = NEXTSC
      INPLEV = INPLEV + 1
      INPDEV(INPLEV) = DEVICE
      INPLIN(INPLEV) = 0
      INPSLT(INPLEV) = ISL
      INPFOR(INPLEV) = FORLEV
      INPLOC(INPLEV) = LOCLEV
      LINLEN = 0
      NEXTSC = 1
C
   40 RETURN
C
C Copyright (C) 1988,1989,1990:  Synoptics Ltd,  All Rights Reserved
C
      END
