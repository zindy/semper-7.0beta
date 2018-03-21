C Semper 6 system module PRUIND
C
      LOGICAL FUNCTION PRUIND(OPCODE,DEVICE,IDSLOT,TXTBLK,LABBLK,
     +                        SLTYPE,TXTLEN,LABLEN,NAMLEN,NAME)
C
C Reads/Writes an index slot
C
      INCLUDE 'COMMON'
C
      INTEGER OPCODE,DEVICE,IDSLOT
C
      INTEGER*4 TXTBLK,LABBLK
      INTEGER SLTYPE,TXTLEN,LABLEN,NAMLEN
      INTEGER NAME(PRNACT)
C
C An index slot is of size PRCSZE blocks and contains:
C
C     TXTBLK*4 - block number of procedure text start
C              - (pointer to text index)
C     LABBLK*4 - block number of procedure label table
C              - (or zero if none)
C     SLTYPE   - slot type (0 = free, 1 = deleted, 2 = normal)
C     TXTLEN   - number of blocks in text area (for run manager)
C     LABLEN   - number of blocks in label area (for run manager)
C     NAMLEN   - number of items in name (1 to PRCACT)
C     NAME     - actual name
C
      LOGICAL DISC
      INTEGER L4N,L2S,L2N,L1S
C
C Length of INTEGER*4 section, start of INTEGER section
C Length of INTEGER section, start of BYTE section
C
      PARAMETER (L4N=2,L2N=4)
      PARAMETER (L2S=(L4N*(LNINT4/LNINT)+1))
      PARAMETER (L1S=L2S+L2N)
C
      INTEGER*4 I4N,LNPRC4,L4(L4N)
      PARAMETER (LNPRC4=LNPROC)
C
      INTEGER LABEL(LNPROC/LNINT),L2(L2N)
C
      EQUIVALENCE (LABEL,L4),(LABEL(L2S),L2)
C
      PRUIND=.TRUE.
C
      IF (OPCODE .EQ. 2) THEN
         L4(1)=TXTBLK
         L4(2)=LABBLK
         L2(1)=SLTYPE
         L2(2)=TXTLEN
         L2(3)=LABLEN
         L2(4)=0
         IF (SLTYPE .EQ. 2) THEN
C
            IF (NAMLEN.LT.0 .OR. NAMLEN .GT. PRNACT) THEN
               ERROR = 77
               IDMESS = 'Internal name error in PRUIND(WRITE)'
               GOTO 10
            ELSE
C
C Write name as well
C
               L2(4)=NAMLEN
               I4N=NAMLEN
               CALL CFORM(NAME,LABEL(L1S),NFMINT,NFMBYT,I4N)
            ENDIF
         ELSE
         ENDIF
      ENDIF
C
C Calculate start block number from slot number
C
      I4N=IDSLOT-1
      I4N=I4N*PRCSZE
C
C Add 1 for header block, 1 for info block and 1 as block 0 is the first
C
      I4N=I4N+3
      IF (DISC(OPCODE,DEVICE,LNPRC4,LABEL,I4N,NFMBYT,NFMBYT)) GOTO 10
C
      IF (OPCODE .EQ. 1) THEN
         TXTBLK=L4(1)
         LABBLK=L4(2)
         SLTYPE=L2(1)
         TXTLEN=L2(2)
         LABLEN=L2(3)
         NAMLEN=L2(4)
C
         IF (L2(1) .EQ. 2) THEN
            IF (NAMLEN .LT. 0 .OR. NAMLEN .GT. PRNACT) THEN
               ERROR = 77
               IDMESS = 'Internal name error in PRUIND(READ)'
               GOTO 10
            ELSE
C
C Read name as well
C
               I4N=NAMLEN
               CALL CFORM(LABEL(L1S),NAME,NFMBYT,NFMINT,I4N)
            ENDIF
         ENDIF
      ENDIF
C
      PRUIND = .FALSE.
C
   10 RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
