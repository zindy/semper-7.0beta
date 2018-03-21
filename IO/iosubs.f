C
C Semper 6 read routine MRDBIN for Binary files
C
      LOGICAL FUNCTION MRDBIN(FD,IXFR,LBUF,LFORM,LSWAP)
      INTEGER FD,IXFR,LFORM
      LOGICAL LSWAP
      LOGICAL*1 LBUF(IXFR)
C
C FD    :-   file descriptor
C IXFR  :-   bytes to transfer
C LBUF  :-   buffer
C LFORM :-   data form
C LSWAP  :-   if TRUE then need motorola byte packing
C LDM correction: TRUE just means do a swap, not "motorola" packing
C
      LOGICAL EIKBYA
C
      INCLUDE 'COMMON'
C
      IF ( EIKBYA ( 1, FD, LBUF, IXFR ) ) GOTO 10
C
C     If HOST order is not required order then swap
C
      IF (.TRUE. .NEQV. LSWAP) THEN
         IF (LFORM .EQ. NFMINT) THEN
            CALL IROWSW(LBUF,LBUF,IXFR/2)
         ELSE IF (LFORM .EQ. NFMFP .OR. LFORM .EQ. NFMCOM) THEN
            CALL FROWSW(LBUF,LBUF,IXFR/4)
         ENDIF
      ENDIF
      MRDBIN=.FALSE.
      GOTO 20
C
   10 MRDBIN=.TRUE.
   20 RETURN
C
C Copyright (C) 1990-1993 Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 write routine MWRBIN for Binary files
C
      LOGICAL FUNCTION MWRBIN(FD,IXFR,LBUF,LFORM,LSWAP)
      INTEGER FD,IXFR,LFORM
      LOGICAL LSWAP
      LOGICAL*1 LBUF(IXFR)
C
C FD    :-   file descriptor
C IXFR  :-   bytes to transfer
C LBUF  :-   buffer
C LFORM :-   data form
C LSWAP  :-   if TRUE then need motorola byte packing
C
      LOGICAL EIKBYA
C
      INCLUDE 'COMMON'
C
C
C     If HOST order is not required order then swap
C
      IF (.TRUE. .NEQV. LSWAP) THEN
         IF (LFORM .EQ. NFMINT) THEN
            CALL IROWSW(LBUF,LBUF,IXFR/2)
         ELSE IF (LFORM .EQ. NFMFP .OR. LFORM .EQ. NFMCOM) THEN
            CALL FROWSW(LBUF,LBUF,IXFR/4)
         ENDIF
      ENDIF
C
      IF ( EIKBYA ( 2, FD, LBUF, IXFR ) ) GOTO 10
C
      MWRBIN=.FALSE.
      GOTO 20
C
   10 MWRBIN=.TRUE.
   20 RETURN
C
C Copyright (C) 1990-1993 Synoptics Ltd,  All Rights Reserved
C
      END
