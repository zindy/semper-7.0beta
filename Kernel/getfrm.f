C Semper 6 system module GETFRM
C
      LOGICAL FUNCTION GETFRM(IFORM,IDEF)
C
C  Sets IFORM = form number corresponding to options BYTE, INTEGER,
C  FP and COMPLEX if set.  If no options are set, sets IFORM = IDEF.
C
C  Returns GETFRM = .TRUE. in case of error
C
      INTEGER IFORM,IDEF
C
      LOGICAL OPT
C
      INTEGER I,J,NNAMES(4),NFORM(4)
C
      INCLUDE 'COMMON'
C
      INTEGER NBYTE,NINTEG,NFP,NCOMPL
      PARAMETER (NBYTE=4220, NINTEG=14980,NFP=10240, NCOMPL=5413)
C
C NNAMES contains BYTE, INTEGER, FP and COMPLEX
C NFORM contains corresponding form numbers
C
      DATA NNAMES / NBYTE,NINTEG,NFP,NCOMPL /
      DATA NFORM  / NFMBYT,NFMINT,NFMFP,NFMCOM /
C
      GETFRM=.TRUE.
C
C  Examine each form option in turn
C
      J=0
      DO 10 I=1,4
         IF (OPT(NNAMES(I))) THEN
            IF (J.EQ.0) THEN
               J=I
            ELSE
C
C  Conflicting options (e.g. BYTE and FP)
C
               ERROR=60
               IDERR=NNAMES(I)
               IDERR2=NNAMES(J)
               GOTO 20
            ENDIF
         ENDIF
   10 CONTINUE
C
C  Set value of IFORM
C
      IF(J.EQ.0) THEN
         IFORM=IDEF
      ELSE
         IFORM=NFORM(J)
      ENDIF
C
      GETFRM=.FALSE.
C
   20 RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
