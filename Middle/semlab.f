C Semper 6 system module SEMLAB
C
      LOGICAL FUNCTION SEMLAB(IOP,LABEL,LPN)
C
C Reads/writes (IOP=1/2) label for pic LPN, setting LBLINC on
C successful reads
C
      INTEGER IOP,LABEL(*),LPN
C
      LOGICAL SEMOP2,SEMLA2,DISC
C
      INCLUDE 'COMMON'
C
      INTEGER*4 BLKN,I41
      PARAMETER (I41=1)
      INTEGER DEVICE,MEDIUM,NUMBER,ID
C
      SEMLAB=.TRUE.
C
      DEVICE=DEVN(LPN)
      NUMBER=PICN(LPN)
C
      ID = 1000*DEVICE
      IF (NUMBER .GT. 0) ID = ID + NUMBER
C
C Check label on writing
C
      IF (IOP.EQ.2) THEN
         IF (SEMOP2(LABEL)) THEN
            ERROR=149
            IDERR=ID
            GOTO 30
         ENDIF
         IF (SEMLA2(IOP,LABEL,ID)) GOTO 30
      ENDIF
C
C Switch on device type
C
      MEDIUM = MEDN(DEVICE)
      IF (MEDIUM .EQ. MEDDC .OR. MEDIUM .EQ. MEDVM) THEN
C
C Disc/memory
C
         BLKN=ADDR(LPN)
      ELSE IF (MEDIUM .EQ. MEDDS) THEN
C
C Display
C
         DEVICE=0
         BLKN = WRKDPD + (NUMBER-1)*(DPDSZE+LABSZE) + DPDSZE
      ENDIF
C
      IF (DISC(IOP,DEVICE,LNLAB4,LABEL,BLKN,NFMINT,NFMBYT)) GOTO 30
C
C Check label after reading
C
      IF (IOP.EQ.1) THEN
         IF (SEMOP2(LABEL)) THEN
            ERROR=52
            IDERR=ID
            GOTO 30
         ENDIF
         IF (SEMLA2(IOP,LABEL,ID)) GOTO 30
         LBLINC=.TRUE.
      ENDIF
C
      SEMLAB=.FALSE.
C
   30 RETURN
C
C Copyright (C) 1987-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 subsidiary system module SEMLA2
C
      LOGICAL FUNCTION SEMLA2(IOP,LABEL,I)
      INTEGER IOP,LABEL(*)
C
C Checks label fields - reporting error for serious problems
C                       outputting a warning for others
C
      LOGICAL SEMLA3,SEMLA4
C
      INCLUDE 'COMMON'
C
      INTEGER NCOL,NROW,NLAY,N,I,F
C
      CHARACTER*6 FIELDS(8)
C
      DATA FIELDS /
     +     'FORM',
     +     'CLASS',
     +     'PLTYPE',
     +     'NCOL',
     +     'NROW',
     +     'NLAY',
     +     'RANGE',
     +     'TITLE' /
C
      SEMLA2 = .TRUE.
C
      F = 1
      N = LABEL(LBFORM)
      IF (N .NE. NFMBYT .AND. N .NE. NFMINT .AND.
     +    N .NE. NFMFP .AND. N .NE. NFMCOM) GOTO 30
C
      F = 2
      N = LABEL(LBCLAS)
      IF (N .EQ. 0) GOTO 30
C
      F = 3
      IF (N .EQ. NCLPLI) THEN
         N = LABEL(LBPLTY)
         IF (N .EQ. 0 .OR. N .GT. 3) GOTO 30
      ENDIF
C
      F = 4
      N = 256*LABEL(LBNC1) + LABEL(LBNC2)
      IF (N .EQ. 0) GOTO 30
      NCOL = N
C
      F = 5
      N = 256*LABEL(LBNR1) + LABEL(LBNR2)
      IF (N .EQ. 0) GOTO 30
      NROW = N
C
      F = 6
      N = 256*LABEL(LBNL1) + LABEL(LBNL2)
      IF (N .EQ. 0) GOTO 30
      NLAY = N
C
      F = 7
      N = LABEL(LBNCRR)
      IF (N .GT. (LBRR2-LBRR1+1)) GOTO 30
C
      F = 8
      N = LABEL(LBNCTT)
      IF (N .GT. (LBTT2-LBTT1+1)) GOTO 30
C
C All subsequent checking is for reasonable values only
C
      WRITE(RECORD,10) I
   10 FORMAT(' Warning: picture',I6,', ')
C
      N = 256*LABEL(LBCC1) + LABEL(LBCC2)
      IF (N .LT. 1 .OR. N .GT. NCOL) THEN
         IF (SEMLA4('column')) GOTO 20
      ENDIF
      N = 256*LABEL(LBCR1) + LABEL(LBCR2)
      IF (N .LT. 1 .OR. N .GT. NROW) THEN
         IF (SEMLA4('row')) GOTO 20
      ENDIF
      N = 256*LABEL(LBCL1) + LABEL(LBCL2)
      IF (N .LT. 1 .OR. N .GT. NLAY) THEN
         IF (SEMLA4('layer')) GOTO 20
      ENDIF
C
      N = LABEL(LBMON)
      IF (N .EQ. 0 .OR. N .GT. 12) THEN
         IF (SEMLA3('month',N)) GOTO 20
      ENDIF
C
      N = LABEL(LBDAY)
      IF (N .EQ. 0 .OR. N .GT. 31) THEN
         IF (SEMLA3('day',N)) GOTO 20
      ENDIF
C
      N = LABEL(LBHOUR)
      IF (N .GT. 23) THEN
         IF (SEMLA3('hour',N)) GOTO 20
      ENDIF
C
      N = LABEL(LBMIN)
      IF (N .GT. 59) THEN
         IF (SEMLA3('minute',N)) GOTO 20
      ENDIF
C
      N = LABEL(LBSEC)
      IF (N .GT. 59) THEN
         IF (SEMLA3('seconds',N)) GOTO 20
      ENDIF
C
      SEMLA2 = .FALSE.
C
   20 RETURN
C
   30 IDERR = I
      IDERR2 = N
      IDMESS = FIELDS(F)
      IF (IOP .EQ. 1) THEN
         ERROR = 147
      ELSE
         ERROR = 148
      ENDIF
      GOTO 20
C
C Copyright (C) 1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C
      LOGICAL FUNCTION SEMLA3(MESS,N)
      CHARACTER*(*) MESS
      INTEGER N
C
      LOGICAL SEMDIA
C
      INCLUDE 'COMMON'
C
      WRITE(RECORD(27:),10) MESS,N
   10 FORMAT(A,' field in label has bad value:',I5)
      SEMLA3 = SEMDIA(RECORD,NDIWAR)
C
      RETURN
C
C Copyright (C) 1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C
      LOGICAL FUNCTION SEMLA4(MESS)
      CHARACTER*(*) MESS
C
      LOGICAL SEMDIA
C
      INCLUDE 'COMMON'
C
      WRITE(RECORD(27:),10) MESS
   10 FORMAT(A,' origin outside picture')
      SEMLA4 = SEMDIA(RECORD,NDIWAR)
C
      RETURN
C
C Copyright (C) 1992:  Synoptics Ltd,  All Rights Reserved
C
      END
