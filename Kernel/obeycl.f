C Semper 6 system routine OBEYCL
C---------------------------------------------------------------------
C
C      LOGICAL FUNCTION OBEYCL(STRING)
C      -----------------------------
C
C      PARAMETERS:
C
C      string - INPUT : command string to be obeyed
C
C      If possible, queues the command string for execution after
C      termination of the current command. Returns true if any error
C      occurs.
C
C---------------------------------------------------------------------
C
      LOGICAL FUNCTION OBEYCL(STRING)
C
      CHARACTER*(*) STRING
C
      LOGICAL DISC
      INTEGER LNBLNK
C
C Locates and attaches a program
C
      INCLUDE 'COMMON'
C
      INTEGER ISLEN
      INTEGER*4 BLKN,N4
C
      OBEYCL = .TRUE.
      ISLEN = LNBLNK(STRING)
      IF (ISLEN .EQ. 0) GOTO 10
C
      IF (.NOT.OBEYME) THEN
         IF (INPLEV .EQ. INPMAX) THEN
            ERROR = 108
            GOTO 20
         ENDIF
C
C Save interactive line ...
C
         IF (INPDEV(INPLEV) .EQ. 0) THEN
            BLKN = WRKLIN + (INPLEV*LINSZE)
            N4 = LINLEN
            IF (DISC(2,WRKDEV,N4,LINBUF,BLKN,NFMINT,NFMBYT)) GOTO 20
            BLKN = WRKIND + (INPLEV*IDXSZE)
            N4 = LNINDX
            IF (DISC(2,WRKDEV,N4,LINDEX,BLKN,NFMINT,NFMINT)) GOTO 20
         ENDIF
C
C Save current line length and command position
C
         INPLEN(INPLEV) = LINLEN
         INPNXT(INPLEV) = NEXTSC
         INPLEV = INPLEV + 1
         INPDEV(INPLEV) = 0
         INPLIN(INPLEV) = 0
         INPSLT(INPLEV) = 0
         INPFOR(INPLEV) = FORLEV
         INPLOC(INPLEV) = LOCLEV
         LINLEN = 0
         NEXTSC = 1
         OBEYPT = 0
         OBEYME = .TRUE.
      ENDIF
C
C Need separator ?
C
      IF (OBEYPT .GT. 0 .AND. OBEYPT .LT. LNLINB) THEN
         IF (OBEYLN(OBEYPT) .NE. KSEMIC) THEN
            OBEYPT = OBEYPT + 1
            OBEYLN(OBEYPT) = KSEMIC
         ENDIF
      ENDIF
C
C Can we fit command into buffer ?
C
      IF (OBEYPT + ISLEN .GT. LNLINB) THEN
         ERROR = 139
         GOTO 20
      ENDIF
C
C Insert string
C
      CALL SEMICS(STRING(1:ISLEN),OBEYLN(OBEYPT+1),ISLEN)
      OBEYPT = OBEYPT + ISLEN
C
   10 OBEYCL = .FALSE.
   20 RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
