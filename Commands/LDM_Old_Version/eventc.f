C Semper 6 processing module EVENTC
C---------------------------------------------------------------------
C
C      SUBROUTINE EVENTC
C      -----------------
C
C      PARAMETERS:
C
C      None.
C
C      Handles command EVENT
C
C---------------------------------------------------------------------
C
      SUBROUTINE EVENTC
C
C     =================
C
      LOGICAL EQFLUS,EQNQRE,EQREAD,EQSETS,EVENT2
      LOGICAL OPT,SEMLU
C     INTEGER IPACK
C
      INCLUDE 'COMMON'
      INCLUDE 'EVENTS'
C
      LOGICAL LKEYBO,LBUTTO,LPOINT,LBREAK,LCLOSE,LOPEN,LCOUNT,LREAD
      LOGICAL LFLUSH,LSTAT,LVERIF
C
      INTEGER AQUEUE,QSTA,QLEN,QCOUNT,I1,I2,I3,IENT
      REAL QC,V
C
      LKEYBO = OPT(17825)
      LBUTTO = OPT(4060)
      LPOINT = OPT(26209)
      LBREAK = OPT(3925)
      LCLOSE = OPT(5295)
      LOPEN = OPT(24645)
      LCOUNT = OPT(5421)
      LREAD = OPT(29001)
      LFLUSH = OPT(10101)
      LSTAT = OPT(31201)
      LVERIF = OPT(-3419)
C
C For STAT or FLUSH assume all queues if none given
C
      IF (.NOT.(LKEYBO .OR. LBUTTO .OR. LPOINT)) THEN
         IF (.NOT.(LBREAK .OR. LCLOSE .OR. LOPEN .OR. LREAD)) THEN
            LKEYBO = .TRUE.
            LBUTTO = .TRUE.
            LPOINT = .TRUE.
         ENDIF
      ENDIF
C
      IF (LOPEN .AND. LCLOSE) THEN
         IDERR = 24645
         IDERR2 = 5295
         ERROR = 60
         GOTO 10
      ENDIF
C
C Fault multiple queues on single queue options
C
      IF (LOPEN .OR. LCLOSE) THEN
         IF (LKEYBO) THEN
            IF (LBUTTO .OR. LPOINT .OR. LBREAK) THEN
               IDERR = 17825
               IF (LBUTTO) THEN
                  IDERR2 = 4060
               ELSE IF (LBREAK) THEN
                  IDERR2 = 3925
               ELSE
                  IDERR2 = 26209
               ENDIF
               ERROR = 60
               GOTO 10
            ENDIF
         ELSE IF (LBUTTO) THEN
            IF (LPOINT .OR. LBREAK) THEN
               IDERR = 4060
               IF (LBREAK) THEN
                  IDERR2 = 3925
               ELSE
                  IDERR2 = 26209
               ENDIF
               ERROR = 60
               GOTO 10
            ENDIF
         ELSE
            IF (LBREAK) THEN
               IDERR = 26209
               IDERR2 = 3925
               ERROR = 60
               GOTO 10
            ENDIF
         ENDIF
C
C and then do the open/close
C
         IF (LBREAK) THEN
            AQUEUE = MBREAK
         ELSE IF (LKEYBO) THEN
            AQUEUE = MKEY
         ELSE IF (LPOINT) THEN
            AQUEUE = MPOINT
         ELSE
            AQUEUE = MBUT
         ENDIF
C
         IF (LOPEN) THEN
            QSTA = QRUN
         ELSE
            QSTA = QCLOSE
         ENDIF
C
         IF (EQSETS(AQUEUE,QSTA)) GOTO 20
      ENDIF
C
      IF (LCOUNT) THEN
         IF (LKEYBO) THEN
            IF (EQNQRE(MKEY,QSTA,QLEN,QCOUNT)) GOTO 20
            QC = QCOUNT
            IF (SEMLU(1,22840,QC)) GOTO 10
         ENDIF
         IF (LPOINT) THEN
            IF (EQNQRE(MPOINT,QSTA,QLEN,QCOUNT)) GOTO 20
            QC = QCOUNT
            IF (SEMLU(1,23040,QC)) GOTO 10
         ENDIF
         IF (LBUTTO) THEN
            IF (EQNQRE(MBUT,QSTA,QLEN,QCOUNT)) GOTO 20
            QC = QCOUNT
            IF (SEMLU(1,22480,QC)) GOTO 10
         ENDIF
      ENDIF
C
      IF (LREAD) THEN
         IF (LKEYBO) THEN
            IENT = 0
            IF (EQREAD(MKEY,QTAKE,IENT,I1,I2,I3)) GOTO 20
C
C If nothing to read return key value -1
C
            IF (IENT .EQ. 0) I1 = -1
            QC = I1
            IF (SEMLU(1,22400,QC)) GOTO 10
         ENDIF
         IF (LPOINT) THEN
            IENT = 0
            IF (EQREAD(MPOINT,QTAKE,IENT,I1,I2,I3)) GOTO 20
C
C If nothing to read return 0,0
C
            IF (IENT .EQ. 0) THEN
               I1 = 0
               I2 = 0
            ENDIF
            QC = I1
            IF (SEMLU(1,7360,QC)) GOTO 10
            QC = I2
            IF (SEMLU(1,7400,QC)) GOTO 10
         ENDIF
         IF (LBUTTO) THEN
            IENT = 0
            IF (EQREAD(MBUT,QTAKE,IENT,I1,I2,I3)) GOTO 20
C
C If nothing to read snapshot buttons and return 0,0,closure
C
            IF (IENT .EQ. 0) THEN
               IF (EQREAD(MBUT,QSNAP,IENT,I1,I2,I3)) GOTO 20
               I1 = 0
               I2 = 0
            ENDIF
            QC = I1
            IF (SEMLU(1,22483,QC)) GOTO 10
            QC = I2
            IF (SEMLU(1,22495,QC)) GOTO 10
            QC = I3
            IF (SEMLU(1,22482,QC)) GOTO 10
         ENDIF
      ENDIF
C
      IF (LFLUSH) THEN
         IF (LKEYBO) THEN
            IF (EQFLUS(MKEY)) GOTO 20
         ENDIF
         IF (LPOINT) THEN
            IF (EQFLUS(MPOINT)) GOTO 20
         ENDIF
         IF (LBUTTO) THEN
            IF (EQFLUS(MBUT)) GOTO 20
         ENDIF
      ENDIF
C
      IF (LSTAT) THEN
         IF (LBREAK) THEN
            IF (EQNQRE(MBREAK,QSTA,QLEN,QCOUNT)) GOTO 20
            IF (QSTA .EQ. QRUN) THEN
               V = 1.0
            ELSE
               V = 0.0
            ENDIF
            IF (SEMLU(1,22457,V)) GOTO 10
         ENDIF
         IF (LKEYBO) THEN
            IF (EQNQRE(MKEY,QSTA,QLEN,QCOUNT)) GOTO 20
            IF (EVENT2('Key',QSTA,QLEN,QCOUNT,
     +                       22857,LVERIF)) GOTO 10
         ENDIF
         IF (LPOINT) THEN
            IF (EQNQRE(MPOINT,QSTA,QLEN,QCOUNT)) GOTO 20
            IF (EVENT2('Pointer',QSTA,QLEN,QCOUNT,
     +                           23057,LVERIF)) GOTO 10
         ENDIF
         IF (LBUTTO) THEN
            IF (EQNQRE(MBUT,QSTA,QLEN,QCOUNT)) GOTO 20
            IF (EVENT2('Button',QSTA,QLEN,QCOUNT,
     +                          22497,LVERIF)) GOTO 10
         ENDIF
      ENDIF
C
   10 RETURN
C
   20 ERROR = 161
      GOTO 10
C
C Copyright (C) 1990-1993:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Subroutine EVENT2
C
      LOGICAL FUNCTION EVENT2(NAME,STATE,LENGTH,COUNT,PNAME,LVERIF)
C
      CHARACTER*(*) NAME
      INTEGER STATE,LENGTH,COUNT,PNAME
      LOGICAL LVERIF
C
      LOGICAL SEMCON,SEMLU
C
      INCLUDE 'COMMON'
      INCLUDE 'EVENTS'
C
      REAL V
      INTEGER IPTR
      LOGICAL STATUS
C
      IF (STATE .EQ. QRUN) THEN
         V = 1.0
      ELSE
         V = 0.0
      ENDIF
      STATUS = SEMLU(1,PNAME,V)
C
      IF (LVERIF .AND. .NOT.STATUS) THEN
         RECORD = NAME // ' queue is '
         IPTR = LEN(NAME) + 10
         IF (STATE .EQ. QCLOSE) THEN
            RECORD(IPTR+1:IPTR+6) = 'closed'
            IPTR = IPTR + 6
         ELSE
            IF (STATE .EQ. QWAIT) THEN
               RECORD(IPTR+1:IPTR+7) = 'waiting'
            ELSE IF (STATE .EQ. QRUN) THEN
               RECORD(IPTR+1:IPTR+7) = 'running'
            ELSE
               RECORD(IPTR+1:IPTR+7) = 'failing'
            ENDIF
            WRITE(RECORD(IPTR+8:),10) COUNT,LENGTH
   10       FORMAT(' with ',I3,' entries of a maximum ',I3)
            IPTR = IPTR + 41
         ENDIF
         STATUS = SEMCON(RECORD(1:IPTR))
      ENDIF
      EVENT2 = STATUS
      RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
