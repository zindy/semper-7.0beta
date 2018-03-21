C Semper 6 processing module ASSIG
C
      SUBROUTINE ASSIG
C
C ASSIGN command
C
      INTEGER IPACK,IVAL
      LOGICAL OPT,OPTNO,VARSET,SEMCON,SETVAR
      LOGICAL ASSSCR,ASSDIS,ASSNEW,ASSOLD,ASSSIZ
      LOGICAL ASSMEM
      LOGICAL ASSFIL
C
      INTEGER DEVICE,I,J,FLTYPE,DRSIZE
      INTEGER*4 FLSIZE
C
      INCLUDE 'COMMON'
C
      INTEGER NOPTS
      PARAMETER (NOPTS=4)
      LOGICAL LOPT(NOPTS),LSCRAT,LMEMOR,LFILE,LDISPL
C
      EQUIVALENCE (LOPT(1),LSCRAT)
      EQUIVALENCE (LOPT(2),LMEMOR)
      EQUIVALENCE (LOPT(3),LFILE)
      EQUIVALENCE (LOPT(NOPTS),LDISPL)
C
      CHARACTER*7 NAME(NOPTS)
C
      DATA NAME / 'scratch', 'memory', 'file',
     +            'display' /
C
C See if options SCRATCH, MEMORY, TAPE and FILE are set
C
      DO 10 I=1,NOPTS-1
         LOPT(I)=OPT(IPACK(NAME(I)))
   10 CONTINUE
C
C See if DISPLAY option is set
C
C Note:  this requires some unpleasant looking code because of the
C        clash with the protected DISPLAY variable, which means that
C        the normal mechanism for checking command line options does
C        not work.  The DISPLAY option is simulated by using the $1
C        key (with a zero default value) to pick up the presence of
C        the DISPLAY variable as a numerical expression.
C
      IF (IVAL(-12441).EQ.NINT(DISPLA)) THEN
         LDISPL=.TRUE.
      ELSE IF (IVAL(-12441).EQ.0) THEN
         LDISPL=.FALSE.
      ELSE
         ERROR=17
         GOTO 60
      ENDIF
C
C Fault conflicting options SCRATCH, MEMORY, TAPE, FILE and DISPLAY
C
      DO 30 I=1,NOPTS-1
         DO 20 J=I+1,NOPTS
            IF (LOPT(I).AND.LOPT(J)) THEN
               ERROR=60
               IDERR=IPACK(NAME(I))
               IDERR2=IPACK(NAME(J))
               GOTO 60
            ENDIF
   20    CONTINUE
   30 CONTINUE
C
C Determine device number
C
      IF (LDISPL) THEN
C
C Display device number must be 1
C
         DEVICE=1
      ELSE
C
C See if DEVICE key is set
C
         IF (VARSET(6622)) THEN
C
C Fetch value for DEVICE key
C
            DEVICE=IVAL(6622)
C
C Fault bad device number - must not be 1 as this is used for DISPLAY!
C
            IF (DEVICE.LT.2.OR.DEVICE.GT.NDVS) THEN
               ERROR=76
               IDERR=DEVICE
               GOTO 60
            ENDIF
C
C Otherwise, find a free device number
C
         ELSE
            DO 40 DEVICE=2,NDVS
               IF (MEDN(DEVICE).EQ.0) GOTO 50
   40       CONTINUE
C
C Fault no free device numbers
C
            ERROR=45
            GOTO 60
         ENDIF
      ENDIF
C
C Fault device number already in use
C
      IF (MEDN(DEVICE).NE.0) THEN
         ERROR=121
         IDERR=DEVICE
         GOTO 60
      ENDIF
C
C Call appropriate routine to carry out device assignment
C
   50 IF (LSCRAT) THEN
         IF (ASSSIZ(FLTYPE,FLSIZE,DRSIZE)) GOTO 60
         IF (ASSSCR(DEVICE,FLTYPE,FLSIZE,DRSIZE)) GOTO 60
      ELSE IF (LMEMOR) THEN
         IF (ASSSIZ(FLTYPE,FLSIZE,DRSIZE)) GOTO 60
         IF (ASSMEM(DEVICE,FLTYPE,FLSIZE,DRSIZE)) GOTO 60
      ELSE IF (LFILE) THEN
         IF (ASSFIL(DEVICE)) GOTO 60
      ELSE IF (LDISPL) THEN
         IF (ASSDIS(DEVICE)) GOTO 60
      ELSE
         IF (OPT(22623)) THEN
            IF (ASSSIZ(FLTYPE,FLSIZE,DRSIZE)) GOTO 60
            IF (ASSNEW(DEVICE,FLTYPE,FLSIZE,DRSIZE)) GOTO 60
         ELSE
            IF (ASSOLD(DEVICE)) GOTO 60
         ENDIF
      ENDIF
C
C Verify successful assignment
C
      IF (.NOT.OPTNO(-3419)) THEN
         WRITE (RECORD,70) DEVICE
         IF (SEMCON(RECORD(1:18))) GOTO 60
      ENDIF
C
C Return device number in variable N
C
      IF (SETVAR(22400,REAL(DEVICE))) GOTO 60
C
   60 RETURN
C
   70 FORMAT ('Device',I3,' assigned')
C
C Copyright (C) 1993-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
