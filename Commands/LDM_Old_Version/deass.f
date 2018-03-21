C Semper 6 processing module DEASS
C
      SUBROUTINE DEASS
C
C DEASSIGN command
C
      INTEGER IVAL !,IPACK
      LOGICAL SEMMED,SEMCLS,OPT,SEMDPD,YESNO,OPTNO,SEMCON,PRUPRI
      LOGICAL FSDE61,FILCLS,ASSNAM
C
      INTEGER DEVICE,MEDIUM,I,N,MODE
      LOGICAL LDISPL,LDELET,DONOT
C
      INCLUDE 'COMMON'
C
      INTEGER NAME(FILMAX+1)
      CHARACTER*(FILMAX) FILENM
C
      INTEGER*4 I40,I41
      PARAMETER ( I40 = 0, I41 = 1 )
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
         GOTO 50
      ENDIF
C
C If DISPLAY option set, device number must be 1
C
      IF (LDISPL) THEN
         DEVICE=1
C
C Otherwise, device number given by DEVICE key
C
      ELSE
         DEVICE=IVAL(6622)
      ENDIF
C
      IDERR=DEVICE
C
C Fetch medium number for device (faults bad device number and device
C not assigned)
C
      IF (SEMMED(DEVICE,MEDIUM)) GOTO 50
C
C If picture storage device, close any associated pictures
C
      IF (DVTYP(DEVICE).EQ.FLTPIC) THEN
         DO 10 I=1,NLPS
            IF (DEVN(I).EQ.DEVICE) THEN
               IF (SEMCLS(I)) GOTO 50
            ENDIF
   10    CONTINUE
C
C If program library, check to see if device is in use
C
      ELSE IF (DVTYP(DEVICE).EQ.FLTRUN) THEN
         DO 20 I=1,INPLEV
            IF (INPDEV(I).EQ.DEVICE) THEN
               ERROR=113
               GOTO 50
            ENDIF
   20    CONTINUE
      ENDIF
C
C Deassign the device according to its type
C
C Log file
C ========
C
      IF (MEDIUM.EQ.MEDFL) THEN
C
C See if DELETE option is set
C
         LDELET=OPT(6612)
C
C Close the log file (and delete if DELETE option is set)
C
         IF (FILCLS(DVHAN(DEVICE),LDELET)) GOTO 50
C
C Display
C =======
C
      ELSE IF (MEDIUM.EQ.MEDDS) THEN
C
C Delete all display partitions and pictures
C
         DO 30 I=1,NDPDS
            IF (SEMDPD(1,I)) GOTO 50
            DPTYP=-1
            IF (SEMDPD(2,I)) GOTO 50
   30    CONTINUE
C
C Deassign the display
C
         IF (FSDE61(ERROR)) GOTO 50
C
C Ignore DELETE option
C
         LDELET=.FALSE.
C
C Disc
C ====
C
      ELSE IF (MEDIUM.EQ.MEDDC) THEN
C
C Fetch device file name
C
         IF (ASSNAM(1,DEVICE,NAME)) GOTO 50
C
C If scratch disc, set flag to delete it unconditionally
C
         IF (DVSCR(DEVICE).NE.0) THEN
            LDELET=.TRUE.
C
C Otherwise, see if DELETE option is set
C
         ELSE IF (OPT(6612)) THEN
C
C Check to see if device is write-protected
C
            IF (PROTN(DEVICE).NE.0) THEN
               ERROR=41
               GOTO 50
            ENDIF
C
C Convert file name into character variable form
C
            CALL SEMCHS(FILENM,NAME(2),NAME(1))
C
C Seek confirmation for deleting the file
C
            IF (YESNO('Delete file ',FILENM(1:NAME(1)),DONOT)) GOTO 50
C
            IF (DONOT) THEN
               IF (SEMCON('No action taken')) GOTO 50
               GOTO 50
            ENDIF
C
C Set flag to close and delete disc file
C
            LDELET=.TRUE.
C
C Otherwise, set flag simply to close the disc file
C
         ELSE
            LDELET=.FALSE.
         ENDIF
C
C Close and possibly delete the disc file
C
         IF (LDELET) THEN
            MODE=7
         ELSE
            MODE=6
         ENDIF
C
         CALL MCDC61(MODE,DEVICE,I40,0,NAME,ERROR)
C
         IF (ERROR.NE.0) GOTO 50
C
C Memory
C ======
C
      ELSE IF (MEDIUM.EQ.MEDVM) THEN
C
C Free memory
C
         CALL MCDC61(10,DEVICE,I40,0,RB1,ERROR)
C
         IF (ERROR.NE.0) GOTO 50
C
C Ignore DELETE option
C
         LDELET=.FALSE.
      ENDIF
C
C If program device, remove it from program priority queue
C
      IF (DVTYP(DEVICE).EQ.FLTRUN) THEN
         IF (PRUPRI(2,DEVICE)) GOTO 50
      ENDIF
C
C Record device deassignment
C
      MEDN(DEVICE)=0
C
C Verify successful deassignment
C
      IF (.NOT.OPTNO(-3419)) THEN
         WRITE (RECORD,60) DEVICE
C
         IF (LDELET) THEN
            RECORD(11:17)='deleted'
            N=17
         ELSE
            RECORD(11:20)='deassigned'
            N=20
         ENDIF
C
         IF (SEMCON(RECORD(1:N))) GOTO 50
      ENDIF
C
   50 RETURN
C
   60 FORMAT ('Device',I3,' ')
C
C Copyright (C) 1993-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
