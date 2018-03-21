C Semper 6 processing module REINIT
C
      SUBROUTINE REINIT
C
C REINITIALISE command
C
      INTEGER IPACK,IVAL
      LOGICAL SEMMED,ASSNAM,YESNO,SEMCLS,VARSET,ASSHDR,OPTNO,SEMCON
C
      INTEGER   DEVICE,MEDIUM,I,FLTYPE,SLOTS,SLTMAX,DRSIZE
      LOGICAL   DONOT
C
      INCLUDE 'COMMON'
C
      INTEGER NAME(FILMAX+1)
      CHARACTER*(FILMAX) FILENM
C
C Fetch value for DEVICE key
C
      DEVICE=IVAL(6622)
C
C Fetch medium number for device (faults bad device number and device
C not assigned)
C
      IF (SEMMED(DEVICE,MEDIUM)) GOTO 30
C
C Fault medium if not disc or memory
C
      IF (.NOT.(MEDIUM.EQ.MEDDC.OR.MEDIUM.EQ.MEDVM)) THEN
         ERROR=29
         IDERR=DEVICE
         GOTO 30
      ENDIF
C
C Check to see if device is write-protected
C
      IF (PROTN(DEVICE).NE.0) THEN
         ERROR=41
         IDERR=DEVICE
         GOTO 30
      ENDIF
C
C Fetch device type
C
      FLTYPE=DVTYP(DEVICE)
C
C If picture storage device, close any associated pictures
C
      IF (FLTYPE.EQ.FLTPIC) THEN
         DO 10 I=1,NLPS
            IF (DEVN(I).EQ.DEVICE) THEN
               IF (SEMCLS(I)) GOTO 30
            ENDIF
   10    CONTINUE
C
C If program library, check to see if device is in use
C
      ELSE IF (FLTYPE.EQ.FLTRUN) THEN
         DO 20 I=1,INPLEV
            IF (INPDEV(I).EQ.DEVICE) THEN
               ERROR=113
               IDERR=DEVICE
               GOTO 30
            ENDIF
   20    CONTINUE
      ENDIF
C
C See if SLOTS key is set
C
      IF (VARSET(30895)) THEN
C
C Fetch value for SLOTS key
C
         SLOTS=IVAL(30895)
C
C Fault bad value for SLOTS key
C
         IF (SLOTS.LT.1) THEN
            ERROR=3
            IDERR=30895
            GOTO 30
         ENDIF
C
C Otherwise, determine default value for SLOTS key according to
C device type
C
      ELSE
         IF (FLTYPE.EQ.FLTPIC) THEN
            SLOTS=2002
         ELSE IF (FLTYPE.EQ.FLTRUN) THEN
            SLOTS=64
         ELSE IF (FLTYPE.EQ.FLTHEL) THEN
            SLOTS=400
         ENDIF
      ENDIF
C
C Determine the directory size in logical disc blocks according to the
C device type
C
      IF (FLTYPE.EQ.FLTPIC) THEN
         SLTMAX=LNBUF/LNINT4/2
         DRSIZE=1+(MIN(SLOTS,2002,SLTMAX)*2*LNINT4-1)/LNBLK
      ELSE IF (FLTYPE.EQ.FLTRUN) THEN
         SLTMAX=LNBUF/LNINT
         DRSIZE=(3+MIN(SLOTS,SLTMAX))*PRCSZE
      ELSE IF (FLTYPE.EQ.FLTHEL) THEN
         DRSIZE=SLOTS
      ENDIF
C
C Seek confirmation for reinitialising the file
C
      IF (MEDIUM.EQ.MEDDC) THEN
C
C Fetch device file name
C
         IF (ASSNAM(1,DEVICE,NAME)) GOTO 30
C
C Convert file name into character variable form
C
         CALL SEMCHS(FILENM,NAME(2),NAME(1))
C
C Prompt for confirmation
C
         IF (YESNO('Wipe existing data in ',FILENM(1:NAME(1)),DONOT))
     +      GOTO 30
      ELSE IF (MEDIUM.EQ.MEDVM) THEN
         IF (YESNO('Wipe existing data in ','memory',DONOT)) GOTO 30
      ENDIF
C
      IF (DONOT) THEN
         IF (SEMCON('No action taken')) GOTO 30
         GOTO 30
      ENDIF
C
C Record new directory size
C
      DRSIZ(DEVICE)=DRSIZE
C
C Rewrite file header and create empty directory
C
      IF (ASSHDR(2,DEVICE)) GOTO 30
C
C Verify successful reinitialisation
C
      IF (.NOT.OPTNO(-3419)) THEN
         WRITE (RECORD,40) DEVICE
         IF (SEMCON(RECORD)) GOTO 30
      ENDIF
C
   30 RETURN
C
   40 FORMAT ('Device',I3,' reinitialised')
C
C Copyright (C) 1993: Synoptics Ltd, All Rights Reserved
C
      END
