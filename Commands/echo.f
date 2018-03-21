C Semper 6 processing module ECHO
C
      SUBROUTINE ECHO
C
C Provides verb ECHO.  The echoing to standard output (terminal or
C output text window), standard error or file, of data passed to each
C of the six logical output streams (console, diagnostic, log, monitor
C commands and input output streams) is controlled by this command.
C ECHO accepts the options STANDARD OUTPUT (TERMINAL option is treated
C as a synonym for this), STANDARD ERROR or the DEVICE key, to
C determine the physical output stream being referred to.  If the
C DEVICE key is used, the device specified must be an Ascii disc file
C (see ASSIGN FILE).  The options CONSOLE, DIAGNOSTICS, LOG, MONITOR,
C COMMANDS, INPUT, ALL and NONE are used in different combinations to
C turn on or off echo of the six logical output stream to the physical
C output stream in question.  The default echo settings for the
C standard output stream at start-up are CONSOLE, DIAGNOSTICS, NOLOG,
C MONITOR, NOCOMMANDS AND NOINPUT, for the standard error stream the
C default is NONE and for a log file when it is assigned the default
C is also NONE.
C
C Syntax:  Echo :ECHO console diagnostics log monitor commands input +
C                     all none standard output error terminal device=
C
      INTEGER IPACK,IVAL
      LOGICAL OPT,VARSET,CONOPT,OPTNO,SEMMED
C
      INTEGER DEVICE,MEDIUM,I
      LOGICAL LSOUT,LSERR,LDEVIC,ECHFLG(6)
      CHARACTER*11 NAMES(6)
C
      INCLUDE 'COMMON'
C
C Names for options for logical output streams
C
      DATA NAMES / 'console', 'diagnostics', 'log',
     +             'monitor', 'commands', 'input' /
C
C Fault conflict between options STANDARD OUTPUT (or TERMINAL),
C STANDARD ERROR and DEVICE key
C
      LSOUT=OPT(24860).OR.OPT(-219)
      LSERR=OPT(8738)
      LDEVIC=VARSET(6622)
C
      IF (LSOUT.AND.LSERR) THEN
         ERROR=60
         IF (OPT(24860)) THEN
            IDERR=24860
         ELSE
            IDERR=-219
         ENDIF
         IDERR2=8738
         GOTO 100
      ENDIF
C
      IF (LSOUT.AND.LDEVIC) THEN
         ERROR=60
         IF (OPT(24860)) THEN
            IDERR=24860
         ELSE
            IDERR=-219
         ENDIF
         IDERR2=6622
         GOTO 100
      ENDIF
C
      IF (LSERR.AND.LDEVIC) THEN
         ERROR=60
         IDERR=8738
         IDERR2=6622
         GOTO 100
      ENDIF
C
C If STANDARD OUTPUT (or TERMINAL) option is set, pick up current
C echo settings for standard output
C
      IF (LSOUT) THEN
         DO 10 I = 1,6
            ECHFLG(I) = SOUFLG(I)
   10    CONTINUE
      ENDIF
C
C If STANDARD ERROR option is set, pick up current echo settings for
C standard error
C
      IF (LSERR) THEN
         DO 20 I = 1,6
            ECHFLG(I) = SERFLG(I)
   20    CONTINUE
      ENDIF
C
C If DEVICE key is set, pick up current echo settings for that device
C from the device table
C
      IF (LDEVIC) THEN
C
C Fetch value of DEVICE key
C
         DEVICE=IVAL(6622)
C
C Fetch corresponding medium number
C
         IF (SEMMED(DEVICE,MEDIUM)) GOTO 100
C
C Fault medium number if not indicating a file
C
         IF (MEDIUM.NE.MEDFL) THEN
            ERROR=29
            IDERR=DEVICE
            GOTO 100
         ENDIF
C
C Fault disc device type if not a text file
C
         IF (DVTYP(DEVICE).NE.FLTTEX) THEN
            ERROR=124
            IDERR=DEVICE
            GOTO 100
         ENDIF
C
C Pick up current echo settings
C
         DO 30 I=1,6
            ECHFLG(I)=FILFLG(DEVICE,I)
   30    CONTINUE
      ENDIF
C
C Fault conflict between options ALL and NONE
C
      IF (CONOPT(2092,23014)) GOTO 100
C
C If option ALL is set, turn on echoing of all logical output streams
C
      IF (OPT(2092)) THEN
         DO 40 I = 1,6
            ECHFLG(I) = .TRUE.
   40    CONTINUE
      ENDIF
C
C If option NONE is set, turn off echoing of all logical output streams
C
      IF (OPT(23014)) THEN
         DO 50 I = 1,6
            ECHFLG(I) = .FALSE.
   50    CONTINUE
      ENDIF
C
C See if any individual logical output stream it to be turned on or off
C
      DO 60 I = 1,6
         IF (OPT  (IPACK(NAMES(I)))) ECHFLG(I) = .TRUE.
         IF (OPTNO(IPACK(NAMES(I)))) ECHFLG(I) = .FALSE.
   60 CONTINUE
C
C If STANDARD OUTPUT (or TERMINAL) option is set, return echo settings
C for the standard output stream
C
      IF (LSOUT) THEN
         DO 70 I = 1,6
            SOUFLG(I) = ECHFLG(I)
   70    CONTINUE
      ENDIF
C
C If STANDARD ERROR option is set, return echo settings for the
C standard error stream
C
      IF (LSERR) THEN
         DO 80 I = 1,6
            SERFLG(I) = ECHFLG(I)
   80    CONTINUE
      ENDIF
C
C If device key is set, return echo settings for that device back into
C the device table
C
      IF (LDEVIC) THEN
         DO 90 I = 1,6
            FILFLG(DEVICE,I) = ECHFLG(I)
   90    CONTINUE
      ENDIF
C
  100 RETURN
C
C Copyright (C) 1989,1993:  Synoptics Ltd,  All Rights Reserved
C
      END
