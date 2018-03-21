C Semper 6 processing module MONITM
C
      SUBROUTINE MONITM
C
C Provides verb MONITOR, controlling optional facilities for
C monitoring Fortran module calls / results
C
C Routines write debugging information to the monitor output stream
C by means of the routine SEMMON.  The logical flag MONIT controls
C whether or not SEMMON is called.  As well as the output string,
C SEMMON is also passed the name of the calling routine and the
C appropriate monitor channel number.  The logical array MCHANL
C determines whether output is enabled for each individual monitor
C channel.  This verb controls the settings of MONIT and MCHANL.
C
C Overall control over monitoring is provided by means of the keys ON
C and OFF.  In addition, each monitor channel can be explicitly enabled
C or disabled, either by specifying one of the options PROCESSING,
C ROWIO, OPENS or RANGE or by means of the CHANNEL key (negated for
C off).  The options ALL or NONE can be used to turn all channels on
C or off.  If any monitor channel is enabled, the flag variable MONIT
C is set.  If option SHOW is set, the monitor status and the status of
C each of the monitor channels is output to the console output stream.
C
      INTEGER IVAL
      LOGICAL CONOPT,OPT,OPTNO,SEMCON
C
      INTEGER NAMES(4),I,ICHANN
C
C Packed names
C
      INTEGER NON,NOFF,NALL,NNONE,NCHANN,NSHOW,NPROCE
      INTEGER NROWIO,NOPENS,NRANGE
      PARAMETER (NON=24560,NOFF=24246)
      PARAMETER (NALL=2092,NNONE=23014,NCHANN=5121,NSHOW=30735)
      PARAMETER (NPROCE=26335,NROWIO=29423,NOPENS=24645,NRANGE=28854)
C
      INCLUDE 'COMMON'
C
      DATA NAMES / NPROCE,NROWIO,NOPENS,NRANGE /
C
C Fault conflicting options ON and OFF
C
      IF (CONOPT(NON,NOFF)) GOTO 60
C
C Fault conflicting options ALL and NONE
C
      IF (CONOPT(NALL,NNONE)) GOTO 60
C
C Fault bad value for CHANNEL key
C
      ICHANN = IVAL(NCHANN)
      IF (ABS(ICHANN).GT.NMCHAN) THEN
         ERROR = 3
         IDERR = NCHANN
         GOTO 60
      ENDIF
C
C If option ALL is set, enable all monitor channels
C
      IF (OPT(NALL)) THEN
         DO 10 I=1,NMCHAN
            MCHANL(I)=.TRUE.
   10    CONTINUE
      ENDIF
C
C If option NONE is set, disable all monitor channels
C
      IF (OPT(NNONE)) THEN
         DO 20 I=1,NMCHAN
            MCHANL(I)=.FALSE.
   20    CONTINUE
      ENDIF
C
C See if any of the specific monitor channels options are set
C
      DO 30 I=1,4
         IF (OPT(NAMES(I))) MCHANL(I)=.TRUE.
         IF (OPTNO(NAMES(I))) MCHANL(I)=.FALSE.
   30 CONTINUE
C
C See if specific monitor channel enabled by means of the CHANNEL key
C
      IF (ICHANN.LT.0) MCHANL(-ICHANN)=.FALSE.
      IF (ICHANN.GT.0) MCHANL(ICHANN)=.TRUE.
C
C Turn monitoring on or off, according to options ON or OFF
C
      IF (OPT(NON)) MONIT=.TRUE.
      IF (OPT(NOFF)) MONIT=.FALSE.
C
C If SHOW option is set, print monitor and channel status
C
      IF (OPT(NSHOW)) THEN
         IF (SEMCON(' ')) GOTO 60
C
         RECORD='Monitor status:'
         IF (MONIT) THEN
            RECORD(17:19)=' on'
         ELSE
            RECORD(17:19)='off'
         ENDIF
         IF (SEMCON(RECORD(1:19))) GOTO 60
C
         IF (SEMCON('Channel status:')) GOTO 60
C
         DO 50 I=1,NMCHAN
            WRITE (RECORD,40) I
   40       FORMAT(I5)
            IF (MCHANL(I)) THEN
               RECORD(8:15)=' enabled'
            ELSE
               RECORD(8:15)='disabled'
            ENDIF
            IF (SEMCON(RECORD(1:15))) GOTO 60
   50    CONTINUE
C
         IF (SEMCON(' ')) GOTO 60
      ENDIF
C
   60 RETURN
C
C Copyright (C) 1987,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
