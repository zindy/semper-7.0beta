C Semper 6 subsidiary module SHOW6
C
      LOGICAL FUNCTION SHOW6(ALL,N1,N2)
      INTEGER N1,N2
      LOGICAL ALL
C
      LOGICAL SEMCON,SEMIOE,SHOWLF,ABANDN
C
      INCLUDE 'COMMON'
C
      INTEGER IOLC,IOSIOE,N
C
      INTEGER IOS
      CHARACTER*80 FORM
      CHARACTER*3 ERRNUM,FMTYPE
C
      INTEGER ZERO
      PARAMETER (ZERO=0)
      CHARACTER*3 DOTS
C
      SHOW6 = .TRUE.
      DOTS = '...'
C
      IF (SHOWLF( )) GOTO 80
C
C Rewind error message file
C
      REWIND (ERMESU,ERR=60,IOSTAT=IOS)
C
C Check for abandon
C
   10 IF (ABANDN(ERROR)) GOTO 80
C
C Read next line from error message file
C
      READ (ERMESU,20,ERR=60,END=70,IOSTAT=IOS) ERRNUM,FMTYPE,FORM
   20 FORMAT(2A3,1X,A)
C
C Terminating record will have a blank format type
C     (This method preferred to get round short record problem
C      on INTEGER reads. e.g. on Dash860)
C
      IF (FMTYPE .EQ. '   ') GOTO 70
C
      READ (ERRNUM,30,ERR=60,IOSTAT=IOS) N
      READ (FMTYPE,30,ERR=60,IOSTAT=IOS) IOLC
   30 FORMAT (I3)
C
C See if terminating record
C
      IF (IOLC.EQ.0) GOTO 70
C
C See if this error message not to be output
C
      IF (.NOT.ALL) THEN
         IF (N.LT.N1 .OR. N.GT.N2) GOTO 10
      ENDIF
C
C Assemble error message string
C
      IF (IOLC.EQ. 1) THEN
         WRITE (RECORD,FORM,ERR=40)
      ELSE IF (IOLC.EQ.2) THEN
         WRITE (RECORD,FORM,ERR=40) DOTS
      ELSE IF (IOLC.EQ.3) THEN
         WRITE (RECORD,FORM,ERR=40) ZERO
      ELSE IF (IOLC.EQ.4 .OR. IOLC.EQ.5) THEN
         WRITE (RECORD,FORM,ERR=40) ZERO,ZERO
      ELSE IF (IOLC.EQ.6) THEN
         WRITE (RECORD,FORM,ERR=40) ZERO,ZERO,ZERO
      ELSE IF (IOLC.EQ.7) THEN
         WRITE (RECORD,FORM,ERR=40) DOTS,DOTS
      ELSE IF (IOLC.EQ.9) THEN
         WRITE (RECORD,FORM,ERR=40) DOTS
      ELSE IF (IOLC.EQ.10) THEN
         WRITE (RECORD,FORM,ERR=40) ZERO,ZERO,DOTS
      ELSE
         GOTO 10
      ENDIF
C
C Print error message string
C
      IF (SEMCON(RECORD)) GOTO 80
      GOTO 10
C
C Some form of error in FORMAT or internal WRITE
C
   40 WRITE (RECORD,50) N
   50 FORMAT('Error in FORMAT for error',I4,' format is:')
      IF (SEMCON(RECORD)) GOTO 80
      IF (SEMCON(FORM)) GOTO 80
      GOTO 10
C
C Fortran error detected during rewind/read of error message file
C
   60 INQUIRE(ERMESU,NAME=FORM)
      IOSIOE = IOS
      IF (SEMIOE(IOSIOE,ERMESU,FORM)) GOTO 80
      GOTO 80
C
C Scan through error message file complete
C
   70 SHOW6 = .FALSE.
C
   80 RETURN
C
C Copyright (C) 1987-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
