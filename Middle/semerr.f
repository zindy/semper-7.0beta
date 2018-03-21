C Semper 6 system module SEMERR
C
      SUBROUTINE SEMERR (STRING)
C
C Returns error message string for error number in COMMON variable ERROR
C
      CHARACTER*(*) STRING
C
C     Change: LDM, July 2005
C     Ensure that gfortran does not think of range, unpack as intrinsics
      EXTERNAL RANGE, UNPACKF
      CHARACTER*3 UNPACKF
C
      CHARACTER*80 FORM
      CHARACTER*3 ERRNUM,FMTYPE
      INTEGER LNBLNK
      INTEGER N,IOLC
      INTEGER IOS
C
      INCLUDE 'COMMON'
C
C Clear return string
C
      STRING=' '
C
C See if valid error number generated
C
      IF (ERROR.NE.0) THEN
C
C If previous rewind/read error detected, do not try to access the
C error file SEMPER.ERR
C
         IF (LSERF) GOTO 50
C
C Rewind error message file
C
         REWIND (UNIT=ERMESU,ERR=110,IOSTAT=IOS)
C
C Read next record from error message file
C
   10    READ (ERMESU,20,ERR=110,END=70,IOSTAT=IOS) ERRNUM,FMTYPE,FORM
   20    FORMAT (2A3,1X,A)
C
C Terminating record will have a blank format type
C     (This method preferred to get round short record problem
C      on INTEGER reads. e.g. on Dash860)
C
         IF (FMTYPE .EQ. '   ') GOTO 70
         READ (ERRNUM,30,ERR=110,IOSTAT=IOS) N
         READ (FMTYPE,30,ERR=110,IOSTAT=IOS) IOLC
   30    FORMAT (I3)
C
C See if this is the terminating record
C
         IF (IOLC .EQ. 0) GOTO 70
C
C See if this is the required record
C
         IF (N .EQ. ERROR) THEN
C
C If so, assemble error message string
C
            IF (IOLC .EQ. 1) THEN
               WRITE (STRING,FORM,ERR=90,IOSTAT=IOS)
            ELSE IF (IOLC .EQ. 2) THEN
               WRITE (STRING,FORM,ERR=90,IOSTAT=IOS) UNPACKF(IDERR)
            ELSE IF (IOLC .EQ. 3) THEN
               WRITE (STRING,FORM,ERR=90,IOSTAT=IOS) IDERR
            ELSE IF (IOLC .EQ. 4) THEN
               WRITE (STRING,FORM,ERR=90,IOSTAT=IOS) IDERR,I4IDER
            ELSE IF (IOLC .EQ. 5) THEN
               WRITE (STRING,FORM,ERR=90,IOSTAT=IOS) IDERR,IDERR2
            ELSE IF (IOLC .EQ. 6) THEN
               WRITE (STRING,FORM,ERR=90,IOSTAT=IOS) IDERR,CURRF(IDERR),
     +                                                  CURRB(IDERR)-1
            ELSE IF (IOLC .EQ. 7) THEN
               WRITE (STRING,FORM,ERR=90,IOSTAT=IOS) UNPACKF(IDERR),
     +                                            UNPACKF(IDERR2)
            ELSE IF (IOLC .EQ. 9) THEN
               N = LNBLNK(IDMESS)
               WRITE (STRING,FORM,ERR=90,IOSTAT=IOS) IDMESS(1:N)
            ELSE IF (IOLC .EQ. 10) THEN
               N = LNBLNK(IDMESS)
               WRITE (STRING,FORM,ERR=90,IOSTAT=IOS) IDERR,IDERR2,
     +                                            IDMESS(1:N)
            ENDIF
C
C Otherwise, go back for more
C
         ELSE
            GOTO 10
         ENDIF
      ENDIF
C
   40 ERROR=0
C
      RETURN
C
C Access error file to error file disabled
C Assemble default error message string
C
   50 WRITE (STRING,60) ERROR
   60 FORMAT ('?',I3,': Cannot access information for this error code')
      GOTO 40
C
C Unable to access error file or no record found for this error
C Assemble default error message string
C
   70 WRITE (STRING,80) ERROR
   80 FORMAT ('?',I3,': No information for this error code')
      GOTO 40
C
C Error detected when using format string from error file SEMPER.ERR
C
   90 WRITE (STRING,100) ERROR
  100 FORMAT ('?',I3,': Error using format string from SEMPER.ERR')
      GOTO 40
C
C Fortran error detected during rewind/read
C
  110 WRITE (STRING,120) ERROR
  120 FORMAT ('?',I3,': Fortran error while accessing SEMPER.ERR')
C
C Set flag to indicate Fortran error while accessing SEMPER.ERR
C (disables any further acces to SEMPER.ERR)
C
      LSERF=.TRUE.
      GOTO 40
C
C Copyright (C) 1987-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
