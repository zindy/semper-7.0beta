C Semper 6 system module SEMINI
C
      SUBROUTINE SEMINI
C
C Called at start of session to perform session Initialisation
C
      INTEGER LNBLNK,IPACK
      LOGICAL DISC,EQINIT,EQNQRE,EQSETS,EQTERM,FSIN61,FSLQ61,FORTRD
      LOGICAL SEMDPD,SEMDIA,SEMCON,SEMSIG,SEMIN2,SEMIN5,SETPPR
      LOGICAL SX11IN
C
      CHARACTER*255 UCLINE,LINE,FILENM
      CHARACTER*53 STRIKE
      LOGICAL FOUND
      INTEGER IND,IU,IOS
C
      LOGICAL EXITST
      INTEGER*4 I4N,I4ZERO,I4ONE,BLKN
      PARAMETER (I4ZERO=0,I4ONE=1)
      INTEGER STRING(80),I,N
      CHARACTER*11 DATSTR
      CHARACTER*8  TIMSTR
C
C Default number of partitions
C
      INTEGER DPDNUM
      PARAMETER (DPDNUM=100)
C
      INCLUDE 'COMMON'
      INCLUDE 'EVENTS'
C
      EQUIVALENCE (RB1,STRING)
C
C Maximum size of device name string array
C
      INTEGER NAMSZE
      PARAMETER ( NAMSZE = FILMAX+1 )
C
C Number of logical blocks to accomodate device name string
C
      INTEGER NAMBLK
      PARAMETER ( NAMBLK = 1+(NAMSZE-1)/LNBLK )
C
      CHARACTER*34 SERNUM
C
C The following format for the serial number MUST NOT be changed as
C it is required for the Production system serial number patcher
C
C     SERNUM = 'Serial Number SIGRXV11004ESY000000'
      SERNUM = 'Semper Build  7.0-Beta01-July-2005'
C
      EXITST = .FALSE.
C
C Set-up carriage-control flags for terminal output
C
      TERCR = .TRUE.
      TERLF = .TRUE.
C
      TERXCR = TERCR
      TERXLF = TERLF
C
C Default page size (79,24) and aspect ratio (2.00), output line count
C and pagination modes (PROMPT and NOWRAP)
C
      TERWID = 128
      TERLEN = 128
      TERASP = 2.00
      TERCNT = 0
      TERPRO = .TRUE.
      TERWRA = .FALSE.
      TERQUI = .TRUE.
C
C Default echo settings for standard output and error streams
C
      SOUCON = .TRUE.
      SOUDIA = .TRUE.
      SOULOG = .FALSE.
      SOUMON = .TRUE.
      SOUCOM = .FALSE.
      SOUINP = .FALSE.
C
      SERCON = .FALSE.
      SERDIA = .FALSE.
      SERLOG = .FALSE.
      SERMON = .FALSE.
      SERCOM = .FALSE.
      SERINP = .FALSE.
C
C Common block initialisation
C
      ERROR = 0
      IDERR = 0
      IDERR2 = 0
      CURRWH = 0
      BUFROW = 0
      VERB = 0
      OPLPN = 0
      PICSEL = 0
      REQDCF = .FALSE.
      REQDTS = .FALSE.
      REQFSF = .FALSE.
      REQDRR = .FALSE.
      NOERCN = .TRUE.
      MONIT = .FALSE.
      JMPDST = 0
      NPMODU = 0
      LBLINC = .FALSE.
C
      INPLEV = 0
      FORLEV = 0
      LOCLEV = 0
      VRBLEV = 0
      INPFOR(0) = 0
      INPLOC(0) = 0
      INPDEV(0) = 0
      INPSLT(0) = 0
      INPLIN(0) = 0
C
C Set command interpreter and program index values
C
      LINPTR = 0
      LINLEN = 0
      LINNUM = 0
      LASTSC = 0
      CURRWH = 1
C
      LINDEV = -1
      LINSLT = -1
C
      OBEYME = .FALSE.
C
C Initialise keyboard buffer count and pointer
C
      KBUFN = 0
      KBUFP = 0
C
C Initialise EVOPEN/EVCLOS flag
C
      LEVENT = .FALSE.
C
C Initialise cursor keys step size
C
      KBSTEP = 1
C
C ------ MONITOR ------
C
      DO 10 I=1,NMCHAN
         MCHANL(I)=.FALSE.
   10 CONTINUE
C
C ------ ------- ------
C
      DO 20 I = 1,NLPS
         WHEN(I) = 0
   20 CONTINUE
C
      DO 30 I = 1,NDVS
         MEDN(I) = 0
   30 CONTINUE
C
C Initialise error strings and flags
C
      ERRREC = ' '
      TRPREC = ERRREC
      LSERF = .TRUE.
C
      LIOVLD = .FALSE.
C
C ****** CHANGE ******
C
C Initialise contents of Semper variable table: other variables can be
C assigned initially by adding their names to this list (preserving
C the terminating 1) and their values to the list below
C ?? NB: CFRAME to be added to prot list in due course
C
      VNAMES(1) = 29163
      VNAMES(2) = 25960
      VNAMES(3) = -8220
      VNAMES(4) = 23000
      VNAMES(5) = 3260
      VNAMES(6) = 6779
      VNAMES(7) = 30612
      VNAMES(8) = 5058
      VNAMES(9) = 5301
      VNAMES(10) = 28920
      VNAMES(11) = -1967
C
C === end of protected variables ( see NPROT in PARAMS.FOR ) ===
C === should assert that NPROT is correct here - but require ===
C === events and I/O etc. to be properly initialised ??      ===
C
      VNAMES(12) = -722
      VNAMES(13) = 21174
      VNAMES(14) = 20864
      VNAMES(15) = 21001
      VNAMES(16) = 21032
      VNAMES(17) = 30560
      VNAMES(18) = 4960
      VNAMES(19) = 10360
      VNAMES(20) = -11201
C
C Say how many variables are set at present
C
      NSEMVE = 20
C
C Initialise values for variables listed above
C
      ROVVS(1) = 6.46
      ROVVS(2) = PI
      ROVVS(3) = 1.
      ROVVS(4) = 0.
      BATCH    = 0.
      DISPLA   = 1001.
      SELECT   = 2001.
      CFRAME   = 1.
      CLUT     = 1.
      RC       = 0.
      UIF      = 0.
      TRAP     = 0.
      VMIN     = 0.
C
C      VMAX and VMEAN set up later
C
      VME2     = 0.
      VSD      = 0.
      CD       = 2.
      FS       = 1.
      DOLLAR   = 0.
C
C Program text management - number of devices assigned
C
      PTNUMB = 0
C
C Other systems untested as yet - leave closing units for now
C
C
      CALL GETCL(UCLINE)
      CALL SEMILC(UCLINE,LINE)
C
C Check for monitor switch
C
      IND = INDEX(LINE,'/monitor')
      IF (IND .EQ. 0) IND = INDEX(LINE,'-monitor')
      IF (IND .NE. 0) THEN
         LINE(IND:IND+7) = '       '
         UCLINE(IND:IND+7) = '       '
         MONIT = .TRUE.
         DO 40 I = 1,NMCHAN
            MCHANL(I)=.TRUE.
   40    CONTINUE
      ENDIF
C
C Check for batch switch
C
      IND = INDEX(LINE,'/batch')
      IF (IND .EQ. 0) IND = INDEX(LINE,'-batch')
      IF (IND .NE. 0) THEN
         LINE(IND:IND+5) = '     '
         UCLINE(IND:IND+5) = '     '
         BATCH = 1.0
      ENDIF
C
C
C Signal startup
C
      IF (SEMSIG(SSIGBE,LINE)) GOTO 170
C
C Initialise event management
C
      IF ( EQINIT ( 1 ) ) THEN
         RECORD = 'Event queue initialisation'
         GOTO 180
      ENDIF
C
      IF ( BATCH .EQ. 0.0 ) THEN
         IF ( EQSETS ( MKEY, QRUN ) ) THEN
            RECORD = 'KEYBOARD queue initialisation'
            GOTO 180
         ENDIF
C
         IF ( EQNQRE ( MKEY, I, N, N )) THEN
            RECORD = 'KEYBOARD queue enquiry'
            GOTO 180
         ENDIF
C
         IF ( I .EQ. QCLOSE ) THEN
            BATCH = 1.0
         ELSE
            IF ( EQSETS ( MBREAK, QRUN ) ) THEN
               RECORD = 'BREAK queue initialisation'
               GOTO 180
            ENDIF
         ENDIF
      ENDIF
C
C X11 initialisation
C
      FOUND = BATCH .EQ. 1.0
      IF (SX11IN(FOUND)) THEN
         RECORD = 'Window initialisation'
         GOTO 180
      ENDIF
C
C Now set up page prompt
C
      IF (SETPPR()) THEN
         RECORD = 'Call to SETPPR'
         GOTO 180
      ENDIF
C
C Initialise framestore routines
C
      IF (FSIN61(ERROR)) THEN
         RECORD = 'Call to FSIN61'
         GOTO 180
      ENDIF
C
C Help startup syntax
C
      IND = INDEX(LINE,'/?')
      IF (IND .EQ. 0) IND = INDEX(LINE,'-?')
      IF (IND .NE. 0) THEN
         CALL SEMUSA
         GOTO 200
      ENDIF
C
C Open run file, library file and error message file:
C adjust OPEN statement parameters as necessary locally
C
C Open run file read-only, in case write-protected
C
      IU = RUNFLE
      IND = INDEX(LINE,'/norun')
      IF (IND .EQ. 0) IND = INDEX(LINE,'-norun')
      IF (IND .EQ. 0) THEN
         IF (SEMIN2(LINE,UCLINE,FILENM,'r','.run',FOUND)) GOTO 270
      ELSE
         FOUND = .FALSE.
         LINE(IND:IND+5) = '      '
         UCLINE(IND:IND+5) = '      '
      ENDIF
      IF (FOUND) THEN
         IF (FORTRD(IU,FILENM,IOS)) GOTO 80
         INPUT = RUNFLE
      ELSE
         IF (IND .EQ. 0) THEN
            IND = LNBLNK(FILENM)
            IF (IND .EQ. 0) IND = 1
            RECORD = 'Warning: Run file '//FILENM(1:IND)//' not found'
            IF (SEMDIA(RECORD,NDIWAR)) GOTO 200
         ENDIF
         INPUT = TERM1
      ENDIF
C
C Open error message file read-only, allowing sharing
C
      IU = ERMESU
      IF (SEMIN2(LINE,UCLINE,FILENM,'e','.err',FOUND)) GOTO 270
      IF (FOUND) THEN
         IF (FORTRD(IU,FILENM,IOS)) GOTO 80
         LSERF = .FALSE.
      ELSE
         IND = LNBLNK(FILENM)
         IF (IND .EQ. 0) IND = 1
         RECORD = 'Warning: Error message file '//FILENM(1:IND)//
     +            ' not found'
         IF (SEMDIA(RECORD,NDIWAR)) GOTO 200
         IF (SEMDIA('         Errors will be displayed by number only',
     +      NDIWAR)) GOTO 200
      ENDIF
C
      GOTO 120
C
C OPEN error returns
C
   80 CONTINUE
      EXITST = .TRUE.
      WRITE (RECORD,90) IOS,IU
      RECORD(43:) = FILENM(1:LNBLNK(FILENM))
      GOTO 190
   90 FORMAT ('Error code ',I6,' on unit ',I2,' opening file ')
C
C Set NDPDS, which limits the maximum number of display partitions that
C can be defined within a session; value may be reset freely at this
C point
C
  120 IF (SEMIN5(LINE,UCLINE,'p',NDPDS,1,DPDMAX,DPDNUM)) GOTO 270
C
C Set LUTLEN and LUTMAX by calling the enquiry routine FSLQ61.  LUTLEN
C and LUTMAX define the look-up table size and range for the display
C device.  The value of LUTLEN is needed here in order to determine the
C size of the work disc.  The values of LUTLEN and LUTMAX remain fixed
C for the duration of a Semper session.  LUTLEN and LUTMAX however are
C not compile-time PARAMETERs to avoid having different compiled
C versions of Semper to cope with different configurations of the
C display device (workstation screens mainly).
C
      IF (FSLQ61(LUTLEN,LUTMAX)) THEN
         RECORD = 'Call to FSLQ61'
         GOTO 180
      ENDIF
C
C Now set MAX and MEAN from the expected maximum value
C
      VMAX  = REAL(LUTLEN-1)
      VMEAN = (VMAX-VMIN)/2.0
C
C Determine number of disc blocks to hold each display look-up table
C on the work disc
C
      LUTSZE=1+(3*LUTLEN*LNINT-1)/LNBLK
C
C Set NLUTS, which limits the maximum number of look-up tables that can
C be defined.  NLUTS may not exceed LUTNUM (compile time limit) and may
C be less than LUTNUM if the size of the work disc will not allow for
C LUTNUM look-up tables.
C
      I = MIN(25,LUTNUM)
      IF (SEMIN5(LINE,UCLINE,'l',NLUTS,1,LUTNUM,I)) GOTO 270
C
C If anything is left in the command line fault it!
C (Could preassign discs etc. in a later version...)
C
      I = LNBLNK(UCLINE)
      IF (I .NE. 0) THEN
         RECORD = 'Error in command line near:'//UCLINE(1:I)
         GOTO 190
      ENDIF
C
C Initialise CBNUM, CBSIZE to number of cache buffers provided within
C module DISC, and their size in disc blocks; initialisation provided
C here in case any local mechanism can make the values run-time
C assignable.  (Set CBNUM=0, CBSIZE=1 if installation is non-caching.)
C
      CBNUM = 8
      CBSIZE = 128
C
C Insert here anything else necessary locally
C
C ****** ****** ******
C
C Initialise cache buffers (if applicable)
C Ignore any error - means disc i/o will simply not be cached
C
      IF (DISC(0,0,I4ZERO,RB1,I4ZERO,0,0)) THEN
         ERROR = 0
      ENDIF
C
C Determine size of work buffer
C
      I4N = NDVS + NDPDS*(LABSZE+DPDSZE) + NLUTS*LUTSZE +
     +      (INPMAX * (LINSZE + IDXSZE))
C
C Create work buffer
C
C Work buffer = virtual memory array
C
      I4N = I4N * LNBLK4
      CALL MCDC61(9,0,I4N,0,RB1,ERROR)
      IF (ERROR .NE. 0) THEN
         RECORD = 'Unable to open work memory'
         GOTO 190
      ENDIF
C
C Initialise area pointers
C
      WRKDVS = 1
      WRKDPD = WRKDVS + (NDVS*NAMBLK)
      WRKLUT = WRKDPD + (NDPDS*(DPDSZE+LABSZE))
      WRKLIN = WRKLUT + (NLUTS*LUTSZE)
      WRKIND = WRKLIN + (INPMAX*LINSZE)
C
C Next area is WRKIND + (INPMAX*IDXSZE)
C
C Initialise device names to null string (store zero character count)
C
      DO 140 N = 1,NDVS
         STRING(1) = 0
         BLKN = N
         IF (DISC(2,WRKDEV,I4ONE,STRING,BLKN,NFMINT,NFMBYT)) GOTO 140
  140 CONTINUE
C
C Initialise display partition descriptors
C
      DPTYP = -1
      DPDEV = 1
      DO 150 N = 1,NDPDS
         DPNUM = N
         IF (SEMDPD(2,N)) GOTO 150
  150 CONTINUE
C
C Initialise look-up tables
C
      DO 160 I = 1,NLUTS
         LUTMOD(I) = 0
  160 CONTINUE
C
      IF (SEMCON('Portable Semper 7.0')) GOTO 200
C     + 'Copyright (C) 1987-1996 Synoptics Ltd.')) GOTO 200
      STRIKE = '-----------------------------------------------------'
      IF (SEMCON(STRIKE)) GOTO 200
      IF (SEMCON(
     + '***** DEMONSTRATION VERSION WITH LIMITED FUNCTIONALITY *****'))
     +   GOTO 200
      IF (SEMCON(
     + '*****   DATA FILES PRODUCED BY THIS VERSION ARE ONLY   *****'))
     +   GOTO 200
      IF (SEMCON(
     + '*****    ACCESSIBLE FROM THE FULL VERSION OF SEMPER    *****'))
     +   GOTO 200
C
      IF (SEMCON(SERNUM)) GOTO 200
C
      IF ( BATCH .NE. 0.0 ) THEN
         IF (SEMCON('Batch mode (no interactive keyboard)')) GOTO 200
      ENDIF
C
      CALL MCTIME(DTBUFF)
C
      RECORD( 1:12)='Started at: '
      RECORD(13:23)=DATSTR(DTBUFF(3),DTBUFF(2),DTBUFF(1))
      RECORD(24:24)=' '
      RECORD(25:32)=TIMSTR(DTBUFF(4),DTBUFF(5),DTBUFF(6))
      IF (SEMCON(RECORD(1:32))) GOTO 200
C
C Signal begin
C
      IF (.NOT.SEMSIG(SSIGGO,LINE)) RETURN
C
  170 IND = LNBLNK(LINE)
      RECORD = 'Call to SEMSIG('//LINE(1:IND)//')'
C
C Here RECORD contains a string to be followed with a failed message
C
  180 IND = LNBLNK(RECORD)
      RECORD(IND+1:) = ' failed in SEMINI'
C
C Here RECORD may conatin a fatal error string
C
  190 IND = LNBLNK(RECORD)
      IF (IND .NE. 0) THEN
         IF (SEMDIA(RECORD(1:IND),NDIFAT)) GOTO 200
      ENDIF
C
C
C Error stop -
C              Report value of error (if reasonable) then output an
C              additional blank line to assist legibility of previous
C              message on Unix systems etc.
C
C              Finally wait a while to allow message to be read
C
  200 IF (ERROR.NE.0 .AND. ERROR .LT. 1000 .AND. ERROR .GT. -1000) THEN
         WRITE(RECORD,210) ERROR
  210    FORMAT('Last known error value is ',I4)
         IF (SEMDIA(RECORD(1:30),NDIFAT)) GOTO 220
      ENDIF
      IF (SEMDIA(' ',NDIFAT)) GOTO 220
  220 CONTINUE
      CALL WAITS(25.0)
C
C Attempt to close event queues ignoring errors (for SUN etc.)
C
      IF (EQSETS(MBREAK,QCLOSE)) GOTO 240
  240 IF (EQSETS(MKEY,QCLOSE)) GOTO 250
  250 IF (EQTERM(1)) GOTO 260
  260 CONTINUE
      CALL SEMXIT(EXITST)
      STOP
C
  270 RECORD = 'Error processing command line'
      GOTO 190
C
C Copyright (C) 1987-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
C
      SUBROUTINE SEMUSA
C
      INCLUDE 'COMMON'
C
      LOGICAL SEMCON
C
      RECORD = 'Semper accepts command line options beginning with'//
     +         ' ''-'' or ''/'''
      IF (SEMCON(RECORD)) GOTO 10
      RECORD = ' For example: -norun and /norun are identical'
      IF (SEMCON(RECORD)) GOTO 10
      RECORD = '(The options are listed in the first form only)'
      IF (SEMCON(RECORD)) GOTO 10
      RECORD = 'This Semper accepts the following options:'
      IF (SEMCON(RECORD)) GOTO 10
      RECORD = '-norun           no automatic startup file'
      IF (SEMCON(RECORD)) GOTO 10
      RECORD =
     + '-run=filename    run file ''filename'' instead of semper.run'
      IF (SEMCON(RECORD)) GOTO 10
      RECORD =
     + '-error=filename  use file ''filename'' instead of semper.err'
      IF (SEMCON(RECORD)) GOTO 10
      RECORD =
     + '-lut=number      specify maximum number of luts to use'
      IF (SEMCON(RECORD)) GOTO 10
      RECORD =
     + '-par=number      specify maximum number of partitions to use'
      IF (SEMCON(RECORD)) GOTO 10
      RECORD =
     + '-batch           force non-interactive session'
      IF (SEMCON(RECORD)) GOTO 10
      RECORD =
     + '-monitor         switch on all diagnostic channels'
      IF (SEMCON(RECORD)) GOTO 10
   10 RETURN
C
C Copyright (C) 1987-1995:  Synoptics Ltd,  All Rights Reserved
C
      END
      SUBROUTINE SEMILC(UCLINE,LCLINE)
      CHARACTER*(*) UCLINE,LCLINE
C
      INCLUDE 'ICSET'
C
      INTEGER LNBLNK
      INTEGER IND,ICH
      LCLINE = UCLINE
C
C Lowercase input line
C
      DO 10 IND = 1,LNBLNK(LCLINE)
         ICH = ICHAR(LCLINE(IND:IND))
         IF (ICH .GE. KUCA .AND. ICH .LE. KUCZ) THEN
            ICH = ICH + (KLCA - KUCA)
            LCLINE(IND:IND) = CHAR(ICH)
         ENDIF
   10 CONTINUE
      RETURN
C
C Copyright (C) 1987-1995:  Synoptics Ltd,  All Rights Reserved
C
      END
