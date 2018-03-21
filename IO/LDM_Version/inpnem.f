C Semper 6 processing module INPNEM
C
      SUBROUTINE INPNEM
C
C Provides command INPUT NEMA/DICOM:
C  Reads pictures from a version of an ACR/NEMA or DICOM format
C  file that is dynamically opened.
C
C     INTEGER*4 UNSIGN
      INTEGER ASCINT,IVALPN,LNBLNK,SEMFRM !, IPACK
      LOGICAL FILSEA,FILSTR,OPT,OPTNO,SEMCON,SEMDIA,SEMLAB,SEMOPN
      LOGICAL SEMROW,SEMSEL,MRDBIN,NEMDAT,NEMOUT,NEMTIM
      LOGICAL RNEMAT,RNEMAS,RTIFI2,RTIFI4,RTSEEK
C
      INTEGER CLASS,FORM,INFORM
C
      INCLUDE 'COMMON'
C
      CHARACTER*(FILMAX) FILENM,FILE
      CHARACTER*255 DESCRP,STRING
      CHARACTER*21 DATIME
      CHARACTER*4 DFNAM
C
      INTEGER ELEM,FD,I,IGROUP,J,K,N,ND,NF,NGROUP,IXFR
C
      INTEGER BITSAL,BITSHF,BITSST,HEIGHT,HIBIT,IWIDTH
      INTEGER NROW,NLAY,NCOL,ORIENT
C
      INTEGER IB1(LNBUF/LNINT),LABEL(256)
      INTEGER BU(LNBUF/LNINT)
C
      LOGICAL EXISTS,DICOM,MOTOR,Q,UNQ,DATED,TIMED,ISINT
C
      EQUIVALENCE (RB1,IB1,LABEL)
      EQUIVALENCE (RB3,BU)
C
      INTEGER*4 I4N,LENGTH,POSN
      INTEGER I2(2) !, IBCHAR
      EQUIVALENCE (I4N,I2)
C
      INTEGER*4 I44
      PARAMETER (I44=4)
C
C     'C' functions
C
      LOGICAL EIKOPE,EIKBYA,EIKCLO
C
C Set defaults - required first, then defaultables
C
      IWIDTH = -1
      HEIGHT = -1
C
      BITSAL = -1
      BITSST = -1
      HIBIT = -1
C
      ORIENT = 1
C
      DATED = .FALSE.
      TIMED = .FALSE.
C
C     Pick up options and mandatory keys
C
      Q = OPTNO(-3419)
      UNQ = OPT(-3419)
C
C     Fetch file name from key NAME, prompting if key absent
C
      DFNAM = '.dgf'
      CALL INPDEF(DFNAM)
      IF (FILSTR(' ',FILE,NF,.FALSE.)) GOTO 220
      IF (NF .EQ. 0) GOTO 220
C
C     See if file exists on the path if reading
C
      IF (FILSEA(FILE(1:NF),DFNAM,FILENM,EXISTS)) GOTO 220
      IF (EXISTS) THEN
         NF  = LNBLNK(FILENM)
      ELSE
C
C     Error - non-existent file requested for read
C
         ERROR  = 130
         IDMESS = FILE(1:NF)
         GOTO 220
      ENDIF
C
C     Try to open the file
C
      IF (EIKOPE(1,FD,FILENM(1:NF))) GOTO 150
C
C     Copy to description
C
      DESCRP = FILENM(1:NF)
      ND = NF
C
      IXFR = 2
      IF (MRDBIN(FD,IXFR,I,NFMBYT,.FALSE.)) GOTO 190
      POSN = 2
C
C File should start with group number
C
      DICOM = .FALSE.
      NGROUP = 8
      IF (I .EQ. NGROUP) THEN
         MOTOR = .FALSE.
      ELSE IF (I .EQ. (NGROUP*256)) THEN
         MOTOR = .TRUE.
      ELSE
C
C May be DICOM header - skip 126 and look for 'DICM'
C
         IXFR = 126
         IF (EIKBYA(1,FD,IB1,IXFR)) GOTO 190
         IXFR = 4
         IF (EIKBYA(1,FD,IB1,IXFR)) GOTO 190
         CALL CFORM(IB1,IB1,NFMBYT,NFMINT,I44)
         IF (IB1(1) .NE. ICHAR('D') .OR.
     +       IB1(1) .NE. ICHAR('I') .OR.
     +       IB1(1) .NE. ICHAR('C') .OR.
     +       IB1(1) .NE. ICHAR('M')) THEN
            GOTO 180
         ENDIF
C
C Looks like DICOM! Get byte ordering...
C
         IXFR = 2
         IF (MRDBIN(FD,IXFR,I,NFMBYT,.FALSE.)) GOTO 190
         POSN = 134
         NGROUP = 2
         IF (I .EQ. NGROUP) THEN
            MOTOR = .FALSE.
         ELSE IF (I .EQ. (NGROUP*256)) THEN
            MOTOR = .TRUE.
         ELSE
            GOTO 180
         ENDIF
         DICOM = .TRUE.
      ENDIF
C
C Restart file ready to read real data
C
      NGROUP = -1
C
C Need to step back over group number
C
      I4N = POSN - 2
C
      IF (.NOT.Q) THEN
         WRITE(STRING,10) FILENM(1:NF)
   10    FORMAT('Scanning file ',A)
         IF (SEMCON(' ')) GOTO 210
         IF (SEMCON(STRING)) GOTO 210
      ENDIF
C
C Seek to group start position
C
   20 IF (RTSEEK(FD,I4N,POSN,FILENM(1:NF))) GOTO 200
C
C At this point we expect a group ID + element 0 + length
C
      IF (RNEMAT(FD,IGROUP,ELEM,LENGTH,POSN,MOTOR)) GOTO 190
C
C Valid group info ?
C
   30 IF (IGROUP .EQ. 0) THEN
         IF (DICOM) THEN
            STRING = 'DICOM/NEMA'
         ELSE
            STRING = 'LUMISYS'
         ENDIF
         I = LNBLNK(STRING)
         WRITE(IDMESS,40) STRING(1:I),FILENM(1:NF)
   40    FORMAT('No image data in ',A,' file ',A)
         GOTO 200
      ENDIF
C
      IF (ELEM .NE. 0) THEN
         WRITE(IDMESS,50) ELEM
   50    FORMAT('Unexpected Group Element ',I6,' at group start')
         GOTO 200
      ENDIF
C
C Length should be 4
C
      IF (LENGTH .NE. 4) GOTO 180
C
      IF (RTIFI4(FD,LENGTH,POSN,MOTOR)) GOTO 190
C
C If this is not a group we are interested in then leap over it
C
      IF (IGROUP .NE. 8 .AND. IGROUP .NE. 16 .AND.
     +    IGROUP .NE. 32 .AND. IGROUP .NE. 40 .AND.
     +    IGROUP .NE. 32736) THEN
         I4N = LENGTH + POSN
         IF (UNQ) THEN
            WRITE(STRING,60) IGROUP
   60       FORMAT('Ignoring entire group (0x',Z4,')')
            IF (SEMCON(STRING)) GOTO 210
         ENDIF
         GOTO 20
      ENDIF
C
C One of the groups we know about
C
      NGROUP = IGROUP
C
C Next tagged element
C
   70 IF (RNEMAT(FD,IGROUP,ELEM,LENGTH,POSN,MOTOR)) GOTO 190
C
C Same group ?
C
      IF (IGROUP .NE. NGROUP) GOTO 30
C
C Acquisition information?
C
      IF (IGROUP .EQ. 8) THEN
         IF (ELEM .EQ. 1) THEN
            GOTO 90
         ELSE IF (ELEM .EQ. 16) THEN
            IF (RNEMAS(FD,BU,LENGTH,POSN)) GOTO 190
            IF (.NOT.Q) THEN
               IF (NEMOUT('Recognition code',
     +                    STRING,BU,LENGTH)) GOTO 210
            ENDIF
            GOTO 70
         ELSE IF (ELEM .EQ. 32) THEN
            IF (RNEMAS(FD,BU,LENGTH,POSN)) GOTO 190
            IF (NEMDAT('Study',STRING,BU,LENGTH,Q)) GOTO 210
            GOTO 70
         ELSE IF (ELEM .EQ. 33) THEN
            IF (RNEMAS(FD,BU,LENGTH,POSN)) GOTO 190
            IF (NEMDAT('Series',STRING,BU,LENGTH,Q)) GOTO 210
            GOTO 70
         ELSE IF (ELEM .EQ. 34) THEN
            IF (RNEMAS(FD,BU,LENGTH,POSN)) GOTO 190
            IF (NEMDAT('Acquisition',STRING,BU,LENGTH,Q)) GOTO 210
            DATIME(1:10) = STRING(1:10)
            DATED = .TRUE.
            GOTO 70
         ELSE IF (ELEM .EQ. 48) THEN
            IF (RNEMAS(FD,BU,LENGTH,POSN)) GOTO 190
            IF (NEMTIM('Study',STRING,BU,LENGTH,Q)) GOTO 210
            GOTO 70
         ELSE IF (ELEM .EQ. 49) THEN
            IF (RNEMAS(FD,BU,LENGTH,POSN)) GOTO 190
            IF (NEMTIM('Series',STRING,BU,LENGTH,Q)) GOTO 210
            GOTO 70
         ELSE IF (ELEM .EQ. 50) THEN
            IF (RNEMAS(FD,BU,LENGTH,POSN)) GOTO 190
            IF (NEMTIM('Acquisition',STRING,BU,LENGTH,Q)) GOTO 210
            DATIME(12:21) = STRING(1:10)
            TIMED = .TRUE.
            GOTO 70
         ELSE IF (ELEM .EQ. 96) THEN
            IF (RNEMAS(FD,BU,LENGTH,POSN)) GOTO 190
            IF (.NOT.Q) THEN
               IF (NEMOUT('Modality',
     +                    STRING,BU,LENGTH)) GOTO 210
            ENDIF
            GOTO 70
         ELSE IF (ELEM .EQ. 112) THEN
            IF (RNEMAS(FD,BU,LENGTH,POSN)) GOTO 190
            IF (.NOT.Q) THEN
               IF (NEMOUT('Manufacturer',
     +                    STRING,BU,LENGTH)) GOTO 210
            ENDIF
            GOTO 70
         ELSE IF (ELEM .EQ. 128) THEN
            IF (RNEMAS(FD,BU,LENGTH,POSN)) GOTO 190
            IF (.NOT.Q) THEN
               IF (NEMOUT('Institution',
     +                    STRING,BU,LENGTH)) GOTO 210
            ENDIF
            GOTO 70
         ELSE IF (ELEM .EQ. 4144) THEN
            IF (RNEMAS(FD,BU,LENGTH,POSN)) GOTO 190
            IF (.NOT.Q) THEN
               IF (NEMOUT('Study Description',
     +                    STRING,BU,LENGTH)) GOTO 210
            ENDIF
            GOTO 70
         ELSE IF (ELEM .EQ. 4160) THEN
            IF (RNEMAS(FD,BU,LENGTH,POSN)) GOTO 190
            IF (.NOT.Q) THEN
               IF (NEMOUT('Department',
     +                    STRING,BU,LENGTH)) GOTO 210
            ENDIF
            GOTO 70
         ELSE IF (ELEM .EQ. 4208) THEN
            IF (RNEMAS(FD,BU,LENGTH,POSN)) GOTO 190
            IF (.NOT.Q) THEN
               IF (NEMOUT('Operator',
     +                    STRING,BU,LENGTH)) GOTO 210
            ENDIF
            GOTO 70
         ELSE IF (ELEM .EQ. 4240) THEN
            IF (RNEMAS(FD,BU,LENGTH,POSN)) GOTO 190
            IF (.NOT.Q) THEN
               IF (NEMOUT('Model',
     +                    STRING,BU,LENGTH)) GOTO 210
            ENDIF
            GOTO 70
         ENDIF
C
C Patient information?
C
      ELSE IF (IGROUP .EQ. 16) THEN
         IF (ELEM .EQ. 16) THEN
            IF (RNEMAS(FD,BU,LENGTH,POSN)) GOTO 190
            IF (.NOT.Q) THEN
               IF (NEMOUT('Patient''s Name',
     +                    STRING,BU,LENGTH)) GOTO 210
            ENDIF
            GOTO 70
         ELSE IF (ELEM .EQ. 32) THEN
            IF (RNEMAS(FD,BU,LENGTH,POSN)) GOTO 190
            IF (.NOT.Q) THEN
               IF (NEMOUT('Patient''s ID',
     +                    STRING,BU,LENGTH)) GOTO 210
            ENDIF
            GOTO 70
         ELSE IF (ELEM .EQ. 48) THEN
            IF (RNEMAS(FD,BU,LENGTH,POSN)) GOTO 190
            IF (NEMDAT('Patient''s Birth',STRING,BU,LENGTH,Q)) GOTO 210
            GOTO 70
         ELSE IF (ELEM .EQ. 50) THEN
            IF (RNEMAS(FD,BU,LENGTH,POSN)) GOTO 190
            IF (NEMTIM('Patient''s Birth',STRING,BU,LENGTH,Q)) GOTO 210
            GOTO 70
         ELSE IF (ELEM .EQ. 64) THEN
            IF (RNEMAS(FD,BU,LENGTH,POSN)) GOTO 190
            IF (.NOT.Q) THEN
               IF (NEMOUT('Patient''s Sex',
     +                    STRING,BU,LENGTH)) GOTO 210
            ENDIF
            GOTO 70
         ENDIF
C
C Image relationship information?
C
      ELSE IF (IGROUP .EQ. 32) THEN
         IF (ELEM .EQ. 17) THEN
            IF (RNEMAS(FD,BU,LENGTH,POSN)) GOTO 190
            IF (.NOT.Q) THEN
               IF (NEMOUT('Series Number',STRING,BU,LENGTH)) GOTO 210
            ENDIF
            GOTO 70
         ELSE IF (ELEM .EQ. 19) THEN
            IF (RNEMAS(FD,BU,LENGTH,POSN)) GOTO 190
            IF (.NOT.Q) THEN
               IF (NEMOUT('Image Number',STRING,BU,LENGTH)) GOTO 210
            ENDIF
            GOTO 70
         ENDIF
C
C Image presentation information?
C
      ELSE IF (IGROUP .EQ. 40) THEN
         IF (ELEM .EQ. 16) THEN
            IF (RTIFI2(FD,HEIGHT,POSN,MOTOR)) GOTO 190
            GOTO 70
         ELSE IF (ELEM .EQ. 17) THEN
            IF (RTIFI2(FD,IWIDTH,POSN,MOTOR)) GOTO 190
            GOTO 70
         ELSE IF (ELEM .EQ. 256) THEN
            IF (RTIFI2(FD,BITSAL,POSN,MOTOR)) GOTO 190
            GOTO 70
         ELSE IF (ELEM .EQ. 257) THEN
            IF (RTIFI2(FD,BITSST,POSN,MOTOR)) GOTO 190
            GOTO 70
         ELSE IF (ELEM .EQ. 258) THEN
            IF (RTIFI2(FD,HIBIT,POSN,MOTOR)) GOTO 190
            GOTO 70
         ENDIF
C
C Pixel data information? (If we don't get this we're shafted!)
C
      ELSE IF (IGROUP .EQ. 32736) THEN
         IF (ELEM .EQ. 16) THEN
C
C Pixel data itself!
C
            GOTO 100
C
         ENDIF
      ENDIF
C
C Unknown or ignorable field - skip data
C
      IF (UNQ) THEN
         WRITE(STRING,80) IGROUP,ELEM,LENGTH
   80    FORMAT('Ignoring item (0x',Z4,',0x',Z4,') of length',I8)
         IF (SEMCON(STRING)) GOTO 210
      ENDIF
C
   90 I4N = POSN + LENGTH
      IF (RTSEEK(FD,I4N,POSN,FILENM(1:NF))) GOTO 200
      GOTO 70
C
C Pixel data found - check fields
C
  100 IF (IWIDTH .EQ. -1 .OR. HEIGHT .EQ. -1) THEN
         IDMESS = 'Missing image dimensions'
         GOTO 200
      ENDIF
C
      IF (BITSAL .LE. 0 .OR .BITSST .LE. 0) THEN
         IDMESS = 'Bit depth not specified in data'
         GOTO 200
      ENDIF
C
      NCOL = IWIDTH
      NROW = HEIGHT
      NLAY = 1
C
      IF (BITSAL .EQ. 16) THEN
         ISINT = .TRUE.
         INFORM = NFMINT
      ELSE IF (BITSAL .EQ. 8) THEN
         ISINT = .FALSE.
         INFORM = NFMBYT
      ELSE
         WRITE(IDMESS,110) BITSAL
  110    FORMAT('Unsupported Bits per pixel allocated value:',I6)
         GOTO 200
      ENDIF
      IF (HIBIT .EQ. -1) HIBIT = BITSST-1
C
      IF (ISINT .AND. BITSST .EQ. 16) THEN
         BITSST = 15
         IF (SEMDIA('Warning: Reducing pixel depth to 15 bits',
     +              NDIWAR)) GOTO 210
      ENDIF
C
C     Calculate bit shift required - 0 implies none
C
      BITSHF = (HIBIT+1) - BITSST
C
      FORM = SEMFRM(INFORM)
      CLASS = NCLIMA
C
C     Open picture
C
      LP1 = 0
      IF (SEMOPN(2,IVALPN(-601),NCOL,NROW,NLAY,CLASS,
     +           FORM,LP1)) GOTO 210
C
C     Form title from description
C
      N = MIN(ND,LBTT2 - LBTT1)
      LABEL(LBNCTT) = N
      CALL SEMICS(DESCRP,LABEL(LBTT1),N)
C
C     Date and Time supplied ?
C
      IF (TIMED .AND. DATED) THEN
         LABEL(LBYEAR) = ASCINT(DATIME(1:4))
         IF (LABEL(LBYEAR) .GT. 1900) THEN
            LABEL(LBYEAR) = LABEL(LBYEAR) - 1900
         ENDIF
         LABEL(LBMON)  = ASCINT(DATIME(6:7))
         LABEL(LBDAY)  = ASCINT(DATIME(9:10))
         LABEL(LBHOUR) = ASCINT(DATIME(12:13))
         LABEL(LBMIN)  = ASCINT(DATIME(15:16))
         LABEL(LBSEC)  = ASCINT(DATIME(18:19))
      ENDIF
C
C     Update label if possible
C
      IF (N.NE.0 .AND. LBLINC) THEN
         IF (SEMLAB(2,LABEL,LP1)) GOTO 210
      ENDIF
C
      DO 140 N = 1,NLAY
C
C        Loop over rows
C
         IF (ORIENT .EQ. 1) THEN
            I = 1
         ELSE
            I = NROW
         ENDIF
C
         I4N = NCOL
         IXFR = NCOL
         IF (ISINT) IXFR = IXFR * 2
C
         DO 130 J = 1,NROW
C
C           Read source row from file
C
            IF (MRDBIN(FD,IXFR,IB1,INFORM,MOTOR)) GOTO 190
C
C           Do we need to bitshift ?
C
            IF (BITSHF .NE. 0) THEN
C
C              Convert between forms if required
C
               IF (.NOT.ISINT) THEN
                  CALL CFORM(IB1,IB1,NFMBYT,NFMINT,I4N)
               ENDIF
               DO 120 K = 1,NCOL
                  IB1(K) = RSHIFT(IB1(K),BITSHF)
  120          CONTINUE
               IF (.NOT.ISINT) THEN
                  CALL CFORM(IB1,IB1,NFMINT,NFMBYT,I4N)
               ENDIF
            ENDIF
C
C     Store source row in LP1
C
            IF (SEMROW(2,IB1,INFORM,I,N,LP1)) GOTO 210
            IF (ORIENT .EQ. 1) THEN
               I = I + 1
            ELSE
               I = I - 1
            ENDIF
  130    CONTINUE
  140 CONTINUE
C
C     Close file
C
      IF ( EIKCLO ( FD ) ) GOTO 160
C
C     Finally, select this image
C
      IF (SEMSEL(LP1)) GOTO 220
      GOTO 220
C
C     Deal with UNIX I/O errors
C
  150 IDMESS = 'Error opening file '//FILENM(1:NF)
      GOTO 170
C
  160 IDMESS = 'Error closing file '//FILENM(1:NF)
  170 ERROR = 77
      GOTO 220
C
  180 IDMESS = 'Unrecognised or unsupported file format'
      GOTO 200
C
  190 IDMESS = 'Error reading file '//FILENM(1:NF)
      GOTO 200
C
C     Closing a file after an error (ignore any error on closing file)
C
  200 ERROR = 77
  210 IF ( EIKCLO ( FD ) ) GOTO 220
C
C     All done
C
  220 RETURN
C
C Copyright (C) 1990-1996 Synoptics Ltd,  All Rights Reserved
C
      END
C
      LOGICAL FUNCTION RNEMAT(FD,GROUP,ELEM,LENGTH,POSN,MOTOR)
      INTEGER FD,GROUP,ELEM
      INTEGER*4 LENGTH,POSN
      LOGICAL MOTOR
C
      LOGICAL RTIFI2,RTIFI4
C
      RNEMAT = .FALSE.
      IF (RTIFI2(FD,GROUP,POSN,MOTOR)) THEN
C
C Failure to read group number probably indicates end of file
C
         GROUP = 0
         GOTO 10
      ENDIF
      RNEMAT = .TRUE.
C
      IF (RTIFI2(FD,ELEM,POSN,MOTOR)) GOTO 10
      RNEMAT = RTIFI4(FD,LENGTH,POSN,MOTOR)
C
   10 RETURN
C
C Copyright (C) 1990-1996 Synoptics Ltd,  All Rights Reserved
C
      END
C
      LOGICAL FUNCTION RNEMAS(FD,IB,LENGTH,POSN)
      INTEGER FD,IB(*)
      INTEGER*4 LENGTH,POSN
C
      INTEGER J
      INTEGER*4 I4N
C
      LOGICAL EIKBYA
C
      INCLUDE 'PARAMS'
C
      J = LENGTH
      I4N = J
C
C Read the bytes
C
      RNEMAS = .TRUE.
      IF (EIKBYA(1,FD,IB,J)) GOTO 10
      POSN = POSN + I4N
      CALL CFORM(IB,IB,NFMBYT,NFMINT,I4N)
      RNEMAS = .FALSE.
   10 RETURN
C
C Copyright (C) 1990-1996 Synoptics Ltd,  All Rights Reserved
C
      END
C
      SUBROUTINE NEMSTR(IB,STRING,LENS,N)
      INTEGER IB(*),LENS
      INTEGER*4 N
      CHARACTER*(*) STRING
C
      INTEGER*4 I,K,L
C
      L = LEN(STRING)
      K = N
      IF (K .GT. L) K = L
C
      STRING = ' '
C
C String length count
C
      LENS = 0
      DO 10 I = 1,K
C
C Terminate if NULL encountered
C
         IBCHAR = ICHAR('?')
         IF (IB(I) .EQ. 0) GOTO 20
         IF (IB(I) .LT. 32) IB(I) = IBCHAR
         STRING(I:I) = CHAR(IB(I))
         LENS = LENS + 1
   10 CONTINUE
   20 CONTINUE
C
C Disallow zero length strings!
C
      IF (LENS .EQ. 0) THEN
         LENS = 1
      ENDIF
      RETURN
C
C Copyright (C) 1990-1996 Synoptics Ltd,  All Rights Reserved
C
      END
C
      LOGICAL FUNCTION NEMOUT(TITLE,STRING,BU,LENGTH)
      CHARACTER*(*) TITLE,STRING
      INTEGER BU(*)
      INTEGER*4 LENGTH
C
      LOGICAL SEMCON
C
      INTEGER E,J,L,S
C
      CHARACTER*78 MESS
C
      L = LEN(TITLE)+1
C
      MESS = TITLE//':'
C
      S = 1
      CALL NEMSTR(BU,STRING,E,LENGTH)
   10 J = INDEX(STRING(S:E),'^')
      IF (J .EQ. 0) THEN
         MESS(L+1:) = STRING(S:E)
      ELSE
         J = S + J - 1
         MESS(L+1:) = STRING(S:J)
      ENDIF
      IF (SEMCON(MESS)) THEN
         NEMOUT = .TRUE.
         RETURN
      ENDIF
      IF (J .NE. 0) THEN
   20    J = J + 1
         IF (STRING(J:J) .EQ. '^') THEN
            IF (J .LT. E) GOTO 20
         ENDIF
         IF (J .LE. E) THEN
            S = J
            MESS(1:L-1) = ' '
            GOTO 10
         ENDIF
      ENDIF
      NEMOUT = .FALSE.
      RETURN
C
C Copyright (C) 1990-1996 Synoptics Ltd,  All Rights Reserved
C
      END
C
      LOGICAL FUNCTION NEMDAT(TITLE,STRING,BU,LENGTH,QUIET)
      CHARACTER*(*) TITLE,STRING
      INTEGER BU(*)
      INTEGER*4 LENGTH
      LOGICAL QUIET
C
      INTEGER LNBLNK
      LOGICAL SEMCON
C
      INTEGER E,I,J,L
C
      CHARACTER*4 YEAR
      CHARACTER*2 DAY,MON
      CHARACTER*10 MONTH,MONTHS(12)
      CHARACTER*10 DAFORM
      DATA MONTHS /
     +  'January',      'February',     'March',        'April',
     +  'May',          'June',         'July',         'August',
     +  'September',    'October',      'November',     'December'/
C
      CALL NEMSTR(BU,STRING,E,LENGTH)
      J = ICHAR('0')
      IF (E.EQ.8) THEN
         YEAR = STRING(1:4)
         DAY = STRING(7:8)
         MON = STRING(5:6)
         I = ICHAR(MON(2:2)) - J
         IF (MON(1:1) .EQ. '1') I = I + 10
      ELSE IF (E.EQ.10) THEN
         YEAR = STRING(1:4)
         DAY = STRING(9:10)
         MON = STRING(6:7)
         I = ICHAR(MON(2:2)) - J
         IF (MON(1:1) .EQ. '1') I = I + 10
      ELSE
         YEAR = '1900'
         DAY = '01'
         MON = '01'
         I = 0
      ENDIF
      IF (I .LT. 1 .OR. I .GT. 12) THEN
         MONTH = 'Invalid'
      ELSE
         MONTH = MONTHS(I)
      ENDIF
C
      DAFORM = YEAR//':'//MON//':'//DAY
      L = LNBLNK(TITLE)
      I = LNBLNK(MONTH)+1
C
      STRING = TITLE(1:L)//' Date:'//MONTH(1:I)//DAY//', '//YEAR
      IF (QUIET) THEN
         NEMDAT = .FALSE.
      ELSE
         NEMDAT = SEMCON(STRING)
      ENDIF
      STRING = DAFORM
      RETURN
C
C Copyright (C) 1990-1996 Synoptics Ltd,  All Rights Reserved
C
      END
C
      LOGICAL FUNCTION NEMTIM(TITLE,STRING,BU,LENGTH,QUIET)
      CHARACTER*(*) TITLE,STRING
      INTEGER BU(*)
      INTEGER*4 LENGTH
      LOGICAL QUIET
C
      INTEGER LNBLNK
      LOGICAL SEMCON
C
      INTEGER E,P,L
C
      CHARACTER*2 HOUR
      CHARACTER*3 MIN,SEC
      CHARACTER*10 FRAC
      CHARACTER*10 TIFORM
C
      L = LNBLNK(TITLE)
C
      CALL NEMSTR(BU,STRING,E,LENGTH)
C
C Format is hhmmss.frac  or hh:mm:ss:frac
C
      IF (E .GE. 2) THEN
         HOUR = STRING(1:2)
         IF (E .GT. 2 .AND. STRING(3:3) .EQ. ':') THEN
            P = 4
         ELSE
            P = 3
         ENDIF
         IF (E .GE. P+1) THEN
            MIN = ':'//STRING(P:P+1)
            P = P + 2
            IF (E .GT. P .AND. STRING(P:P) .EQ. ':') THEN
               P = P + 1
            ENDIF
            IF (E .GE. P+1) THEN
               SEC = ':'//STRING(P:P+1)
               P = P + 2
               IF (E .GT. P) THEN
                  P = P + 1
                  FRAC = '.'//STRING(P:E)
               ELSE
                  FRAC = ' '
               ENDIF
            ELSE
               SEC = ' '
            ENDIF
         ELSE
            MIN = ' '
         ENDIF
         STRING = TITLE(1:L)//' Time:'//HOUR//MIN//SEC//FRAC
      ELSE
         HOUR = '00'
         MIN = ':00'
         SEC = ':00'
         STRING = TITLE(1:L)//' Time:Invalid'
      ENDIF
C
      TIFORM = HOUR//MIN//SEC
      IF (QUIET) THEN
         NEMTIM = .FALSE.
      ELSE
         NEMTIM = SEMCON(STRING)
      ENDIF
      STRING = TIFORM
      RETURN
C
C Copyright (C) 1990-1996 Synoptics Ltd,  All Rights Reserved
C
      END
