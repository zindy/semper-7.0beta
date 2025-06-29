C Semper 6 processing module INPTIF
C
      SUBROUTINE INPTIF
C
C Provides command INPUT TIFF:
C  Reads pictures from a TIFF file that is
C  dynamically opened.
C
      INTEGER*4 UNSIGN
      INTEGER ASCINT,IPACK,IVALPN,LNBLNK,SEMFRM
      LOGICAL FILSEA,FILSTR,OPTNO,SEMCLS,SEMCON,SEMLAB,SEMOPN
      LOGICAL SEMROW,SEMSEL,VARSET
      LOGICAL MRDBIN,RTIFAS,RTIFI2,RTIFI4,RTSEEK
C
      INTEGER CLASS,FORM
C
      INCLUDE 'COMMON'
C
      CHARACTER*(FILMAX) FILENM,FILE
      CHARACTER*255 DESCRP,STRING,DATIME
      CHARACTER*4 DFNAM
C
      INTEGER FD,I,J,K,N,ND,NF,IXFR
      INTEGER NROW,NLAY,NCOL,NROW2,NLAY2,NCOL2,P
      INTEGER*2 BPSAMP(3),ORIENT,PLANAR,PMI,SAMPPP
      INTEGER*2 STRIPN,ROWLEF,STRIPS
C
      INTEGER*4 STROFF(LNBUF/LNINT4)
C
      INTEGER IB1(LNBUF/LNINT),LABEL(256)
      INTEGER IBU(256)
      INTEGER BLUE(0:LNBUF/LNINT)
      INTEGER GREEN(0:LNBUF/LNINT)
      INTEGER RED(0:LNBUF/LNINT)
C
C Maximum number of tags we will consider
C
      INTEGER*2 MAXTAG
      PARAMETER (MAXTAG=36)
      INTEGER*2 TAGBYT,TAGASC,TAGSHT,TAGLON,TAGRAT
      PARAMETER (TAGBYT=1,TAGASC=2,TAGSHT=3,TAGLON=4,TAGRAT=5)
C
      LOGICAL TAGPTR(MAXTAG)
      INTEGER*2 TAGNUM(MAXTAG),TAGTYP(MAXTAG),NTAG,TAGNO
      INTEGER*4 TAGLEN(MAXTAG),TAGVAL(MAXTAG),TAGIND(MAXTAG)
C
      LOGICAL COLOUR,EXISTS,LMAP,LTABLE,LGREY,LNONE,MOTOR,QUIET,TIMED
C
      EQUIVALENCE (RB1,IB1,LABEL)
      EQUIVALENCE (RB3,STROFF)
      EQUIVALENCE (RB4,BLUE),(RB5,GREEN),(RB6,RED)
C
      INTEGER*4 I4N,POSN,TARGET,IFDPTR,NXTIFD
      INTEGER*2 I2(2),I2WRK
      EQUIVALENCE (I4N,I2)
C
      INTEGER*4 IWIDTH,HEIGHT,ROWSPS

      INTEGER*2 I2WRK_AND_MASK, ITEMP
C
      INTEGER*4 I42,I43
      PARAMETER (I42=2,I43=3)
C
C     'C' functions
C
      LOGICAL EIKOPE,EIKBYA,EIKCLO
C
C     LDM Additions
      CHARACTER*20 ESTRING
      INTEGER SEMPBUILD
      Logical endian, IsMotorola
C     Pick up options and mandatory keys
C
      I2WRK_AND_MASK = 32767

      QUIET = OPTNO(-3419)
      LMAP = VARSET(20856)
      IF (LMAP) THEN
         DO 10 J = 0,255
            BLUE(J) = J
            GREEN(J) = J
            RED(J) = J
   10    CONTINUE
      ENDIF
C
C     Fetch file name from key NAME, prompting if key absent
C
      DFNAM = '.tif'
      CALL INPDEF(DFNAM)
      IF (FILSTR(' ',FILE,NF,.FALSE.)) GOTO 370
      IF (NF .EQ. 0) GOTO 370
C
C     See if file exists on the path if reading
C
      IF (FILSEA(FILE(1:NF),DFNAM,FILENM,EXISTS)) GOTO 370
      IF (EXISTS) THEN
         NF  = LNBLNK(FILENM)
      ELSE
C
C     Error - non-existent file requested for read
C
         ERROR  = 130
         IDMESS = FILE(1:NF)
         GOTO 370
      ENDIF
C
C     Try to open the file
C
      IF (EIKOPE(1,FD,FILENM(1:NF))) GOTO 290
C
C     Copy to description
C
      DESCRP = FILENM(1:NF)
      ND = NF
C
      IXFR = 2
      ITEMP = I
      IF (MRDBIN(FD,IXFR,ITEMP,NFMBYT,.FALSE.)) GOTO 340
      CALL CFORM(I,IB1,NFMBYT,NFMINT,I42)
C
C File should start with 'II' or 'MM'
C
      IF (IB1(1) .NE. IB1(2)) then
         write(6,*)'Unknown Tiff structure',CHAR(IB1(1)),CHAR(IB1(2))
        GOTO 320
        ENDIF

      IF (IB1(1) .EQ. ICHAR('I')) THEN
         MOTOR = .FALSE.
      ELSE IF (IB1(1) .EQ. ICHAR('M')) THEN
         MOTOR = .TRUE.
      ELSE
         write(6,*)'Unknown Tiff structure',CHAR(IB1(1)),CHAR(IB1(2))
         GOTO 320
      ENDIF
C     Check that this agrees with endian character of CPU
      ENDIAN = IsMotorola(I)

      POSN = 2
C     LDM Change
C     MOTOR should be whether we need to do a swap, period
      MOTOR = MOTOR .EQV. ENDIAN
C
C Get version number
C
      IF (RTIFI2(FD,I2WRK,POSN,MOTOR)) GOTO 340
C     LDM Comment: maybe I values should depend on endian character?
      IF ((I2WRK .NE. 42) .and. (I2WRK .NE. 10752)) THEN
        write(6,*)'Unknown Version Number ',I2WRK
        GOTO 320
        ENDIF
C
C Get IFD offset
C
      IF (RTIFI4(FD,IFDPTR,POSN,MOTOR)) GOTO 340
C
C Valid IFD ?
C
   20 IF (IFDPTR .EQ. 0) THEN
         IDMESS = 'No Full Resolution images in TIFF file'
         GOTO 350
      ENDIF
      IF (RTSEEK(FD,IFDPTR,POSN,FILENM(1:NF))) GOTO 350
C
C Read TAG count
C
      IF (RTIFI2(FD,NTAG,POSN,MOTOR)) GOTO 340
C
C Read all tags first
C
      DO 30 I = 1,NTAG
         IF (RTIFI2(FD,TAGNUM(I),POSN,MOTOR)) GOTO 340
         IF (RTIFI2(FD,TAGTYP(I),POSN,MOTOR)) GOTO 340
         IF (RTIFI4(FD,TAGLEN(I),POSN,MOTOR)) GOTO 340
C
         TAGPTR(I) = .FALSE.
C
C Does the value part fit in the pointer ??
C
         IF ((TAGTYP(I) .EQ. TAGBYT .AND. TAGLEN(I) .LE. 4) .OR.
     +       (TAGTYP(I) .EQ. TAGASC .AND. TAGLEN(I) .LE. 4)) THEN
            IF (EIKBYA(1,FD,TAGVAL(I),4)) GOTO 340
            POSN = POSN + 4
         ELSE IF (TAGTYP(I) .EQ. TAGSHT .AND. TAGLEN(I) .LE. 2) THEN
C
            IF (RTIFI2(FD,I2(1),POSN,MOTOR)) GOTO 340
            IF (RTIFI2(FD,I2(2),POSN,MOTOR)) GOTO 340
            IF (TAGLEN(I) .EQ. 1) THEN
               I2(2) = 0
            ENDIF
            TAGVAL(I) = I4N
C
         ELSE IF (TAGTYP(I) .EQ. TAGLON .AND. TAGLEN(I) .EQ. 1) THEN
            IF (RTIFI4(FD,TAGVAL(I),POSN,MOTOR)) GOTO 340
         ELSE
            IF (RTIFI4(FD,TAGIND(I),POSN,MOTOR)) GOTO 340
            TAGPTR(I) = .TRUE.
         ENDIF
C
C Helper - for bad TIFF files! - set pointer even if not valid!
C
         IF (.NOT.TAGPTR(I)) TAGIND(I) = TAGVAL(I)
   30 CONTINUE
C
C Remember next IFD in case this one is dodgy!
C
      IF (RTIFI4(FD,NXTIFD,POSN,MOTOR)) GOTO 340
C
C Set defaults - required first, then defaultables
C
      IWIDTH = -1
      HEIGHT = -1
      ROWSPS = -1
      PMI = -1
C
      BPSAMP(1) = 1
      BPSAMP(2) = 1
      BPSAMP(3) = 1
      PLANAR = 1
      SAMPPP = 1
      ORIENT = 1
C
      LTABLE = .FALSE.
      TIMED = .FALSE.
C
C Now read the tags - they should be in order, but we can
C treat them otherwise!
C
      TAGNO = 0
C
   40 TAGNO = TAGNO + 1
   50 IF (TAGNO .GT. NTAG) GOTO 220
C
C NewSubfileType ?
C
      IF (TAGNUM(TAGNO) .EQ. 254) THEN
         IF (TAGVAL(TAGNO) .NE. 0) THEN
C
C Not a full resolution image - is there another IFD?
C
            IF (NXTIFD .NE. 0) THEN
               IFDPTR = NXTIFD
               GOTO 20
            ENDIF
         ENDIF
         GOTO 40
      ENDIF
C
C SubfileType ?
C
      IF (TAGNUM(TAGNO) .EQ. 255) THEN
         I4N = TAGVAL(TAGNO)
         IF (I2(1) .NE. 1) THEN
C
C Not a full resolution image - is there another IFD?
C
            IF (NXTIFD .NE. 0) THEN
               IFDPTR = NXTIFD
               GOTO 20
            ENDIF
         ENDIF
         GOTO 40
      ENDIF
C
C ImageWidth ?
C
      IF (TAGNUM(TAGNO) .EQ. 256) THEN
         IF (TAGTYP(TAGNO) .EQ. TAGLON) THEN
            IWIDTH = TAGVAL(TAGNO)
         ELSE
            I4N = TAGVAL(TAGNO)
            IWIDTH = I2(1)
         ENDIF
         GOTO 40
      ENDIF
C
C ImageLength ?
C
      IF (TAGNUM(TAGNO) .EQ. 257) THEN
         IF (TAGTYP(TAGNO) .EQ. TAGLON) THEN
            HEIGHT = TAGVAL(TAGNO)
         ELSE
            I4N = TAGVAL(TAGNO)
            HEIGHT = I2(1)
         ENDIF
         GOTO 40
      ENDIF
C
C BitsPerSample ?
C
      IF (TAGNUM(TAGNO) .EQ. 258) THEN
         IF (TAGLEN(TAGNO) .LE. 2) THEN
            I4N = TAGVAL(TAGNO)
            BPSAMP(1) = I2(1)
            IF (TAGLEN(TAGNO) .EQ. 2) THEN
               BPSAMP(2) = I2(2)
            ELSE
               BPSAMP(2) = BPSAMP(1)
            ENDIF
            BPSAMP(3) = BPSAMP(2)
         ELSE
            IF (TAGLEN(TAGNO) .GT. 3) THEN
               IDMESS = 'Too many BitsPerSample fields'
               GOTO 350
            ENDIF
C
C Seek to TAG and read
C
            IF (RTSEEK(FD,TAGIND(TAGNO),POSN,FILENM(1:NF))) GOTO 350
            DO 60 I = 1,TAGLEN(TAGNO)
               IF (RTIFI2(FD,BPSAMP(I),POSN,MOTOR)) GOTO 340
   60       CONTINUE
         ENDIF
C
C Any sample depths not byte wide ?
C
         IF (BPSAMP(1) .NE. 8 .OR. BPSAMP(2) .NE. 8 .OR.
     +       BPSAMP(3) .NE. 8) THEN
            IDMESS = 'Unsupported BitsPerSample Value'
            GOTO 350
         ENDIF
         GOTO 40
      ENDIF
C
C Compression ?
C
      IF (TAGNUM(TAGNO) .EQ. 259) THEN
         I4N = TAGVAL(TAGNO)
         IF (I2(1) .EQ. 2) THEN
            IDMESS = 'CCITT compression not supported'
            GOTO 350
         ELSE IF (I2(1) .EQ. 5) THEN
            IDMESS = 'LZW compression not supported'
            GOTO 350
         ELSE
            IF (UNSIGN(I2(1)) .EQ. 32773) THEN
               IDMESS = 'PackBits compression not supported'
               GOTO 350
            ENDIF
         ENDIF
         IF (I2(1) .NE. 1) THEN
            WRITE(IDMESS,70) I2(1)
   70       FORMAT('Unknown Compression value',I6)
            GOTO 350
         ENDIF
         GOTO 40
      ENDIF
C
C PhotoMetric Interpretation ?
C
      IF (TAGNUM(TAGNO) .EQ. 262) THEN
         I4N = TAGVAL(TAGNO)
         PMI = I2(1)
         IF (PMI .LT. 0 .OR. PMI .GT. 5) THEN
            WRITE(IDMESS,80) PMI
   80       FORMAT('Unknown PhotoMetricInterpretation value ',I6)
            GOTO 350
         ENDIF
         GOTO 40
      ENDIF
C
      IF (TAGNUM(TAGNO) .EQ. 263) THEN
         GOTO 40
      ENDIF
C
C CellWidth
C
      IF (TAGNUM(TAGNO) .EQ. 264) THEN
         GOTO 40
      ENDIF
C
C CellLength
C
      IF (TAGNUM(TAGNO) .EQ. 265) THEN
         GOTO 40
      ENDIF
C
C FillOrder
C
      IF (TAGNUM(TAGNO) .EQ. 266) THEN
         I4N = TAGVAL(TAGNO)
C
C Only msbits supported at present
C
         IF (I2(1) .NE. 1) THEN
            WRITE(IDMESS,90) I2(1)
   90       FORMAT('Unsupported FillOrder value',I6)
            GOTO 350
         ENDIF
         GOTO 40
      ENDIF
C
C DocumentName
C
      IF (TAGNUM(TAGNO) .EQ. 269) THEN
         GOTO 40
      ENDIF
C
C Image Description - copy string to description
C
      IF (TAGNUM(TAGNO) .EQ. 270) THEN
         IF (RTIFAS(FD,POSN,FILENM(1:NF),IBU,
     +              TAGIND(TAGNO),TAGLEN(TAGNO),TAGVAL(TAGNO))) GOTO 350
         CALL TIFSTR(IBU,DESCRP,ND,J)
         GOTO 40
      ENDIF
C
C Make/manufacturer (of source)
C
      IF (TAGNUM(TAGNO) .EQ. 271) THEN
         IF (RTIFAS(FD,POSN,FILENM(1:NF),IBU,
     +              TAGIND(TAGNO),TAGLEN(TAGNO),TAGVAL(TAGNO))) GOTO 350
         J = TAGLEN(TAGNO)
         CALL TIFSTR(IBU,STRING,I,J)
         WRITE(IDMESS,150) ' Manufacturer',STRING(1:I)
         IF (.NOT.QUIET) THEN
            IF (SEMCON(IDMESS)) GOTO 360
         ENDIF
         GOTO 40
      ENDIF
C
C Model (of source)
C
      IF (TAGNUM(TAGNO) .EQ. 272) THEN
         IF (RTIFAS(FD,POSN,FILENM(1:NF),IBU,
     +              TAGIND(TAGNO),TAGLEN(TAGNO),TAGVAL(TAGNO))) GOTO 350
         J = TAGLEN(TAGNO)
         CALL TIFSTR(IBU,STRING,I,J)
         WRITE(IDMESS,150) '   Model name',STRING(1:I)
         IF (.NOT.QUIET) THEN
            IF (SEMCON(IDMESS)) GOTO 360
         ENDIF
         GOTO 40
      ENDIF
C
C StripOffsets
C
      IF (TAGNUM(TAGNO) .EQ. 273) THEN
         IF (TAGPTR(TAGNO)) THEN
C
C Seek to TAG and read
C
            IF (RTSEEK(FD,TAGIND(TAGNO),POSN,FILENM(1:NF))) GOTO 350
            DO 100 I = 1,TAGLEN(TAGNO)
               IF (TAGTYP(TAGNO) .EQ. TAGLON) THEN
                  IF (RTIFI4(FD,STROFF(I),POSN,MOTOR)) GOTO 340
               ELSE
                  IF (RTIFI2(FD,I2WRK,POSN,MOTOR)) GOTO 340
                  ITEMP=I2WRK
                  STROFF(I) = UNSIGN(ITEMP)
               ENDIF
  100       CONTINUE
         ELSE
            IF (TAGTYP(TAGNO) .EQ. TAGLON) THEN
               STROFF(1) = TAGVAL(TAGNO)
            ELSE
               I4N = TAGVAL(TAGNO)
               STROFF(1) = UNSIGN(I2(1))
               IF (TAGLEN(TAGNO) .EQ. 2) STROFF(2) = UNSIGN(I2(2))
            ENDIF
         ENDIF
         GOTO 40
      ENDIF
C
C Orientation
C
      IF (TAGNUM(TAGNO) .EQ. 274) THEN
         I4N = TAGVAL(TAGNO)
         ORIENT = I2(1)
         IF (ORIENT .NE. 4) THEN
            IF (ORIENT .NE. 1) THEN
               WRITE(IDMESS,110) ORIENT
  110          FORMAT('Unknown Orientation value ',I6,' - ignored')
               IF (.NOT.QUIET) THEN
                  IF (SEMCON(IDMESS)) GOTO 360
               ENDIF
               ORIENT = 1
            ENDIF
         ENDIF
         GOTO 40
      ENDIF
C
C SamplesPerPixel ?
C
      IF (TAGNUM(TAGNO) .EQ. 277) THEN
         I4N = TAGVAL(TAGNO)
         SAMPPP = I2(1)
         IF (SAMPPP .NE. 1 .AND. SAMPPP .NE. 3) THEN
            WRITE(IDMESS,120) SAMPPP
  120       FORMAT('Unexpected SamplesPerPixel value ',I6)
            GOTO 350
         ENDIF
         GOTO 40
      ENDIF
C
C RowsPerStrip ?
C
      IF (TAGNUM(TAGNO) .EQ. 278) THEN
         IF (TAGTYP(TAGNO) .EQ. TAGLON) THEN
            ROWSPS = TAGVAL(TAGNO)
         ELSE
            I4N = TAGVAL(TAGNO)
            ROWSPS = I2(1)
         ENDIF
         GOTO 40
      ENDIF
C
C SampleByteCounts ?
C
      IF (TAGNUM(TAGNO) .EQ. 279) THEN
         GOTO 40
      ENDIF
C
C MinSampleValue - could use to set range?
C
      IF (TAGNUM(TAGNO) .EQ. 280) THEN
         WRITE(IDMESS,130) 'MinSampleValues'
  130    FORMAT('Ignoring TIFF TAG - ',A)
         IF (.NOT.QUIET) THEN
            IF (SEMCON(IDMESS)) GOTO 360
         ENDIF
         GOTO 40
      ENDIF
C
C MaxSampleValue - could use to set range?
C
      IF (TAGNUM(TAGNO) .EQ. 281) THEN
         WRITE(IDMESS,130) 'MaxSampleValues'
         IF (.NOT.QUIET) THEN
            IF (SEMCON(IDMESS)) GOTO 360
         ENDIF
         GOTO 40
      ENDIF
C
C XResolution
C
      IF (TAGNUM(TAGNO) .EQ. 282) THEN
         GOTO 40
      ENDIF
C
C YResolution
C
      IF (TAGNUM(TAGNO) .EQ. 283) THEN
         GOTO 40
      ENDIF
C
C PlanarConfiguration ?
C
      IF (TAGNUM(TAGNO) .EQ. 284) THEN
         I4N = TAGVAL(TAGNO)
         PLANAR = I2(1)
         IF (PLANAR .NE. 1 .AND. PLANAR .NE. 2) THEN
            WRITE(IDMESS,140) PLANAR
  140       FORMAT('Unexpected PlanarConfiguration value ',I6)
            GOTO 350
         ENDIF
         GOTO 40
      ENDIF
C
C PageName
C
      IF (TAGNUM(TAGNO) .EQ. 285) THEN
         GOTO 40
      ENDIF
C
C XPosition
C
      IF (TAGNUM(TAGNO) .EQ. 286) THEN
         GOTO 40
      ENDIF
C
C YPosition
C
      IF (TAGNUM(TAGNO) .EQ. 287) THEN
         GOTO 40
      ENDIF
C
C FreeOffsets
C
      IF (TAGNUM(TAGNO) .EQ. 288) THEN
         GOTO 40
      ENDIF
C
C FreeByteCounts
C
      IF (TAGNUM(TAGNO) .EQ. 289) THEN
         GOTO 40
      ENDIF
C
C GrayResponseUnit - ignore for now
C
      IF (TAGNUM(TAGNO) .EQ. 290) THEN
         GOTO 40
      ENDIF
C
C GrayResponseCurve - ignore for now
C
      IF (TAGNUM(TAGNO) .EQ. 291) THEN
         WRITE(IDMESS,130) 'GreyResponseCurve'
         IF (.NOT.QUIET) THEN
            IF (SEMCON(IDMESS)) GOTO 360
         ENDIF
         GOTO 40
      ENDIF
C
C Group3Options
C
      IF (TAGNUM(TAGNO) .EQ. 292) THEN
         GOTO 40
      ENDIF
C
C Group4Options
C
      IF (TAGNUM(TAGNO) .EQ. 293) THEN
         GOTO 40
      ENDIF
C
C ResolutionUnit
C
      IF (TAGNUM(TAGNO) .EQ. 296) THEN
         GOTO 40
      ENDIF
C
C PageNumber
C
      IF (TAGNUM(TAGNO) .EQ. 297) THEN
         GOTO 40
      ENDIF
C
C ColorResponseCurves - ignore for now
C
      IF (TAGNUM(TAGNO) .EQ. 301) THEN
         WRITE(IDMESS,130) 'ColorResponseCurves'
         IF (.NOT.QUIET) THEN
            IF (SEMCON(IDMESS)) GOTO 360
         ENDIF
         GOTO 40
      ENDIF
C
C Software
C
      IF (TAGNUM(TAGNO) .EQ. 305) THEN
         IF (RTIFAS(FD,POSN,FILENM(1:NF),IBU,
     +              TAGIND(TAGNO),TAGLEN(TAGNO),TAGVAL(TAGNO))) GOTO 350
         J = TAGLEN(TAGNO)
         CALL TIFSTR(IBU,STRING,I,J)
         WRITE(IDMESS,150) '     Software',STRING(1:I)
  150    FORMAT(A,':',A)
         IF (.NOT.QUIET) THEN
            IF (SEMCON(IDMESS)) GOTO 360
         ENDIF
         GOTO 40
      ENDIF
C
C DateTime ?
C
      IF (TAGNUM(TAGNO) .EQ. 306) THEN
         IF (RTIFAS(FD,POSN,FILENM(1:NF),IBU,
     +              TAGIND(TAGNO),TAGLEN(TAGNO),TAGVAL(TAGNO))) GOTO 350
         J = TAGLEN(TAGNO)
         CALL TIFSTR(IBU,DATIME,I,J)
C
C Format should be YYYY:MM:DD HH:MM:SS
C Check length first
C
         IF (I .LT. 19) GOTO 40
C
C Check colons in place!
C
         IF (DATIME(5:5) .EQ. ':' .AND. DATIME(8:8) .EQ. ':' .AND.
     +       DATIME(14:14) .EQ. ':' .AND. DATIME(17:17) .EQ. ':') THEN
C
C Validate numeric characters
C
            DO 170 I = 1,19
               IF (I .NE. 5 .AND. I .NE. 8 .AND. I .NE. 14 .AND.
     +             I .NE. 17) THEN
                  IF (INDEX('0123456789 ',DATIME(I:I)) .EQ. 0) THEN
                     WRITE(IDMESS,160) DATIME(1:19)
  160                FORMAT('Invalid DateTime format - ',A)
                     GOTO 350
                  ENDIF
               ENDIF
  170       CONTINUE
            TIMED = .TRUE.
         ENDIF
         GOTO 40
      ENDIF
C
C Artist
C
      IF (TAGNUM(TAGNO) .EQ. 315) THEN
         IF (RTIFAS(FD,POSN,FILENM(1:NF),IBU,
     +              TAGIND(TAGNO),TAGLEN(TAGNO),TAGVAL(TAGNO))) GOTO 350
         J = TAGLEN(TAGNO)
         CALL TIFSTR(IBU,STRING,I,J)
         WRITE(IDMESS,150) ' Image Artist',STRING(1:I)
         IF (.NOT.QUIET) THEN
            IF (SEMCON(IDMESS)) GOTO 360
         ENDIF
         GOTO 40
      ENDIF
C
C HostComputer
C
      IF (TAGNUM(TAGNO) .EQ. 316) THEN
         IF (RTIFAS(FD,POSN,FILENM(1:NF),IBU,
     +              TAGIND(TAGNO),TAGLEN(TAGNO),TAGVAL(TAGNO))) GOTO 350
         J = TAGLEN(TAGNO)
         CALL TIFSTR(IBU,STRING,I,J)
         WRITE(IDMESS,150) 'Host Computer',STRING(1:I)
         IF (.NOT.QUIET) THEN
            IF (SEMCON(IDMESS)) GOTO 360
         ENDIF
         GOTO 40
      ENDIF
C
C Predictor
C
      IF (TAGNUM(TAGNO) .EQ. 317) THEN
         GOTO 40
      ENDIF
C
C WhitePoint
C
      IF (TAGNUM(TAGNO) .EQ. 318) THEN
         GOTO 40
      ENDIF
C
C PrimaryChromaticities
C
      IF (TAGNUM(TAGNO) .EQ. 319) THEN
         GOTO 40
      ENDIF
C
C ColorMap
C
      IF (TAGNUM(TAGNO) .EQ. 320) THEN
         IF (LMAP) THEN
C
C Seek to maps and read - since we only handle 8 bpp
C                         assume 2**8 = 256 entries
C
            IF (RTSEEK(FD,TAGIND(TAGNO),POSN,FILENM(1:NF))) GOTO 350
            DO 180 I = 0,255
               IF (RTIFI2(FD,I2WRK,POSN,MOTOR)) GOTO 340
C               K = IAND(RSHIFT(I2WRK,8),32767)
               K = IAND(ISHFT(I2WRK,-8),I2WRK_AND_MASK)
               RED(I) = K
  180       CONTINUE
            DO 190 I = 0,255
               IF (RTIFI2(FD,I2WRK,POSN,MOTOR)) GOTO 340
C               K = IAND(RSHIFT(I2WRK,8),32767)
               K = IAND(ISHFT(I2WRK,-8),I2WRK_AND_MASK)
               GREEN(I) = K
  190       CONTINUE
            DO 200 I = 0,255
               IF (RTIFI2(FD,I2WRK,POSN,MOTOR)) GOTO 340
C               K = IAND(RSHIFT(I2WRK,8),32767)
               K = IAND(ISHFT(I2WRK,-8),I2WRK_AND_MASK)
               BLUE(I) = K
  200       CONTINUE
         ENDIF
         LTABLE = .TRUE.
         GOTO 40
      ENDIF
C
C Unrecognised TAG
C
      I4N = UNSIGN(TAGNUM(TAGNO))
      WRITE(IDMESS,210) I4N
  210 FORMAT('Unrecognised TIFF TAG ',I6,' - ignored')
      IF (.NOT.QUIET) THEN
         IF (SEMCON(IDMESS)) GOTO 360
      ENDIF
      TAGNO = TAGNO + 1
      GOTO 50
C
C End of TAGS  - are we missing any we need?
C
  220 CONTINUE
C
C Check fields
C
      IF (IWIDTH .EQ. -1 .OR. HEIGHT .EQ. -1) THEN
         IDMESS = 'Missing image dimensions'
         GOTO 350
      ENDIF
C
      IF (PMI .EQ. -1) THEN
         IF (SEMCON(
     +       'Missing PhotometricInterpretation field - ignored')
     +       ) GOTO 360
         PMI = 1
      ENDIF
C
      IF ((PMI .EQ. 2 .AND. SAMPPP .NE. 3) .OR.
     +    (PMI .EQ. 3 .AND. SAMPPP .NE. 1)) THEN
         IDMESS =
     +     'Inconsistent SamplesPerPixel and PhotometricInterpretation'
         GOTO 350
      ENDIF
C
      IF (PLANAR .EQ. 2 .AND. SAMPPP .NE. 3) THEN
         IDMESS = 'Inconsistent SamplesPerPixel and PlanarConfiguration'
         GOTO 350
      ENDIF
C
      NCOL = IWIDTH
      NROW = HEIGHT
      NLAY = SAMPPP
      IF (NLAY .EQ. 1) THEN
         COLOUR = .FALSE.
      ELSE
         COLOUR = .TRUE.
      ENDIF
C
C     Calculate number of strips
C
      IF (ROWSPS .EQ. -1) THEN
         ROWSPS = NROW
         STRIPS = 1
         IF (COLOUR .AND. PLANAR .EQ. 2) THEN
            ROWSPS = ROWSPS * SAMPPP
         ENDIF
      ELSE
         I4N = NROW
         I4N = I4N + (ROWSPS - 1)
         I4N = I4N / ROWSPS
         IF (COLOUR .AND. PLANAR .EQ. 2) I4N = I4N * SAMPPP
         STRIPS = I4N
      ENDIF
C
      LGREY = .TRUE.
      LNONE = .TRUE.
C
      IF (LMAP .AND. LTABLE) THEN
C
C Colour map now in RED,GREEN,BLUE
C   Could write now if MAP given - but should first check for
C   greyscale (red=green=blue) and ascending (lut(n)=n)
C
         DO 230 J = 0,255
            IF (BLUE(J) .NE. GREEN(J) .OR. BLUE(J) .NE. RED(J)) THEN
               LGREY = .FALSE.
               GOTO 240
            ENDIF
            IF (BLUE(J) .NE. J) LNONE = .FALSE.
  230    CONTINUE
C
  240    CONTINUE
C
C Store away the LUT
C
         IF ( LGREY ) THEN
            NROW2 = 1
         ELSE
            NROW2 = 3
         ENDIF
         NCOL2 = 256
         NLAY2 = 1
         IF (LUTLEN .EQ. 256) THEN
            CLASS = NCLLUT
         ELSE
            CLASS = NCLIMA
         ENDIF
         FORM = NFMINT
C
C Open picture
C
         LP2 = 0
         IF (SEMOPN(2,IVALPN(20856),
     +              NCOL2,NROW2,NLAY2,CLASS,FORM,LP2)) GOTO 360
C
C Form title from description + (map)
C
         N = MIN(ND+5,LBTT2 - LBTT1)
         LABEL(LBNCTT) = N
         FILE = DESCRP(1:ND)//'(map)'
         CALL SEMICS(FILE,LABEL(LBTT1),N)
C
C Update label if possible
C
         IF (N.NE.0 .AND. LBLINC) THEN
            IF (SEMLAB(2,LABEL,LP2)) GOTO 360
         ENDIF
C
C Now copy data
C
         K = 1
         IF (SEMROW(2,RED,NFMINT,1,1,LP2)) GOTO 360
         IF (NROW2 .EQ. 3) THEN
            IF (SEMROW(2,GREEN,NFMINT,2,1,LP2)) GOTO 360
            IF (SEMROW(2,BLUE,NFMINT,3,1,LP2)) GOTO 360
         ENDIF
C
C and close the image
C
         IF (SEMCLS(LP2)) GOTO 360
      ENDIF
C
      FORM = SEMFRM(NFMBYT)
      CLASS = NCLIMA
C
C     Open picture
C
      LP1 = 0
      IF (SEMOPN(2,IVALPN(-601),NCOL,NROW,NLAY,CLASS,
     +           FORM,LP1)) GOTO 360
C
C     Form title from description
C
      N = MIN(ND,LBTT2 - LBTT1)
      LABEL(LBNCTT) = N
      CALL SEMICS(DESCRP,LABEL(LBTT1),N)
C
C     Date and Time supplied ?
C
      IF (TIMED) THEN
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
         IF (SEMLAB(2,LABEL,LP1)) GOTO 360
      ENDIF
C
      IXFR = NCOL
      IF (COLOUR .AND. PLANAR .EQ. 1) IXFR = IXFR * 3
      I4N = IXFR
C
C     Initialise strip pointers
C
      STRIPN = 0
      ROWLEF = 0
C
      IF (COLOUR .AND. PLANAR .EQ. 1) NLAY = 1
      DO 280 N = 1,NLAY
C
C        Loop over rows
C
         IF (ORIENT .EQ. 1) THEN
            I = 1
         ELSE
            I = NROW
         ENDIF
C
         DO 270 J = 1,NROW
            IF (COLOUR .AND. PLANAR .EQ. 1) THEN
               IF (ROWLEF .EQ. 0) THEN
                  IF (STRIPN .EQ. STRIPS) GOTO 330
                  STRIPN = STRIPN + 1
                  ROWLEF = ROWSPS
                  TARGET = STROFF(STRIPN)
                  IF (RTSEEK(FD,TARGET,POSN,FILENM(1:NF))) GOTO 350
               ENDIF
C
C     Read source row from file
C
               IF (EIKBYA(1,FD,IB1,IXFR)) GOTO 340
               ROWLEF = ROWLEF - 1
               POSN = POSN + I4N
               CALL CFORM(IB1(1),IB1,NFMBYT,NFMINT,I4N)
C
C     Unpack the pixels
C
               P = 1
               DO 250 K = 0,NCOL-1
                  RED(K) = IB1(P)
                  GREEN(K) = IB1(P+1)
                  BLUE(K) = IB1(P+2)
                  P = P + 3
  250          CONTINUE
C
C     Store the rows to the destination in LP1
C
               IF (SEMROW(2,RED,NFMINT,I,1,LP1)) GOTO 360
               IF (SEMROW(2,GREEN,NFMINT,I,2,LP1)) GOTO 360
               IF (SEMROW(2,BLUE,NFMINT,I,3,LP1)) GOTO 360
            ELSE
C
C     Read source row from file
C
               IF (ROWLEF .EQ. 0) THEN
                  IF (STRIPN .EQ. STRIPS) GOTO 330
                  STRIPN = STRIPN + 1
                  ROWLEF = ROWSPS
                  TARGET = STROFF(STRIPN)
                  IF (RTSEEK(FD,TARGET,POSN,FILENM(1:NF))) GOTO 350
               ENDIF
               IF (EIKBYA(1,FD,IB1,IXFR)) GOTO 340
               ROWLEF = ROWLEF - 1
               POSN = POSN + I4N
               IF (PMI .EQ. 0) THEN
C
C     Need to reverse order of black to white
C
                  CALL CFORM(IB1(1),IB1,NFMBYT,NFMINT,I4N)
                  DO 260 K = 1,IXFR
                     IB1(K) = 255 - IB1(K)
  260             CONTINUE
                  IF (SEMROW(2,IB1,NFMINT,I,N,LP1)) GOTO 360
               ELSE
C
C     Store source row in LP1
C
                  IF (SEMROW(2,IB1,NFMBYT,I,N,LP1)) GOTO 360
               ENDIF
            ENDIF
            IF (ORIENT .EQ. 1) THEN
               I = I + 1
            ELSE
               I = I - 1
            ENDIF
  270    CONTINUE
         IF (PLANAR .NE. 2) ROWLEF = 0
  280 CONTINUE
C
C     Close file
C
      IF ( EIKCLO ( FD ) ) GOTO 300
C
C     Finally, select this image
C
      IF (SEMSEL(LP1)) GOTO 370
      GOTO 370
C
C     Deal with UNIX I/O errors
C
  290 IDMESS = 'Error opening file '//FILENM(1:NF)
      GOTO 310
C
  300 IDMESS = 'Error closing file '//FILENM(1:NF)
  310 ERROR = 77
      GOTO 370
C
  320 IDMESS = 'Unrecognised or unsupported file format'
      GOTO 350
C
  330 IDMESS = 'Insufficient data strips in file'
      GOTO 350
C
  340 IDMESS = 'Error reading file: Tiff Input '//FILENM(1:NF)
      GOTO 350
C
C     Closing a file after an error (ignore any error on closing file)
C
  350 ERROR = 77
  360 IF ( EIKCLO ( FD ) ) GOTO 370
C
C     All done
C
  370 RETURN
C
C Copyright (C) 1990-1996 Synoptics Ltd,  All Rights Reserved
C
      END
C
      LOGICAL FUNCTION RTIFI2(FD,I,POSN,MOTOR)
      INTEGER FD
      INTEGER*2 I
      INTEGER*4 POSN
      LOGICAL MOTOR
C
      INCLUDE 'PARAMS'
C
      INTEGER N
C
      LOGICAL MRDBIN
C
      POSN = POSN + 2
      N = 2
      
      RTIFI2 = MRDBIN(FD,N,I,NFMINT,MOTOR)
      RETURN
      END
C
      LOGICAL FUNCTION RTIFI4(FD,I,POSN,MOTOR)
      INTEGER FD
      INTEGER*4 I, POSN
      INTEGER*2 ITEMP
      LOGICAL MOTOR
C
      INCLUDE 'PARAMS'
C
      INTEGER N
C
      LOGICAL MRDBIN
C
      POSN = POSN + 4
      N = 4
C
C Use NFMFP as there isn't an NFMI4
C
      ITEMP = I
      RTIFI4 = MRDBIN(FD,N,ITEMP,NFMFP,MOTOR)
      RETURN
      END
C
C TIFF seek function
C
      LOGICAL FUNCTION RTSEEK(FD,TARGET,POSN,FILENM)
      INTEGER FD
      INTEGER*4 TARGET,POSN
      CHARACTER*(*) FILENM
C
      INTEGER*4 DIFF,I4N
      INTEGER BUFFER(0:255),IXFR
C
      LOGICAL EIKBYA,EIKCLO,EIKOPE
C
      INCLUDE 'COMMON'
C
      RTSEEK = .TRUE.
C
C     Where are we?
C
      DIFF = TARGET - POSN
C
      IF (DIFF .LT. 0) THEN
C
C        Need to rewind and restart file
C
         IF (EIKCLO(FD)) GOTO 40
C
C        Try to reopen the file
C
         IF (EIKOPE(1,FD,FILENM)) GOTO 30
         POSN = 0
         DIFF = TARGET
      ENDIF
C
C     Skip forward in appropriate sized chunks if required
C
   10 IF (DIFF .GT. 0) THEN
         IF (DIFF .GT. 512) THEN
C
C           More than one block to read - align 512 to assist
C
            I4N = POSN / 512
            I4N = I4N * 512
            I4N = POSN - I4N
            I4N = 512 - I4N
            IXFR = (I4N)
         ELSE
            IXFR = DIFF
         ENDIF
         IF (EIKBYA(1,FD,BUFFER,IXFR)) GOTO 50
         DIFF = DIFF - IXFR
         POSN = POSN + IXFR
         GOTO 10
      ENDIF
C
C     Now at the right point in the file
C
      RTSEEK = .FALSE.
   20 RETURN
C
   30 IDMESS = 'Error re-opening file: Tiff Seek '//FILENM
      GOTO 20
   40 IDMESS = 'Error closing file: Tiff Seek '//FILENM
      GOTO 20
   50 IDMESS = 'Error reading file: Tiff Seek '//FILENM
      GOTO 20
      END
C
C Read String
C
      SUBROUTINE TIFSTR(IB,STRING,LENS,N)
      INTEGER IB(*),LENS,N
      CHARACTER*(*) STRING
C
      INTEGER I,K
C
      K = N
      IF (K .GT. 256) K = 256
C
      DO 10 I = 1,K
         IF (IB(I) .EQ. 0) GOTO 20
         IF (IB(I) .LT. 32) IB(I) = ICHAR('?')
         STRING(I:I) = CHAR(IB(I))
         LENS = I
   10 CONTINUE
   20 CONTINUE
      RETURN
      END
C
C TIFF ASCII TAG read
C
      LOGICAL FUNCTION RTIFAS(FD,POSN,FILENM,IB,TAGIND,TAGLEN,TAGVAL)
      INTEGER FD,IB(*)
      INTEGER*4 POSN,TAGIND,TAGLEN,TAGVAL
      CHARACTER*(*) FILENM
C
      INTEGER J,C
      INTEGER CBUF(1)

      INTEGER*4 I41,I4N
      PARAMETER (I41=1)
C
      EQUIVALENCE(C, CBUF)
      LOGICAL EIKBYA,RTSEEK
C
      INCLUDE 'COMMON'
C
      J = TAGLEN
      I4N = J
      IF (J .LE. 4) THEN
C
C Characters already in VAL field - unless badly packed!
C
         CALL CFORM(TAGVAL,IB,NFMBYT,NFMINT,I4N)
         IF (J .EQ. 1 .AND. IB(J) .NE. 0) THEN
            IF (RTSEEK(FD,TAGVAL,POSN,FILENM)) GOTO 30
C
C Read the bytes (up to null pad)
C
   10       IF (EIKBYA(1,FD,CBUF,1)) GOTO 20
            POSN = POSN + I41
            CALL CFORM(C,IB(J),NFMBYT,NFMINT,I41)
            IF (IB(J) .NE. 0) THEN
               J = J + 1
               GOTO 10
            ENDIF
            TAGLEN = J
         ELSE
            IB(J+1) = 0
         ENDIF
      ELSE
         IF (RTSEEK(FD,TAGIND,POSN,FILENM)) GOTO 30
C
C Read the bytes (will be null padded)
C
         IF (EIKBYA(1,FD,IB,J)) GOTO 20
         POSN = POSN + I4N
         CALL CFORM(IB(1),IB,NFMBYT,NFMINT,I4N)
      ENDIF
      RTIFAS = .FALSE.
      RETURN
C
   20 IDMESS = 'Error reading file '//FILENM
   30 RTIFAS = .TRUE.
      RETURN
      END
C
C Convert I2 to I4 unsigned
C
      INTEGER*4 FUNCTION UNSIGN(J)
      INTEGER*2 J
C
      INTEGER*4 I4N
      INTEGER*2 I2WRK_AND_MASK
      I2WRK_AND_MASK = 32767
C
      IF (J .LT. 0) THEN
         I4N = IAND(J,I2WRK_AND_MASK)
         I4N = I4N + 32768
         UNSIGN = I4N
      ELSE
         UNSIGN = J
      ENDIF
      RETURN
      END
C
C Convert string to int
C
C
      INTEGER FUNCTION ASCINT(STRING)
      CHARACTER*(*) STRING
C
      INTEGER L,I,N,R
C
      R = 0
      L = LEN(STRING)
      DO 10 I = 1,L
         N = INDEX('0123456789',STRING(I:I))
         IF (N .GT. 0) N = N  - 1
         R = R * 10
         R = R + N
   10 CONTINUE
C
      ASCINT = R
      RETURN
      END
