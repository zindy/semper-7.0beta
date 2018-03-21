C Semper 6 processing module OUTTIF
C
      SUBROUTINE OUTTIF(LNEW)
      LOGICAL LNEW
C
C Provides commands OUTPUT TIFF:
C  Writes pictures to a sequential binary TIFF file.
C  Optionally takes a colour map (LUT) using the MAP keyword.
C
      INTEGER IPACK,IVALPN,LNBLNK
      LOGICAL FILMAK,FILSEA,FILSTR,OPT,OUTNEW,SEMLNF
      LOGICAL SEMOPN,SEMROW,VARSET
C
      INCLUDE 'COMMON'
C
      INTEGER CLASS,FORM
      INTEGER DOFF,FD,I,IXFR,J,K,LPN,N,NF,PLANAR,T,T2
      INTEGER NROW,NLAY,NCOL,NFIT,NTAG
      INTEGER POKE(2),TIMDAY(7)
C
      INTEGER*4 I4BPIM,I4NEXT,I4N,I40,I41,I42,I43
      PARAMETER (I40=0,I41=1,I42=2,I43=3)
C
      INTEGER TITLEN
      PARAMETER (TITLEN=LBTT2-LBTT1+1)
C
      INTEGER IB1(LNBUF/LNINT),LABEL(256)
      INTEGER*2 IB2(LNBUF/2), I2WRK
      INTEGER*2 TITLE(TITLEN)
      INTEGER*4 INCHES(2)
      INTEGER BLUE(0:LNBUF/LNINT)
      INTEGER GREEN(0:LNBUF/LNINT)
      INTEGER RED(0:LNBUF/LNINT)
      INTEGER*2 RED2(0:LNBUF/2),GREEN2(0:LNBUF/2),BLUE2(0:LNBUF/2)
C
      CHARACTER*4 DFNAM
      CHARACTER*20 MYDATE
      CHARACTER*(TITLEN) TITSTR
      CHARACTER*(FILMAX) FILENM,FILE
C
      LOGICAL EXISTS,LSWAP,LMAP,COLOUR,LPLAN
C
      EQUIVALENCE (RB1,IB1,IB2,LABEL)
      EQUIVALENCE (RB4,BLUE),(RB5,GREEN),(RB6,RED)
C
C     'C' functions
C
      LOGICAL EIKOPE,EIKBYA,EIKCLO
C     LDM Additions
      CHARACTER*20 ESTRING
      INTEGER SEMPBUILD
      Logical endian, motor, SEMCON, IsMotorola
C
      LMAP = VARSET(20856)
      IF (LMAP) THEN
         I = IVALPN(20856)
         IF (SEMOPN(1,I,NCOL,NROW,NLAY,CLASS,FORM,LPN)) GOTO 180
         IF (NCOL .NE. 256 .OR. NROW .NE. 3 .OR. NLAY .NE. 1) GOTO 140
         IF (SEMROW(1,RED,NFMINT,1,1,LPN)) GOTO 180
         IF (SEMROW(1,GREEN,NFMINT,2,1,LPN)) GOTO 180
         IF (SEMROW(1,BLUE,NFMINT,3,1,LPN)) GOTO 180
C
C Now have the data in low byte - transfer to the high one
C
         DO J=1,256
                RED2(J)=RED(J)
                GREEN2(J)=GREEN(J)
                BLUE2(J)=BLUE(J)
         ENDDO
         CALL IROWSW(RED2,RED2,256)
         CALL IROWSW(GREEN2,GREEN2,256)
         CALL IROWSW(BLUE2,BLUE2,256)
      ENDIF
      LPLAN = OPT(26081)
      IF (LPLAN) THEN
         PLANAR = 2
      ELSE
         PLANAR = 1
      ENDIF
C
C Fetch dimensions, etc.
C
      NCOL = NCOLS(LP1)
      NROW = NROWS(LP1)
      NLAY = NLAYS(LP1)
      IF (NLAY .EQ. 1) THEN
         COLOUR = .FALSE.
      ELSE IF (NLAY .EQ. 3) THEN
         COLOUR = .TRUE.
C
C No colour map needed with RGB image
C
         IF (LMAP) THEN
            ERROR = 64
            GOTO 150
         ENDIF
C
C build colour response curves
C
         J = 0
         DO 10 I = 0,255
            RED(I) = J
            J = J + 256
   10    CONTINUE
         I4N = 256
         CALL CFORM(RED,GREEN,NFMINT,NFMINT,I4N)
         CALL CFORM(RED,BLUE,NFMINT,NFMINT,I4N)
         DO J=1,256
                RED2(J)=RED(J)
                GREEN2(J)=GREEN(J)
                BLUE2(J)=BLUE(J)
         ENDDO
      ELSE
         GOTO 130
      ENDIF
C
      I4N = NCOL
      I4BPIM = NROW
      I4BPIM = I4BPIM * I4N
      IF (COLOUR .AND. .NOT.LPLAN) I4BPIM = I4BPIM * 3
      I4NEXT = I4BPIM
      IF (IAND(NCOL,1) .EQ. 1 .AND. IAND(NROW,1) .EQ. 1) THEN
C
C Image is odd number of bytes - grow accordingly
C
         I4NEXT = I4NEXT + 1
      ENDIF
C
      CLASS= CLASSN(LP1)
      IF (CLASS .NE. NCLIMA) GOTO 110
      FORM = FORMN(LP1)
C
C     Calculate row fitting values - use 8k (recommended) buffer size
C
      NFIT = 8192/NCOL
      IF (NFIT .EQ. 0) NFIT = 1
C
C      NEEDED = (NROW+NFIT-1)/NFIT
C
      IF (SEMLNF(FORM,IXFR)) GOTO 180
C
      DFNAM = '.tif'
C
C     Fetch file name from key NAME, prompting if key absent
C
      CALL OUTDEF(DFNAM)
      IF (FILSTR(' ',FILE,NF,.TRUE.)) GOTO 180
      IF (NF .EQ. 0) GOTO 180
C
C     Build full pathname, and see if file exists
C
      IF ( FILMAK ( FILE, DFNAM, FILENM ) ) GOTO 180
      NF = LNBLNK ( FILENM )
      FILE = FILENM(1:NF)
C
      IF ( FILSEA ( FILENM(1:NF), DFNAM, FILE, EXISTS ) ) GOTO 180
C
C     If file already exists, delete it if NEW requested, otherwise
C     raise an error.
C
      IF ( EXISTS ) THEN
         IF ( OUTNEW ( LNEW, FILE, FILENM(1:NF) ) ) GOTO 180
      ENDIF
C
C     Try to open the file
C
      IF ( EIKOPE ( 2, FD, FILENM(1:NF) ) ) GOTO 80
C
C See if picture date and title available
C
      IF (LBLINC) THEN
         N = LABEL(LBNCTT)
         DO 20 I = 1,N
            TITLE(I) = LABEL(LBNCTT+I)
   20    CONTINUE
         CALL SEMCHS(TITSTR,TITLE,N)
         TIMDAY(1) = LABEL(LBYEAR)
         TIMDAY(2) = LABEL(LBMON)
         TIMDAY(3) = LABEL(LBDAY)
         TIMDAY(4) = LABEL(LBHOUR)
         TIMDAY(5) = LABEL(LBMIN)
         TIMDAY(6) = LABEL(LBSEC)
      ELSE
         TITSTR = '(Untitled)'
         N = 10
         CALL MCTIME(TIMDAY)
      ENDIF
      IF (TIMDAY(1) .LT. 1000) TIMDAY(1) = TIMDAY(1) + 1900
C
C Date in format: YYYY:MM:DD HH:MM:SS
C
      I = TIMDAY(1) / 100
      CALL WRITI2(MYDATE(1:2),I)
      I = TIMDAY(1) - (I*100)
      CALL WRITI2(MYDATE(3:4),I)
      MYDATE(5:5) = ':'
      CALL WRITI2(MYDATE(6:7),TIMDAY(2))
      MYDATE(8:8) = ':'
      CALL WRITI2(MYDATE(9:10),TIMDAY(3))
      MYDATE(11:11) = ' '
      CALL WRITI2(MYDATE(12:13),TIMDAY(4))
      MYDATE(14:14) = ':'
      CALL WRITI2(MYDATE(15:16),TIMDAY(5))
      MYDATE(17:17) = ':'
      CALL WRITI2(MYDATE(18:19),TIMDAY(6))
      MYDATE(20:20) = ' '
C
C Determine byte ordering
C
      POKE(1) = 1
      POKE(2) = 0
      CALL CFORM(POKE,POKE,NFMINT,NFMBYT,I42)
      LSWAP = POKE(1) .NE. 1
C
C Build header
C
      IF (LSWAP) THEN
C
C Motorola
C
         POKE(1) = ICHAR('M')
         MOTOR = .true.
      ELSE
C
C Intel
C
         POKE(1) = ICHAR('I')
         MOTOR = .false.
      ENDIF
C     Check that this agrees with endian character of CPU
C      I = SEMPBUILD ( 1 , ESTRING)
C      IF ( ESTRING(1:I) .EQ. 'WORDBIGE' ) THEN
      if(IsMotorola(I))THEN
C       Same as 'MM', i.e. big-endian, motorola
        ENDIAN = .TRUE.
        POKE(1) = ICHAR('M')
      ELSE
C       Same as 'II', i.e. little-endian, intel
        ENDIAN = .FALSE.
        POKE(1) = ICHAR('I')
      ENDIF

361   POKE(2) = POKE(1)
      CALL CFORM(POKE,IB1,NFMINT,NFMBYT,I42)
C
C Version number
C
      IB2(2) = 42
C
C IFD
C
      I4N = 8
      CALL TIFLON(IB2(3),I4N)
C
C Now the actual IFD
C
C Number of tags
C
      NTAG = 16
      IF (LMAP .OR. COLOUR) NTAG = NTAG + 1
C
C (response curve)      IF (COLOUR) NTAG = NTAG + 1
C
      IB2(5) = NTAG
      T = 6
      T2 = 256
C
C Available tag data area
C   8 bytes + IFD tag count + next IFD + (Ntags * 12)
C
      DOFF = 8 + 2 + 4 + (NTAG * 12)
C
C NewSubfileType,Long,1,0
C
      IB2(T) = 254
      IB2(T+1) = 4
      CALL TIFLON(IB2(T+2),I41)
      CALL TIFLON(IB2(T+4),I40)
      T = T + 6
C
C ImageWidth,Short,1,NCOL
C
      IB2(T) = 256
      IB2(T+1) = 3
      CALL TIFLON(IB2(T+2),I41)
      IB2(T+4) = NCOL
      IB2(T+5) = 0
      T = T + 6
C
C ImageHeight,Short,1,NROW
C
      IB2(T) = 257
      IB2(T+1) = 3
      CALL TIFLON(IB2(T+2),I41)
      IB2(T+4) = NROW
      IB2(T+5) = 0
      T = T + 6
C
C BitsPerSample,Short,1,8  or 3,(8,8,8)
C
      IB2(T) = 258
      IB2(T+1) = 3
      IF (COLOUR) THEN
         CALL TIFLON(IB2(T+2),I43)
         IB2(T2-1) = 8
         IB2(T2-2) = 8
         IB2(T2-3) = 8
         T2 = T2 - 3
C
C Adjust to zero-based byte offset
C
         I4N = T2 - 1
         I4N = I4N + I4N
         CALL TIFLON(IB2(T+4),I4N)
      ELSE
         CALL TIFLON(IB2(T+2),I41)
         IB2(T+4) = 8
         IB2(T+5) = 0
      ENDIF
C
      T = T + 6
C
C Photometric interpretation,Short,1,1 or 2 or 3
C
      IB2(T) = 262
      IB2(T+1) = 3
      CALL TIFLON(IB2(T+2),I41)
      IF (COLOUR) THEN
         IB2(T+4) = 2
      ELSE IF (LMAP) THEN
         IB2(T+4) = 3
      ELSE
         IB2(T+4) = 1
      ENDIF
      IB2(T+5) = 0
      T = T + 6
C
C Image title
C
      CALL TIFASC(IB2(T),IB2,DOFF,270,TITSTR(1:N))
      T = T + 6
C
C Manufacturer
C
C      CALL TIFASC(IB2(T),IB2,DOFF,271,"Synoptics Limited")
      CALL TIFASC(IB2(T),IB2,DOFF,271,"Semper 7.0 Beta01")
      T = T + 6
C
C Should be: StripOffsets,Long,1,DataOffset
C but for convenience we make the DataOffset 512 or 2048
C
      IB2(T) = 273
      IB2(T+1) = 4
      IF (LMAP .OR. COLOUR) THEN
         I4N = 2048
      ELSE
         I4N = 512
      ENDIF
      IF (COLOUR .AND. LPLAN) THEN
         CALL TIFLON(IB2(T+2),I43)
         CALL TIFLON(IB2(T2-6),I4N)
         I4N = I4N + I4NEXT
         CALL TIFLON(IB2(T2-4),I4N)
         I4N = I4N + I4NEXT
         CALL TIFLON(IB2(T2-2),I4N)
         T2 = T2 - 6
C
C Adjust to zero-based byte offset
C
         I4N = T2 - 1
         I4N = I4N + I4N
         CALL TIFLON(IB2(T+4),I4N)
      ELSE
         CALL TIFLON(IB2(T+2),I41)
         CALL TIFLON(IB2(T+4),I4N)
      ENDIF
      T = T + 6
C
C SamplesPerPixel,Short,1,1 or 3
C
      IB2(T) = 277
      IB2(T+1) = 3
      CALL TIFLON(IB2(T+2),I41)
      IF (COLOUR) THEN
         IB2(T+4) = 3
      ELSE
         IB2(T+4) = 1
      ENDIF
      IB2(T+5) = 0
      T = T + 6
C
C RowsPerStrip,Short,1,NROW - later use NFIT
C
      IB2(T) = 278
      IB2(T+1) = 3
      CALL TIFLON(IB2(T+2),I41)
      IB2(T+4) = NROW
      IB2(T+5) = 0
      T = T + 6
C
C SamplesByteCounts,Long,1 or 3,BytesPerImageLayer
C
      IB2(T) = 279
      IB2(T+1) = 4
      IF (COLOUR .AND. LPLAN) THEN
         CALL TIFLON(IB2(T+2),I43)
         CALL TIFLON(IB2(T2-2),I4BPIM)
         CALL TIFLON(IB2(T2-4),I4BPIM)
         CALL TIFLON(IB2(T2-6),I4BPIM)
         T2 = T2 - 6
C
C Adjust to zero-based byte offset
C
         I4N = T2 - 1
         I4N = I4N + I4N
         CALL TIFLON(IB2(T+4),I4N)
      ELSE
         CALL TIFLON(IB2(T+2),I41)
         CALL TIFLON(IB2(T+4),I4BPIM)
      ENDIF
      T = T + 6
C
C XResolution,Rational,1,Xinches
C
      IB2(T) = 282
      IB2(T+1) = 5
      CALL TIFLON(IB2(T+2),I41)
      I4N = DOFF
      CALL TIFLON(IB2(T+4),I4N)
      T = T + 6
      INCHES(1) = 72
      INCHES(2) = NCOL
      CALL TIFRAT(IB2,DOFF,INCHES)
C
C YResolution,Rational,1,Yinches
C
      IB2(T) = 283
      IB2(T+1) = 5
      CALL TIFLON(IB2(T+2),I41)
      I4N = DOFF
      CALL TIFLON(IB2(T+4),I4N)
      T = T + 6
      INCHES(1) = 72
      INCHES(2) = NROW
      CALL TIFRAT(IB2,DOFF,INCHES)
C
      IF (COLOUR) THEN
C
C PlanarConfiguration,Short,1,1 (RGB) or 2 (separate planes)
C
         IB2(T) = 284
         IB2(T+1) = 3
         CALL TIFLON(IB2(T+2),I41)
         IB2(T+4) = PLANAR
         IB2(T+5) = 0
         T = T + 6
      ENDIF
C
C ResolutionUnit,Short,1,2 (inches)
C
      IB2(T) = 296
      IB2(T+1) = 3
      CALL TIFLON(IB2(T+2),I41)
      IB2(T+4) = 2
      IB2(T+5) = 0
      T = T + 6
C
C (response curve)      IF (COLOUR) THEN
C
      IF (.FALSE.) THEN
C
C Should be: ColorResponseCurves,Short,768,MapOffset
C but for convenience we make the MapOffset 512
C
         IB2(T) = 301
         IB2(T+1) = 3
         I4N = 768
         CALL TIFLON(IB2(T+2),I4N)
         I4N = 512
         CALL TIFLON(IB2(T+4),I4N)
         T = T + 6
      ENDIF
C
C Software product
C
      CALL TIFASC(IB2(T),IB2,DOFF,305,"Semper 7 port")
      T = T + 6
C
C Image creation date
C
      CALL TIFASC(IB2(T),IB2,DOFF,306,MYDATE)
      T = T + 6
C
      IF (LMAP) THEN
C
C Should be: ColorMap,Short,768,MapOffset
C but for convenience we make the MapOffset 512
C
         IB2(T) = 320
         IB2(T+1) = 3
         I4N = 768
         CALL TIFLON(IB2(T+2),I4N)
         I4N = 512
         CALL TIFLON(IB2(T+4),I4N)
         T = T + 6
      ENDIF
C
C Next IFD
C
      IB2(T) = 0
      IB2(T+1) = 0
C
C Output header line
C
C Would be DOFF but see above (StripOffsets)
C
      IXFR = 512
      IF (EIKBYA(2,FD,IB2,IXFR)) GOTO 160
C
C Output colour map/response curves (note IXFR already correct!)
C
      IF (LMAP .OR. COLOUR) THEN
         IF (EIKBYA(2,FD,RED2,IXFR)) GOTO 160
         IF (EIKBYA(2,FD,GREEN2,IXFR)) GOTO 160
         IF (EIKBYA(2,FD,BLUE2,IXFR)) GOTO 160
      ENDIF
C
      IF (COLOUR .AND. .NOT.LPLAN) THEN
         IXFR = NCOL*3
C
C Loop over layers
C
         DO 50 K=1,NLAY,3
C
C Loop over rows
C
            DO 40 J=1,NROW
               IF (SEMROW(1,RED,NFMINT,J,K,LP1)) GOTO 160
               IF (SEMROW(1,GREEN,NFMINT,J,K+1,LP1)) GOTO 160
               IF (SEMROW(1,BLUE,NFMINT,J,K+2,LP1)) GOTO 160
               T = 1
               DO 30 N = 0,NCOL-1
                  IB1(T) = RED(N)
                  IB1(T+1) = GREEN(N)
                  IB1(T+2) = BLUE(N)
                  T = T + 3
   30          CONTINUE
               I4N = IXFR
               CALL CFORM(IB1,IB1,NFMINT,NFMBYT,I4N)
C
C Write source row to file
C
               IF (EIKBYA(2,FD,IB1,IXFR)) GOTO 170
   40       CONTINUE
C
C Write padding byte if required
C
            IF (I4BPIM .NE. I4NEXT) THEN
               IF (EIKBYA(2,FD,IB1,1)) GOTO 170
            ENDIF
   50    CONTINUE
      ELSE
         IXFR = NCOL
C
C Loop over layers
C
         DO 70 K=1,NLAY
C
C Loop over rows
C
            DO 60 J=1,NROW
               IF (SEMROW(1,RB1,NFMBYT,J,K,LP1)) GOTO 160
C
C Write source row to file
C
               IF (EIKBYA(2,FD,IB1,IXFR)) GOTO 170
   60       CONTINUE
C
C Write padding byte if required
C
            IF (I4BPIM .NE. I4NEXT) THEN
               IF (EIKBYA(2,FD,IB1,1)) GOTO 170
            ENDIF
   70    CONTINUE
      ENDIF
C
C Close file
C
      IF ( EIKCLO ( FD ) ) GOTO 90
      GOTO 180
C
C     Deal with EIKxxx errors - first those where no EIKCLO is required
C
   80 IDMESS = 'Error opening file '//FILENM(1:NF)
      GOTO 100
C
   90 IDMESS = 'Error closing file '//FILENM(1:NF)
  100 ERROR = 77
      GOTO 180
C
C     Errors where a EIKCLO should be attempted
C
C     Wrong class for output
C
  110 ERROR = 6
  120 IDERR = IVALPN(10335)
      GOTO 180
C
C     Wrong size for output
C
  130 ERROR = 5
      GOTO 120
C
C     Wrong size for map
C
  140 ERROR = 5
  150 IDERR = IVALPN(20856)
      GOTO 180
C
  160 IDMESS = 'Error writing file '//FILENM(1:NF)
      ERROR = 77
C
C     Closing a file after an error (ignore any error on closing file)
C
  170 IF ( EIKCLO ( FD ) ) GOTO 180
C
C     All done
C
  180 RETURN
C
C Copyright (C) 1990-1996 Synoptics Ltd,  All Rights Reserved
C
      END
C
C Support routine TIFASC
C
      SUBROUTINE TIFASC(TAG,IB,BOFF,TNUM,STRING)
      INTEGER*2 TAG(6),IB(*)
      INTEGER BOFF,TNUM
      CHARACTER*(*) STRING
C
      INTEGER*4 I4N,I40
      PARAMETER (I40 = 0)
      INTEGER ILZ,IND,ICNT
C
      INTEGER BUFSIZ
      PARAMETER (BUFSIZ=200)
      INTEGER BUFFER(BUFSIZ)
C
      INCLUDE 'COMMON'
C
      ICNT = LEN(STRING)
      IF (ICNT .GE. BUFSIZ) ICNT = BUFSIZ - 1
      ILZ = ICNT + 1
      IF (IAND(ILZ,1) .EQ. 1) ILZ = ILZ + 1
      IF (ILZ .LE. 4) ILZ = 6
C
C     Create ASCII TAG
C
      TAG(1) = TNUM
      TAG(2) = 2
      I4N = ILZ
      CALL TIFLON(TAG(3),I4N)
      CALL TIFLON(TAG(5),I40)
C
C      IF (ILZ .LE. 4) THEN
C
C        Short string - can't do at present
C
C         CALL TIFLON(TAG(3),I40)
C      ELSE
C
         DO 10 IND = 1,ICNT
            BUFFER(IND) = ICHAR(STRING(IND:IND))
   10    CONTINUE
C
C        Add trailing zero(es)
C
         DO 20 IND = ICNT+1,ILZ
            BUFFER(IND) = 0
   20    CONTINUE
C
C        Round to even boundary
C
         IF (IAND(BOFF,1) .EQ. 1) BOFF = BOFF + 1
         I4N = BOFF
         CALL TIFLON(TAG(5),I4N)
C
C        Convert into byte array
C
         I4N = ILZ
         IND = (BOFF/2)+1
         CALL CFORM(BUFFER,IB(IND),NFMINT,NFMBYT,I4N)
         BOFF = BOFF + ILZ
C
C      ENDIF
C
      RETURN
C
C Copyright (C) 1993-1996 Synoptics Ltd,  All Rights Reserved
C
      END
C
C Support routine TIFLON - avoids alignment problems
C
      SUBROUTINE TIFLON(ARR,I4N)
      INTEGER*2 ARR(*)
      INTEGER*4 I4N
      INTEGER*4 POKEL
      INTEGER*2 PEEK(2)
      EQUIVALENCE(POKEL,PEEK)
C
      POKEL = I4N
      ARR(1) = PEEK(1)
      ARR(2) = PEEK(2)
      RETURN
C
C Copyright (C) 1993-1996 Synoptics Ltd,  All Rights Reserved
C
      END
C
C Support routine TIFRAT - store RATIONAL number
C
      SUBROUTINE TIFRAT(IB,DOFF,RAT)
      INTEGER*2 IB(*)
      INTEGER   DOFF, OFF
      INTEGER*4 RAT(2)
C
      OFF = (DOFF/2)+1
      CALL TIFLON(IB(OFF),RAT(1))
      OFF = OFF + 2
      CALL TIFLON(IB(OFF),RAT(2))
      DOFF = DOFF + 8
C
      RETURN
C
C Copyright (C) 1993-1996 Synoptics Ltd,  All Rights Reserved
C
      END
C
C WRITI2 - write integer into character*2 string
C          Used because of problems in OLE controls
C
      SUBROUTINE WRITI2(STRING,I)
      CHARACTER*2 STRING
      INTEGER I
C
      INTEGER T,U
      CHARACTER*1 DIGITS(0:10)
C
      DATA DIGITS /'0','1','2','3','4','5','6','7','8','9','*'/
C
      T = I/10
      U = I - (T*10)
      IF (T .LT. 0 .OR. T .GT. 10) T = 10
      IF (U .LT. 0 .OR. U .GT. 10) U = 10
C
      STRING(1:1) = DIGITS(T)
      STRING(2:2) = DIGITS(U)
      RETURN
C
C Copyright (C) 1993-1996 Synoptics Ltd,  All Rights Reserved
C
      END
