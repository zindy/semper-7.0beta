C Semper 6 processing module OUTBMP
C
      SUBROUTINE OUTBMP(LNEW)
      LOGICAL LNEW
C
C Provides commands OUTPUT BMP:
C  Writes pictures to a Windows BitMap format file that is
C  dynamically opened. Optionally takes a colour map (LUT)
C  using the MAP keyword.
C
      INTEGER IPACK,IVALPN,LNBLNK
      LOGICAL FILMAK,FILSEA,FILSTR,SEMOPN,SEMROW,VARSET
      LOGICAL MWRBIN,OUTNEW,WBMPI2,WBMPI4
C
      INCLUDE 'COMMON'
C
      INTEGER FD,I,J,JR,K,IXFR,LPN,NF,NROW,NLAY,NCOL,NWIDTH,P
      INTEGER CLASS,FORM
C
      CHARACTER*4 DFNAM
      CHARACTER*(FILMAX) FILENM,FILE
C
      INTEGER RGBYTE(4)
      INTEGER IBM(2)
      INTEGER*4 I4N
C
      LOGICAL EXISTS,COLOUR, LSWAP, ISMOTOROLA
C
      INTEGER IB1(0:LNBUF/LNINT)
      INTEGER BLUE(0:LNBUF/LNINT)
      INTEGER GREEN(0:LNBUF/LNINT)
      INTEGER RED(0:LNBUF/LNINT)
C
      EQUIVALENCE (RB1,IB1)
      EQUIVALENCE (RB4,BLUE),(RB5,GREEN),(RB6,RED)
C
C Windows BitMap image header info
C
      INTEGER BFTYPE
      INTEGER*4 BFSIZE
      INTEGER BFRES1,BFRES2
      INTEGER*4 BFOFFB,BISIZE,BIWIDTH,BIHEIGHT
      INTEGER BIPLANES,BIBITCOUNT
      INTEGER*4 BICOM,BISIZEIM,BIXPM,BIYPM
      INTEGER*4 BICLRUSED,BICLRIMP,CLR(0:255)
      INTEGER CLRI(0:511)
C
C This is really a structure!
C
C      EQUIVALENCE (BFTYPE,IB1(1))
C      EQUIVALENCE (BFSIZE,IB1(2))
C      EQUIVALENCE (BFRES1,IB1(4))
C      EQUIVALENCE (BFRES2,IB1(5))
C      EQUIVALENCE (BFOFFB,IB1(6))
C      EQUIVALENCE (BISIZE,IB1(8))
C      EQUIVALENCE (BIWIDTH,IB1(10))
C      EQUIVALENCE (BIHEIGHT,IB1(12))
C      EQUIVALENCE (BIPLANES,IB1(14))
C      EQUIVALENCE (BIBITCOUNT,IB1(15))
C      EQUIVALENCE (BICOM,IB1(16))
C      EQUIVALENCE (BISIZEIM,IB1(18))
C      EQUIVALENCE (BIXPM,IB1(20))
C      EQUIVALENCE (BIYPM,IB1(22))
C      EQUIVALENCE (BICLRUSED,IB1(24))
C      EQUIVALENCE (BICLRIMP,IB1(26))
C      EQUIVALENCE (CLR,CLRI,IB1(28))
C
      EQUIVALENCE (CLR,CLRI)
C
      INTEGER*4 I42,I44
      PARAMETER (I42=2,I44=4)
C
C 'C' functions
C
      LOGICAL EIKOPE,EIKCLO
C
C Colour map specified ?
C
      RGBYTE(4) = 0
      IF (VARSET(20856)) THEN
         I = IVALPN(20856)
         IF (SEMOPN(1,I,NCOL,NROW,NLAY,CLASS,FORM,LPN)) GOTO 150
         IF (NCOL .NE. 256 .OR. NROW .NE. 3 .OR. NLAY .NE. 1) GOTO 120
         IF (SEMROW(1,RED,NFMINT,1,1,LPN)) GOTO 150
         IF (SEMROW(1,GREEN,NFMINT,2,1,LPN)) GOTO 150
         IF (SEMROW(1,BLUE,NFMINT,3,1,LPN)) GOTO 150
         DO 10 J = 0,255
C
C Order is Blue,Green,Red,Ignore
C
            RGBYTE(1) = BLUE(J)
            RGBYTE(2) = GREEN(J)
            RGBYTE(3) = RED(J)
            CALL CFORM(RGBYTE,CLR(J),NFMINT,NFMBYT,I44)
   10    CONTINUE
      ELSE
C
C Build default colour map
C
         DO 20 J = 0,255
C
C Order is Blue,Green,Red,Ignore
C
            RGBYTE(1) = J
            RGBYTE(2) = J
            RGBYTE(3) = J
            CALL CFORM(RGBYTE,CLR(J),NFMINT,NFMBYT,I44)
   20    CONTINUE
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
      ELSE
         GOTO 110
      ENDIF
      CLASS= CLASSN(LP1)
      IF (CLASS .NE. NCLIMA) GOTO 90
      FORM = FORMN(LP1)
C
      DFNAM = '.bmp'
C
C     Fetch file name from key NAME, prompting if key absent
C
      CALL OUTDEF(DFNAM)
      IF (FILSTR(' ',FILE,NF,.TRUE.)) GOTO 150
      IF (NF .EQ. 0) GOTO 150
C
C     Build full pathname, and see if file exists
C
      IF ( FILMAK ( FILE, DFNAM, FILENM ) ) GOTO 150
      NF = LNBLNK ( FILENM )
      FILE = FILENM(1:NF)
C
      IF ( FILSEA ( FILENM(1:NF), DFNAM, FILE, EXISTS ) ) GOTO 150
C
C     If file already exists, delete it if NEW requested, otherwise
C     raise an error.
C
      IF ( EXISTS ) THEN
         IF ( OUTNEW ( LNEW, FILE, FILENM(1:NF) ) ) GOTO 150
      ENDIF
C
C     Try to open the file
C
      IF ( EIKOPE ( 2, FD, FILENM(1:NF) ) ) GOTO 60
      NWIDTH = NCOL
C
C Need to round up transfers to nearest longword in length
C
      NWIDTH = NWIDTH + 3
      NWIDTH = NWIDTH / 4
      NWIDTH = NWIDTH * 4
C
C     Set the header and output it
C
      IBM(1) = ICHAR('B')
      IBM(2) = ICHAR('M')
      CALL CFORM(IBM,BFTYPE,NFMINT,NFMBYT,I42)
C
      BFRES1 = 0
      BFRES2 = 0
      BFOFFB = 54
      IF (.NOT.COLOUR) BFOFFB = BFOFFB + (256 * 4)
C
      BISIZEIM = NWIDTH
      BISIZEIM = BISIZEIM * NROW
      IF (COLOUR) BISIZEIM = BISIZEIM * 3
      BFSIZE = BISIZEIM + BFOFFB
      BISIZE = 40
      BIWIDTH = NWIDTH
      BIHEIGHT = NROW
      BIPLANES = 1
      IF (COLOUR) THEN
         BIBITCOUNT = 24
      ELSE
         BIBITCOUNT = 8
      ENDIF
      BICOM = 0
      BIXPM = 0
      BIYPM = 0
      IF (COLOUR) THEN
         BICLRUSED = 0
         BICLRIMP = 0
      ELSE
         BICLRUSED = 256
         BICLRIMP = 256
      ENDIF
C
C     SWAP ?
      LSWAP=.not.ISMOTOROLA(JUNK)
C     Write header
C
      IXFR = 2
      IF ( MWRBIN ( FD,IXFR,BFTYPE,NFMBYT,LSWAP ) ) GOTO 130
      IF ( WBMPI4 ( FD,BFSIZE ) ) GOTO 130
      IF ( WBMPI2 ( FD,BFRES1 ) ) GOTO 130
      IF ( WBMPI2 ( FD,BFRES2 ) ) GOTO 130
      IF ( WBMPI4 ( FD,BFOFFB ) ) GOTO 130
      IF ( WBMPI4 ( FD,BISIZE ) ) GOTO 130
      IF ( WBMPI4 ( FD,BIWIDTH ) ) GOTO 130
      IF ( WBMPI4 ( FD,BIHEIGHT ) ) GOTO 130
      IF ( WBMPI2 ( FD,BIPLANES ) ) GOTO 130
      IF ( WBMPI2 ( FD,BIBITCOUNT ) ) GOTO 130
      IF ( WBMPI4 ( FD,BICOM ) ) GOTO 130
      IF ( WBMPI4 ( FD,BISIZEIM ) ) GOTO 130
      IF ( WBMPI4 ( FD,BIXPM ) ) GOTO 130
      IF ( WBMPI4 ( FD,BIYPM ) ) GOTO 130
      IF ( WBMPI4 ( FD,BICLRUSED ) ) GOTO 130
      IF ( WBMPI4 ( FD,BICLRIMP ) ) GOTO 130
      IF (.NOT.COLOUR) THEN
         IXFR = 256 * 4
         IF ( MWRBIN ( FD,IXFR,CLRI,NFMBYT,LSWAP ) ) GOTO 130
      ENDIF
C
      FORM = NFMBYT
      IXFR = NWIDTH
      IF (COLOUR) IXFR = IXFR * 3
C
C Need to round up transfers to nearest longword in length
C
      IXFR = IXFR + 3
      IXFR = IXFR / 4
      IXFR = IXFR * 4
C
C     Loop over rows
C
      JR = NROW
      DO 50 J=1,NROW
C
C        Read source row(s) from LP1
C
         IF (COLOUR) THEN
            IF (SEMROW(1,RED,NFMINT,JR,1,LP1)) GOTO 140
            IF (SEMROW(1,GREEN,NFMINT,JR,2,LP1)) GOTO 140
            IF (SEMROW(1,BLUE,NFMINT,JR,3,LP1)) GOTO 140
            P = 0
            DO 30 K = 0,NCOL-1
               IB1(P) = BLUE(K)
               IB1(P+1) = GREEN(K)
               IB1(P+2) = RED(K)
               P = P+3
   30       CONTINUE
C
C Pad to boundary
C
            IF (NCOL .NE. NWIDTH) THEN
               DO 40 K = NCOL+1,NWIDTH
                  IB1(P) = 0
                  IB1(P+1) = 0
                  IB1(P+2) = 0
                  P = P+3
   40          CONTINUE
            ENDIF
            P = P + 1
            I4N = P
            CALL CFORM(IB1,IB1,NFMINT,FORM,I4N)
         ELSE
            IF (SEMROW(1,IB1,FORM,JR,1,LP1)) GOTO 140
         ENDIF
         JR = JR - 1
C
C        Write source row to file. Straight output can be used
C        as we don't write RLE at present.
C
         IF (MWRBIN(FD,IXFR,IB1,FORM,LSWAP)) GOTO 130
   50 CONTINUE
C
C     Close file
C
      IF ( EIKCLO ( FD ) ) GOTO 70
      GOTO 150
C
C     Deal with EIKxxx errors - first those where no EIKCLO is required
C
   60 IDMESS = 'Error opening file '//FILENM(1:NF)
      GOTO 80
C
   70 IDMESS = 'Error closing file '//FILENM(1:NF)
   80 ERROR = 77
      GOTO 150
C
C     Wrong class for output
C
   90 ERROR = 6
  100 IDERR = IVALPN(10335)
      GOTO 150
C
C     Wrong size for output
C
  110 ERROR = 5
      GOTO 100
C
C     Wrong size for map
C
  120 ERROR = 5
      IDERR = IVALPN(20856)
      GOTO 150
C
C     Errors where a EIKCLO should be attempted
C
  130 IDMESS = 'Error writing file '//FILENM(1:NF)
      ERROR = 77
C
C     Closing a file after an error (ignore any error on closing file)
C
  140 IF ( EIKCLO ( FD ) ) GOTO 150
C
C     All done
C
  150 RETURN
C
C Copyright (C) 1990-1996 Synoptics Ltd,  All Rights Reserved
C
      END
C
      LOGICAL FUNCTION WBMPI2 ( FD, I )
      INTEGER FD,I
C
      INCLUDE 'PARAMS'
C
      INTEGER N
C
      LOGICAL MWRBIN, LSWAP, ISMOTOROLA
      LSWAP=.not.ISMOTOROLA(JUNK)
C
      N = 2
      WBMPI2 = MWRBIN ( FD,N,I,NFMINT,LSWAP )
      RETURN
      END
C
      LOGICAL FUNCTION WBMPI4 ( FD, I )
      INTEGER FD
      INTEGER*4 I
C
      INCLUDE 'PARAMS'
C
      INTEGER N
C
      LOGICAL MWRBIN, LSWAP,ISMOTOROLA
      LSWAP=.not.ISMOTOROLA(JUNK)
C
      N = 4
C
C Use NFMFP as there isn't an NFMI4
C
      WBMPI4 = MWRBIN ( FD,N,I,NFMFP,LSWAP )
      RETURN
      END
