C Semper 6 processing module INPBMP
C
      SUBROUTINE INPBMP
C
C Provides command INPUT BMP:
C  Reads pictures from a Windows BitMap file that is
C  dynamically opened.
C
      INTEGER IPACK,IVALPN,LNBLNK,SEMFRM
      LOGICAL FILSEA,FILSTR,SEMCLS,SEMLAB,SEMOPN
      LOGICAL SEMROW,SEMROWI,SEMSEL,VARSET
      LOGICAL MRDBIN,RBMPI2,RBMPI4
C
      INTEGER CLASS,FORM
C
      INCLUDE 'COMMON'
C
      CHARACTER*(FILMAX) FILENM,FILE
      CHARACTER*4 DFNAM
C
      INTEGER FD,I,J,K,N,NF,IXFR
      INTEGER NROW,NLAY,NCOL,NROW2,NLAY2,NCOL2,P
C
      INTEGER IB1(LNBUF/LNINT),LABEL(256)
      INTEGER BLUE(0:LNBUF/LNINT)
      INTEGER GREEN(0:LNBUF/LNINT)
      INTEGER RED(0:LNBUF/LNINT)
      INTEGER RGBYTE(3)
C
      LOGICAL COLOUR,EXISTS,LMAP,LGREY,LNONE

C
      EQUIVALENCE (RB1,IB1,LABEL)
      EQUIVALENCE (RB4,BLUE),(RB5,GREEN),(RB6,RED)
C
      INTEGER*4 I4N
C
C Windows BitMap image header info
C
      INTEGER BFTYPE
      INTEGER*4 BFSIZE
      INTEGER BFRES1,BFRES2
      INTEGER*4 BFOFFB,BISIZE,BIWIDT,BIHGHT
      INTEGER BIPLAN,BIBITC
      INTEGER*4 BICOM,BISZIM,BIXPM,BIYPM
      INTEGER*4 BICLRU,BICLRI,CLR(0:255)
      INTEGER CLRI(0:511)
C
C This is really a structure!
C
      EQUIVALENCE (CLR,CLRI,IB1)
C
      INTEGER*4 I42,I43
      PARAMETER (I42=2,I43=3)
C
C     'C' functions
C
      LOGICAL EIKOPE,EIKBYA,EIKCLO
C
C     SWAP ?
      LOGICAL ISMOTOROLA, LSWAP
      LSWAP=.not.ISMOTOROLA(JUNK)
C     Pick up options and mandatory keys
C
      LMAP = VARSET(20856)
C
C     Fetch file name from key NAME, prompting if key absent
C
      DFNAM = '.bmp'
      CALL INPDEF(DFNAM)
      IF (FILSTR(' ',FILE,NF,.FALSE.)) GOTO 150
      IF (NF .EQ. 0) GOTO 150
C
C     See if file exists on the path if reading
C
      IF ( FILSEA ( FILE(1:NF), DFNAM, FILENM, EXISTS ) ) GOTO 150
      IF ( EXISTS ) THEN
         NF  = LNBLNK ( FILENM )
      ELSE
C
C     Error - non-existent file requested for read
C
         ERROR  = 130
         IDMESS = FILE(1:NF)
         GOTO 150
      ENDIF
C
C     Try to open the file
C
      IF ( EIKOPE ( 1, FD, FILENM(1:NF) ) ) GOTO 70
C
      IXFR = 2
      IF ( MRDBIN ( FD,IXFR,BFTYPE,NFMBYT,LSWAP ) ) GOTO 120
      CALL CFORM ( BFTYPE,IB1,NFMBYT,NFMINT,I42 )
C
C File should start with 'BM'
C
      IF (IB1(1) .NE. ICHAR('B') .OR. IB1(2) .NE. ICHAR('M')) GOTO 100
      IF ( RBMPI4 ( FD,BFSIZE ) ) GOTO 120
      IF ( RBMPI2 ( FD,BFRES1 ) ) GOTO 120
      IF ( RBMPI2 ( FD,BFRES2 ) ) GOTO 120
      IF ( RBMPI4 ( FD,BFOFFB ) ) GOTO 120
      IF ( RBMPI4 ( FD,BISIZE ) ) GOTO 120
      IF ( RBMPI4 ( FD,BIWIDT ) ) GOTO 120
      IF ( RBMPI4 ( FD,BIHGHT ) ) GOTO 120
      IF ( RBMPI2 ( FD,BIPLAN ) ) GOTO 120
      IF ( RBMPI2 ( FD,BIBITC ) ) GOTO 120
      IF ( RBMPI4 ( FD,BICOM ) ) GOTO 120
      IF ( RBMPI4 ( FD,BISZIM ) ) GOTO 120
      IF ( RBMPI4 ( FD,BIXPM ) ) GOTO 120
      IF ( RBMPI4 ( FD,BIYPM ) ) GOTO 120
      IF ( RBMPI4 ( FD,BICLRU ) ) GOTO 120
      IF ( RBMPI4 ( FD,BICLRI ) ) GOTO 120
C
      IF (BIPLAN .NE. 1 .OR. BICOM .NE. 0) GOTO 100
      NCOL = BIWIDT
      NROW = BIHGHT
      IF (BIBITC .EQ. 8) THEN
         NLAY = 1
         COLOUR = .FALSE.
         IF (BICLRU .EQ. 0) BICLRU = 256
      ELSE IF (BIBITC .EQ. 24) THEN
         NLAY = 3
         COLOUR = .TRUE.
      ELSE
C
C Unsupported pixel depth
C
         GOTO 110
      ENDIF
C
      LGREY = .TRUE.
      LNONE = .TRUE.
C
      IF (BICLRU .NE. 0) THEN
C
C Read in the colour map
C
         K = BICLRU
         IXFR = K * 4
         IF (EIKBYA(1,FD,CLRI,IXFR)) GOTO 120
         DO 10 J = 0,K-1
            CALL CFORM(CLR(J),RGBYTE,NFMBYT,NFMINT,I43)
            BLUE(J) = RGBYTE(1)
            GREEN(J) = RGBYTE(2)
            RED(J) = RGBYTE(3)
   10    CONTINUE
         IF (K .LT. 256) THEN
            DO 20 J = K,255
               BLUE(J) = J
               GREEN(J) = J
               RED(J) = J
   20       CONTINUE
         ENDIF
C
C Colour map now in RED,GREEN,BLUE
C   Could write now if MAP given - but should first check for
C   greyscale (red=green=blue) and ascending (lut(n)=n)
C
         DO 30 J = 0,255
            IF (BLUE(J) .NE. GREEN(J) .OR. BLUE(J) .NE. RED(J)) THEN
               LGREY = .FALSE.
               GOTO 40
            ENDIF
            IF (BLUE(J) .NE. J) LNONE = .FALSE.
   30    CONTINUE
C
   40    CONTINUE
C
C Store away the LUT
C
         IF (LMAP) THEN
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
     +                 NCOL2,NROW2,NLAY2,CLASS,FORM,LP2)) GOTO 140
C
C Form title from input file name + (map)
C
            N = MIN(NF+5,LBTT2 - LBTT1)
            LABEL(LBNCTT) = N
            FILE = FILENM(1:NF)//'(map)'
            CALL SEMICS(FILE,LABEL(LBTT1),N)
C
C Update label if possible
C
            IF (N.NE.0 .AND. LBLINC) THEN
               IF (SEMLAB(2,LABEL,LP2)) GOTO 140
            ENDIF
C
C Now copy data
C
            K = 1
            IF (SEMROWI(2,RED,NFMINT,1,1,LP2)) GOTO 140
            IF (NROW2 .EQ. 3) THEN
               IF (SEMROWI(2,GREEN,NFMINT,2,1,LP2)) GOTO 140
               IF (SEMROWI(2,BLUE,NFMINT,3,1,LP2)) GOTO 140
            ENDIF
C
C and close the image
C
            IF (SEMCLS(LP2)) GOTO 140
         ENDIF
      ENDIF
C
      FORM = SEMFRM(NFMBYT)
      CLASS = NCLIMA
C
C     Open picture
C
      LP1 = 0
      IF (SEMOPN(2,IVALPN(-601),NCOL,NROW,NLAY,CLASS,
     +           FORM,LP1)) GOTO 140
C
C     Form title from input file name
C
      N = MIN(NF,LBTT2 - LBTT1)
      LABEL(LBNCTT) = N
      CALL SEMICS(FILENM,LABEL(LBTT1),N)
C
C     Update label if possible
C
      IF (N.NE.0 .AND. LBLINC) THEN
         IF (SEMLAB(2,LABEL,LP1)) GOTO 140
      ENDIF
C
C Round width to nearest longword...
C
      IXFR = NCOL
      IF (COLOUR) IXFR = IXFR * 3
      IXFR = IXFR + 3
      IXFR = IXFR / 4
      IXFR = IXFR * 4
      I4N = IXFR
C
C     Loop over rows
C
      I = NROW
      DO 60 J=1,NROW
         IF (COLOUR) THEN
C
C     Read source row from file
C
            IF (EIKBYA(1,FD,IB1,IXFR)) GOTO 120
            CALL CFORM(IB1(1),IB1,NFMBYT,NFMINT,I4N)
C
C     Unpack the pixels
C
            P = 1
            DO 50 K = 0,NCOL-1
               BLUE(K) = IB1(P)
               GREEN(K) = IB1(P+1)
               RED(K) = IB1(P+2)
               P = P + 3
   50       CONTINUE
C
C     Store the rows to the destination in LP1
C
            IF (SEMROWI(2,RED,NFMINT,I,1,LP1)) GOTO 140
            IF (SEMROWI(2,GREEN,NFMINT,I,2,LP1)) GOTO 140
            IF (SEMROWI(2,BLUE,NFMINT,I,3,LP1)) GOTO 140
         ELSE
C
C     Read source row from file
C
            IF (EIKBYA(1,FD,IB1,IXFR)) GOTO 120
C
C     Store source row in LP1
C
            IF (SEMROW(2,RB1,NFMBYT,I,1,LP1)) GOTO 140
         ENDIF
         I = I - 1
   60 CONTINUE
C
C     Close file
C
      IF ( EIKCLO ( FD ) ) GOTO 80
C
C     Finally, select this image
C
      IF (SEMSEL(LP1)) GOTO 150
      GOTO 150
C
C     Deal with UNIX I/O errors
C
   70 IDMESS = 'Error opening file '//FILENM(1:NF)
      GOTO 90
C
   80 IDMESS = 'Error closing file '//FILENM(1:NF)
   90 ERROR = 77
      GOTO 150
C
  100 IDMESS = 'Unrecognised or unsupported file format'
      GOTO 130
C
  110 IDMESS = 'Unsupported pixel bit depth'
      GOTO 130
C
  120 IDMESS = 'Error reading file '//FILENM(1:NF)
      GOTO 130
C
C     Closing a file after an error (ignore any error on closing file)
C
  130 ERROR = 77
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
      LOGICAL FUNCTION RBMPI2 ( FD, I )
      INTEGER FD,I
C
      INCLUDE 'PARAMS'
C
      INTEGER N
C
      LOGICAL MRDBIN
C     SWAP ?
      LOGICAL ISMOTOROLA, LSWAP
      LSWAP=.not.ISMOTOROLA(JUNK)
C
      N = 2
      RBMPI2 = MRDBIN ( FD,N,I,NFMINT,LSWAP )
      RETURN
      END
C
      LOGICAL FUNCTION RBMPI4 ( FD, I )
      INTEGER FD
      INTEGER*4 I
C
      INCLUDE 'PARAMS'
C
      INTEGER N
C
      LOGICAL MRDBIN
C     SWAP ?
      LOGICAL ISMOTOROLA, LSWAP
      LSWAP=.not.ISMOTOROLA(JUNK)
C
      N = 4
C
C Use NFMFP as there isn't an NFMI4
C
      RBMPI4 = MRDBIN ( FD,N,I,NFMFP,LSWAP )
      RETURN
      END
