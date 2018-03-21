C Semper 6 processing module INPRAS
C
      SUBROUTINE INPRAS
C
C Provides command INPUT RASTER:
C  Reads pictures from a sequential binary file in
C  SunRaster format.
C
      INTEGER IPACK,IVALPN,LNBLNK
      LOGICAL FILSEA,FILSTR,SEMLAB,SEMOPN,SEMROW,VARSET
      LOGICAL MRDBIN,MRDRFF
C
      INTEGER CLASS,FORM
      INTEGER*2 SWTEMP(6)
C
      INCLUDE 'COMMON'
C
      CHARACTER*(FILMAX) FILENM,FILE
      CHARACTER*4 DFNAM
C
C Raster File header fields
C
      INTEGER*4 RASFIL(8)
      INTEGER*4 RMAGIC,RWIDTH,RHITE,RDEPTH,RLENG,RTYPE,RMAPT,RMAPL
      EQUIVALENCE (RASFIL(1),RMAGIC),(RASFIL(2),RWIDTH)
      EQUIVALENCE (RASFIL(3),RHITE), (RASFIL(4),RDEPTH)
      EQUIVALENCE (RASFIL(5),RLENG), (RASFIL(6),RTYPE)
      EQUIVALENCE (RASFIL(7),RMAPT), (RASFIL(8),RMAPL)
C
C Raster parameters
C
C RAS_MAGIC  0x59a66a95
C
      INTEGER*4 RASMGC
      PARAMETER (RASMGC=1504078485)
      INTEGER*4 RTOLD,RTSTD,RTBENC
      PARAMETER (RTOLD=0,RTSTD=1,RTBENC=2)
      INTEGER*4 RMTNON,RMTRGB,RMTRAW
      PARAMETER (RMTNON=0,RMTRGB=1,RMTRAW=2)
C
      INTEGER FD,J,K,N,NF,IXFR
      INTEGER NROW,NLAY,NCOL,NROW2,NLAY2,NCOL2
      INTEGER RXFR,RINDX,RDAT,RPT
C
      INTEGER IB1(LNBUF/LNINT),IB4(LNBUF/LNINT),LABEL(256)
      INTEGER*2 TITLE(LBTT2-LBTT1+1),SLABEL(256)
C
      LOGICAL EXISTS,ISRAST,LMAP
C
      EQUIVALENCE (RB1,IB1,LABEL),(RB4,IB4,SLABEL)
      EQUIVALENCE (TITLE,RB6),(RASFIL,SWTEMP)
C
C     'C' functions
C
      LOGICAL EIKOPE,EIKBYA,EIKCLO
C
      ISRAST = .TRUE.
      LMAP = VARSET(20856)
C
C     Fetch file name from key NAME, prompting if key absent
C
      DFNAM = '.rff'
      CALL INPDEF(DFNAM)
      IF (FILSTR(' ',FILE,NF,.FALSE.)) GOTO 110
      IF (NF .EQ. 0) GOTO 110
C
C     See if file exists on the path if reading
C
      IF ( FILSEA ( FILE(1:NF), DFNAM, FILENM, EXISTS ) ) GOTO 110
      IF ( EXISTS ) THEN
         NF  = LNBLNK ( FILENM )
      ELSE
C
C        Error - non-existent file requested for read
C
         ERROR  = 130
         IDMESS = FILE(1:NF)
         GOTO 110
      ENDIF
C
C     Try to open the file
C
      IF ( EIKOPE ( 1, FD, FILENM(1:NF) ) ) GOTO 50
C
      IXFR = 32
      IF ( MRDBIN ( FD,32,SWTEMP,NFMFP,.TRUE. ) ) GOTO 80
C
      IF ( RMAGIC .NE. RASMGC ) THEN
         IDMESS = 'Bad Magic number in Raster File'
         GOTO 90
      ENDIF
C
C What depths can we plumb at present?
C
      IF ( RDEPTH .NE. 8 ) THEN
         WRITE(IDMESS,10) RDEPTH
   10    FORMAT('Cannot read Raster of depth',I3,' at present')
         GOTO 90
      ENDIF
C
      IF ( RLENG .EQ. 0 ) THEN
C
C Length not set - can we compute it ?
C
         IF ( RTYPE .EQ. RTOLD .OR. RTYPE .EQ. RTSTD ) THEN
            RLENG = RWIDTH * RHITE * RDEPTH
            RLENG = RLENG / 8
            IF ( RLENG .LE. 0 ) THEN
               IDMESS = 'Bad dimension in Raster File header'
               GOTO 90
            ENDIF
         ELSE
            IDMESS = 'No image length in Raster File header'
            GOTO 90
         ENDIF
      ENDIF
C
      IF ( RMAPT .NE. RMTNON .AND. RMAPL .NE. 0 ) THEN
         IXFR = (RMAPL)
         IF (LMAP) THEN
            IF ( RMAPT .EQ. RMTRGB ) THEN
               NROW2 = 3
               IXFR = IXFR/3
            ELSE
               NROW2 = 1
            ENDIF
            NCOL2 = IXFR
            NLAY2 = 1
            CLASS = NCLIMA
            FORM = NFMBYT
C
C Open picture
C
            LP2 = 0
            IF (SEMOPN(2,IVALPN(20856),
     +                 NCOL2,NROW2,NLAY2,CLASS,FORM,LP2)) GOTO 100
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
               IF (SEMLAB(2,LABEL,LP2)) GOTO 100
            ENDIF
C
C Now copy data
C
            K = 1
            DO 20 J=1,NROW2
               IF (EIKBYA(1,FD,RB1,NCOL2)) GOTO 80
C
C Store source row in LP2
C
               IF (SEMROW(2,RB1,FORM,J,K,LP2)) GOTO 100
   20       CONTINUE
         ELSE
C
C Skip colormap details if MAP key not given
C
            IF ( EIKBYA ( 1, FD, RB1, IXFR ) ) GOTO 80
         ENDIF
      ENDIF
C
      NCOL = (RWIDTH)
      NROW = (RHITE)
      NLAY = 1
      FORM = NFMBYT
C
      IF (RTYPE .EQ. RTBENC) THEN
C
C Initialise Raster unpacker
C
         RDAT = 0
         RPT  = 0
         RXFR = 0
         RINDX = 0
      ENDIF
C
C Images of depth 8 are no problem!
C
      IF (RDEPTH .EQ. 8) THEN
         IXFR = NCOL
         IF (RTYPE .EQ. RTOLD .OR. RTYPE .EQ. RTSTD) THEN
C
C Can treat as raw binary!
C
            ISRAST = .FALSE.
         ENDIF
      ENDIF
      CLASS = NCLIMA
C
C Open picture
C
      LP1 = 0
      IF (SEMOPN(2,IVALPN(-601),NCOL,NROW,NLAY,CLASS,
     +           FORM,LP1)) GOTO 100
C
C Form title from input file name
C
      N = MIN(NF,LBTT2 - LBTT1)
      LABEL(LBNCTT) = N
      CALL SEMICS(FILENM,LABEL(LBTT1),N)
C
C Update label if possible
C
      IF (N.NE.0 .AND. LBLINC) THEN
         IF (SEMLAB(2,LABEL,LP1)) GOTO 100
      ENDIF
C
C Loop over layers
C
      DO 40 K=1,NLAY
C
C Loop over rows
C
         DO 30 J=1,NROW
C
C Read source row from file
C
            IF (ISRAST) THEN
C
C Read next IXFR items
C
               IF (MRDRFF(FD,IXFR,IB1,IB4,FORM,
     +                    RDAT,RPT,RXFR,RINDX,RLENG,RDEPTH)) THEN
                  IF (ERROR .NE. 0) GOTO 100
                  GOTO 80
               ENDIF
            ELSE
               IF (MRDBIN(FD,IXFR,IB1,FORM,.TRUE.)) GOTO 80
            ENDIF
C
C Store source row in LP1
C
            IF (SEMROW(2,RB1,FORM,J,K,LP1)) GOTO 100
   30    CONTINUE
   40 CONTINUE
C
C Close file
C
      IF ( EIKCLO ( FD ) ) GOTO 60
      GOTO 110
C
C     Deal with UNIX I/O errors
C
   50 IDMESS = 'Error opening file '//FILENM(1:NF)
      GOTO 70
C
   60 IDMESS = 'Error closing file '//FILENM(1:NF)
   70 ERROR = 77
      GOTO 110
C
   80 IDMESS = 'Error reading file '//FILENM(1:NF)
   90 ERROR = 77
C
C     Closing a file after an error (ignore any error on closing file)
C
  100 IF ( EIKCLO ( FD ) ) GOTO 110
C
C     All done
C
  110 RETURN
C
C Copyright (C) 1990-1994 Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 read routine MRDRFF for Packed Raster files
C
      LOGICAL FUNCTION MRDRFF(FD,IXFR,DBUF,SBUF,FORM,RDAT,RPT,
     +                        RXFR,RINDX,RLENG,RDEPTH)
      INTEGER FD,IXFR,FORM,RDAT,RPT,RXFR,RINDX
      INTEGER DBUF(IXFR),SBUF(*)
      INTEGER*4 RLENG,RDEPTH
C
C FD     :-   file descriptor
C IXFR   :-   bytes to transfer
C DBUF   :-   destination buffer
C SBUF   :-   source buffer
C FORM   :-   data form - normally byte
C RDAT   :-   current byte to repeat
C RPT    :-   current repeat count
C RXFR   :-   current read length
C RINDX  :-   current read pointer
C RLENG  :-   remaining bytes in image file
C RDEPTH :-   image depth (in bits) - not used at present must be 8
C
      LOGICAL MRDRFI
C
      INCLUDE 'COMMON'
C
      INTEGER*4 I4N
C
      INTEGER DPOINT,ESCAPE
      PARAMETER (ESCAPE=128)
C
      MRDRFF = .TRUE.
      IF (RDEPTH .NE. 8) GOTO 20
      DPOINT = 0
C
   10 IF (DPOINT .LT. IXFR) THEN
         IF (RPT .GT. 0) THEN
            DPOINT = DPOINT + 1
            DBUF(DPOINT) = RDAT
            RPT = RPT - 1
            GOTO 10
         ENDIF
C
C Need more data
C
         RINDX = RINDX + 1
         IF (RINDX .GT. RXFR) THEN
            IF (MRDRFI(FD,SBUF,RXFR,RINDX,RLENG)) GOTO 20
         ENDIF
         RDAT = SBUF(RINDX)
         IF (RDAT .EQ. ESCAPE) THEN
C
C Read next two bytes as RPT and RDAT
C
            RINDX = RINDX + 1
            IF (RINDX .GT. RXFR) THEN
               IF (MRDRFI(FD,SBUF,RXFR,RINDX,RLENG)) GOTO 20
            ENDIF
C
C Add 1 to count as single bytes never encoded
C
            RPT = SBUF(RINDX) + 1
            IF (RPT .EQ. 1) THEN
C
C Was really ESCAPE!
C
               RPT = 1
               RDAT = ESCAPE
            ELSE
               RINDX = RINDX + 1
               IF (RINDX .GT. RXFR) THEN
                  IF (MRDRFI(FD,SBUF,RXFR,RINDX,RLENG)) GOTO 20
               ENDIF
               RDAT = SBUF(RINDX)
            ENDIF
         ELSE
            RPT = 1
         ENDIF
         GOTO 10
      ENDIF
C
      IF (FORM .NE. NFMINT) THEN
         I4N = IXFR
         CALL CFORM(DBUF,DBUF,NFMINT,FORM,I4N)
      ENDIF
C
      MRDRFF = .FALSE.
   20 RETURN
C
C Copyright (C) 1991-1993 Synoptics Ltd,  All Rights Reserved
C
      END
C
      LOGICAL FUNCTION MRDRFI(FD,DBUF,RXFR,RINDX,RLENG)
C
      INTEGER FD,RXFR,RINDX
      INTEGER*4 RLENG
      INTEGER DBUF(*)
C
C FD     :-   file descriptor
C DBUF   :-   receiver buffer
C RXFR   :-   current read length
C RINDX  :-   current read pointer
C RLENG  :-   remaining bytes in image file
C RDEPTH :-   image depth (in bits) - not used at present must be 8
C
      LOGICAL MRDBIN
C
      INCLUDE 'COMMON'
C
      INTEGER*4 I4N
C
      MRDRFI = .TRUE.
      RXFR = LNBUF/LNINT
C
      IF (RXFR .GT. RLENG) RXFR = RLENG
      IF (RXFR .GT. 0) THEN
         IF (.NOT.MRDBIN(FD,RXFR,DBUF,NFMBYT,.TRUE.)) THEN
            I4N = RXFR
            RLENG = RLENG - I4N
            CALL CFORM(DBUF,DBUF,NFMBYT,NFMINT,I4N)
            RINDX = 1
            MRDRFI = .FALSE.
         ENDIF
      ELSE
         ERROR = 77
         IDMESS = 'Input Raster File exhausted'
      ENDIF
      RETURN
C
C Copyright (C) 1991-1993 Synoptics Ltd,  All Rights Reserved
C
      END
