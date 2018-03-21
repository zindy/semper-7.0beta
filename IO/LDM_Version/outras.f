C Semper 6 processing module OUTRAS
C
      SUBROUTINE OUTRAS(LNEW)
      LOGICAL LNEW
C
C Provides commands OUTPUT RASTER:
C  Writes pictures to a SunRaster format file that is
C  dynamically opened.
C
      INTEGER IVALPN,LNBLNK !,IPACK
      LOGICAL FILMAK,FILSEA,FILSTR,MWRBIN,OUTNEW,SEMLNF,SEMROW
C
      INCLUDE 'COMMON'
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
      INTEGER FD,I,J,K,IXFR,NF,NROW,NLAY,NCOL
      INTEGER CLASS,FORM
C
      CHARACTER*4 DFNAM
      CHARACTER*(FILMAX) FILENM,FILE
C
      INTEGER IB1(LNBUF/LNINT),IB6(LNBUF/LNINT)
C
      LOGICAL EXISTS
C
      EQUIVALENCE (RB1,IB1),(RB6,IB6,RASFIL)
C
C     'C' functions
C
      LOGICAL EIKOPE,EIKCLO
C
C Fetch dimensions, etc.
C
      NCOL = NCOLS(LP1)
      NROW = NROWS(LP1)
      NLAY = NLAYS(LP1)
      CLASS= CLASSN(LP1)
      IF (CLASS .NE. NCLIMA) GOTO 60
      FORM = FORMN(LP1)
C
      IF (SEMLNF(FORM,I)) GOTO 80
      IF (FORM .EQ. NFMINT) I = 2
      IXFR = I * NCOL
C
      DFNAM = '.rff'
C
C     Fetch file name from key NAME, prompting if key absent
C
      CALL OUTDEF(DFNAM)
      IF (FILSTR(' ',FILE,NF,.TRUE.)) GOTO 90
      IF (NF .EQ. 0) GOTO 90
C
C     Build full pathname, and see if file exists
C
      IF ( FILMAK ( FILE, DFNAM, FILENM ) ) GOTO 90
      NF = LNBLNK ( FILENM )
      FILE = FILENM(1:NF)
C
      IF ( FILSEA ( FILENM(1:NF), DFNAM, FILE, EXISTS ) ) GOTO 90
C
C     If file already exists, delete it if NEW requested, otherwise
C     raise an error.
C
      IF ( EXISTS ) THEN
         IF ( OUTNEW ( LNEW, FILE, FILENM(1:NF) ) ) GOTO 90
      ENDIF
C
C     Try to open the file
C
      IF ( EIKOPE ( 2, FD, FILENM(1:NF) ) ) GOTO 30
C
C     Output header line
C
      RMAGIC = RASMGC
      RWIDTH = NCOL
      RHITE  = NROW
      RDEPTH = 8
      RLENG = RWIDTH * RHITE
      RTYPE = RTSTD
      RMAPT = RMTNON
      RMAPL = 0
C
C     Write header
C
      IXFR = 32
      IF ( MWRBIN ( FD,32,IB6,NFMFP,.TRUE. ) ) GOTO 70
      FORM = NFMBYT
      IXFR = NCOL
C
C     Loop over layers
C
      DO 20 K=1,NLAY
C
C        Loop over rows
C
         DO 10 J=1,NROW
C
C           Read source row from LP1
C
            IF (SEMROW(1,RB1,FORM,J,K,LP1)) GOTO 80
C
C           Write source row to file. Straight output can be used
C           as we don't write encoded rasters at present.
C
            IF (MWRBIN(FD,IXFR,IB1,FORM,.TRUE.)) GOTO 70
   10    CONTINUE
   20 CONTINUE
C
C     Close file
C
      IF ( EIKCLO ( FD ) ) GOTO 40
      GOTO 90
C
C     Deal with EIKxxx errors - first those where no EIKCLO is required
C
   30 IDMESS = 'Error opening file '//FILENM(1:NF)
      GOTO 50
C
   40 IDMESS = 'Error closing file '//FILENM(1:NF)
   50 ERROR = 77
      GOTO 90
C
C     Errors where a EIKCLO should be attempted
C
C     Wrong class for output
C
   60 ERROR = 6
      IDERR = IVALPN(10335)
      GOTO 80
C
   70 IDMESS = 'Error writing file '//FILENM(1:NF)
      ERROR = 77
C
C     Closing a file after an error (ignore any error on closing file)
C
   80 IF ( EIKCLO ( FD ) ) GOTO 90
C
C     All done
C
   90 RETURN
C
C Copyright (C) 1990-1996 Synoptics Ltd,  All Rights Reserved
C
      END
