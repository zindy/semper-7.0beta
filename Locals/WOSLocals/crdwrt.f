C Semper 6 processing module CRDWRT
C
      SUBROUTINE CRDWRT
C
C Provides verbs CGET, CPUT:  Reads/Writes Biorad confocal microscope
C images that have originated from a PC based system
C
      LOGICAL FILMAK,FILSEA,FILSTR,FILDCD,OPT,SEMOPN,SEMROW
      INTEGER IVALPN,SEMFRM,LNBLNK
C
      INCLUDE 'COMMON'
C
      INTEGER I,J,K,L,NF,NCOL,NROW,NLAY
C
      LOGICAL*1 ALIGNS(-1:76)
      LOGICAL*1 HEADER(76)
      INTEGER*2 NX,NY,NZ,RAMP0,RMP255,ISBYTE,FILNO,A1NAME(16),A1TEMP(32)
      INTEGER*2 ISMERG,COL,FILEID,LAMP0,LMP255,LCOL,ISEDIT
      INTEGER*2 LENS,DUM(3)
      INTEGER*4 MAGFAC,NOTER
C
      EQUIVALENCE (ALIGNS(1),HEADER(1))
      EQUIVALENCE (NX,HEADER(1)),(NY,HEADER(3)),(NZ,HEADER(5))
      EQUIVALENCE (RAMP0 ,HEADER(7) ),(RMP255,HEADER(9))
      EQUIVALENCE (NOTER ,HEADER(11))
      EQUIVALENCE (ISBYTE,HEADER(15)),(FILNO ,HEADER(17))
      EQUIVALENCE (A1NAME,HEADER(19)),(ISMERG,HEADER(51))
      EQUIVALENCE (COL   ,HEADER(53)),(FILEID,HEADER(55))
      EQUIVALENCE (LAMP0 ,HEADER(57)),(LMP255,HEADER(59))
      EQUIVALENCE (LCOL  ,HEADER(61)),(ISEDIT,HEADER(63))
      EQUIVALENCE (LENS  ,HEADER(65)),(MAGFAC,HEADER(67))
      EQUIVALENCE (DUM   ,HEADER(71))
C
      INTEGER*4 NCHARS
C
      INTEGER FD,IOP
C
      LOGICAL LWRITE,EXISTS
C
      CHARACTER*(FILMAX) PATHNM,FILE,NAME,PREFIX
      CHARACTER*4 EXTEN
C
C     Packed names
C
      INTEGER NCPUT,NTO,NFROM,NNEW
      PARAMETER (NCPUT=5461,NTO=-601,NFROM=10335,NNEW=22623)
C
C     'C' functions
C
      LOGICAL EIKEXI,EIKDEL,EIKOPE,EIKCHA,EIKINT,EIKBYA,EIKCLO
C
      LWRITE = VERB .EQ. NCPUT
C
C     Fetch file name from key NAME, prompting if key absent
C
      IF ( FILSTR ( ' ', FILE, NF, LWRITE ) ) GOTO 130
      IF ( NF .EQ. 0 ) GOTO 130
C
C     Branch on whether reading or writing file
C
      IF ( LWRITE ) THEN
C
C        Build full pathname, and see if file exists
C
         IF ( FILMAK ( FILE, '.pic', PATHNM ) ) GOTO 130
         NF = LNBLNK ( PATHNM )
C
         IF ( EIKEXI ( PATHNM(1:NF), EXISTS ) ) GOTO 130
C
C        If file already exists, delete it if NEW requested, otherwise
C        raise an error.
C
         IF ( EXISTS ) THEN
            IF ( OPT ( NNEW ) ) THEN
               IF ( EIKDEL ( PATHNM(1:NF) ) ) GOTO 70
               EXISTS = .FALSE.
            ELSE
               ERROR  = 135
               IDMESS = PATHNM(1:NF)
               GOTO 130
            ENDIF
         ENDIF
         IOP = 2
      ELSE
C
C        See if file exists on the path if reading
C
         IF ( FILSEA ( FILE(1:NF), '.pic', PATHNM,
     +                 EXISTS ) ) GOTO 130
         IF ( EXISTS ) THEN
            NF  = LNBLNK ( PATHNM )
            IOP = 1
         ELSE
C
C           Error - non-existent file requested for read
C
            ERROR  = 130
            IDMESS = FILE(1:NF)
            GOTO 130
         ENDIF
      ENDIF
C
C     Try to open the file
C
      IF ( EIKOPE ( IOP, FD, PATHNM(1:NF) ) ) GOTO 80
C
C     Switch code on whether reading or writing file header info.
C
      IF ( LWRITE ) THEN
C
C        Code for CPUT
C        -------------
C        Fetch dimensions, etc. from picture
C
         NX   = NCOLS(LP1)
         NCOL = NX
         NY   = NROWS(LP1)
         NROW = NY
         NZ   = NLAYS(LP1)
         NLAY = NZ
         RAMP0  = 0
         RMP255 = 255
         NOTER  = 0
         ISBYTE = 1
         FILNO  = 0
         ISMERG = 0
         COL    = 7
         FILEID = 12345
         LAMP0  = 0
         LMP255 = 255
         LCOL   = 0
         ISEDIT = 0
         LENS   = 10
C
C MAGFAC Should be 1.0 !
C
         MAGFAC = 1065353216
         DUM(1) = 0
         DUM(2) = 0
         DUM(3) = 0
C
         IF (FILDCD(FILE,PREFIX,NAME,EXTEN)) GOTO 120
C
C Build name in A1NAME
C
         K = LNBLNK(NAME)
         IF (K .GT. 32) K = 32
         DO 10 J = K+1, 32
            A1TEMP(I) = 0
   10    CONTINUE
C
         CALL SEMICS(NAME(1:NCHARS),A1TEMP,K)
         NCHARS = K
         CALL CFORM(A1TEMP,A1NAME,NFMINT,NFMBYT,NCHARS)
C
C        Output header line
C
         CALL HEADSW ( HEADER )
         IF ( EIKBYA ( IOP, FD, HEADER, 76 ) )   GOTO 90
C
         DO 30 J = 1,NLAY
            DO 20 I = 1,NROW
               IF ( SEMROW ( 1,RB1,NFMBYT,I,J,LP1 ) ) GOTO 120
               IF ( EIKBYA ( IOP, FD, RB1, NCOL ) ) GOTO 90
   20       CONTINUE
   30    CONTINUE
      ELSE
C
C        Code for CGET
C        -------------
C        Read header line
C
         IF ( EIKBYA ( IOP, FD, HEADER, 76 ) )   GOTO 90
         CALL HEADSW ( HEADER )
         IF ( FILEID .NE. 12345 ) THEN
            ERROR = 77
            IDMESS = 'File is not a valid confocal picture file'
            GOTO 120
         ENDIF
C
C        Open Semper picture
C
         LP1 = 0
         NCOL = NX
         NROW = NY
         NLAY = NZ
         IF ( SEMOPN ( 2, IVALPN(NTO), NCOL, NROW, NLAY, NCLIMA,
     +                 SEMFRM(NFMBYT), LP1 ) ) GOTO 120
         DO 60 J = 1,NLAY
            IF (ISBYTE .NE. 0) THEN
               DO 40 I = 1,NROW
                  IF ( EIKBYA ( IOP, FD, RB1, NCOL ) ) GOTO 90
                  IF ( SEMROW ( 2,RB1,NFMBYT,I,J,LP1 ) ) GOTO 120
   40          CONTINUE
            ELSE
               DO 50 I = 1,NROW
                  IF ( EIKBYA ( IOP, FD, RB1, NCOL+NCOL ) ) GOTO 90
                  CALL IROWSW ( RB1, RB3, NCOL )
                  IF ( SEMROW ( 2,RB3,NFMINT,I,J,LP1 ) ) GOTO 120
   50          CONTINUE
            ENDIF
   60    CONTINUE
      ENDIF
C
      IF ( EIKCLO ( FD ) ) GOTO 100
      GOTO 130
C
C     Deal with UNIX I/O errors
C
   70 IDMESS = 'Error deleting existing file '//PATHNM(1:NF)
      GOTO 110
C
   80 IDMESS = 'Error opening file '//PATHNM(1:NF)
      GOTO 110
C
   90 IF ( IOP .EQ. 1 ) THEN
         IDMESS = 'Error reading file '//PATHNM(1:NF)
      ELSE
         IDMESS = 'Error writing file '//PATHNM(1:NF)
      ENDIF
      ERROR = 77
      GOTO 120
C
  100 IDMESS = 'Error closing file '//PATHNM(1:NF)
C
  110 ERROR = 77
      GOTO 130
C
C     Closing a file after an error (ignore any error on closing file)
C
  120 IF ( EIKCLO ( FD ) ) GOTO 130
C
C     All done
C
  130 RETURN
C
C Copyright (C) 1987-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C Subroutine HEADSW - swap integer packing order in header
C
      SUBROUTINE HEADSW(HEADER)
C
      LOGICAL*1 HEADER(76),HOLD
C
      INTEGER I
C
      DO 10 I = 1,9,2
         HOLD = HEADER(I)
         HEADER(I) = HEADER(I+1)
         HEADER(I+1) = HOLD
   10 CONTINUE
C
      HOLD = HEADER(11)
      HEADER(11) = HEADER(14)
      HEADER(14) = HOLD
C
      HOLD = HEADER(12)
      HEADER(12) = HEADER(13)
      HEADER(13) = HOLD
C
      DO 20 I = 15,65,2
         HOLD = HEADER(I)
         HEADER(I) = HEADER(I+1)
         HEADER(I+1) = HOLD
   20 CONTINUE
C
      HOLD = HEADER(67)
      HEADER(67) = HEADER(70)
      HEADER(70) = HOLD
C
      HOLD = HEADER(68)
      HEADER(68) = HEADER(69)
      HEADER(69) = HOLD
C
      DO 30 I = 71,75,2
         HOLD = HEADER(I)
         HEADER(I) = HEADER(I+1)
         HEADER(I+1) = HOLD
   30 CONTINUE
C
      RETURN
C
C Copyright (C) 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
