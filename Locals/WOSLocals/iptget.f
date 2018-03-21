C Semper 6 processing module IPTGET
C
      SUBROUTINE IPTGET
C     =================
C
C Provides verbs IGET, IPUT:  Reads/Writes SG image files.
C Reads/writes pictures from/to a sequential file that is dynamically
C opened.  The file name is supplied by means of the text key NAME.
C
      LOGICAL FILEXI,FILMAK,FILSEA,FILSTR,GETRNG
      LOGICAL OPT,SEMTIT,SEMOPN,SEMROW,RANGE
      LOGICAL IMGOPE,IMGCLO,IMGDEL,IMGROW,IMGNAM,IMGRNG
      INTEGER IVALPN,SEMFRM,LNBLNK,IPACK
C
      INTEGER BPP,FORM,J,K,NF,NPIC,NCOL,NROW,NLAY
      REAL RMIN,RMAX
C
      CHARACTER*80  TITLE
C
      LOGICAL LWRITE,EXISTS
C
      INCLUDE 'COMMON'
C
      CHARACTER*(FILMAX) FILE,PATHNM
C
C
            ERROR = 77
            IDMESS = 'Code not implimented'
C
      END
