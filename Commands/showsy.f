C Semper 6 sub-processing module SHOWSY
C
      LOGICAL FUNCTION SHOWSY()
C
C Prints system wide session status information.
C
      REAL VAL
      LOGICAL SEMTPS,SEMXA1,SHOW0,SEMCON,SHOWLF,SHOWNL
C
      INTEGER I,IT,IW,PTR,SempBuild
C
      CHARACTER*5  VERSTR
      CHARACTER*80 LINE,STRIKE
C
      INTEGER TEXT(80)
C
C Packed name
C
      INTEGER NRIC
      PARAMETER (NRIC=29163)
C
      INCLUDE 'COMMON'
C
      EQUIVALENCE (RB1,TEXT)
C
   10 FORMAT(A,I10,I15,I12)
   20 FORMAT('Number of row buffers: ',I8)
   30 FORMAT('Total row buffer space:',I8,' bytes')
   40 FORMAT(A,':',I5)
C
      SHOWSY = .TRUE.
C
      IF (SEMTPS(IW,IT)) GOTO 50
C
      IF (SHOWLF()) GOTO 50
C
C The following format for the serial number MUST NOT be changed as
C it is required for the Production system serial number patcher
C
C     RECORD = 'Serial Number SIGRXV11004ESY000000'
C     RECORD = 'Semper Build  7.0-Beta01-July-2005'

C      IF (SEMCON(RECORD)) GOTO 50
C     Basic build information
      ISOUT1 = SempBuild(2,LINE)
      ISOUT1=ISOUT1+1
      LINE(ISOUT1:ISOUT1)=' '
      ISOUT1=ISOUT1+1
      ISOUT2 = SempBuild(3,STRIKE)
      LINE(ISOUT1:ISOUT1+ISOUT2-1) =STRIKE(1:ISOUT2)
      ISOUT1=ISOUT1+ISOUT2
      LINE(ISOUT1:ISOUT1)=' '
      ISOUT1=ISOUT1+1
      ISOUT2 = SempBuild(5,STRIKE)
      LINE(ISOUT1:ISOUT1+ISOUT2-1)=STRIKE(1:ISOUT2)
      ISOUT1=ISOUT1+ISOUT2-1
      IF(SEMCON(LINE(1:ISOUT1))) goto 50
      LINE(1:ISOUT1)=' '
C
C     Compilers & Patches
      LINE(1:11) = 'Compilers: '
      ISOUT2 = SempBuild(6,STRIKE)
      LINE(12:11+ISOUT2)=STRIKE(1:ISOUT2)
      ISOUT1 = 12 + ISOUT2
      LINE(ISOUT1:ISOUT1)=' '
      ISOUT1=ISOUT1+1
      ISOUT2 = SempBuild(7,STRIKE)
      ISOUT1 = ISOUT1 + 1
      LINE(ISOUT1:ISOUT1+ISOUT2-1)=STRIKE(1:ISOUT2)
      ISOUT1=ISOUT1+ISOUT2
      LINE(ISOUT1:ISOUT1+8)=' Patches '
      ISOUT1=ISOUT1+9
      ISOUT2 = SempBuild(8,STRIKE)
      LINE(ISOUT1:ISOUT1+ISOUT2-1)=STRIKE(1:ISOUT2)
      ISOUT1=ISOUT1+ISOUT2-1
      IF(SEMCON(LINE(1:ISOUT1))) goto 50
C
C     Byte ordering
      LINE(1:6)='Bytes '
      ISOUT2=SempBuild(1,STRIKE)
      LINE(7:6+ISOUT2) = STRIKE(1:ISOUT2)
      ISOUT1=6+ISOUT2
      IF(SEMCON(LINE(1:ISOUT1))) goto 50
C
C     Installation path
      LINE(1:8)='Install '
      ISOUT2=SempBuild(10,STRIKE)
      LINE(9:8+ISOUT2) = STRIKE(1:ISOUT2)
      ISOUT1=8+ISOUT2
      IF(SEMCON(LINE(1:ISOUT1))) goto 50
C
      PTR = 1
      IF (SEMXA1(4,TEXT,80,PTR,VAL(NRIC),I)) THEN
         PTR = 5
         VERSTR = '*.***'
      ELSE
         PTR = PTR - 1
         CALL SEMCHS(VERSTR,TEXT,PTR)
      ENDIF
C
      RECORD = 'Version number:  '//VERSTR(1:PTR)
      IF (SEMCON(RECORD)) GOTO 50
C
      IF (SHOWNL('Data form  Pixel size  Max. row length   Edge'))
     +   GOTO 50
      IF (SEMCON('             (bytes)      (pixels)      pixels'))
     +   GOTO 50
      WRITE (RECORD,10) 'Byte   ',
     +                  LNBYTE,LNBUF/LNBYTE,LNEDGE*LNREAL/LNBYTE
      IF (SEMCON(RECORD)) GOTO 50
      WRITE (RECORD,10) 'Integer',
     +                  LNINT ,LNBUF/LNINT ,LNEDGE*LNREAL/LNINT
      IF (SEMCON(RECORD)) GOTO 50
      WRITE (RECORD,10) 'Fp     ',
     +                  LNREAL,LNBUF/LNREAL,LNEDGE*LNREAL/LNREAL
      IF (SEMCON(RECORD)) GOTO 50
      WRITE (RECORD,10) 'Complex',
     +                  LNCOMP,LNBUF/LNCOMP,LNEDGE*LNREAL/LNCOMP
      IF (SEMCON(RECORD)) GOTO 50
C
      IF (SHOWLF()) GOTO 50
C
      WRITE (RECORD,20) NNBUF
      IF (SEMCON(RECORD)) GOTO 50
      WRITE (RECORD,30) LNSBUF
      IF (SEMCON(RECORD)) GOTO 50
C
      IF (SHOWNL('Implementation limits:')) GOTO 50
C
C ****** WOS ******
C Some rewording, and MaxProgLabels added
      WRITE (RECORD,40)       '             Devices',NDVS
      IT = 26
      IF (SHOW0(RECORD,IT,26,IW)) GOTO 50
      WRITE(RECORD(IT+1:),40) '  Display partitions',NDPDS
      IT = IT + 26
      IF (SHOW0(RECORD,IT,26,IW)) GOTO 50
      WRITE(RECORD(IT+1:),40) '                LUTs',NLUTS
      IT = IT + 26
      IF (SHOW0(RECORD,IT,26,IW)) GOTO 50
      WRITE(RECORD(IT+1:),40) '      FOR loop depth',FORMAX
      IT = IT + 26
      IF (SHOW0(RECORD,IT,26,IW)) GOTO 50
      WRITE(RECORD(IT+1:),40) '  Library call depth',INPMAX
      IT = IT + 26
      IF (SHOW0(RECORD,IT,26,IW)) GOTO 50
      WRITE(RECORD(IT+1:),40) ' Program name length',PRNACT
      IT = IT + 26
      IF (SHOW0(RECORD,IT,26,IW)) GOTO 50
      WRITE(RECORD(IT+1:),40) '       Open pictures',NLPS
      IT = IT + 26
      IF (SHOW0(RECORD,IT,26,IW)) GOTO 50
      WRITE(RECORD(IT+1:),40) '       Set variables',NVARS
      IT = IT + 26
      IF (SHOW0(RECORD,IT,26,IW)) GOTO 50
      WRITE(RECORD(IT+1:),40) '     Local variables',LOCMAX
      IT = IT + 26
      IF (SHOW0(RECORD,IT,26,IW)) GOTO 50
      WRITE(RECORD(IT+1:),40) '  Look-up table size',LUTLEN
      IT = IT + 26
      IF (SHOW0(RECORD,IT,26,IW)) GOTO 50
      WRITE(RECORD(IT+1:),40) '       Max LUT value',LUTMAX
      IT = IT + 26
      IF (SHOW0(RECORD,IT,26,IW)) GOTO 50
      WRITE(RECORD(IT+1:),40) ' Command buffer size',LNLINB
      IT = IT + 26
      IF (SHOW0(RECORD,IT,26,IW)) GOTO 50
      WRITE(RECORD(IT+1:),40) '      Program labels',LNINDX/4-2
      IT = IT + 26
      IF (SEMCON(RECORD(1:IT))) GOTO 50
C ****** *** ******      
C
      SHOWSY = .FALSE.
C
   50 RETURN
C
C Copyright (C) 1987-1993: Synoptics Ltd,  All Rights Reserved
C
      END
