c     routine which gets files from Optronics Microdensitometer
C
      SUBROUTINE micro
C
      INCLUDE 'COMMON'
C     ldm updated for semper 7 March 2007
C
      LOGICAL typeo,labme,semlu,varset,semktx
      INTEGER*4 IB(1024)
      INTEGER FD,J,K,N,NF,IXFR,NROW,NLAY,NCOL
      LOGICAL LSWAP
C
C
      character*80 file_name
      character*1024 buffer
      character*1 yon,yon1(20)
      integer*4 lbyte
      integer semppn
      character*4096 val
      integer*4 buff(128)
      logical lname, exists

      CHARACTER*4 DFNAM
C      EQUIVALENCE (RB1,IB1,LABEL),(RB5,A1FILE,A1FORM),(TITLE,RB6)
      EQUIVALENCE (buff,buffer)

      CHARACTER*(FILMAX) FILE,FILENM
C     'C' functions
C
      LOGICAL EIKOPE,EIKBYA,EIKCLO
      INTEGER IPACK,IVALPN,SEMFRM,LNBLNK
      LOGICAL FILSEA,FILSTR,SEMLAB,SEMLNF,SEMOPN,SEMROW
      LOGICAL MRDBIN
      LOGICAL ISMOTOROLA
C     SWAP ?

C INITIALISE
C      IU=IVAL(NUNIT)
C      iu=10
C     picture number
C      np=ival(nfrom)
C      npic=semppn(np)
      LSWAP=.false.
c
      DFNAM = '.IMG'
      CALL INPDEF(DFNAM)
      IF (FILSTR(' ',FILE,NF,.FALSE.)) GOTO 80
      IF (NF .EQ. 0) GOTO 80
C
C     Try to open the file
C
      IF ( EIKOPE ( 1, FD, FILE(1:NF) ) ) GOTO 80
C

C
C READ PICTURE DIMENSIONS
C
      icl=1
      iform=0
      nlay=1
      ntitle=120
C      val_point=file_start
      ntitle=32
      IF  (MRDBIN(FD,512,buffer,iform,LSWAP)  ) goto 803
C     Swap is needed, at least with cygwin

      IF(.not.ISMOTOROLA(JUNK))CALL FROWSW(buff,buff,128)

      lrow=buff(26)
      nrow=buff(27)

 1000 IF (SEMOPN(2,IVALPN(-601),LROW,NROW,NLAY,ICL,
     +             IFORM,LV1)) then
         if( EIKCLO ( FD )   ) goto 802
         return
      endif
C
      IOP=1
      val_point=val_point+512
      do 3000 k=1,1
      DO 3000 J=1,NROW
      if( EIKBYA ( 1, FD, val, lrow ) ) goto 803
      if(semrow(2,val,iform,j,k,lv1))return
 3000 CONTINUE
C
      if( EIKCLO ( FD )   ) goto 802
      RETURN
C
C ERRORS
   70 write(6,*)'Error: file name problem'
      if( EIKCLO ( FD )   ) return
      RETURN
   80 IDMESS = 'Error opening file '//FILENM(1:NF)
      ERROR = 77
      if( EIKCLO ( FD )   ) return
      return
  802 IDMESS = 'Error closing file '//FILENM(1:NF)
      ERROR = 77
      return
  803 IDMESS = 'Error reading file '//FILENM(1:NF)
      ERROR = 77
      if( EIKCLO ( FD )   ) goto 802
      RETURN
      
C
C Copyright (C) 1988, 2007:  Northwestern University,  All Rights Reserved
C
      END
