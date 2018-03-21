c     routine which gets files from a numis write
C
      SUBROUTINE picget2
C
      INCLUDE 'COMMON'
C     ldm updated for semper 7 March 2007
C
      LOGICAL typeo,labme,semlu,varset,semktx
      INTEGER*4 IBB(1024*1024)
      INTEGER FD,J,K,N,NF,IXFR,NROW,NLAY,NCOL
      LOGICAL LSWAP
C
C
      character*80 file_name
      character*1024 buffer
      character*1 yon,yon1(20)
      integer*4 lbyte
      integer semppn
      integer*4 buff(78)
      logical lname, exists

      CHARACTER*4 DFNAM
C      EQUIVALENCE (RB1,IB1,LABEL),(RB5,A1FILE,A1FORM),(TITLE,RB6)
      EQUIVALENCE (buff,buffer)

      CHARACTER*(FILMAX) FILE,FILENM
C     'C' functions
C
      LOGICAL EIKOPE,EIKBYA,EIKCLO, EIKLIN
      INTEGER IPACK,IVALPN,SEMFRM,LNBLNK
      LOGICAL FILSEA,FILSTR,SEMLAB,SEMLNF,SEMOPN,SEMROW
      LOGICAL SEMCLS,MRDBIN
      LOGICAL ISMOTOROLA, DOSWAP
C     SWAP ?

C INITIALISE
C     Number to get
      NPIC=ival(23253)
      if(NPIC .lt. 1)NPIC=1
      LSWAP=.true.
c
      DFNAM = '.img'
      CALL INPDEF(DFNAM)
      IF (FILSTR(' ',FILE,NF,.FALSE.)) GOTO 80
      IF (NF .EQ. 0) GOTO 80
C
C     Try to open the file using semperise
C
      IF ( EIKOPE ( 1, FD, FILE(1:NF) ) ) GOTO 80
      if( EIKCLO ( FD )   ) goto 80
C
C     Good old Fortran
      open(unit=31,file=FILE(1:NF),form='unformatted')
C
C     LOOP OVER PICTURES
      IPIC=IVALPN(-601)-1
      DO IDO=1,NPIC
      IPIC=IPIC+1
C
C READ PICTURE Header (as bytes)
C
      icl=1
      iform=0
      nlay=1
      ntitle=32
      read(31,err=803,end=803)buff
C     IF  (MRDBIN(FD,324,buffer,iform,LSWAP)  ) goto 803
      lrow=buff(1)
      nrow=buff(2)
      iform=buff(3)
      icl=1
      nlay=1
 1000 IF (SEMOPN(2,IPIC,LROW,NROW,NLAY,ICL,
     +             IFORM,LV1)) then
         if( EIKCLO ( FD )   ) goto 802
         return
      endif
C
      IOP=1
      LR=LROW
      if((IFORM .eq. 1).or.(IFORM .eq. 2)) LR=LR*4
      if(IFORM .eq. 3)LR=LR*8
      LR4=LR/4
      read(31,err=803,end=803)(ibb(jj),jj=1,LR4*NROW)

      do 3000 k=1,nlay
      DO 3000 J=1,NROW
      JJ=(J-1)*LR4 +1
C     IF  (MRDBIN(FD,LR,ib,0,LSWAP)  ) goto 803
C     IF(DOSWAP)CALL FROWSW(ib,ib,1024)
      if(semrow(2,ibb(JJ),iform,j,k,lv1))return
 3000 CONTINUE
C
C     Close the picture ??
C     Close picture after reading (Jim)
      DO I=1,NLPS
         IF (SEMCLS(I)) RETURN
      enddo
C     END LOOP OVER PICTURES
      ENDDO
C
C     if( EIKCLO ( FD )   ) goto 802
      close(31)
      RETURN
C
C ERRORS
   70 write(6,*)'Error: file name problem'
      close(31)
      if( EIKCLO ( FD )   ) return
      RETURN
   80 IDMESS = 'Error opening file '//FILENM(1:NF)
      ERROR = 77
      close(31)
C      if( EIKCLO ( FD )   ) return
      return
  802 IDMESS = 'Error closing file '//FILENM(1:NF)
      ERROR = 77
      return
  803 IDMESS = 'Error reading file '//FILENM(1:NF)
      ERROR = 77
C      if( EIKCLO ( FD )   ) goto 802
      CLOSE(31)
      RETURN
      
C
C Copyright (C) 1988, 2007:  Northwestern University,  All Rights Reserved
C
      END


