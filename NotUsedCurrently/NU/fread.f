c     routine which gets or puts files
C
      SUBROUTINE FREAD
C
C

%INCLUDE '/semper/primitives/common.ins'

c     ldm updated for semper 6 9/26/88

c
      LOGICAL SEMROW,SEMopn,typeo,labme,semlu,varset,semktx
      INTEGER*4 IB(1024)
C
C
      character*80 file_name
      character*1 yon,yon1(20)
      integer*4 file_start,file_st,file_point,val_point,from_ptr,vptr
      integer*4 lbyte
      integer semppn
      dimension val(1024)
      pointer /val_point/val
      common /file_common/file_start,file_st,file_on,file_raw
      logical file_on,file_raw,file_there,lopen,blk_set,file_open_1
      common /file2_common/iu_store
      save no_file,from_ptr, typeo
      logical lname,show,opt
      EQUIVALENCE (RB1,IB1,LABEL),(RB5,A1FILE,A1FORM),(TITLE,RB6)

C PACKED NAMES
      DATA NPUT/26460/,NUNIT/-2170/,NREWIN/29023/,NMAX/20864/,
     +NBLK/3691/,nfrom/10335/,nshow/3075/

C
C INITIALISE
      IU=IVAL(NUNIT)

      file_open_1=.false.
      IF (IU.LE.0) GOTO 80
c
c     see if open
      if(.not.file_on.or.iu.ne.iu_store)then
c
c     close old file if there
      if(file_on)then
      call mclose_f(file_start)
      endif
c
      iu_store=iu
      file_on=.true.
c
c     ask for file_name
      file_name(1:80)=' '
      LNAME = VARSET(22453)
      IF (LNAME) THEN
         NF = 80
         IF (SEMKTX(22453,'File name (as textstring): ',
     +              A1FILE,NF,.FALSE.)) GOTO 70
         IF (NF .EQ. 0) GOTO 70
         CALL SEMCHS(FILE_name,A1FILE,NF)
      file_name(NF+1:80)=' '
      else
      WRITE (6,200) 
200   format('Please enter file name > '$)
      read(5,210)file_name
210   format(a80)
      endif
c
      file_open_1=.true.
      if(blk_set)then
      no_file=iblk
      else
      no_file=1
      endif
230   format(a1)
c
      RETURN
C
C Copyright (C) 1988:  Northwestern University,  All Rights Reserved
C
      END
