c     routine which gets or puts files
C
      SUBROUTINE PGT
C
C
c   Fuhui modified on 10/20/87
c   GET, PUT can be used alternatively: the current block
c   setting is valid to next GET or PUT command.
c   The block number is determined by the following rules:
c       BLK set -- no_file=iblk 
c       REW set -- no_file=1
c       BLK,REW unset -- unit set 1st time -- no_file=1
c                        unit already set  -- no_file=no_file+1
c                                            (next to current block)

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
     +NBLK/3691/,nfrom/10335/,nshow/30735/

C
C INITIALISE
      IU=IVAL(NUNIT)

c     picture number
      np=ival(nfrom)
      show=opt(nshow)
      npic=semppn(np)
      file_open_1=.false.
      blk_set=semlu(-1,nblk,x)
      if(blk_set) iblk=ival(nblk)
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
      file_raw=.true.
      call file_set_semper(file_name,st)
      if(st.ne.0)goto 80

      endif
c
c     if rewind, get or put the 1st block no matter what file state is.
c     don't need to check.
      IF (SEMLU(-1,NREWIN,X))then
      file_st=file_start
      no_file=1
      from_ptr=file_st    
      goto 2000
      endif
C
C READ PICTURE DIMENSIONS
c
c if BLK set, get or put the blk'th block;
c if BLK unset and the file is just open, get or put the 1st block;
c if BLK unset and the file is already open, get or put the next block.
c
      if(.not.blk_set)then
           if(file_open_1)then
                no_file=1
                from_ptr=file_st
           endif
      else
c
c          point to certain block whose previous blocks 
c          may be different sizes !
c
           from_ptr=file_st
           val_point=from_ptr
           no_file=iblk
           if (iblk.eq.1) goto 2000
           do 246 jj=1,iblk-1
           call vec_$copy(val,ib,int4(78))
           if(typeo)then
             from_ptr=from_ptr+312+ib(1)*ib(2)*int4(4)
             if(ib(3).gt.1)
     $       from_ptr=from_ptr+312+ib(1)*ib(2)*int4(4)
             else
             from_ptr=from_ptr+312+ib(1)*ib(2)*int4(2**ib(9))*ib(7)
             endif
246        val_point=from_ptr
      endif
c
2000  val_point=from_ptr
      IOP=1
      IF (VERB.EQ.NPUT) GOTO 1000
      IOP=2

c
C ********************************
C         CODE FOR GET
C ********************************
280   call vec_$copy(val,ib,int4(78))
      vptr=from_ptr
      from_ptr=from_ptr+312
      val_point=from_ptr
      lrow=ib(1)
      nrow=ib(2)

c     trap type
c     old type, ib(4,5,6) month,day,year
c     new type  ib(4,5,6) year,month,day
      typeo=.false.
      if(ib(6).gt.31)typeo=.true.

      if(typeo)then
      if(show)
     $ write(6,250)no_file,ib(1),ib(2),ib(4),ib(5),ib(6)
      month=ib(4)
      iday=ib(5)
      iyear=ib(6)-1900
      else
      if(show)
     $ write(6,250)no_file,ib(1),ib(2),ib(5),ib(6),ib(4)+1900
      month=ib(5)
      iday=ib(6)
      iyear=ib(4)
      endif

250   format(' Image ',i4,' size ',i4,' by ',i4,' created on ',i2,
     $ '/',i2,'/',i4)
      no_file=no_file+1
      iblk=iblk+1

c     if old, 7-36 is title
c     if new then nlay,iclass,iform in 7-9
      if(typeo)then
      if(show)
     $ write(6,260)(ib(j),j=7,36)
      icl=1
      iform=2
      if(ib(3).gt.2)then
           iform=3
      endif
      nlay=1
      ntitle=120
      else
      nlay=ib(7)
      icl=ib(8)
      iform=ib(9)
c     title length in ib(10)
      ntitle=ib(10)
c     title in 11 to ntitle bytes
      nmore=(ntitle/4)
      if(nmore*4.lt.ntitle)nmore=nmore+1
      if(show)
     $ write(6,260)(ib(j),j=11,nmore+11)
      endif

260   format(18a4/' ',14a4)
C
C OPEN PICTURE
c 1000 IF (SEMPIC(IOP,NPIC,LV1,LROW,NROW,ICL))then
 1000 if(semopn(iop,npic,lrow,nrow,nlay,icl,iform,lv1))then
         from_ptr=vptr
         no_file=no_file-1
         return
      endif
C
C SWITCH ON VERB NAME
      IF (VERB.EQ.NPUT) GOTO 500
c
c     copy label data
      if(typeo)then
      if(labme(month,iday,iyear,ib(7),ntitle,
     $ .false.,lv1))then
      write(6,*)'Error writing label'
      return
      endif
      else
      if(labme(month,iday,iyear,ib(11),ntitle,
     $ .false.,lv1))then
      write(6,*)'Error writing label'
      return
      endif
      endif

      lbyte=int4(lrow)*int4(2**iform)
      do 3000 k=1,nlay
      DO 3000 J=1,NROW
      if(semrow(2,val,iform,j,k,lv1))return
      from_ptr=from_ptr+lbyte
      val_point=from_ptr
 3000 CONTINUE
C
      RETURN

C **********************************
C          CODE FOR PUT
C **********************************
c 
c  check if the requested block is already occupied; if yes, then let user
c  choose to over-write or to leave it alone.
c
500   call vec_$copy(val,ib,int4(78))
      if ((ib(1).le.1024.and.ib(1).gt.0).and.
     $(ib(2).le.1024.and.ib(2).gt.0))then
      write(6,590)no_file
590   format(' Block',i4,' already exists, do you want to cover it ?'
     $ / ' > '$)
594   read(5,596)(yon1(j),j=1,20)
596   format(20a1)
      j=1
597   if (yon1(j).eq.' ')then
           j=j+1
           goto 597
      else
           if (yon1(j).eq.'n'.or.yon1(j).eq.'N') return
           if (yon1(j).ne.'y'.and.yon1(j).ne.'Y')then
                write(6,598)
598             format(' Unknown answer, please answer again !'/ ' > '$)
                goto 594
           endif
           no_file=no_file+1
      endif
      else
            goto 4000
      endif
c
c     check next block to see if the updated data will destroy
c     regular block structure -- check 2 things:
c     if current block and updated block have different sizes,
c     if next block exists.
c
      if (ib(1).eq.lrow.and.ib(2).eq.nrow) goto 4000
      vptr=val_point
      if(typeo)then
           val_point=val_point+312+ib(1)*ib(2)*int4(4)
           else
           nl=ib(9)
           val_point=val_point+312+ib(1)*ib(2)*(int4(2)**ib(9))*nl
      endif

      call vec_$copy(val,ib,int4(78))
      if ((ib(1).le.1024.and.ib(1).gt.0).and.
     $(ib(2).le.1024.and.ib(2).gt.0))then
      write(6,790)
790   format(' Next block exists and you will destroy current block '
     $ / ' structure, do you want to cover it ?'
     $ / ' > '$)
794   read(5,796)(yon1(j),j=1,20)
796   format(20a1)
      j=1
797   if (yon1(j).eq.' ')then
           j=j+1
           goto 797
      else
           if (yon1(j).eq.'n'.or.yon1(j).eq.'N') return
           if (yon1(j).ne.'y'.and.yon1(j).ne.'Y')then
                write(6,798)
798             format(' Unknown answer, please answer again !'/ ' > '$)
                goto 794
           endif
           no_file=no_file+1
      endif
      endif
      val_point=vptr
c
c
c     size and class (ib(3) not used)
4000   ib(1)=lrow
      ib(2)=nrow
      ib(3)=1 
      if(labme(month,iday,iyear,ib(11),ntitle,
     $ .true.,lv1))then
      write(6,*)'Error reading label'
      return
      endif
      ib(7)=nlay
      ib(8)=icl
      ib(9)=iform
c     title length in ib(10)
      ib(10)=ntitle
c     date and time
      ib(4)=iyear
      ib(5)=month
      ib(6)=iday
c
c     write label to file
      call vec_$copy(ib,val,int4(78))
      from_ptr=from_ptr+312
      val_point=from_ptr
c
      lbyte=int4(lrow)*int4(2**iform)
      do 700 k=1,nlay
      DO 700 J=1,NROW
      if(semrow(1,val,iform,j,k,lv1))return
c      IF (SEMROW(1,val,J,LV1)) RETURN
      from_ptr=from_ptr+lbyte
      val_point=from_ptr
  700 continue
c
   40 RETURN
C ERRORS
   80 write(6,*)'Error: Invalid unit number or wrong file name'
      Error=58
      RETURN
   70 write(6,*)'Error: file name'
      Error=58
      RETURN
C
C Copyright (C) 1988:  Northwestern University,  All Rights Reserved
C
      END
