c     routine which reads titles from a get/put file
C
      SUBROUTINE inq
C
c     ldm updated for semper 6 9/26/88

c
      LOGICAL SEMROW,SEMopn,typeo,labme,semlu
      INTEGER*4 IB(1024)
C
C
      character*80 file_name
      character*1 yon,yon1(20)
      integer*4 file_start,file_st,file_point,val_point,from_ptr,vptr
      integer*4 nrow,nlay,iform,lrow
      integer*4 lbyte
      integer semppn
      pointer /val_point/ib
      common /file_common/file_start,file_st,file_on,file_raw
      logical file_on,file_raw,file_there,lopen,blk_set,file_open_1
      common /file2_common/iu_store
      save no_file,from_ptr, typeo

C PACKED NAMES
      DATA NPUT/26460/,NUNIT/-2170/,NREWIN/29023/,NMAX/20864/,
     +NBLK/3691/,nfrom/10335/

C
C INITIALISE
      IU=IVAL(NUNIT)

c     picture number
      npic=ival(nfrom)
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
      WRITE (6,200) 
      read(5,210)file_name
210   format(a80)
c
      file_open_1=.true.
230   format(a1)
c
      file_raw=.true.
      call file_set_semper(file_name,st)
      if(st.ne.0)goto 80

      endif
c
      no_file=0
      val_point=file_start
C
C READ PICTURE DIMENSIONS
c
10    continue
d     write(6,*)npic,no_file
      lrow=ib(1)
      nrow=ib(2)
      no_file=no_file+1
c     trap type
c     old type, ib(4,5,6) month,day,year
c     new type  ib(4,5,6) year,month,day
      typeo=.false.
      if(ib(6).gt.31)typeo=.true.

      if(no_file.ne.npic)then
      if (typeo)then
      val_point=val_point+lrow*int4(4)*nrow+312
      else
      iform=ib(9)
      lbyte=int4(lrow)*int4(2**iform)*nrow*ib(7)
      val_point=val_point+lbyte
      endif
      goto 10
C
      else
C
      if(typeo)then
      write(6,250)no_file,ib(1),ib(2),ib(4),ib(5),ib(6)
      write(6,260)(ib(j),j=7,36)
      else
      write(6,250)no_file,ib(1),ib(2),ib(5),ib(6),ib(4)+1900
c     title length in ib(10)
      ntitle=ib(10)
c     title in 11 to ntitle bytes
      nmore=(ntitle/4)
      if(nmore*4.lt.ntitle)nmore=nmore+1
      write(6,260)(ib(j),j=11,nmore+11)
      endif
C
      endif

      return

   80 write(6,210)'Error: Invalid unit number'
      RETURN
C
200   format('Please enter file name > '$)
250   format(' Image ',i3,' size ',i4,' by ',i4,' created on ',i2,
     $ '/',i2,'/',i4)
260   format(18a4/' ',14a4)
C
      END
