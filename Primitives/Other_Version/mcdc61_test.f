C     Semper 6 primitive proving program for MCDC61
C
C     Performs randomly ordered write on 100 'tracks' of CBSIZE blocks each,
C     storing track number in elements 1,11,21.. of the data buffer, and
C     verifies these in a (differently) randomly ordered read on all tracks.
C     For testing caching installations, simply set CBSIZE appropriately;
C     for non-caching installations, test with CBSIZE set to 1 and to some
C     other greater value as well.
C
      program mcdc61_test
      integer tty1,tty2
      integer*4 dsize,i4add_r,i40
      character*1 delflg
      parameter (i40=0)
C
C ****** CHANGE ******
C     Number of bytes in standard INTEGER datatype, logical block length
C     in bytes, and cache buffer length in blocks  (NB: in Semper itself
C     CBSIZE is in common, not a parameter; if the primitive is trying to
C     accommodate variations in CBSIZE, it may be necessary to include the
C     standard common blocks here, changing the parameter name, and assign
C     the initial value.
C
c      parameter (lnint=2,lnblk=64,lnblk4=lnblk,cbsize=120)
       integer tcbsize
       parameter (tcbsize=120)
       include 'COMMON'
C
C     tty1,2 are the ttyinal input,output units respectively
	parameter(tty1=5,tty2=6)
C
C
C     Maximum number of devices to be tested simultaneously
C
      parameter (nddvs=4)
C     ****** ****** ******
C
      logical errflg
      integer add_r,wbase(10),rbase(10)
      integer name(80,nddvs)
c
      parameter (len=lnblk/lnint*tcbsize)
      integer b(len),bb(len)
      integer*2 IOP,IDEV,IBUFF,NUM,NDEV,MODE
c
      data wbase/0,8,7,4,2,1,6,3,9,5/
      data rbase/8,3,6,9,2,4,5,1,7,0/
c
      cbsize=tcbsize
      IOP=0
      IDEV=0
      call mcdc61(IOP,IDEV,lnblk4,cbsize,b,error)
      if (error.ne.0) then
         write (tty2,10) error
10       format (' ERROR',I4,' reported on initialising')
C         stop
      endif
c
c     Fetch disc file name and reformat
c
      ndev=0
20    ndev=ndev+1
      errflg=.false.
      write (tty2,30)
30    format (' Enter a (scratch!) disc file name (RETURN to quit): ')
      read (tty1,40) (name(i,ndev),i=2,80)
40    format (80a1)
      call a1conv(name(2,ndev),79)
      n=80
50    if (name(n,ndev).ne.32) goto 60
      n=n-1
      if (n.ne.1) goto 50
c
C     Name blank: deassign and quit
c
      ndev=ndev-1
      goto 200
60    name(1,ndev)=n-1
C
C     Assign disc file
C
      dsize=100*cbsize
      error=0
      IOP=5
      NUM=0
      call mcdc61(IOP,ndev,dsize,NUM,name(1,ndev),error)
      if (error.ne.0) then
         write (tty2,70) error
70       format (' ERROR',I4,' reported on assigning file')
         stop
      endif
C
C     Random writing pattern initialises disc
c
      write (tty2,80)
80    format (' Write 100 tracks in random order')
      do 110 nc=1,10
      do 110 no=1,10
         add_r=cbsize*(nc+wbase(no)*10-1)+1
         i4add_r=add_r
C        ..in case installation needs to read disc before writing..
         error=0
C         IOP=1
C         call mcdc61(IOP,ndev,i4add_r,cbsize,b,error)
c         call print_out(int2(1),b,cbsize)
C         if(error.ne.0)then
C            write (tty2,130) error,add_r
C            errflg=.true.
C         endif
         do 90 i=1,len,10
90       b(i)=add_r
c         print *,'Write data from buffer to idev'
c         print *,'The original data is:'
c         write(tty2,95)len
c95       format('buffer size=',i8)
c         write(tty2,96)(b(i),i=1,len)
c96       format(20i8)
         error=0
         IOP=2
         call mcdc61(IOP,ndev,i4add_r,cbsize,b,error)
         IOP=1
         call mcdc61(IOP,ndev,i4add_r,cbsize,bb,error)
c         call print_out(int2(2),bb,cbsize)
         if (error.ne.0) then
            write (tty2,100) error,add_r
100         format (' ERROR',I3,' on writing at block',I7,'ff')
            errflg=.true.
         endif
110   continue
C
C     Similar, but different, reading pattern
c
      write (tty2,120)
120   format (' Recover and check tracks in (different) random order')
      do 180 nc=1,10
      do 170 no=1,10
         add_r=cbsize*(nc+rbase(no)*10-1)+1
         error=0
         i4add_r=add_r
         IOP=1
         call mcdc61(IOP,ndev,i4add_r,cbsize,b,error)
c         call print_out(int2(1),b,cbsize)
         if (error.ne.0) then
            write (tty2,130) error,add_r
130         format (' ERROR',I3,' on reading at block',I7,'ff')
            errflg=.true.
         endif
         do 140 i=1,len,20
            if (b(i).ne.add_r) goto 150
140      continue
         goto 170
150      write (tty2,160) add_r,i
160      format (' Data error at block',I4,'ff, element',I7)
         errflg=.true.
170   continue
180   continue
C
C     Verify successful i/o
c
      if (.not.errflg) write (tty2,190)
190   format (' Device tested without errors')
      goto 20
C
C     Deassign
c
200   write (tty2,210)
210   format (' Delete on deassigning? (Y/RETURN)')
      read (tty1,220) delflg
220   format (a)
      if (delflg.eq.'y'.or.delflg.eq.'Y') then
         mode=7
      else
         mode=6
      endif
      do 240 n=1,ndev
         error=0
         num=0
         call mcdc61(mode,n,i40,num,name(1,n),error)
         if (error.ne.0) then
            write (tty2,230) error,n
230         format (' ERROR',I4,' reported on deassigning device',I3)
            errflg=.true.
         endif
240   continue
C
C     Verify completion
c
      if (.not.errflg) write (tty2,250)
250   format (' Test completed without errors')
      stop
      end
c
c     printing out the result in buffer
c
      subroutine print_out(iop,buf,size)
c
      integer*2 iop
      integer*4 buf(*)
      integer*4 size
c
      if(iop.eq.1)then
         print *,'Read data from idev to buffer'
      else
         print *,'Write data from buffer to idev'
      endif
      write(6,10)size
10    format('buffer size=',i8)
      write(6,15)(buf(i),i=1,size)
15    format(20i8)
C
C Copyright (C) 1987,1988:  Synoptics Ltd,  All Rights Reserved
C
      end
C      subroutine semini
C      write(6,*)'Got to semini somehow'
C      call exit(0)
C      end
      subroutine semain
      write(6,*)'Got to semain somehow'
      call exit(0)
      end
 
