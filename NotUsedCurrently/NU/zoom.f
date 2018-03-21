      subroutine zoom
      logical mapfile,map_init,dummy
      character*40 file_name
c
c
      integer*4 st,display_id2
      integer*2 size(2)
      integer*2 window(4),hi_plane,pane_id2
      logical unobs
      logical created,disc_on
      integer*2 origin(2),pos(2)
c
c     display paramters
%include '../primitives/display_common.ins'
c
      Parameter (NTIMES=-374)
      Parameter (NLEFT=19406,NRIGHT=29167,NUP=-2241,NDOWN=7023)
c
      integer plot
c	Not Implimented as yet
	write(6,*)'Zoom not implimented'
	ijunk=-1
	if(ijunk.lt.0)return
c     Initialise
      itim=ival(ntimes)
      plot=ival(26095)
      if(itim.lt.2)itim=2
      if(itim.gt.16)itim=16
      ir=ival(nright)-ival(nleft)
      id=ival(ndown)-ival(nup)

      i1=256/itim
      i2=itim*i1
c
c     check against overflow
      if(256-ir.lt.i1)then
      ir=256-i1
      else if(ir.lt.-i1)then
      ir=-i1
      endif
      if(256-id.lt.i1)then
      id=256-i1
      else if(id.lt.-i1)then
      id=-i1
      endif
c     Bitmap size
      size(1)=int2(512)
      size(2)=int2(512)    
c
c      disp_acq=gpr_$acquire_display(st)
c     set raster ops
      do j=0,7
c      call gpr_$set_raster_op(int2(j),int2(3),st)
      enddo
c
      jj=-itim
      window(1)=int2(256-i1+ir)
      window(3)=int2(512/itim)
      window(4)=int2(1)
      pos(1)=int2(768-i1)

      if(plot.eq.2)then
      pos(1)=pos(1)-512
      window(1)=window(1)+512
      endif

      do 99 j=1,512/itim
      jj=jj+itim
      window(2)=int2(256-i1+j-1-id)
      pos(2)=int2(jj)
      do jk=1,itim
c      call gpr_$pixel_blt(display_id,window,pos,st)
      pos(2)=pos(2)+int2(1)
      if(st.ne.0)then
      if(j.gt.512/itim)goto 992
      call error_pfm(st)
      error=40.
      goto 1000
      endif
      enddo
99    continue
c
992   jj=-itim
      window(2)=int2(0)
      window(3)=int2(1)
      window(4)=int2(512)
      pos(2)=int2(0)
      ii=768
      if(plot.eq.2)ii=256
      do 991 j=0,i1
      jj=jj+itim
      if(j.gt.i1)goto 993
      window(1)=int2(ii-i1+j)
      pos(1)=int2(ii-i2+jj-itim+1)
      do jk=1,itim
c      call gpr_$pixel_blt(display_id,window,pos,st)
      pos(1)=pos(1)+int2(1)
      enddo
c
      window(1)=int2(ii+i1-j+1)
      pos(1)=int2(ii+i2-jj+1)
      pos(2)=int2(0)
      do jk=1,itim
c      call gpr_$pixel_blt(display_id,window,pos,st)
      pos(1)=pos(1)+int2(1)
      enddo

991   continue
c
993   continue
c
1000  disp_acq=.false.
c
c      call gpr_$force_release(ic,st)
      disp_borrowed=.false.
      return
C
C Copyright (C) 1986:  Northwestern University,  All Rights Reserved
C
      end

