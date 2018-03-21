C
C  Copyright (c) 2005 L. D. Marks
C
C  This program is free software; you can redistribute it and/or modify
C  it under the terms of the GNU General Public License as published by
C  the Free Software Foundation; either version 2 of the License, or
C  (at your option) any later version.
C
C  This program is distributed in the hope that it will be useful,
C  but WITHOUT ANY WARRANTY; without even the implied warranty of
C  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C  GNU General Public License for more details.
C
C Semper VI local pput
C
      SUBROUTINE PPUT                                  

C
C
C     Change: LDM, July 2005
C     Ensure that gfortran does not think of range, unpack as intrinsics
      EXTERNAL RANGE, UNPACK
      LOGICAL SEMOPN,semrow,varset,semktx,lname,range
C
      INCLUDE 'COMMON'
      EQUIVALENCE (RB1,IB1,LABEL),(RB5,A1FILE,A1FORM),(TITLE,RB6)
C
C Packed names
      PARAMETER (nfrom=10335, npget=25885,npcol=25735,nwid=-5165)
      parameter (nred=29004, ngreen=11925, nblue=3701)
      character*1 cbuf(2048),cbuf2(2048),cbuf3(2048)
      character*128 commline
      character*80 file_name
      integer pix(3)
      logical semlu,pcol,opt
C
      np=ivalpn(nfrom)
c     pseudocolor option
      pcol=opt(npcol)
c     Red, Green and Blue values
        ir=255.*val(nred)
        ig=255.*val(ngreen)
        ib=255.*val(nblue)
        if(ir.le.20)ir=255
        if(ig.le.20)ig=210
        if(ib.le.20)ib=100
      wid=val(nwid)
      if(wid.lt.1.5)wid=1.5
      
      
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
200   format('Please enter file name > ',$)
      read(5,210)file_name
210   format(a80)
      nf=80
      endif

c      IF (VERB.EQ.NPGET) GOTO 1000
c
C       Open picture
        IF (SEMOPN(1,np,NCOL,NROW,nlay,iclass,iform,LP1)) RETURN
C Establish intensity range for region of picture
         IF (RANGE(1,LP1)) return
C
C Fault zero intensity range
         IF (VMIN.EQ.VMAX) THEN
            ERROR=12
            IDERR=NP
            return
         ENDIF
C
C Set up coefficients to transform intensity range
         A=255./(VMAX-VMIN)
        open(unit=11,file='magiktmp.miff')
        write(11,1)ncol,nrow
1       format('id=ImageMagick columns=',i4,' rows=',i4,' :'/,$)
c     read everything from the input picture
      iform=2
      do j=1,nrow
           if(semrow(1,rb1,iform,j,1,LP1)) goto 100
           do k=1,ncol
                p=(rb1(k)-vmin)*a
                if(p.lt.1e-2)then
                        cbuf(k)=char(0)
                else if(p.gt.255)then
                        cbuf(k)=char(255)
                else
                        cbuf(k)=char(nint(p))
                endif
                if(pcol)then
                        ipix=ichar(cbuf(k))
                        call pcolor(ipix,pix,wid,ir,ig,ib)
                        cbuf(k)=char(pix(1))
                        cbuf2(k)=char(pix(2))
                        cbuf3(k)=char(pix(3))
                endif
                                
           enddo
           if(pcol)then 
           write(11,3)(cbuf(k),cbuf2(k),cbuf3(k),k=1,ncol)
           else
           write(11,3)(cbuf(k),cbuf(k),cbuf(k),k=1,ncol)
           endif
           
3          format(64A1,$)
      enddo
      close(unit=11)
      do j=1,128
      commline(j:j)=' '
      enddo
      if(pcol)then
      commline(1:38)='convert -colorspace RGB  magiktmp.miff'
      else
      commline(1:38)='convert -colorspace GRAY magiktmp.miff'
      endif
      commline(40:119)=file_name(1:80)
      ifail=system(commline)
      write(6,*)commline(1:119)
C      ifail=system('rm magiktmp.miff')
      return
C
1000  continue
      do j=1,128
      commline(j:j)=' '
      enddo
      commline(1:25)='convert -colorspace GRAY '
      commline(26:105)=file_name(1:80)
      commline(107:119)='magiktmp.miff'
      ifail=system(commline)
c     read everything from the input picture
      open(unit=11,file='magiktmp.miff')
c     search for the ':' terminator
      do j=1,30
      read(11,4)commline
4       format(a)
        k=1
        do kk=1,107
        k=k+1
        if(commline(k:k+4).eq.'rows=')then
                read(commline(k+5:k+9),*)nrow
                k=k+5
                endif
        if(commline(k:k+7).eq.'columns=')then
                read(commline(k+8:k+12),*)ncol
                k=k+8
                endif
        if(commline(k:k).eq.':')goto 300
        enddo
      enddo
300   nlay=1
      iclass=0
      iform=0  
      IF (SEMOPN(2,np,NCOL,NROW,nlay,iclass,iform,LP1)) RETURN
      do j=1,nrow
           iform=0
           read(11,3)(cbuf(k),cbuf(k),cbuf(k),k=1,ncol)
           if(semrow(2,cbuf,iform,j,1,LP1)) goto 100
      enddo
      close(unit=11)
c      ifail=system('rm magiktmp.miff')
C
100   RETURN
   70 write(6,*)'Error: file name'
      Error=58
      RETURN
      END


      subroutine pcolor(ipix,pix,xwid,ir,ig,ib)
      INTEGER pix(3)
C
                wid=255./xwid
                wid=wid*wid
                x1=ipix-35
                x2=(ipix-255.*0.5-35)
                x3=255-ipix
                pix(3)=exp(-x1*x1/wid)*ib
                pix(2)=exp(-x2*x2/wid)*ig
                pix(1)=exp(-x3*x3/wid)*ir
        
        do j=1,3
                if(pix(j).lt.0)pix(j)=0
                if(pix(j).gt.255)pix(j)=255
        enddo
      return
      end

