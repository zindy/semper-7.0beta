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
      subroutine ice
C  Purpose
C        Image Compression and Expansion
C        To convert segmented images where there are signficant areas
C        of constant grey level into a compressed RUN LENGTH ENCODED
C        format as a standard SEMPER image of UNDEFINED class.
C        The command can be used to re-expand image.  Conversion is
C        exact, but only valid for BYTE and INTEGER images, and
C        image with a row length less than (or equal to) 2048 pixels
C
C        The default is compression.  The verify option will
C        indicate the amount of space saved by compression (this
C        does not count the header and trailer bytes)
C
C Syntax:
C
C  ice    :ICE    >$r3 compress expand verify
C
C          Author:   N.K. Tovey with modifications by M.W. Hounslow
C                    and J. Wang
C This version has been tested on SEMPER 6.4 SYNAPSE and SYENERGY
C                                 SEMPER 6.3 VGA
C                       WINDOWS   SEMPER 6.4
C                                 SEMPER 6.4 SPRYNT
C                SUN Workstation  SEMPER 6.3 SunView
      
c      implicit none **** IS THIS NEEDED
       INCLUDE 'COMMON'
      integer i,j,k,l,m,n,line(4096),line2(4096),iform
      logical semrow,semopn,semlab,comp,exp,opt,SEMCON,verify
      real val,p
      integer class,ncomp,nexp,npic,npic2,nfrom,nto,npic3,nver
      integer nlay2,nrow2,ncol2
cs      integer*4 mrow2
      parameter(ncomp=5413, nexp=8976,nfrom=10335,nto=-601,nver=-3419)
      EQUIVALENCE(RB1,LINE(1)),(RB2,LINE2(1))

C   first decode the options
      verify=opt(nver)
      comp=opt(ncomp)
      exp=opt(nexp)
      if(.not.exp)then
          comp=.true.
      endif
      if(comp.and.exp)then
         error=60
         return
      endif
      npic=ivalpn(nfrom)
      npic2=ivalpn(nto)
      if(comp)then
         if(semlab(1,line2,lp1))return
         iform=line2(20)
         if(iform.gt.1.or.nrows(lp1).gt.4096)then
c MY VERSION OF SEMPER DOES NOT HAVE semcon
            write(6,*)' ICE can only be used for BYTE/',
     #  'INTEGER images and row length <= 4096'
c            if(semcon(record))return
            error=10
            return
         endif
         ncol2=lnblk
C next lines are inserted to make routine universal and avoid use of INT4
C needed for some compilers a row length of LNBLK is used except for large
C images where it is necessary to increase row length
C Note temporary image can only have a maximum of 32767 rows
         i=int(float(ncols(lp1))/float(lnblk))
         j=int(float(nrows(lp1))/float(lnblk))
         if(i*j.gt.250)then
            k=nint(float(i*j)/250.0)
            ncol2=nint(float(i*j)/250.0)*lnblk
            if(i*j.ne.k*250)then
               write(6,*)ncol2,' Old ncol2'
               ncol2=ncol2+lnblk
            endif
         endif
c MY COMPILERS COMPLAINS THAT monit IS NOT A LOGICAL   
         if(monit)then   
            write(6,*)' i,  j,  k,  ncol2 ',i,j,k,ncol2
            if(semcon(record))return
         endif
               
cs         mrow2=2*int4(NCOLS(LP1))*int4(NROWS(LP1))/int4(ncol2)
cs         nrow2=int2(mrow2)
       
C next line is test to see if unique file can be written for PCs
           nrow2=2*int(float(ncols(lp1))*float(nrows(lp1))
     #         /float(ncol2)+0.5)
         if(MONIT)THEN   
            WRITE(6,*)' Compressing image ',NPIC
            IF(SEMCON(RECORD))RETURN
         ENDIF
         if(semopn(3,npic3,ncol2,nrow2,1,nclund,iform,lp2))return
         l=1
         k=1
         j=1
         do 200 i=1,NROWS(LP1)
            if(semrow(1,line,nfmint,i,1,lp1))RETURN
            m=line(1)
            l=1
            DO 300 N=2,NCOLS(LP1)
               if(line(n).ne.m.or.l.eq.255.OR.N.EQ.NCOLS(LP1))then
                  if(n.eq.NCOLS(LP1).and.line(n).eq.m)then
                        l=l+1
C warning if l.eq.255.and.n.eq.NCOLS(LP1) then l = 256 and is stored as 0
                  endif 
                  line2(k)=l
                  line2(k+1)=m
                  k=k+2
                  if(k.gE.NCOL2)then  
                     if(semrow(2,line2,nfmint,j,1,lp2))return
                     j=j+1
                     k=1
                  endif
                  if(n.eq.NCOLS(LP1).and.line(n).ne.(m))then
                     line2(k)=1
                     line2(k+1)=line(NCOLS(LP1))
                     k=k+2 
                     if(k.gE.NCOL2)then  
                        if(semrow(2,line2,nfmint,j,1,lp2))return
                        j=j+1
                        k=1
                     endif
                  endif
                  m=line(n)
                  l=0
               endif
               l=l+1
300         continue
200      continue
         do 400 n=k,NCOL2
            line2(n)=0
400      continue
         if(semrow(2,line2,nfmint,j,1,lp2))return
      p=100.0*(1.0-float(j)*float(ncol2)/(float(NROWS(LP1))*
     + float(NCOLS(LP1))))
         IF(verify.and.abs(p).lt.100)THEN
          write(6,*)'Size attempted ',ncol2,j
          WRITE(6,*)
     +    ' Percentage saving from data compression = ',P,' %'
          IF(SEMCON(RECORD))RETURN
         ENDIF
         if(p.lt.0)then
             WRITE(6,*)
     +       ' No reduction in storage possible - abandoned' 
             IF(SEMCON(RECORD))RETURN
            error=10
            return
         endif
         if(semopn(2,npic2,ncol2,j,1,nclund,iform,lp3))return
         do 500 i=1,j
            if(semrow(1,line2,nfmint,i,1,lp2))return
            if(semrow(2,line2,nfmint,i,1,lp3))return
500      continue
         if(semlab(1,line2,lp1))return
         if(semlab(1,line,lp3))return
C  the following are stored in the header label of the compressed image         
C  Synoptics have indicated that there are no plans to use this space at the
C  moment
C  positions 90 and 91 store no of columns, 92 and 93 no of rows
C  94 no of layers,  95 form of image (INT and BYTE) only
         line(90)=line2(7)
         line(91)=line2(8)
         line(92)=line2(9)
         line(93)=line2(10)
         line(94)=line2(12)
         line(95)=line2(20)
         line(lbncrr)=line2(lbncrr)
c THE NEXT LINE LOOKS DODGY ON SOME COMPILERS: do i=i,... ?
         do 1100 i=i,line(lbncrr)
            line(lbncrr+i)=line2(lbncrr+i)
1100     continue
         do 1700 i=1,157
            line(lbnctt+i-1)=line2(lbnctt+i-1)
1700      continue
         if(semlab(2,line,lp3))return
      
         return
      else
         IF(MONIT)THEN
          WRITE(6,*)' EXPLODING IMAGE ',NPIC
          IF(SEMCON(RECORD))RETURN
         ENDIF
         class=classn(lp1)
         if(class.ne.nclund)then
            error=6
            return
         endif 
         if(semlab(1,line,lp1))return
         ncol2=line(90)*256+line(91)
         nrow2=line(92)*256+line(93)
         nlay2=line(94)
         iform=line(95)
         if(semopn(2,npic2,ncol2,nrow2,nlay2,1,iform,lp2))return
         if(semlab(1,line2,lp2))return
         do 600 i=1,157
            line2(lbnctt+i-1)=line(lbnctt+i-1)
600      continue
         if(semlab(2,line2,lp2))return
         n=0
         L=0
         do 700 i=1,NROWS(LP1)
            if(semrow(1,line,nfmint,i,1,lp1))return
            do 800 j=1,NCOLS(LP1),2
C    case where 0 value is stored
               if(line(j).eq.0)line(j)=256

               do 900 k=1,line(j)
                  line2(l+k)=line(j+1)
900            continue
               l=l+line(j)
               if(l.ge.ncol2)then
                  l=0
                  n=n+1
                  if(N.gt.nrow2)return
                  if(semrow(2,line2,nfmint,n,1,lp2))return
               endif
800         continue 
700      continue
         return
      endif
      end
