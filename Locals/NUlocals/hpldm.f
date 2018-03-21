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
C Semper VI processing module HPLDM
C
      SUBROUTINE HPLDM                                  

C
c	Performs a brute force radial HP with edging
C
      LOGICAL SEMOPN,semrow
      real buf(2048,2048)
      integer itemp(200)
      common /big_buf/buf
C
      INCLUDE 'COMMON'
C
C Packed names
      PARAMETER (NTO=-601, nrad=28844)
      PARAMETER (nfrom=10335)
      real*8 sum
C
      np0=ivalpn(nfrom)
      np2=ivalpn(nto)
      rad=val(nrad)
C Open input picture
         IF (SEMOPN(1,np0,NCOL,NROW,nlay,iclass,iform,LP1)) RETURN
C Fault multi-layer source picture
      IF (NLAYS(LP1).NE.1) THEN
         ERROR=62
         IDERR=VERB
         RETURN
      ENDIF
C
C
c     read everything from the input picture
      do j=1,nrow
           if(semrow(1,rb1,iform,j,1,LP1)) goto 100
           do i=1,ncol
                buf(i,j)=rb1(i)
           enddo
      enddo

c Open output picture
         LP2=LP1
         IF (SEMOPN(2,np2,NCOL,NROW,1,iclass,iform,LP2)) goto 100
C

c     Work out coefficients of circle and store in RB1
      ir=nint(rad)
      n=1
      npts=0
      do j=-ir,ir
      x=sqrt(ir*ir-j*j+1e-3)
      itemp(n)=nint(x)
      n=n+1
      npts=npts+2*nint(x)+1
      enddo
      xpts=1./npts
C      write(6,*)npts,xpts,ir,LP1,LP2
      DO 40 J=1,NROW
C
c	Setup the sum for the edge
            sum=0.0
            n=1
            do iy=-ir,ir
            iix=itemp(n)
            n=n+1
            j1=j+iy
            if(j1.lt.1)j1=1
            if(j1.gt.nrow)j1=nrow
            i=1
            do ix=-iix,iix
            i1=i+ix
            if(i1.lt.1)i1=1
            if(i1.gt.ncol)i1=ncol
            sum=sum+buf(i1,j1)
            enddo
            enddo

            rb2(i)=buf(i,j)-sum*xpts
            DO 10 I=2,NCOL

            n=1
c	Increment sum
            do iy=-ir,ir
            j1=j+iy
            if(j1.lt.1)then
               j1=1
            else if(j1.gt.nrow)then
               j1=nrow
            endif
            iix=itemp(n)
            i1=i-iix-1
            i2=i+iix
            if(i1.lt.1)i1=1
            if(i2.gt.ncol)i2=ncol
            sum=sum-buf(i1,j1)+buf(i2,j1)
            n=n+1
            enddo
            rb2(i)=buf(i,j)-sum*xpts
C
10    continue
C
      IF (SEMROW(2,RB2,iform,J,1,LP2)) goto 100
40    continue
C
100   RETURN
      END

