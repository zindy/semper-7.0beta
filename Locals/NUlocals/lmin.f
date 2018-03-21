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
      subroutine lmin(x,y,a,val)
c     given a 3x3 array a, finds the local maximum,
c     returns the expected value there
      dimension a(-1:1,-1:1)
      dxx=1./3.*(a(1,1)-2.*a(0,1)+a(-1,1)+a(1,0)
     $     -2.*a(0,0)+a(-1,0)+a(1,-1)-2.*a(0,-1)+a(-1,-1))
      dyy=1./3.*(a(1,1)-2.*a(1,0)+a(1,-1)+a(0,1)
     $     -2.*a(0,0)+a(0,-1)+a(-1,1)-2.*a(-1,0)+a(-1,-1))
      dxy=0.25*(a(1,1)-a(1,-1)-a(-1,1)+a(-1,-1))
      dx=0.5*(a(1,0)-a(-1,0))
      dy=0.5*(a(0,1)-a(0,-1))
      det=(dxx*dyy-dxy*dxy)
C     write(6,*)dxx,dyy,dxy,dx,dy,det,a
      if(abs(det).lt.1e-10)then
           x=0
           y=0
           val=a(0,0)
      return
      endif
      x=-(dyy*dx-dxy*dy)/det
      y=-(-dxy*dx+dxx*dy)/det
      val=a(0,0)+dx*x+dy*y+0.5*(x*x*dxx+y*y*dyy)+x*y*dxy
      return
      end
