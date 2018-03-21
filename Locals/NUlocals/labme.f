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
      logical function labme(month,iday,iyear,title,nt,
     $ readme,lpn)
c     routine to read/write label data for picture lpn
      logical readme,semlab
      character*1 title(*), temp
C	The next statement must point to where the parameters and
C	related information for semper are stored - change as needed.
      include 'PARAMS'
      integer label(lnlab)

c     true on error
      labme=.true.

c     get old data first
      if(semlab(1,label,lpn))return

      if(.not.readme)then
c     write

c     year, month, day, (zero others)
      label(lbyear)=iyear
      label(lbmon)=month
      label(lbday)=iday
      label(lbhour)=0
      label(lbmin)=0
      label(lbsec)=0

c     number of characters in title
      label(lbnctt) = nt

c     lbtt1 to whatever the title
      do j=0,nt
      temp=title(j+1)
      label(lbtt1+j)=ichar(temp)
      enddo

c     write back label
      if(semlab(2,label,lpn))return

      else
c     read
      iyear=label(lbyear)
      month=label(lbmon)
      iday=label(lbday)

c     number of characters in title
      nt=label(lbnctt) 

c     lbtt1 to whatever the title
      do j=0,nt
      itemp=label(j+lbtt1)
      title(j+1)=char(itemp)
      enddo
      nn=nt/4
      if(nn*4.lt.nt)then
      do j=nt+1,(nn+1)*4
      title(j)=' '
      enddo
      endif

      endif
      labme=.false.
      return
      end

