C -----------------------------------------------------------------------------
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
C  -----------------------------------------------------------------------------
C     Interface to Semper help files
      subroutine Helpfl6(iname,ilen,found)

      character*200 browser
      character*200 filestring
      character*200 commstring
      character*80 name
      logical found
      integer iname(*),SempBuild,access
      integer*4 system,ifail
C        write(6,*)ilen
        do j=1,80
                name(j:j)=' '
        enddo
        do j=1,ilen
                name(j:j)=char(iname(j))
        enddo
C        write(6,*)'Name ',name(1:ilen),ilen
C     Get the browser name
        IBR=SempBuild(9,browser)
C        write(6,*)'Browser ',browser(1:IBR),ibr
C     Get the installation prefix
        IPRE=SempBuild(10,filestring)
C        write(6,*)'Prefix ',filestring(1:IPRE),ipre
C     Tack togethor the location
        Lname=lnblnk(name)
        IPRE=IPRE+1
        filestring(IPRE:IPRE+5)='/help/'
        IPRE=IPRE+5
        filestring(IPRE+1:IPRE+Lname)=name(1:Lname)
        IPRE=IPRE+Lname
C     check if the file exists 
C      inquire(file=filestring(1:IPRE),exist=found)
        found= ACCESS(filestring(1:IPRE),'r').EQ.0
C        write(6,*)'File ',filestring(1:IPRE),IPRE
      if(found)then
C
C     Use Browser
        commstring(1:IBR)=browser(1:IBR)
        IC=IBR+1
        commstring(IC:IC)=' '
        IC=IC+1
        commstring(IC:IC+IPRE-1)=filestring(1:IPRE)
        IC=IC+IPRE
        commstring(IC:IC+2) = ' & '
        IC=IC+2
C       write(6,*)'Debug ',commstring(1:IC)
        ifail=system(commstring(1:IC))
C       write(6,*)'Debug ',commstring(1:IC)
      else
C        write(6,*)'Not found ',filestring(1:IPRE)
      endif
      return
      end
