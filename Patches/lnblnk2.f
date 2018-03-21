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
        integer function lnblnk(string)
        character*(*) string
        i=len(string)
        m=j
        do j=i,1,-1
                if(string(j:j).ne.' ')goto 10
                m=m-1;
        enddo
10      lnblnk=m
        return
        end


