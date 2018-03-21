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
      subroutine pclose
c     Closes a picture (if it is open)
C Close output picture
      parameter (NFROM=10335,NALL=2092,NTEMP=-214)
C
      INCLUDE 'COMMON'
C
      integer semppn
      logical semlu,semcls,semdel

c     Release All PCB's (dangerous ??)
      if(semlu(-1,nall,temp))goto 2000

c     Release All temporary PCB's (dangerous ??)
      if(semlu(-1,ntemp,temp))goto 3000

c     Close PCB for a given picture
      NPIC=IVAL(NFROM)
      IDEV=SEMPPN(NPIC)/1000
      IF (NPIC.LE.0) return

c     Scan over open pictures, looking for the appropriate one
   40 DO 50 I=1,NLPS
      IF (DEVN(I).NE.IDEV) GOTO 50
      IF (PICN(I).EQ.NPIC) THEN
         IF (SEMCLS(I)) RETURN
         ENDIF
   50 CONTINUE
      return

2000  continue
      DO I=1,NLPS
         IF (SEMCLS(I)) RETURN
      enddo
      return

3000  continue
c     delete temporaries (?)
      if(SEMDEL(2,NUMBER))return
      return
      end
