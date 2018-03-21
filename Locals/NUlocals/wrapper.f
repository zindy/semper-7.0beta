      subroutine wrapper(i1,i2)
c     simple shifting into unit cell routine
10    if(i1.gt.i2)then
           i1=i1-i2
           goto 10
      endif
20    if(i1.lt.1)then
           i1=i1+i2
           goto 20
           endif
      return
      end
