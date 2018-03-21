       character*120 line,buffer
       character*120 aliases(50), unlinka
       integer lalias(50),tlen
       integer*4 system,ifail
       open(unit=10,file='Helpinfo.shl')
10     read(10,1,err=600,end=600)line
1      format(a)
C
C      Does it have '$add' in front
       if(line(1:4).eq.'$add')goto 100
C      No, so output it, trimmed
       n=100
       do j=100,1,-1
          if(line(j:j).ne.' ')goto 20
          n=n-1
       enddo
20     if((n.eq.1).and.(line(1:1).eq.'$'))then
          line(1:1)=' '
       endif
       write(20,1)line(1:n)
       goto 10
100    continue
C      It's a new command
C      Find out what they are
       n1=6
       n=5
       nc=0
       write(6,1)line
200    n=n+1
       if(line(n:n).eq.' ')then
C         End of command
          las=n-n1
C         if las=0, we found '  '
          if((las.eq.1).and.(line(n1:n-1).eq.' '))goto 300
C         Continuation line
          if((las.eq.1).and.(line(n1:n-1).eq.'+'))then
             read(10,1)line
             write(6,1)line
             n1=6
             n=5
             goto 200
         endif
C         Store, and increment counters
          nc=nc+1
          aliases(nc)(1:las)=line(n1:n-1)
          lalias(nc)=las
C          write(6,*)'Found ',n,n1,nc,las,aliases(nc)(1:las)
          n1=n+1
          n=n1

       endif
C      Jump back if n<120
       if(n.lt.120)goto 200
300    continue
C      Process
C      Strip .command if there from initial file
       las=lalias(1)
       if(aliases(1)(las-7:las).eq.'.command')then
          las=las-3
       endif
C      Change ? error codes
       if(aliases(1)(1:1).eq.'?')then
          do j=las,2,-1
             aliases(1)(5+j:5+j)=aliases(1)(j:j)
          enddo
          aliases(1)(1:6)='error_'
          las=las+5
       endif
C       las=las+5

       do k=2,nc
         lasn=lalias(k)
          if(aliases(k)(1:1).eq.'?')then
          do j=lasn,2,-1
             aliases(k)(5+j:5+j)=aliases(k)(j:j)
          enddo
          aliases(k)(1:6)='error_'
          lalias(k)=lalias(k)+5
          endif
       enddo
          
C      Note: skip : at end
       if(aliases(1)(las:las).eq.':')las=las-1
       lalias(1)=las
C      In case it already exists, unlink it
       unlinka=' '
       unlinka(1:las)=aliases(1)(1:las)
       unlinka(las+1:las+1)=char(0)
       call unlink(unlinka,istatus)
C      Open new output file
       open(unit=20,file=aliases(1)(1:las),err=999)
       if(nc.gt.1)then
       do j=2,nc
          L1=lalias(1)
          L2=lalias(j)
          write(buffer,310)aliases(1)(1:L1),
     $     aliases(j)(1:L2)
310        format('ln -f -s ',a,' ',a)
           tlen=9+L1+1+L2
           ifail=system(buffer(1:tlen))
C           write(77,*)buffer(1:tlen)
       enddo
       endif
       goto 10
600    continue
C      Maybe it worked ?
       call exit(0)
999    continue
C      Open failure, reduce las by one
       las=las-1
       lalias(1)=las
       goto 300
       end
