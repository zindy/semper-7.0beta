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
C     routine which gets or puts files
C
      SUBROUTINE enter
C
C
c   Fuhui modified on 10/20/87
c   GET, PUT can be used alternatively: the current block
c   setting is valid to next GET or PUT command.
c   The block number is determined by the following rules:
c       BLK set -- no_file=iblk 
c       REW set -- no_file=1
c       BLK,REW unset -- unit set 1st time -- no_file=1
c                        unit already set  -- no_file=no_file+1
c                                            (next to current block)

        INCLUDE 'COMMON'

c     ldm updated for semper 6 9/26/88

c
      LOGICAL semlu,semktx,varset
C
C
      character*80 file_name
      character*1 yon,yon1(20)
      integer*4 lbyte
      integer semppn
      logical file_on,file_open_1
      logical lname,show,opt
      save iu_store

C PACKED NAMES
      DATA NUNIT/-2170/,iu_store/999/,NVVS/23281/
      data file_on/.false./,nclose/5295/

C
C INITIALISE
c       check for close option
        if(opt(nclose))then
        IF (IU_store.LE.7) GOTO 80
                close(unit=iu_store,err=140)
                file_on=.false.
                return
                endif
      IU=IVAL(NUNIT)
      n=ival(nvvs)

      IF (IU.LE.7) GOTO 80

      if(n.lt.1.or.n.gt.9)goto 120
c
c     see if open
      if(.not.file_on.or.iu.ne.iu_store)then
c
c     close old file if there
        if(file_on)close(unit=iu_store)
c
        iu_store=iu
        file_on=.true.
c
c       ask for file_name
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
200             format('Please enter file name > ',$)
                read(5,210,err=70,end=70)file_name
210             format(a80)
        endif
c
230   format(a1)
      open(unit=iu_store,file=file_name,err=130)
      endif
      read(iu_store,*,err=90,end=100)(rb1(j),j=1,n)
      do j=0,n-1
      nv=-4401-j*40
c     store the values
      if(semlu(1,nv,rb1(j+1)))goto 110
      enddo
      
c
c
   40 RETURN
C ERRORS
  120 write(6,*)'Error, only 1 to 10 variables allowed'
      Error=20
      RETURN
  110 write(6,*)'Error storing data'
      Error=167
      RETURN
  100 write(6,*)'Error while reading data'
      Error=167
      RETURN
   90 write(6,*)'Error: EOF detected'
      Error=167
      RETURN
   80 write(6,*)'Error: Invalid unit number (< 8)'
      Error=20
      RETURN
   70 write(6,*)'Error: No file name'
      Error=167
      RETURN
  130 write(6,*)'Error: cannot open ',file_name
      error=167
      return
  140 write(6,*)'Error: cannot close unit ',iu_store
      error=167
      return
C
      END
