c  s6rwb.f
c  Reads and writes Semper 6 read/write binary format

c--------------------------------------------------------------------------
      function gs6rwb()

c  read semper 6 read/write binary file

c  local variables

c  date & time
      integer*4 year, month, day, hour, min, sec
      integer*4 i, j

c  common variables
      include "vars.h"


c  now get on with it
      if (v .ge. 3) write (6,"('starting gs6rwb')")
      if (v .ge. 3) write (6,"('file name: ',a)") path(1:lpath)

c  readonly not supported on mac g77
c      open (2, file=path, err=130, form='unformatted', readonly)
      open (2, file=path, err=130, form='unformatted')

c  read first header line
      read (2) ncol, nrow, nlay, iclass, iform, iflag

      iversn = iflag/10000
      ilabel = mod(iflag, 10000)/1000
      ntitle = mod(iflag, 1000)

c  verify values
      if (v .ge. 3) then
         write (6,"('ncol     ',i6)") ncol
         write (6,"('nrow     ',i6)") nrow
         write (6,"('nlay     ',i6)") nlay
         write (6,"('iclass   ',i6)") iclass
         write (6,"('iform    ',i6)") iform
         write (6,"('iflag    ',i6)") iflag
         write (6,"('iversn   ',i6)") iversn
         write (6,"('ilabel   ',i6)") ilabel
         write (6,"('ntitle   ',i6)") ntitle
      endif

c  read title if present
      if (ntitle .gt. 0) then
         if (iversn .eq. 0) then
            read (2) (bufi2(i), i=0,ntitle-1)
            do i=1,ntitle
               title(i:i) = char(bufi2(i-1))
            enddo
         else
            read (2) title(1:ntitle)
         endif
         ltitle = ntitle
         if (v .ge. 3) then
            write (6,"('title (length ',i6,')')") ltitle
            write (6,"('""',a,'""')") title(1:ltitle)
         endif

c  search for keywords in title
         im1dcs(1:ltitle) = title(1:ltitle)
         call lower(im1dcs(1:ltitle))
         call eqtosp(im1dcs(1:ltitle))
         i = index(im1dcs(1:ltitle), 'evzero')
         if (i .gt. 0) then
            read(im1dcs(i+6:ltitle), *, err=10, end=10) evzero
            fevzero = .true.
         endif
 10      i = index(im1dcs(1:ltitle), 'evpch')
         if (i .gt. 0) then
            read(im1dcs(i+5:ltitle), *, err=20, end=20) evpch
            fevpch = .true.
         endif
 20      continue

      endif

c  read label if present
      if (ilabel .gt. 0) then
         read (2) (labeli2(i), i=1,256)

c  set values from label
         origx = 256*labeli2(13) + labeli2(14)
         forigx = .true.
         origy = 256*labeli2(15) + labeli2(16)
         forigy = .true.
c  variables. set fsvars if any one is not=0
         do i=0,9
            j = 96 - 4*i
            cf4(1:1) = char(labeli2(j))
            cf4(2:2) = char(labeli2(j+1))
            cf4(3:3) = char(labeli2(j+2))
            cf4(4:4) = char(labeli2(j+3))
            svars(i) = f4
            if (svars(i) .ne. 0.) fsvars = .true.
         enddo
c  scale
         if (fsvars) then
            if (svars(1) .ne. 0.) then
               nmorx = svars(0)
               fnmorx = .true.
               nmppx = svars(1)
               fnmppx = .true.
               if (nrow .gt. 1) then
                  nmory = svars(2)
                  fnmory = .true.
                  nmppy = svars(3)
                  fnmppy = .true.
               endif
               if (nlay .gt. 1) then
                  nmorz = svars(4)
                  fnmorz = .true.
                  nmppz = svars(5)
                  fnmppz = .true.
               endif
            endif
         endif
c  time
         year = labeli2(22) + 1900
         month = labeli2(23)
         day = labeli2(24)
         hour = labeli2(25)
         min = labeli2(26)
         sec = labeli2(27)
         imtime = uxtime(year, month, day, hour, min, sec)
         fimtime = .true.
      else

c  set default origin - in middle. remember y counts from top
         origx = nint(real(ncol+1)/2.0)
         forigx = .true.
         origy = nint(real(nrow+1)/2.0)
         forigy = .true.
      endif

c  correct evzero for origin
      if (fevzero) then
         if (fevpch) then
            evzero = evzero - (origx-1)*evpch
         else
            evzero = evzero - (origx-1)
         endif
c      else
c         if (fevpch) then
c            evzero = -(origx-1)*evpch
c         else
c            evzero = -(origx-1)
c         endif
c         fevzero = .true.
      endif

c  set some values
      imwid = ncol
      fimwid = .true.
      imhei = nrow
      fimhei = .true.
      if (nlay .ne. 1) then
         if (v .ge. 0) write
     +   (6,"('picture has ',i6,' layers, only converting first')") nlay
      endif
      fpbyte = .true.
      fptype = .true.
      fpsign = .true.

c  read picture
      if (iform .eq. 0) then
         pbyte = 1
         ptype = 'i'
         psign = 0
         if (nrow .eq. 1) then
            read (2) (im1di1(i), i=0,ncol-1)
         else
            do j=0,nrow-1
               read (2) (imagi1(i,j), i=0,ncol-1)
            enddo
         endif
      elseif (iform .eq. 1) then
         pbyte = 2
         ptype = 'i'
         psign = 1
         if (nrow .eq. 1) then
            read (2) (im1di2(i), i=0,ncol-1)
         else
            do j=0,nrow-1
               read (2) (imagi2(i,j), i=0,ncol-1)
            enddo
         endif
      elseif (iform .eq. 2) then
         pbyte = 4
         ptype = 'f'
         psign = 1
         if (nrow .eq. 1) then
            read (2) (im1dr(i), i=0,ncol-1)
         else
            do j=0,nrow-1
               read (2) (imagr(i,j), i=0,ncol-1)
            enddo
         endif
      elseif (iform .eq. 3) then
         pbyte = 8
         ptype = 'c'
         psign = 1
         if (nrow .eq. 1) then
            read (2) (im1dx(i), i=0,ncol-1)
         else
            do j=0,nrow-1
               read (2) (imagx(i,j), i=0,ncol-1)
            enddo
         endif
      else
         if (v .ge. 3) write (6,"('unknown form, ',i6)") iform
         fpbyte = .false.
         fptype = .false.
         fpsign = .false.
         gs6rwb = 1
         return
      endif

c  image read so set fim
      fim = .true.

c  close
      close (2)
      gs6rwb = 0

      return

c  problem opening file
 130  if (v .ge. 3) write (6,"('can''t open')")
      gs6rwb = 1
      return

      end

c--------------------------------------------------------------------------
      function ws6rwb()

c  write semper 6 read/write binary file
c  see ws6lab for details of semper variables etc

c  local variables
      integer*4 i, j, k

c  common variables
      include "vars.h"


c  now get on with it
      if (v .ge. 3) write (6,"('ws6rwb: starting...')")

c  sort out semper variables etc and picture label
      ws6rwb = ws6lab()
      if (ws6rwb .ne. 0) return

c  calculate iflag
      iversn = 2
      ilabel = 1      ! no label if =0
      iflag = 10000*iversn + 1000*ilabel + ntitle

c  now open file
      if (v .ge. 3) write (6,"('output file name ""',a,'""')")
     +                     opath(1:lopath)
      open (2, file=opath, err=130, form='unformatted')

C  write header line and title
      write (2) ncol, nrow, nlay, iclass, iform, iflag
      if (ntitle .gt. 0) write (2) stitle(1:ntitle)

c  write label
      if (ilabel .gt. 0) write (2) (labeli2(i), i=1,256)

c  write picture - need to do for all types
      if (ptype .eq. 'i') then
         if (pbyte .eq. 1) then
            if (psign .eq. 0) then
c   unsigned byte -> byte
               if (v .ge. 3) write (6,"('unsigned byte -> byte')")
c  for some horrible reason byte pictures only have even length records
               if (nrow .eq. 1) then
                  write (2) (im1di2(i), i=0,(ncol-1)/2)
               else
                  do j=0,nrow-1
                     write (2) (imagi2(i,j), i=0,(ncol-1)/2)
                  enddo
               endif
            else
c   signed byte -> integer
               if (v .ge. 3) write (6,"('signed byte -> integer')")
               if (nrow .eq. 1) then
                  do i=0,ncol-1
                     bufi2(i) = im1di1(i)
                  enddo
                  write (2) (bufi2(i), i=0,ncol-1)
               else
                  do j=0,nrow-1
                     do i=0,ncol-1
                        bufi2(i) = imagi1(i,j)
                     enddo
                     write (2) (bufi2(i), i=0,ncol-1)
                  enddo
               endif
            endif
         elseif (pbyte .eq. 2) then
            if (psign .eq. 0) then
c   unsigned i2 -> fp
               if (v .ge. 3) write (6,"('unsigned i2 -> fp')")
               if (nrow .eq. 1) then
                  do i=0,ncol-1
                     bufr(i) = im1di2(i)
                  enddo
                  write (2) (bufr(i), i=0,ncol-1)
               else
                  do j=0,nrow-1
                     do i=0,ncol-1
                        if (imagi2(i,j) .ge. 0) then
                           bufr(i) = imagi2(i,j)
                        else
                           bufr(i) = 65536.0 + imagi2(i,j)
                        endif
                     enddo
                     write (2) (bufr(i), i=0,ncol-1)
                  enddo
               endif
            else
c   signed i2 -> integer
               if (v .ge. 3) write (6,"('signed i2 -> integer')")
               if (nrow .eq. 1) then
                  write (2) (im1di2(i), i=0,ncol-1)
               else
                  do j=0,nrow-1
                     write (2) (imagi2(i,j), i=0,ncol-1)
                  enddo
               endif
            endif
         elseif (pbyte .eq. 4) then
            if (psign .eq. 0) then
c   unsigned i4 -> fp
               if (v .ge. 3) write (6,"('unsigned i4 -> fp')")
               if (nrow .eq. 1) then
                  do i=0,ncol-1
                     if (im1di4(i) .ge. 0) then
                        bufr(i) = im1di4(i)
                     else
                        bufr(i) = 4294967296. + im1di4(i)
                     endif
                  enddo
                  write (2) (bufr(i), i=0,ncol-1)
               else
                  do j=0,nrow-1
                     do i=0,ncol-1
                        if (imagi4(i,j) .ge. 0) then
                           bufr(i) = imagi4(i,j)
                        else
                           bufr(i) = 4294967296. + imagi4(i,j)
                        endif
                     enddo
                     write (2) (bufr(i), i=0,ncol-1)
                  enddo
               endif
            else
c   signed i4 -> fp
               if (v .ge. 3) write (6,"('signed i4 -> fp')")
               if (nrow .eq. 1) then
                  do i=0,ncol-1
                     bufr(i) = im1di4(i)
                  enddo
                  write (2) (bufr(i), i=0,ncol-1)
               else
                  do j=0,nrow-1
                     do i=0,ncol-1
                        bufr(i) = imagi4(i,j)
                     enddo
                     write (2) (bufr(i), i=0,ncol-1)
                  enddo
               endif
            endif
         else
            if (v .ge. 0) write (6,"('ws6rwb: pbyte=',i6)") pbyte
            goto 120
         endif
      elseif (ptype .eq. 'f') then
         if (pbyte .eq. 4) then
c   real*4 -> fp
            if (v .ge. 3) write (6,"('real*4 -> fp')")
            if (nrow .eq. 1) then
               write (2) (im1dr(i), i=0,ncol-1)
            else
               do j=0,nrow-1
                  write (2) (imagr(i,j), i=0,ncol-1)
               enddo
            endif
         elseif (pbyte .eq. 8) then
c   real*8 -> fp
            if (v .ge. 3) write (6,"('real*8 -> fp')")
            if (nrow .eq. 1) then
               do i=0,ncol-1
                  bufr(i) = im1dr8(i)
               enddo
               write (2) (bufr(i), i=0,ncol-1)
            else
               do j=0,nrow-1
                  do i=0,ncol-1
                     bufr(i) = imagr8(i,j)
                  enddo
                  write (2) (bufr(i), i=0,ncol-1)
               enddo
            endif
         else
            if (v .ge. 0) write (6,"('ws6rwb: pbyte=',i6)") pbyte
            goto 120
         endif
      elseif (ptype .eq. 'c') then
c   complex*8 -> complex
         if (pbyte .eq. 8) then
            if (v .ge. 3) write (6,"('complex*8 -> complex')")
            if (nrow .eq. 1) then
               write (2) (im1dx(i), i=0,ncol-1)
            else
               do j=0,nrow-1
                  write (2) (imagx(i,j), i=0,ncol-1)
               enddo
            endif
         elseif (pbyte .eq. 16) then
c   complex*16 -> complex
            if (v .ge. 3) write (6,"('complex*16 -> fp')")
            if (nrow .eq. 1) then
               do i=0,ncol*2-1
                  bufr(i) = im1dr8(i)
               enddo
               write (2) (bufr(i), i=0,ncol*2-1)
            else
               do j=0,nrow-1
                  do i=0,ncol*2-1
                     bufr(i) = imagr8(i,j)
                  enddo
                  write (2) (bufr(i), i=0,ncol*2-1)
               enddo
            endif
         else
            if (v .ge. 0) write (6,"('ws6rwb: pbyte=',i6)") pbyte
            goto 120
         endif
      elseif (ptype .eq. 'r') then
c   rgb -> 3 layer byte
c   remember odd row length byte pics have to be even...
         if (v .ge. 3) write (6,"('rgb -> 3 layer byte')")
         if (nrow .eq. 1) then
            do k=0,nlay-1
               write(2) (im1dc2(i,k), i=0,(ncol-1)/2)
            enddo
         else
            do k=0,nlay-1
               do j=0,nrow-1
                  write (2) (imagc2(i,j,k), i=0,(ncol-1)/2)
               enddo
            enddo
         endif
      else
         if (v .ge. 0) write (6,"('ws6rwb: unknown ptype=""',a,'""')")
     +                       ptype
         goto 120
      endif


      close(2)

      ws6rwb = 0
      return

c  error somewhere
 120  ws6rwb = 1
      return

c  error opening file
 130  if (v .ge. 0) write (6,"('couldn''t open ',a)") opath(1:lopath)
      ws6rwb = 1
      return


      end
