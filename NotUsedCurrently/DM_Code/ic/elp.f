c  elp.f
c  Reads and writes EL/P files

c--------------------------------------------------------------------------
      function getelp()

c  read EL/P binary file

c  local variables
      character cname*2       ! tag name
      character ctype*1       ! tag type (char, int, float)
      integer*1 ibytes        ! bytes/number
      integer  ilength       ! length
      integer*4 ia            ! temp
      integer*8 pos8

c  common variables
      include "vars.h"


c  now get on with it
      if (v .ge. 3) write (6,"('starting getelp')")

c  open file
      fname = path(1:lpath)
      ifdes = fileopenr(fname)
      if (v .ge. 3) write
     +   (6,"('fileopenr returned ',z8)")
     +   ifdes
      if (ifdes .le. 0 .and .v .ge. 0) then
         write (6,"('can''t find ""',a,'""')") path(1:lpath)
         goto 120
      endif

      if (v .ge. 3) write (6,"('dstart = ',i11)") dstart

c  skip to start if macbinary
      if (dstart .gt. 0) then
         pos8 = dstart
         rv = fileseek(ifdes, pos8)
         if (rv .ne. dstart) then
            if (v .ge. 0) write
     +           (6,"('getelp: fileseek returned ',i11,' not ',i6)")
     +           rv, dstart
            goto 120
         endif
      endif

c  read first 2 bytes
      rv = filereadi2(ifdes, ai2, bigend)
      if (ai2 .ne. x'0002') then
         write (6,"('getelp: first bytes = ',z4,', not 0002')") ai2
         goto 120
      endif

c      if (v .ge. 3) write (6,"('read first 2 bytes')")

c  read in rest of file
c  first the tag
 1       rv = fileread(ifdes, cname, 2)
         if (rv .eq. 1) then
            if (v .ge. 0) write (6,"('only read 1 not 2 bytes')")
            goto 110
         endif
         if (rv.le.0) goto 110
         rv = fileread(ifdes, ctype, 1)
         if (rv.le.0) goto 110
         rv = filereadi1(ifdes, ibytes)
         if (rv.le.0) goto 110
         rv = filereadi2(ifdes, ilength, bigend)
         if (rv.le.0) goto 110

c         if (v .ge. 3) write (6,"('read tag: ',a2,a1,i4,i6)") 
c     +                      cname, ctype, ibytes, ilength

c  some checks
         if (ctype .ne. 'c' .and. ctype .ne. 'i' .and. ctype .ne. 'f')
     +      then
            if (v .ge. 0) write (6,"('tag type is ',a1)") ctype
            goto 120
         endif
         if (ctype .eq. 'c' .and. ibytes .ne. 1) then
            if (v .ge. 0) write (6,"('tag type c, bytes = ',i3)") ibytes
            goto 120
         endif
         if (ctype .eq. 'f' .and. ibytes .ne. 4) then
            if (v .ge. 0) write (6,"('tag type f, bytes = ',i3)") ibytes
            goto 120
         endif
         if (ibytes .ne. 1 .and. ibytes .ne. 2 .and. ibytes .ne. 4) then
            if (v .ge. 0) write (6,"('ibytes=',i3,' not 1,2,4')") ibytes
            goto 120
         endif

c  read tag value
         if (ibytes*ilength .gt. 0) then
c            if (v .ge. 3) write (6,"('ibytes*ilength=',i10)")
c     +                         ibytes*ilength
            ia = ibytes*ilength
c            write (6,"('ia=',i10)") ia
            rv = fileread(ifdes, im1dc, ia)
c            write (6,"('rv=',i10, '...=',i10)") rv, ibytes*ilength
            if (rv .lt. ibytes*ilength) goto 110
            if (rv .gt. ibytes*ilength) then
               write (6,"('fileread rv=',i6,' asked for ',i6)")
     +                rv, ibytes*ilength
               goto 120
            endif
         endif

c         if (v .ge. 3) write (6,"('read more still')") 

c  sort out integer value
         if (ctype .eq. 'i') then
            if (ibytes .eq. 1) then
               ia = im1di1(0)
            elseif (ibytes .eq. 2) then
               ia = im1di2(0)
c               write (6,"('im1di2(0)=',i6,' ia=',i3)") im1di2(0), ia
            else
               ia = im1di4(0)
            endif
         endif

c         if (v .ge. 3) write (6,"('read further')") 

c  verify value
         if (v .ge. 3) then
            if (ctype .eq. 'c')
     +         write (6,"('""',a2,'"" ""',a1,'"" ',i3,i6,' ""',a,'""')")
     +                cname, ctype, ibytes, ilength,
     +                im1dcs(1:ilength)
            if (ctype .eq. 'i')
     +         write (6,"('""',a2,'"" ""',a1,'"" ',i3,i6,i12)")
     +                cname, ctype, ibytes, ilength, ia
            if (ctype .eq. 'f')
     +         write (6,"('""',a2,'"" ""',a1,'"" ',i3,i6,1pe15.6)")
     +                cname, ctype, ibytes, ilength, im1dr(0)
         endif

c  set useful values
c  assumes da (the spectrum values) is last in file
         if (cname .eq. 'cm') then
            title = im1dcs(1:ilength)
            ltitle = ilength
         elseif (cname .eq. 'E1') then
            evzero = im1dr(0)
            fevzero = .true.
         elseif (cname .eq. 'dE') then
            evpch = im1dr(0)
            fevpch = .true.
         elseif (cname .eq. 'fC') then
            choff = ia
            fchoff = .true.
         elseif (cname .eq. 'aT' .and. ia .ne. 0) then
c  ignore time if =0 as if not set el/p writes 0
            imtime = ia - deltat
            fimtime = .true.
         elseif (cname .eq. 'da') then
            fim = .true.
            imwid = ilength
            fimwid = .true.
            imhei = 1
            fimhei = .true.
            pbyte = ibytes
            fpbyte = .true.
            ptype = ctype
            fptype = .true.
            psign = 1
            fpsign = .true.
            goto 100
         endif

c  go back & read the next tag
         goto 1

c  evzero in el/p includes the offset, choff. convert so evzero
c  corresponds to the leftmost real channel
 100  if (fchoff .and. fevzero) then
         if (fevpch) then
            evzero = evzero + choff*evpch
         else
            evzero = evzero + choff
         endif
      endif

c  set x origin
c  position of origin (left channel = 1, right = imwid)
      if (fimwid .and. fevzero .and. fevpch) then
         ia = nint(1 - evzero/evpch)
         if (ia .lt. 1) then
c  origin is off left edge of spectrum, put it at the left
            origx = 1
            forigx = .true.
         elseif (ia .gt. imwid) then
c  origin is off right edge of spectrum (unlikely), put it at right
            origx = imwid
            forigx = .true.
         else
c  origin is in middle of spectrum
            origx = ia
            forigx = .true.
         endif
      endif

c  normal end of file
      if (v .ge. 3) write (6,'(''getelp: normal end of EL/P file'')')
      getelp = 0
      return

c  unexpected end of file
 110  if (v .ge. 0) write (6,'(''getelp: unexpected end of file'')')
      getelp = 1
      return

c  error somewhere
 120  if (v .ge. 0) write (6,'(''getelp: error somewhere'')')
      getelp = 1
      return

      end

c--------------------------------------------------------------------------
      function welp()

c  write EL/P binary file

c  local variables
      integer  ilength       ! length
      character clength*2     ! char version of ilength

c  common variables
      include "vars.h"

      equivalence (ilength, clength)


c  now get on with it
      if (v .ge. 3) write (6,"('starting welp')")

c  open file
      fname = opath(1:lopath)
      ofdes = fileopenw(fname)
      if (v .ge. 3) write
     +   (6,"('welp: fileopenw returned ',z8)")
     +   ofdes
      if (ofdes .le. 0 .and. v .ge. 0) then
         write (6,"('welp: can''t open ""',a,'""')") opath(1:lopath)
         welp = 1
         return
      endif

c  write first 2 bytes (always 0002)
      bufc = char(0) // char(2)
      rv = filewrite(ofdes, bufc, 2)
      if (rv .lt. 0) then
         if (v .ge. 0) write (6,"('welp: write returned ',i6)")
     +                 rv
         welp = 1
         return
      endif

c  write title
      if (ltitle .ge. 0) then
         ilength = ltitle
         if (v .ge. 3) write (6,"('writing title, length ',i6)") ilength
         bufc = 'cmc' // char(1) // clength // title(1:ltitle)
         rv = filewrite(ofdes, bufc, ltitle+6)
         if (rv .lt. 0) then
            if (v .ge. 0) write
     +         (6,"('welp: filewrite returned ',i6)") rv
            welp = 1
            return
         endif
      endif

c  write choff, channel offset
      if (fchoff) then
         ilength = 1
         i2 = choff
         if (v .ge. 3) write (6,"('writing choff=',i6)") i2
         bufc = 'fCi' // char(2) // clength // ci2
         rv = filewrite(ofdes, bufc, 8)
         if (rv .lt. 0) then
            if (v .ge. 0) write
     +         (6,"('welp: write returned ',i6)") rv
            welp = 1
            return
         endif
      endif

c  write energy of first channel (evzero)
      if (fevzero) then
         f4 = evzero
      else
         if (forigx) then
            if (fevpch) then
               f4 = -(origx-1)*evpch
            else
               f4 = -(origx-1)
            endif

         else
            f4 = 0
         endif
      endif

c  my evzero refers to the first real channel, if choff is set, need to
c  allow for choff
      if (fchoff) then
         if (fevpch) then
            f4 = f4 - choff*evpch
         else
            f4 = f4 - choff
         endif
      endif

c  evzero refers to the left channel regardless of origx
c      if (forigx) then
c         if (fevpch) then
c            f4 = f4 - (origx-1)*evpch
c         else
c            f4 = f4 - (origx-1)
c         endif
c      endif
      ilength = 1
      bufc = 'E1f' // char(4) // clength // cf4
      rv = filewrite(ofdes, bufc, 10)
      if (rv .lt. 0) then
         if (v .ge. 0) write
     +      (6,"('welp: filewrite returned ',i6)") rv
         welp = 1
         return
      endif

c  write energy per channel (evpch)
      if (fevpch) then
         f4 = evpch
      else
         f4 = 1
      endif
      if (f4 .eq. 0.) f4 = 1
      ilength = 1
      bufc = 'dEf' // char(4) // clength // cf4
      rv = filewrite(ofdes, bufc, 10)
      if (rv .lt. 0) then
         if (v .ge. 0) write
     +      (6,"('welp: filewrite returned ',i6)") rv
         welp = 1
         return
      endif

c  write time
      if (fimtime) then
         ilength = 1
         i4 = imtime + deltat
         if (v .ge. 3) write (6,"('writing imtime=',z8)") i4
         bufc = 'aTi' // char(4) // clength // ci4
         rv = filewrite(ofdes, bufc, 10)
         if (rv .ne. 10) then
            if (v .ge. 0) write
     +         (6,"('welp: filewrite returned ',i6)") rv
            welp = 1
            return
         endif
      endif

c  write spectrum itself
      ilength = imwid
      bufc = 'daf' // char(4) // clength
      rv = filewrite(ofdes, bufc, 6)
      if (rv .lt. 0) then
         if (v .ge. 0) write
     +      (6,"('welp: filewrite returned ',i6)") rv
         welp = 1
         return
      endif

      if (ptype .eq. 'f' .and. pbyte .eq. 4) then
         rv = filewrite(ofdes, im1dcs(1:imwid*4), imwid*4)
         if (rv .lt. 0) then
            if (v .ge. 0) write
     +         (6,"('welp: filewrite returned ',i6)") rv
            welp = 1
            return
         endif
      else
         if (v .ge. 0) write (6,"('Oops, needs to be a fp picture')")
         welp = 1
      endif

      welp = 0

c  close file
 120  rv = fileclose(ofdes)
      if (rv .ne. 0) then
         if (v .ge. 0) write
     +                 (6,"('welp: fileclose returned ',i6,' not 0')")
     +                 rv
         welp = 1
      endif

      return

      end
