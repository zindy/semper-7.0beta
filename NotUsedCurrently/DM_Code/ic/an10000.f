c  an10000.f
c  Reads and writes Link/Oxford AN10000 files
c  Started 21 oct 2006
c  based on elp.f

c--------------------------------------------------------------------------
      function getan10000(dump)

c  read AN10000 binary file

c  functions
      real*4   an10real

c  local variables
      integer*4 dump          ! 0 = getfile, 1 = dump file
      integer  imagetype     ! word 0
      integer  headerlength  ! word 1
      integer  mhorder       ! word 2
      integer  hd1           ! word 3
      integer  hd2           ! word 4
      character*3 xunits      ! word 5-6
c      integer*1 xscalei1(0:3) ! words 7-8
c      integer*1 xzeroi1(0:3)  ! words 9-10
      character xscalec*4     ! words 7-8
      character xzeroc*4      ! words 9-10
      real*4    xscale, xzero
      character imlabel*34    ! words 11-27
      integer  iscale        ! word 29
      integer  nscale        ! word 30
      integer  livetime      ! X-ray spectrum word 36
      integer  realtime      ! " word 37
      integer*4 cpsec         ! " word 39-40
      integer  vd1           ! Linescan word 36
      integer  vd2           ! Linescan word 37
      character*3 yunits      ! Linescan words 38-39
      character yscalec*4     ! Linescan words 40-41
      character yzeroc*4      ! Linescan words 42-43
      real*4    yscale, yzero

      integer*4 ia
      integer*8 pos8

c  common variables
      include "vars.h"


c  now get on with it
      if (v .ge. 3) write (6,"('getan10000: starting getan10000')")

c  open file
      fname = path(1:lpath)
      ifdes = fileopenr(fname)
      if (v .ge. 3) write
     +   (6,"('getan10000: fileopenr returned ',z8)")
     +   ifdes
      if (ifdes .le. 0 .and .v .ge. 0) then
         write (6,"('getan10000: can''t find ""',a,'""')") path(1:lpath)
         goto 120
      endif

      if (v .ge. 3) write (6,"('getan10000: dstart = ',i11)") dstart

c  skip to start if macbinary
      if (dstart .gt. 0) then
         pos8 = dstart
         rv = fileseek(ifdes, pos8)
         if (rv .ne. dstart) then
            if (v .ge. 0) write
     +           (6,"('getan10000: fileseek returned ',i11,' not ',i6)")
     +           rv, dstart
            goto 120
         endif
      endif


c  read in 256 word, 512 byte file header
      rv = filereadi2(ifdes, imagetype, bigend)
      if (rv .ne. 2) goto 130
      rv = filereadi2(ifdes, headerlength, bigend)
      if (rv .ne. 2) goto 130
      rv = filereadi2(ifdes, mhorder, bigend)
      if (rv .ne. 2) goto 130
      ptype = 'i'
      fptype = .true.
      if (mhorder .eq. x'0008') then
         bufc = 'unsigned 8 bit'
         pbyte = 1
         fpbyte = .true.
         psign = 0
         fpsign = .true.
      elseif (mhorder .eq. x'0108') then
         bufc = 'signed 8 bit'
         pbyte = 1
         fpbyte = .true.
         psign = 1
         fpsign = .true.
      elseif (mhorder .eq. x'0010') then
         bufc = 'unsigned 16 bit'
         pbyte = 2
         fpbyte = .true.
         psign = 0
         fpsign = .true.
      elseif (mhorder .eq. x'0110') then
         bufc = 'signed 16 bit'
         pbyte = 2
         fpbyte = .true.
         psign = 1
         fpsign = .true.
      elseif (mhorder .eq. x'0020') then
         bufc = 'unsigned 32 bit'
         pbyte = 4
         fpbyte = .true.
         psign = 0
         fpsign = .true.
      elseif (mhorder .eq. x'0120') then
         bufc = 'signed 32 bit'
         pbyte = 4
         fpbyte = .true.
         psign = 1
         fpsign = .true.
      else
         bufc = 'unrecognised value'
         fptype = .false.
      endif

      rv = filereadi2(ifdes, hd1, bigend)
      if (rv .ne. 2) goto 130
      rv = filereadi2(ifdes, hd2, bigend)
      if (rv .ne. 2) goto 130
      imwid = hd2 - hd1 + 1
      if (imwid .gt. 0) then
         fimwid = .true.
      else
         if (rv .ge. 1) write (6,"('Problem getting image width ',i10)")
     +                        imwid
      endif
      rv = fileread(ifdes, xunits, 3)
      if (rv .ne. 3) goto 130
      rv = fileseekr(ifdes, int8(1))
      rv = fileread(ifdes, xscalec, 4)
      if (rv .ne. 4) goto 130
      xscale = an10real(xscalec)
      rv = fileread(ifdes, xzeroc, 4)
      if (rv .ne. 4) goto 130
c  set evzero and evpch from xzero and xscale
      xzero = an10real(xzeroc)
      if (xscale .eq. 0) xscale = 1
      evpch = 1000./xscale
      fevpch = .true.
      evzero = -1000.*xzero/xscale
      fevzero = .true.

c  set title from label
      rv = fileread(ifdes, imlabel, 34)
      if (rv .ne. 34) goto 130
      call ctofstring(imlabel)
      if (lnblnk(imlabel) .gt. 0) then
         title = imlabel
         ltitle = lnblnk(imlabel)
      endif

      rv = fileseek(ifdes, int8(dstart + 2*29))
      rv = filereadi2(ifdes, iscale, bigend)
      if (rv .ne. 2) goto 130
      rv = filereadi2(ifdes, nscale, bigend)
      if (rv .ne. 2) goto 130


c      i = an10real(char(x'42')//char(x'10')//char(x'00')//char(x'00'))
c      i = an10real(char(x'41')//char(x'80')//char(x'00')//char(x'00'))
c      i = an10real(char(x'41')//char(x'40')//char(x'00')//char(x'00'))
c      i = an10real(char(x'41')//char(x'10')//char(x'00')//char(x'00'))
c      i = an10real(char(x'40')//char(x'80')//char(x'00')//char(x'00'))
c      i = an10real(char(x'c2')//char(x'10')//char(x'00')//char(x'00'))

c  dump
      if (dump .eq. 1) then
         write (6,"('AN10000 image/spectrum file'/)")
         write (6,"('Image type:             ',8x,z4)") imagetype
         write (6,"('Header length:          ',i12,' words, ',
     +              i12,' bytes')") headerlength, 2*headerlength
         write (6,"('MHorder:                ',8x,z4,', ',a)") mhorder,
     +                                      bufc(1:lnblnk(bufc))
         write (6,"('First horizontal datum: ',i12)") hd1
         write (6,"('Last horizontal datum:  ',i12)") hd2
         write (6,"('x axis units:           ',9x,a3)") xunits
         write (6,"('x scale:                ',f12.5,' channels/keV')")
     +                    xscale
c         write (6,"('xscalec ',4i4)") (ichar(xscalec(i:i)), i=1,4)
         write (6,"('x zero:         channel ',f12.5)") xzero
c         write (6,"('xzeroc  ',4i4)") (ichar(xzeroc(i:i)), i=1,4)
         write (6,"('evzero:                 ',f12.5,' eV')") evzero
         write (6,"('evpch:                  ',f12.5,' eV/ch')") evpch
         write (6,"('label (title):          ""',a,'""')")
     +            imlabel(1:lnblnk(imlabel))
         write (6,"('label length:           ',i12)") lnblnk(imlabel)
         write (6,"('ISCALE:                 ',i12)") iscale
         write (6,"('NSCALE:                 ',i12)") nscale
      endif


c  move to word 36
      rv = fileseek(ifdes, int8(dstart + 2*36))

c  header variables specific to image type
      if (imagetype .eq. int2(x'd541')) then
c  X-ray spectrum
         rv = filereadi2(ifdes, livetime, bigend)
         if (rv .ne. 2) goto 130
         rv = filereadi2(ifdes, realtime, bigend)
         if (rv .ne. 2) goto 130

         rv = fileseek(ifdes, int8(dstart + 2*39))
         rv = filereadi4(ifdes, cpsec, bigend)
         if (rv .ne. 4) goto 130

         fim = .true.
         imhei = 1
         fimhei = .true.

c  set x origin (copied from el/p)
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

c  dump
         if (dump .eq. 1) then
            write (6,"(/'X-ray spectrum')")
            write (6,"('Livetime:               ',i12)") livetime
            write (6,"('Realtime:               ',i12)") realtime
            write (6,"('Counts/sec:             ',i12)") cpsec
         endif

      elseif (imagetype .le. int2(x'd580') .or.
     +        imagetype .ge. int2(x'd587')) then
c  linescan (remember d580 etc are negative)
         rv = filereadi2(ifdes, vd1, bigend)
         if (rv .ne. 2) goto 130
         rv = filereadi2(ifdes, vd2, bigend)
         if (rv .ne. 2) goto 130
         rv = fileread(ifdes, yunits, 3)
         if (rv .ne. 3) goto 130

         rv = fileseek(ifdes, int8(dstart + 2*40))
         rv = fileread(ifdes, yscalec, 4)
         if (rv .ne. 4) goto 130
         yscale = an10real(yscalec)
         rv = fileread(ifdes, yzeroc, 4)
         if (rv .ne. 4) goto 130
         yzero = an10real(yzeroc)

         fim = .true.
         imhei = 1
         fimhei = .true.

c dump
         if (dump .eq. 1) then
            write (6,"(/'Linescan')")
            write (6,"('First vertical datum:   ',i12)") vd1
            write (6,"('Last vertical datum:    ',i12)") vd2
            write (6,"('y axis units:           ',9x,a3)") yunits
            write (6,"('yscale:                 ',f12.5)") yscale
            write (6,"('yzero:                  ',f12.5)") yzero
         endif

      else
         if (v .ge. 1) write
     +            (6,"('Need to write something for this image type')")
         goto 120
      endif

c  move to start of data (headerlength is in words not bytes)
      rv = fileseek(ifdes, int8(dstart + 2*headerlength))

c  check pbyte is set
      if (.not. fpbyte) goto 120

c  data length
      ia = imwid*pbyte

c  read data
      rv = fileread(ifdes, im1dc, ia)
      if (rv .ne. ia) then
         if (v .ge. 1) write (6,"('Reading data, rv=',i10,' not ',i10)")
     +                       rv, ia
         goto 120
      endif


c  normal end of file
      if (v .ge. 3) write
     +              (6,'(''getan10000: normal end of AN10000 file'')')
      getan10000 = 0
      return

c  unexpected end of file
 110  if (v .ge. 0) write (6,'(''getan10000: unexpected end of file'')')
      getan10000 = 1
      return

c  error somewhere
 120  if (v .ge. 0) write (6,'(''getan10000: error somewhere'')')
      getan10000 = 1
      return

c  fileread error
 130  if (v .ge. 0) write (6,
     +              "('getan10000: problem reading file, rv = ',i6)") rv
      getan10000 = 1
      return

      end

c--------------------------------------------------------------------------
      function wan10000()

c  write AN10000 binary file

c  local variables

c  common variables
      include "vars.h"



c  now get on with it
      if (v .ge. 3) write (6,"('starting wan10000')")

c  open file
      fname = opath(1:lopath)
      ofdes = fileopenw(fname)
      if (v .ge. 3) write
     +   (6,"('wan10000: fileopenw returned ',z8)")
     +   ofdes
      if (ofdes .le. 0 .and. v .ge. 0) then
         write (6,"('wan10000: can''t open ""',a,'""')") opath(1:lopath)
         wan10000 = 1
         return
      endif





c  normal end of file
      wan10000 = 0

c  close file
 120  rv = fileclose(ofdes)
      if (rv .ne. 0) then
         if (v .ge. 0) write
     +              (6,"('wan10000: fileclose returned ',i6,' not 0')")
     +               rv
         wan10000 = 1
      endif

      return

      end

c-----------------------------
      function an10real(c4)
c  converts an10000 real numbers

      real*4      an10real
      character*4 c4

      real*4 te, tm

      if (ichar(c4(1:1)) .ge. 128) then
c  negative sign, strip sign
         te = ichar(c4(1:1)) - 128
      else
c  positive sign
         te = ichar(c4(1:1))
      endif
      tm = ichar(c4(2:2))*65536 + ichar(c4(3:3))*256 + ichar(c4(4:4))
      tm = tm/16777216
      an10real = 16.**(te-64.) * tm
      if (ichar(c4(1:1)) .ge. 128) an10real = -an10real

c      write (6,"('an10real: ',4z3,', ',4i4,', ',f12.5,
c     +          ': ',f8.2,', ',f14.8)")
c     +       (ichar(c4(i:i)), i=1,4), (ichar(c4(i:i)), i=1,4), an10real,
c     +       te, tm

      return

      end
