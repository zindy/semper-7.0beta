c  s6dsk.f
c  Dumps and writes Semper 6 dsk files

c  Routines
c  function s6dskdump()     Dump semper 6 picture disc
c  function ws6dsk()        Write picture to semper disc
c  function cs6dsk(dskpath, ldskpath, siz)   Create semper disc
c  function gdsks(fd)       Get size of semper disc in blocks
c  function wdsks(fd, siz)  Write size of semper disc in blocks
c  subroutine gdskdir(fd)   Get semper disc directory
c  function ws6lab()        Sorts out semper variables & creates label
c  function slabnp(picn)    Search semper dir for next free pic number
c  function slabnewpic(fd, picn, blockn)   Write semper dir for new pic
c  function gsdir(fd, slot, k)   Get number from semper disc directory slot
c  function wsdir(fd, slot, k, value)  Write number to directory slot


c--------------------------------------------------------------------------
      function s6dskdump()

c  dump Semper 6 picture disc

c  local variables
      integer*2 sum1, sum2
      integer*4 nfile, ndir, pic, addr, psiz, ai4, bi4, ssiz, i, j
      integer*8 nnfile, pos8
      logical*4 border
      real      secs

c  common variables
      include "vars.f"


c  now do it
      if (v .ge. 3) write (6,"('starting s6dskdump')")

c  open file
      fname = path(1:lpath)
      ifdes = fileopenr(fname)
      if (v .ge. 3) write
     +   (6,"('fileopen returned ',z8)")
     +   ifdes
      if (ifdes .le. 0 .and .v .ge. 0) then
         write (6,"('can''t find ""',a,'""')") path(1:lpath)
         goto 120
      endif

      write (6,"(/'Dump of ""',a,'"",')") path(1:lpath)

c  get actual file size
      ai4 = filesize(ifdes)
      write (6,"(/'Actual file size (disc space used):')")
      write (6,"('File size ',i9,' blocks, ',i10,' bytes, ',i7,
     +           ' Kbytes, ',i4,' Mbytes')")
     +      ai4/64, ai4, ai4/1024, ai4/1024/1024


c  skip to start if macbinary (I suppose it is just possible a semper
c  disc could be in macbinary...)
      if (clseek(ifdes, dstart, 's6dskdump') .ne. 0) goto 120

c  read in header block, 64 bytes
      rv = fileread(ifdes, bufc, 64)
c  rv=0 means end of file
      if (rv .eq. 0) goto 110
      if (rv .ne. 64) then
         write (6,"('s6dskdump: rv returned as ',i6,
     +              ' when reading header')") rv
         goto 120
      endif

c  write contents of header
      write (6,"(/'Header, block 1, 64 bytes (32 words)')")
      ci4(1:1) = char(0)
      ci4(2:4) = bufc(14:16)
      nfile = strtoi4(ci4, bigend)
      write (6,"('Old file size (bytes 14-16):',i12,' blocks')") nfile

      ci4(1:4) = bufc(33:36)
      nnfile = strtoi4(ci4, bigend)
      if (nnfile .lt. 0) nnfile = nnfile + 65536*65536
      write (6,"('New file size (bytes 33-36):',i12,' blocks')") nnfile

      if (nnfile .lt. nfile) nnfile = nfile

      write (6,"('File size ',i9,' blocks, ',i10,' bytes, ',i7,
     +           ' Kbytes, ',i4,' Mbytes')")
     +      nnfile, nnfile*64, nnfile*64/1024, nnfile*64/1024/1024

      ndir = strtoi2(bufc(17:18), bigend)
      write (6,"('Directory size',i5,' blocks, ',i10,' bytes, ',i7,
     +           ' slots')")
     +      ndir, ndir*64, ndir*64/8

c  checksums
      sum1 = ichar(bufc(17:17)) + ichar(bufc(18:18)) + 90
      sum1 = and(sum1, 255)
      sum2 = ichar(bufc(15:15)) + ichar(bufc(16:16)) + 165
      sum2 = and(sum2, 255)
      write (6,"(/'Byte 19, checksum:',i4,', should be ',i4)")
     +      ichar(bufc(19:19)), sum1
      write (6,"('Byte 20, checksum:',i4,', should be ',i4)")
     +      ichar(bufc(20:20)), sum2

c  date and time
      write (6,"(/'Date:',i5,'/',i3,'/',i3)") ichar(bufc(21:21))+1900,
     +      ichar(bufc(22:22)), ichar(bufc(23:23))
      secs = ichar(bufc(26:26))
      secs = secs + float(ichar(bufc(27:27)))/100.0
      write (6,"('Time:',i4,':',i3,':',f6.2)") ichar(bufc(24:24)),
     +      ichar(bufc(25:25)), secs

c  check rest of header
      write (6,"(/'Rest of header (bytes 37 to 64), should be zero')")
c      do i=9,32
c         if (bufi2(i) .ne. 0) write
c     +          (6,"('word',i3,' contains ',i6,' (',z4,'h)')")
c     +          i, bufi2(i), bufi2(i)
c      enddo
      do i=37,63,2
         i2 = strtoi2(bufc(i:i+1), bigend)
         if (i2 .ne. 0) write
     +          (6,"('word',i3,' contains ',i6,' (',2z2,'h)')")
     +          (i+1)/2, i2, (bufc(j:j),j=i,i+1)
      enddo

c  read in directory
      i = 64*ndir
      rv = fileread(ifdes, bufc, i)
c  rv=0 means end of file
      if (rv .eq. 0) goto 100
      if (rv .ne. i) then
         write (6,"('s6dskdump: rv returned as ',i6,
     +              ' when reading header')") rv
         goto 110
      endif

c  Get byte order
c  will not work if first picture has pic=0 ie free
      pic = strtoi4(bufc(1:4), bigend)
      addr = strtoi4(bufc(5:8), bigend)
      if (pic .ge. 0 .and. pic .le. 1000 .and. addr .ge. 0) then
c  bigendian
         border = bigend
         write (6,"(/'Big endian (Mac) byte order')")
      else
c  littleendian
         border = littleend
         write (6,"(/'Little endian (PC) byte order')")
      endif

c  write directory
      write (6,"(/'Directory, space for',i5,' slots')") ndir*64/8
      write (6,"( 'starts block 2, ends block',i5)") ndir+1
      write (6,"(/'slot contents  start addr dir length     pic size',
     +            '     byte/pix pic length')")
      write (6,"( '     (pic no.)  (blocks)   (blocks)')")
      do i=0, ndir*64/8-1
c         pic = bufi4(2*i)
c         addr = bufi4(2*i+1)
c         psiz = bufi4(2*i+3) - addr
         pic = strtoi4(bufc(8*i+1:8*i+4), border)
         addr = strtoi4(bufc(8*i+5:8*i+8), border)
         psiz = strtoi4(bufc(8*i+13:8*i+16), border) - addr
c  get picture label
c  seek to position of label
         if (pic .ne. 1000 .and. pic .ne. 0) then
            ai4 = (addr-1)*64
            if (v .ge. 3) write (6,"('seeking to ',i11)") ai4
            pos8 = ai4
            rv = fileseek(ifdes, pos8)
            if (rv .ne. ai4) then
               if (v .ge. 0) write
     +            (6,"('s6dskdump: fileseek returned ',i11,' not ',i6)")
     +              rv, ai4
               s6dskdump = 1
               goto 120
            endif
c  read in label into picture area
            ai4 = 256
            rv = fileread(ifdes, im1dc, ai4)
c  rv=0 means end of file
            if (rv .eq. 0) goto 110
            if (rv .ne. ai4) then
               write (6,"('s6dskdump: rv returned as ',i6,
     +                    ' when reading header')") rv
               goto 110
            endif
c  set variables from label. Always big endian
c            ncol = im1di2(3)
c            nrow = im1di2(4)
c            nlay = im1di2(5)
            ncol = strtoi2(im1dcs(7:8), bigend)
            nrow = strtoi2(im1dcs(9:10), bigend)
            nlay = strtoi2(im1dcs(11:12), bigend)
            iform = im1di1(19)
            if (iform .eq. 0) then
               bi4 = 1
            elseif (iform .eq. 1) then
               bi4 = 2
            elseif (iform .eq. 2) then
               bi4 = 4
            elseif (iform .eq. 3) then
               bi4 = 8
            else
               write (6,"('Oops, iform = ',i6)") iform
               bi4 = 1
            endif
            ssiz = roundup(ncol*bi4, 64)*nrow*nlay/64 + 4
         endif

c  write directory contents
         if (pic .eq. 0) then
            write (6,"(i4,5x,'free',i10,i10)") i+1, addr, psiz
         elseif (pic .eq. 1000) then
            write (6,"(i4,6x,'end',i10)") i+1, addr
            goto 10
         else
            if (im1dcs(1:6) .eq. 'Semper') then
               write (6,"(i4,i9,i10,i10,4x,i5,',',i5,',',i5,i6,i11)")
     +               i+1, pic, addr, psiz, ncol, nrow, nlay, bi4, ssiz
               if (psiz .ne. ssiz) write
     +           (6,"('directory and picture sizes are not the same')")
            else
               write (6,"(i4,i9,i10,i10,4x,
     +                    'label does not contain ""Semper""')")
     +               i+1, pic, addr, psiz
            endif
         endif
      enddo
 10   continue


c  normal end of file
 100  if (v .ge. 3) write (6,"('s6dskdump: normal end of file')")
      s6dskdump = 0
      return

c  unexpected end of file
 110  if (v .ge. 3) write (6,"('s6dskdump: unexpected end of file')")
      s6dskdump = 1
      return

c  error somewhere
 120  if (v .ge. 0) write (6,"('s6dskdump: error somewhere')")
      s6dskdump = 1
      return

      end

c--------------------------------------------------------------------------
      function ws6dsk()

c  write picture to semper disc

c  local variables
      integer*4 ai4, bi4, bneed
      integer*4 ncoli4, i, j, k

c  common variables
      include "vars.f"

c  open semper disc (dskopath)
c  get directory
c  work out how many blocks picture will take
c  check how many blocks free
c  if not enough free
c     if allowed to enlarge then enlarge with wdsks
c     otherwise return an error
c  write picture & update directory
c  close

      if (v .ge. 3) write (6,"('ws6dsk: starting...')")

c  open semper disc
      ofdes = copenrw(dskopath(1:ldskopath), 'ws6dsk')
      if (ofdes .le. 0) then
         if (v .ge. 0) write (6,"('ws6dsk: can't open semper disc ',a)")
     +                       dskopath(1:ldskopath)
         goto 120
      endif

c  get directory
      call gdskdir(ofdes)
      if (.not. fsemdir) goto 120

c  find next free picture number
      ai4 = slabnp(ndskpic)
      if (ai4 .le. 0) then
         if (v .ge. 0) write (6,"(
     +         'ws6dsk: can''t find a free picture above ',i6)") ndskpic
         goto 120
      endif
      ndskpic = ai4

c  work out number of blocks required for picture
      if (psign .eq. 1 .and. ptype .eq. 'i' .and. pbyte .eq. 1) then
c        signed byte -> i2
         bneed = roundup(imwid*pbyte*2, 64)*imhei/64 + 4
      elseif (psign .eq. 0 .and. ptype .eq. 'i' .and. pbyte .eq. 2) then
c        unsigned i2 -> fp
         bneed = roundup(imwid*pbyte*2, 64)*imhei/64 + 4
      elseif (ptype .eq. 'f' .and. pbyte .eq. 8) then
c        real*8 -> fp
         bneed = roundup(imwid*pbyte/2, 64)*imhei/64 + 4
      elseif (ptype .eq. 'c' .and. pbyte .eq. 16) then
c        complex*16 -> complex
         bneed = roundup(imwid*pbyte/2, 64)*imhei/64 + 4
      elseif (ptype .eq. 'r') then
c        rgb -> 3 layer byte
         bneed = roundup(imwid*pbyte, 64)*imhei*3/64 + 4
      else
         bneed = roundup(imwid*pbyte, 64)*imhei/64 + 4
      endif
      if (v .ge. 3) write (6,"(
     +        'ws6dsk: semper blocks needed for this pic ',i8)") bneed

c  check to see whether there is enough space in the semper disc
      if (semfree .lt. bneed) then
c  not enough space, so enlarge (if possible)
c  size needed
         bi4 = semdflen + bneed - semfree
         if (v .ge. 3) write (6,"(
     +        'ws6dsk: semper blocks free in dsk ',i8)") semfree
         ws6dsk = wdsks(ofdes, bi4)
         if (ws6dsk .ne. 0) then
            if (v .ge. 3) write (6,"(
     +      'ws6dsk: cannot enlarge semper disc by ',i8,' blocks')") bi4
            goto 120
         endif
      endif

c  OK, now have enough space to write picture
c  ... but just check...
      if (semfree .lt. bneed) then
         if (v .ge. 0) write (6,"('ws6dsk: not enough space, ',i8,
     +       ' blocks needed, ',i8,' blocks left')") bneed, semfree
         goto 120
      endif

c  sort out semper variables etc and picture label
      if (ws6lab() .ne. 0) goto 120

c  write picture label
c  seek to start of free space
      if (clseek(ofdes, (semlastp-1)*64, 'ws6dsk') .ne. 0) goto 120

c  write label
      if (cwrite(ofdes, label, 256, 'ws6dsk') .ne. 0) goto 120


c  write picture... need i4 version of icol to pass to subroutines
      ncoli4 = ncol

c  write picture - need to do for all types
      if (ptype .eq. 'i') then
         if (pbyte .eq. 1) then
            if (psign .eq. 0) then
c   unsigned byte -> byte
               if (v .ge. 3) write (6,"('unsigned byte -> byte')")
c  for some horrible reason byte pictures only have even length records
c  (but not for semper discs)
               if (nrow .eq. 1) then
                  if (cwri64(ofdes, im1dc, ncoli4, 'ws6dsk') .ne. 0)
     +                   goto 120
               else
                  do j=0,nrow-1
                     if (cwri64(ofdes, imagc(0,j), ncoli4, 'ws6dsk')
     +                   .ne. 0) goto 120
                  enddo
               endif
            else
c   signed byte -> integer
               if (v .ge. 3) write (6,"('signed byte -> integer')")
               if (nrow .eq. 1) then
                  do i=0,ncol-1
                     bufi2(i) = im1di1(i)
                  enddo
                  if (cwri64(ofdes, bufc, ncoli4*2, 'ws6dsk') .ne. 0)
     +                   goto 120
               else
                  do j=0,nrow-1
                     do i=0,ncol-1
                        bufi2(i) = imagi1(i,j)
                     enddo
                     if (cwri64(ofdes, bufc, ncoli4*2, 'ws6dsk') .ne. 0)
     +                   goto 120
                  enddo
               endif
            endif
         elseif (pbyte .eq. 2) then
            if (psign .eq. 0) then
c   unsigned i2 -> fp
               if (v .ge. 3) write (6,"('unsigned i2 -> fp')")
               if (nrow .eq. 1) then
                  do i=0,ncol-1
                     if (im1di2(i) .ge. 0) then
                        bufr(i) = im1di2(i)
                     else
                        bufr(i) = 65536.0 + im1di2(i)
                     endif
                  enddo
                  if (cwri64(ofdes, bufc, ncoli4*4, 'ws6dsk') .ne. 0)
     +                   goto 120
               else
                  do j=0,nrow-1
                     do i=0,ncol-1
                        if (imagi2(i,j) .ge. 0) then
                           bufr(i) = imagi2(i,j)
                        else
                           bufr(i) = 65536.0 + imagi2(i,j)
                        endif
                     enddo
                     if (cwri64(ofdes, bufc, ncoli4*4, 'ws6dsk') .ne. 0)
     +                   goto 120
                  enddo
               endif
            else
c   signed i2 -> integer
               if (v .ge. 3) write (6,"('signed i2 -> integer')")
               if (nrow .eq. 1) then
                  if (cwri64(ofdes, im1dc, ncoli4*2, 'ws6dsk') .ne. 0)
     +                   goto 120
               else
                  do j=0,nrow-1
                     if (cwri64(ofdes, imagc(0,j), ncoli4*2, 'ws6dsk')
     +                   .ne. 0) goto 120
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
                  if (cwri64(ofdes, bufc, ncoli4*4, 'ws6dsk') .ne. 0)
     +                   goto 120
               else
                  do j=0,nrow-1
                     do i=0,ncol-1
                        if (imagi4(i,j) .ge. 0) then
                           bufr(i) = imagi4(i,j)
                        else
                           bufr(i) = 4294967296. + imagi4(i,j)
                        endif
                     enddo
                     if (cwri64(ofdes, bufc, ncoli4*4, 'ws6dsk') .ne. 0)
     +                   goto 120
                  enddo
               endif
            else
c   signed i4 -> fp
               if (v .ge. 3) write (6,"('signed i4 -> fp')")
               if (nrow .eq. 1) then
                  do i=0,ncol-1
                     bufr(i) = im1di4(i)
                  enddo
                  if (cwri64(ofdes, bufc, ncoli4*4, 'ws6dsk') .ne. 0)
     +                   goto 120
               else
                  do j=0,nrow-1
                     do i=0,ncol-1
                        bufr(i) = imagi4(i,j)
                     enddo
                     if (cwri64(ofdes, bufc, ncoli4*4, 'ws6dsk') .ne. 0)
     +                   goto 120
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
               if (cwri64(ofdes, im1dc, ncoli4*4, 'ws6dsk') .ne. 0)
     +                   goto 120
            else
               do j=0,nrow-1
                  if (cwri64(ofdes, imagc(0,j), ncoli4*4, 'ws6dsk')
     +                   .ne. 0) goto 120
               enddo
            endif
         elseif (pbyte .eq. 8) then
c   real*8 -> fp
            if (v .ge. 3) write (6,"('real*8 -> fp')")
            if (nrow .eq. 1) then
               do i=0,ncol-1
                  bufr(i) = im1dr8(i)
               enddo
               if (cwri64(ofdes, bufc, ncoli4*4, 'ws6dsk') .ne. 0)
     +                   goto 120
            else
               do j=0,nrow-1
                  do i=0,ncol-1
                     bufr(i) = imagr8(i,j)
                  enddo
                  if (cwri64(ofdes, bufc, ncoli4*4, 'ws6dsk') .ne. 0)
     +                   goto 120
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
               if (cwri64(ofdes, im1dc, ncoli4*8, 'ws6dsk') .ne. 0)
     +                   goto 120
            else
               do j=0,nrow-1
                  if (cwri64(ofdes, imagc(0,j), ncoli4*8, 'ws6dsk')
     +                   .ne. 0) goto 120
               enddo
            endif
         elseif (pbyte .eq. 16) then
c   complex*16 -> complex
            if (v .ge. 3) write (6,"('complex*16 -> fp')")
            if (nrow .eq. 1) then
               do i=0,ncol*2-1
                  bufr(i) = im1dr8(i)
               enddo
               if (cwri64(ofdes, bufc, ncoli4*8, 'ws6dsk') .ne. 0)
     +                   goto 120
            else
               do j=0,nrow-1
                  do i=0,ncol*2-1
                     bufr(i) = imagr8(i,j)
                  enddo
                  if (cwri64(ofdes, bufc, ncoli4*8, 'ws6dsk')
     +                   .ne. 0) goto 120
               enddo
            endif
         else
            if (v .ge. 0) write (6,"('ws6rwb: pbyte=',i6)") pbyte
            goto 120
         endif
      elseif (ptype .eq. 'r') then
c   rgb -> 3 layer byte
c   remember odd row length byte pics have to be even...
c   (but not for semper discs)
         if (v .ge. 3) write (6,"('rgb -> 3 layer byte')")
         if (nrow .eq. 1) then
            do k=0,nlay-1
c  was...
c               if (cwri64(ofdes, im1dc2(0,k), ncoli4, 'ws6dsk') .ne. 0)
               if (cwri64(ofdes, im1dcc(0,k), ncoli4, 'ws6dsk') .ne. 0)
     +                   goto 120
            enddo
         else
            do k=0,nlay-1
               do j=0,nrow-1
c  was...
c                  if (cwri64(ofdes, imagc2(0,j,k), ncoli4, 'ws6dsk')
                  if (cwri64(ofdes, imagcc(0,j,k), ncoli4, 'ws6dsk')
     +                   .ne. 0) goto 120
               enddo
            enddo
         endif
      else
         if (v .ge. 0) write (6,"('ws6rwb: unknown ptype=""',a,'""')")
     +                       ptype
         goto 120
      endif


c  update directory
c  after writing, next free block should be at semlastp + bneed
      if (slabnewpic(ofdes, ndskpic, semlastp + bneed) .ne. 0) goto 120

c  finished normally, close semper disc
 100  ws6dsk = 0
      if (v .ge. 3) write (6,"('ws6dsk: finishing...')")

      if (fileclose(ofdes) .ne. 0) then
         if (v .ge. 0) write (6,"('ws6dsk: problem closing file')")
         ws6dsk = 1
      endif

      return

c  finished with error, close semper disc
 120  ws6dsk = 1
      if (v .ge. 3) write (6,"('ws6dsk: error somewhere')")

      if (fileclose(ofdes) .ne. 0) then
         if (v .ge. 0) write (6,"('ws6dsk: problem closing file')")
         ws6dsk = 1
      endif

      return

      end

c--------------------------------------------------------------------------
      function cs6dsk(dskpath, ldskpath, siz)

c  Creates semper disc

c  Creates semper disc equal to or greater than size siz KB
c  with filename dskpath and standard 2000 slots

c    siz    is the size asked for in KB
c    dsksb  is the actual disc size created in bytes

c  functions (fd = file descriptor, file assumed open)
c    gdsks(fd)   get disc size (blocks) from semper disc
c    gdskdir(fd) get disc directory into semdir(2,2008)
c    wdsks(fd, siz)   write disc size (blocks) to semper disc
c    filesize(fd)     get actual file size in bytes (returns i8)

c  local variables
      character*(*) dskpath
      integer*4 ldskpath, ai4, dsksb, siz, i, ii
      integer*8 pos8
      logical*4 dskorder
      integer*4 datear(3), timear(3)
      integer*2 sum1, sum2

c  common variables
      include "vars.f"

      cs6dsk = 0

c      v = 3

c  now get on with it

c  dskorder is the byte order to write the semper disc in
c  for now write only in machine order
      dskorder = progbyteorder()

c  first check to see whether it exists, if it does, check size
c  open it for read and write & check return value
      if (v .ge. 3) write
     +         (6,"('checking for existing semper disc ""',a,'""')")
     +                    dskpath(1:ldskpath)
      ofdes = copenrw(dskpath(1:ldskpath), 'cs6dsk')

c  ofdes >0 means it exists
      if (ofdes .gt. 0) then
         if (v .ge. 3) write (6,
     +         "('cs6dsk: semper disc already exists, fd = ',i4)") ofdes
         call gdskdir(ofdes)
         if (.not. fsemdir) then
            if (v .gt. 0) write  (6,
     +            "('cs6dsk: unable to get semper disc directory')")
            cs6dsk = 1
            goto 200
         endif
         if (fdskenl) then
c  existing semper disc can be enlarged
            if (semdflen .ge. siz*1024/64) then
c  existing disc already big enough
               if (v .ge. 3) write (6,
     +        "('cs6dsk: existing semper disc correct size or bigger')")
            else
               if (v .ge. 3) write (6,
     +            "('cs6dsk: existing semper disc needs enlarging')")
               if (wdsks(ofdes, siz*1024/64) .ne. 0) then
                  if (v .ge. 0) write (6,
     +                     "('cs6dsk: couldn''t enlarge semper disc')")
                  cs6dsk = 1
               endif
            endif
         endif
         goto 200
      endif

c  disc doesn't exist, so create one
      if (v .ge. 3) write (6,"('creating semper disc ""',a,'""')")
     +                    dskpath(1:ldskpath)


c  check disc size and convert to bytes in dsksb
      if (siz .gt. x'7fffffff'/16) then
         if (v .ge. 1) write (6,"('semper disc size ',i11,', ',z8,
     +                 'h too big, max 128GB')") siz, siz
         dsksb = x'7fffffff'*64
      else
         dsksb = siz*1024
      endif
c  minimum disc size must leave space for directory
      dsksb = max(dsksb, 16128)

c  verify things
      if (v .ge. 3) then
         write (6,"('semper disc size asked for ',i11,' Kbytes')")
     +               siz
         write (6,"('semper disc size used      ',i11,' bytes')") dsksb
         write (6,"('semper file name ""',a,'""')") dskpath(1:ldskpath)
      endif

c  open semper disc file
      fname = dskpath(1:ldskpath)
      ofdes = fileopenw(fname)
      if (v .ge. 3) write
     +   (6,"('cs6dsk: open returned ',z8)") ofdes
      if (ofdes .le. 0 .and. v .ge. 0) then
         write (6,"('cs6dsk: can''t open ""',a,'""')")
     +         dskpath(1:ldskpath)
         goto 200
      endif

c  write header block into bufc
c  bytes 1-13  semper.disc + version
      bufc(1:11) = 'Semper.disc'
      bufc(12:12) = char(6)
      bufc(13:13) = char(1)
c  bytes 14-16  old file size in (64 byte) blocks
      ii = min(dsksb/64, x'ffffff')
      ci4 = i4tostr(ii, bigend)
      bufc(14:16) = ci4(2:4)
      if (v .ge. 3) write
     +   (6,"('i4 = ',i11,', ci4 = ',z8,', bufc(14:16) = ',z6)")
     +   strtoi4(ci4,bigend), ci4, bufc(14:16)
c  bytes 33-36  new file size in (64 byte) blocks
      ci4 = i4tostr(dsksb/64, bigend)
      bufc(33:36) = ci4(1:4)
c  bytes 17-18  directory size in blocks (251, FBh) for 2000 slots
c      i2 = 251 (you can't put 251 in function as 251 is i4)
      bufc(17:18) = i2tostr(int2(251), bigend)

c  bytes 19,20  checksums
      sum1 = ichar(bufc(17:17)) + ichar(bufc(18:18)) + 90
      bufc(19:19) = char(and(sum1, 255))
      sum2 = ichar(bufc(15:15)) + ichar(bufc(16:16)) + 165
      bufc(20:20) = char(and(sum2, 255))

c  bytes 21-27  date and time
c  datear(1) = day (1-31)
c  datear(2) = month (1-12)
c  datear(3) = year (4 digits)
c  timear(1) = hour
c  timear(2) = mins
c  timear(3) = secs
      call idate(datear)
      call itime(timear)
      bufc(21:21) = char(datear(3) - 1900)
      bufc(22:22) = char(datear(2))
      bufc(23:23) = char(datear(1))
      bufc(24:24) = char(timear(1))
      bufc(25:25) = char(timear(2))
      bufc(26:26) = char(timear(3))
      bufc(27:27) = char(0)

c  zero the rest of the block
      do i=28,32
         bufc(i:i) = char(0)
      enddo
      do i=37,64
         bufc(i:i) = char(0)
      enddo

c  write directory into bufc
c  one empty picture, one for last used entry
c      bufi4(64/4) = 0
c      bufi4(68/4) = 253
c      bufi4(72/4) = 1000
c      bufi4(76/4) = dsksb/64 + 1
      bufc(64+1:64+4) = i4tostr(0, dskorder)
      bufc(68+1:68+4) = i4tostr(253, dskorder)
      bufc(72+1:72+4) = i4tostr(1000, dskorder)
      bufc(76+1:76+4) = i4tostr(dsksb/64 + 1, dskorder)
c  zero rest of directory - not necessary, but well why not...
      do i=80/4,16128/4
         bufi4(i) = 0
      enddo

      if (v .ge. 3) write
     +   (6,"('bufc(1:16)  =',4(' ',z8))")
     +   bufc(1:4), bufc(5:8), bufc(9:12), bufc(13:16)
      if (v .ge. 3) write
     +   (6,"('bufc(17:32) =',4(' ',z8))")
     +   bufc(17:20), bufc(21:24), bufc(25:28), bufc(29:32)
      if (v .ge. 3) write
     +   (6,"('bufc(65:80) =',4(' ',z8))")
     +   bufc(65:68), bufc(69:72), bufc(73:76), bufc(77:80)
      if (v .ge. 3) write
     +   (6,"('bufc(65:80) =',4(' ',i8),' ints')")
     +   bufc(65:68), bufc(69:72), bufc(73:76), bufc(77:80)

c  write bufc to file
      if (v .ge. 3) write
     +              (6,"('writing semper disc header & directory')")
      rv = filewrite(ofdes, bufc, 16128)
      if (rv .lt. 0) then
         if (v .ge. 0) write
     +      (6,"('cs6dsk: write returned ',i6)") rv
         cs6dsk = 1
         goto 200
      endif

c  seek to end of file if dsksb is greater than current file size
      if (dsksb .gt. filesize(ofdes)) then
         ai4 = dsksb - 1
         pos8 = ai4
         rv = fileseek(ofdes, pos8)
         if (rv .ne. ai4) then
            if (v .ge. 0) write
     +           (6,"('cs6dsk: fileseek returned ',i11,' not ',i6)")
     +           rv, ai4
            cs6dsk = 1
            goto 200
         endif

c  write a byte at end of file
c  you have to write something after seeking otherwise the file is not
c  extended.
c  does filewrite only write at end of file on linux?
         rv = filewrite(ofdes, char(0), 1)
         if (rv .lt. 0) then
            if (v .ge. 0) write
     +           (6,"('cs6dsk: write returned ',i6)") rv
            cs6dsk = 1
            goto 200
         endif
      endif

c  having created this disc, can enlarge it
      fdskenl = .true.

c  close file
 200  rv = fileclose(ofdes)
      if (rv .ne. 0) then
         if (v .ge. 0) write
     +       (6,"('cs6dsk: close returned ',i6,' not 0')") rv
         cs6dsk = 1
      endif
      if (v .ge. 3) write
     +       (6,"('cs6dsk: new semper disc closed')")

      return

      end

c--------------------------------------------------------------------------
      function gdsks(fd)

c  gets size of semper disc in blocks assumed to be already open with fd
c  if there is a problem returns -1
c  leaves file pointer after file size
c  currently this function is i4, will be problems with negative numbers
c  if file size is too large

c  local variables
      integer*4 ai4, fd, i, j, nfile
      integer*8 pos8, nnfile

c  common variables
      include "vars.f"


c  so dump the lot...
      if (v .ge. 3) then
         if (clseek(fd, 0, 'gdsks') .ne. 0) then
            gdsks = -1
            return
         endif
         ai4 = 4
         do i=0,5
            rv = fileread(fd, ci4, ai4)
            if (rv .ne. ai4) write (6,"('gdsks: rv = ',i8,
     +                         ', should be ',i8)") rv, ai4
            write (6,"('gdsks: ',i3,': ',4z2)") i*4, (ci4(j:j),j=1,4)
         enddo
      endif

c  seek to start of old file size at byte 14 (first byte = 1)
      ai4 = dstart + 13
      pos8 = ai4
      rv = fileseek(fd, pos8)
      if (rv .ne. ai4) then
         if (v .ge. 0) write
     +        (6,"('gdsks: fileseek returned ',i11,' not ',i6)")
     +        rv, ai4
         gdsks = -1
         return
      endif

c  get old file size (bytes 14-16)
c  set ci4 and hence i4
      ci4(1:1) = char(0)
c  read in the 3 bytes of file size
      ai4 = 3
      rv = fileread(fd, ci4(2:4), ai4)
c  rv=0 means end of file
      if (rv .ne. ai4) then
         write (6,"('gdsks: rv returned as ',i6,
     +              ' when reading disc size')") rv
         gdsks = -1
         return
      endif

      nfile = strtoi4(ci4, bigend)
c      write (6,"('Old size: ',i12)") nfile

c  seek to start of new file size at byte 33 (first byte = 1)
      ai4 = dstart + 32
      pos8 = ai4
      rv = fileseek(fd, pos8)
      if (rv .ne. ai4) then
         if (v .ge. 0) write
     +        (6,"('gdsks: fileseek returned ',i11,' not ',i6)")
     +        rv, ai4
         gdsks = -1
         return
      endif

c  get new file size (bytes 33-36)
c  read in the 4 bytes of file size
      ai4 = 4
      rv = fileread(fd, ci4(1:4), ai4)
c  rv=0 means end of file
      if (rv .ne. ai4) then
         write (6,"('gdsks: rv returned as ',i6,
     +              ' when reading disc size')") rv
         gdsks = -1
         return
      endif

      nnfile = strtoi4(ci4, bigend)
c      write (6,"('New size: ',i12)") nnfile
      if (nnfile .lt. 0) nnfile = nnfile + 65536*65536
c      write (6,"('New size after if: ',i12)") nnfile

c  use old value if new value is less than old (ie usually zero)
      if (nnfile .lt. nfile) nnfile = nfile

c  return disc size in blocks
      gdsks = nnfile
c      write (6,"('Final size: ',i12)") gdsks


      if (v .ge. 3) write (6,"('gdsks: recorded disc size ',i8,
     +           ', from fd ',i4)") gdsks, fd

c  verify...
      if (v .ge. 3) write (6,
     +        "('gdsks: semper disc size recorded in file'
     +         /'File size ',i9,' blocks, ',i10,' bytes, ',i7,
     +           ' Kbytes, ',i4,' Mbytes')")
     +     gdsks, gdsks*64, gdsks*64/1024, gdsks*64/1024/1024


      return

      end

c--------------------------------------------------------------------------
      function wdsks(fd, siz)

c  writes size of semper disc in blocks assumed to be already open with fd
c  if there is a problem returns 1
c  leaves file pointer after file size

c  can't at present shrink a semper disc because I don't know how to
c  truncate a file to a smaller size. Otherwise everything else here should
c  work for shrinking

c  local variables
      integer*4 fd, siz, ii
      logical*4 dskorder
      integer*2 sum2
      character*1 ch1, ch2

c  common variables
      include "vars.f"


      if (v .ge. 3) write (6,"('wdsks: starting...')")

c  dskorder is the byte order to write the semper disc in
c  for now write only in machine order
      dskorder = progbyteorder()

c  check whether we are allowed to modify disc size
      if (.not. fdskenl) then
         if (v .ge. 0) write (6,"('wdsks: can''t enlarge')")
         wdsks = 1
         return
      endif


c  check size
      if (siz .gt. x'ffffff') then
         if (v .ge. 0) write (6,"('Warning: ',i10,'k (',i10,
     +           ') blocks is greater than maximum (',i9,')'
     +           /'for older versions of Semper')")
     +           siz/16, siz, x'ffffff'
      endif

      if (siz .gt. x'7fffffff') then
         if (v .ge. 0) write (6,"('wdsks: size trying to write (',i9,
     +           ') blocks is greater than maximum (',i9,')')")
     +           siz, x'7fffffff'
         wdsks = 1
         return
      endif


c  get directory
      call gdskdir(fd)
      if (.not. fsemdir) then
         wdsks = 1
         if (v .ge. 3) write (6,"(
     +           'wdsks: problem getting semper disc directory')")
         return
      endif
      if (v .ge. 3) write (6,"('wdsks: got directory, first time')")

c  check space to shrink... (can't shrink, so don't allow at moment)
c      if (siz .lt. semlastp) then
c         if (v .ge. 3) write (6,"('wdsks: trying to shrink to ',i8,
c     +          ' blocks, smallest possible size is ',i8,' blocks')")
c     +                 siz, semlastp
c         siz = semlastp
c         wdsks = 0
c         return
c      endif

      if (siz .le. semdflen) then
         if (v .ge. 3) write (6,"('wdsks: trying to shrink to ',i8,
     +          ' blocks, smallest possible size is ',i8,' blocks')")
     +                 siz, semdflen
         siz = semdflen
         wdsks = 0
         return
      endif

c  enlarge file
c  seek to end of file
      if (v .ge. 3) write (6,"('wdsks: enlarging, seeking...')")
      wdsks = clseek(fd, siz*64 - 1, 'wdsks')
      if (wdsks .ne. 0) return

c  write a byte at end of file
c  you have to write something after seeking otherwise the file is not
c  extended.
      if (v .ge. 3) write (6,"('wdsks: enlarging, writing...')")
      wdsks = cwrite(fd, char(0), 1, 'wdsks')
      if (wdsks .ne. 0) return

c  write file size: old size, bytes 14-16
c  seek to start of file size at byte 14 (first byte = 1)
      if (v .ge. 3) write (6,"(
     +              'wdsks: enlarging done, writing old file size')")
      wdsks = clseek(fd, 13, 'wdsks')
      if (wdsks .ne. 0) return

      if (siz .gt. x'ffffff') then
         ci4 = i4tostr(x'ffffff', bigend)
      else
         ci4 = i4tostr(siz, bigend)
      endif
c  write the 3 bytes of file size
      wdsks = cwrite(fd, ci4(2:4), 3, 'wdsks')
      if (wdsks .ne. 0) return

c  seek to byte 33 for new file size at bytes 33-36
      if (v .ge. 3) write (6,"(
     +              'wdsks: writing new file size')")
      wdsks = clseek(fd, 32, 'wdsks')
      if (wdsks .ne. 0) return

      ci4 = i4tostr(siz, bigend)
c  write the 4 bytes of file size
      wdsks = cwrite(fd, ci4(1:4), 4, 'wdsks')
      if (wdsks .ne. 0) return

c  write disc size checksum
      wdsks = clseek(fd, 14, 'wdsks')
      if (wdsks .ne. 0) return
      ii = fileread(fd, ch1, 1)
      ii = fileread(fd, ch2, 1)
      sum2 = ichar(ch1) + ichar(ch2) + 165
      ch1 = char(and(sum2, 255))
      wdsks = clseek(fd, 19, 'wdsks')
      if (wdsks .ne. 0) return
      ii = filewrite(fd, ch1, 1)

c  if last picture is empty...
      if (semdir(1,semdlsd-1) .gt. 0 .and.
     +    semdir(1,semdlsd-1) .lt. 1000) then
         if (semdlen .lt. (semdlsd+1)*8/64) then
c  not enough directory space for another entry
            if (v .ge. 0) write (6,
     +         "('wdsks: not enough directory space',
     +           ' for another directory entry')")
            wdsks = 1
            return
         endif
c  seek & write entry for empty space
         wdsks = clseek(fd, dstart + 64 + (semdlsd-1)*8, 'wdsks')
         if (wdsks .ne. 0) return
         i4 = 0
         wdsks = cwrite(fd, ci4, 4, 'wdsks')
         if (wdsks .ne. 0) return
         semdlsd = semdlsd + 1
      elseif (semdir(1,semdlsd-1) .ne. 0) then
         if (v .ge. 0) write (6,"('wdsks: panic... this directory',
     +                            ' entry should not be ',i6)")
     +                       semdir(1,semdlsd-1)
         wdsks = 1
         return
      endif

c  write last directory entry...
c  seek to end of directory
      wdsks = clseek(fd, dstart + 64 + (semdlsd-1)*8, 'wdsks')
      if (wdsks .ne. 0) return
c      i4 = 1000
      ci4 = i4tostr(1000, dskorder)
      wdsks = cwrite(fd, ci4, 4, 'wdsks')
      if (wdsks .ne. 0) return
c      i4 = siz + 1
      ci4 = i4tostr(siz + 1, dskorder)
      wdsks = cwrite(fd, ci4, 4, 'wdsks')
      if (wdsks .ne. 0) return

c  get directory again
      call gdskdir(fd)

c  verify...
      if (v .ge. 3) write (6,
     +        "('wdsks: semper disc size recorded in file'
     +         /'File size ',i9,' blocks, ',i10,' bytes, ',i7,
     +           ' Kbytes, ',i4,' Mbytes')")
     +    semdflen, semdflen*64, semdflen*64/1024, semdflen*64/1024/1024

      return

      end

c--------------------------------------------------------------------------
      subroutine gdskdir(fd)

c  gets semper disc directory assumed to be already open with fd
c  if there is a problem returns -1
c  sets fsemdir .true. if sucessful
c  leaves file pointer after end of directory

c  local variables
      integer*4 ai4, fd, i
      integer*8 pos8
      logical*4 dskorder

c  common variables
      include "vars.f"


      if (v .ge. 3) write (6,"('gdskdir 1: starting...')")

      fsemdir = .false.


c  dskorder is the byte order to write the semper disc in
c  for now write only in machine order
      dskorder = progbyteorder()

c  get semper disc file size from header and actual file size
      if (v .ge. 3) write (6,"('gdskdir 2: fd = ',i4)") fd
      semflen = filesize(fd)
      semdflen = gdsks(fd)
      if (semdflen*64 .ne. semflen) then
         if (v .ge. 0) write (6,
     +          "('gdskdir 3: existing semper disc has ',i10,
     +           ' blocks'/'(',i12,' bytes) recorded but file size is '
     +           ,i12)") semdflen, semdflen*64, semflen
         if (semdflen*64 .gt. semflen) then
            fsemdir = .false.
            return
         endif
      endif

c  seek to start of directory size at byte 17 (first byte = 1)
      ai4 = dstart + 16
      pos8 = ai4
      rv = fileseek(fd, pos8)
      if (rv .ne. ai4) then
         if (v .ge. 0) write
     +        (6,"('gdskdir 4: fileseek returned ',i11,' not ',i6)")
     +        rv, ai4
         fsemdir = .false.
         return
      endif

c  set ci4 and hence i4
      ci4(1:2) = char(0)//char(0)
c  read in the 2 bytes of directory size
      ai4 = 2
      rv = fileread(fd, ci4(3:4), ai4)
c  rv=0 means end of file
      if (rv .ne. ai4) then
         write (6,"('gdskdir 5: rv returned as ',i6,
     +              ' when reading disc size')") rv
         fsemdir = .false.
         return
      endif
c      semdlen = i4
      semdlen = strtoi4(ci4, bigend)

c  seek to start of directory at byte 65 (first byte = 1)
      ai4 = dstart + 64
      pos8 = ai4
      rv = fileseek(fd, pos8)
      if (rv .ne. ai4) then
         if (v .ge. 0) write
     +        (6,"('gdskdir 6: fileseek returned ',i11,' not ',i6)")
     +        rv, ai4
         fsemdir = .false.
         return
      endif

      do i=1,2008
c  This is how it was... read 8 bytes using read() into a i4 array...
c         ai4 = 8
c         rv = read(fd, semdir(1,i), ai4)
         rv = filereadi4(fd, semdir(1,i), dskorder)
         rv = filereadi4(fd, semdir(2,i), dskorder)
c         if (i .le. 4) write(6,"(3i12)") i, semdir(1,i), semdir(2,i)
c  rv=0 means end of file
         if (rv .ne. 4) then
            write (6,"('gdskdir 7: rv returned as ',i6,
     +                 ' when reading disc directory')") rv
            fsemdir = .false.
            return
         endif

c  stop when reach entry marking end
         if (semdir(1,i) .eq. 1000) then
            semdlsd = i
            goto 10
         endif
         if (i*8 .ge. semdlen*64) then
            if (v .ge. 0) write (6,
     + "('gdskdir 8: reached end of directory without finding pic 1000'
     +)")
            fsemdir = .false.
            return
         endif
      enddo
 10   continue
      if (semdir(2,semdlsd)-1 .ne. semdflen) then
         if (v .ge. 0) write (6,"('gdskdir 9: last directory entry (',
     +         i10,') is not 1 greater than file size (',i10,')')")
     +         semdir(2,semdlsd), semdflen
         fsemdir = .false.
         return
      endif
      fsemdir = .true.

c  work out number of free blocks
      if (semdir(1,semdlsd-1) .eq. 0) then
c  last entry is free
         semlastp = semdir(2,semdlsd-1)
         semfree = semdir(2,semdlsd) - semlastp
      else
         semlastp = semdir(2,semdlsd)
         semfree = 0
      endif

c  verify...
      if (v .ge. 3) then
         write (6,"('gdskdir 10: directory read OK')")
         write (6,
     +        "('File size ',i9,' blocks, ',i10,' bytes, ',i7,
     +           ' Kbytes, ',i4,' Mbytes')")
     +    semdflen, semdflen*64, semdflen*64/1024, semdflen*64/1024/1024
         write (6,"('directory length ',i9,' blocks, ',i10,' bytes')")
     +              semdlen, semdlen*64
         write (6,"('directory entry of last picture (1000) is ',i6)")
     +              semdlsd
         write (6,"('free space at end ',i9,' blocks, ',i10,' bytes')")
     +              semfree, semfree*64
      endif

      if (v .ge. 3) write (6,"('gdskdir 11: finished')")

      return

      end

c--------------------------------------------------------------------------
      function ws6lab()

c  sorts out semper variables & creates label for writing semper pictures
c  used by ws6rwb and ws6dsk

c  returns 0 if OK, returns 1 if problem

c  semper class and form
c  class
c    1    image               
c    2    macro
c    3    fourier
c    4    spectrum
c    5    correlation
c    6    undefined
c    7    walsh
c    8    position list (plist)
c    9    histogram
c   10    display look up table (lut)
c  form
c    0    byte
c    1    integer (2 byte)
c    2    floating point (fp)
c    3    complex

c  Silicon Graphics semper limitations ("show system" in semper)
c   Data form  Pixel size  Max. row length   Edge
c                (bytes)      (pixels)      pixels
c   Byte            1          32448          40
c   Integer         2          16224          20
c   Fp              4           8112          10
c   Complex         8           4056           5

c  number translations
c                              *=translation
c   original      psign ptype pbyte      semper  form
c   unsigned byte  0     i     1         byte    0
c   signed   byte  1     i     1     *   integer 1
c   unsigned i2    0     i     2     *   fp      2
c   signed   i2    1     i     2         integer 1
c   unsigned i4    0     i     4     *   fp      2
c   signed   i4    1     i     4     *   fp      2
c   real*4        any    f     4         fp      2
c   real*8        any    f     8     *   fp      2
c   complex       any    c     8         complex 3
c   complex*16    any    c    16     *   complex 3
c   rgb 3 layer    0     r     1         byte    0

c   NB fortran considers bytes (ie integer*1) to be signed
c      ie range -128 to 127



c  local variables
      integer*4 i, j
      real*4 tevpch
      logical*4 fwvars

c  common variables
      include "vars.f"

      if (v .ge. 3) write (6,"('ws6lab: starting...')")


c  set limit for row length of byte picture (used for warning message only)
      blim = 32448

c  set picture variables
      ncol = imwid
      nrow = imhei
      nlay = 1
      if (ffou) then
         iclass = 3      ! 3=fourier
      else
         iclass = 1      ! 1=image
      endif

c  check...
      if (psign .ne. 0 .and. psign .ne. 1) then
         if (v .ge. 0) write (6,"('ws6rwb: psign=',i6)") psign
         ws6lab = 1
         return
      endif

c  set type of numbers (iform)
      if (ptype .eq. 'i') then
         if (pbyte .eq. 1) then
            if (psign .eq. 0) then
               iform = 0
            else
               iform = 1
            endif
         elseif (pbyte .eq. 2) then
            if (psign .eq. 0) then
               iform = 2
            else
               iform = 1
            endif
         elseif (pbyte .eq. 4) then
            iform = 2
         else
            if (v .ge. 0) write (6,"('ws6rwb: pbyte=',i6)") pbyte
            ws6lab = 1
            return
         endif
      elseif (ptype .eq. 'f') then
         iform = 2
      elseif (ptype .eq. 'c') then
         iform = 3
      elseif (ptype .eq. 'r') then
         iform = 0
         nlay = 3
      else
         if (v .ge. 0) write (6,"('ws6rwb: ptype=',a)") ptype
         ws6lab = 1
         return
      endif
      if (v .ge. 3) write (6,"('psign=',i4,' ptype=',a,' pbyte=',i4,
     +              ' iform=',i4)") psign, ptype, pbyte, iform


c  sort out file name and evpch etc in semper title
c  first put evzero & evpch in bufc
c  evzero always corresponds to the leftmost channel. For semper
c  want evzero to refer to the semper origin, which should have been
c  set by getelp
c  first set iccoln to semper origin
      if (forigx) then
         iccoln = origx
      else
         iccoln = nint(real(ncol+1)/2.0)
      endif
c  work out ev of semper origin
      if (fevpch) then
         tevpch = evpch
      else
         tevpch = 1
      endif
      lbufc = 0
      if (fevzero) then
         evsemzero = evzero + (iccoln-1)*tevpch
         write (bufc, "('evzero ',f11.5)") evsemzero
         lbufc = 18
      endif
      if (fevpch) then
         if (lbufc .gt. 0) then
            lbufc = lbufc + 1
            bufc(lbufc:lbufc) = ' '
         endif
         write (bufc(lbufc+1:), "('evpch ',f11.5)") evpch
         lbufc = lbufc + 17
      endif

c  now put filename in stitle
      if (lfile .gt. 0) then
         stitle(1:lfile) = file(1:lfile)
         ntitle = lfile
      else
         ntitle = 0
      endif

c  if ltitle>0 and lbufc>0 then need to add a :  after filename
      if (ltitle+lbufc .gt. 0) then
         if (ntitle .gt. 0) then
            stitle(ntitle+1:ntitle+2) = ': '
            ntitle = ntitle + 2
         endif

c  find length of title that can be used and add title + bufc
         if (ltitle .gt. 0 .and. lbufc .gt. 0) then
c  need a space between them
            j = min(ltitle, 156-(ntitle+lbufc+1))
         else
            j = min(ltitle, 156-(ntitle+lbufc))
         endif
         if (j .gt. 0) then
c  need to remove cr or lf or any other control chars from title
            do i=1,j
               stitle(ntitle+i:ntitle+i) =
     +                char(max(ichar(title(i:i)), 32))
            enddo
            ntitle = ntitle + j
         endif
         if (lbufc .gt. 0) then
            if (j .gt. 0) then
               ntitle = ntitle + 1
               stitle(ntitle:ntitle) = ' '
            endif
            stitle(ntitle+1:ntitle+lbufc) = bufc(1:lbufc)
            ntitle = ntitle + lbufc
         endif
      endif

c  sort out time. Convert unix time to tarray
      call gmtime(imtime, tarray)
      iyear = tarray(6) + 1900
      imonth = tarray(5) + 1
      iday = tarray(4)
      ihour = tarray(3)
      iminut = tarray(2)
      isec = tarray(1)

c  sort out label. zero first
      do i=1,256
         labeli2(i) = 0
      enddo
c  Semper
      labeli2(1) = 83
      labeli2(2) = 101
      labeli2(3) = 109
      labeli2(4) = 112
      labeli2(5) = 101
      labeli2(6) = 114
c  ncol, nrow, nlay
      labeli2(7) = ncol/256
      labeli2(8) = mod(ncol, 256)
      labeli2(9) = nrow/256
      labeli2(10) = mod(nrow, 256)
      labeli2(11) = nlay/256
      labeli2(12) = mod(nlay, 256)
c  origin. x origin already set above
      labeli2(13) = iccoln/256
      labeli2(14) = mod(iccoln, 256)
      if (forigy) then
         labeli2(15) = origy/256
         labeli2(16) = mod(origy, 256)
      else
         i = nint(real(nrow+1)/2.0)
         labeli2(15) = i/256
         labeli2(16) = mod(i, 256)
      endif
      i = nint(real(nlay+1)/2.0)
      labeli2(17) = i/256
      labeli2(18) = mod(i,256)
c  bits & pieces
      labeli2(19) = iclass
      labeli2(20) = iform
      labeli2(21) = 0        ! wp flag, 0=not protected
      labeli2(22) = iyear - 1900
      labeli2(23) = imonth
      labeli2(24) = iday
      labeli2(25) = ihour
      labeli2(26) = iminut
      labeli2(27) = isec
      labeli2(28) = 0        ! ncrang, chars in range string
      labeli2(56) = 0        ! ipltyp, plist type
c  variables v0 to v9. Zero first if not already set
      if (.not. fsvars) then
         do i=0,9
            svars(i) = 0
         enddo
      endif
      fwvars = .false.
      if (fsvars) then
         fwvars = .true.
      elseif (fnmppx) then
c        x scale set
         svars(1) = nmppx
         if (fnmorx) svars(0) = nmorx
         if (fnmory) svars(2) = nmory
         if (fnmppy) svars(3) = nmppy
         if (fnmorz) svars(4) = nmorz
         if (fnmppz) svars(5) = nmppz
         fwvars = .true.
      elseif (fevpch) then
c        evpch set
         svars(1) = evpch
         if (fevzero) svars(0) = evzero + (iccoln-1)*evpch
         fwvars = .true.
      endif
      if (fwvars) then
c  write variables if fwvars true, in machine order
         do i=0,9
            j = 96 - 4*i
            f4 = svars(i)
            labeli2(j) = ichar(cf4(1:1))
            labeli2(j+1) = ichar(cf4(2:2))
            labeli2(j+2) = ichar(cf4(3:3))
            labeli2(j+3) = ichar(cf4(4:4))
         enddo
      endif
c  title
      labeli2(100) = ntitle
      do i=1,ntitle
         labeli2(100+i) = ichar(stitle(i:i))
      enddo

c  copy labeli2() into label
      do i=1,256
         label(i:i) = char(labeli2(i))
      enddo

      ws6lab = 0
      if (v .ge. 3) write (6,"('ws6lab: finishing...')")

      return

      end
c--------------------------------------------------------------------------
      function slabnp(picn)

c  searches through the semper directory (assumed already read in with
c  gdskdir) for next available picture number after (and possibly including)
c  picn

c  local variables
      integer*4 picn, i, j

c  common variables
      include "vars.f"

c  check directory read in
      if (.not. fsemdir) then
         if (v .ge. 0) write (6,
     +              "('slabnp: semper directory not read')")
         goto 120
      endif

      if (picn .gt. 999) goto 120

c  now search through directory for next free picture...
      do i=picn,999
c        for picture numbers, i, from picn to 999
         do j=1,semdlsd
c           search through whole directory
c           if this directory entry matches the current pic no, i, goto next i
            if (semdir(1,j) .eq. i) goto 10
         enddo
c        have not found this pic no, so it must be free
         slabnp = i
         goto 20
 10      continue
      enddo
c  searched through all pictures up to 999 and all taken
      if (v .ge. 0) write (6,"('slabnp: no free pictures above ',i6)")
     +                    picn
      goto 120

c  have found next free picture, return with it
 20   return


 120  slabnp = -1
      return

      end

c--------------------------------------------------------------------------
      function slabnewpic(fd, picn, blockn)

c  writes info in semper directory for new picture number picn
c  _ending_ at blockn
c  assumes directory read in already

c  local variables
      integer*4 fd, picn, blockn, ai4

c  common variables
      include "vars.f"


      if (v .ge. 3) write (6,"('slabnewpic: starting...')")

c  check last block
      if (blockn .le. semlastp) then
         if (v .ge. 0) write (6,"(
     +       'slabnewpic: block after last pic ',i8,
     +       ' new pic ends at ',i8)") semlastp, blockn
         goto 120
      endif

      if (blockn .gt. semdflen+1) then
         if (v .ge. 0) write (6,"(
     +       'slabnewpic: new pic ends at ',i8,
     +       ' block after end of file is ',i8)") blockn, semdflen + 1
         goto 120
      endif


c  check we can find picture 1000
      ai4 = gsdir(fd, semdlsd, 1)
      if (ai4 .ne. 1000) then
         if (v .ge. 0) write (6,"('slabnewpic: found ',i6,' not 1000')")
     +                       ai4
         goto 120
      endif

c  check value at pic 1000 is size of file (semdflen) + 1
      ai4 = gsdir(fd, semdlsd, 2)
      if (ai4 .ne. semdflen+1) then
         if (v .ge. 0) write (6,"('slabnewpic: found ',i8,' not ',i8)")
     +                       ai4, semdflen+1
         goto 120
      endif

c  check previous picture is indeed free
      ai4 = gsdir(fd, semdlsd-1, 1)
      if (ai4 .ne. 0) then
         if (v .ge. 0) write (6,"('slabnewpic: found ',i6,' not 0')")
     +                       ai4
         goto 120
      endif

c  if blockn is equal to semdflenC+1 then new picture exactly fills
c  available space, so just convert the last picture 0 to the new pic
      if (v .ge. 3) write (6,"('slabnewpic: blockn = ',i8
     +         ' semdflen+1 = ',i8)") blockn, semdflen+1
      if (blockn .eq. semdflen+1) then
         if (wsdir(fd, semdlsd-1, 1, picn) .ne. 0) goto 120
      else
c  need to create a new entry & move pic 1000 up one
         if (wsdir(fd, semdlsd+1, 1, 1000) .ne. 0) goto 120
         if (wsdir(fd, semdlsd+1, 2, semdflen+1) .ne. 0) goto 120
c  write free space
         if (wsdir(fd, semdlsd, 1, 0) .ne. 0) goto 120
         if (wsdir(fd, semdlsd, 2, blockn) .ne. 0) goto 120
c  write pic number
         if (wsdir(fd, semdlsd-1, 1, picn) .ne. 0) goto 120
      endif

c  reread directory
      call gdskdir(fd)
      if (.not. fsemdir) goto 120
      slabnewpic = 0
      if (v .ge. 3) write (6,"('slabnewpic: finished normally')")

      return

 120  if (v .ge. 3) write (6,"('slabnewpic: ended with error')")

      slabnewpic = 1
      return

      end

c--------------------------------------------------------------------------
      function gsdir(fd, slot, k)

c  gets number from semper disc directory slot
c  returns negative if problem

c  local variables
      integer*4 fd, slot, k, ai4

c  common variables
      include "vars.f"

      if (v .ge. 3) write (6,"('gsdir: starting...')")

c  check sensible values
      if (slot .lt. 1 .or. slot .gt. semdlen*64/8) then
         if (v .ge. 0) write (6,"('gsdir: slot = ',i8)") slot
         goto 120
      endif
      if (k .lt. 1 .or. k .gt. 2) then
         if (v .ge. 0) write (6,"('gsdir: k = ',i8)") k
         goto 120
      endif

c  read value via ci4, i4
      if (clseek(fd, 64 + (slot-1)*8 + (k-1)*4, 'gsdir') .ne. 0)
     +                                            goto 120
      ai4 = 4
      rv = fileread(fd, ci4, ai4)
      if (rv .ne. ai4) then
         write (6,"('gsdir: rv returned as ',i6,
     +                 ' when reading disc directory')") rv
         goto 120
      endif
      gsdir = i4
      if (v .ge. 3) write (6,"('gsdir: finished normally')")

      return

 120  gsdir = -1
      if (v .ge. 3) write (6,"('gsdir: finished with error')")
      return

      end

c--------------------------------------------------------------------------
      function wsdir(fd, slot, k, value)

c  writes number to semper disc directory slot
c  returns 0 of OK, 1 if problem

c  local variables
      integer*4 fd, slot, k, value

c  common variables
      include "vars.f"

      if (v .ge. 3) write (6,"('wsdir: starting...')")

c  check sensible values
      if (slot .lt. 1 .or. slot .gt. semdlen*64/8) then
         if (v .ge. 0) write (6,"('wsdir: slot = ',i8)") slot
         goto 120
      endif
      if (k .lt. 1 .or. k .gt. 2) then
         if (v .ge. 0) write (6,"('wsdir: k = ',i8)") k
         goto 120
      endif

c  read value via ci4, i4
      if (clseek(fd, 64 + (slot-1)*8 + (k-1)*4, 'wsdir') .ne. 0)
     +                                            goto 120
      i4 = value
      if (cwrite(fd, ci4, 4, 'wsdir') .ne. 0) goto 120
      wsdir = 0

      return

 120  wsdir = 1
      return

      end
