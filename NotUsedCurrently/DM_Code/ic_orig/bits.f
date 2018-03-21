c  bits.f

c  Contains misc functions and subroutines for ic
c  All these are related to ic and usually need to include vars.f

c--------------------------------------------------------------------------
      subroutine typhelp

c  type short help information

      character cdate*100
      integer*4 lnblnk

c  cdate.f is one line that sets cdate to the date & time of compiling
      include "cdate.f"

      write (6,1) cdate(1:lnblnk(cdate))
 1    format (
     + 'usage:'
c     +/'ic [-h] [-q -vn] [-m -nom] [<output file type>] [file ...]'
     +/'ic [-h] [-q -vn] [<output file type>] [file ...]'
     +/
     +/' -h        give this help'
     +/' -q        quiet, equivalent to -v0'
     +/' -vn       verification level, where n='
     +/'   -1         very quiet, not even error messages produced'
     +/'   0          quiet, only error messages'
     +/'   1          default, one line verification per image'
     +/'   2          more verbose verification'
     +/'   3          gratuitous amounts of stuff for debugging'
c     +/' -m        MacBinary format'
c     +/' -nom      no MacBinary format'
     +/' -d        dump of headers of file (DM or semper disc)'
     +/' -oxx.dsk  output semper disc name xx.dsk'
     +/' -s        create/enlarge semper disc to just fit files'
     +/'           needed to enlarge an existing semper disc'
     +/' -s1234    create/enlarge semper disc to 1234 Kbytes'
     +/' -n123     start writing from picture number 123'
     +/' -imgn     convert only the nth image from all dm3 images'
     +/
     +/'Output file types, only one, default used if none supplied'
     +/' -dsk      Semper 6 picture disc (for unix)'
     +/'           all files copied to one disc'
     +/' -unf      Semper 6 read/write binary (*.unf, for unix)'
     +/' -elp      EL/P EELS file (Mac)'
     +/' -dm       Digital Micrograph 2 Gatan format'
     +/
     +/'To enlarge a semper disc xx.dsk to 1234Kbytes ',
     +'(or create a new one)'
     +/'ic -s1234 -oxx.dsk'
     +/
     +/'Options and filenames can be in any order'
     +/
     +/'Default output file types:'
     +/'Input file type      Output file type'
     +/
c     +/'Semper               Digital Micrograph'
     +/'EL/P                 Semper disc'
     +/'Digital Micrograph   Semper disc'
     +/'Semper read/write    Digital Micrograph 2'
     +/
     +/a
     +)

      return

      end

c--------------------------------------------------------------------------
      subroutine splitp(pathn, lpathn)

c  splits path in pathn (length lpathn) into dir, file, froot, fext
c  allows fext to be only allowed values

      character*(*) pathn
      integer*4 lpathn
      integer*4 i

c  common variables
      include "vars.f"

c  search backward to find last /
c  this sets dir to the directory and file to the filename
      dir = ' '
      ldir = 0
      file = pathn
      lfile = lpathn
      do i=lpathn, 1, -1
         if (pathn(i:i) .eq. '/') then
            dir = pathn(1:i-1)
            ldir = i-1
            file = pathn(i+1:lpathn)
            lfile = lpathn - i
            goto 10
         endif
      enddo
 10   continue

c  dir & file now set, split file into froot & fext
      froot = file
      lfroot = lfile
      fext = ' '
      lfext = 0
      do i=lfile, 1, -1
         if (file(i:i) .eq. '.') then
            froot = file(1:i-1)
            lfroot = i-1
            fext = file(i+1:lfile)
            lfext = lfile - i
            goto 20
         endif
      enddo
 20   continue

c  copy fext into c and make lower case
      c = fext
      lc = lfext
      call lower(c)

c  allow fext to only be allowed extension, upper or lower case
      if (c(1:lfext) .ne. 'unf' .and.
     +    c(1:lfext) .ne. 'dm3' .and.
     +    c(1:lfext) .ne. 'elp' .and.
     +    c(1:lfext) .ne. 'dat' .and.
     +    c(1:lfext) .ne. 'bin' .and.
     +    c(1:lfext) .ne. 'sp') then
         froot = file(1:lfile)
         lfroot = lfile
         fext = ' '
         lfext = 0
      endif

c  make up ofdr, the output filename root with directory name
      if (ldir .gt. 0) then
         ofdr = dir(1:ldir) // '/' // froot(1:lfroot)
         lofdr = ldir + 1 + lfroot
      else
         ofdr = froot(1:lfroot)
         lofdr = lfroot
      endif

c  verify bits of pathname
      if (v .ge. 3) then
         write (6,"('dir     ""',a,'""')") dir(1:ldir)
         write (6,"('file    ""',a,'""')") file(1:lfile)
         write (6,"('froot   ""',a,'""')") froot(1:lfroot)
         write (6,"('fext    ""',a,'""')") fext(1:lfext)
         write (6,"('ofdr    ""',a,'""')") ofdr(1:lofdr)
      endif

      return

      end

c--------------------------------------------------------------------------
      function copenrw(filename, funcname)

c  opens file for reading and writing, assuming it exists
c  returns file descriptor
c     >0  file exists
c      0  file doesn't exist
c     <0  problem
c  funcname is the name of the calling routine for use in error messages

c  local variables
      integer*4 ai4
      character*(*) filename
      character*(*) funcname

c  common variables
      include "vars.f"

      if (v .ge. 3) write (6,"('copenrw: called from ',a,
     +                         ' filename ',a)")
     +                    funcname, filename

      ai4 = fileopenrw(filename)
      if (v .ge. 3 .and. ai4 .lt. 0) write
     +   (6,"('copenrw: returned ',z8)")
     +   ofdes
      copenrw = ai4

      return

      end

c--------------------------------------------------------------------------
      function copenr(filename, funcname)

c  opens file for reading only, assuming it exists
c  returns file descriptor
c     >0  file exists
c      0  file doesn't exist
c     <0  problem
c  funcname is the name of the calling routine for use in error messages

c  local variables
      integer*4 ai4
      character*(*) filename
      character*(*) funcname

c  common variables
      include "vars.f"

      if (v .ge. 3) write (6,"('copenr: called from ',a,
     +                         ' filename ',a)")
     +                    funcname, filename

      ai4 = fileopenr(filename)
      if (v .ge. 4) write
     +   (6,"('copenr: returned ',z8)")
     +   ofdes
      if (v .ge. 3) then
         if (ai4 .eq. 0) write (6,"(a,':copenr: file ',a,
     +                   ' does not exist')") funcname, fname
         if (ai4 .lt. 0) write (6,"(a,':copenr: problem opening ',a,
     +                   ', returned ',z8)") funcname, fname, ai4
      endif
      copenr = ai4

      return

      end

c--------------------------------------------------------------------------
      function clseek(fd, pos, funcname)

c  lseeks to position pos in file fd. funcname is the name of the calling
c  routine for use in error messages
c  returns 0 of sucessful, 1 if there is a problem

c  local variables
      integer*4 pos, fd
      integer*8 pos8
      character*(*) funcname

c  common variables
      include "vars.f"


      if (v .ge. 3) write (6,"('clseek: called from ',a,' pos ',i12)")
     +                    funcname, pos

c  seek to pos
      pos8 = pos
      rv = fileseek(fd, pos8)
      if (rv .ne. pos) then
         if (v .ge. 0) write
     +        (6,"(a,': fileseek returned ',i11,' not ',i6)") funcname,
     +        rv, pos
         clseek = 1
      else
         clseek = 0
      endif

      return

      end

c--------------------------------------------------------------------------
      function clseekr(fd, pos, funcname)

c  lseeks to relative position pos in file fd.
c  funcname is the name of the calling
c  routine for use in error messages
c  returns 0 of sucessful, 1 if there is a problem

c  local variables
      integer*4 pos, fd, ai4
      integer*8 pos8
      character*(*) funcname

c  common variables
      include "vars.f"


      if (v .ge. 4) write (6,"(a,':clseekr: seeking forward by ',i12)")
     +                    funcname, pos

c  seek forward by pos
      ai4 = filepos(fd)
      pos8 = pos
      if (v .ge. 4) write (6,"(a,':clseekr: was at ',i12,2x,z8)")
     +                    funcname, ai4, ai4
      rv = fileseekr(fd, pos8)
      if (rv .ne. pos+ai4) then
         if (v .ge. 0) write
     +        (6,"(a,': fileseekr returned ',i11,' not ',i6)") funcname,
     +        rv, pos+ai4
         clseekr = 1
      else
         clseekr = 0
      endif

      if (v .ge. 4) write (6,"(a,':clseekr: now at ',i12,2x,z8,
     +                     ' returned ',i4)") funcname, rv, rv, clseekr

      return

      end

c--------------------------------------------------------------------------
      function cnextb(fd, unit, funcname)

c  Moves position in file to next multiple of unit
c  mainly for moving to next block in semper
c  funcname is the name of the calling routine for use in error messages
c  returns 0 if sucessful, 1 if not

c  local variables
      integer*4 fd, unit, ai4
      integer*8 pos8
      character*(*) funcname

c  common variables
      include "vars.f"


      if (v .ge. 4) write (6,"('cnextb: called from ',a)")
     +                    funcname

c  find current position
      ai4 = filepos(fd)
      ai4 = roundup(ai4, unit)

      pos8 = ai4
      rv = fileseek(fd, pos8)
      if (rv .ne. ai4) then
         if (v .ge. 0) write
     +        (6,"(a,': fileseek returned ',i11,' not ',i6)") funcname,
     +        rv, ai4
         cnextb = 1
      else
         cnextb = 0
      endif

      return

      end

c--------------------------------------------------------------------------
      function creadi2(fd, ai2, fbyteord, funcname)

c  read i2 from file fd in byte order determined by byterev.
c  funcname is the name of the calling routine for use in error messages
c  returns 0 of sucessful, 1 if there is a problem

c  local variables
      integer*4 fd, i
      character*(*) funcname
      character string*2
      logical   byterev           ! true if bytes need to be reversed
      logical   fbyteord          ! true if file is little endian (mainly for dm3)

c  common variables
      include "vars.f"


      if (v .ge. 4) write (6,"('creadi2: starting from ',a
     +          )") funcname

c  set the byte order
      byterev = fbyteord .neqv. progbyteorder()

c  read the number
      if (byterev) then
         rv = fileread(fd, string, 2)
      else
         rv = fileread(fd, ci2, 2)
      endif

      if (rv .eq. 0) then
         if (v .ge. 0) write
     +      (6,"(a,': end of file')") funcname
         creadi2 = 1
      elseif (rv .ne. 2) then
         if (v .ge. 0) write
     +      (6,"(a,': fileread returned ',i6)") funcname, rv
         creadi2 = 1
      else
         creadi2 = 0
      endif

c  reverse bytes if byterev is true
      if (byterev) then
         do i=1,2
            ci2(i:i) = string(3-i:3-i)
         enddo
         if (v .ge. 4) write (6,"('creadi2: reverse byte order')")
      else
         if (v .ge. 4) write (6,"('creadi2: normal byte order')")
      endif

      ai2 = i2

      return

      end

c--------------------------------------------------------------------------
      function creadi4(fd, ai4, fbyteord, funcname)

c  read i4 from file fd in byte order determined by byterev.
c  funcname is the name of the calling routine for use in error messages
c  returns 0 of sucessful, 1 if there is a problem

c  local variables
      integer*4 fd, ai4, i
      character*(*) funcname
      character string*4
      logical   byterev           ! true if bytes need to be reversed
      logical   fbyteord          ! true if file is little endian (mainly for dm3)

c  common variables
      include "vars.f"


      if (v .ge. 4) write (6,"('creadi4: starting from ',a
     +          )") funcname

c  set the byte order
      byterev = fbyteord .neqv. progbyteorder()

c  read the number
      if (byterev) then
         rv = fileread(fd, string, 4)
      else
         rv = fileread(fd, ci4, 4)
      endif

      if (rv .eq. 0) then
         if (v .ge. 0) write
     +      (6,"(a,': end of file')") funcname
         creadi4 = 1
      elseif (rv .ne. 4) then
         if (v .ge. 0) write
     +      (6,"(a,': fileread returned ',i6)") funcname, rv
         creadi4 = 1
      else
         creadi4 = 0
      endif

c  reverse bytes if byterev is true
      if (byterev) then
         do i=1,4
            ci4(i:i) = string(5-i:5-i)
         enddo
         if (v .ge. 4) write (6,"('creadi4: reverse byte order')")
      else
         if (v .ge. 4) write (6,"('creadi4: normal byte order')")
      endif

      ai4 = i4

      return

      end

c--------------------------------------------------------------------------
      function creadf4(fd, af4, fbyteord, funcname)

c  read f4 from file fd in byte order determined by byterev.
c  funcname is the name of the calling routine for use in error messages
c  returns 0 of sucessful, 1 if there is a problem

c  local variables
      integer*4 fd, i
      character*(*) funcname
      character string*4
      real*4    af4
      logical   byterev           ! true if bytes need to be reversed
      logical   fbyteord          ! true if file is little endian (mainly for dm3)

c  common variables
      include "vars.f"


      if (v .ge. 4) write (6,"('creadf4: starting from ',a
     +          )") funcname

c  set the byte order
      byterev = fbyteord .neqv. progbyteorder()

c  read the number
      if (byterev) then
         rv = fileread(fd, string, 4)
      else
         rv = fileread(fd, cf4, 4)
      endif

c         write (6,"('af4 = ',e11.3)") af4
      if (rv .eq. 0) then
         if (v .ge. 0) write
     +      (6,"(a,': end of file')") funcname
         creadf4 = 1
      elseif (rv .ne. 4) then
         if (v .ge. 0) write
     +      (6,"(a,': fileread returned ',i6)") funcname, rv
         creadf4 = 1
      else
         creadf4 = 0
      endif

c  reverse bytes if byterev is true
      if (byterev) then
         do i=1,4
            cf4(i:i) = string(5-i:5-i)
         enddo
         if (v .ge. 4) write (6,"('creadf4: reverse byte order')")
      else
         if (v .ge. 4) write (6,"('creadf4: normal byte order')")
      endif


      af4 = f4
c      write (6,"('creadf4 = ',i11)") creadf4

      return

      end

c--------------------------------------------------------------------------
      function creadf8(fd, af8, fbyteord, funcname)

c  read f8 from file fd in byte order determined by byterev.
c  funcname is the name of the calling routine for use in error messages
c  returns 0 of sucessful, 1 if there is a problem

c  local variables
      integer*4 fd, i
      character*(*) funcname
      character string*8
      real*8    af8
      logical   byterev           ! true if bytes need to be reversed
      logical   fbyteord          ! true if file is little endian (mainly for dm3)

c  common variables
      include "vars.f"


      if (v .ge. 4) write (6,"('creadf8: starting from ',a
     +          )") funcname

c  set the byte order
      byterev = fbyteord .neqv. progbyteorder()

c  read the number
      if (byterev) then
         rv = fileread(fd, string, 8)
      else
         rv = fileread(fd, cf8, 8)
      endif

c         write (6,"('af8 = ',d11.3)") af8
      if (rv .eq. 0) then
         if (v .ge. 0) write
     +      (6,"(a,': end of file')") funcname
         creadf8 = 1
      elseif (rv .ne. 8) then
         if (v .ge. 0) write
     +      (6,"(a,': fileread returned ',i6)") funcname, rv
         creadf8 = 1
      else
         creadf8 = 0
      endif

c  reverse bytes if byterev is true
      if (byterev) then
         do i=1,8
            cf8(i:i) = string(9-i:9-i)
         enddo
         if (v .ge. 4) write (6,"('creadf8: reverse byte order')")
      else
         if (v .ge. 4) write (6,"('creadf8: normal byte order')")
      endif


      af8 = f8
c      write (6,"('creadf8 = ',i11)") creadf8

      return

      end

c--------------------------------------------------------------------------
      function creadc(fd, string, lstring, funcname)

c  read character string from file
c  funcname is the name of the calling routine for use in error messages
c  returns 0 of sucessful, 1 if there is a problem

c  local variables
      integer*4 fd, lstring
      character*(*) funcname, string

c  common variables
      include "vars.f"


      if (v .ge. 4) write (6,"('creadc: starting from ',a,
     +          ' string length ',i8)") funcname, lstring

c  read the number
      rv = fileread(fd, string, lstring)
      if (rv .eq. 0 .and. lstring .ne. 0) then
         if (v .ge. 0) write
     +      (6,"(a,': end of file')") funcname
         creadc = 1
      elseif (rv .ne. lstring) then
         if (v .ge. 0) write
     +      (6,"(a,': fileread returned ',i6,' not ',i6)")
     +            funcname, rv, lstring
         creadc = 1
      else
         creadc = 0
      endif

      return

      end

c--------------------------------------------------------------------------
      function cwrite(fd, string, lstring, funcname)

c  write string to file fd.
c  funcname is the name of the calling routine for use in error messages
c  returns 0 of sucessful, 1 if there is a problem

c  local variables
      integer*4 fd, lstring
      character*(*) string
      character*(*) funcname

c  common variables
      include "vars.f"


      if (v .ge. 4) write (6,"('cwrite: starting from ',a,
     +          ' string length ',i8)") funcname, lstring

c  write the string
      rv = filewrite(fd, string, lstring)
      if (rv .lt. 0) then
         if (v .ge. 0) write
     +      (6,"(a,': filewrite returned ',i6)") funcname, rv
         cwrite = 1
      else
         cwrite = 0
      endif

      return

      end

c--------------------------------------------------------------------------
      function cwri64(fd, string, lstring, funcname)

c  write string to file fd. moves to the next 64 byte block
c  funcname is the name of the calling routine for use in error messages
c  returns 0 of sucessful, 1 if there is a problem

c  local variables
      integer*4 fd, lstring, ai4
      integer*8 pos8
      character*(*) string
      character*(*) funcname

c  common variables
      include "vars.f"


      if (v .ge. 4) write (6,"('cwri64: starting from ',a,
     +          ' string length ',i8)") funcname, lstring

c  write the string
      rv = filewrite(fd, string, lstring)
      if (rv .lt. 0) then
         if (v .ge. 0) write
     +      (6,"(a,': filewrite returned ',i6)") funcname, rv
         cwri64 = 1
         return
      else
         cwri64 = 0
      endif

c  find current position & seek to next block
      ai4 = filepos(fd)
      ai4 = roundup(ai4, 64)

      pos8 = ai4
      rv = fileseek(fd, pos8)
      if (rv .ne. ai4) then
         if (v .ge. 0) write
     +        (6,"(a,': fileseek returned ',i11,' not ',i6)") funcname,
     +        rv, ai4
         cwri64 = 1
      else
         cwri64 = 0
      endif

      return

      end

c--------------------------------------------------------------------------
      subroutine survey()

c  Surveys image to find image min and image max (imgmin, imgmax)

c  14 Nov 2003: seem to have started this but not finished it...
c  21 Aug 2004: now more finished version

c  local variables
      integer*4 i, j, k, kmax
      real*4 ar4

c  common variables
      include "vars.f"


c  find max k. Set for complex and rgb images to search all layers
      kmax = 0
      if (ptype .eq. 'c') kmax = 1
      if (ptype .eq. 'r') kmax = 2

c  set imgmin and imgmax to value at (0,0)
c  really need function to get a value from image without knowing type...
c  now have it - imgreal8
      imgmin = imgreal8(0,0,0)
      imgmax = imgmin

c  search image for min and max.
      do j=0,imhei-1
         do i=0,imwid-1
            do k=0,kmax
               ar4 = imgreal8(i,j,k)
               if (ar4 .lt. imgmin) then
                  imgmin = ar4
               elseif (ar4 .gt. imgmax) then
                  imgmax = ar4
               endif
            enddo
         enddo
      enddo

      fimgmin = .true.
      fimgmax = .true.

      return

      end

c--------------------------------------------------------------------------
      function imgreal8(i, j, k)

c  returns value at point i,j in real*8 in current image
c  for complex, k=0 means real part, k=1 means imaginary part
c  for rgb, k=0, 1, 2 means r, g, b parts

c  local variables
      integer*4 i, j, k

c  common variables
      include "vars.f"


      imgreal8 = 0

 
      if (ptype .eq. 'i') then
c  integer...
         if (pbyte .eq. 1) then
c  ...byte
            if (imhei .eq. 1) then
               imgreal8 = im1di1(i)
            else
               imgreal8 = imagi1(i,j)
            endif
            if (psign .eq. 0 .and. imgreal8 .lt. 0)
     +                   imgreal8 = imgreal8 + 256.
         elseif (pbyte .eq. 2) then
c  ...integer*2
            if (imhei .eq. 1) then
               imgreal8 = im1di2(i)
            else
               imgreal8 = imagi2(i,j)
            endif
            if (psign .eq. 0 .and. imgreal8 .lt. 0)
     +                   imgreal8 = imgreal8 + 256.*256.
         elseif (pbyte .eq. 4) then
c  ...integer*4
            if (imhei .eq. 1) then
               imgreal8 = im1di4(i)
            else
               imgreal8 = imagi4(i,j)
            endif
            if (psign .eq. 0 .and. imgreal8 .lt. 0)
     +                   imgreal8 = imgreal8 + 256.**4
         else
            if (v .gt. 0) write (6,"('imgreal8: pbyte = ',i6)") pbyte
         endif

      elseif (ptype .eq. 'f') then
c  real...
         if (pbyte .eq. 4) then
            if (imhei .eq. 1) then
               imgreal8 = im1dr(i)
            else
               imgreal8 = imagr(i,j)
            endif
         elseif (pbyte .eq. 8) then
            if (imhei .eq. 1) then
               imgreal8 = im1dr8(i)
            else
               imgreal8 = imagr8(i,j)
            endif
         else
            if (v .gt. 0) write (6,"('imgreal8: ptype = ',a,
     +                                 ' pbyte = ',i6)") ptype, pbyte
         endif

      elseif (ptype .eq. 'c') then
c  complex...
         if (pbyte .eq. 8) then
c  ...complex*8 (normal complex)
            if (k .eq. 0) then
               if (imhei .eq. 1) then
                  imgreal8 = real(im1dx(i))
               else
                  imgreal8 = real(imagx(i,j))
               endif
            elseif (k .eq. 1) then
               if (imhei .eq. 1) then
                  imgreal8 = aimag(im1dx(i))
               else
                  imgreal8 = aimag(imagx(i,j))
               endif
            else
               if (v .gt. 0) write (6,"('imgreal8: k = ',i6)") k
            endif
         elseif (pbyte .eq. 16) then
c  ...complex*16 (double precision complex)
            if (k .eq. 0) then
               if (imhei .eq. 1) then
                  imgreal8 = im1dr8(2*i)
               else
                  imgreal8 = imagr8(2*i,j)
               endif
            elseif (k .eq. 1) then
               if (imhei .eq. 1) then
                  imgreal8 = im1dr8(2*i+1)
               else
                  imgreal8 = imagr8(2*i+1,j)
               endif
            else
               if (v .gt. 0) write (6,"('imgreal8: k = ',i6)") k
            endif
         else
            if (v .gt. 0) write (6,"('imgreal8: ptype = ',a,
     +                                 ' pbyte = ',i6)") ptype, pbyte
         endif

      elseif (ptype .eq. 'r') then
c  rgb...
         if (pbyte .eq. 1) then
c  ...byte
            if (imhei .eq. 1) then
               imgreal8 = im1dco(i,k)
            else
               imgreal8 = imagco(i,j,k)
            endif
            if (imgreal8 .lt. 0) imgreal8 = imgreal8 + 256.
         elseif (pbyte .eq. 2) then
c  ...integer*2
            if (imhei .eq. 1) then
               imgreal8 = im1dc2(i,k)
            else
               imgreal8 = imagc2(i,j,k)
            endif
         else
            if (v .gt. 0) write (6,"('imgreal8: ptype = ',a,
     +                                 ' pbyte = ',i6)") ptype, pbyte
         endif

      else
c  unknown image type
         if (v .gt. 0) write (6,"('imgreal8: ptype = ',a)") ptype
      endif
 
      return

      end

c--------------------------------------------------------------------------
      subroutine ctofstring(string)

c  converts a c type string (null terminated) into a fortran type
c  string (padded with spaces)
c  looks for a null and sets all subsequent characters to spaces

      character*(*) string

c  local variables
      integer*4 length, i
      logical   fnull


c  set fnull to false
      fnull = .false.

c  find string length
      length = len(string)

c  for each character in string
      do i=1,length

c  if character is a null, set fnull true
         if (string(i:i) .eq. char(0)) fnull = .true.

c  if fnull (ie null  has been found), set character to space
         if (fnull) string(i:i) = char(32)
      enddo

      return

      end
