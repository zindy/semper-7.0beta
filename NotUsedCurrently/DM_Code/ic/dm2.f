c  dm2.f
c  Reads, writes and dumps Digital Micrograph 2 files

c--------------------------------------------------------------------------
      function gdm()

c  gets Digital Micrograph file
c  unfortunately the image comes first, so a set of equivalenced
c  arrays of different types is needed as well as im...()

c  local variables
      integer  tag, ffff
      integer*4 dlen, blen, i, j, k
      integer*8 pos8

c  common variables
      include "vars.h"

c  set ffff to x'ffff' so can use for tests below
      ffff = x'ffff'

c  now get on with it
      if (v .ge. 3) write (6,"('starting gdm')")

c  open file
      fname = path(1:lpath)
      ifdes = fileopenr(fname)
      if (v .ge. 3) write
     +   (6,"('gdm: fileopenr returned ',z8)")
     +   ifdes
      if (ifdes .le. 0 .and. v .ge. 0) then
         write (6,"('gdm: can''t find ""',a,'""')") path(1:lpath)
         goto 120
      endif

c  skip to start if macbinary (fileseek returns i8...)
      if (dstart .gt. 0) then
         pos8 = dstart
         rv = fileseek(ifdes, pos8)
         if (rv .ne. dstart) then
            if (v .ge. 0) write
     +           (6,"('gdm: fileseek returned ',i11,' not ',i6)")
     +           rv, dstart
            goto 120
         endif
      endif

c  go through each field until end of file
 10   continue

c  read tag
         rv = filereadi2(ifdes, tag, bigend)
c  rv=0 means end of file
         if (rv .eq. 0) goto 100
         if (rv .ne. 2) then
            if (v .ge. 0) write
     +          (6,"('gdm: rv returned as ',i6,' when reading tag')") rv
            goto 110
         endif

c  rv=2, ie tag read in, so read length
         rv = filereadi4(ifdes, dlen, bigend)
         if (rv .ne. 4) then
            if (v .ge. 0) write
     +          (6,"('gdm: rv returned as ',i6,' when reading length')")
     +          rv
            goto 110
         endif

c  check dlen
         if (dlen .lt. 0) then
            if (v .ge. 0) write (6,"('gdm: dlen = ',i8)") dlen
            goto 120
         endif

c  verify
         if (v .ge. 3) write (6,"('gdm: tag ',z4,
     +                 ', length (dlen) = ',i8)") tag, dlen

c  data part of field still to be read in, but look for useful tags
c  and do the right thing with each

         if (tag .eq. x'0') then
c  end of file
            if (v .ge. 0 .and. dlen .ne. 0) then
               write (6,"('gdm: tag 0 (end of file), dlen = ',
     +              i12,' not zero')") dlen
               goto 120
            endif
            goto 100

c  on mac cant do (tag .eq. x'ffff') because x'ffff' is i4 and = 65535 not -1
         elseif (tag .eq. ffff) then
c  the image itself. read first 8 bytes (width, height, bytes/pixel, data type)

c  set values
            if (creadi2(ifdes, ai2, bigend, 'gdm') .gt. 0) goto 110
            imwid = ai2
            fimwid = .true.

            if (creadi2(ifdes, ai2, bigend, 'gdm') .gt. 0) goto 110
            imhei = ai2
            fimhei = .true.

            if (creadi2(ifdes, ai2, bigend, 'gdm') .gt. 0) goto 110
            pbyte = ai2
            fpbyte = .true.

            if (creadi2(ifdes, ai2, bigend, 'gdm') .gt. 0) goto 110

c  verify...
            if (v .ge. 3) write
     +         (6,"('gdm: header, width ',i6,' height ',i6,
     +              ' bytes/pixel ',i6,' data type ',i6)")
     +         imwid, imhei, pbyte, ai2

c  determine image type from data type in ai2
            if (ai2 .eq.  6 .or. ai2 .eq. 10 .or.
     +          ai2 .eq. 11 .or. ai2 .eq. 14) then
c  unsigned integer
               ptype = 'i'
               fptype = .true.
               psign = 0
               fpsign = .true.
            elseif (ai2 .eq. 9 .or. ai2 .eq. 1 .or.
     +              ai2 .eq. 7) then
c  signed integer
               ptype = 'i'
               fptype = .true.
               psign = 1
               fpsign = .true.
            elseif (ai2 .eq. 2 .or. ai2 .eq. 12) then
c  floating point (real)
               ptype = 'f'
               fptype = .true.
               psign = 1
               fpsign = .true.
            elseif (ai2 .eq. 3 .or. ai2 .eq. 13) then
c  complex
               ptype = 'c'
               fptype = .true.
               psign = 1
               fpsign = .true.
            elseif (ai2 .eq. 5) then
c  packed complex. convert to half plane Fourier, orig at left
               imwid = imwid/2+1
               pbyte = pbyte*2
               origx = 1
               forigx = .true.
               ffou = .true.
               ptype = 'c'
               fptype = .true.
               psign = 1
               fpsign = .true.
            elseif (ai2 .eq. 8) then
c  rgb
               ptype = 'r'
               fptype = .true.
               psign = 0
               fpsign = .true.
               pbyte = 1
            else
c  unknown
               fptype = .false.
               fpsign = .false.
            endif

c  verify
            if (v .ge. 3) then
               write (6,"('gdm: size ',i5, ' by ',i5,', ',i3,
     +                ' bytes/pixel')") imwid, imhei, pbyte
               if (ffou) write (6,"('Fourier transform')")
               if (fptype .and. fpsign) write
     +                 (6,"('gdm: type ',a,', sign ',i2)") ptype, psign
            endif

c  check image height (should do width as well but this is a bit more
c  complicated)
            if (imhei .gt. maxsiz) then
               if (v .ge. 0) write (6,"('gdm: image height (',i6,
     +              ') greater than max size (',i6,')'
     +              /5x,'increase maxsiz and recompile')") imhei, maxsiz
               goto 120
            endif

c  read image
            if (ptype .eq. 'r') then
c  rgb image, need to translate into 3 pictures
c  read each row into buf, then put each byte into im in right place
               if (imhei .eq. 1) then
                  blen = imwid*4
                  rv = fileread(ifdes, bufc, blen)
                  if (rv .ne. blen) then
                     if (v .ge. 0) write (6,"('rv = ',i8,',
     +                         blen = ',i8)") rv, blen
                     goto 110
                  endif
                  do i=0,imwid-1
                     do k=0,2
                        im1dco(i,k) = bufi1(4*i+1+k)
                     enddo
                  enddo
               else
                  blen = imwid*4
                  do j=0,imhei-1
                     rv = fileread(ifdes, bufc, blen)
                     if (rv .ne. blen) then
                        if (v .ge. 0) write (6,"('rv = ',i8,',
     +                            dlen = ',i8)") rv, blen
                        goto 110
                     endif
                     do i=0,imwid-1
                        do k=0,2
                           imagco(i,j,k) = bufi1(4*i+1+k)
                        enddo
                     enddo
                  enddo
               endif
            elseif (ai2 .eq. 5) then
c  packed complex image. read as half plane complex
               if (imhei .eq. 1) then
                  do i=0,2*(imwid-1)-1
                     if (creadf4(ifdes, im1dr(i), bigend, 'gdm') .gt. 0)
     +                    goto 110
                  enddo
c  move real parts to rightful places & zero imag parts
                  im1dr(imwid*2-2) = im1dr(1)
                  im1dr(1) = 0
                  im1dr(imwid*2-1) = 0
               else
                  do j=0,imhei-1
                     do i=0,2*(imwid-1)-1
                        if (creadf4(ifdes, imagr(i,j), bigend, 'gdm')
     +                       .gt. 0) goto 110
                     enddo
                  enddo
c  move real parts to rightful places & zero imag parts etc
                  imagr(imwid*2-2, imhei/2) = imagr(0, 0)
                  imagr(imwid*2-1, imhei/2) = 0
                  imagr(imwid*2-2, 0) = imagr(1, 0)
                  imagr(imwid*2-1, 0) = 0
                  imagr(0, 0) = imagr(1, imhei/2)
                  imagr(1, 0) = 0
                  imagr(1, imhei/2) = 0
                  do j=1, imhei/2-1
                     imagr(imwid*2-2, imhei/2+j) = imagr(0, j)
                     imagr(imwid*2-1, imhei/2+j) = imagr(1, j)
                     imagr(imwid*2-2, imhei/2-j) = imagr(0, j)
                     imagr(imwid*2-1, imhei/2-j) = -imagr(1, j)
                  enddo
                  do j=1, imhei/2-1
                     imagr(0, imhei/2-j) = imagr(0, imhei/2+j)
                     imagr(1, imhei/2-j) = -imagr(1, imhei/2+j)
                  enddo
               endif
            else
c  any other image
               if (progbyteorder()) then
c  little endian (linux), need to swop bytes
                  write (6,"('gdm linux: not written this bit yet')")
                  goto 120
               else
c  big endian, can just read in blindly
                  if (imhei .eq. 1) then
                     blen = imwid*pbyte
                     rv = fileread(ifdes, im1dc, blen)
                     if (rv .ne. blen) then
                        if (v .ge. 0) write (6,"('rv = ',i8,',
     +                        blen = ',i8)") rv, blen
                        goto 110
                     endif
                  else
                     do j=0,imhei-1
                        blen = imwid*pbyte
                        rv = fileread(ifdes, imagc(0,j), blen)
                        if (rv .ne. blen) then
                           if (v .ge. 0) write (6,"('rv = ',i8,
     +                                       ', blen = ',i8)") rv, blen
                           goto 110
                        endif
                     enddo
                  endif
               endif
            endif
c  image read in so set fim
            fim = .true.
         elseif (tag .eq. x'3c') then

c  Notes
            rv = filereadi4(ifdes, i, bigend)
            if (rv .ne. 4) then
               if (v .ge. 0) write (6,"('gdm: notes length, rv = ',i8,
     +                              ' not 4')") rv
               goto 110
            endif
            ltitle = i
            rv = fileread(ifdes, title, ltitle)
            if (rv .ne. ltitle) then
               if (v .ge. 0) write (6,"('gdm: notes, rv = ',i8,
     +                              ' not ', i6)") rv, ltitle
               goto 110
            endif

c  space for elseif to deal with other tags here

         else
c  unknown or uninteresting tags. Just read to the end.
c  need to read in dlen bytes to get to end of this tag
c  read into bufc in lumps of bufsiz*4, the length of bufc
            if (dlen .gt. 0) then
               j = bufsiz*4
               do i=1,int(dlen/j)
                  rv = fileread(ifdes, bufc, j)
                  if (rv .ne. j) then
                     if (v .ge. 0) write
     +                  (6,"('gdm: unk tag 1, rv = ',i8,
     +                     ' not ',i6,', dlen = ',i8)") rv, j, dlen
                     goto 110
                  endif
               enddo
               i = mod(dlen, bufsiz*4)
               rv = fileread(ifdes, bufc, i)
               if (rv .ne. i) then
                  if (v .ge. 0) write
     +               (6,"('gdm: unk tag 2, rv = ',i8,
     +                                 ' not ', i6)") rv, i
                  goto 110
               endif
            endif

c  finished with tag
         endif


c  go back and read next field
      goto 10


c  normal end of file
 100  if (v .ge. 3) write (6,"('gdm: normal end of file')")
      gdm = 0
      return

c  unexpected end of file
 110  if (v .ge. 3) write (6,"('gdm: unexpected end of file')")
      gdm = 1
      return

c  error somewhere
 120  if (v .ge. 0) write (6,'(''gdm: error somewhere'')')
      gdm = 1
      return

      end

c--------------------------------------------------------------------------
      function dmdump()

c  dump Digital Micrograph file

c  structure is:
c    2 bytes       tag, identifies what rest of field contains
c    4 bytes       dlen, length of rest of field in bytes
c    dlen bytes    the data


c  local variables
      character tname*100, tval*100
      integer  tag
      integer*4 dlen, ltname, ltval, i, j, ipos
      integer*8 pos8

c  common variables
      include "vars.h"


c  now get on with it
      if (v .ge. 3) write (6,"('starting dmdump')")

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

c  skip to start if macbinary
      if (dstart .gt. 0) then
         pos8 = dstart
         rv = fileseek(ifdes, pos8)
         if (rv .ne. dstart) then
            if (v .ge. 0) write
     +           (6,"('dmdump: fileseek returned ',i11,' not ',i6)")
     +           rv, dstart
            goto 120
         endif
      endif

c  go through each field until end of file
 10   continue

c  read tag
         rv = filereadi2(ifdes, tag, bigend)
c  rv=0 means end of file
         if (rv .eq. 0) goto 100
         if (rv .ne. 2) then
            write (6,"('rv returned as ',i6,' when reading tag')") rv
            goto 110
         endif

c  rv=2, ie tag read in, so read length
         rv = filereadi4(ifdes, dlen, bigend)
         if (rv .ne. 4) then
            write (6,"('rv returned as ',i6,' when reading length')") rv
            goto 110
         endif

c  check dlen
         if (dlen .lt. 0) then
            write (6,"('dlen = ',i8)") dlen
            goto 120
         endif

c  read data part of field
         if (dlen .gt. 0) then
            rv = fileread(ifdes, im1dc, dlen)
            if (rv .ne. dlen) then
               write (6,"('rv = ',i8,', dlen = ',i12)") rv, dlen
               goto 120
            endif
         endif

c  write out
c  first look for known tags
         if (tag .eq. x'ffff') then
c  image
            write (6,"(z4, i8, ' Image, size ',i5,' by ',i5,', ',i3,
     +             ' bytes/pixel, type ',i3)")
     +            tag, dlen, (im1di2(i), i=0,3)
            write (6,"(12x, 12(z3.2, z2.2))")
     +            (im1dc(i), i=8,min(dlen-8,24)+7)
         elseif (tag .eq. x'3d') then
c  version number
            write (6,"(z4, i8,' DM version: ',f5.2,i10)")
     +            tag, dlen, im1di4(0)/100.0, im1di4(0)
         elseif (tag .eq. x'3c') then
c  notes
            write (6,"(z4, i8, ' Notes, length ',i5)")
     +            tag, dlen, im1di4(0)
            write (6,"(a)") im1dcs(5:dlen)
         elseif (tag .eq. x'3b') then
c  local picture tags
            write (6,"(z4, i8, ' Local tags. Total number: ', i6)")
     +            tag, dlen, im1di4(0)
c            do j=0,int((dlen-1)/16)
c               if = 16*j+1
c               in = min(dlen - 16*j, 16)
c               bufc = im1dcs(if:if+in-1)
c               call printchar(bufc(1:in))
c               write (6,"(12x, 4(1x,4z2.2), 4(1x,4a1))")
c     +            (im1di1(i), i=if-1,if+in-2), (bufc(i:i), i=1,in)
c            enddo
c            write (6,"(13x,'last line prints wrongly...')")
            ipos = 5
            do j=1,im1di4(0)
c  first copy the integers & chars into tname, tval, bufi2 (via bufc)
               bufc(1:8) = im1dcs(ipos:ipos+7)
               ipos = ipos + 8
               ltname = bufi2(3)
               tname = im1dcs(ipos:ipos+ltname-1)
               ipos = ipos + ltname
               bufc(9:24) = im1dcs(ipos:ipos+15)
               ipos = ipos + 16
               ltval = bufi2(11)
               tval = im1dcs(ipos:ipos+ltval-1)
               ipos = ipos + ltval
               bufc(25:44) = im1dcs(ipos:ipos+19)
               ipos = ipos + 20
c  now write out
               write (6,"(13x,'tag ',i3 /13x,4i6 /13x,'""',a,'""'
     +                   /13x,8i6 /13x,'""',a,'""' /13x, 10i6 /)"), j,
     +               (bufi2(i), i=0,3), tname(1:ltname),
     +               (bufi2(i), i=4,11), tval(1:ltval),
     +               (bufi2(i), i=12,21)
            enddo

         elseif (tag .eq. x'2d') then
c  display type = raster image
            write (6,"(z4, i8,' Display type = raster image?')")
     +            tag, dlen
         elseif (tag .eq. x'2e') then
c  display type = surface plot
            write (6,"(z4, i8,' Display type = surface plot?')")
     +            tag, dlen
         elseif (tag .eq. x'1f4') then
c  display type = line plot
            write (6,"(z4, i8,' Display type = line plot?')")
     +            tag, dlen
         elseif (tag .eq. x'16') then
c  display magnification
            write (6,"(z4, i8,' Display magnification: ',f12.5,
     +             ' screen pixels/image pixel')")
     +            tag, dlen, im1dr(0)
         elseif (tag .eq. x'3e') then
c  picture position in window
            write (6,"(z4, i8,
     +             ' Position of top left of picture in window: ',
     +             i6,', ',i6)")
     +            tag, dlen, im1di2(0), im1di2(1)
         elseif (tag .eq. x'1b') then
c  picture maximum value
            write (6,"(z4, i8,' Picture maximum value: ',
     +             f12.5, 1pe12.5)")
     +            tag, dlen, im1dr(0), im1dr(0)
         elseif (tag .eq. x'1c') then
c  picture minimum value
            write (6,"(z4, i8,' Picture minimum value: ',
     +             f12.5, 1pe12.5)")
     +            tag, dlen, im1dr(0), im1dr(0)
         elseif (tag .eq. x'23') then
c  normal/inverted contrast
            if (im1di1(0) .eq. 0) then
               write (6,"(z4, i8,' Normal contrast (0 = black)')")
     +               tag, dlen
            elseif (im1di1(0) .eq. 1) then
               write (6,"(z4, i8,' Inverted contrast (0 = white)')")
     +               tag, dlen
            else
               write (6,"(z4, i8,
     +                ' picture contrast, should be 0 or 1')")
     +               tag, dlen
               bufc = im1dcs(1:dlen)
               call printchar(bufc(1:dlen))
               write (6,"(z4, i8, 1x, z2.2, 18x, a1, 7x, i11)")
     +               tag, dlen, ichar(im1dc(0)), bufc(1:dlen),
     +               ichar(im1dc(0))
            endif
         elseif (tag .eq. x'd') then
c  colour mode
            write (6,"(z4, i8,' Colour mode: ', i2)")
     +            tag, dlen, im1di2(0)
         elseif (tag .eq. x'c') then
c  contrast mode
            write (6,"(z4, i8,' Contrast mode: ', i2)")
     +            tag, dlen, im1di2(0)
         elseif (tag .eq. x'27') then
c  survey
            if (im1di1(0) .eq. 0) then
               write (6,"(z4, i8,' Picture survey off')")
     +            tag, dlen
            elseif (im1di1(0) .eq. 1) then
               write (6,"(z4, i8,' Picture survey on')")
     +            tag, dlen
            else
               write (6,"(z4, i8,' Picture survey: ', i3)")
     +            tag, dlen, im1di1(0)
            endif
         elseif (tag .eq. x'28') then
c  cross wires
            if (im1di2(0) .eq. 0) then
               write (6,"(z4, i8,' Survey cross-wires')")
     +            tag, dlen
            elseif (im1di2(0) .eq. 1) then
               write (6,"(z4, i8,' Survey entire image')")
     +            tag, dlen
            else
               write (6,"(z4, i8,' Survey type: ', i3)")
     +            tag, dlen, im1di2(0)
            endif
         elseif (tag .eq. x'11') then
c  displayed black value
            write (6,"(z4, i8,' Value to display as black: ',
     +             f12.5, 1pe12.5)")
     +            tag, dlen, im1dr(0), im1dr(0)
         elseif (tag .eq. x'12') then
c  displayed white value
            write (6,"(z4, i8,' Value to display as white: ',
     +             f12.5, 1pe12.5)")
     +            tag, dlen, im1dr(0), im1dr(0)
         elseif (tag .eq. x'26') then
c  minimum contrast
            write (6,"(z4, i8,' Minimum contrast: ',
     +             f12.5, 1pe12.5)")
     +            tag, dlen, im1dr(0), im1dr(0)
         elseif (tag .eq. x'19') then
c  postion of window on screen
            write (6,"(z4, i8,' Window position & size on screen')")
     +            tag, dlen
            write (6,"(13x, 'Top: ',i6,', left: ',i6,
     +             ', bottom: ',i6,', right: ',i6)") (im1di2(i), i=0,3)
         elseif (tag .eq. x'0') then
c  end of file
            write (6,"(z4, i8,' End of file')")
     +            tag, dlen

c  now go for tags of obvious length
         elseif (dlen .eq. 0) then
            write (6,"(z4, i8)") tag, dlen
         elseif (dlen .eq. 1) then
            bufc = im1dcs(1:dlen)
            call printchar(bufc(1:dlen))
            write (6,"(z4, i8, 1x, z2.2, 18x, a1, 7x, i11)")
     +            tag, dlen, ichar(im1dc(0)), bufc(1:dlen),
     +            ichar(im1dc(0))
         elseif (dlen .eq. 2) then
            bufc = im1dcs(1:dlen)
            call printchar(bufc(1:dlen))
            write (6,"(z4, i8, 1x, z4.4, 16x, a2, 6x, i11)")
     +            tag, dlen, im1di2(0), bufc(1:dlen), im1di2(0)
         elseif (dlen .eq. 4) then
            bufc = im1dcs(1:dlen)
            call printchar(bufc(1:dlen))
            if (im1di4(0) .eq. 0 .or.
     +          (im1di4(0) .gt. x'01000000' .and.
     +           im1di4(0) .lt. x'7e000000') .or.
     +          (im1di4(0) .gt. x'81000000' .and.
     +           im1di4(0) .lt. x'fe000000')) then
               write (6,"(z4, i8, 2z5.4, 11x, a4, 4x, i11,
     +             f12.5, 1pe12.5)")
     +            tag, dlen, im1di2(0), im1di2(1), bufc(1:dlen),
     +            im1di4(0), im1dr(0), im1dr(0)
            else
               write (6,"(z4, i8, 2z5.4, 11x, a4, 4x, i11)")
     +            tag, dlen, im1di2(0), im1di2(1), bufc(1:dlen),
     +            im1di4(0)
            endif
         elseif (dlen .eq. 6) then
            bufc = im1dcs(1:dlen)
            call printchar(bufc(1:dlen))
            write (6,"(z4, i8, 3z5.4, 6x, a6, 2x, 3i6)")
     +            tag, dlen, (im1di2(i), i=0,2), bufc(1:dlen),
     +            (im1di2(i), i=0,2)
         elseif (dlen .eq. 8) then
            bufc = im1dcs(1:dlen)
            call printchar(bufc(1:dlen))
            write (6,"(z4, i8, 4z5.4, 1x, a8, 4i6)")
     +            tag, dlen, (im1di2(i), i=0,3), bufc(1:dlen),
     +            (im1di2(i), i=0,3)
         else
c  finally any other tags
            j = 60
            bufc = im1dcs(1:min(dlen,j))
            call printchar(bufc(1:min(dlen,j)))
            write (6,"(z4, i8, 12(z3.2, z2.2))")
     +            tag, dlen, (im1dc(i), i=0,min(dlen,24)-1)
            write (6,"(13x, 60a1)") (bufc(i:i), i=1,min(dlen,j))
         endif

c  go back and read next field
      goto 10


c  normal end of file
 100  if (v .ge. 3) write (6,"('dmdump: normal end of file')")
      dmdump = 0
      return

c  unexpected end of file
 110  if (v .ge. 3) write (6,"('dmdump: unexpected end of file')")
      dmdump = 1
      return

c  error somewhere
 120  if (v .ge. 0) write (6,"('dmdump: error somewhere')")
      dmdump = 1
      return

      end

c--------------------------------------------------------------------------
      function wdm2()

c  write Digital Micrograph 2 file
c  NOT FINISHED YET doesn't do rgb or packed complex

c  structure is:
c    2 bytes       tag, identifies what rest of field contains
c    4 bytes       dlen, length of rest of field in bytes
c    dlen bytes    the data


c  local variables
c      character tname*100, tval*100
c      integer*4 dlen, ltname, ltval, i, j, ipos
c      integer*8 pos8
      integer  tag
      integer*4 dlen, dtype, blen, i, j, k
      integer*8 imdlenpos
      character ctag*2, cdlen*4

c  common variables
      include "vars.h"

c  local equivalences
      equivalence (tag, ctag), (dlen, cdlen)


c  set wdm2 to 1 so any returns in program are errors
c  only set wdm2 to 0 at end of program
      wdm2 = 1

c  now get on with it
      if (v .ge. 3) write (6,"('wdm2: starting wdm2')")

c  open file to write to
      if (v .ge. 3) write (6,"('wdm2: output file name ""',a,'""')")
     +                     opath(1:lopath)
      ofdes = fileopenw(opath(1:lopath))
      if (ofdes .le. 0 .and. v .ge. 0) then
         write (6,"('cannot open ',a)") opath(1:lopath)
         return
      endif

c  set min and max values. Want to end up with dismin and dismax set
      if (.not. fdismin .or. .not. fdismax) then
c  either dismin or dismax not set, need to find from somewhere
c  first try getting from imgmin and imgmax
         if (.not. fdismin .and. fimgmin) then
            dismin = imgmin
            fdismin = .true.
         endif
         if (.not. fdismax .and. fimgmax) then
            dismax = imgmax
            fdismax = .true.
         endif
      endif

c  now check fdismin and fdismax again and survey if not set
      if (.not. fdismin .or. .not. fdismax) then
c  either dismin or dismax not set, need to survey image
         call survey()
         if (.not. fdismin .and. fimgmin) then
            dismin = imgmin
            fdismin = .true.
         endif
         if (.not. fdismax .and. fimgmax) then
            dismax = imgmax
            fdismax = .true.
         endif
      endif

c  Just to be paranoid
      if (.not. fdismin .or. .not. fdismax) then
         if (v .gt. 0) write
     +       (6,"('wdm2: Either fdismin or fdismax not set)")
      endif

c  write version tag, 200 for DM 2.0
      tag = x'3d'
      dlen = 4
      i4 = 200
      bufc = ctag // cdlen // ci4
      if (cwrite(ofdes, bufc, 10, 'wdm2') .gt. 0) goto 120

c  write image
c  set data type, dtype. need to do before writing pbyte
      dtype = -1
      if (ptype .eq. 'i') then
         if (pbyte .eq. 1) then
            if (psign .eq. 0) then
c              1 byte unsigned int
               dtype = 6
            elseif (psign .eq. 1) then
c              1 byte signed int
               dtype = 9
            endif
         elseif (pbyte .eq. 2) then
            if (psign .eq. 0) then
c              2 byte unsigned int
               dtype = 10
            elseif (psign .eq. 1) then
c              2 byte signed int
               dtype = 1
            endif
         elseif (pbyte .eq. 4) then
            if (psign .eq. 0) then
c              4 byte unsigned int
               dtype = 11
            elseif (psign .eq. 1) then
c              4 byte signed int
               dtype = 7
            endif
         endif
      elseif (ptype .eq. 'f') then
         if (pbyte .eq. 4) then
c           4 byte real
            dtype = 2
         elseif (pbyte .eq. 8) then
c           8 byte real
            dtype = 12
         endif
      elseif (ptype .eq. 'c') then
         if (pbyte .eq. 8 .and. ffou .and. forigx .and. origx .eq. 1)
     +           then
c           packed complex
            dtype = 5
         elseif (pbyte .eq. 8) then
c           8 byte complex
            dtype = 3
         elseif (pbyte .eq. 16) then
c           16 byte complex
            dtype = 13
         endif
      elseif (ptype .eq. 'r') then
c        4 bytes, rgb colour
         dtype = 8
      else
         if (v .ge. 2) write (6,"(
     +   'Unknown image type: ptype=',a1,', pbyte=',i3,', psign=',i3)")
     +      ptype, pbyte, psign
         goto 120
      endif

c  write tag length and check at end
      tag = x'ffff'
      if (cwrite(ofdes, ctag, 2, 'wdm2') .gt. 0) goto 120

      dlen = imwid*imhei*pbyte + 8
      if (dtype .eq. 5) dlen = 2*(imwid-1)*imhei*pbyte/2 + 8
      if (dtype .eq. 8) dlen = imwid*imhei*pbyte*4 + 8
      if (cwrite(ofdes, cdlen, 4, 'wdm2') .gt. 0) goto 120
      imdlenpos = filepos(ofdes)

c  now write image itself
c  first 8 bytes of header
      i2 = imwid
      if (dtype .eq. 5) i2 = 2*(imwid-1)
      if (cwrite(ofdes, ci2, 2, 'wdm2') .gt. 0) goto 120
      i2 = imhei
      if (cwrite(ofdes, ci2, 2, 'wdm2') .gt. 0) goto 120
      i2 = pbyte
      if (dtype .eq. 5) i2 = pbyte/2
      if (dtype .eq. 8) i2 = pbyte*4
      if (cwrite(ofdes, ci2, 2, 'wdm2') .gt. 0) goto 120
      i2 = dtype
      if (cwrite(ofdes, ci2, 2, 'wdm2') .gt. 0) goto 120
      if (v .ge. 3) write (6,"('wdm2: pbyte = ',i6,', dtype = ',i6)")
     +              pbyte, dtype

c  write image
      if (dtype .eq. 8) then
c  rgb image
         if (v .gt. 3) write (6,"('rgb image')")
         if (imhei .eq. 1) then
c  Assemble the line into bufc and write in one go
            do i=0,imwid-1
               do k=0,2
                  bufi1(4*i+1+k) = im1dco(i,k)
               enddo
               bufi1(4*i) = 0
            enddo
            blen = imwid*4
            if (cwrite(ofdes, bufc, blen, 'wdm2') .gt. 0) goto 120
         else
c  Assemble each line into bufc and write in one go
            blen = imwid*4
            do j=0,imhei-1
               do i=0,imwid-1
                  do k=0,2
                     bufi1(4*i+1+k) = imagco(i,j,k)
                  enddo
                  bufi1(4*i) = 0
               enddo
               if (cwrite(ofdes, bufc, blen, 'wdm2') .gt. 0) goto 120
            enddo
         endif

      elseif (dtype .eq. 5) then
c  packed complex
         if (imhei .eq. 1) then

c            write (6,"('imwid = ',i10)") imwid
c            do i=0,imwid-1
c               write (6,"(i10,2f12.5)") i,im1dr(2*i), im1dr(2*i+1)
c            enddo

            f4 = im1dr(0)
            if (cwrite(ofdes, cf4, 4, 'wdm2') .gt. 0) goto 120
            f4 = im1dr(2*imwid-2)
            if (cwrite(ofdes, cf4, 4, 'wdm2') .gt. 0) goto 120
            blen = pbyte*(imwid-2)
            if (cwrite(ofdes, im1dcs(9:9+blen-1), blen, 'wdm2') .gt. 0)
     +          goto 120
         else
            write (6,"('2-D packed complex')")
         endif

      else
c  all other image types, just copy the data
         blen = imwid*pbyte
         if (imhei .eq. 1) then
            if (cwrite(ofdes, im1dcs, blen, 'wdm2') .gt. 0) goto 120
         else
            do j=0,imhei-1
               if (cwrite(ofdes, imagc(0,j), blen, 'wdm2') .gt. 0)
     +            goto 120
            enddo
         endif
c  finished writing image
      endif

c  check position agrees with dlen
      i4 = filepos(ofdes) - imdlenpos
      if (i4 .ne. dlen) then
         if (v .ge. 0) write (6,"('wdm2: Not in expected position'/
     +                 'dlen = ',i10,', moved by ',i10)") dlen, i4
      endif

c  Notes box tag - title
      if (ltitle .gt. 0) then
         tag = x'3c'
         dlen = ltitle + 4
         i4 = ltitle
         bufc = ctag // cdlen // ci4
         if (cwrite(ofdes, bufc, 10, 'wdm2') .gt. 0) goto 120
         if (cwrite(ofdes, title, ltitle, 'wdm2') .gt. 0) goto 120
      endif

      if (dtype .eq. 8) then
c  For rgb images must set tag 37
         tag = x'37'
         dlen = 0
         bufc = ctag // cdlen
         if (cwrite(ofdes, bufc, 6, 'wdm2') .gt. 0) goto 120
      else
c  Min and max values not needed for rgb images
c  image maximum value
         if (fimgmax) then
            tag = x'1b'
            dlen = 4
            f4 = imgmax
            bufc = ctag // cdlen // cf4
            if (cwrite(ofdes, bufc, 10, 'wdm2') .gt. 0) goto 120
         endif

c  image minimum value
         if (fimgmin) then
            tag = x'1c'
            dlen = 4
            f4 = imgmin
            bufc = ctag // cdlen // cf4
            if (cwrite(ofdes, bufc, 10, 'wdm2') .gt. 0) goto 120
         endif

c  display maximum value
         if (fdismax) then
            tag = x'12'
            dlen = 4
            f4 = dismax
            bufc = ctag // cdlen // cf4
            if (cwrite(ofdes, bufc, 10, 'wdm2') .gt. 0) goto 120
         endif

c  display minimum value
         if (fdismin) then
            tag = x'11'
            dlen = 4
            f4 = dismin
            bufc = ctag // cdlen // cf4
            if (cwrite(ofdes, bufc, 10, 'wdm2') .gt. 0) goto 120
         endif
      endif

c  write final tag
      tag = 0
      dlen = 0
      bufc = ctag // cdlen
      if (cwrite(ofdes, bufc, 6, 'wdm2') .gt. 0) goto 120

c  finished writing with no problems
      wdm2 = 0

c  close file
 120  rv = fileclose(ofdes)
      if (rv .ne. 0) then
         if (v .ge. 0) write
     +                 (6,"('wdm2: fileclose returned ',i6,' not 0')")
     +                 rv
         wdm2 = 1
      endif

      return

      end
