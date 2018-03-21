c  dm3.f
c  Reads and dumps Digital Micrograph 3 files

c--------------------------------------------------------------------------
      function gdm3(imgget)

c  gets Digital Micrograph 3 file
c  unfortunately the image comes first, so a set of equivalenced
c  arrays of different types is needed as well as im...()

c  imgget is the image we actually want to get. It is usually the first
c  image, but for enfina spectra it is the second...

c  local variables
      character tname*100, tag*1, nc*1, adate*100, atime*100, ampm*10
      integer*2 ltnamei2, t14n2, ni2, ladate, latime, lampm
      integer*4 i, j, ai4, flength, ltname, t14n1, nnum, ntype, ni4
      integer*4 it1, it2, it3, it4
      integer*4 itype, ngroup, k, nfentries, dimen, dtype, dpos, blen
      integer*4 datatype, imgnum, imgget, prevnum
      integer*4 year, month, day, hour, minute, sec
      logical   brev, progbo, swopb
      real*4    nf4
      real*8    nf8

c  common variables
      include "vars.f"

c  byte order...
c   brev   = .true.  bytes in dm3 image are reversed (little endian, PC order)
c                    set from header of dm3 file
c   progbo = .true.  program is running on linux (little endian)
c   swopb  = .true.  bytes need to be swopped
c   swopb  = .not. (progbo .eqv. brev)


c  now get on with it
      if (v .ge. 3) write (6,"('starting gdm3')")

c  find program byte order
      progbo = progbyteorder()

c  imgnum is the number of the image in the file. It is initially
c  set to 0 and incremented when the tag ImageData is found.
      imgnum = 0

c  dimen =0 if not found dimensions tag, =1 if found, =2 if set width
c    =3 if set height
      dimen = 0

c  dpos is positon of data. set to -1 so can check if not found
      dpos = -1

c  adate and atime are the acquisition date & time strings
      adate = ' '
      ladate = 0
      atime = ' '
      latime = 0

c  open file
      ifdes = copenr(path(1:lpath), 'gdm3')
      if (ifdes .le. 0) goto 120

c  skip to start if macbinary
      if (dstart .gt. 0) then
         if (clseek(ifdes, dstart, 'gdm3') .gt. 0) goto 120
      endif

c  header...
c  first i4 is 3
      if (creadi4(ifdes, ai4, .false., 'gdm3') .ne. 0) goto 120
      if (v .ge. 3 .and. ai4 .ne. 3) write (6,"(First number is ',i12
     +     ,' not 3')") ai4

c  second is file length - 16
      if (creadi4(ifdes, flength, .false., 'gdm3') .ne. 0) goto 120
c  third is byte reverse flag
      if (creadi4(ifdes, ai4, .false., 'gdm3') .ne. 0) goto 120
      if (ai4 .eq. 0) then
         brev = .false.
      elseif (ai4 .eq. 1) then
         brev = .true.
      else
         brev = .false.
         if (v .ge. 0) write (6,"('gdm3: byte reverse flag = ',i11,
     +            ' not 0 or 1')") ai4
      endif

c  set swopb. If set means bytes have to be swopped when reading in
c  data part of dm3 file
      swopb  = .not. (progbo .eqv. brev)

c  forth number is unknown, and always seems to be 1
      if (creadi4(ifdes, ai4, .true., 'gdm3') .ne. 0) goto 120
c  fifth is likewise, & is either 18 or 20...
      if (creadi2(ifdes, ai2, .false., 'gdm3') .ne. 0) goto 120


c  go through each field until end of file
 10   continue

c  this sets all of tname to ' ', not just first char, needed for
c  a30 later on
         tname = ' '

c  read tag
         if (creadc(ifdes, tag, 1, 'gdm3') .ne. 0) goto 120
         if (v .ge. 3) write (6,"('gdm3: tag = ',z2)") tag

c  check tag value...

         if (ichar(tag) .eq. 0) then
c  tag = 0, end of file
            goto 20

         elseif (ichar(tag) .eq. x'14') then
c  tag = 14
c  read length of tag name
            if (creadi2(ifdes, ltnamei2, .false., 'gdm3') .ne. 0)
     +               goto 120
            if (v .ge. 3) write (6,"('gdm3: ltnamei2 = ',i11)")
     +                          ltnamei2
            ltname = ltnamei2
            if (ltname .gt. 100) then
               if (v .ge. 0) write (6,"('gdm3: ',z2
     +              ,' Extra long tag name, ',i4,', something''s up')")
     +              tag, ltname
               goto 120
            endif
c  read tag name
            if (creadc(ifdes, tname, ltname, 'gdm3') .ne. 0) goto 120
            if (v .ge. 3) write (6,"('gdm3: tag name = ""',a,'""')")
     +                          tname(1:ltname)
c  read numbers
            if (creadi4(ifdes, t14n1, .true., 'gdm3') .ne. 0) goto 120
            if (v .ge. 3) write (6,"('gdm3: t14n1 = ',i11)")
     +                          t14n1
            if (creadi2(ifdes, t14n2, .false., 'gdm3') .ne. 0) goto 120
            if (v .ge. 3) write (6,"('gdm3: t14n2 = ',i11)")
     +                          t14n2

c  look for useful value(s)
c  increment imgnum when tag ImageData is found
            if (tname(1:ltname) .eq. 'ImageData') then
               imgnum = imgnum + 1
               if (v .ge. 3) write (6,"('gdm3: starting image number ',
     +                             i4)") imgnum
c                if imgnum>imgget then there another image beyond the one
c                we are getting, so set fanim
               if (imgnum .gt. imgget) then
                  fanim = .true.
                  if (v .ge. 3) write (6,"(
     +                          'gdm3: another image still to get')")
               endif
            endif

c  set dimen to show dimensions found, but only if it is the first occurence
c  and imgget=imgnum
c  Also set fimwid and fimhei to false as they may have been set earlier
            if (tname(1:ltname) .eq. 'Dimensions') then
               if (v .ge. 3) write (6,"('gdm3: Dimensions found')")
               if (imgget .eq. imgnum .and. dimen .eq. 0) then
                  dimen = 1
                  fimwid = .false.
                  fimhei = .false.
               endif
            endif

         elseif (ichar(tag) .eq. x'15') then
c  tag = 15
c  read length of tag name
            if (creadi2(ifdes, ltnamei2, .false., 'gdm3') .ne. 0)
     +               goto 120
            ltname = ltnamei2
            if (ltname .gt. 100) then
               if (v .ge. 0) write (6,"('gdm3: 'z2
     +              ,' Extra long tag name, ',i4,', something''s up')")
     +              tag, ltname
               goto 120
            endif
c  read tag name
            if (creadc(ifdes, tname, ltname, 'gdm3') .ne. 0) goto 120
c  read %%%%
            if (creadc(ifdes, ci4, 4, 'gdm3') .ne. 0) goto 120
            if (ci4 .ne. '%%%%') then
               if (v .ge. 0) write (6,"(z2,' ',a30
     +              ,' next part not %%%%')") tag, tname
               goto 120
            endif
c  read number of numbers
            if (creadi4(ifdes, nnum, .false., 'gdm3') .ne. 0) goto 120
c  read all numbers into bufi4
            do i=1,nnum
               if (creadi4(ifdes, bufi4(i), .false., 'gdm3') .ne. 0)
     +                     goto 120
            enddo
c  verify entries if v>=3
            if (v .ge. 3 .and. nnum .ge. 1) then
               write (6,"('    First 11 entries')")
               do i=1,min(nnum,11)
                  write (6,"(4x,i4,i11,3x,z8)") i, bufi4(i), bufi4(i)
               enddo
            endif

            nfentries = 0

c  check to see if bufi4(1) is 14h
            if (bufi4(1) .eq. x'14') then
c              itype is the position of the data type
               itype = 2
c              ngroup is the number in the group
               ngroup = bufi4(nnum)
c              ntype is the data type (may be f)
               ntype = bufi4(itype)
c              write out
               if (ntype .eq. x'f') then
                  nfentries = bufi4(4)
                  if (v .ge. 3) write (6,"(z2,' ',a30,' nnum',i6
     +                 ,', type 14, ngroup',i6,', type ',z2
     +                 ,', entries ',i6)") tag, tname, nnum, ngroup,
     +                 ntype, nfentries
c  check number of entries
                  if (nfentries*2+5 .ne. nnum) then
                     if (v .ge. 0) write (6,"
     +                    ('Confused, expect 4th number (',i11
     +                    ,') *2 + 5'
     +                    /'   to be the number of numbers (',i11,')')"
     +                    ) bufi4(4), nnum
                     if (v .ge. 3) then
                        write (6,"('  First 11 entries')")
                        do i=1,min(nnum,11)
                           write (6,"(4x,i4,i11,3x,z8)")
     +                       i,bufi4(i), bufi4(i)
                        enddo
                     endif
                     goto 120
                  endif
               else
                  if (v .ge. 3) write (6,"(z2,' ',a30,' nnum',i6
     +                 ,', type 14, ngroup',i11,', type ',z2)") tag,
     +                 tname, nnum, ngroup, ntype
               endif

c  look for data and save position etc.
c  set values only if first time, ie if (dpos = -1) now imgnum = imgget
               if (tname(1:ltname) .eq. 'Data') then
                  if (ntype .eq. x'f') then
c                    set to first dtype, assumes all the same...
                     dtype = bufi4(6)
                  else
                     dtype = ntype
                  endif
                  if (imgget .eq. imgnum) then
                     dpos = filepos(ifdes)
                     if (dpos .le. 0) goto 120
                     if (v .ge. 3) write (6,"(
     +                 'gdm3: image data starts at ',i11)") dpos
                  endif
               endif
            else
               itype = 1
               ngroup = 1
               ntype = bufi4(itype)
               if (ntype .eq. x'f') then
                  nfentries = bufi4(3)
                  if (v .ge. 3) write (6,"(z2,' ',a30,' nnum',i6
     +                 ,', type ',z2,', entries ',i6)") tag, tname, nnum
     +                 , ntype, nfentries
c  check number of entries
                  if (nfentries*2+3 .ne. nnum) then
                     if (v .ge. 0) write (6,"
     +                    ('Confused, expect 3rd number (',i11
     +                    ,') *2 + 3'
     +                    /'   to be the number of numbers (',i11,')')"
     +                    ) bufi4(3), nnum
                     if (v .ge. 3) then
                        write (6,"('  First 11 entries')")
                        do i=1,min(nnum,11)
                           write (6,"(4x,i4,i11,3x,z8)")
     +                        i,bufi4(i), bufi4(i)
                        enddo
                     endif
                     goto 120
                  endif
               else

c  not 14, not f, ie one number only. get data as well here.
                  if (ntype .eq. 2) then
c                    i2 rev
                     if (creadi2(ifdes, ni2, brev, 'gdm3') .ne. 0)
     +                                                         goto 120
                     if (v .ge. 3) write (6,"(z2,' ',a30,'    i2     '
     +                    ,i11)") tag, tname, ni2
                  elseif (ntype .eq. 3) then
c                    i4 rev
                     if (creadi4(ifdes, ni4, brev, 'gdm3') .ne. 0)
     +                                                         goto 120
                     if (v .ge. 3) write (6,"(z2,' ',a30,'    i4     '
     +                    ,i11)") tag, tname, ni4
                  elseif (ntype .eq. 4) then
c                    i2 rev unsigned (read as signed here...)
                     if (creadi2(ifdes, ni2, brev, 'gdm3') .ne. 0)
     +                                                         goto 120
                     if (v .ge. 3) write (6,"(z2,' ',a30,'    i2 uns '
     +                    ,i11)") tag, tname, ni2
                  elseif (ntype .eq. 5) then
c                    i4 rev unsigned (read as signed here...)
                     if (creadi4(ifdes, ni4, brev, 'gdm3') .ne. 0)
     +                                                         goto 120
                     if (v .ge. 3) write (6,"(z2,' ',a30,'    i4 uns '
     +                    ,i11)") tag, tname, ni4
                  elseif (ntype .eq. 6) then
c                    f4 rev
                     if (creadf4(ifdes, nf4, brev, 'gdm3') .ne. 0)
     +                                                         goto 120
                     if (v .ge. 3) write (6,"(z2,' ',a30,'    f4     '
     +                    ,e11.3)") tag, tname, nf4
                  elseif (ntype .eq. 7) then
c                    f8 rev
                     if (creadf8(ifdes, nf8, brev, 'gdm3') .ne. 0)
     +                                                         goto 120
                     if (v .ge. 3) write (6,"(z2,' ',a30,'    f8     '
     +                    ,d11.3)") tag, tname, nf8
                  elseif (ntype .eq. 8) then
c                    byte
                     if (creadc(ifdes, nc, 1, 'gdm3') .ne. 0)
     +                                                         goto 120
                     if (v .ge. 3) write (6,"(z2,' ',a30,'    byte   '
     +                    ,i11)") tag, tname, ichar(nc)
                  else
                     if (v .ge. 0) write (6,"(z2,' ',a30
     +                    ,'    unknown type ',i11)") tag, tname, ntype
                     goto 120
                  endif

c  look for useful values, only interested in image imgget)
                  if (ltname .eq. 0 .and. imgnum .eq. imgget) then
c  picture dimensions
                     ai4 = dimen
                     if (dimen .eq. 1) then
c  dimen = 1, Dimensions found, must be width
                        imwid = ni4
                        fimwid = .true.
                        ai4 = 2
                        if (v .ge. 3) write (6,"('gdm3: image width '
     +                        ,i11)") imwid
                     elseif (dimen .eq. 2) then
c  dimen = 2, Dimensions found, must be height
                        imhei = ni4
                        fimhei = .true.
                        ai4 = 3
                        if (v .ge. 3) write (6,"('gdm3: image height '
     +                        ,i11)") imhei
                     endif
                     dimen = ai4

                  elseif (tname(1:ltname) .eq. 'PixelDepth'
     +                    .and. imgnum .eq. imgget) then
c  bytes/pixel
                     pbyte = ni4
                     fpbyte = .true.
c  Bit of a bodge this - set dimen = 10 when find PixelDepth as PixelDepth follows
c  Dimensions
                     dimen = 10
                  elseif (tname(1:ltname) .eq. 'DataType'
     +                    .and. imgnum .eq. imgget) then
c  Image data type...
                     datatype = ni4
                  elseif (tname(1:ltname) .eq. 'ImageIndex') then
c  Imageindex is the number of the preview (thumbnail) image (first = 0)
c  set fpreview if the image we are getting is a preview image
c  it can then be ignored in ic.f
                     prevnum = ni4
                     if (prevnum+1 .eq. imgget) then
                        fpreview = .true.
                     else
                        fpreview = .false.
                     endif
c  Min and max - it is not at all clear where these are stored... below is wrong
c                  elseif (tname(1:ltname) .eq. 'Reference level'
c     +                    .and. imgnum .eq. imgget) then
c  display min (Reference level)
c                     dismin = nf4
c                     fdismin = .true.
c  xxxxxxxxxxxxx test
c                     write(6,"('dismin = ',f12.5)") dismin
c                  elseif (tname(1:ltname) .eq. 'Maximum level'
c     +                    .and. imgnum .eq. imgget) then
c  display max (Maximum level)
c                     dismax = nf4
c                     fdismax = .true.
c  xxxxxxxxxxxxx test
c                     write(6,"('dismax = ',f12.5)") dismax
                  endif
               endif
            endif

c  have written out single line, now need to do type f
            if (ntype .eq. x'f') then


c  for each entry read according to type to find total bytes
               ai4 = 0
               do i=1,nfentries
                  j = (i-1)*2 + 4 + itype
                  if (bufi4(j) .eq. 2) then
                     ai4 = ai4 + 2
                     if (creadi2(ifdes, ni2, brev, 'gdm3')
     +                          .ne. 0) goto 120
                     if (v .ge. 3) write (6,"(33x,'    i2     ',i11)")
     +                    ni2
                  elseif (bufi4(j) .eq. 3) then
                     ai4 = ai4 + 4
                     if (creadi4(ifdes, ni4, brev, 'gdm3')
     +                          .ne. 0) goto 120
                     if (v .ge. 3) write (6,"(33x,'    i4     ',i11)")
     +                    ni4
                  elseif (bufi4(j) .eq. 4) then
                     ai4 = ai4 + 2
                     if (creadi2(ifdes, ni2, brev, 'gdm3')
     +                          .ne. 0) goto 120
                     if (v .ge. 3) write (6,"(33x,'    i2     ',i11)")
     +                    ni2
                  elseif (bufi4(j) .eq. 5) then
                     ai4 = ai4 + 4
                     if (creadi4(ifdes, ni4, brev, 'gdm3')
     +                          .ne. 0) goto 120
                     if (v .ge. 3) write (6,"(33x,'    i4    ?',i11)")
     +                    ni4
                  elseif (bufi4(j) .eq. 6) then
                     ai4 = ai4 + 4
                     if (creadf4(ifdes, nf4, brev, 'gdm3')
     +                          .ne. 0) goto 120
                     if (v .ge. 3) write (6,"(33x,'    f4     ',e11.3)")
     +                    nf4
                  elseif (bufi4(j) .eq. 7) then
                     ai4 = ai4 + 8
                     if (creadf8(ifdes, nf8, brev, 'gdm3')
     +                          .ne. 0) goto 120
                     if (v .ge. 3) write (6,"(33x,'    f8     ',d11.3)")
     +                    nf8
                  elseif (bufi4(j) .eq. 8) then
                     ai4 = ai4 + 1
                     if (creadc(ifdes, nc, 1, 'gdm3')
     +                          .ne. 0) goto 120
                     if (v .ge. 3) write (6,"(33x,'    byte   ',i11)")
     +                    ichar(nc)
                  else
                     if (v .ge. 0) write (6,"('Entry ',i6
     +                    ,' unknown type ',i11)")i, bufi4(j)
                     goto 120
                  endif
               enddo
               if (v .ge. 3) write (6,"(30x,'bytes/group ',i6)") ai4

c  seek to end of data (if not 14, this seeks by 0 bytes)
               if (ai4*(ngroup-1) .gt. 0) then
                  if (clseekr(ifdes, ai4*(ngroup-1), 'gdm3')
     +                   .ne. 0) then
                     if (v .ge. 1) write (6,"('clseek returned 0')")
                     goto 120
                  endif
                  if (v .ge. 3) write (6,"(33x,'...plus ',i11
     +                 ,' more of the same')") ngroup-1
               endif
            elseif (bufi4(1) .eq. x'14') then

c  not type f, but is type 14
               if (ntype .eq. 2) then
c                 i2 rev signed
                  ai4 = 2
                  if (creadi2(ifdes, ni2, brev, 'gdm3') .ne. 0)
     +                        goto 120
                  if (v .ge. 3) write (6,"(33x,'    i2     ',i11)") ni2
               elseif (ntype .eq. 3) then
c                 i4 rev signed
                  ai4 = 4
                  if (creadi4(ifdes, ni4, brev, 'gdm3') .ne. 0) goto 120
                  if (v .ge. 3) write (6,"(33x,'    i4     ',i11)") ni4
               elseif (ntype .eq. 4) then
                  if (tname(1:ltname) .eq. 'Data') then
c                    i2 rev unsigned
                     ai4 = 2
                     if (creadi2(ifdes, ni2, brev, 'gdm3') .ne. 0)
     +                                                      goto 120
                     if (v .ge. 3) write (6,"(33x,'    i2     ',i11)")
     +                                   ni2
                  else
c                    string as i2 rev (read all in so ai4 = 0)
                     ai4 = 0
                     do i=1,ngroup
                        if (creadi2(ifdes, ni2, brev, 'gdm3') .ne. 0)
     +                                                      goto 120
                        c(i:i) = char(ni2)
                     enddo
                     if (v .ge. 3) write (6,"(33x,
     +                                       ' i2 string ""',a'""')")
     +                                   c(1:ngroup)
                  endif
               elseif (ntype .eq. 5) then
c                 i4 rev unsigned
                  ai4 = 4
                  if (creadi4(ifdes, ni4, brev, 'gdm3') .ne. 0) goto 120
                  if (v .ge. 3) write (6,"(33x,'    i4    ?',i11)") ni4
               elseif (ntype .eq. 6) then
c                 f4 rev
                  ai4 = 4
                  if (creadf4(ifdes, nf4, brev, 'gdm3') .ne. 0) goto 120
                  if (v .ge. 3) write (6,"(33x,'    f4     ',e11.3)")
     +                 nf4
               elseif (ntype .eq. 7) then
c                 f8 rev
                  ai4 = 8
                  if (creadf8(ifdes, nf8, brev, 'gdm3') .ne. 0) goto 120
                  if (v .ge. 3) write (6,"(33x,'    f8     ',d11.3)")
     +                 nf8
               elseif (ntype .eq. 8) then
c                 byte
                  ai4 = 1
                  if (creadc(ifdes, nc, 1, 'gdm3') .ne. 0) goto 120
                  if (v .ge. 3) write (6,"(33x,'    byte   ',i11)")
     +                 ichar(nc)
               elseif (ntype .eq. 9) then
c                 string (read all in so ai4 = 0)
                  ai4 = 0
                  if (creadc(ifdes, c, ngroup, 'gdm3') .ne. 0)
     +                                                    goto 120
                  if (v .ge. 3) write (6,"(33x,'    string ',a)")
     +                 c(1:ngroup)
               elseif (ntype .eq. x'a') then
c                 byte
                  ai4 = 1
                  if (creadc(ifdes, nc, 1, 'gdm3') .ne. 0) goto 120
                  if (v .ge. 3) write (6,"(33x,'    byte   ',i11)")
     +                 ichar(nc)
               else
                  if (v .ge. 0) write (6,"(33x,'    unknown type ',i11)"
     +                 ) ntype
                  goto 120
               endif

c  look for description (ie title)
               if (tname(1:ltname) .eq. 'Description'
     +             .and. imgget .eq. imgnum+1) then
c                 the description comes before the image no. is incremented
                  if (v .ge. 3) write
     +                          (6,"('Description found...',i10,i10)")
     +                          imgget, imgnum
                  ltitle = ngroup
                  title(1:ltitle) = c(1:ltitle)
               endif

c  look for acquisition date and time strings
               if (imgget .eq. imgnum) then
                  if (tname(1:ltname) .eq. 'Acquisition Date') then
                     adate = c(1:ngroup)
                     ladate = ngroup
                     if (v .ge. 3) write
     +                             (6,"('Acquisition Date ""',a,'""')")
     +                             adate(1:ladate)
                  endif
                  if (tname(1:ltname) .eq. 'Acquisition Time') then
                     atime = c(1:ngroup)
                     latime = ngroup
                     if (v .ge. 3) write
     +                             (6,"('Acquisition Time ""',a,'""')")
     +                             atime(1:latime)
                  endif
               endif


c  seek to end of data
               if (ai4*(ngroup-1) .gt. 0) then
                  if (clseekr(ifdes, ai4*(ngroup-1), 'gdm3')
     +                   .ne. 0) goto 120
                  if (v .ge. 3) write (6,"(33x,'...plus ',i11
     +                 ,' more of the same')")ngroup-1
               endif
            endif


         else
c  unknown tag or we are lost
            if (v .ge. 0) write (6,"('Unknown tag ',z2
     +           ,', or I am lost')") tag
            goto 120
         endif

c  go back and read next field
      goto 10


c  jump here from finding tag = 0
 20   continue

c  set various things that depend on datatype
      if (datatype .eq.  6 .or. datatype .eq. 10 .or.
c     +    datatype .eq. 11 .or. datatype .eq. 14 .or.
c     +    datatype .eq. 23) then
     +    datatype .eq. 11 .or. datatype .eq. 14) then
c  unsigned integer
         ptype = 'i'
         fptype = .true.
         psign = 0
         fpsign = .true.
      elseif (datatype .eq. 9 .or. datatype .eq. 1 .or.
     +        datatype .eq. 7) then
c  signed integer
         ptype = 'i'
         fptype = .true.
         psign = 1
         fpsign = .true.
      elseif (datatype .eq. 2 .or. datatype .eq. 12) then
c  floating point (real)
         ptype = 'f'
         fptype = .true.
         psign = 1
         fpsign = .true.
      elseif (datatype .eq. 3 .or. datatype .eq. 13) then
c  complex
         ptype = 'c'
         fptype = .true.
         psign = 1
         fpsign = .true.
      elseif (datatype .eq. 5) then
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
      elseif (datatype .eq. 8.or. datatype .eq. 23) then
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
         if (v .ge. 2) write (6,"('gdm3: unknown DataType ',i6)")
     +                       datatype
      endif

c  convert acquisition date and time strings
c      write (6,"('xxx ', i6,i6)") ladate, latime
      if (ladate .gt. 0 .and. latime .gt. 0) then
c     date (NB can be in either UK or US order... Yuck!)
         it4 = 0
         it1 = index(adate(1:ladate), '/')
         it4 = 1
         if (it1 .lt. 2) goto 90
         it4 = 2
         read (adate(1:it1-1), "(i10)", err=90, end=90) day
         it2 = it1 + index(adate(it1+1:ladate), '/')
c         write (6,"('xxx ',3i6)") it1, it2, ladate
         it4 = 3
         if (it2 .le. it1 .or. it2 .ge. ladate) goto 90
         it4 = 4
         read (adate(it1+1:it2-1), "(i10)", err=90, end=90) month
         it4 = 5
         read (adate(it2+1:ladate), "(i10)", err=90, end=90) year

         if (month .gt. 12) then
c        american date...
            it1 = month
            month = day
            day = it1
         endif

         if (year .lt. 90) then
            year = year + 2000
         elseif (year .le. 99) then
            year = year + 1900
         endif

         if (v .ge. 3) write (6,"(a,' = ',i3,'/',i3,'/',i5)")
     +                       adate(1:ladate), day, month, year
         goto 95
 90      if (v .ge. 1) write
     +             (6,"('Problem understanding date ',i4,' ""',a,'""'
     +                  /'it1 = ',i4,', it2 = ',i4,', ladate = ',i4
     +                  /'day = ',i4,', month = ',i4,', year = ',i4)")
     +          it4, adate(1:ladate), it1, it2, ladate, day, month, year

c      time
 95      it4 = 0
         call lower(atime(1:latime))
         it1 = index(atime(1:latime), ':')
c         write (6,"('atime = ""',a,'""')") atime(1:latime)
         it4 = 1
         if (it1 .lt. 2) goto 98
         it4 = 2
         read (atime(1:it1-1), "(i10)", err=98, end=98) hour
         it2 = it1 + index(atime(it1+1:latime), ':')
         it4 = 3
         if (it2 .le. it1 .or. it2 .ge. latime-1) goto 98
         it4 = 4
         read (atime(it1+1:it2-1), "(i10)", err=98, end=98) minute
         it3 = it2 + index(atime(it2+1:latime), ' ')
c         write (6,"('xxx ',4i6)") it1, it2, it3, latime
         if (it3 .eq. 0) it3 = it2 + index(atime(it2+1:latime), 'p')
         it4 = 5
         if (it3 .eq. it2) it3 = latime + 1
c         if (it3 .le. it2 .or. it3 .ge. latime-1) goto 98
         if (it3 .le. it2) goto 98
         it4 = 6
         read (atime(it2+1:it3-1), "(i10)", err=98, end=98) sec
         lampm = 0
         if (it3 .lt. latime) then
            ampm = atime(it3+1:latime)
            lampm = latime - it3

            if (ampm(1:lampm) .eq. 'am' .and. hour .eq. 12) then
c           12 am = 0000
               hour = 0
            elseif (ampm(1:lampm) .eq. 'pm' .and. hour .ne. 12) then
c           xx pm = (12+xx)00
               hour = hour + 12
            endif
         endif

         if (v .ge. 3) write
     +                 (6,"(a,' (""',a,'"") = ',i3,':',i3,':',i3)")
     +                 atime(1:latime), ampm(1:lampm), hour, minute, sec

         imtime = uxtime(year, month, day, hour, minute, sec)
         fimtime = .true.

         goto 99
 98      if (v .ge. 1) write
     +             (6,"('Problem understanding time ',i4,' ""',a,'""')")
     +                 it4, atime(1:latime)


 99   endif

c  for 1D images fimhei may not be set
      if (fimwid .and. .not. fimhei) then
         imhei = 1
         fimhei = .true.
      endif

c  verify
      if (v .ge. 3) then
         write (6,"('gdm3: size ',i5, ' by ',i5,', ',i3,
     +        ' bytes/pixel')") imwid, imhei, pbyte
         if (ffou) write (6,"('Fourier transform')")
         if (fptype .and. fpsign) write
     +        (6,"('gdm3: type ',a,', sign ',i2)")
     +        ptype, psign
         write (6,"('gdm3: image data type ',i6)") datatype
         write (6,"('gdm3: image starts at ',i11,', type ',i6)")
     +                   dpos, dtype
      endif

c  check found everything
      if (dimen .eq. 0) then
         write (6,"('gdm3: Didn''t find picture dimensions tag')")
         goto 120
      endif
      if (dpos .lt. 0) then
         write (6,"('gdm3: Didn''t find data tag')")
         goto 120
      endif

c  check image height (should do width as well but this is a bit more
c  complicated)
      if (imhei .gt. maxsiz) then
         if (v .ge. 0) write (6,"('gdm3: image height (',i6,
     +        ') greater than max size (',i6,')'
     +        /5x,'increase maxsiz and recompile')") imhei, maxsiz
         goto 120
      endif

c  seek to start of image
      if (clseek(ifdes, dpos, 'gdm3') .gt. 0) goto 120

c  read image
      if (ptype .eq. 'r' .and. .not. swopb) then
c  rgb image, need to translate into 3 pictures
c  read each row into buf, then put each byte into im in right place
c  normal byte order
         if (imhei .eq. 1) then
            blen = imwid*4
            rv = fileread(ifdes, bufc, blen)
            if (rv .ne. blen) then
               if (v .ge. 0) write (6,"('rv = ',i8,',
     +                         blen = ',i8)") rv, blen
               goto 120
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
                  goto 120
               endif
               do i=0,imwid-1
                  do k=0,2
                     imagco(i,j,k) = bufi1(4*i+1+k)
                  enddo
               enddo
            enddo
         endif
      elseif (ptype .eq. 'r' .and. swopb) then
c  rgb image, need to translate into 3 pictures
c  read each row into buf, then put each byte into im in right place
c  need to reverse bytes...
         if (imhei .eq. 1) then
            blen = imwid*4
            rv = fileread(ifdes, bufc, blen)
            if (rv .ne. blen) then
               if (v .ge. 0) write (6,"('rv = ',i8,',
     +                         blen = ',i8)") rv, blen
               goto 120
            endif
            do i=0,imwid-1
               do k=0,2
                  im1dco(i,k) = bufi1(4*i+(2-k))
               enddo
            enddo
         else
            blen = imwid*4
            do j=0,imhei-1
               rv = fileread(ifdes, bufc, blen)
               if (rv .ne. blen) then
                  if (v .ge. 0) write (6,"('rv = ',i8,',
     +                            dlen = ',i8)") rv, blen
                  goto 120
               endif
               do i=0,imwid-1
                  do k=0,2
                     imagco(i,j,k) = bufi1(4*i+(2-k))
                  enddo
               enddo
            enddo
         endif
      elseif (datatype .eq. 5) then
c  packed complex image, normal byte order. read as half plane complex
         if (imhei .eq. 1) then
            blen = (imwid-1)*pbyte
            if (swopb) then
               rv = fileread(ifdes, bufc, blen)
               if (rv .ne. blen) then
                  if (v .ge. 0) write (6,"('rv = ',i8,',
     +                         dlen = ',i8)") rv, blen
                  goto 120
               endif
c              reverse bytes
               do i=0,blen/4-1
                  do k=0,3
                     im1di1(i*4+k) = bufi1(i*4+(4-1-k))
                  enddo
               enddo
            else
               rv = fileread(ifdes, im1dc, blen)
               if (rv .ne. blen) then
                  if (v .ge. 0) write (6,"('rv = ',i8,',
     +                         dlen = ',i8)") rv, blen
                  goto 120
               endif
            endif
c  move real parts to rightful places & zero imag parts
            im1dr(imwid*2-2) = im1dr(1)
            im1dr(1) = 0
            im1dr(imwid*2-1) = 0
         else
            blen = (imwid-1)*pbyte
            if (swopb) then
               do j=0,imhei-1
                  blen = (imwid-1)*pbyte
                  rv = fileread(ifdes, bufc, blen)
                  if (rv .ne. blen) then
                     if (v .ge. 0) write (6,"('rv = ',i8,
     +                                       ', blen = ',i8)") rv, blen
                     goto 120
                  endif
c                 reverse bytes
                  do i=0,blen/4-1
                     do k=0,3
                        imagi1(i*4+k,j) = bufi1(i*4+(4-1-k))
                     enddo
                  enddo
               enddo
            else
               do j=0,imhei-1
                  rv = fileread(ifdes, imagc(0,j), blen)
                  if (rv .ne. blen) then
                     if (v .ge. 0) write (6,"('rv = ',i8,
     +                                       ', blen = ',i8)") rv, blen
                     goto 120
                  endif
               enddo
            endif
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
c  any other image, just read it in blindly... but have to reverse bytes...
         if (dtype .ge. 2 .and. dtype .le. 7 .and. swopb) then
c  dtypes  2  i2 rev signed
c          3  i4 rev signed
c          4  i2 rev unsigned, string as i2
c          5  i4 rev unsigned
c          6  f4 rev
c          7  f8 rev

c  first determine number of bytes to reverse
            if (ptype .eq. 'c') then
               ai4 = pbyte/2
            else
               ai4 = pbyte
            endif

            if (imhei .eq. 1) then
               blen = imwid*pbyte
               rv = fileread(ifdes, bufc, blen)
               if (rv .ne. blen) then
                  if (v .ge. 0) write (6,"('rv = ',i8,',
     +                         blen = ',i8)") rv, blen
                  goto 120
               endif
c              reverse bytes
               do i=0,blen/ai4-1
                  do k=0,ai4-1
                     im1di1(i*ai4+k) = bufi1(i*ai4+(ai4-1-k))
                  enddo
               enddo
            else
               do j=0,imhei-1
                  blen = imwid*pbyte
                  rv = fileread(ifdes, bufc, blen)
                  if (rv .ne. blen) then
                     if (v .ge. 0) write (6,"('rv = ',i8,
     +                    ', blen = ',i8)") rv, blen
                     goto 120
                  endif
c                 reverse bytes
                  do i=0,blen/ai4-1
                     do k=0,ai4-1
                        imagi1(i*ai4+k,j) = bufi1(i*ai4+(ai4-1-k))
                     enddo
                  enddo
               enddo
            endif
         else
c  other types, just read blindly
            if (imhei .eq. 1) then
               blen = imwid*pbyte
               rv = fileread(ifdes, im1dc, blen)
               if (rv .ne. blen) then
                  if (v .ge. 0) write (6,"('rv = ',i8,',
     +                         blen = ',i8)") rv, blen
                  goto 120
               endif
            else
               do j=0,imhei-1
                  blen = imwid*pbyte
                  rv = fileread(ifdes, imagc(0,j), blen)
                  if (rv .ne. blen) then
                     if (v .ge. 0) write (6,"('rv = ',i8,
     +                    ', blen = ',i8)") rv, blen
                     goto 120
                  endif
               enddo
            endif
         endif
      endif

c  should have read in image now, set fim
      fim = .true.


c  normal end of file
c  close file
 100  rv = fileclose(ifdes)
      if (rv .ne. 0) then
         if (v .ge. 0) write
     +                 (6,"('gdm3: fileclose returned ',i6,' not 0')")
     +                 rv
         goto 120
      endif

      if (v .ge. 3) write (6,"('gdm3: normal end of file')")
      gdm3 = 0
      return

c  error somewhere
c  close file
 120  rv = fileclose(ifdes)
      if (rv .ne. 0) then
         if (v .ge. 0) write
     +                 (6,"('gdm3: fileclose returned ',i6,' not 0')")
     +                 rv
      endif

      if (v .ge. 0) write (6,"('gdm3: error somewhere')")
      gdm3 = 1
      return

      end
c--------------------------------------------------------------------------
      function dm3dump()

c  dump Digital Micrograph version 3 file

c  structure is complicated:

c   header: purpose of most of this unknown
c   ------
c              i4     = 3
c              i4     file length - 16
c              i4     = byte order, 0 = mac, 1 = PC, reversed
c              i4 rev = 1
c              i2     = eh, (14) or 12h (18) or 14h (20)

c   main part of file: - repeated
c   -----------------
c    tag       a1     tag, either 14 or 15 or 0 for end of file
c    ltnamei2  i2     tag name length, subsequently stored as ltname
c    tname     a      tag name, length ltname, may be length 0

c  rest of tag depends on tag type (14 or 15)
c  tag = 14  "directory"
c    t14n1     i4 rev = 0 or 1 (purpose unknown)
c    t14n2     i2     number of tags in this "directory"

c  tag = 15
c              a4     = "%%%%"
c  nnum        i4     number of numbers following, not including data
c  bufi4(nnum) i4     numbers indicating type & number of data
c   bufi4(1)          data type  2  i2 rev signed
c                                3  i4 rev signed
c                                4  i2 rev representing chars
c                                5  i4 rev
c                                6  f4 rev
c                                7  f8 rev
c                                8  i1
c                                9  a1
c                               10  i1
c                     f   group of data
c                         bufi4(2) = 0
c                         bufi4(3) = number in group
c                         bufi4(2*n+4) = 0
c                         bufi4(2*n+5) data type for each value in group
c                     14  multiple data or groups bufi4(nnum) = number = ngroup
c                         bufi4(2) is then treated as bufi4(1) above

c  tag levels
c  tagdir(2,100) keeps track of 'directories' & 'subdirectories' of tags
c  tagdir(1,x)   is the number of 'files' in level x
c  tagdir(2,x)   is the number of the current file/directory in level x

c  local variables
      character tname*100, tag*1, nc*1, space*100
      integer*2 ltnamei2, t14n2, ni2
      integer*4 ai4, flength, ltname, t14n1, nnum, ntype, ni4, i, j
      integer*4 itype, ngroup, nfentries, imgnum
      integer*4 tagdir(2,100), taglev
      logical   brev, progbo, swopb
      real*4    nf4
      real*8    nf8

c  common variables
      include "vars.f"

c  byte order...
c   brev   = .true.  bytes in dm3 image are reversed (little endian, PC order)
c                    set from header of dm3 file
c   progbo = .true.  program is running on linux (little endian)
c   swopb  = .true.  bytes need to be swopped
c   swopb  = .not. (progbo .eqv. brev)


c  find program byte order
      progbo = progbyteorder()

c  initialise tagdir
      do i=1,100
         do j=1,2
            tagdir(j,i) = 0
         enddo
      enddo
      taglev = 1

c  initialise space to spaces
      space = ' '

c  imgnum is the number of the image in the file
      imgnum = 0

c  now get on with it
      if (v .ge. 3) write (6,"('starting dm3dump')")

c  write file name
      write (6,"('Contents of ""',a,'""'/)") path(1:lpath)

c  open file
      fname = path(1:lpath)
      ifdes = fileopenr(fname)
      if (v .ge. 3) write
     +   (6,"('open returned ',z8)")
     +   ifdes
      if (ifdes .le. 0 .and .v .ge. 0) then
         write (6,"('can''t find ""',a,'""')") path(1:lpath)
         goto 120
      endif

c  skip to start if macbinary
      if (dstart .gt. 0) then
         if (clseek(ifdes, dstart, 'dm3dump') .ne. 0) goto 120
      endif

c  header...
c  first i4 is 3
      if (creadi4(ifdes, ai4, .false., 'dm3dump') .ne. 0) goto 120
      if (ai4 .ne. 3) write (6,"(First number is ',i12,' not 3')") ai4
c  second is file length - 16
      if (creadi4(ifdes, flength, .false., 'dm3dump') .ne. 0) goto 120
      write (6,"('File length - 16 ',i12,' bytes')") flength

c  third is byte reverse flag
      if (creadi4(ifdes, ai4, .false., 'dm3dump') .ne. 0) goto 120
      if (ai4 .eq. 0) then
         brev = .false.
         write (6,"('Normal byte order (mac order)')")
      elseif (ai4 .eq. 1) then
         brev = .true.
         write (6,"('Reverse byte order (PC order)')")
      else
         brev = .false.
         write (6,"('byte reverse flag, should be 0 or 1... ',i12)") ai4
      endif

c  set swopb. If set means bytes have to be swopped when reading in
c  data part of dm3 file
      swopb  = .not. (progbo .eqv. brev)

c  fourth is usually 1
      if (creadi4(ifdes, ai4, .true., 'dm3dump') .ne. 0) goto 120
      write (6,"('normally is 1 ',i12)") ai4

c  fifth is number of tags at level 1
      if (creadi2(ifdes, ai2, .false., 'dm3dump') .ne. 0) goto 120
      write (6,"('number of tags at level 1',i12)") ai2
      tagdir(1,taglev) = ai2



c  go through each field until end of file
 10   continue

c         write (6,"('pos 1')")
c  this sets all of tname to ' ', not just first char, needed for
c  a30 later on
         tname = ' '

c  read tag
         if (creadc(ifdes, tag, 1, 'dm3dump') .ne. 0) goto 120
         if (v .ge. 3) write (6,"('dm3dump: tag = ',z2)") tag

c         write (6,"('pos 2')")

c  new tag so
c    if not at end of current directory
c       increment tagdir(2,taglev)
c    if at end if current directory (tagdir(2,taglev) = tagdir(1,taglev))
c       set tagdir(1,taglev)=0, set tagdir(2,taglev)=0
c       decrement taglev
c       go back to 'new tag so'
 15      if (tagdir(2,taglev) .lt. tagdir(1,taglev)) then
            tagdir(2,taglev) = tagdir(2,taglev) + 1
         else
            tagdir(1,taglev) = 0
            tagdir(2,taglev) = 0
            taglev = taglev - 1
            if (taglev .gt. 0) goto 15
         endif

c  write out tag level
         if (v .ge. 3) then
            if (taglev .gt. 0) then
               write (6,"('taglevel ',i4,', current no ',i4,
     +                    ', total no ',i4)")
     +               taglev, tagdir(2,taglev), tagdir(1,taglev)
            else
               write (6,"('taglevel ',i4,', end of file')") taglev
            endif
         endif

c  check tag value...

         if (ichar(tag) .eq. 0) then
c  tag = 0, end of file
            write (6,"('End of file')")
            goto 999

         elseif (ichar(tag) .eq. x'14') then
c  tag = 14
c  read length of tag name
            if (creadi2(ifdes, ltnamei2, .false., 'dm3dump') .ne. 0)
     +            goto 120
            if (v .ge. 3) write (6,"('dm3dump: ltnamei2 = ',i11)")
     +                          ltnamei2
            ltname = ltnamei2
            if (ltname .gt. 100) then
               write (6,"(z2,' Extra long tag name, ',i4,
     +                    ', something''s up')") tag, ltname
               goto 120
            endif
c  read tag name
            if (creadc(ifdes, tname, ltname, 'dm3dump') .ne. 0) goto 120
            if (v .ge. 3) write (6,"('dm3dump: tag name = ""',a,'""')")
     +                          tname(1:ltname)
c  read numbers
            if (creadi4(ifdes, t14n1, .true., 'dm3dump') .ne. 0)
     +                 goto 120
            if (v .ge. 3) write (6,"('dm3dump: t14n1 = ',i11)")
     +                          t14n1
            if (creadi2(ifdes, t14n2, .false., 'dm3dump') .ne. 0)
     +              goto 120
            if (v .ge. 3) write (6,"('dm3dump: t14n2 = ',i11)")
     +                          t14n2

c  new directory
c    increment taglev
c    set tagdir(1,taglev)=no. of files
c    set tagdir(2,taglev)=0
            taglev = taglev + 1
            tagdir(1,taglev) = t14n2
            tagdir(2,taglev) = 0

c  write out new tag level
            if (v .ge. 3) then
               if (taglev .gt. 0) then
                  write (6,"('taglevel ',i4,', current no ',i4,
     +                       ', total no ',i4)")
     +                  taglev, tagdir(2,taglev), tagdir(1,taglev)
               else
                  write (6,"('taglevel ',i4,', end of file')") taglev
               endif
            endif

c  write out
            write (6,"(a,z2,' ',a30,' ',i10,i11)")
     +            space(1:taglev-1), tag, tname, t14n1, t14n2

c  write out image number etc
            if (tname(1:ltname) .eq. 'ImageList') then
               if (v .ge. 0) write
     +             (6,"('File contains ',i6,' images')") t14n2
            endif
            if (tname(1:ltname) .eq. 'ImageData') then
               imgnum = imgnum + 1
               if (v .ge. 0) write
     +             (6,"('Image number ',i6)") imgnum
            endif

         elseif (ichar(tag) .eq. x'15') then
c         write (6,"('pos 3')")
c  tag = 15
c  read length of tag name
            if (creadi2(ifdes, ltnamei2, .false., 'dm3dump') .ne. 0)
     +               goto 120
            ltname = ltnamei2
            if (ltname .gt. 100) then
               write (6,"(z2,' Extra long tag name, ',i4,
     +                    ', something''s up')") tag, ltname
               goto 120
            endif
c         write (6,"('pos 4')")
c  read tag name
            if (creadc(ifdes, tname, ltname, 'dm3dump') .ne. 0) goto 120
c  read %%%%
            if (creadc(ifdes, ci4, 4, 'dm3dump') .ne. 0) goto 120
c         write (6,"('pos 5')")
            if (ci4 .ne. '%%%%') then
               write (6,"(z2,' ',a30,' next part not %%%%')")
     +               tag, tname
               goto 120
            endif
c  read number of numbers
            if (creadi4(ifdes, nnum, .false., 'dm3dump') .ne. 0)
     +               goto 120
c         write (6,"('pos 6')")

c  read all numbers into bufi4
            do i=1,nnum
               if (creadi4(ifdes, bufi4(i), .false., 'dm3dump') .ne. 0)
     +                     goto 120
            enddo

c  verify entries if v>=3
            if (v .ge. 3 .and. nnum .gt. 1) then
               write (6,"('    First 11 entries')")
               do i=1,min(nnum,11)
                  write (6,"(4x,i4,i11,3x,z8)") i, bufi4(i), bufi4(i)
               enddo
            endif

            nfentries = 0

c         write (6,"('pos 7')")
c  check to see if bufi4(1) is 14h
            if (bufi4(1) .eq. x'14') then
c              itype is the position of the data type
               itype = 2
c              ngroup is the number in the group
               ngroup = bufi4(nnum)
c              ntype is the data type (may be f)
               ntype = bufi4(itype)
c         write (6,"('pos 8')")
c              write out
               if (ntype .eq. x'f') then
                  nfentries = bufi4(4)
                  write (6,"(a,z2,' ',a30,' nnum',i6,', type 14, ',
     +                      'ngroup',i10,', type ',z2,', entries ',i6)")
     +                  space(1:taglev), tag, tname, nnum, ngroup,
     +                  ntype, nfentries
c  check number of entries
                  if (nfentries*2+5 .ne. nnum) then
                     write (6,"('Confused, expect 4th number (',i11,
     +          ') *2 + 5'/'   to be the number of numbers (',i11,')')")
     +                     bufi4(4), nnum
                     write (6,"('  First 11 entries')")
                     do i=1,min(nnum,11)
                        write (6,"(4x,i4,i11,3x,z8)")
     +                       i,bufi4(i), bufi4(i)
                     enddo
                     goto 120
                  endif
               else
                  write (6,"(a,z2,' ',a30,' nnum',i6,', type 14, ',
     +                        'ngroup',i10,', type ',z2)")
     +                  space(1:taglev), tag, tname, nnum, ngroup, ntype
               endif
c         write (6,"('pos 9')")
            else
               itype = 1
               ngroup = 1
               ntype = bufi4(itype)
               if (ntype .eq. x'f') then
                  nfentries = bufi4(3)
                  write (6,"(a,z2,' ',a30,' nnum',i6,', type ',z2,
     +                       ', entries ',i6)")
     +                  space(1:taglev), tag, tname, nnum, ntype,
     +                  nfentries
c  check number of entries
                  if (nfentries*2+3 .ne. nnum) then
                     write (6,"('Confused, expect 3rd number (',i11,
     +          ') *2 + 3'/'   to be the number of numbers (',i11,')')")
     +                     bufi4(3), nnum
                     write (6,"('  First 11 entries')")
                     do i=1,min(nnum,11)
                        write (6,"(4x,i4,i11,3x,z8)")
     +                        i,bufi4(i), bufi4(i)
                     enddo
                     goto 120
                  endif
               else

c  not 14, not f, ie one number only. get data as well here.
                  if (ntype .eq. 2) then
c                    i2 rev
                     if (creadi2(ifdes, ni2, brev, 'dm3dump') .ne. 0)
     +                                                         goto 120
                     write (6,"(a,z2,' ',a30,'    i2     ',i11)")
     +                     space(1:taglev), tag, tname, ni2
                  elseif (ntype .eq. 3) then
c                    i4 rev
                     if (creadi4(ifdes, ni4, brev, 'dm3dump') .ne. 0)
     +                                                         goto 120
                     write (6,"(a,z2,' ',a30,'    i4     ',i11)")
     +                     space(1:taglev), tag, tname, ni4
                  elseif (ntype .eq. 4) then
c                    i2 rev unsigned (but I'm writing it out signed here...)
                     if (creadi2(ifdes, ni2, brev, 'dm3dump') .ne. 0)
     +                                                         goto 120
                     write (6,"(a,z2,' ',a30,'    i2 uns ',i11)")
     +                     space(1:taglev), tag, tname, ni2
                  elseif (ntype .eq. 5) then
c                    i4 rev unsigned (but I'm writing it out signed here...)
                     if (creadi4(ifdes, ni4, brev, 'dm3dump') .ne. 0)
     +                                                         goto 120
                     write (6,"(a,z2,' ',a30,'    i4 uns ',i11)")
     +                     space(1:taglev), tag, tname, ni4
                  elseif (ntype .eq. 6) then
c                    f4 rev
                     if (creadf4(ifdes, nf4, brev, 'dm3dump') .ne. 0)
     +                                                         goto 120
                     write (6,"(a,z2,' ',a30,'    f4     ',e11.3)")
     +                     space(1:taglev), tag, tname, nf4
                  elseif (ntype .eq. 7) then
c                    f8 rev
                     if (creadf8(ifdes, nf8, brev, 'dm3dump') .ne. 0)
     +                                                         goto 120
                     write (6,"(a,z2,' ',a30,'    f8     ',d11.3)")
     +                     space(1:taglev), tag, tname, nf8
                  elseif (ntype .eq. 8) then
c                    byte
                     if (creadc(ifdes, nc, 1, 'dm3dump') .ne. 0)
     +                                                         goto 120
                     write (6,"(a,z2,' ',a30,'    byte   ',i11)")
     +                     space(1:taglev), tag, tname, ichar(nc)
                  else
                     write (6,"(a,z2,' ',a30,'    unknown type ',i11)")
     +                     space(1:taglev), tag, tname, ntype
                  endif
               endif
            endif

c  have written out single line, now need to do type f
            if (ntype .eq. x'f') then


c         write (6,"('pos 10')")
c  for each entry read according to type to find total bytes
               if (ngroup .gt. 1) write (6,"(30x,'first group')")
               ai4 = 0
               do i=1,nfentries
                  j = (i-1)*2 + 4 + itype
                  if (bufi4(j) .eq. 2) then
                     ai4 = ai4 + 2
                     if (creadi2(ifdes, ni2, brev, 'dm3dump')
     +                          .ne. 0) goto 120
                     write (6,"(33x,'    i2     ',i11)") ni2
                  elseif (bufi4(j) .eq. 3) then
                     ai4 = ai4 + 4
                     if (creadi4(ifdes, ni4, brev, 'dm3dump')
     +                          .ne. 0) goto 120
                     write (6,"(33x,'    i4     ',i11)") ni4
                  elseif (bufi4(j) .eq. 5) then
                     ai4 = ai4 + 4
                     if (creadi4(ifdes, ni4, brev, 'dm3dump')
     +                          .ne. 0) goto 120
                     write (6,"(33x,'    i4    ?',i11)") ni4
                  elseif (bufi4(j) .eq. 6) then
                     ai4 = ai4 + 4
                     if (creadf4(ifdes, nf4, brev, 'dm3dump')
     +                          .ne. 0) goto 120
                     write (6,"(33x,'    f4     ',e11.3)") nf4
                  elseif (bufi4(j) .eq. 7) then
                     ai4 = ai4 + 8
                     if (creadf8(ifdes, nf8, brev, 'dm3dump')
     +                          .ne. 0) goto 120
                     write (6,"(33x,'    f4     ',d11.3)") nf8
                  elseif (bufi4(j) .eq. 8) then
                     ai4 = ai4 + 1
                     if (creadc(ifdes, nc, 1, 'dm3dump')
     +                          .ne. 0) goto 120
                     write (6,"(33x,'    byte   ',i11)") ichar(nc)
                  else
                     write (6,"('Entry ',i6,' unknown type ',i11)")
     +                    i, bufi4(j)
                     goto 120
                  endif
               enddo
               if (v .ge. 3) write (6,"(30x,'bytes/group ',i6)") ai4

c         write (6,"('pos 11')")
c  seek to end of data (if not 14, this seeks by 0 bytes)
               if (ai4*(ngroup-1) .gt. 0) then
                  if (clseekr(ifdes, ai4*(ngroup-1), 'dm3dump')
     +                   .ne. 0) goto 120
                  write (6,"(33x,'...plus ',i11,' more of the same')")
     +                        ngroup-1
               endif
            elseif (bufi4(1) .eq. x'14') then

c         write (6,"('pos 12')")
c  not type f, but is type 14
               if (ntype .eq. 2) then
c                 i2 rev
                  ai4 = 2
                  if (creadi2(ifdes, ni2, brev, 'dm3dump') .ne. 0)
     +                    goto 120
                  write (6,"(33x,'    i2     ',i11)")
     +                  ni2
               elseif (ntype .eq. 3) then
c                 i4 rev
                  ai4 = 4
                  if (creadi4(ifdes, ni4, brev, 'dm3dump') .ne. 0)
     +                   goto 120
                  write (6,"(33x,'    i4     ',i11)")
     +                  ni4
               elseif (ntype .eq. 4) then
                  if (tname(1:ltname) .eq. 'Data') then
c                    i2 rev unsigned
                     ai4 = 2
                     if (creadi2(ifdes, ni2, brev, 'dm3dump') .ne. 0)
     +                                                      goto 120
                     write (6,"(33x,'    i2 uns ',i11)")
     +                     ni2
                  else
c                    string as i2 rev (read all in so ai4 = 0)
                     ai4 = 0
                     do i=1,ngroup
                        if (creadi2(ifdes, ni2, brev, 'dm3dump') .ne. 0)
     +                                                      goto 120
                        c(i:i) = char(ni2)
                     enddo
                     write (6,"(33x,' i2 string ""',a,'""')")
     +                     c(1:ngroup)
                  endif
               elseif (ntype .eq. 5) then
c                 i4 rev unsigned
                  ai4 = 4
                  if (creadi4(ifdes, ni4, brev, 'dm3dump') .ne. 0)
     +                    goto 120
                  write (6,"(33x,'    i4 uns ',i11)")
     +                  ni4
               elseif (ntype .eq. 6) then
c                 f4 rev
                  ai4 = 4
                  if (creadf4(ifdes, nf4, brev, 'dm3dump') .ne. 0)
     +                    goto 120
                  write (6,"(33x,'    f4     ',e11.3)")
     +                  nf4
               elseif (ntype .eq. 7) then
c                 f8 rev
                  ai4 = 8
                  if (creadf8(ifdes, nf8, brev, 'dm3dump') .ne. 0)
     +                       goto 120
                  write (6,"(33x,'    f8     ',d11.3)")
     +                  nf8
               elseif (ntype .eq. 8) then
c                 byte
                  ai4 = 1
                  if (creadc(ifdes, nc, 1, 'dm3dump') .ne. 0) goto 120
                  write (6,"(33x,'    byte   ',i11)")
     +                  ichar(nc)
               elseif (ntype .eq. 9) then
c                 string (read all in so ai4 = 0)
                  ai4 = 0
                  if (creadc(ifdes, c, ngroup, 'dm3dump') .ne. 0)
     +                                                    goto 120
                  write (6,"(33x,'    string ',a)")
     +                  c(1:ngroup)
               elseif (ntype .eq. x'a') then
c                 byte
                  ai4 = 1
                  if (creadc(ifdes, nc, 1, 'dm3dump') .ne. 0) goto 120
                  write (6,"(33x,'    byte   ',i11)")
     +                  ichar(nc)
               else
                  write (6,"(33x,'    unknown type ',i11)")
     +                  ntype
                  goto 120
               endif

c  seek to end of data
               if (ai4*(ngroup-1) .gt. 0) then
                  if (clseekr(ifdes, ai4*(ngroup-1), 'dm3dump')
     +                   .ne. 0) goto 120
                  write (6,"(33x,'...plus ',i11,' more of the same')")
     +                        ngroup-1
               endif
            endif


         else
c  unknown tag or we are lost
            write (6,"('Unknown tag ',z2,', or I am lost')") tag
            goto 120
         endif



c  go back and read next field
      goto 10


 999  continue


c  normal end of file
c  close file
 100  rv = fileclose(ifdes)
      if (rv .ne. 0) then
         if (v .ge. 0) write
     +               (6,"('gdm3dump: fileclose returned ',i6,' not 0')")
     +                 rv
         goto 120
      endif

      if (v .ge. 3) write (6,"('dm3dump: normal end of file')")
      dm3dump = 0
      return

c  unexpected end of file
c  close file
 110  rv = fileclose(ifdes)
      if (rv .ne. 0) then
         if (v .ge. 0) write
     +                 (6,"('gdm3: fileclose returned ',i6,' not 0')")
     +                 rv
      endif

      if (v .ge. 3) write (6,"('dm3dump: unexpected end of file')")
      dm3dump = 1
      return

c  error somewhere
c  close file
 120  rv = fileclose(ifdes)
      if (rv .ne. 0) then
         if (v .ge. 0) write
     +                 (6,"('gdm3: fileclose returned ',i6,' not 0')")
     +                 rv
      endif

      if (v .ge. 0) write (6,'(''dm3dump: error somewhere'')')
      dm3dump = 1
      return

      end
