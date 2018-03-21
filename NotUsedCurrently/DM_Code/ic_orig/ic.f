c  converts between image formats
c  After ic74.f split into separate files. Now this is only the main program

c  On mac:
c  compile with f77 -c -O -fno-underscoring -fbounds-check -Wall -Wimplicit

c  On SG:
c  compile with f77 -C -trapuv -w0 -noisam ic.f -o ic

c  subroutines to read and write images etc

c     getelp   get   EL/P binary file
c     welp     write  "

c     gs6rwb   get   semper 6 read/write binary file
c     ws6rwb   write  "

c     ws6dsk   write semper 6 disc (one file for all images)
c     s6dskdump dump semper 6 disc to screen

c     gdm      get   Digital Micrograph file
c     wdm      write  "    (not written yet)
c     dmdump   dump  Digital Micrograph files to screen

c     gdm3     get   Digital Micrograph 3 file
c     dm3dump  dump  Digital Micrograph 3 files to screen


c  logic for semper discs
c     ic *           creates semper disc named from first file to be converted.
c                    if semper disc exists don't enlarge it
c     ic -oxx.dsk *  creates xx.dsk of size just big enough for files
c                    if xx.dsk exists, don't enlarge it
c     ic -s *        if semper disc exists enlarge it to just fit files
c                    Aug 2004 changed so can always enlarge
c     ic -s1234 *    create semper disc size 1234 Kbyte
c                    if it exists, enlarge (or shrink?) to 1234 Kybte
c     ic -dsk *      force files to be converted to semper disc. otherwise
c                    files are converted to their default formats
c     ic -n123 *     start from picture number 123

c  for semper discs:
c     outtyp    = s6dsk, set by -dsk from command line or from defout
c     opath     general output file name, chages for each file read
c     dskopath  output semper disc, set on command line with -o or
c               by default from first file
c     dsksiz    semper disc size requested from command line in Kbyte.
c               = 0 means use smallest size needed
c     fdskenl   true if semper disc can be enlarged (-s set)
c               was set false at start, now set true so can always enlarge
c     dsksb     actual disc size created by cs6dsk in bytes (local)
c     ndskpic   next dsk picture number, set from -n or = 1


c  uses system calls to open, read and write binary files rather than
c  normal fortran as it is difficult to do something as simple as read a
c  given number of bytes in fortran without getting stuck with records etc.
c  The following define the access mode (oflag) for open. Mode is the usual
c  octal number used in chmod, and is I think not relevant for reading.
c  read(fdes, variable, number of bytes) can't seem to cope with
c  the number of bytes being 2 variables multiplied eg ibytes*ilength

c  to convert apple time (secs since 0000h 1 Jan 1904) to
c  unix time (secs since 0000h 1 Jan 1970):
c  days between these 2 dates = 66*365 + 17 = 24107
c  secs   "       "   "   "   = 24107*60*60*24 = 2082844800 = x'7c25b080'
c  thus expect  unix = apple - 2082844800
c  but apple time is -ve for i4 integers, so need to add 2^32
c               unix = apple - 2082844800 + 2^32
c                    = apple + 2212122496 (no good as > 2^31)
c  use          unix = apple - deltat

c  To add a new variable:
c    declare it in vars.f
c    declare a flag for it in vars.f
c    include variable & flag in common statements in vars.f
c    set flag to false initially a few lines below
c    verify it in v=2 verification
c    set it in get subroutines
c    write it in write subroutines

c  variables local to the main program
      integer*4 errg, errw, err, ai4, bi4, imgget, i, ipath, imgnum
      integer*8 fsize

      include "vars.f"

c  amount of verification
c  0 = errors only, 1 = just files done, 2 = lots, 3 = debug
      v = 1

c  numbers that need setting once only
      deltat = 2082844800

c  initial number of path (file) names
      npathns = 0

c  output file type
      outtyp = ' '
      louttyp = 0

c  set mac binary false initially
      clmacb = 0

c  semper disc size (if not set by command line)
      dsksiz = 0

c  true if semper disc can be enlarged (-s option set)
c  was set false, now (Aug 2004) set true
      fdskenl = .true.

c  semper disc filename
      dskopath = ' '
      ldskopath = 0

c  semper disc next picture number
      ndskpic = 1

c  true if semper disc directory read
      fsemdir = .false.

c  number of dm3 image to get (0 means get all)
      imgnum = 0

c.......................................................................
c  now get on with it

c  get number of arguments
      nclargs = iargc()
      if (v .ge. 3) write (6,"(i6,' arguments')") nclargs
      if (nclargs .gt. maxarg) then
         if (v .ge. 0) write (6,"('Number of options/files (',i6,') is',
     +                            'greater than the maximum (',i6,')'/
     +                       'Recompile with larger value of maxarg')")
     +                                  nclargs, maxarg
         nclargs = maxarg
      endif

c  normally won't print as v not set yet
      if (v .ge. 3) write
     +  (6,"('starting to read & check command line arguments')")

c  if arguments supplied, get each one and deal with it
      if (nclargs .gt. 0) then
         do i=1,nclargs

c  get argument
            call getarg(i, clarg)
c  find length using intrinsic function lnblnk
            lclarg = lnblnk(clarg)

            if (v .ge. 3) write (6,"(i6, ' ""',a,'""')")
     +                    lclarg, clarg(1:lclarg)
            if (lclarg .lt. 1) then
               if (v .ge. 0) write (6,"('panic, argument length=',i6)")
     +                             lclarg
               call exit
            endif

c  now go through finding out what the argument means
            if (clarg(1:1) .eq. '-') then
c  its an option
               if (clarg(2:2) .eq. 'v') then
                  if (lclarg .gt. 2) then
                     read (clarg, "(2x,i10)", err=30, end=30) v
                  else
                     v = 2
                  endif
 30               if (v .ge. 3) write (6,"('verification v=',i6)") v
               elseif (clarg(2:2) .eq. 'q') then
                  v = 0
               elseif (clarg(2:3) .eq. 's6' .and. lclarg .eq. 3) then
                  outtyp = 's6rwb'
                  louttyp = 5
               elseif (clarg(2:4) .eq. 'unf') then
                  outtyp = 's6rwb'
                  louttyp = 5
               elseif (clarg(2:4) .eq. 'elp') then
                  outtyp = 'el/p'
                  louttyp = 4
               elseif (clarg(2:2) .eq. 'd' .and. lclarg .eq. 2) then
                  outtyp = 'd'
                  louttyp = 1
               elseif (clarg(2:3) .eq. 'dm' .and. lclarg .eq. 3) then
                  outtyp = 'dm2'
                  louttyp = 3
               elseif (clarg(2:4) .eq. 'dm2') then
                  outtyp = 'dm2'
                  louttyp = 3
               elseif (clarg(2:4) .eq. 'dsk' .and. lclarg .eq. 4) then
                  outtyp = 's6dsk'
                  louttyp = 5
               elseif (clarg(2:2) .eq. 's') then
                  if (v .ge. 3) write (6,"('clarg = ""',a,'""')")
     +                          clarg(1:lclarg)
                  fdskenl = .true.
                  if (lclarg .gt. 2) then
                     read (clarg, "(2x,i10)", err=40, end=40) dsksiz
                  else
                     dsksiz = 0
                  endif
 40               if (v .ge. 3) write
     +            (6,"('semper disc size set from command line =',i6)")
     +            dsksiz
               elseif (clarg(2:2) .eq. 'n') then
                  if (v .ge. 3) write (6,"('clarg = ""',a,'""')")
     +                          clarg(1:lclarg)
                  if (lclarg .gt. 2) then
                     read (clarg, "(2x,i10)", err=50, end=50) ndskpic
                  else
                     ndskpic = 1
                  endif
 50               if (v .ge. 3) write
     +            (6,"('next semper pic set from command line to',i6)")
     +            ndskpic
               elseif (clarg(2:2) .eq. 'o') then
                  if (v .ge. 3) write (6,"('clarg = ""',a,'""')")
     +                          clarg(1:lclarg)
                  if (lclarg .gt. 2) then
                     dskopath = clarg(3:lclarg)
                     ldskopath = lclarg - 2
                  endif
                  if (v .ge. 3) write
     +            (6,"('semper disc set from command line to ""'
     +                ,a,'""')")
     +            dskopath(1:ldskopath)
               elseif (clarg(2:4) .eq. 'img') then
                  if (v .ge. 3) write (6,"('clarg = ""',a,'""')")
     +                          clarg(1:lclarg)
                  if (lclarg .gt. 4) then
                     read (clarg, "(4x,i10)", err=60, end=60) imgnum
                  else
                     imgnum = 0
                  endif
 60               if (v .ge. 3) write
     +            (6,"('get from dm3 image number ',i6)")
     +            imgnum
               elseif (clarg(2:2) .eq. 'h') then
                  call typhelp
                  call exit
               elseif (clarg(2:2) .eq. 'm') then
                  clmacb = 2
               elseif (clarg(2:4) .eq. 'nom') then
                  clmacb = 1
               else
                  if (v .ge. 0) write (6,"('unknown option ',a)")
     +                                clarg(1:lclarg)
               endif
            else
c  its a filename
               npathns = npathns + 1
               pathns(npathns) = clarg
               lpathns(npathns) = lclarg
            endif
         enddo
      else
c  no arguments given so give up and type help
         if (v .ge. 0) write (6,"('No files to convert')")
         if (v .ge. 1) call typhelp
         call exit
      endif

c  arguments list gone through. verify nclargs since it won't show
c  normally because v will be 1 at the start
      if (v .ge. 3) write
     +    (6,"('finished reading & checking command line arguments')")
      if (v .ge. 3) write (6,"(i6,' arguments')") nclargs

c  check we have at least one file to convert
c  but carry on anyway in case trying to enlarge semper disc
c  see if -s set to enlarge a semper disc
      if (npathns .le. 0) then
         if (fdskenl .and. ldskopath .ge. 1) then
            if (v .ge. 3) write
     +     (6,"('No files to convert but want to enlarge semper disc')")
            rv = cs6dsk(dskopath, ldskopath, dsksiz)
            if (rv .gt. 0) then
               if (v .gt. 0) write (6,"('Couldn''t enlarge ""',a,'""')")
     +                                dskopath(1:ldskopath)
            else
               if (v .gt. 0) write
     +                     (6,"('""',a,'"" enlarged to size ',i8,'KB')")
     +                                dskopath(1:ldskopath), dsksiz
            endif
            call exit
         else
c  no files to convert but some arguments must have been given, give up anyway
            if (v .ge. 0) write (6,"('No files to convert')")
            call exit
         endif
      endif

c  verify list of filenames
      if (v .ge. 3) then
         write (6,"(i6,' filenames')") npathns
         do i=1,npathns
            write (6,"(i6,' ""',a,'""')") lpathns(i),
     +                                    pathns(i)(1:lpathns(i))
         enddo
      endif


c  ------------------

c  have now deciphered command line arguments and made sure there
c  is at least 1 file to convert in the list, pathns().
c  or there is a semper disc to enlarge
c  next go through each name in the list and think about it
      if (v .ge. 3) write
     +    (6,"('reading through list of files & converting')")

      do ipath=1,npathns
         path = pathns(ipath)
         lpath = lpathns(ipath)

c  xxxxx for testing...
c         write (6,"('ipath = ',i6)") ipath
c         if (ipath .ge. 125) v = 3

c  verify pathname etc
         if (v .ge. 3) then
            write (6,"('============================================')")
            write (6,"(/'Converting file number ',i6,' ""',a,'""')")
     +                   ipath, path(1:lpath)
         endif

c  first need to split full pathname into directory (dir), filename (file)
c  and filename into root filename (froot) and extension (fext)
         call splitp(path, lpath)

c  set imgget from imgnum from command line, unless imgnum = 0 in which case
c  set imgget to 1, so get first image from dm3 file
         if (imgnum .gt. 0) then
            imgget = imgnum
         else
            imgget = 1
         endif

c  jump here if need to read second image from same file
 10      continue

c  things that need zeroing before each picture is read in
         fsize = -1
         ltitle = 0
         fim    = .false.
         fimwid = .false.
         fimhei = .false.
         fevzero = .false.
         fevpch = .false.
         fchoff = .false.
         fpbyte = .false.
         fptype = .false.
         fpsign = .false.
         forigx = .false.
         forigy = .false.
         forigz = .false.
         fnmorx = .false.
         fnmory = .false.
         fnmorz = .false.
         fnmppx = .false.
         fnmppy = .false.
         fnmppz = .false.
         fimtime = .false.
         ffou = .false.
         fwmacb = .false.
         frmacb = .false.
         fcrtime = .false.
         fmotime = .false.
         fmfname = .false.
         fmftype = .false.
         fmfcrea = .false.
         fanim = .false.
         fwim = .true.
         fpreview = .false.
         fimgmin = .false.
         fimgmax = .false.
         fdismin = .false.
         fdismax = .false.
         fsvars = .false.

c  call function to read in picture
c  here need to read in first bit of file and decide what sort it is
c  first open the file
         fname = path(1:lpath)
         ifdes = fileopenr(fname)
         if (v .ge. 3) write
     +      (6,"('fileopenr returned ',z8)")
     +      ifdes
         if (ifdes .le. 0 .and .v .ge. 0) then
            write (6,"('can''t find ""',a,'""')") path(1:lpath)
            goto 100
         endif

c  read in first bit
         rv = fileread(ifdes, im1dc, 256)
         if (rv .le. 0) then
            if (v .ge. 0) write (6,"('couldn''t read anything')")
            goto 100
         endif

c  find file size (remember fsize is i8)
         fsize = filesize(ifdes)
         if (fsize .le. 0) then
            if (v .ge. 0) write (6,"('filesize <= 0: ',i8)") fsize
         endif
         if (v .ge. 3) write (6,"('File size: ',i12,' bytes')") fsize

c  close file
         err = fileclose(ifdes)
         if (err .ne. 0) then
            if (v .ge. 0) write (6,"('problem closing file')")
         endif

c  Write first few bytes
         if (v .ge. 3) write (6,"('First 16 bytes  ',16z3)")
     +                 (im1dc(i),i=0,15)

c  Write first few bytes skipping 128 (for MacBinary)
         if (v .ge. 3) write (6,"('After 128 bytes ',16z3)")
     +                 (im1dc(i+128),i=0,15)

c  Now look at start of file to see what sort it is and call right function
c  to read it in

c  First find out whether it is MacBinary and set dstart to start of
c  data and dlength to length of data
c  This doesn't work very well as it is quite easy for a random file to
c  have bytes that satisfy these conditions and not be macbinary. Have added
c  the condition that the data fork length be <23000000 (ie will just fit an
c  imaging plate file). Also now check the resource fork length - must
c  be less than 10000 (an arbitrary number to allow for small resource forks
c  eg as in Digital micrograph files)

c  Set ai4 and bi4 to data and resorce fork lengths (using ci4/i4)
c  Macbinary comes from a mac so it must be big endian
c         ci4 = im1dcs(84:87)
c         ai4 = i4
         ai4 = strtoi4(im1dcs(84:87), bigend)
c         ci4 = im1dcs(88:91)
c         bi4 = i4
         bi4 = strtoi4(im1dcs(88:91), bigend)

         if (v .ge. 3) then
            write (6,"('MacBinary test:')")
            write (6,"('  byte ',i4,' = ',i4,'  ',z2,'h')")
     +                               0, ichar(im1dc(0)), im1di1(0)
            write (6,"('  byte ',i4,' = ',i4,'  ',z2,'h')")
     +                               1, ichar(im1dc(1)), im1di1(1)
            write (6,"('  byte ',i4,' = ',i4,'  ',z2,'h')")
     +                               74, ichar(im1dc(74)), im1di1(74)
            write (6,"('  byte ',i4,' = ',i4,'  ',z2,'h')")
     +                               82, ichar(im1dc(82)), im1di1(82)
            write (6,"('  byte ',i4,' = ',i4,'  ',z2,'h')")
     +                               83, ichar(im1dc(83)), im1di1(83)
            write (6,"('  byte ',i4,' = ',i4,'  ',z2,'h')")
     +                               84, ichar(im1dc(84)), im1di1(84)
            write (6,"('  byte ',i4,' = ',i4,'  ',z2,'h')")
     +                               85, ichar(im1dc(85)), im1di1(85)
            write (6,"('  byte ',i4,' = ',i4,'  ',z2,'h')")
     +                               86, ichar(im1dc(86)), im1di1(86)
            write (6,"('  data fork length     = ',i12,'  ',z8,'h')")
     +                               ai4, ai4
            write (6,"('  byte ',i4,' = ',i4,'  ',z2,'h')")
     +                               87, ichar(im1dc(87)), im1di1(87)
            write (6,"('  resource fork length = ',i12,'  ',z8,'h')")
     +                               bi4, bi4
         endif

         if (ichar(im1dc(0)) .eq. 0
     +       .and. ichar(im1dc(1)) .ge. 1 .and. ichar(im1dc(1)) .le. 63
     +       .and. ichar(im1dc(74)) .eq. 0
     +       .and. ichar(im1dc(82)) .eq. 0
     +       .and. ichar(im1dc(83)) .le. 127
     +       .and. ai4 .gt. 0 .and. ai4 .lt. fsize
     +       .and. ichar(im1dc(87)) .le. 127
     +       .and. bi4 .ge. 0 .and. bi4 .lt. 50000
     +       .and. bi4 .lt. fsize) then
            frmacb = .true.
            if (v .ge. 3) write (6,"('MacBinary file')")
            lmfname = ichar(im1dc(1))
            mfname = im1dcs(3:lmfname+2)
            fmfname = .true.
            lmftype = 4
            mftype = im1dcs(66:69)
            fmftype = .true.
            lmfcrea = 4
            mfcrea = im1dcs(70:73)
            fmfcrea = .true.
c            ci4 = im1dcs(92:95)
c            crtime = i4 - deltat
            crtime = strtoi4(im1dcs(92:95), bigend) - deltat
            fcrtime = .true.
c            ci4 = im1dcs(96:99)
c            motime = i4 - deltat
            motime = strtoi4(im1dcs(96:99), bigend) - deltat
            fmotime = .true.
            dstart = 128
            dlength = ai4
         else
            frmacb = .false.
            if (v .ge. 3) write (6,"('Not a MacBinary file')")
            dstart = 0
            dlength = 0
         endif

         if (v .ge. 3) write (6,"('dstart = ',i11)") dstart

c         if (im1di2(dstart/2) .eq. x'0002') then
         if (im1dcs(dstart+1:dstart+2) .eq. achar(0)//achar(2)) then
c  el/p eels file
            if (v .ge. 3) write (6,"('el/p file')")
            intyp = 'el/p'
            lintyp = 4
            if (outtyp .eq. 'd') then
               if (v .ge. 0) write (6,"('can''t dump EL/P files')")
               errg = 1
            else
               errg = getelp()
            endif
            if (errg .ne. 0 .and. v .ge. 3) write
     +                   (6,"('something went wrong reading file')")
            defout = 's6dsk'
            ldefout = 5
         elseif (im1dcs(dstart+1:dstart+1) .eq. achar(x'd5')) then
c  Link AN10000 file
            if (v .ge. 3) write (6,"('Link An10000 file')")
            intyp = 'An10'
            lintyp = 4
            if (outtyp .eq. 'd') then
               errg = getan10000(1)
            else
               errg = getan10000(0)
            endif
            defout = 's6dsk'
            ldefout = 5
c         elseif (im1di4(dstart/4) .eq. x'0000000c') then
         elseif (im1dcs(dstart+1:dstart+4) .eq.
     +           achar(0)//achar(0)//achar(0)//achar(x'0c')) then
c  Semper 6 unformatted read/write file
            if (v .ge. 3) write
     +                    (6,"('Semper 6 unformatted write file')")
            intyp = 's6rwb'
            lintyp = 5
            if (outtyp .eq. 'd') then
               if (v .ge. 0) write
     +                (6,"('can''t dump semper 6 unformatted files')")
               errg = 1
            else
               errg = gs6rwb()
            endif
            
            defout = 'dm2'
            ldefout = 3
         elseif (im1dcs(dstart+1:dstart+2) .eq.
     +           achar(0)//achar(x'3d') .or.
     +           im1dcs(dstart+1:dstart+2) .eq.
     +           achar(x'ff')//achar(x'ff')) then
c  Digital Micrograph file
            if (v .ge. 3) write (6,"('Digital Micrograph file')")
            intyp = 'dm2'
            lintyp = 3
            if (outtyp .eq. 'd') then
               errg = dmdump()
            else
               errg = gdm()
            endif
            defout = 's6dsk'
            ldefout = 5
         elseif (im1dcs(dstart+1:dstart+4) .eq. 
     +          achar(0)//achar(0)//achar(0)//achar(3)) then
c  Digital Micrograph version 3 file
            if (v .ge. 3) write (6,
     +                  "('Digital Micrograph version 3 file')")
            intyp = 'dm3'
            lintyp = 3
            if (outtyp .eq. 'd') then
               errg = dm3dump()
            else
               errg = gdm3(imgget)
            endif
            defout = 's6dsk'
            ldefout = 5
         elseif (im1dcs(dstart+1:dstart+11) .eq. 'Semper.disc') then
c  Semper 6 picture disc
            if (v .ge. 3) write (6,"('Semper 6 picture disc')")
            intyp = 's6dsk'
            lintyp = 5
            if (outtyp .eq. 'd') then
               errg = s6dskdump()
            else
               if (v .ge. 0) write
     +           (6,"('can''t convert semper 6 picture disc files')")
               errg = 1
            endif
            if (errg .ne. 0 .and. v .ge. 3) write
     +                   (6,"('something went wrong reading file')")
            defout = 'dm2'
            ldefout = 3
         else
c  unrecognised file type
            if (v .ge. 0) write
     +         (6,"('don''t recognise this file type: ',4z2)")
     +         (im1dc(i+dstart),i=0,3)
            intyp = 'unknown'
            lintyp = 7
            fanim = .false.
            goto 100
         endif

c  output file type, if set on command line
         if (outtyp .eq. ' ') then
            outtyp = defout
            louttyp = ldefout
         endif
         if (v .ge. 3) write (6,"('outtyp = ',a)") outtyp(1:louttyp)

c  set mac binary
         if (clmacb .eq. 0) then
            if (outtyp .eq. 'dm2' .or. outtyp .eq. 'elp')
     +              fwmacb = .true.
         elseif (clmacb .eq. 1) then
            fwmacb = .false.
         elseif (clmacb .eq. 2) then
            fwmacb = .true.
         endif

c  file read in
c  checks...
         if (v .ge. 0 .and. outtyp .ne. 'd') then
            if (.not. fim) write (6,"('image not found')")
         endif
         if (v .ge. 0 .and. outtyp .ne. 'd') then
            if (.not. fimwid) write (6,"('image width not found')")
            if (.not. fimhei) write (6,"('image height not found')")
         endif
         if (.not. fimwid .or. .not. fimhei) then
            if (v .ge. 3) write
     +         (6,"('image width or image height not set,',
     +              ' skipping this image')")
            goto 100
         endif
         if (v .ge. 0) then
            if (imhei .eq. 1 .and. imwid .gt. maxsiz**2)
     +          write (6,"('1D image length (',i10,
     +          ') greater than max size (',i10,')')") imwid, maxsiz**2
            if (imhei .gt. 1 .and. imwid .gt. maxsiz)
     +          write (6,"('2D image width (',i6,
     +          ') greater than max size (',i6,')')") imwid, maxsiz
            if (imhei .gt. maxsiz) write (6,"('image height (',i6,
     +          ') greater than max size (',i6,')')") imhei, maxsiz
         endif
         if ((imhei .eq. 1 .and. imwid .gt. maxsiz**2)
     +       .or. (imhei .gt. 1 .and. imwid .gt. maxsiz)
     +       .or. (imhei .gt. maxsiz)) goto 100


c  set time if not already set
         if (.not. fimtime) then
            if (fmotime) then
               imtime = motime
               fimtime = .true.
            elseif (fcrtime) then
               imtime = crtime
               fimtime = .true.
            else
               imtime = time()
               fimtime = .true.
            endif
         endif

c  verify values set from image read in
         if (v .ge. 2) then
            if (fim) then
               write (6,"('Image found')")
            else
               write (6,"('Image not found')")
            endif
            if (.not. fwim) write (6,"('Do not want this image')")
            if (ltitle .eq. 0) then
               write (6,"('no title')")
            elseif (ltitle .gt. 0) then
               write (6,"('title length ',i6)") ltitle
               write (6,"('""',a,'""')") title(1:ltitle)
            endif
            if (fimwid)  write (6,"('width        ',i6)") imwid
            if (fimhei)  write (6,"('height       ',i6)") imhei
            if (fimgmin) write (6,"('image min    ',f12.5)") imgmin
            if (fimgmax) write (6,"('image max    ',f12.5)") imgmax
            if (fdismin) write (6,"('display min  ',f12.5)") dismin
            if (fdismax) write (6,"('display max  ',f12.5)") dismax
            if (fevzero) write (6,"('eV zero      ',f12.5)") evzero
            if (fevpch)  write (6,"('eV/channel   ',f12.5)") evpch
            if (fchoff)  write (6,"('ch offset    ',i6)") choff
            if (fpbyte)  write (6,"('bytes/pixel  ',i6)") pbyte
            if (fptype)  write (6,"('number type  ''',a1,'''')") ptype
            if (fpsign)  write (6,"('1=signed     ',i6)") psign
            if (forigx)  write (6,"('x origin     ',i6)") origx
            if (forigy)  write (6,"('y origin     ',i6)") origy
            if (forigz)  write (6,"('z origin     ',i6)") origz
            if (fnmorx)  write (6,"('x nm at orig ',f12.5)") nmorx
            if (fnmory)  write (6,"('y nm at orig ',f12.5)") nmory
            if (fnmorz)  write (6,"('z nm at orig ',f12.5)") nmorz
            if (fnmppx)  write (6,"('x nm per pix ',f12.5)") nmppx
            if (fnmppy)  write (6,"('y nm per pix ',f12.5)") nmppy
            if (fnmppz)  write (6,"('z nm per pix ',f12.5)") nmppz
            if (fimtime) then
               write (6,"('time         ',z8,', ',a)")
     +               imtime, ctime(imtime)
            endif
            if (ffou)    write (6,"('Fourier transform')")
            if (frmacb)  write (6,"('Reading mac binary file')")
            if (fwmacb)  write (6,"('writing mac binary')")
            if (fmfname) write (6,"('Mac file name ""',a,'""')")
     +                         mfname(1:lmfname)
            if (fmftype) write (6,"('Mac file type ""',a,'""')")
     +                         mftype(1:lmftype)
            if (fmfcrea) write (6,"('Mac file creator ""',a,'""')")
     +                         mfcrea(1:lmfcrea)
            if (fcrtime) write (6,"('Mac create time ',z8,', ',a)")
     +               crtime, ctime(crtime)
            if (fmotime) write (6,"('Mac modified time ',z8,', ',a)")
     +               motime, ctime(motime)
            if (fpreview) then
               write (6,"('This is a preview image')")
            else
               write (6,"('This is not a preview image')")
            endif
            if (fimgmin) write (6,"('Display minimum ',f12.5)") imgmin
            if (fimgmax) write (6,"('Display maximum ',f12.5)") imgmax
            if (fdismin) write (6,"('Image minimum ',f12.5)") dismin
            if (fdismax) write (6,"('Image maximum ',f12.5)") dismax
            if (fsvars) write (6,"('Semper variables'/5f12.5/5f12.5)")
     +                         (svars(i),i=0,9)
            write (6,"('output file type ',a)") outtyp
         endif

c  do not write image if fpreview set or output is a dump
         if (fpreview .or. outtyp .eq. 'd') fwim = .false.

c  write output file only if there is something sensible to write
         if (fim .and. fwim .and. fimwid .and. fimhei .and. fpbyte .and.
     +       fptype) then

c  write output file depending on outtyp
            if (outtyp .eq. 's6rwb') then
c  write semper 6 read/write binary file
               opath = ofdr(1:lofdr) // '.unf'
               lopath = lofdr + 4
               errw = ws6rwb()
            elseif (outtyp .eq. 'el/p') then
c  write EL/P file
               opath = ofdr(1:lofdr) // '.elp'
               lopath = lofdr + 4
               errw = welp()
            elseif (outtyp .eq. 's6dsk') then
c  write semper 6 disc
               dstart = 0
               opath = ofdr(1:lofdr) // '.dsk'
               lopath = lofdr + 4
               if (ldskopath .le. 0) then
c  first time writing to a semper disc, generate dskopath
                  if (v .ge. 3) write
     +                 (6,"('No semper disc name used before,',
     +                       ' generating one')")
                  dskopath = opath
                  ldskopath = lopath
                  if (v .ge. 3) write
     +               (6,"('semper disc name ""',a,'""')")
     +               dskopath(1:ldskopath)
               else
                  opath = dskopath
                  lopath = ldskopath
               endif
c  create semper disc if it doesn't already exist
               errw = cs6dsk(dskopath, ldskopath, dsksiz)
               if (v .ge. 1 .and. errw .ne. 0) write
     +             (6,"('problem creating semper disc')")
               if (errw .eq. 0) errw = ws6dsk()
            elseif (outtyp .eq. 'dm2') then
c  write DM2 file
c  if first image (imgget=1) or only image (imgnum>0) do not add number
c  to fileneme
               if (imgget .eq. 1 .or. imgnum .gt. 0) then
                  opath = ofdr(1:lofdr) // '.dm2'
                  lopath = lofdr + 4
               else
                  write (ci2,"(i2)") imgget
                  opath = ofdr(1:lofdr) // ci2 // '.dm2'
                  lopath = lofdr + 6
               endif
               errw = wdm2()
            else
c  unrecognised output file type
               if (v .ge. 0) write
     +                 (6,"('unrecognised output type: ',a)") outtyp
               errw = 1
            endif
         endif

c  one line verification for v=1
         if (v .ge. 3) write (6,"('errg=',i6,', errw=',i6)") errg, errw
         if (v .ge. 1 .and. fwim) then
            if (errg .eq. 0) then
               if (errw .eq. 0) then
                  write (6,"(a,' (',a,') -> ',a,' (',a,')')")
     +                   path(1:lpath), intyp(1:lintyp),
     +                   opath(1:lopath), outtyp(1:louttyp)
               else
                  write
     +            (6,"(a,' (',a,'): problem writing ',a,' (',a,')')")
     +                   path(1:lpath), intyp(1:lintyp),
     +                   opath(1:lopath), outtyp(1:louttyp)
               endif
            else
               write (6,"('problem reading ',a,' (',a,')')")
     +                path(1:lpath), intyp(1:lintyp)
            endif
         endif

c  if another image in this file then back go and do it
c  but only if imgnum not set
 100     if (fanim .and. imgnum .eq. 0) then
            imgget = imgget + 1
            goto 10
         endif

c  done what we can with this filename, go onto the next
      enddo

c  ----------------

      call exit

c  jump here if problem with semper disc to close and exit
c  This not used at present, and would have to think about
c  whether ofdes should be common...
 200  rv = fileclose(ofdes)
      if (rv .ne. 0) then
         if (v .ge. 0) write
     +        (6,"('ws6dsk: close returned ',i6,' not 0')") rv
      endif
      end
