c  vars.f
c  ======

c  Contains the variables etc that need to be included at the start of
c  every function

c  Max size of 2D images assuming 4 bytes/pixel and square
      integer*4 maxsiz, maxarg, bufsiz, charlen
      parameter (maxsiz=4096, maxarg=1000, bufsiz=16384)
c      parameter (maxsiz=1500, maxarg=1000, bufsiz=16384)
      parameter (charlen = 4*maxsiz*maxsiz)

c  buffer for storing image. All are equivalenced together
c  Assume image maxsiz n*n with 4 bytes/pixel ie total bytes used 4*n*n

c                    2D size        1D size
c  char/i1           4*n by n       4*n*n
c  i2                2*n by n       2*n*n
c  i4/real           n   by n       n*n
c  complex/real*8    n/2 by n       n*n/2
c  complex*16        n/4 by n       n*n/4
c  rgb byte 3 layer  n   by n by 3  n*n by 3

      character   im1dcs*(4*maxsiz*maxsiz)    ! NB runs from 1 to 4*maxsiz*maxsiz
      character*1 im1dc (0:4*maxsiz*maxsiz-1)
      integer*1   im1di1(0:4*maxsiz*maxsiz-1)
      integer    im1di2(0:2*maxsiz*maxsiz-1)
      integer*4   im1di4(0:maxsiz*maxsiz-1)
      real*4      im1dr (0:maxsiz*maxsiz-1)
      real*8      im1dr8(0:maxsiz*maxsiz/2-1)
      complex*8   im1dx (0:maxsiz*maxsiz/2-1)
      character*1 im1dcc(0:maxsiz*maxsiz-1,   0:2)
      integer*1   im1dco(0:maxsiz*maxsiz-1,   0:2)
      integer    im1dc2(0:maxsiz*maxsiz/2-1, 0:2)
      character*1 imagc (0:4*maxsiz-1, 0:maxsiz-1)
      integer*1   imagi1(0:4*maxsiz-1, 0:maxsiz-1)
      integer    imagi2(0:2*maxsiz-1, 0:maxsiz-1)
      integer*4   imagi4(0:maxsiz-1,   0:maxsiz-1)
      real*4      imagr (0:maxsiz-1,   0:maxsiz-1)
      real*8      imagr8(0:maxsiz/2-1, 0:maxsiz-1)
      complex*8   imagx (0:maxsiz/2-1, 0:maxsiz-1)
      character*1 imagcc(0:maxsiz-1,   0:maxsiz-1, 0:2)
      integer*1   imagco(0:maxsiz-1,   0:maxsiz-1, 0:2)
      integer    imagc2(0:maxsiz/2-1, 0:maxsiz-1, 0:2)

c  general one line buffer, all equivalenced together
      character   bufc*(bufsiz*4)      ! NB runs from 1 to bufsiz*4
      integer*1   bufi1(0:bufsiz*4-1)
      integer    bufi2(0:bufsiz*2-1)
      integer*4   bufi4(0:bufsiz-1)
      real*4      bufr (0:bufsiz-1)
      integer*4   lbufc                ! length of bufc

c  variables to do with the image. *=must be set
      integer*4 imwid, imhei           !*image width & height
      character title*1024             ! title
      integer*4 ltitle                 !*length of title
      real*4    evzero, evpch          ! eV of zero channel, eV per channel
c  evzero is always the left channel, no matter what origx is set to
c  evzero in elp is the left channel including the offset, choff
c  here evzero is converted to be the leftmost real channel
      integer  choff                  ! channel offset
      integer  pbyte                  !*bytes/pixel
      character ptype*1                !*i=int, f=float, c=complex
c                                        r=rgb
      integer  psign                  !*0=unsigned, 1=signed
      integer*4 origx, origy, origz    ! origin (top left=1,1)
      real*4    nmorx, nmory, nmorz    ! nm at origin
      real*4    nmppx, nmppy, nmppz    ! scale nm per pixel
      integer*4 imtime                 ! time recorded in image (unix style)
      integer*4 crtime                 ! file creation time (unix style)
      integer*4 motime                 ! file last modified time (unix style)
      logical   ffou                   ! true if image is a FT
      integer*4 clmacb                 ! 0=default, 1=force no macb, 2=macb
      logical   fwmacb                 ! true if write in mac binary
      logical   frmacb                 ! true if file being read is mac binary
      character mfname*64              ! mac file name
      character mftype*4               ! mac file type
      character mfcrea*4               ! mac file creator
      real*4    imgmin                 ! min value for image
      real*4    imgmax                 ! max value for image
      real*4    dismin                 ! min value for display
      real*4    dismax                 ! max value for display
      real*4    svars(0:9)             ! Semper variables v0 to v9

c  flags to say what is set
      logical   fim, fimwid, fimhei, fevzero, fevpch, fchoff
      logical   fpbyte, fptype, fpsign, forigx, forigy, forigz
      logical   fnmorx, fnmory, fnmorz, fnmppx, fnmppy, fnmppz, fimtime
      logical   fcrtime, fmotime, fmfname, fmftype, fmfcrea
      logical   fimgmin, fimgmax, fdismin, fdismax, fsvars

c  general bits and pieces
      integer*4 rv, cv
      character path*200       ! full pathname
      character dir*200        ! directory name
      character file*200       ! file name
      character froot*200      ! root file name (before the .)
      character fext*200       ! file extension (after the .)
      character ofdr*200       ! output dir + root (all except extension)
      character opath*200      ! full output path name (above + ext)
      character dskopath*200   ! full output path name of semper disc
      integer*4 lpath, ldir, lfile, lfroot, lfext, lofdr, lopath
      integer*4 ldskopath
      character pathns(maxarg)*200 ! array of path names from args
      integer*4 lpathns(maxarg)    ! length of path names
      integer*4 npathns            ! number of path names
      character fname*200      ! temp filename
      integer  ai2            ! temp
      character c*2000         ! temp
      integer*4 lc             !  "
      integer*4 ifdes          ! input file descriptor
      integer*4 ofdes          ! output file descriptor
      integer*4 v              ! verification, 0=errors only, 3=lots
      character intyp*12       ! input file type
      character defout*12      ! output file type
      character outtyp*12      ! output file type from command line
      integer*4 lintyp, ldefout, louttyp
      integer*4 deltat         ! unix = apple - deltat, deltat = 2082844800
      integer*4 dstart         ! start of data eg for macbinary
      integer*4 dlength        ! length of data (from macbinary)
      integer*4 lmfname, lmftype, lmfcrea
      integer*4 dsksiz         ! semper disc size requested (Kbyte)
      integer*4 ndskpic        ! next semper disc picture number
      logical   fdskenl        ! true if semper disc can be enlarged
      integer*4 semdir(2,2008) ! semper disc label
      integer*4 semdlen        ! length (blocks) of semper directory
      integer*4 semdflen       ! length (blocks) of semper disc from dir
      integer*4 semdlsd        ! number of last SD in dir (pic 1000)
      integer*4 semflen        ! actual length (bytes) of semper disc
      integer*4 semlastp       ! block after last picture
      integer*4 semfree        ! free blocks at end
      logical   fsemdir        ! true if semper directory read
      logical   fanim          ! true if there is another image in a picture
      logical   fwim           ! normally true, false if don't want to write im
      logical   fpreview       ! true if dm image is a preview image
      logical   bigend         ! big endian (false)
      parameter (bigend = .false.)
      logical   littleend      ! little endian (true)
      parameter (littleend = .true.)

c  semper variables, all need to be i2
      integer  ncol, nrow, nlay, iclass, iform, ntitle, ilabel
      integer  iversn, iflag, blim, iccoln
      integer  labeli2(256)
      integer  iyear, imonth, iday, ihour, iminut, isec
      integer*4 tarray(9)
      real*4    evsemzero, tevzero
      character stitle*156, label*256

c  temporary variables for conversions, equivalent, but not common
      integer  i2            ! temp
      character ci2*2         ! char version of i2
      integer*4 i4            ! temp
      character ci4*4         ! char version of i4
      real*4    f4            ! temp
      character cf4*4         ! char version of f4
      real*8    f8            ! temp
      character cf8*8         ! char version of f8

c  command line arguments
      character clarg*200
      integer*4 nclargs, lclarg
      integer*4 iargc

c  functions
      integer*4 getelp, gs6rwb, ws6rwb, welp, uxtime, dmdump, cs6dsk
      integer*4 wdm2, getan10000, wan10000
      integer*4 s6dskdump, gdsks, wdsks, gfs, clseek, cwrite, ws6dsk
      integer*4 copenrw, roundup, ws6lab, slabnp, slabnewpic
      integer*4 gsdir, wsdir, cnextb, cwri64, dm3dump, creadi4, creadi2
      integer*4 creadi4rev, creadi2rev, creadc
      integer*4 creadf4, creadf4rev, clseekr
      integer*4 gfpos, gdm, gdm3, copenr, creadf8, creadf8rev
      integer*4 strtoi4
      integer   strtoi2
      logical   progbyteorder
      real*4    strtof4
      real*8    imgreal8
      character*2 i2tostr
      character*4 i4tostr

c  functions in syscalls.f
      integer*4 fileopenr, fileopenw, fileopenrw, fileopen
      integer*4 fileread
      integer*4 filewrite, fileclose, filereadi1, filereadi2, filereadi4
      integer*4 filereadf4

      integer*8 fileseek, fileseekr, fileseeke, filepos, filesize

c  system functions
c      integer*4 open, close, read, write, time
      character ctime*24

c  commons and equivalences (it seems chars have to be separate)
c  i2 has to be after i4
      common /image/ im1dc
      common /buf/   bufc
      common /imvar4/ imwid, imhei, evzero, evpch, ltitle, origx, origy,
     +                origz, nmorx, nmory, nmorz, nmppx, nmppy, nmppz,
     +                imtime, deltat, clmacb, crtime, motime,
     +                imgmin, imgmax, dismin, dismax, svars
      common /imvar2/ choff, pbyte, psign
      common /logic/ fim, fimwid, fimhei, fevzero, fevpch, fchoff,
     +               fpbyte, fptype, fpsign, forigx, forigy,
     +               fnmorx, fnmory, fnmorz, fnmppx, fnmppy, fnmppz,
     +               fimtime,
     +               ffou, fwmacb, frmacb, fcrtime, fmotime, fmfname,
     +               fmftype, fmfcrea, fdskenl, fsemdir, fanim,
     +               fwim, fpreview, fimgmin, fimgmax, fdismin, fdismax,
     +               fsvars
      common /chars/ clarg, title, ptype, path, dir, file, froot, fext,
     +               intyp, defout, outtyp, ofdr, opath, dskopath,
     +               mfname, mftype, mfcrea
      common /generali4/ v, nclargs, lclarg, lpath, ldir, lfile,
     +                 lfroot, lfext, lintyp, ldefout, louttyp, lofdr,
     +                 lopath, ldskopath, lbufc, dstart, lmfname,
     +                 lmftype, lmfcrea, dsksiz, ndskpic, semdir,
     +                 semdlen, semdflen, semdlsd, semflen, semfree,
     +                 semlastp
      common /semi2/   ncol, nrow, nlay, iclass, iform, ntitle, ilabel,
     +                 iversn, iflag, blim, iccoln, labeli2,
     +                 iyear, imonth, iday, ihour, iminut, isec
      common /semi4/   tarray
      common /semr/    evsemzero, tevzero
      common /semchar/ stitle, label

      equivalence (im1dc, im1di1, im1di2, im1di4, im1dr, im1dr8, im1dx,
     +             im1dcc, im1dco, im1dc2, im1dcs,
     +             imagc, imagi1, imagi2, imagi4, imagr, imagr8, imagx,
     +             imagcc, imagco, imagc2)

      equivalence (bufc, bufi1, bufi2, bufi4, bufr)
      equivalence (i2, ci2)
      equivalence (i4, ci4)
      equivalence (f4, cf4)
      equivalence (f8, cf8)
