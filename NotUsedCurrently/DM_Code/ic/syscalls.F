c  Fortran functions to call system routines for reading & writing etc.
c  Keep all system calls here so they can be easily modified to work
c  on both mac and SG.

c  This version modified from mac version for SG

c  On Sg:
c  f77 -Dirix -C syscalls.f -c

c  on mac:
c  f77 -Ddarwin -fno-underscoring -fbounds-check -Wall -O syscalls.f -c

c--------------------------------------------------------------------------
      function fileopenr(filename)

c  opens files for reading

      integer*4 fileopenr
      character*(*) filename

c  local variables
      integer*4 O_RDONLY, fileopen, rwflag

c  set rwflag for read only
      O_RDONLY   = x'0000'     ! open for reading only
      rwflag = O_RDONLY

c  call fileopen to open file
      fileopenr = fileopen(filename, rwflag)

      return

      end

c--------------------------------------------------------------------------
      function fileopenw(filename)

c  opens files for writing. Creates if doesn't exist and truncates if does
c  flag numbers come from /usr/include/fcntl.h or bits/fcntl.h (linux)

      integer*4 fileopenw
      character*(*) filename

c  local variables
      integer*4 O_WRONLY, O_CREAT, O_TRUNC, fileopen, rwflag

c  set rwflag for read only
      O_WRONLY   = x'0001'     ! open for writing only
#ifdef darwin
      O_CREAT    = x'0200'     ! create if nonexistant (mac)
      O_TRUNC    = x'0400'     ! truncate to zero length (mac)
#else
#ifdef irix
      O_CREAT    = x'0100'     ! create if nonexistant (SG)
      O_TRUNC    = x'0200'     ! truncate to zero length (SG)
#else
#ifdef linux
      O_CREAT    = 0100     ! create if nonexistant (linux)
      O_TRUNC    = 1000     ! truncate to zero length (linux)
#endif
#endif
#endif
      rwflag = O_WRONLY + O_CREAT + O_TRUNC
c      write (6,"('fileopenw: rwflag=',z8)") rwflag

c  call fileopen to open file
      fileopenw = fileopen(filename, rwflag)
c      write (6,"('fileopenw: fileopen returned ',i6)") fileopenw

      return

      end

c--------------------------------------------------------------------------
      function fileopenrw(filename)

c  opens files for reading and writing.
c  does not create file if it doesn't exist

      integer*4 fileopenrw
      character*(*) filename

c  local variables
      integer*4 O_RDWR, fileopen, rwflag

c  set rwflag for reading and writing
      O_RDWR     = x'0002'     ! open for reading and writing
      rwflag = O_RDWR

c  call fileopen to open file
      fileopenrw = fileopen(filename, rwflag)

      return

      end

c--------------------------------------------------------------------------
      function fileopen(filename, rwflag)

c  fileopen is called by fileopenr, fileopenw, fileopenrw
c  have fileerror as common string for errors
c  opens file for reading and writing, assuming it exists
c  returns file descriptor
c     >0  file exists
c     -1  file doesn't exist (it was 0 on SG)

      integer*4 fileopen, rwflag
      character*(*) filename

c  local variables
      integer*4 lfilename, lnblnk, mode, ai4, open
      character*200 fname0

c  find length of filename
      lfilename = lnblnk(filename)

c  add null to end of filename
      fname0 = filename(1:lfilename) // char(0)
c      write (6,"('fileopen: filename=""',a,'""')") filename(1:lfilename)

c  mode setting rw-r--r--
      mode = o'644'
c      write (6,"('fileopen: rwflag=',z8,' mode=',z8)") rwflag, mode

c  open file
#ifdef darwin
      ai4 = open(fname0, %val(rwflag), %val(mode))   ! (mac)
#else
#ifdef irix
      ai4 = open(fname0, rwflag, mode)    ! (SG)
#else
#ifdef linux
      ai4 = open(fname0, %val(rwflag), %val(mode))   ! (linux)
#endif
#endif
#endif

c      write (6,"('filename ""',a,'""')") fname0
c      write
c     +      (6,"('fileopen: open returned ',z8,' with rwflag ',z8,
c     +       ' mode ',o8)")
c     +      ai4, rwflag, mode

      fileopen = ai4

      return

      end
      
c--------------------------------------------------------------------------
C$$$      function lenstring(string)

C$$$c  Use lnblnk(string) as it is an intrinsic that does the same
C$$$c  Find the length of string by counting backwards looking for non-spaces
C$$$c  length can be 0

C$$$      integer*4 lenstring
C$$$      character*(*) string

C$$$c  local variables
C$$$      integer*4 lstring, i

C$$$c  empty string may have a length of 0
C$$$      lstring = 0

C$$$c  find length of string by counting backwards
C$$$      do i=len(string), 1, -1
C$$$         if (string(i:i) .ne. ' ') then
C$$$            lstring = i
C$$$            goto 10
C$$$         endif
C$$$      enddo
C$$$ 10   continue

C$$$      lenstring = lstring
C$$$      return

C$$$      end
c--------------------------------------------------------------------------
      function fileread(fd, string, lstring)

c  read character string from file
c  returns number of characters read

      integer*4 fileread, fd, lstring
      character*(*) string

c  local variables
      integer*4 rv, read


c  call read
#ifdef darwin
      rv = read(%val(fd), string, %val(lstring))    ! (mac)
#else
#ifdef irix
      rv = read(fd, string, lstring)    ! (SG)
#else
#ifdef linux
      rv = read(%val(fd), string, %val(lstring))    ! (linux)
#endif
#endif
#endif
c      write(6,"('rv = ',i12)") rv
c      if (rv .ne. lstring) then
c         write
c     +      (6,"('fileread: read returned ',i6,' not ',i6)")
c     +            rv, lstring
c      endif
      fileread = rv

      return

      end
c--------------------------------------------------------------------------
      function filewrite(fd, string, lstring)

c  write character string to file
c  returns number of characters written

      integer*4 filewrite, fd, lstring
      character*(*) string

c  local variables
      integer*4 rv, write

      integer*4 filepos, filesize

c  call write
#ifdef darwin
      rv = write(%val(fd), string, %val(lstring))   ! (mac)
#else
#ifdef irix
      rv = write(fd, string, lstring)    ! (SG)
#else
#ifdef linux
      rv = write(%val(fd), string, %val(lstring))   ! (linux)
#endif
#endif
#endif

c      if (rv .ne. lstring) then
c         write
c     +      (6,"('filewrite: write returned ',i6,' not ',i6)")
c     +            rv, lstring
c      endif
      filewrite = rv

      return

      end
c--------------------------------------------------------------------------
      function fileclose(fd)

c  close file
c  returns 0 if OK, -1 if problem

      integer*4 fileclose, fd

c  local variables
      integer*4 close

c  close file
#ifdef darwin
      fileclose = close(%val(fd))    ! (mac)
#else
#ifdef irix
      fileclose = close(fd)    ! (SG)
#else
#ifdef linux
      fileclose = close(%val(fd))    ! (linux)
#endif
#endif
#endif

      return

      end
c--------------------------------------------------------------------------
      function fileseek(fd, pos)

c  seek to position in file relative to the beginning of the file
c  returns resulting position in file relative to beginning
c  returns -1 if problem

      integer*8 fileseek, pos
      integer*4 fd
 
c  local variables
      integer*4 SEEK_SET
#ifdef darwin
      integer*8 lseek    ! (mac)
#else
#ifdef irix
      integer*4 lseek, pos4    ! (SG)
#else
#ifdef linux
      integer*4 lseek, pos4    ! (linux)
#endif
#endif
#endif

c  values for "whence" come from /usr/include/sys/unistd.h
      SEEK_SET = 0
#ifdef irix
      pos4 = pos    ! (SG)
#else
#ifdef linux
      pos4 = pos    ! (linux)
#endif
#endif

c  seek to position
c      write (6,"('fd = ',i6,', pos = ',i6,', SEEK_SET = ',i6)")
c     +      fd, pos, SEEK_SET
#ifdef darwin
      fileseek = lseek(%val(fd), %val(pos), %val(SEEK_SET))    ! (mac)
#else
#ifdef irix
      fileseek = lseek(fd, pos4, SEEK_SET)    ! (SG)
#else
#ifdef linux
      fileseek = lseek(%val(fd), %val(pos4), %val(SEEK_SET))    ! (linux)
#endif
#endif
#endif
c      write (6,"('fileseek = ',z16)") fileseek

      return

      end
c--------------------------------------------------------------------------
      function fileseekr(fd, pos)

c  seek to position in file relative to the current position
c  returns resulting position in file relative to beginning
c  returns -1 if problem

      integer*8 fileseekr, pos
      integer*4 fd
 
c  local variables
      integer*4 SEEK_CUR
#ifdef darwin
      integer*8 lseek    ! (mac)
#else
#ifdef irix
      integer*4 lseek, pos4    ! (SG)
#else
#ifdef linux
      integer*4 lseek, pos4    ! (linux)
#endif
#endif
#endif

c  values for "whence" come from /usr/include/sys/unistd.h
      SEEK_CUR = 1
#ifdef irix
      pos4 = pos    ! (SG)
#else
#ifdef linux
      pos4 = pos    ! (linux)
#endif
#endif

c  seek to position
#ifdef darwin
      fileseekr = lseek(%val(fd), %val(pos), %val(SEEK_CUR))    ! (mac)
#else
#ifdef irix
      fileseekr = lseek(fd, pos4, SEEK_CUR)    ! (SG)
#else
#ifdef linux
      fileseekr = lseek(%val(fd), %val(pos4), %val(SEEK_CUR))    ! (linux)
#endif
#endif
#endif

      return

      end
c--------------------------------------------------------------------------
      function fileseeke(fd, pos)

c  seek to position in file relative to the end of the file
c  returns resulting position in file relative to beginning
c  returns -1 if problem

      integer*8 fileseeke, pos
      integer*4 fd
 
c  local variables
      integer*4 SEEK_END
#ifdef darwin
      integer*8 lseek    ! (mac)
#else
#ifdef irix
      integer*4 lseek, pos4    ! (SG)
#else
#ifdef linux
      integer*4 lseek, pos4    ! (linux)
#endif
#endif
#endif

c  values for "whence" come from /usr/include/sys/unistd.h
      SEEK_END = 2
#ifdef irix
      pos4 = pos    ! (SG)
#else
#ifdef linux
      pos4 = pos    ! (linux)
#endif
#endif

c  seek to position
#ifdef darwin
      fileseeke = lseek(%val(fd), %val(pos), %val(SEEK_END))    ! (mac)
#else
#ifdef irix
      fileseeke = lseek(fd, pos4, SEEK_END)    ! (SG)
#else
#ifdef linux
      fileseeke = lseek(%val(fd), %val(pos4), %val(SEEK_END))    ! (linux)
#endif
#endif
#endif

      return

      end
c--------------------------------------------------------------------------
      function filepos(fd)

c  returns position in file
c  returns -1 if problem

      integer*8 filepos
      integer*4 fd
 
c  local variables
      integer*4 SEEK_CUR
      integer*8 pos
#ifdef darwin
      integer*8 lseek    ! (mac)
#else
#ifdef irix
      integer*4 lseek, pos4    ! (SG)
#else
#ifdef linux
      integer*4 lseek, pos4    ! (linux)
#endif
#endif
#endif

c  values for "whence" come from /usr/include/sys/unistd.h
      SEEK_CUR = 1
      pos = 0
#ifdef irix
      pos4 = pos    ! (SG)
#else
#ifdef linux
      pos4 = pos    ! (linux)
#endif
#endif

c  seek to offset of 0 relative to current position
c      write (6,"('filepos: fd = ',i6,', pos = ',i6,', SEEK_CUR = ',i6)")
c     +      fd, pos, SEEK_CUR
#ifdef darwin
      filepos = lseek(%val(fd), %val(pos), %val(SEEK_CUR))    ! (mac)
#else
#ifdef irix
      filepos = lseek(fd, pos4, SEEK_CUR)    ! (SG)
#else
#ifdef linux
c      write (6,"('linux')")
      filepos = lseek(%val(fd), %val(pos4), %val(SEEK_CUR))    ! (linux)
#endif
#endif
#endif
c      write (6,"('filepos: filepos = ',z16)") filepos

      return

      end
c--------------------------------------------------------------------------
      function filesize(fd)

c  returns size of file
c  returns -1 if problem

      integer*8 filesize
      integer*4 fd
 
c  local variables
      integer*4 SEEK_END
      integer*8 filepos, fileseek, pos, curpos
#ifdef darwin
      integer*8 lseek    ! (mac)
#else
#ifdef irix
      integer*4 lseek, pos4    ! (SG)
#else
#ifdef linux
      integer*4 lseek, pos4    ! (linux)
#endif
#endif
#endif

c  values for "whence" come from /usr/include/sys/unistd.h
      SEEK_END = 2
      pos = 0
#ifdef irix
      pos4 = pos    ! (SG)
#else
#ifdef linux
      pos4 = pos    ! (linux)
#endif
#endif

c  find current position
      curpos = filepos(fd)

c      write (6,"('curpos = ',i12)") curpos

c  seek to end
c      write (6,"('fd = ',i6,', pos = ',i6,', SEEK_END = ',i6)")
c     +      fd, pos, SEEK_END
#ifdef darwin
      filesize = lseek(%val(fd), %val(pos), %val(SEEK_END))    ! (mac)
#else
#ifdef irix
      filesize = lseek(fd, pos4, SEEK_END)    ! (SG)
#else
#ifdef linux
      filesize = lseek(%val(fd), %val(pos4), %val(SEEK_END))    ! (linux)
#endif
#endif
#endif
c      write (6,"('filesize = ',z16)") filesize

c  go back to original position
      pos = fileseek(fd, curpos)

      return

      end
c--------------------------------------------------------------------------
      function filereadi1(fd, i)

c  Reads 1 byte and returns an i1 integer

      integer*4 fd, filereadi1

c  local variables
      character*1 ci1
      integer*1 i1, i
      integer*4 fileread, rv

      equivalence (ci1, i1)

      i1 = 0

      rv = fileread(fd, ci1, 1)
      i = i1
      filereadi1 = rv

      return

      end
c--------------------------------------------------------------------------
      function filereadi2(fd, i, byteorder)

c  Reads 2 bytes and returns an i2 integer

      integer*4 fd, filereadi2
      logical byteorder

c  local variables
      character*2 ci2
      integer  i, strtoi2
      integer*4 fileread, rv


      rv = fileread(fd, ci2, 2)
      i = strtoi2(ci2, byteorder)
      filereadi2 = rv

      return

      end
c--------------------------------------------------------------------------
      function filereadi4(fd, i, byteorder)

c  Reads 4 bytes and returns an i4 integer

      integer*4 fd, filereadi4
      logical byteorder

c  local variables
      character*4 ci4
      integer*4 i, strtoi4
      integer*4 fileread, rv


      rv = fileread(fd, ci4, 4)
      i = strtoi4(ci4, byteorder)
      filereadi4 = rv

      return

      end
c--------------------------------------------------------------------------
      function filereadf4(fd, f, byteorder)

c  Reads 4 bytes and returns a 4 byte real
c  No checking that it is a real...

      integer*4 fd, filereadf4
      logical byteorder

c  local variables
      character*4 cf4
      real*4 f, strtof4
      integer*4 fileread, rv


      rv = fileread(fd, cf4, 4)
      f = strtof4(cf4, byteorder)
      filereadf4 = rv

      return

      end
