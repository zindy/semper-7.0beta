c  misc.f

c  Contains misc functions and subroutines that are not related to ic
c  and can be used by any program

c--------------------------------------------------------------------------
      subroutine lower(string)

c  converts character string into lower case

      character*(*) string
      integer*4 i

      do i=1,len(string)
         if (ichar(string(i:i)) .ge. 65 .and.
     +       ichar(string(i:i)) .le. 90)
     +       string(i:i) = char(ichar(string(i:i)) + 32)
      enddo

      return

      end

c--------------------------------------------------------------------------
      subroutine eqtosp(string)

c  converts = into space in a character string

      character*(*) string
      integer*4 i

      do i=1,len(string)
         if (string(i:i) .eq. '=') string(i:i) = ' '
      enddo

      return

      end

c--------------------------------------------------------------------------
      subroutine printchar(string)

c  converts a character string to printable characters only, replacing
c  others with a .

      character*(*) string
      integer*4 i

      do i=1,len(string)
         if (ichar(string(i:i)) .le. 31 .or.
     +       ichar(string(i:i)) .ge. 127) string(i:i) = '.'
      enddo

      return

      end

c--------------------------------------------------------------------------
      function uxtime(year, month, day, hour, min, sec)

c  converts times to unix format times
c  should work OK up to 2100, which is not a leap year

      integer*4 uxtime, year, month, day, hour, min, sec
      integer*4 iyear
      integer*4 mon(12)

      data mon /0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334/

      if (year .le. 255) then
         iyear = year - 70
      else
         iyear = year - 1970
      endif

c  work out no. of days allowing for leap years
      uxtime = iyear*365 + (iyear+1)/4 + mon(month) + day-1

c  add 1 day if leap year and month is after Feb
      if (mod(year, 4) .eq. 0 .and. month .ge. 3) uxtime = uxtime + 1

c  convert to seconds
      uxtime = ((uxtime*24 + hour)*60 + min)*60 + sec

      return

      end

c--------------------------------------------------------------------------
      function roundup(num, unit)

c  rounds up num to nearest multiple of unit

c  local variables
      integer*4 roundup, num, unit, ai4

      ai4 = num/unit
      if (mod(num, unit) .gt. 0) ai4 = ai4 + 1
      roundup = ai4*unit

      return

      end

c--------------------------------------------------------------------------
      function progbyteorder()

c  Tests byte order of machine program is compiled on
c  Returns false if machine is big endian (Mac, SG, Motorola)
c  Returns true if machine is little endian (PC, Intel)

      logical progbyteorder

c  local variables
      integer  n
      character*2 s
      equivalence (n, s)

      n = 1
      progbyteorder = s(1:1) .eq. char(1)

      return

      end

c--------------------------------------------------------------------------
      function strtoi2(string, byteorder)

c  Converts a 2 byte string into an integer
c  byteorder = false, string is big endian (mac)
c  byteorder = true, string is little endian (PC)

      integer  strtoi2
      logical byteorder
      character*2 string

c  local variables

      integer  n
      character*2 s
      equivalence (n, s)
      logical progbo, progbyteorder

      progbo = progbyteorder()

      if (progbo .eqv. byteorder) then
         s = string
      else
         call stringrev2(string, s)
      endif

      strtoi2 = n

      return

      end

c--------------------------------------------------------------------------
      function strtoi4(string, byteorder)

c  Converts a 4 byte string into an integer
c  byteorder = false, string is big endian (mac)
c  byteorder = true, string is little endian (PC)

      integer*4 strtoi4
      logical byteorder
      character*4 string

c  local variables

      integer*4 n
      character*4 s
      equivalence (n, s)
      logical progbo, progbyteorder

      progbo = progbyteorder()

      if (progbo .eqv. byteorder) then
         s = string
      else
         call stringrev2(string, s)
      endif

      strtoi4 = n

      return

      end

c--------------------------------------------------------------------------
      function strtof4(string, byteorder)

c  Converts a 4 byte string into a real
c  byteorder = false, string is big endian (mac)
c  byteorder = true, string is little endian (PC)

      real*4 strtof4
      logical byteorder
      character*4 string

c  local variables

      real*4 n
      character*4 s
      equivalence (n, s)
      logical progbo, progbyteorder

      progbo = progbyteorder()

      if (progbo .eqv. byteorder) then
         s = string
      else
         call stringrev2(string, s)
      endif

      strtof4 = n

      return

      end

c--------------------------------------------------------------------------
      function i2tostr(int, byteorder)

c  Converts a 2 byte integer to a string
c  byteorder = false, string is big endian (mac)
c  byteorder = true, string is little endian (PC)

      character*2 i2tostr
      logical byteorder
      integer  int

c  local variables

      integer  n
      character*2 s
      equivalence (n, s)
      logical progbo, progbyteorder

      progbo = progbyteorder()

      n = int

      if (progbo .eqv. byteorder) then
         i2tostr = s
      else
         call stringrev2(s, i2tostr)
      endif

      return

      end

c--------------------------------------------------------------------------
      function i4tostr(int, byteorder)

c  Converts a 4 byte integer to a string
c  byteorder = false, string is big endian (mac)
c  byteorder = true, string is little endian (PC)

      character*4 i4tostr
      logical byteorder
      integer*4 int

c  local variables

      integer*4 n
      character*4 s
      equivalence (n, s)
      logical progbo, progbyteorder

      progbo = progbyteorder()

      n = int

      if (progbo .eqv. byteorder) then
         i4tostr = s
      else
         call stringrev2(s, i4tostr)
      endif

      return

      end

c--------------------------------------------------------------------------
      subroutine stringrev(string)
c  Reverses the bytes in string
c  Reverses all bytes, even if the end bytes are spaces

      character*(*) string

c  local variables
      integer*4 l, i, j
      character*1 s

      l = len(string)

      do i=1,int(l/2)
         j = l+1-i
         s = string(i:i)
         string(i:i) = string(j:j)
         string(j:j) = s
      enddo

      end

c--------------------------------------------------------------------------
      subroutine stringrev2(string, string2)
c  Reverses the bytes in string and puts in string2, leaving string unchanged
c  Reverses all bytes, even if the end bytes are spaces
c  Assumes string2 is same length as string
c  May be slightly faster than stringrev if this matters

      character*(*) string, string2

c  local variables
      integer*4 l, i, j

      l = len(string)

      do i=1,l
         j = l+1-i
         string2(j:j) = string(i:i)
      enddo

      end
