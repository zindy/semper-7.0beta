      subroutine file_set_semper(file_name,st)
c     Initialiser for file writing
c
c
%include '/semper/primitives/common.ins'
c
      logical file_there,extend
      character*1 answer
      character*80 file_name
      integer*4 file_st,file_pointer,st,real_size,idum
      integer*4 Buffer_bytes,end_pointer,test_st
      integer*4 file_start,file_size
      integer*2 stream_id
c
c     Pointer to end
      pointer /end_pointer/end_point
c     Common for pointer to file start
      pointer /file_pointer/dummy
      common /for_mic/file_size
      common /file_common/file_start,file_st,file_on,file_raw
      logical file_on,file_raw
c
      data extend/.true./
c
c     file name length
      ijunk=1
11    if(file_name(ijunk:ijunk).eq.' ')goto 10
      if(ijunk.eq.80)goto 10
      ijunk=ijunk+1
      goto 11
10    ijunk=ijunk-1
c     See if file exists
      file_size=int4(1024)*int4(10000)
      call mapper_f(file_size,file_start,file_name(1:ijunk))
d      write(6,*)real_size,file_start
      st=0
c      file_size=real_size
c
c     done
      file_st=file_start
      return
      end
