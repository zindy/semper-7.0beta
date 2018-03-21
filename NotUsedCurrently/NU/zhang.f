c     routine which gets or puts files, raw data
C
      SUBROUTINE zhang
C
C

%INCLUDE '/semper/primitives/common.ins'

c     ldm updated for semper 6 9/26/88

c
      LOGICAL SEMROW,SEMopn,typeo,labme,semlu,varset,semktx
      INTEGER*4 IB(1024)
C
C
      character*80 file_name
      character*1024 buffer
      character*1 yon,yon1(20)
      integer*4 lbyte
      integer semppn
      integer*2 val(1024)
      integer*4 buff(128)
      integer*2 flip
      character*1 bit1(2),bit2
      equivalence (flip,bit1)
      character*20 rrec
      logical lname
      integer*4 istore,istorage, file_size,is
      integer*4 file_start,file_st,file_point,val_point,from_ptr,vptr
      pointer /val_point/val
      common /file_common/file_start,file_st,file_on,file_raw
      logical file_on,file_raw,file_there,lopen,blk_set,file_open_1
     $ ,old_form,flipper
      common /file2_common/iu_store
      common /for_mic/file_size
      save no_file,from_ptr, typeo
      EQUIVALENCE (RB1,IB1,LABEL),(RB5,A1FILE,A1FORM),(TITLE,RB6)

C PACKED NAMES
      DATA NPUT/26460/,NUNIT/-2170/,NREWIN/29023/,NMAX/20864/,
     +NBLK/3691/,nfrom/10335/,nold/24484/,nbit/3580/

C
C INITIALISE
	write(6,*)'Raw byte or i*2 data read'
      IU=IVAL(NUNIT)
      iu=10
c     picture number
      np=ival(nfrom)
      npic=semppn(np)
      flipper=varset(nbit)
c
c     ask for file_name
      file_name(1:80)=' '
      LNAME = VARSET(22453)
      old_form=varset(nold)
      IF (LNAME) THEN
         NF = 80
         IF (SEMKTX(22453,'File name (as textstring): ',
     +              A1FILE,NF,.FALSE.)) GOTO 70
         IF (NF .EQ. 0) GOTO 70
         CALL SEMCHS(FILE_name,A1FILE,NF)
      file_name(NF+1:80)=' '
      else
      WRITE (6,200) 
200   format('Please enter file name > '$)
      read(5,210)file_name
210   format(a80)
      endif

      file_raw=.true.
      call file_set_semper(file_name,st)
      if(st.ne.0)goto 80
c

C
C READ PICTURE DIMENSIONS
c
c
      val_point=file_start
      write(6,501)
501   FORMAT('Enter row/col size _',$)
      read(5,*)lrow,nrow
      write(6,502)
502   format('Enter 0 for 8bit, 1 for 16 bit',$)
      read(5,*)nbit
      write(6,503)
503   format('Enter number of bytes to skip ',$)
      read(5,*)noff
      if(noff.gt.0)then
      	val_point=val_point+noff
      	endif
      
d     write(6,*)lrow,nrow
c
      IOP=2

c
C ********************************
C         CODE FOR GET
C ********************************
      icl=1
      iform=nbit
      nlay=1
      ntitle=120
 1000 if(semopn(iop,npic,lrow,nrow,nlay,icl,iform,lv1))then
      write(6,*)'Error: cannot open semper image'
      call mclose_f(file_start)
        return
      endif
C
      IOP=1
      do 3000 k=1,nlay
      DO 3000 J=1,NROW

c     bit flip ?
      if(flipper)then
      do jj=1,lrow
      flip=val(j)
      bit2=bit1(1)
      bit1(1)=bit1(2)
      bit1(2)=bit2
      val(j)=flip
      enddo
      endif

      if(semrow(2,val,iform,j,k,lv1))return
      if(nbit.eq.0)then
      val_point=val_point+lrow
      else
      val_point=val_point+2*lrow
      endif
 3000 CONTINUE
C
      call mclose_f(file_start)
      RETURN
c
C ERRORS
   70 write(6,*)'Error: file name problem'
      call mclose_f(file_start)
      RETURN
   80 write(6,*)'Error: mapping file problem'
      call mclose_f(file_start)
      return
C
C
      END
