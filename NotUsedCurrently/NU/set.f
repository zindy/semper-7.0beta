      subroutine set

c     display paramters
%include '/semper/primitives/display_common.ins'
%include '/semper/primitives/common.ins'
      integer*4 st
      integer*2 size(2),a1(80)
      character*80 file_name
      PARAMETER (NSIZE=30786, NSI2=30792)
      parameter (NSYNC=31414, NUNSYNC=-2180)
      parameter (nfon=10214, nref=29006,nfra=10321,npic=25963
     $ , noref=23018, nbw=4120,noutput=24860)
      logical varset,semktx,outset,opt

c     syncronize ?
      if(varset(nsync))then
           call ssync
      else if(varset(nuncync))then
           call sunsync
      endif

c     set auto-refresh
c     this slows down the operations substantially
      iref=ival(nref)
      inref=ival(noref)
c     load a new font
      ifont=ival(nfon)
      outset=opt(noutput)

      if(inref.gt.0)then
      write(6,*)'setting norefresh not relevant'
c      call gpr_$set_auto_refresh(.false.,st)
c      if(st.ne.0)goto 999

      else if(iref.gt.0)then
      write(6,*)'setting refresh not relevant'
c      call gpr_$set_auto_refresh(.true.,st)
c      if(st.ne.0)goto 999
      endif

      if(outset)then
c     ask for file_name
      file_name(1:80)=' '
      write(6,10)'File name _'
10    format(a,$)
      read(5,11,err=70,end=70)file_name
11    format(a)
      ijunk=1
110   if(file_name(ijunk:ijunk).eq.' ')goto 100
      if(ijunk.eq.80)goto 100
      ijunk=ijunk+1
      goto 110
100   ijunk=ijunk-1
      if(ijunk.eq.0)goto 70
      IU=LOGFLE
      OPEN(IU,FILE=file_name(1:ijunk),STATUS='append',ERR=71)
      endif

      if(ifont.gt.0)then
      write(6,*)'Font setting not implimented yet'
           if(ifont.eq.1.or.ifont.gt.4)then
d     write(6,*)'setting font std.color'
           else if(ifont.eq.2)then
d     write(6,*)'setting font f5x9'
           else if(ifont.eq.3)then
d     write(6,*)'setting font f7x11'
           else if(ifont.eq.4)then
d     write(6,*)'setting font b.12'
           endif


c      nchx=size(1)/10
c      nchy=size(2)+1
c      chsiz(1)=nchx
c      chsi2(1)=nchy

      endif

c     reset frame size - not operational
      if (varset(nfra))then
d     write(6,*)'setting frame'
      IF (VARSET(NSIZE)) THEN
         Nfx=IVAL(NSIZE)
C
         IF (VARSET(NSI2)) THEN
            Nfy=IVAL(NSI2)
         ELSE
            Nfy=nfx
         ENDIF
      endif
d     write(6,*)'setting frame width to ',nfx
d     write(6,*)'setting frame depth to ',nfy
      frsiz(1)=nfx
      frsi2(1)=nfy
      endif

c     reset picture size (?)
      if (varset(npic))then
d     write(6,*)'setting monitor'
      IF (VARSET(NSIZE)) THEN
         Nmx=IVAL(NSIZE)
C
         IF (VARSET(NSI2)) THEN
            Nmy=IVAL(NSI2)
         ELSE
            Nmy=nmx
         ENDIF
      endif
d     write(6,*)'setting monitor width to ',nmx
d     write(6,*)'setting monitor width to ',nmy
      monsiz(1)=nmx
      monsi2(1)=nmy
      endif

      return
999   write(6,*)'Error in set '
      return
   70 write(6,*)'Error: file name bad: ',file_name
      RETURN
   71 write(6,*)'Error: unable to open new output file'
      return
      end
