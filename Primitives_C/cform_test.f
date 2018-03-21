C Semper 6 primitive proving program for CFORM
C
      program cform_test

C     include '../primitives/display_common.ins'
      INTEGER TERM2
C
C ****** CHANGE ******
C TERM2 is the unit number for verification messages
      PARAMETER (TERM2=6)
C ****** ****** ******
C
      REAL B2(512)
      INTEGER*2 IB1(256),IB2(256),oldf,newf
      INTEGER*4 I4256
      PARAMETER (I4256=256)
      EQUIVALENCE (B2(1),IB2(1))
C
C List of forms adopted in turn
c     duplicate too much
c      PARAMETER (NFORMS=24)
      parameter (nforms=14)
      INTEGER*4 FORMS(NFORMS)
c      DATA FORMS/0,1,2,1,3,1,0,2,1,2,0,1,0,3,1,3,0,1,2,3,1,3,2,1/
      data forms/0,1,2,1,3,1,0,2,0,3,0,2,3,2/

c
c create a screen to display data
C
C ****** CHANGE ******
C Initialise configuration parameters requested of FSAS61 if nec
      NFRS=0
      FRSIZ=0
      FRSI2=0
      VWSIZ=0
      VWSIZ=0
      CHSIZ=0
      CHSI2=0
      GPSIZ=0
      PROTN=0
C ****** ****** ******
C
C      FSRET=FSAS61(NFRS,FRSIZ,FRSI2,VWSIZ,VWSI2,CHSIZ,CHSI2,
C     +   GPSIZ,PROTN,ERROR)
C      disp_acq=gpr_$acquire_display(st)
C      call gpr_$clear(int4(0),st)
C      call gpr_$force_release(ic,st)
C      disp_acq=.false.

C
C Set values in random order
      DO 10 I=1,256
   10 IB2(I)=0
      IT=0
      DO 20 I=1,256
         IT=MOD(IT,256)
         IB1(I)=IT
   20 IT=IT+11
      WRITE (TERM2,30) IB1
   30 FORMAT (/' Test data prepared as INTEGER values'/(1X,16i4))
C
C Copy to B2 via CFORM
      WRITE (TERM2,40)
   40 FORMAT (/' calling cform(ib1,ib2,1,1,256)')
      it1=1
      CALL CFORM(IB1,IB2,it1,it1,I4256)
      CALL CFVER(IB2,TERM2)
C
C Pass through large number of conversion in turn,
      print *,'-----------------------------'
      OLDF=1
      ix=0
      iy=0
      bl=0.
      wh=200.
      DO 60 N=1,NFORMS
         NEWF=FORMS(N)
         WRITE (TERM2,50) OLDF,NEWF
50       FORMAT (' calling cform(b2,b2,',i1,',',i1,',256)')
c         call write_out(ib2,oldf)
         CALL CFORM(IB2,IB2,OLDF,NEWF,I4256)
c
C         do 80 i=1,60
C           it1=1
C            FSRET=FSRO61(ib2,it1,256,it1,NEWF,
C     $            IX,IY,it1,BL,WH,0,ERROR)
c           fsret=fsri61(row2,256,newf,ix,iy,
c     $           frame,bl,wh,0,error)
c                 
C            iy=iy+1
C80       continue   
         iy=iy+4
c         write (6,85)
c85       format('-----------------------'/)
C         call write_out(ib2,newf)
         IF (NEWF.EQ.1) CALL CFVER(IB2,TERM2)
         if(n.eq.8)then
            ix=ix+256
            iy=0
         endif
c         ix=ix+256 
60    OLDF=NEWF
C
      WRITE (TERM2,70)
70    FORMAT (/' Test completed without errors')
C      call gpr_$force_release(ic,st)
C      disp_acq=.false.
C      pause
      STOP
C
C Copyright (C) 1987:  Synoptics Ltd,  All Rights Reserved
C
      END
C Subroutine for CFORM tester - checks current array contents
      SUBROUTINE CFVER(N,TERM2)
      INTEGER*2 N(256),TERM2
      IT=0
      DO 20 I=1,256
      IT=MOD(IT,256)
      IF (N(I).NE.IT) THEN
         WRITE (TERM2,10) I,N
   10    FORMAT (' Data mismatch found beginning at pixel ',I3
     +   /(1X,16I4))
         STOP
         ENDIF
   20 IT=IT+11
      WRITE (TERM2,30)
   30 FORMAT (' .. data confirmed correct at this point')
      RETURN
      END
c
c Subroutine for writing out array contents
C      subroutine write_out(m,kk)
C      character*1 m(*)
C      integer*4   point0,point1,point2,point3
C      character*1 a0(256)
C      integer*2   a1(256)
C      real*4      a2(256)
C      complex     a3(256)
C      pointer     /point0/a0
C      pointer     /point1/a1
C      pointer     /point2/a2
C      pointer     /point3/a3
C      integer*2   kk
C      if(kk.eq.0)then       
C         point0=iaddr(m)
Cc         do 5 i=1,256
Cc            a1(i)=ichar(a0(i))
Cc5        continue
C         write (6,10)a0
C      else if(kk.eq.1)then
C              point1=iaddr(m)
C              write (6,20)a1
C      else if(kk.eq.2)then  
C              point2=iaddr(m)
C              write (6,*)a2
C      else if(kk.eq.3)then  
C              point3=iaddr(m)
C              write (6,*)a3
C      endif
C10    format (/(2X,16a1))
C20    format (/(2x,16i6))
C30    format (/(2x,16f8.2))
C40    format (/(2x,16g8.2))
C      return
C      end
C

