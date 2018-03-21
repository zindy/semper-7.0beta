        SUBROUTINE GLOPEN
* version 1.0 October 1995 - NKT 
* with modifications to attempt full opening and closing 1st December 1995
* Purpose:-
*         To provide grey-level morphology components as described in
*         Prod'homme et al. (1992)  Morphological Filtering and
*         Glanulometric Analysis on Scanning Electron Micrographs.
*         Scanning Microscopy, Supplement 6 (1992) p 255 - 268.
*
*         Allows in addition, octagonal, diamond, and true circle algorithms
*         and also includes true border option based on image reflection
* 
* Syntax:
*   GLM    :GLOPEN >$r3 radius=7 lmean square circle octagon diamond +
*              erode dilate open close border blank reflect 
*
* NOTE: Modification is needed to standard SEMPER "COMMON.FOR" file to
*       increase the number of ROW BUFFERS for PC Versions of this algorithm
*
*  The following lines must be added in the COMMON.FOR file - a convenient place
*  is immediately after the declaration for RB6
*
*      REAL RB7LHS(LNEDGE),RB7(LNBUF/LNREAL),RB7RHS(LNEDGE)
*      REAL RB8LHS(LNEDGE),RB8(LNBUF/LNREAL),RB8RHS(LNEDGE)
*
*   For Windows and SUN Workstation versions,  the row buffers are longer
*   and thus larger radii masks can be used, and there is no need to
*   specifically declare more than 6 ROW buffers
*   In these cases, 80 lines (mask radius 39 ) are possible for systems
*   with LNBUF at 16384 or 160 lines with systems with LNBUF = 32448
*

* Uses approx 7.5 row buffers- last part of rb8 is used for various variables
* to save space for PC versions
* keys:
*  FROM, TO  -    The normal input, output. Output image is RAD*2 smaller
*                 than the input image- ie no blank pixels created, but see
*                 BORDER options  
*                 Images can only be of byte or Integer Form, and sequence 
*                 is faulted if either dimension of picture is smaller than
*                 diameter of mask.
*                 Ouput picture no. OUTPIC
* RADIUS [7] -    Radius of circle/square/octagon/diamond. Total width of 
*                 mask is RAD*2+1. Only accepts a radius upto MAXLIN/2-2. 
*                 This means that maximum radius is dependant on row length 
*                 of input image.  For DOS systems,  the maximum Radius is
*                 29 at present for a 512 pixel wide image (14 for one
*                 1024 wide). 
* LMEAN           - Generates an output image which is the mean of input over
*                  the mask radius.  Equivalent to LMEAN but better
*
* options:
*
* [CIRCLE]      -  Mask is true Euclidean circle of radius RAD
* SQUARE        -  The mask used is square instead of circular, with edge
*                  length RAD*2+1 
* OCTAGON       -  The mask used is octagonal of effective radius RAD
* DIAMOND       -  The mask used ia diamond shape with maximum dimension
*                  equal to 2*RAD +1
* BORDER        -  makes o/p pic same size as input picture and fills
*                  boundary with blanks.  The default has output picture
*                  ncols(lp1)-2*rad in width
* [BLANK]       -  default if BORDER option used
* REFLECT       -  fills boundary with algorithm based on image reflection at
*                  boundary
* [ERODE]       -  does a grey level erosion
* DILATE        -  does a grey level dilation
* OPEN          -  does a grey level opening - 2 passes through algorithm
* CLOSE         _  does a grey level closing - 2 passes through algorithm
*
* The algorithm was developed on a PC version running SYNAPSE under
* SEMPER 6.4 and should thus also be suitable for SYNERGY 6.4
* The algorithm will be tested on other systems where minor modifications
* are likely because of differences in compilers in near future
*
* following is map of the last buffer rb8 (ie after edge array)
* values indicate the leftmost byte/element
* ³             º                                                             ³ 
*real:             890             1402        1790  1832   1854     ³
* 1            884  È  HIS                        side       ¼     small ³
* ³             º                                                variabls³
* ³             º                                                             ³
* ³             º                                                             ³
* ³             º                                                             ³
*byte:                                                                        ³
*57904       61440                                                            ³
* ³         end of IB                                                         ³
* ³                                                                   end of 
*                                                                      rb8 
* To economise on memory for PC systems,  the Histogram values are stored
* after the row buffers storing input data. This position is marked 
* by variable POSHIS and is exaclty the same.  POSHIS is set at 890
* as this is compatible with 3 different LNBUF values of 8192, 16384 or 32448
* position with the command DOMAIN.  Also for compatibility with DOMAIN
* variosu other variables are stored starting with POSVAR   =POSHIS+970
* Various variables use the space after HIS the start of these is
* set at 1860.R (ie the parameter M)
c       IMPLICIT NONE
%INCLUDE '../source/COMMON'
       INTEGER MAXRL,MINRAD,MAXLRL,MAXLIN,POSHIS,POSVAR,possid
       PARAMETER(MAXRL=512,MAXLIN=60,poshis=890)
*       PARAMETER(MAXRL=512,MAXLIN=80,POSHIS=890)
       PARAMETER(POSVAR=POSHIS+900,POSSID=POSHIS+900)
* cannot use part of row buffers for opb beacuse of way algo. contructed
*       INTEGER IB(MAXRL*MAXLIN),OPB(MAXRL*MAXLIN/((MINRAD*2)+2))
       INTEGER IB(MAXRL*MAXLIN),OPB(MAXRL)
       INTEGER SIDE(-MAXLIN/2:MAXLIN/2),HIS(0:255,0:1),border,bord2
       INTEGER IC,IR,GL,GR,GT,GB,BORD,NFROM,NOHCLS,border3
       INTEGER J,K,DIA,RI,TOT0,CC,GREY,JJ,err
       INTEGER RAD,OUTPIC,LP,D2,NLOUT,IVALPN
       INTEGER MXTR,IPACK,JUMP,lpin,lpout,ioclass,ibsiz
       integer*4 sum0,sum1
       REAL CONR,CONA,R,TOT2
* nb next line (RP) may need to be integer*4 if >8 buffers
       INTEGER RP(MAXLIN,MAXLIN),I,MAXPT,PT,M,iref,erotst
       PARAMETER(CONR=TWOPI/180.0,CONA=180.0/PI,NFROM=10335)
       PARAMETER(MXTR=(MAXLIN-2)/2+1,M=1860)
       LOGICAL SEMROW,SEMOPN,OSQR,OBOR,lmean,repeat
       logical ocir,ooct,odia,semcon,blank
       logical dilate,erode,open,close,nobor
*  arrays
	common /noddy/RB7LHS,RB7,RB7RHS,RB8LHS,RB8,RB8RHS

       REAL RB7LHS(LNEDGE),RB7(LNBUF/LNREAL),RB7RHS(LNEDGE)
       REAL RB8LHS(LNEDGE),RB8(LNBUF/LNREAL),RB8RHS(LNEDGE)
       EQUIVALENCE(RB1,IB)
       equivalence(noddy,ib(2*(lnedge*12+6*(lnbuf/lnreal))+1))
       EQUIVALENCE(RB1,IB)
* References to RB8 should be changed to RB6 for non-DOS systems       
       EQUIVALENCE(RB6(POSSID),SIDE),(RB6(POSHIS),HIS)
* small variables
       equivalence (RB6(POSVAR+6),R),(RB6(POSVAR+7),TOT2)
       EQUIVALENCE (RB6(posvar+8),jump),(RB6(posvar+11),NOHCLS)
       border3=0
*    border3 is used for blank borders for open and close routines
*
* RP() is the array containing row buffer pointers
*  MXTR= max radius 
* nb for a mask radius of 29 max row length=MAXRL
       nohcls=255
* repeat is used for OPEN and CLOSE options to control second pass       
       repeat=.true.
* IREF is used as a counter for reflected border option at bottom of image       
       IREF=0
       IBSIZ=1
* checks number of layers and class AND form
       IF(NLAYS(LP1).GT.1)THEN
          ERROR=62
          IDERR=IVALPN(IPACK('GLM'))
          RETURN
       ENDIF
       IF(CLASSN(LP1).NE.1)THEN
         ERROR=6
         IDERR=IVALPN(NFROM)
         RETURN
       ENDIF
       IF(FORMN(LP1).GT.1)THEN
         ERROR=43
         IDERR=IVALPN(NFROM)
         RETURN
       ENDIF
*  gets the key and option settings
      CALL glopkey(RAD,OUTPIC,lmean,OSQR,OBOR,ocir,ooct,odia,
     #        blank,dilate,erode,open,close,err)
*
       if(err.eq.1.or.err.eq.11)then
         write(6,1001)
1001  format(' Incompatible use of options CIRCLE, DIAMOND, OCTAGON, ',
     # 'SQUARE')
c         if(semcon(record))return
          ERROR=10
          return
       elseif(err.eq.10)then
         write(6,1002)
1002  format(' Incompatible use of options DILATE/ERODE ',
     # 'SQUARE')
c         if(semcon(record))return
          ERROR=10
          return
      endif
       MAXLRL=MAXLIN*MAXRL/((RAD*2)+2)
* check input image is not greater than SIZE of buffer
       IF(NCOLS(LP1).GT.MAXLRL)THEN
          ERROR=3
*          IDERR=IVALPN(NFROM)
           iderr=ipack('RAD')
           RETURN
       ENDIF
*
*     checks the radius does not exceed size of arrays
       IF(RAD.GT.MXTR)THEN
          ERROR=3
          IDERR=IPACK('RAD')
          RETURN
       ENDIF
* checks the radius size does not exceeed size of picture
!       IF(RAD*2+2.GT.NROWS(LP1).OR.RAD*2+2.GT.NCOLS(LP1))THEN
       IF(RAD*2+2.GT.NCOLS(LP1))THEN
         ERROR=5
         IDERR=IVALPN(NFROM)
         RETURN
       ENDIF
* opens the output picture
       LP2=LP1
       if(.not.open.and..not.close)repeat=.false.
       if((open.or.close).and.repeat)then
          ioclass=3
          lpin=lp1
          lpout=lp2
       elseif(open.or.close)then
          ioclass=2
          lpin=lp2
          lpout=lp3
       else
          ioclass=2
          lpout=lp2
          lpin=lp1
       endif
       if((open.or.close).and. repeat.and.blank)then
          nobor=.true.
       else
          nobor = .false.
       endif
       IF(OBOR.and..not.nobor)THEN
             BORD=RAD+1
             IF(SEMOPN(ioclass,OUTPIC,NCOLS(LPIN),NROWS(LPIN),
     +       1,1,0,LPOUT))RETURN
       ELSE
         BORD=1
       IF(SEMOPN(ioclass,OUTPIC,NCOLS(LPIN)-2*RAD,NROWS(LPIN)-2*RAD,
     +      1,1,0,LPout))RETURN
       ENDIF
*  
* calculate the value of the row buffer pointers      
      D2=2*RAD+2
      MAXPT=D2*MAXLRL
      DO 2 K=1,D2
         PT=(K-1)*MAXLRL
         DO 3 I=1,D2
           IF(PT.EQ.MAXPT)THEN
             PT=0
           ENDIF
           RP(I,K)=PT
           PT=PT+MAXLRL
3       CONTINUE
2     CONTINUE
* Calculate the offset positions to the mask sides
*===================next section replaced
      IF(OSQR)THEN 
        DO 4 K=-RAD,RAD
4          SIDE(K)=RAD
      ELSEif(Ocir)then
        k=0
        side(0)=rad
        do 10 i=-rad,-1
           do 11 j = k,-rad,-1 
              if(nint(sqrt(float(i**2+j**2))).le.rad)then
                 side(i)=abs(j)
                 side(-i)=abs(j)
              else
                 go to 112
              endif
11         continue
112         k=side(i)

10      CONTINUE
      elseif(odia)then
         side(0)=rad
         do 130 i=-rad,-1
            side(i)=rad-abs(i)
            side(-i)=rad-abs(i)
130       continue
      elseif(ooct)then
         side(0)=rad
         side(-rad)=nint(rad*tan(3.1415926/12))      
         side(rad)=side(-rad)
         do 140 j=1,side(rad)
            side(j)=rad
            side(-j)=rad
140       continue
         k=1   
         do 141 j=side(rad)+1,rad-1
            side(j)=rad-k
            side(-j)=rad-k
            k=k+1
141       continue
      ENDIF
*            END OF NEW SECTION
* =========================================
* read in initial lines for start of main calculation
142      IF(OBOR.and..not.blank)THEN
         border=rad
         bord2=0
*  new section for true border fill
*  read in top RAD + 1 lines and reflect
*  line 1 of image is stored effectively as rad+1 line in array
      do 330 k=1,rad+1
            IF(SEMROW(1,IB(RP(K+rad,1)+1),NFMINT,K,1,LPIN))RETURN
*  now repeat to reflect
            IF(SEMROW(1,IB(RP(K,1)+1),NFMINT,rad+2-k,1,LPIN))RETURN
330     CONTINUE
*  Now read in one more line because of way Mark originally coded algorithm
            IF(SEMROW(1,IB(RP(2*rad+2,1)+1),NFMINT,rad+2,1,LPIN))RETURN
       do 430 j=1,ibsiz*rad
          opb(j)=0
          opb(ncols(lpin)-j+1)=0
430    continue
          DIA=RAD*2+1
          D2=DIA+1
      elseif(obor.and.blank.and..not.nobor)then
*  original blank border routine
*  output top blank border if wanted
         border=0
         bord2=0
         DO 7 IR=1,ibsiz*RAD
           DO 8 K=1,NCOLS(LPout)
             OPB(K)=0
8          CONTINUE
           IF(SEMROW(2,OPB,NFMINT,IR,1,LPOUT))RETURN
7        CONTINUE
      endif
* now START calculation +++++++++++++++++++++++++++
      if((obor.and.blank).or..not.obor)then    
          border=0
          bord2=0
*   read in the first DIA+1 lines- extra line at bottom because of
*  way loops are constructed
          DIA=RAD*2+1
          D2=DIA+1
          DO 30 K=1,DIA+1
             IF(SEMROW(1,IB(RP(K,1)+1),NFMINT,K,1,LPIN))RETURN
30        CONTINUE
*
      endif
      CC=RAD+1
      NLOUT=NROWS(LPIN)-CC
      RI=1
*
*  -----------------------------------------------------------------
*  start main calculation
* -------------------------------------------------------------------
* zero the histogram array
900       DO 9 K=0,NOHCLS
        DO 9 I=0,1
          HIS(K,I)=0
9      CONTINUE
*
*  calculate initial frequencies
*  CC-   Start column     
*  RI- counter for the row pointer/changes when read in a line
*  HIS(n,0) contains working histogram
*  HIS(n,1) contains histogram for 1st point of each row
*  sum0 contains working sum of intensities in mask area
*  sum1 contains sum of intensities for 1st point of each row 
*
       sum0=0
       sum1=0
       TOT0=0
       JJ=-(RAD+1)
       DO 200 K=1,DIA
          JJ=JJ+1
       DO 201 I=CC-SIDE(JJ)-border,
     #        CC+SIDE(JJ)-border
* array error for reflect option         
          j=i
          if(j.lt.1)j=-j+2
         GREY=IB(RP(K,RI)+j)
         if(.not.lmean)then
            HIS(GREY,0)=HIS(GREY,0)+1
            HIS(GREY,1)=HIS(GREY,0)
         else
            sum0=sum0+nint(float(grey))
            tot0=tot0+1
         endif
         sum1=sum0
201    CONTINUE
200   CONTINUE
* do test on value, and put into array OPB
      if(lmean)then
         opb(bord-border)=nint(float(sum0)/float(tot0))
      else
         opb(bord-border+border3)=erotst(his,erode,dilate,nohcls)
      endif
*
*  start of outer loop working through lines
*  next line modified to include border
      DO 300 IR=RAD+1-BORDER,NROWS(LPIN)-RAD+border
*  inner loop working along lines
        DO 301 IC=CC-border,
     #        NCOLS(LPIN)-CC+border
* subtract trailing edge and add leading edge at same time
          DO 302 K=-RAD,RAD
            LP=K+CC
            j=ic-side(k)  
            if(j.lt.1)j=-j+2
            i=ic+side(k)+1
            if(i.gt.ncols(lpin))i=2*ncols(lpin)-i
            GL=IB(RP(LP,RI)+j)
*            GR=IB(RP(LP,RI)+(IC+SIDE(K))+1)
             GR=IB(RP(LP,RI)+i)
            if(.not.lmean)then 
               HIS(GL,0)=HIS(GL,0)-1
               HIS(GR,0)=HIS(GR,0)+1
            else
               sum0=sum0+nint(float(gr))-nint(float(gl))
            endif
302       CONTINUE
          if(lmean)then
              jj=nint(float(sum0)/float((tot0)))
          else
              jj=erotst(his,erode,dilate,nohcls)
          endif
          OPB((IC-RAD)+BORD+border3)=JJ

301     CONTINUE
*    output line
        IF(OBOR.and..not.nobor)THEN
*   this line seems correct position for output           
           IF(SEMROW(2,OPB,NFMINT,IR+(ibsiz-1)*rad,1,LPOUT))RETURN
        ELSE
           IF(SEMROW(2,OPB,NFMINT,IR-RAD,1,LPOUT))RETURN
        ENDIF
        IF(.NOT.OBOR.OR.(OBOR.AND.BLANK))THEN
           IF(IR.EQ.NROWS(LPIN)-RAD)GOTO 300
        ENDIF
* now prepare frequencies for the next line:  drop top and add bottom
        DO 303 K=-RAD,RAD
          LP=CC-SIDE(K)
          j=k+cc-border
          if(j.lt.1)j=-j+2

*          GT=IB(RP(LP,RI)+K+CC)
* next line replacs for side reflect          
          GT=IB(RP(LP,RI)+j)
          LP=CC+SIDE(K)+1
* see above
          GB=IB(RP(LP,RI)+j)
          if(.not.lmean)then
             HIS(GT,1)=HIS(GT,1)-1
             HIS(GB,1)=HIS(GB,1)+1
          else
             sum1=sum1+nint(float(gb))-nint(float(gt))
          endif
303     CONTINUE
       if(lmean)then
         sum0=sum1
         opb(bord-border)=nint(float(sum0)/float((tot0)))
       else
* transfer initial histogram to working array HIS(n,0)
        HIS(1,0)=HIS(1,1)
        DO 312 K=0,NOHCLS
312       HIS(K,0)=HIS(K,1)
*  output 1st point on next line
         opb(bord-border+border3)=erotst(his,erode,dilate,nohcls)
       endif
*  input new line UNLESS it is last line
        IF(IR.LT.NLOUT)THEN
          IF(SEMROW(1,IB(RP(1,RI)+1),NFMINT,IR+RAD+2-BORD2*ibsiz,1,LPIN)
     #         )RETURN
        elseif(obor.and..not.blank)then
          iref=iref+1
          IF(SEMROW(1,IB(RP(1,RI)+1),NFMINT,nrows(lpin)-iref,1,LPin))
     #          RETURN
        ENDIF

*  increment row pointer counter and current row pointer
        RI=RI+1
        IF(RI.GT.D2)RI=1
300   CONTINUE
      GOTO 950
* -------------------------------------------------------------------
*
950    IF(OBOR.and.blank.and..not.nobor)THEN
*  output bottom blank border if wanted
          DO 910 IR=(NROWS(LPIN)-RAD+border3)+1,
     #                  NROWS(LPIN)+(ibsiz-1)*rad+border3
             DO 911 K=1,NCOLS(LPout)
                OPB(K)=0
911          CONTINUE
             IF(SEMROW(2,OPB,NFMINT,IR,1,LPOUT))RETURN
910       CONTINUE
       ENDIF
      if(repeat.and.(open.or.close))then
         if(open)then
            if(monit.gt.0)then         
               write(6,920)
920            format( 'First Pass in Opening Sequence Complete')            
c               if(semcon(record))return
            endif
            erode=.false.
            dilate=.true.
         else
            if(monit.gt.0)then         
               write(6,921)
921            format( 'First Pass in Closing Sequence Complete')            
c               if(semcon(record))return
            endif
            erode=.true.
            dilate=.false.
         endif
         repeat=.false.
* redefine output image         
          ioclass=2
          lpin=lpout
          lp3=lp1
          lpout=lp3
          ibsiz=1
          if(nobor)then
*   for second pass with blank border, border is double radius all round             
             nobor=.false.
             border3=rad
             ibsiz=2
          endif
          IF(OBOR)THEN
             BORD=RAD+1
       IF(SEMOPN(ioclass,OUTPIC,NCOLS(LP1),NROWS(LP1),1,1,0,LPOUT))
     +       RETURN
          ELSE
             BORD=1
       IF(SEMOPN(ioclass,OUTPIC,NCOLS(LPIN)-2*RAD,NROWS(LPIN)-2*RAD,
     +      1,1,0,LPout))RETURN
          ENDIF
         
         
         go to 142
      endif
* set the range on output picture title to 0 to seg+1 OR seg
1000  RETURN
      END
*  ************************************************************
      INTEGER FUNCTION erotst(his,erode,dilate,nohcls)
*   performs the comparison to a test statistic
      INTEGER HIS(0:255,0:1),K,nohcls
      logical erode,dilate
*
      if(erode)then
        erotst=0
      elseif(dilate)then
         erotst=256
      endif
      if(erode)then
         DO 10 K=0,NOHCLS
            IF(HIS(K,0).GT.0)then
               erotst=K
               go to 1
            endif
10       continue
      else      
        do 20 k=255,0,-1
           if(his(k,0).gt.0)then
              erotst=k
              go to 1
           endif
20      continue
      endif
1     k=0
      continue
      END
* ******************************************
      subroutine glopkey(RAD,OUTPIC,lmean,OSQR,OBOR,
     # ocir,ooct,odia,blank,dilate,erode,open,close,err)
* gets the key and option settings
c      IMPLICIT NONE
%INCLUDE '../source/COMMON'
      INTEGER NRAD,NTO,OUTPIC,NSQR,NBOR,
     + nlmean,noct,ndia,ncir,nop,err,nblank,nref
      PARAMETER(NRAD=28844,NTO=-601,NSQR=31101,NBOR=3818)
      parameter(nlmean=19725)
      INTEGER RAD,IVALPN,MXSEG,TMIN,TMAX,NFROM
      PARAMETER(MXSEG=18,NFROM=10335,noct=24140,ncir=5178,ndia=6761)
      parameter(nblank=3681,nref=29006)
      integer nopen,nclose,nerode,ndil
      REAL VAL
      LOGICAL OPT,OSQR,OBOR,lmean

      logical ocir,odia,ooct,blank,reflect
      logical dilate,erode,open,close
      parameter(nerode=8735,nclose=5295,nopen=24645,ndil=6772)
*
      err=0  
      nop=0
      lmean=opt(nlmean)
      blank=opt(nblank)
      reflect=opt(nref)
      OSQR=OPT(NSQR)
      OCIR=OPT(NCIR)
      OOCT=OPT(NOCT)
      ODIA=OPT(NDIA)
      IF(.NOT.OSQR.AND..NOT.ODIA.AND..NOT.OOCT)OCIR=.TRUE.
      IF(OSQR)nop=nop+1
      if(ocir)nop=nop+1
      if(odia)nop=nop+1
      if(ooct)nop=nop+1
      if(nop.ne.1)then    
         err=1
         RETURN
      ENDIF
*  set the erode/dilate/open/close keys      
* default is [ERODE], and OPEN or CLOSE will overide DILATE or ERODE
      erode=opt(nerode)
      dilate=opt(ndil)
      close=opt(nclose)
      open=opt(nopen)
      if(.not.erode.and..not.dilate.and..not.close.and..not.open
     # .AND..NOT.LMEAN)then
        erode=.true.
      endif
      if(open)then
        dilate=.false.
        erode=.true.
      elseif(close)then
        dilate=.true.
        erode=.false.
      endif
      if(erode.and.dilate)then
         err=10*err+1
         return
      endif
* get initial values of vmin,vmax otherwise RANGE will reset
      TMIN=VMIN
      TMAX=VMAX
      OUTPIC=IVALPN(NTO)
      RAD=NINT(VAL(NRAD))
      OBOR=OPT(NBOR)
      if(blank.or.reflect)obor=.true.
      if(obor.and..not.reflect)blank=.true.
      VMIN=TMIN
      VMAX=TMAX
      RETURN
      END
* ***********************************************
