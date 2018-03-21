C
C  Copyright (c) 2005 L. D. Marks
C
C  This program is free software; you can redistribute it and/or modify
C  it under the terms of the GNU General Public License as published by
C  the Free Software Foundation; either version 2 of the License, or
C  (at your option) any later version.
C
C  This program is distributed in the hope that it will be useful,
C  but WITHOUT ANY WARRANTY; without even the implied warranty of
C  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C  GNU General Public License for more details.
C
      SUBROUTINE EXLDM
C
C  Syntax:                                  
C  Exldm :EXLDM  angle= sixth quartic position= po2= sam= $1=sel from=$1 $2=fro to=$2 

C
C Performs an angle extraction/interpolation using higher-order Lagrangians
C Currently implimented only for real pictures
C Options:
C       Default - 6-point Interpolation
C       Quartic - 4x4 Lagrangian Interpolation
C       Sixth   - 6x6 Lagrangian Interpolation
C Keys:
C       Sampling - Standard Semper Sampling as used in ext
C       Angle    - Angle to rotate by
C       Pos,po2  - Origin for the rotation
C
C Note:
C       At the current moment the output picture is the same size as the
C       input.
C
      LOGICAL SEMOPN,semrow
      real buf(2048,2048)
      common /big_buf/buf
C
      INCLUDE 'COMMON'
C
C Packed names
      PARAMETER (NTO=-601,nquart=28041)
      PARAMETER (NANGLE=2167,nfrom=10335,nsixth=30784)
      parameter (nsam=30453, npos2=26232, npos=26219)
      real*8 p,q,t0,t1,t2,t3,t4,t5,t6,r1,r2,r3,r4,r5,r6
      logical quartr,opt,sixth
C
      np0=ivalpn(nfrom)
      np2=ivalpn(nto)
C Open input picture
         IF (SEMOPN(1,np0,NCOL,NROW,nlay,iclass,iform,LP1)) RETURN
C Fault multi-layer source picture
      IF (NLAYS(LP1).NE.1) THEN
         ERROR=62
         IDERR=VERB
         RETURN
      ENDIF
C
c Open output picture
         LP2=LP1
         IF (SEMOPN(2,np2,NCOL,NROW,1,iclass,iform,LP2)) goto 100
C
C Fault re-use of source picture disc space for output
         IF (LP2.EQ.LP1) THEN
            ERROR=59
            goto 100
         ENDIF
      the=val(nangle)
c     quartic bi-Lagrangian interpolation
      quartr=opt(nquart)
c     sixth-order bi-Lagrangian interpolation
      sixth=opt(nsixth)
      if(sixth.and.quartr)then
C           write(6,*)'Using sixth-roder mode, not quartic'
           quartr=.false.
      endif
C
c     read everything from the input picture
      do j=1,nrow
           if(semrow(1,rb1,iform,j,1,LP1)) goto 100
           do i=1,ncol
                buf(i,j)=rb1(i)
           enddo
      enddo

C Extract pixel values for each block of output rows in turn
C Here X0, Y0 are the origin in terms of the original picture
C sam is the sampling requested (u,v not currently implimented)
        sam=val(nsam)
        X0=val(npos)
        Y0=val(npos2)
        write(6,*)sam,x0,y0
        if(sam.gt.1e-3)then
                sthe=sin(the)*sam
                cthe=cos(the)*sam
                X0=X0/sam
                Y0=Y0/sam
        else
                sthe=sin(the)
                cthe=cos(the)
                sam=1.0
        endif
        
      nr0=nrow/2+1-Y0
      nc0=ncol/2+1+X0
      DO 40 J=1,NROW
C
           y=j-nr0
           ys=y*sthe
           yc=y*cthe
            DO 10 I=1,NCOL
C
           x=i-nc0
           Xn=x*cthe+ys+nc0
           Yn=-x*sthe+yc+nr0
C
c     The guts ... the interpolation
           ix=int(Xn)
           iy=int(Yn)
           ix1=ix+1
           iy1=iy+1
           ixm=ix-1
           iym=iy-1
           if(quartr.or.sixth)then
               ix2=ix+2
               iy2=iy+2
               if(ix2.gt.ncol)ix2=ix2-ncol
               if(ix2.lt.1   )ix2=ix2+ncol
               if(iy2.gt.nrow)iy2=iy2-nrow
               if(iy2.lt.1   )iy2=iy2+nrow
           endif
           if(sixth)then
               ix3=ix+3
               iy3=iy+3
               if(ix3.gt.ncol)ix3=ix3-ncol
               if(ix3.lt.1   )ix3=ix3+ncol
               if(iy3.gt.nrow)iy3=iy3-nrow
               if(iy3.lt.1   )iy3=iy3+nrow
               ixn=ix-3
               iyn=iy-3
               if(ixn.gt.ncol)ixn=ixn-ncol
               if(ixn.lt.1   )ixn=ixn+ncol
               if(iyn.gt.nrow)iyn=iyn-nrow
               if(iyn.lt.1   )iyn=iyn+nrow
           endif
           p=Xn-ix
           q=Yn-iy
           if(ix.gt.ncol)ix=ix-ncol
           if(ix.lt.1   )ix=ix+ncol
           if(iy.gt.nrow)iy=iy-nrow
           if(iy.lt.1   )iy=iy+nrow
           if(ix1.gt.ncol)ix1=ix1-ncol
           if(ix1.lt.1   )ix1=ix1+ncol
           if(iy1.gt.nrow)iy1=iy1-nrow
           if(iy1.lt.1   )iy1=iy1+nrow
           if(ixm.gt.ncol)ixm=ixm-ncol
           if(ixm.lt.1   )ixm=ixm+ncol
           if(iym.gt.nrow)iym=iym-nrow
           if(iym.lt.1   )iym=iym+nrow

C     write(6,*)ix,iy,ix1,iy1,ixm,iym
      if(quartr)then
c     Sixteen-point interpolation
c     Interpolation along the x-direction
           t1=-p*(p-1.d0)*(p-2.d0)/6.0d0
           t2=(p*p-1.0d0)*(p-2.0d0)*0.5d0
           t3=-p*(1.0d0+p)*(p-2.0d0)*0.5d0
           t4=p*(p*p-1.0d0)/6.0d0
           r1=t1*buf(ixm,iym)+t2*buf(ix,iym)+t3*buf(ix1,iym)
     $        +t4*buf(ix2,iym)
           r2=t1*buf(ixm,iy)+t2*buf(ix,iy)+t3*buf(ix1,iy)
     $        +t4*buf(ix2,iy)
           r3=t1*buf(ixm,iy1)+t2*buf(ix,iy1)+t3*buf(ix1,iy1)
     $        +t4*buf(ix2,iy1)
           r4=t1*buf(ixm,iy2)+t2*buf(ix,iy2)+t3*buf(ix1,iy2)
     $        +t4*buf(ix2,iy2)
c     Interpolation along the y-direction
           t1=-q*(q-1.d0)*(q-2.d0)/6.0d0
           t2=(q*q-1.0d0)*(q-2.0d0)*0.5d0
           t3=-q*(1.0d0+q)*(q-2.0d0)*0.5d0
           t4=q*(q*q-1.0d0)/6.0d0
           rb2(i)=t1*r1+t2*r2+t3*r3+t4*r4
      else if (sixth) then
           t1=-p*(p*p-1.d0)*(p-2.d0)*(p-3.d0)/120.d0
           t2= p*(p-1.d0)*(p*p-4.d0)*(p-3.d0)/24.d0
           t3=-(p*p-1.d0)*(p*p-4.d0)*(p-3.d0)/12.d0
           t4= p*(p+1.d0)*(p*p-4.d0)*(p-3.d0)/12.d0
           t5=-p*(p*p-1.d0)*(p+2.d0)*(p-3.d0)/24.d0
           t6= p*(p*p-1.d0)*(p*p-4.d0)/120.d0
           r1=buf(ixn,iyn)*t1+buf(ixm,iyn)*t2+buf(ix,iyn)*t3+
     $        buf(ix1,iyn)*t4  + buf(ix2,iyn)*t5+buf(ix3,iyn)*t6
           r2=buf(ixn,iym)*t1+buf(ixm,iym)*t2+buf(ix,iym)*t3+
     $        buf(ix1,iym)*t4  + buf(ix2,iym)*t5+buf(ix3,iym)*t6
           r3=buf(ixn,iy )*t1+buf(ixm,iy )*t2+buf(ix,iy )*t3+
     $        buf(ix1,iy )*t4  + buf(ix2,iy )*t5+buf(ix3,iy )*t6
           r4=buf(ixn,iy1)*t1+buf(ixm,iy1)*t2+buf(ix,iy1)*t3+
     $        buf(ix1,iy1)*t4  + buf(ix2,iy1)*t5+buf(ix3,iy1)*t6
           r5=buf(ixn,iy2)*t1+buf(ixm,iy2)*t2+buf(ix,iy2)*t3+
     $        buf(ix1,iy2)*t4  + buf(ix2,iy2)*t5+buf(ix3,iy2)*t6
           r6=buf(ixn,iy3)*t1+buf(ixm,iy3)*t2+buf(ix,iy3)*t3+
     $        buf(ix1,iy3)*t4  + buf(ix2,iy3)*t5+buf(ix3,iy3)*t6
           t1=-q*(q*q-1.d0)*(q-2.d0)*(q-3.d0)/120.d0
           t2= q*(q-1.d0)*(q*q-4.d0)*(q-3.d0)/24.d0
           t3=-(q*q-1.d0)*(q*q-4.d0)*(q-3.d0)/12.d0
           t4= q*(q+1.d0)*(q*q-4.d0)*(q-3.d0)/12.d0
           t5=-q*(q*q-1.d0)*(q+2.d0)*(q-3.d0)/24.d0
           t6= q*(q*q-1.d0)*(q*q-4.d0)/120.d0
           rb2(i)=t1*r1+t2*r2+t3*r3+t4*r4+t5*r5+t6*r6
      else
c     Six-point interpolation
           t1=q*(q-1.0d0)*0.5d0
           t2=p*(p-1.0d0)*0.5d0
           t3=(1.d0+p*q-p*p-q*q)
           t4=p*(p-2.0d0*q+1.d0)*0.5
           t5=q*(q-2.d0*p+1.d0)*0.5
           t6=p*q
           rb2(i)=t1*buf(ix,iym)+t2*buf(ixm,iy)
     $  + t3*buf(ix,iy)  + t4*buf(ix1,iy)
     $  + t5*buf(ix,iy1) + t6*buf(ix1,iy1)
      endif
10    continue
C
      IF (SEMROW(2,RB2,iform,J,1,LP2)) goto 100
40    continue
C
100   RETURN
      END

