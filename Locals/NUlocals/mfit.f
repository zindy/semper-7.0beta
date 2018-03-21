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
      subroutine mfit
c
c     Solves an equation of the form A X = B
c     where A is a matrix MxN and B is given of length M
c     The solution of length N is returned in X (TO)
c     LP1 = Matrix A (sel)
c     LP2 = Result (to)
c     LP3 = vector B (with)
c     mfit :MFIT $1=sel from=$1 open(lp1,old)=$1 to=$2 open(lp3,old)=wit
c
c     The code uses the lapack routine SGELS, which is O.K.  The
c     lapack software can be obtained via netlib (send mail to netlib@@ornl.gov
c     or netlib@nac.no in Europe, with only the message "send index" or
c     via ftpanon to netlib2.cs.utk.edu. 128.169.92.17

      INCLUDE 'COMMON'
      integer*4 info,lda,ldb,m,n,lwork,nrhs
      parameter (lda=30, ldb=40, lwork=200, nrhs=1)
      dimension A( LDA, lda ), B( LDB, nrhs ), WORK( LWORK )
      LOGICAL SEMOPN,SEMROW,semcen
c     Lengths M,N
c
c	JUNK out for the moment
c	ijunk=-1
c	error=161
c	write(6,*)'Verb not implimented yet'
c	if(ijunk.lt.0)return
      NCOL=NCOLS(LP1)
      M=NCOL
c     check sizes O.K.
      NROW=NROWS(LP1)
      N=NROW
      M2=NCOLS(LP3)
      if(M.gt.ldb)goto 1000
      if(n.gt.lda)goto 2000
      if(m2.ne.ncol)goto 4000

c     check forms etc
      inform=FORMN(LP1)
      iwform=formn(lp3)
      if(inform.ne.2.or.iwform.ne.2)goto 5000

c     open output
      NPIC=IVALPN(IPACK('TO'))
      iCLASS=NCLIMA
      iform=2
      if(semopn(2,npic,nrow,1,1,iclass,iform,lp2))return
C     write(6,*)'Opened'
      if(semcen(lp2,1,1,1))return
C     write(6,*)'Centered'

c     Sloppy read of martix
      DO J=1,NROW
      IF (SEMROW(1,RB1,IFORM,J,1,LP1)) RETURN
C     write(6,*)'Getting row ',j
         do k=1,NCOL
             A(k,j)=rb1(k)
         enddo
      enddo

c     Sloppy read of vector
      IF (SEMROW(1,RB1,IFORM,1,1,LP3)) RETURN
C     write(6,*)'Got second picture'
      do k=1,ncol
           B(k,1)=rb1(k)
      enddo

C     Call lapack routine to do all the work
      call SGELS( 'N', M, N, NRHS, A, LDA, B, LDB, WORK, LWORK,
     $                  INFO )

c     Did it work (info=0)
      if(info.lt.0)goto 3000


C Store result in LP3
      do j=1,nrow
           rb1(j)=b(j,1)
      enddo
      IF (SEMROW(2,RB1,iform,1,1,LP2)) RETURN
C     write(6,*)'Done'

      return
c     Note: it might not be a bad idea to store the R.M.S. error and
c     even work out error-bars.

c     Errors
1000  write(6,*)'Error: Too many values'
      error=167
      return
2000  write(6,*)'Error: Too many variables'
      error=167
      return
3000  write(6,*)'Error: Parameter ',-info,' was bad'
      error=167
4000  write(6,*)'Error: Inconsistent sizes' 
      error=167
      return
5000  write(6,*)'Error: Input must be FP'
      error=167
      return
      end
