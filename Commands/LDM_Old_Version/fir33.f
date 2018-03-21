C Semper 6 subsidiary module FIR33
C
      SUBROUTINE FIR33(B,PO,P1,P2,P3,L,K)
C
C Applies 3 by 3 filter defined by K to the rows P1,P2,P3
C placing result in PO; Px are buffers pointers wrt B
C
      REAL B(*),K(3,3),C11,C21,C31,C12,C22,C32,C13,C23,C33
      INTEGER L
      INTEGER*4 PO,P1,P2,P3,LASTO,I1,I2,I3,IO
C
C Fetch coefficients to local variables for efficiency
C
      C11=K(1,1)
      C21=K(2,1)
      C31=K(3,1)
      C12=K(1,2)
      C22=K(2,2)
      C32=K(3,2)
      C13=K(1,3)
      C23=K(2,3)
      C33=K(3,3)
C
C Pass along row
C
      LASTO=PO+L-1
      I1=P1
      I2=P2
      I3=P3
      DO 10 IO=PO,LASTO
         B(IO)=C33*B(I1-1) + C23*B(I1) + C13*B(I1+1) +
     +         C32*B(I2-1) + C22*B(I2) + C12*B(I2+1) +
     +         C31*B(I3-1) + C21*B(I3) + C11*B(I3+1)
         I1=I1+1
         I2=I2+1
         I3=I3+1
   10 CONTINUE
      RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
