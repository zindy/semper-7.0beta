C Semper 6 subsidiary module FIR55
C
      SUBROUTINE FIR55(B,PO,P1,P2,P3,P4,P5,L,K)
C
C Applies 5 by 5 filter defined by K to the rows in B1/2/3/4/5,
C placing result in B1; edge pixels of result copied outwards
C
      REAL B(*),K(25),C(25),C11,C12,C13,C14,C15,C21,C22,C23,C24
      REAL C25,C31,C32,C33,C34,C35,C41,C42,C43,C44,C45,C51,C52,C53
      REAL C54,C55
      INTEGER I,L
      INTEGER*4 PO,P1,P2,P3,P4,P5,LASTO,I1,I2,I3,I4,I5,IO
C
      EQUIVALENCE
     +   (C(1), C11),(C(2), C21),(C(3), C31),(C(4), C41),(C(5), C51),
     +   (C(6), C12),(C(7), C22),(C(8), C32),(C(9), C42),(C(10),C52),
     +   (C(11),C13),(C(12),C23),(C(13),C33),(C(14),C43),(C(15),C53),
     +   (C(16),C14),(C(17),C24),(C(18),C34),(C(19),C44),(C(20),C54),
     +   (C(21),C15),(C(22),C25),(C(23),C35),(C(24),C45),(C(25),C55)
C
C Fetch coefficients to local variables for efficiency
C
      DO 10 I=1,25
         C(I)=K(I)
   10 CONTINUE
C
C Pass along row
C
      I1=P1
      I2=P2
      I3=P3
      I4=P4
      I5=P5
      LASTO=PO+L-1
      DO 20 IO=PO,LASTO
      B(IO)=C55*B(I1-2)+C45*B(I1-1)+C35*B(I1)+C25*B(I1+1)+C15*B(I1+2)+
     +      C54*B(I2-2)+C44*B(I2-1)+C34*B(I2)+C24*B(I2+1)+C14*B(I2+2)+
     +      C53*B(I3-2)+C43*B(I3-1)+C33*B(I3)+C23*B(I3+1)+C13*B(I3+2)+
     +      C52*B(I4-2)+C42*B(I4-1)+C32*B(I4)+C22*B(I4+1)+C12*B(I4+2)+
     +      C51*B(I5-2)+C41*B(I5-1)+C31*B(I5)+C21*B(I5+1)+C11*B(I5+2)
      I1=I1+1
      I2=I2+1
      I3=I3+1
      I4=I4+1
   20 I5=I5+1
      RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
