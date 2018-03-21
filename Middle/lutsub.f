C Semper 6 subsidiary module LUTAD2
C
      LOGICAL FUNCTION LUTAD2(NAME1,NAME2,L1,L2)
C
C Converts range specified by named keys NAME1 and NAME2 to nearest
C LUT positions.  If key SCALED is set, the range is specified with
C respect to the intensity range in the current DPD.
C
      INCLUDE 'COMMON'
C
      INTEGER NAME1,NAME2,L1,L2
C
C Functions called
C
      REAL VAL
      LOGICAL VARSET
C
C Local variables
C
      REAL XMIN,XMAX,X1,X2
      INTEGER NSCALE
C
C Packed names
C
      PARAMETER (NSCALE = 30521)
C
      LUTAD2 = .TRUE.
C
C Determine the limits of the intensity range
C
      IF (VARSET(NSCALE)) THEN
         XMIN = DPMIN
         XMAX = DPMAX
      ELSE
         XMIN = 0.0
         XMAX = REAL(LUTLEN-1)
      ENDIF
C
C Determine the range start value
C
      IF (VARSET(NAME1)) THEN
         X1 = VAL(NAME1)
      ELSE
         X1 = XMIN
      ENDIF
C
C Determine the range finish value
C
      IF (VARSET(NAME2)) THEN
         X2 = VAL(NAME2)
      ELSE
         X2 = XMAX
      ENDIF
C
C Convert values to nearest LUT positions
C
      L1 = NINT((X1-XMIN)*REAL(LUTLEN-1)/(XMAX-XMIN))
      L2 = NINT((X2-XMIN)*REAL(LUTLEN-1)/(XMAX-XMIN))
C
C Fault incorrect range values
C
      IF (L1.LT.0 .OR. L2.LT.L1 .OR. L2.GT.(LUTLEN-1)) THEN
         ERROR = 3
         IDERR = NAME1
      ELSE
         LUTAD2=.FALSE.
      ENDIF
C
      RETURN
C
C Copyright (C) 1989,1990:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module LUTADL
C
      SUBROUTINE LUTADL(VALUES,START,END,V,DV)
C
C Sets the array VALUES over the range START..END to have the values
C V,V+ ,V++,...,V+DV.  DV is not used if START=END
C
      REAL VALUES(0:*),V,DV,TV,RV
      INTEGER START,END,I
C
      IF (START.EQ.END) THEN
         VALUES(START) = V
      ELSE
         RV = DV/(END-START)
         TV = V
         DO 10 I = START,END
            VALUES(I) = TV
            TV = TV + RV
   10    CONTINUE
      ENDIF
C
      RETURN
C
C Copyright (C) 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module HSVRGB
C
      SUBROUTINE HSVRGB(INLUT,OUTLUT,START,END)
C
C Converts the HSV-encoded array in INLUT into the
C corresponding RGB array in OUTLUT over array indices START..END.
C OUTLUT array is packed in normal Semper format, i.e. all R, all G,
C then all B.
C RGB array is INTEGER, values 0..LUTMAX
C INLUT array is REAL,    values 0..1
C S and V are clipped to 0..1 before interpretation
C (H is interpreted as an angular measure, period 1)
C
      INCLUDE 'COMMON'
C
      INTEGER OUTLUT(0:*),START,END,I
      INTEGER IH,COLOUR
      REAL INLUT(0:*),H,DH,V
      REAL    RGB(0:6,3)
C                Rd  Yw  Gr  Cy  Bl  Mg  Rd
      DATA RGB / 0.0,0.0,1.0,1.0,1.0,0.0,0.0,
     +           1.0,0.0,0.0,0.0,1.0,1.0,1.0,
     +           1.0,1.0,1.0,0.0,0.0,0.0,1.0 /
C ---------------------------------------------------------------------
      DO 20 I = START,END
C
C Determine the sector in which this hue lies, and how far in
         H = MOD(INLUT(I)*6.0,6.0)
         IF (H.LT.0.0) H = H+6.0
         IH = INT(H)
         DH = H-REAL(IH)
C
C The RGB table defined above generates the COMPLEMENTARY colour to that
C desired; the saturation of the colour then determines how far to move
C away from this.
C ---------------------------------------------------------------------
         DO 10 COLOUR = 1,3
C The amount of this (complementary) colour..
            V = (1.0-DH)*RGB(IH,COLOUR)+DH*RGB(IH+1,COLOUR)
C ..modified by the (clipped) saturation..
            V = (1.0 - MAX(0.0,MIN(1.0,INLUT(I+LUTLEN))) * V )
C ..scaled by the (clipped) brightness and LUTMAX..
            V = REAL(LUTMAX) * MAX(0.0,MIN(1.0,INLUT(I+LUTLEN*2))) * V
C ..and finally converted to the output form
            OUTLUT(I+LUTLEN*(COLOUR-1)) = NINT(V)
   10    CONTINUE
C
   20 CONTINUE
C
      RETURN
C
C Copyright (C) 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module LUTADV
C
      SUBROUTINE LUTADV(INLUT,OUTLUT,START,END)
C
C Converts the brightness array in INLUT into the corresponding RGB
C array in OUTLUT over array indices START..END.
C RGB array is INTEGER, values 0..LUTMAX
C INLUT array is REAL,    values 0..1
C V is clipped to 0..1 before interpretation
C
      INCLUDE 'COMMON'
C
      INTEGER OUTLUT(0:*),START,END,I
      REAL INLUT(0:*)
C ---------------------------------------------------------------------
      DO 10 I = START,END
         OUTLUT(I) = MAX(0,MIN(LUTMAX,NINT(REAL(LUTMAX)*INLUT(I))))
   10 CONTINUE
C
      RETURN
C
C Copyright (C) 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
