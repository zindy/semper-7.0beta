C Semper 6 system module SEMXA1
C
      LOGICAL FUNCTION SEMXA1(IOP,BUFFER,LENGTH,PTR,VALUE,IVALUE)
      INTEGER IOP,BUFFER(*),LENGTH,PTR,IVALUE
      REAL VALUE
C
C Decodes character input and produces character output,
C reading and writing names (with indices) and numbers.
C
C IOP
C  0    reads next non-space from BUFFER(PTR..) to IVALUE
C  1    reads/packs a name from BUFFER(PTR..) to IVALUE
C  2    reads a number from BUFFER(PTR..) to VALUE
C  3    writes a name from IVALUE to BUFFER(PTR..) in IC
C  4    writes a number from VALUE to BUFFER(PTR..) in IC
C  5    reduces LENGTH so as to strip trailing spaces from BUFFER
C
C Opcodes 0 to 4 return FALSE unless BUFFER overflows before item
C can be transferred; opcode 5 returns FALSE unless LENGTH reaches zero.
C
C Opcode 0 advances PTR up to item; others advance it beyond item
C (to LENGTH+1 if buffer overflows) - except that opcode 5 ignores it.
C
      LOGICAL SEMLU,SEMDIA
C
      INCLUDE 'COMMON'
C
      REAL RADIX,SIGN,THRESH,V
      INTEGER S(4),CH,EX,COL,CNV,FLD,INPTR,I,K,IK,IP,IV,RADCH
      LOGICAL EXPON,INDX,FRPT,NST,BINARY,HEX,OCTAL,VALID
C
      SEMXA1 = .FALSE.
C
      IF (IOP .EQ. 3) THEN
         GOTO 110
      ELSE IF (IOP .EQ. 4) THEN
         GOTO 130
      ELSE IF (IOP .EQ. 5) THEN
         GOTO 210
      ELSE IF (IOP .LT. 0 .OR. IOP.GT.5) THEN
         IF (SEMDIA('*** WARNING ***',NDIERR)) GOTO 220
         IF (SEMDIA('Opcodes 6 to 9 in SEMXA1 are no longer supported',
     +      NDIERR)) GOTO 220
         IF (SEMDIA('See Fortran Programmers'' Guide for alternatives',
     +      NDIERR)) GOTO 220
      ENDIF
C
C Input modes - check buffer length and ignore spaces
C
   10 VALUE = 0.
      IVALUE = 0
      IF (PTR .GT. LENGTH) GOTO 230
      I = BUFFER(PTR)
      IF (I .EQ. KSPACE) THEN
         PTR = PTR + 1
         GOTO 10
      ENDIF
C
C Non-space found: switch on IOP
C
      IF (IOP .EQ. 0) THEN
         IVALUE = I
         GOTO 220
      ELSE IF (IOP .EQ. 1) THEN
C
C IOP=1: read name
C ----------------
         INDX = .FALSE.
   20    S(1) = 0
         S(2) = 0
         S(3) = 0
         K = 1
   30    CH = BUFFER(PTR)
C
C Test for alphanumeric (treating lc as uc) and map to R50
C
         IF (CH.GE.KLCA) CH = CH - (KLCA - KUCA)
C
C Alphabetic (or dollar)
C
         IF (CH.GE.KUCA .AND. CH.LE.KUCZ) THEN
C
C Map alphabet
C
            CH = CH - 64
         ELSE IF (CH.EQ.KDOLLA) THEN
C
C Map dollar
C
            CH = 27
         ELSE
C
C Check for digits (still require alphabetic in first position)
C
            IF (CH.LT.KZERO .OR. CH.GT.KNINE .OR. K.EQ.1) GOTO 40
C
C Map digits
C
            CH = CH - 18
         ENDIF
C
C Ignore characters after 3rd
C
         IF (K.LE.3) THEN
            S(K) = CH
            K = K + 1
         ENDIF
         PTR = PTR + 1
         IF (PTR.LE.LENGTH) GOTO 30
C
C Assemble in pseudo-R50 packing
C
   40    NST = S(1).GE.20
         IF (NST) S(1) = S(1)-20
         IVALUE = (S(1)*40 + S(2))*40 + S(3)
         IF (NST) IVALUE = -IVALUE-1
         IF (.NOT.INDX) THEN
            IF (PTR .GT. LENGTH) GOTO 220
            IF (BUFFER(PTR) .NE. KHASH) GOTO 220
C
C Read index name
C
            IF (K.EQ.1 .OR. K.GT.3) GOTO 240
            IV = IVALUE
            IK = K
            PTR = PTR + 1
            IF (PTR.GT.LENGTH) GOTO 240
            IP = PTR
            INDX = .TRUE.
            GOTO 20
         ENDIF
C
C Obtain index value
C
         IF (IP.EQ.PTR) GOTO 240
         IF (.NOT.SEMLU(-1,IVALUE,V)) THEN
C
C Subscript variable unset
C
            ERROR = 25
            GOTO 250
         ENDIF
         I = V
         IF (I.LT.0 .OR. I.GT.99) THEN
C
C Subscript out of range
C
            ERROR = 3
            GOTO 250
         ENDIF
C
C Add index character to name
C
         K = I/10
         IF (IK.NE.3) THEN
            IF (K.EQ.0) THEN
               I = I*40 + 1200
            ELSE
               I = K*30 + I + 1230
            ENDIF
         ELSE
            IF (K.EQ.0) K=I
            I = K+30
         ENDIF
         IF (IV.LT.0) I=-I
         IVALUE = IV + I
         GOTO 220
      ELSE IF (IOP .EQ. 2) THEN
C
C IOP=2: read number
C ------------------
         INPTR = PTR
         FLD = INPTR + 1
         SIGN = 1.
         RADIX = 10.
         RADCH = KNINE
         V = 0.
         EX = 0
         K = 0
         BINARY = .FALSE.
         HEX = .FALSE.
         OCTAL = .FALSE.
         EXPON = .FALSE.
         FRPT = .FALSE.
         NST = .FALSE.
         GOTO 70
C
C Next character
C
   50    NST = .TRUE.
   60    PTR = PTR+1
         IF (PTR .GT. LENGTH) GOTO 80
C
   70    CH = BUFFER(PTR)
         IF (CH .GE. KLCA .AND. CH .LE. KLCZ) CH = CH - KLCA + KUCA
C
         CNV = KZERO
         IF (HEX .AND. (CH .GE. KUCA .AND. CH .LE. KUCF)) THEN
            CNV = KUCA-10
            VALID = .TRUE.
         ELSE
            VALID = CH .GE. KZERO .AND. CH .LE. RADCH
         ENDIF
C
         IF (VALID) THEN
            IF (EXPON) THEN
C
C Exponent handling
C
               I = SIGN
               EX = EX*10 + (CH-KZERO)*I
            ELSE
               V = V*RADIX + FLOAT((CH-CNV))*SIGN
               IF (FRPT) K = K - 1
            ENDIF
            GOTO 50
         ENDIF
C
         IF (PTR .EQ. INPTR) GOTO 100
         IF (FLD .EQ. PTR .AND. V .EQ. 0.0) THEN
C
C Check for Radix changes: input field will be of form
C                 0rnnnedd
C
C      Where r is the radix (b/B=binary,o/O=octal,x/X/h/H=hex)
C            nnnn is first part (unsigned)
C         Note that no exponent is allowed with a radix change
C
            IF (.NOT.(BINARY .OR. HEX. OR. OCTAL)) THEN
               IF (CH .EQ. KUCX .OR. CH .EQ. KUCH .OR.
     +             CH .EQ. KLCX .OR. CH .EQ. KLCH) THEN
                  HEX = .TRUE.
                  RADIX = 16.
                  GOTO 60
               ELSE IF (CH .EQ. KUCB .OR. CH .EQ. KLCB) THEN
                  BINARY = .TRUE.
                  RADIX = 2.
                  RADCH = KONE
                  GOTO 60
               ELSE IF (CH .EQ. KUCO .OR. CH .EQ. KLCO) THEN
                  OCTAL = .TRUE.
                  RADIX = 8.
                  RADCH = KSEVEN
                  GOTO 60
               ENDIF
            ENDIF
         ENDIF
C
C Exponent?
C
         IF (CH .NE. KUCE) GOTO 100
C
C Already defined?
C
         IF (.NOT.(EXPON .OR. BINARY .OR. HEX .OR. OCTAL)) THEN
            SIGN = 1.
            EXPON = .TRUE.
            NST = .FALSE.
            GOTO 60
         ENDIF
C
C Terminated
C
   80    EX = EX + K
   90    IF (EX .LT. 0) THEN
            V = V/10.
            EX = EX + 1
         ELSE IF (EX .GT. 0) THEN
            V = V*10.
            EX = EX - 1
         ELSE
            VALUE = V
            GOTO 220
         ENDIF
         GOTO 90
C
C + - .
C
  100    IF (CH .EQ. KPLUS .OR. CH .EQ. KMINUS) THEN
            IF (NST) GOTO 80
            IF (CH.EQ.KMINUS) SIGN=-1.
            GOTO 60
         ENDIF
         IF (CH .NE. KDOT) GOTO 80
         IF (EXPON .OR. FRPT) GOTO 80
         FRPT = .TRUE.
         GOTO 50
      ENDIF
C
C IOP=3: write name
C -------------------
C Dismantle pseudo-R50 packing
C
  110 I = IVALUE
      NST = I.LT.0
      IF (NST) I = -I-1
      S(4) = 0
      S(2) = I/40
      S(3) = I - S(2)*40
      S(1) = S(2)/40
      S(2) = S(2) - S(1)*40
      IF (NST) S(1) = S(1)+20
C
C Map to internal code and deposit in buffer
C
      K = 1
  120 I = S(K)
C
C Exit on space
C
      IF (I .EQ. 0) GOTO 220
      IF (I .LT. 27) THEN
C
C Map alphabet (I+64 gives upper case if preferred)
C
         I = I + 96
      ELSE IF (I .GT. 27) THEN
C
C Map digits
C
         I = I + 18
      ELSE
C
C Map dollar
C
         I = 36
      ENDIF
C
C Buffer deposit routine
C
      IF (PTR .GT. LENGTH) GOTO 230
      BUFFER(PTR) = I
      PTR = PTR + 1
      K = K + 1
      GOTO 120
C
C IOP=4: write number
C ---------------------
  130 V = VALUE
  140 EX = 0
C
C Separate negative sign, and trap zero itself
C
      IF (V .LT. 0.0) THEN
         V = -V
         I = KMINUS
C
C Buffer deposit routine
C
         IF (PTR .GT. LENGTH) GOTO 230
         BUFFER(PTR) = I
         PTR = PTR + 1
      ELSE IF (V .EQ. 0.0) THEN
         GOTO 170
      ENDIF
C
C If magnitude outside range .001 to 1E6, separate the exponent
C and force back into range 1. to 9.99999
C
      SIGN = 1 000 000.
  150 IF (V .GE. SIGN) THEN
         V = V/10.
         SIGN = 10.
         EX = EX + 1
C
C Check for ridiculous values (mainly for PCs)
C
         IF (EX .GT. 999) GOTO 260
         GOTO 150
      ENDIF
      SIGN = .001
C
  160 IF (V.LT.SIGN) THEN
         V = V*10.
         SIGN = 1.
         EX = EX - 1
C
C Check for ridiculous values (mainly for PCs)
C
         IF (EX .LT. -999) GOTO 260
         GOTO 160
      ENDIF
C
C Add half of last decimal digit, to achieve conventional rounding
C
  170 THRESH = 1.
      SIGN = 100 000.
      DO 180 I = 1,8
         IF (V.GE.SIGN) GOTO 190
         SIGN = SIGN/10.
         THRESH = THRESH*10.
  180 CONTINUE
  190 CONTINUE
C
      V = V + .5/THRESH
      THRESH = 1./THRESH
C
C Encode number until remnant < THRESH; this method ensures that
C trailing zeros are retained before the decimal point AND omitted
C after it
C
      SIGN = 1 000 000.
      COL = 0
      NST = .TRUE.
  200 COL = COL + 1
      K = V/SIGN
      IF (COL .GE. 8) THEN
         IF (V .LE. THRESH) THEN
C
            IF (EX .EQ. 0) GOTO 220
            I = KLCE
C
C Buffer deposit routine
C
            IF (PTR .GT. LENGTH) GOTO 230
            BUFFER(PTR) = I
            PTR = PTR + 1
            V = EX
            GOTO 140
         ENDIF
C
         IF (COL .EQ. 8) THEN
            I = KDOT
C
C Buffer deposit routine
C
            IF (PTR .GT. LENGTH) GOTO 230
            BUFFER(PTR) = I
            PTR = PTR + 1
         ENDIF
      ENDIF
C
      IF (COL .LE. 8 .AND. COL .NE. 7) THEN
         IF (K .EQ. 0 .AND. NST) THEN
            SIGN = SIGN/10.
            GOTO 200
         ENDIF
      ENDIF
C
      I = K + KZERO
C
C Buffer deposit routine
C
      IF (PTR .GT. LENGTH) GOTO 230
      BUFFER(PTR) = I
      PTR = PTR + 1
      NST = .FALSE.
      V = V - FLOAT(K)*SIGN
      SIGN = SIGN/10.
      GOTO 200
C
C IOP=5: strip trailing spaces from BUFFER
C ----------------------------------------
  210 IF (LENGTH .LE. 0) GOTO 230
      IF (BUFFER(LENGTH) .EQ. KSPACE) THEN
         LENGTH = LENGTH - 1
         GOTO 210
      ENDIF
C
  220 RETURN
C
C Buffer exhausted
C
  230 SEMXA1 = .TRUE.
      GOTO 220
C
C Subscript syntax error
C
  240 ERROR = 17
      GOTO 220
C
C Error subscripting
C
  250 IDERR = IVALUE
      GOTO 220
C
C Ridiculous exponent - probable format error
C
  260 ERROR = 92
C
C Buffer deposit routine
C
      IF (PTR .GT. LENGTH) GOTO 230
      BUFFER(PTR) = KUCN
      PTR = PTR + 1
      IF (PTR .GT. LENGTH) GOTO 230
      BUFFER(PTR) = KLCA
      PTR = PTR + 1
      IF (PTR .GT. LENGTH) GOTO 230
      BUFFER(PTR) = KUCN
      PTR = PTR + 1
      GOTO 230
C
C Copyright (C) 1987-1990:  Synoptics Ltd,  All Rights Reserved
C
      END
