C Semper 6 subsidiary module PSOUT2
C
      SUBROUTINE PSOUT2(INSTR,OUTSTR,N)
C
C Returns the input string in the output string with all ocurrences of
C "(", ")" or "\" replaced by the valid Postscript escape sequence "\(",
C "\)" or "\\".  Any non-printing character codes are replaced by the
C escape sequence "\277" (upside-down question mark).  If this process
C exceeds the length of the output string, the excess characters are
C discarded.  The significant (non-blank) length of the output string
C is returned in N.
C
      CHARACTER*(*) INSTR,OUTSTR
      INTEGER N
C
      INCLUDE 'ICSET'
C
      CHARACTER CHA,ESCAPE
      CHARACTER*4 EXTRA
      CHARACTER*(KTILDE-KSPACE+1) VALID
      INTEGER I,J,LNOUTS
C
C Initialise return character count
      N=0
C
C Initialise string length
      LNOUTS=LEN(OUTSTR)
C
C Set up character variable containing escape character "\"
      ESCAPE=CHAR(KBACKS)
C
C Set up string containing all valid printing characters
      J=1
      DO 10 I=KSPACE,KTILDE
         VALID(J:J)=CHAR(I)
         J=J+1
   10 CONTINUE
C
C Initialise input character pointer
      I=0
C
C Extract next character from string
   20 I=I+1
      CHA=INSTR(I:I)
C
C Determine appropriate conversion for character
      IF (INDEX(VALID,CHA).EQ.0) THEN
         EXTRA(1:1)=ESCAPE
         EXTRA(2:4)='277'
         J=4
      ELSE
         IF (CHA.EQ.'('.OR.CHA.EQ.')'.OR.CHA.EQ.ESCAPE) THEN
            EXTRA(1:1)=ESCAPE
            EXTRA(2:2)=CHA
            J=2
         ELSE
            EXTRA=CHA
            J=1
         ENDIF
      ENDIF
C
C If not enough space in output string, return
      IF (N+J.LE.LNOUTS) THEN
C
C Store output characters
         OUTSTR(N+1:N+J)=EXTRA
         N=N+J
C
C If end of string not yet reached, go back for more
         IF (I.LT.LEN(INSTR)) GOTO 20
      ENDIF
C
      RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
