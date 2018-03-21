C Semper 6 system module SEMCPW
C
      LOGICAL FUNCTION SEMCPW(SERNUM)
C
      CHARACTER*20 SERNUM

C "Special" password checking module with minimal actual checking
C of passwords...

      SEMCPW=.FALSE.
C
      RETURN
      IDUMMY = ICHAR(SERNUM(1:1))
      END
