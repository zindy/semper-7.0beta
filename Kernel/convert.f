C     Conversion function integer to integer*4
C
      INTEGER*4 FUNCTION LONGS ( IDAT )
C     ================================
C
      INTEGER  IDAT
C
      LONGS = IDAT
      RETURN
      END
C     Conversion function integer*4 to integer
C
      INTEGER FUNCTION SHORTS ( IDAT )
C     =================================
C
      INTEGER*4 IDAT
C
      SHORTS = IDAT
      RETURN
      END
