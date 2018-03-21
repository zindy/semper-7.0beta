C Semper 6 sub module: DETER
C------------------------------------------------------------------
C DETER:
C
C Calculate the determinant of the LU decomposed array. This is
C trivial and consists of the product of the leading diagonal
C
C Arguments:
C   A        LU decomposed array
C   N        Square size of array
C
C Returns:
C   Determinant of the ORIGINAL (untransformed) matrix
C
      REAL FUNCTION DETER(A, N)
C     =========================
C
      INTEGER I, N
      REAL A(N, N)
C
      DETER = 1.0
      DO 10 I = 1, N
        DETER = DETER * A(I, I)
   10 CONTINUE
      RETURN
C
C Copyright (c) 1988,1989: Synoptics Ltd, All Rights Reserved
C
      END
C Semper 6 sub module: INVERT
C------------------------------------------------------------------
C INVERT:
C
C Calculate the inverse of the LU decomposed matrix provided. This
C is easy and consists of backsubstituting and forward-substituting
C
C Arguments:
C   A        LU decomposed source array
C   B        Matrix to contain inverse
C   N        Square size of matrices
C
      SUBROUTINE INVERT(A, B, N)
C     ==========================
C
      INTEGER H, I, J, N
      REAL A(N, N), B(N, N), SUM
C
C Set up identity matrix
C
      DO 20 I = 1, N
        DO 10 J = 1, N
          B(I, J) = 0.0
   10   CONTINUE
        B(I, I) = 1.0
   20 CONTINUE
C
C Solve for each column of the inverse matrix
C
      DO 70 H = 1, N
C
C Forward substitutions
C
        DO 40 I = 1, N
          SUM = B(I, H)
          DO 30 J = 1, (I - 1)
            SUM = SUM - A(I, J) * B(J, H)
   30     CONTINUE
          B(I, H) = SUM
   40   CONTINUE
C
C Back substitutions
C
        DO 60 I = N, 1, -1
          SUM = B(I, H)
          DO 50 J = (I + 1), N
            SUM = SUM - A(I, J) * B(J, H)
   50     CONTINUE
          B(I, H) = SUM / A(I, I)
   60   CONTINUE
   70 CONTINUE
      RETURN
C
C Copyright (c) 1988,1989: Synoptics Ltd, All Rights Reserved
C
      END
C Semper 6 sub module: CROUT
C------------------------------------------------------------------
C CROUT:
C
C Crout's algorithm for reduction of a square matrix into upper and
C lower triangular form. No pivoting performed as yet (work out how
C to do it!) so may be numerically unstable for certain cases.
C
C Arguments:
C   A      Array to be transformed, result stored here
C   N      (Square) size of matrix
C
C Returns:
C  A [ ] =
C       B...B    this row as per original matrix
C       AB..B
C       AAB.B
C       A.ABB
C       A..AB
C
C Aii = 1.0 in all cases and hence is not stored
C
      SUBROUTINE CROUT(A, N)
C     ======================
C
      INTEGER I, J, K, N
      REAL    A(N, N), SUM
C
      DO 50 J = 1, N
C
C First calculate all beta values on this row
C
        DO 20 I = 1, J
          IF (J .GT. 1) THEN
            SUM = A(I, J)
            DO 10 K = 1, (I - 1)
              SUM = SUM - A(I, K) * A(K, J)
   10       CONTINUE
            A(I, J) = SUM
          ENDIF
   20   CONTINUE
C
C Now for the alpha values
C
        DO 40 I = (J + 1), N
          SUM = A(I, J)
          IF (J .GT. 1) THEN
            DO 30 K = 1, (J - 1)
              SUM = SUM - A(I, K) * A(K, J)
   30       CONTINUE
          ENDIF
          A(I, J) = SUM / A(J, J)
   40   CONTINUE
   50 CONTINUE
      RETURN
C
C Copyright (c) 1988,1989: Synoptics Ltd, All Rights Reserved
C
      END
