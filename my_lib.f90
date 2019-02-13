MODULE MY_LIB

  !OVERLOADING FOR THE TRISECT FUNCTION
  INTERFACE TRISECT
     MODULE PROCEDURE TRISECT, TRISECT_
  END INTERFACE TRISECT
  
  CONTAINS

    FUNCTION FTRANS(K, P, N, M) RESULT(J)
      REAL(KIND=8) :: K, P
      INTEGER :: N, M
      REAL(KIND=8) :: J

      J = 2.D0 * DCOS(K/2.D0) * DSIN(P*DBLE(N+1)+DBLE(M)*K/2.D0) + DSIN(P*DBLE(N))

      RETURN
    END FUNCTION FTRANS

    FUNCTION ENERGY(K, P, M) RESULT(J)
      REAL(KIND=8) :: K,P
      INTEGER :: M
      REAL(KIND=8) :: J

      J = DSQRT(4.D0 * DCOS(K/2.D0)**2 + 1.D0 + 4.D0 * DCOS(K/2.D0)*DCOS(P + DBLE(M)*K/2.D0))

      RETURN
    END FUNCTION ENERGY

    !TRISECT IS A SUBROUTINE THAT FINDS A ROOT OF A FUNCTION GIVEN THE INTERVAL [A,B]
    !IT PROCEEDS AS TO DIVIDE THE INTERVAL IN 3 PARTS AND CHOOSING OF THE THREE TO
    !RECURSIVELY FUNNEL THE INTERVALS INTO THE ROOTS FOR A GIVEN ACCURACY
    RECURSIVE SUBROUTINE TRISECT(F, A, B)
      REAL(KIND=8) :: F, A, B, IA, IB, SEP
      REAL(KIND=8) :: FA,FB, ERROR, DELTA

      DELTA = 1.D-15

      PRINT *, 'STARTING TRISECT ROUTINE'

      FA = F(A)
      FB = F(B)
      ERROR = B-A

      IF(ERROR .LE. DELTA) THEN
         PRINT *, 'PRECISION REACHED, EXITING ROUTINE.'
         RETURN
      END IF

      PRINT *, 'A = ,',A ,'        B= ', B
      PRINT *, 'F(A) = ,',F(A) ,'        F(B)= ', F(B)

      SEP = ERROR/3.D0
      IA = A + SEP
      IB = B - SEP

      PRINT *, 'SEPARATOR = ,',SEP
      PRINT *, 'IA = ,',IA ,'        IB= ', IB
      PRINT *, 'F(IA) = ,',F(IA) ,'        F(IB)= ', F(IB)

      IF(ABS(SIGN(1.D0,F(IA)) - SIGN(1.D0,F(A))) .GE. 1.D-15) THEN
         CALL TRISECT(F, A, IA)
      ELSE IF(ABS(SIGN(1.D0,F(IB)) - SIGN(1.D0,F(B))) .GE. 1.D-15 ) THEN
         CALL TRISECT(F, IB, B)
      ELSE
         CALL TRISECT(F,IA,IB)
      END IF

      RETURN

    END SUBROUTINE TRISECT
    
    RECURSIVE SUBROUTINE TRISECT_(F, K, N, M, LOWER, UPPER, R)
      INTEGER :: N, M
      REAL(KIND=8) :: K

      REAL(KIND=8) :: F, LOWER, UPPER, R, A, B, IA, IB, SEP
      REAL(KIND=8) :: FA, FB, ERROR, DELTA

      A = LOWER
      B = UPPER

      DELTA = 1.D-15

      PRINT *, 'STARTING TRISECT ROUTINE WITH PARAMETER:'
      PRINT '(A4,F8.5,1X,A4,I8.5,1X,A4,I8.5)', 'K = ', K, 'N = ', N, 'M = ', M
      PRINT*,

      FA = F(K,A,N,M)
      FB = F(K,B,N,M)
      ERROR = B-A

      IF(ERROR .LE. DELTA) THEN
         R = A
         PRINT *, '----------PRECISION REACHED, EXITING ROUTINE.------------'
         RETURN
      END IF

      PRINT '(A4,F18.15,1X,A4,F18.15)', 'A = ',A ,'B = ', B
      PRINT '(A7,F18.15,1X,A7,F18.15)', 'F(A) = ',F(K,A,N,M) ,'F(B)= ', F(K,B,N,M)

      SEP = ERROR/3.D0
      IA = A + SEP
      IB = B - SEP

      PRINT '(A12,F18.15)', 'SEPARATOR = ',SEP
      PRINT '(A5,F18.15,1X,A5,F18.15)', 'IA = ',IA ,'IB = ', IB
      PRINT '(A8,F18.15,1X,A8,F18.15)', 'F(IA) = ',F(K,IA,N,M) ,'F(IB)= ', F(K,IB,N,M)

      IF(ABS(SIGN(1.D0,F(K,IA,N,M)) - SIGN(1.D0,F(K,A,N,M))) .GE. 1.D-15) THEN
         PRINT *,
         CALL TRISECT(F, K, N, M, A, IA, R)
      ELSE IF(ABS(SIGN(1.D0,F(K,IB,N,M)) - SIGN(1.D0,F(K,B,N,M))) .GE. 1.D-15 ) THEN
         PRINT *,
         CALL TRISECT(F, K, N, M, IB, B, R)
      ELSE
         PRINT *,
         CALL TRISECT(F, K, N, M, IA, IB, R)
      END IF

      RETURN

    END SUBROUTINE TRISECT_
    
    SUBROUTINE WRITE_VECTOR(FILE_UNIT,X,Y)

      INTEGER,  INTENT(IN)           ::  FILE_UNIT
      REAL(KIND=8), INTENT(IN)           ::  X(:)
      REAL(KIND=8), INTENT(IN), OPTIONAL ::  Y(:)

      INTEGER:: I,NDATA

      NDATA = SIZE(X)

      PRINT *, 'WRITING TO FILE, UNIT:', FILE_UNIT
      IF (PRESENT(Y)) THEN !BOTH X AND Y ARE PRESENT, DATA ARE XY SET
         DO I = 1, NDATA
            WRITE ( FILE_UNIT, * ) X(I), Y(I)
         END DO
      ELSE !ONLY X IS PASSED, DATA ARE INDEX-X SET
         DO I = 1, NDATA
            WRITE ( FILE_UNIT, * ) X(I)
         END DO
      END IF
      PRINT *, 'SUCCESSFULLY WROTE FILE, UNIT:',FILE_UNIT

    END SUBROUTINE WRITE_VECTOR

    SUBROUTINE WRITE_VECTOR_MATRIX(FILE_UNIT,X,Y)
      INTEGER, INTENT(IN) :: FILE_UNIT
      REAL(KIND=8), INTENT(IN) :: X(:)
      REAL(KIND=8), INTENT(IN), OPTIONAL :: Y(:,:)

      INTEGER :: NX,NY,N_CURVES,I1,I2

      NX = SIZE(X)
      NY = SIZE(Y,DIM=1)

      IF(NX .NE. NY) THEN
         PRINT *, 'SIZES ARE NOT COMPATIBLE.'
         RETURN
      END IF

      N_CURVES = SIZE(Y,DIM=2)

      DO I1 = 1,NX
         WRITE(FILE_UNIT, *) X(I1), (Y(I1,I2),I2=1,N_CURVES)
      END DO
      PRINT *, 'SUCESSFULLY WROTE TO FILE, UNIT:', FILE_UNIT

    END SUBROUTINE WRITE_VECTOR_MATRIX
    
END MODULE MY_LIB
